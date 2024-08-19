#2024-08-19
#latest EDI data versions (retrieved in section 1 below):
#   stream/precip chemistry vsn 11
#   discharge vsn 17
#   precip vsn 22

library(tidyverse)
library(lubridate)
library(RCurl)
library(imputeTS)

source('src/helpers.R')

#set these to your liking
cutoff_s <- 10 #number of stream samples per water-year to require (NULL to ignore)
cutoff_p <- NULL #same, but for precip samples
site <- 'W6' #W1-9 are available. appropriate rain gauges are automatically chosen.
bad_codes <- c(955, 969, 970) #remove contaminated/questionable samples
# bad_codes <- c(955, 969, 970, 912, 911, 319) #remove storm/low flow samples too
# bad_codes <- c() #don't remove any samples
chem_maxgap <- 3 #number of consecutive chem samples to interpolate (can be 0)
p_maxgap <- 1 #number of consecutive precip samples to interpolate (can be 0)
q_maxgap <- 1 #number of consecutive discharge samples to interpolate (can be 0)

## 1. setup folders; download data ####

dir.create('data_in', showWarnings = FALSE)

chem_urls <- get_edi_url(prodcode = 208, version = 11)
discharge_urls <- get_edi_url(prodcode = 1, version = 17)
precip_urls <- get_edi_url(prodcode = 13, version = 22)
all_urls <- bind_rows(chem_urls, discharge_urls, precip_urls) %>%
    filter(! grepl('Methods|info', filename))

options(timeout = 3600)
for(i in 1:nrow(all_urls)){

    fl <- file.path('data_in', all_urls$filename[i])
    if(file.exists(fl)) next
    cat('downloading', all_urls$filename[i], 'to data_in/ \n')

    res <- try({
        download.file(url = all_urls$url[i],
                      destfile = fl)
    })

    if(inherits(res, 'try-error')){
        suppressWarnings(file.remove(fl))
        cat('Removed partial file', fl, '\n')
    }
}

if(! length(list.files('data_in')) == 21){
    stop('some files failed to download. run the above loop again.')
}

## 2. load and clean data ####

#establish which precip gauges will be used for N- and S-facing watersheds
north_facing_gauges <- c('N', paste0('RG', c(12:17, 19:21, 23:25)))
south_facing_gauges <- c('S', paste0('RG', c(1:11, 22)))

if(site %in% c('W7', 'W8', 'W9')){
    gauge_set <- north_facing_gauges
} else {
    gauge_set <- south_facing_gauges
}

#drop cols that won't be used (simplifies unit conversion)
#pH and pHmetrohm are merged and converted to [H ion]
#ANC960 and ANCMet are merged
unused_vars <- c('pH', 'DIC', 'temp', 'ANC960', 'ANCMet', 'TMAl', 'OMAl',
                 'Al_ICP', 'Al_ferron', 'DOC', 'TDN', 'DON', 'ionError',
                 'ionBalance', 'pHmetrohm', 'PO4', 'cationCharge',
                 'anionCharge')

drop_cols <- c('timeEST', 'barcode', 'fieldCode', 'notes', 'uniqueID', 'duplicate',
               'sampleType', 'canonical', 'gageHt', 'hydroGraph', 'flowGageHt',
               'precipCatch', unused_vars)

#chemistry
s <- suppressWarnings(read_csv('data_in/HubbardBrook_weekly_stream_chemistry.csv',
                               show_col_types = FALSE)) %>%
    filter(! fieldCode %in% bad_codes) %>%
    # rowwise() %>%
    # mutate(pH = mean(c(pH, pHmetrohm), na.rm = TRUE),
    #        ANC = mean(c(ANC960, ANCMet), na.rm = TRUE)) %>%
    # ungroup() %>%
    # mutate(H = 10^(-pH) * 1.00784 * 1000) %>% #pH -> mg/L
    select(-any_of(drop_cols)) %>%
    group_by(site, date) %>%
    summarize(across(-waterYr, ~mean(., na.rm = TRUE)),
              waterYr = first(waterYr),
              site = first(site),
              .groups = 'drop') %>%
    arrange(site, date)

p <- read_csv('data_in/HubbardBrook_weekly_precipitation_chemistry.csv',
              guess_max = 10000,
              show_col_types = FALSE) %>%
    # rowwise() %>%
    # mutate(pH = mean(c(pH, pHmetrohm), na.rm = TRUE),
    #        ANC = mean(c(ANC960, ANCMet), na.rm = TRUE)) %>%
    # ungroup() %>%
    # mutate(H = 10^(-pH) * 1.00784 * 1000) %>% #pH -> mg/L
    filter(! fieldCode %in% bad_codes,
           site %in% gauge_set) %>%
    select(-any_of(c(drop_cols, 'site'))) %>%
    group_by(date) %>%
    summarize(across(-waterYr, ~mean(., na.rm = TRUE)),
              waterYr = first(waterYr),
              site = 'all',
              .groups = 'drop') %>%
    relocate(site) %>%
    arrange(site, date)

#interpolate gaps in chemistry series
s <- s %>%
    group_by(site) %>%
    mutate(across(-any_of(c('site', 'date', 'waterYr')),
                  ~na_interpolation(., maxgap = chem_maxgap))) %>%
    ungroup()
p <- p %>%
    group_by(site) %>%
    mutate(across(-any_of(c('site', 'date', 'waterYr')),
                  ~na_interpolation(., maxgap = chem_maxgap))) %>%
    ungroup()

#there were four precip ANC values originally. all 0. remove these
p$ANC <- NA

#precip
p0 <- read_csv('data_in/HBEF daily precip.csv',
               show_col_types = FALSE) %>%
    filter(rainGage %in% gauge_set) %>%
    group_by(date = DATE) %>%
    summarize(precip = mean(Precip, na.rm = TRUE),
              site = 'all',
              .groups = 'drop') %>%
    relocate(site) %>%
    arrange(site, date)

#interpolate gaps in precipitation series
p0 <- p0 %>%
    group_by(site) %>%
    mutate(precip = na_interpolation(precip, maxgap = p_maxgap)) %>%
    ungroup()

#discharge
q1 <- map_dfr(list.files('data_in', pattern = 'w[0-9]_.*?2012\\.csv', full.names = TRUE),
              ~read_csv(., show_col_types = FALSE)) %>%
    mutate(site = paste0('W', WS)) %>%
    group_by(site, date = as.Date(DATETIME)) %>%
    summarize(discharge = mean(Discharge_ls, na.rm = TRUE),
              .groups = 'drop')

q2 <- map_dfr(list.files('data_in', pattern = 'w[0-9]_.*?_5min\\.csv', full.names = TRUE),
              ~read_csv(., show_col_types = FALSE)) %>%
    group_by(WS, date = as.Date(DATETIME)) %>%
    summarize(discharge = mean(Discharge_ls, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(site = paste0('W', WS)) %>%
    select(site, date, discharge)

q <- bind_rows(q1, q2) %>%
    arrange(site, date)

#interpolate gaps in discharge series
q <- q %>%
    group_by(site) %>%
    mutate(discharge = na_interpolation(discharge, maxgap = q_maxgap)) %>%
    ungroup()

#join Q, P to C
s <- left_join(s, q, by = c('site', 'date')) %>%
    relocate(waterYr, discharge, .after = 'date')

p <- left_join(p, p0, by = c('site', 'date')) %>%
    relocate(waterYr, precip, .after = 'date')

#convert solutes to ueq
# s <- convert_to_equivalents(s)
# p <- convert_to_equivalents(p)

## 3. volume-weighted SiO2 by water year ####

sio2_vwc <- calc_vwc_year(s, 'SiO2', sample_cutoff = cutoff_s) %>%
    select(-n, -source)
