get_edi_url <- function(prodcode, version){

    name_endpoint <- 'https://pasta.lternet.edu/package/name/eml/'
    dl_endpoint <- 'https://pasta.lternet.edu/package/data/eml/'
    domain_ref <- 'knb-lter-hbr'

    name_request <- paste0(name_endpoint, domain_ref, '/', prodcode, '/',
                           version)

    reqdata <- RCurl::getURLContent(name_request)

    reqdata <- strsplit(reqdata, '\n')[[1]]
    reqdata <- grep('Constants', reqdata, invert = TRUE, value = TRUE) #junk filter for hbef. might need flex
    reqdata <- str_match(reqdata, '([0-9a-zA-Z]+),(.+)')

    element_ids = reqdata[, 2]
    dl_urls <- paste0(dl_endpoint, domain_ref, '/', prodcode, '/', version,
                      '/', element_ids)

    avail_sets <- tibble(url = dl_urls,
                         filename = paste0(reqdata[, 3], '.csv'))

    return(avail_sets)
}

calc_vwc_wateryear <- function(d, var, sample_cutoff = NULL){

    #sample_cutoff: numeric or NULL. remove water years with < sample_cutoff sample
    #   days. if NULL, this parameter is ignored.

    vvar <- intersect(c('precip', 'discharge'), colnames(d))

    d <- d %>%
        select(site, waterYr, !!var, !!vvar) %>%
        filter(! if_any(c(!!var, !!vvar), is.na)) %>%
        group_by(site, waterYr) %>%
        summarize(!!sym(var) := mean(!!sym(var) * !!sym(vvar), na.rm = TRUE) /
                      mean(!!sym(vvar), na.rm = TRUE),
                  n = n(),
                  .groups = 'drop') %>%
        arrange(waterYr)

    d$source <- ifelse(vvar == 'precip', 'Precipitation', 'Streamwater')

    low_n <- which(d$n < sample_cutoff)
    if(length(low_n)){
        z <- ifelse(is.null(sample_cutoff), 'keeping', 'dropping')
        print(paste(z, 'water years with <', sample_cutoff, 'samples:'))
        print(arrange(d[low_n, ], site, waterYr),
              n = 100)
    }

    if(! is.null(sample_cutoff)){
        d <- filter(d, n >= sample_cutoff)
    }

    return(d)
}
