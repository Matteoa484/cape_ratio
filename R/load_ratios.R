# connect to DB
con <- DBI::dbConnect(RSQLite::SQLite(), here::here('database', 'cape.db'))

## load CAPE ratio
cape_xts <-
    # connect to DB and download CAPE from 1970
    dplyr::tbl(con, 'cape') %>%
    dplyr::filter(year >= 1970) %>%
    dplyr::select(month, year, cape) %>%
    dplyr::mutate(cape = as.numeric(cape)) %>%
    dplyr::collect() %>%
    # convert year and month to zoo::yearmon
    dplyr::mutate(date = zoo::as.yearmon(paste0(year, '-', month))) %>%
    dplyr::select(date, cape) %>%
    # convert to xts object
    xts::xts(x = .[, -1], order.by = .[[1]])


## load trailing P/E
tpe_xts <-
    readr::read_csv(here::here('database', 'raw_data', 'sp_hist_pe.csv'),
                    col_types = readr::cols(
                        Date = readr::col_date(format = ""),
                        PE_RATIO = readr::col_double()
                    )
    ) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(
        period = paste0(lubridate::year(Date), '-', lubridate::month(Date)),
        period = zoo::as.yearmon(period)
    ) %>%
    dplyr::select(date = period, pe_ratio = PE_RATIO) %>%
    xts::xts(x = .[, -1], order.by = .[[1]])
