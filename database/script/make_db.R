# libraries
library(magrittr)

### load and clean S&P500 old prices
raw <-
    readr::read_csv(here::here('database', 'raw_data', 'sp_hist_prices.csv'))

# end-of-month prices
eom_px <-
    dplyr::bind_rows(
        # S&P500 Net Total Return
        raw[, 1:2] %>%
            dplyr::mutate(index = 'SPTR500N') %>%
            `colnames<-`(c('date', 'price', 'index')),
        # S&P500 Total Return
        raw[, 5:6] %>%
            dplyr::mutate(index = 'SPXT') %>%
            `colnames<-`(c('date', 'price', 'index'))
    ) %>%
    # year/month as integer for DB search
    dplyr::mutate(year  = as.integer(lubridate::year(date)),
                  month = as.integer(lubridate::month(date))) %>%
    # rearrange cols
    dplyr::select(index, year, month, date, price) %>%
    # date as character for SQLite
    dplyr::mutate(date = as.character(date)) %>%
    # filter NA rows
    dplyr::filter(!is.na(date))


# beginning-of-month prices
bom_px <-
    dplyr::bind_rows(
        # S&P500 Net Total Returns
        raw[, c(1,3,4)] %>%
            dplyr::mutate(index = 'SPTR500N') %>%
            `colnames<-`(c('dt_end', 'dt_start', 'price', 'index')),
        # S&P500 Total Return
        raw[, c(5,7,8)] %>%
          dplyr::mutate(index = 'SPXT') %>%
          `colnames<-`(c('dt_end', 'dt_start', 'price', 'index'))
    ) %>%
    # year/month as integer for DB search
    dplyr::mutate(end_yr = as.integer(lubridate::year(dt_end)),
                  end_mt = as.integer(lubridate::month(dt_end))) %>%
    # rearrange cols
    dplyr::select(index, end_yr, end_mt, dt_end, dt_start, price) %>%
    # dates sa characters for SQLite
    dplyr::mutate(dt_end   = as.character(dt_end),
                  dt_start = as.character(dt_start)) %>%
    # filter NA rows
    dplyr::filter(!is.na(dt_end))


### read, load and clean CAPE data

#url <- 'http://www.econ.yale.edu/~shiller/data/ie_data.xls'

#download.file(url, here::here('database', 'raw_data'))

cape_data <-
    readxl::read_xls(here::here('database', 'raw_data', 'ie_data.xls'),
                     sheet = 5,
                     skip = 7,
                     na = c('', 'NA'),
                     col_types = c('text', rep('numeric', 4), 'text', rep('numeric', 16))
    ) %>%
    janitor::remove_empty(which = 'cols') %>%
    dplyr::select(c(Date:CPI, Price...8:`TR CAPE`)) %>%
    `colnames<-`(c('date', 'price', 'dividend', 'earning', 'cpi', 'real_px', 'real_dvd',
                   'real_px_tr', 'real_earn', 'real_tr_earn', 'cape', 'tr_cape')
    ) %>%
    dplyr::mutate(year = as.integer(stringr::str_sub(date, start = 1, end = 4))) %>%
    dplyr::filter(rowSums(is.na(.)) < 13)


# use the recycling of cbind to attach a list of months for each year
yr_mon <- as.data.frame(cbind(cape_data[['year']], seq(1, 12, 1))) %>%
    `colnames<-`(c('year', 'month'))

cape_data <-
    dplyr::bind_cols(cape_data, yr_mon['month']) %>%
    dplyr::select(year, month, dplyr::everything(), -date)



### load data to DB

# open connection to DB
con <- DBI::dbConnect(RSQLite::SQLite(), here::here('database', 'cape.db'))    

# check list of tables
DBI::dbListTables(con)

# upload end-of-month prices to 'eom' table
DBI::dbAppendTable(con, 'sp_eom_price', eom_px)

# upload beginninbg-of-month prices to 'bom' table
DBI::dbAppendTable(con, 'sp_bom_price', bom_px)

# upload cape data
DBI::dbAppendTable(con, 'cape', cape_data)

# close conncetion to DB
DBI::dbDisconnect(con)
