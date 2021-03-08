con <- DBI::dbConnect(RSQLite::SQLite(), here::here('database', 'cape.db'))

DBI::dbListTables(con)

dplyr::tbl(con, 'cape')
dplyr::tbl(con, 'sp_eom_price')


cape_xts <-
    dplyr::tbl(con, 'cape') %>%
    dplyr::filter(year >= 1970) %>%
    dplyr::select(month, year, cape) %>%
    dplyr::mutate(cape = as.numeric(cape)) %>%
    dplyr::collect() %>%
    dplyr::mutate(date = zoo::as.yearmon(paste0(year, '-', month))) %>%
    dplyr::select(date, cape) %>%
    xts::xts(x = .[, -1], order.by = .[[1]])

dplyr::tbl(con, 'sp_eom_price') %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(min = min(year, na.rm = TRUE))

sp_xts <-
    dplyr::tbl(con, 'sp_eom_price') %>%
    dplyr::filter(index == 'SPXT') %>%
    dplyr::select(year, month, price) %>%
    dplyr::mutate(price = as.numeric(price)) %>%
    dplyr::collect() %>%
    dplyr::mutate(date = zoo::as.yearmon(paste0(year, '-', month))) %>%
    dplyr::select(date, price) %>%
    xts::xts(x = .[, -1], order.by = .[[1]])


sp_ret <-
    xts::merge.xts(
        sp_xts / xts::lag.xts(sp_xts, k = 6) - 1,
        sp_xts / xts::lag.xts(sp_xts, k = 12) - 1,
        sp_xts / xts::lag.xts(sp_xts, k = 36) - 1,
        sp_xts / xts::lag.xts(sp_xts, k = 60) - 1,
        sp_xts / xts::lag.xts(sp_xts, k = 120) - 1
    ) %>%
    `colnames<-`(c('6_m', '1_y', '3_y', '5_y', '10_y'))


    
  
x <- sp_ret$`6_m` %>% na.omit %>% .[1, ]
y <- sp_ret$`1_y` %>% na.omit %>% .[1, ]
z <- sp_ret$`5_y` %>% na.omit %>% .[1, ]


# annualized return for 6-months
((1 + x) ^ (12 / 6)) - 1

# calculate to all data frame
ann_ret_6m <- ((1 + sp_ret$`6_m`) ^ (12 / 6)) - 1

# test plot
xts::merge.xts(cape_xts, ann_ret_6m, join = 'inner') %>%
    na.omit() %>%
    tibble::as_tibble(rownames = 'date') %>%
    dplyr::mutate(date = zoo::as.yearmon(date)) %>%
    `colnames<-`(c('date', 'cape', '6m_ret')) %>%
    ggplot(aes(x = cape, y = `6m_ret`)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE)


# annualized return for 1-year
((1 + y) ^ (12 / 12)) - 1



# annualized return for 5-years
((1 + z) ^ (12 / 60)) - 1

ann_ret_5y <- ((1 + sp_ret$`5_y`) ^ (12 / 60)) - 1

# test plot
xts::merge.xts(cape_xts, ann_ret_5y, join = 'inner') %>%
    na.omit() %>%
    tibble::as_tibble(rownames = 'date') %>%
    dplyr::mutate(date = zoo::as.yearmon(date)) %>%
    `colnames<-`(c('date', 'cape', '5y_ret')) %>%
    ggplot(aes(x = cape, y = `5y_ret`)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
