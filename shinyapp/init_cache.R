source('helper.R')

df_init <- getSymbols('TSLA', from = '2017-08-01', to = '2020-07-31', auto.assign = FALSE) %>% na.approx()

saveRDS(df_init, 'data_input/TSLA.rds')