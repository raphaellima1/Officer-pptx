df
fist_day <- ult_atu - days()

df_dia <- df %>% 
  filter(data_2022 > fist_day & data_2022 <= ult_atu) %>% 
  select(dm, acumulado_2019, acumulado_2020, acumulado_2021, acumulado_2022,variacao22_21)


colnames(df_dia) <- c('Dia/Mês', '2019','2020','2021','2022', 'Variação 22/21')
 

tabela_dia <- df_dia %>% 
  flextable()%>% 
  width(width = 1.4) %>% 
  colformat_double(big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  colformat_num(j = c('2019', '2020','2021','2022'), digits =0,big.mark="." )
tabela_dia

setwd("./../")