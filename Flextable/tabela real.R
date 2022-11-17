
ipcahoje <- data.frame(date = '2022-11-01', IPCA = IP)
#ipcahoje <- data.frame(date = c('2022-10-01', '2022-11-01'), IPCA = c(IP, IP2))


ipca <- get_series(c(IPCA = 433)) %>% 
  filter(date >= '2019-01-01') %>% 
  filter(date < '2023-01-01') %>%
  rbind(ipcahoje) %>% 
  mutate(IPCA = IPCA/100) 
ipca$deflator <- ''
ipca$deflator[1] <- 1


for (i in 2:length(ipca$IPCA)) {
  ipca$deflator[i] <- (as.numeric(ipca$deflator[i-1]))/(1+ ipca$IPCA[i])
}


ipca19 <- ipca %>%
  mutate(mes = month(date),
         ano = year(date)) %>% 
  filter(ano == 2019) %>% 
  select(mes, deflator) %>% 
  rename(deflator19 = 'deflator')


ipca20 <- ipca %>%
  mutate(mes = month(date),
         ano = year(date)) %>% 
  filter(ano == 2020) %>% 
  select(mes, deflator) %>% 
  rename(deflator20 = 'deflator')



ipca21 <- ipca %>%
  mutate(mes = month(date),
         ano = year(date)) %>% 
  filter(ano == 2021) %>% 
  select(mes, deflator) %>% 
  rename(deflator21 = 'deflator')


ipca22 <- ipca %>%
  mutate(mes = month(date),
         ano = year(date)) %>% 
  filter(ano == 2022) %>% 
  select(mes, deflator) %>% 
  rename(deflator22 = 'deflator')


dfipca <- df %>%
  mutate(mes = month(data_2020)) %>% 
  group_by(mes) %>% 
  summarise(valor_2019 = sum(valor_2019),
            valor_2020 = sum(valor_2020),
            valor_2021 = sum(valor_2021),
            valor_2022 = sum(valor_2022)) %>% 
  
  merge(ipca19) %>%
  merge(ipca20) %>%
  merge(ipca21) %>%
  merge(ipca22,all.x = T)

dfipca <- dfipca %>% 
  mutate(valor_R19 = valor_2019* as.numeric(deflator19),
         valor_R20 = valor_2020* as.numeric(deflator20),
         valor_R21 = valor_2021* as.numeric(deflator21),
         valor_R22 = valor_2022* as.numeric(deflator22)) %>% 
  select(mes,valor_R19,valor_R20, valor_R21, valor_R22) %>%
  mutate(mes = month(mes, label = TRUE, abbr = F)) %>% 
  adorn_totals("row") %>%
  mutate(variacao = ((valor_R22- valor_R21)/valor_R21)*100)




tabela_real <- dfipca %>%
  flextable() %>% 
  autofit() %>% 
  colformat_double(j = c('valor_R19','valor_R20','valor_R21','valor_R22'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 0, 
                   na_str = "--") %>% 
  colformat_double(j = c('variacao'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>%
  width(width = 1.8) %>% 
  height(height = 0.25) %>%
  set_header_labels(mes = 'Mês',
                    valor_R19 = '2019',
                    valor_R20 = '2020',
                    valor_R21 = '2021',
                    valor_R22 = '2022',
                    variacao = 'Variação 22/21') %>% 
  #align(align = "right", part = "header")  %>% 
  bg(., i= ~ mes == "Total", part = "body", bg = "#ececec") %>% 
  
  bold(i = 13, bold = TRUE, part = "body") %>% 
  bold( ~ variacao < 0, ~ variacao,  bold = TRUE) %>% 
  color( ~ variacao < 0, ~ variacao,  color = 'red')
 
setwd("./../")
