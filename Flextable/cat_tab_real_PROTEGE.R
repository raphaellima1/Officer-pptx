setwd("./../")
setwd('./Dados/')


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
ipca <- ipca %>% 
  mutate(mesAno = glue('{month(date)}-{year(date)}'),
         deflator = as.numeric(deflator))


#pegar modelos depois de 2020
ICMS_table <- read_excel("ICMS_TOTAL_2022.xlsx", 
                         sheet = "PROTEGE", 
                         col_types = c("text", "text", "numeric", 'numeric', "numeric"))


#renomear a coluna
colnames(ICMS_table) <- c('Tipo', 'Grupo', 'data','Ano', 'Valor')

ICMS_table1 <- ICMS_table %>%
  mutate(data = ymd(data), Ano = year(data)) %>% 
  mutate(mesAno = glue('{month(data)}-{year(data)}')) %>% 
  merge(ipca, by.x = 'mesAno', by.y = 'mesAno') %>% 
  mutate(Valor = Valor*deflator,
         mes = month(date),
         ano = year(date)) %>% 
  drop_na()




data <- max(ICMS_table1$data)
mes1 <- month(data)
mes_1 <- mes1 -1
ifelse(day(data) == 31, 
       mes_pass <- data-days(1)- months(1),
       mes_pass <- data- months(1)) 

Ano <- year(data)
ano_pass <- data - years(1)
ano_1 <- year(data) -1


table_22 <- ICMS_table1 %>% 
  filter(mes == mes1) %>% 
  filter(ano == 2022) %>%
  group_by(Grupo) %>% 
  summarise(Valor = sum(Valor)) %>% 
  arrange(desc(Valor)) %>%
  select(Grupo, Valor) %>% 
  setNames(c('Grupo de receitas', 'mes'))


table_22_pass <- ICMS_table1 %>% 
  filter(data <= mes_pass) %>% 
  filter(ano == 2022) %>% 
  filter(mes == mes_1)%>%
  group_by(Grupo) %>% 
  summarise(Valor = sum(Valor)) %>% 
  select(Grupo, Valor) %>%
  setNames(c('Grupo de receitas', 'mes_pas'))

table_21_pass <- ICMS_table1 %>% 
  filter(data <= ano_pass) %>% 
  filter(Ano == 2021) %>% 
  filter(mes == mes1)%>%
  group_by(Grupo) %>% 
  summarise(Valor = sum(Valor)) %>% 
  arrange(desc(Valor)) %>%
  setNames(c('Grupo de receitas', 'ano_pass'))

tableb <- table_22 %>% 
  merge(table_22_pass, all.x =TRUE) %>% 
  ungroup() %>% 
  arrange(desc(mes)) 
tableb <- tableb %>% 
  merge(table_21_pass)%>% 
  ungroup() %>% 
  #arrange(desc(mes)) %>%  
  adorn_totals("row") %>% 
  mutate(Variacao = (mes-mes_pas)/mes_pas*100,
         Variacao_pass = (mes-ano_pass)/ano_pass*100) 


std_border <- fp_border(color = "black", style = "solid", width = 1.5)

tableb <- tableb[,c(1,2,3,5,4,6)]


tabela_mes_r <- tableb%>% 
  flextable() %>% 
  colformat_double(j =c('mes', 'mes_pas', 'ano_pass'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 0, 
                   na_str = "--") %>%
  
  set_header_labels(mes = glue('{mes1}/{ano}'),
                    mes_pas = glue('{mes_1}/{ano}'),
                    Variacao = glue('Variação % {mes1}/{ano} - {mes_1}/{ano}'),
                    ano_pass = glue('{mes1}/{ano_1}'),
                    Variacao_pass = glue('Variação % {mes1}/{ano} - {mes1}/{ano_1}'))%>% 
  #width(width = 1.8) %>% 
  height(height = 0.38) %>% 
  
  
  colformat_double(j = c('Variacao','Variacao_pass'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  
  color( ~ Variacao < 0, ~ Variacao,  color = 'red' ) %>% 
  
  bold( ~ Variacao < 0, ~ Variacao,  bold = TRUE )%>% 
  
  color( ~ Variacao_pass < 0, ~ Variacao_pass,  color = 'red' ) %>% 
  
  bold( ~ Variacao_pass < 0, ~ Variacao_pass,  bold = TRUE ) %>% 
  
# bold(i = 10, bold = TRUE, part = "body") %>% 
  
  bg(., i= ~ `Grupo de receitas` == "Total", part = "body", bg = "#ececec")  %>% 
  width(j = 1, width = 3.6) %>% 
  width(j = c(2,3,4,5, 6), width = 1.6)

setwd("./../")

