receitas3 <- receitas2 %>% 
  mutate(mes = month(data))

# variaveis para 

data <- ult_atu
mes1 <- month(data)
mes_1 <- mes1 -1

ifelse(day(data) == 31, 
       mes_pass <- data-days(1)- months(1),
       mes_pass <- data- months(1)) 


ano <- year(data)
ano_pass <- data - years(1)
ano_1 <- year(data) -1


table_22 <- receitas3 %>% 
  filter(mes == mes1) %>% 
  filter(Ano == ano) %>% 
  group_by(Tipo) %>% 
  summarise(valor = sum(Valor)) %>% 
  arrange(desc(valor)) %>%
  setNames(c('Grupo de receitas', 'mes'))


table_22_pass <- receitas3 %>% 
  filter(data <= mes_pass) %>% 
  filter(Ano == ano) %>% 
  filter(mes == mes_1)%>%  
  group_by(Tipo) %>% 
  summarise(valor = sum(Valor)) %>% 
  arrange(desc(valor)) %>%
  setNames(c('Grupo de receitas', 'mes_pas'))

table_21_pass <- receitas3 %>% 
  filter(data <= ano_pass) %>% 
  filter(Ano == ano_1) %>% 
  filter(mes == mes1)%>%  
  group_by(Tipo) %>% 
  summarise(valor = sum(Valor)) %>% 
  arrange(desc(valor)) %>%
  setNames(c('Grupo de receitas', 'ano_pass'))

tableb <- table_22 %>% 
  merge(table_22_pass) %>% 
  ungroup() %>% 
  arrange(desc(mes)) 

tableb <- tableb %>% 
  merge(table_21_pass)%>% 
  ungroup() %>% 
  arrange(desc(mes)) %>%  
  adorn_totals("row") %>% 
  mutate(Variacao = (mes-mes_pas)/mes_pas*100,
         Variacao_pass = (mes-ano_pass)/ano_pass*100) 


std_border <- fp_border(color = "black", style = "solid", width = 1.5)

tableb <- tableb[,c(1,2,3,5,4,6)]


tabela_mes <- tableb%>% 
  flextable() %>% 
  # vline(j = c(1,2,4), border = std_border) %>%
  # vline_left( border = std_border) %>%
  # vline_right( border = std_border) %>%
  set_header_labels(mes = glue('{mes1}/{ano}'),
                    mes_pas = glue('{mes_1}/{ano}'),
                    Variacao = glue('Variação % {mes1}/{ano} - {mes_1}/{ano}'),
                    ano_pass = glue('{mes1}/{ano_1}'),
                    Variacao_pass = glue('Variação % {mes1}/{ano} - {mes1}/{ano_1}'))%>% 
  #width(width = 1.8) %>% 
  height(height = 0.38) %>% 
  colformat_double(j =c('mes', 'mes_pas', 'ano_pass'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 0, 
                   na_str = "--") %>%
  
  colformat_double(j = c('Variacao','Variacao_pass'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  
  color( ~ Variacao < 0, ~ Variacao,  color = 'red' ) %>% 
  
  bold( ~ Variacao < 0, ~ Variacao,  bold = TRUE )%>% 
  
  color( ~ Variacao_pass < 0, ~ Variacao_pass,  color = 'red' ) %>% 
  
  bold( ~ Variacao_pass < 0, ~ Variacao_pass,  bold = TRUE ) %>% 
  
  bold(i = 6, bold = TRUE, part = "body") %>% 
  
  bg(., i= ~ `Grupo de receitas` == "Total", part = "body", bg = "#ececec")  %>% 
  width(j = 1, width = 3.6) %>% 
  width(j = c(2,3,4,5, 6), width = 1.6)


setwd('./../')
