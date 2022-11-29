receitas4 <- receitas2 %>% 
  mutate(mes = month(data),
         mesAno = paste(mes, Ano, sep = '-'))

ipca <- ipca %>% 
  mutate(mesAno = paste(month(date), year(date), sep = '-'))


receitas5 <- receitas4 %>% 
  merge(ipca, by.x = 'mesAno', by.y = 'mesAno') %>% 
  mutate(Valor = Valor*as.numeric(deflator),
         mes = month(date),
         ano = year(date)) %>% 
  drop_na()





table_22 <- receitas5 %>% 
  filter(mes == mes1) %>% 
  filter(ano == 2022) %>%
  group_by(Tipo) %>% 
  summarise(Valor = sum(Valor)) %>% 
  arrange(desc(Valor)) %>%
  select(Tipo, Valor) %>% 
  setNames(c('Grupo de receitas', 'mes'))


table_22_pass <- receitas5 %>% 
  filter(data <= mes_pass) %>% 
  filter(ano == 2022) %>% 
  filter(mes == mes_1)%>%
  group_by(Tipo) %>% 
  summarise(Valor = sum(Valor)) %>% 
  select(Tipo, Valor) %>%
  setNames(c('Grupo de receitas', 'mes_pas'))

table_21_pass <- receitas5 %>% 
  filter(data <= ano_pass) %>% 
  filter(Ano == 2021) %>% 
  filter(mes == mes1)%>%
  group_by(Tipo) %>% 
  summarise(Valor = sum(Valor)) %>% 
  arrange(desc(Valor)) %>%
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
  
  bold(i = 6, bold = TRUE, part = "body") %>% 
  
  bg(., i= ~ `Grupo de receitas` == "Total", part = "body", bg = "#ececec")  %>% 
  width(j = 1, width = 3.6) %>% 
  width(j = c(2,3,4,5, 6), width = 1.6)



setwd("./../")

