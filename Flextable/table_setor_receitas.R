receitas2 <- receitas2 %>% 
  mutate(data = ymd(data), Ano = year(data))

table21 <- receitas2 %>% 
  filter(Ano == 2021) %>% 
  group_by(Tipo) %>% 
  summarise(valor = sum(Valor, rm.na = TRUE)) %>% 
  arrange(desc(valor)) 


total21 <- sum(table21$valor)

table21 <- table21 %>% 
  mutate(perc_total = (valor/total21)*100)

colnames(table21) <- c('Grupo', '2021', 'perc_21')


table22 <- receitas2 %>% 
  filter(Ano == 2022) %>% 
  group_by(Tipo) %>% 
  summarise(valor = sum(Valor, rm.na = TRUE))%>%
  arrange(desc(valor)) 

total22 <- sum(table22$valor)

table22 <- table22 %>% 
  mutate(perc_total = (valor/total22)*100)


colnames(table22) <- c('Grupo', '2022', 'perc_22')
table <- table21 %>% 
  merge(table22) %>% 
  arrange(desc(perc_22)) %>% 
  adorn_totals("row")

table <- table[, c(1,2,4,3,5)]

colnames(table) <- c("SETOR ECONÔMICO", " 2021", " 2022", "2021", 
                     "2022")


table_setor <- table %>% 
  flextable() %>% 
  add_header_row(values = c(' ',"VALOR ANUAL", "PARTICIPAÇÃO PERCENTUAL"), 
                 colwidths = c(1,2, 2)) %>% 
  
  colformat_double(big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>%
  
  colformat_num(j = c(' 2021',' 2022'), digits =0,big.mark="." ) %>% 
  
  
  bold(i = 6, bold = TRUE, part = "body") %>% 
  bg(., i= ~ `SETOR ECONÔMICO` == "Total", part = "body", bg = "#ececec") %>% 
  width(j = 1, width = 3.8) %>% 
  width(j = c(2,3,4,5), width = 1.7) %>% 
  height(height = 0.45)



table_setor

# sair da pasta
setwd("./../")


rm(total21, total22, table22,table21)
