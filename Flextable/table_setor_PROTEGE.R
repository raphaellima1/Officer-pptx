setwd("./../")
setwd('./Dados/')

#pegar modelos depois de 2020
ICMS_table <- read_excel("ICMS_TOTAL_2022.xlsx", 
                           sheet = "PROTEGE", 
                           col_types = c("text", "text", "numeric", 'numeric', "numeric"))


#renomear a coluna
colnames(ICMS_table) <- c('Tipo', 'Grupo', 'data','Ano', 'Valor')

ICMS_table <- ICMS_table %>% 
  mutate(data = ymd(data), Ano = year(data))


table21 <- ICMS_table %>% 
  filter(Ano == 2021) %>% 
  group_by(Grupo) %>% 
  summarise(valor = sum(Valor, rm.na = TRUE))%>%
  adorn_totals("row")

total21 <- table21$valor[10]

table21 <- table21 %>% 
  mutate(perc_total = (valor/total21)*100)

colnames(table21) <- c('Grupo', '2021', 'perc_21')


table22 <- ICMS_table %>% 
  filter(Ano == 2022) %>% 
  group_by(Grupo) %>% 
  summarise(valor = sum(Valor, rm.na = TRUE))%>%
  
  adorn_totals("row")

total22 <- table22$valor[10]

table22 <- table22 %>% 
  mutate(perc_total = (valor/total22)*100)


colnames(table22) <- c('Grupo', '2022', 'perc_22')
table <- table21 %>% 
  merge(table22)

table <- table[, c(1,2,4,3,5)]

colnames(table) <- c("SETOR ECONÔMICO", " 2021", " 2022", "2021", 
  "2022")


table_setor <- table %>% 
  flextable() %>% 
  add_header_row(values = c(' ',"VALOR ANUAL", "PARTICIPAÇÃO PERCENTUAL"), 
                 colwidths = c(1,2, 2))%>% 
  
  colformat_double(j = c(' 2021',' 2022'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 0, 
                   na_str = "--") %>% 
  
  colformat_double(j = c('2021','2022'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  

  
  bold(i = 10, bold = TRUE, part = "body") %>% 
  bg(., i= ~ `SETOR ECONÔMICO` == "Total", part = "body", bg = "#ececec") %>% 
  width(j = 1, width = 3.8) %>% 
  width(j = c(2,3,4,5), width = 1.7) %>% 
  height(height = 0.45)

# sair da pasta
setwd("./../")
