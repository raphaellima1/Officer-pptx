
#pegar modelos depois de 2020
ICMS_table20 <- read_excel("C:/Users/raphael.lima/Documents/Trabalhos em R/1- ICMS power point/ICMS_total19_20.xlsx", 
                           sheet = "Adicional_2_perc", 
                           col_types = c("text","text", "text", "numeric", "text", "numeric"))


#renomear a coluna
colnames(ICMS_table20) <- c('receita', 'Tipo', 'Grupo', 'data','Ano', 'Valor')

# ajustar o ano e data
ICMS_table20 <- ICMS_table20 %>% 
  mutate(data = ymd(data), Ano = year(data)) %>% 
  filter(Tipo =='ICMS') 

# inserir os dias que faltam na tabela criando 
# uma nova tabela e usando merge para alinhar 

df_data <-  data.frame(data =seq(as.Date("2019-01-01"),
                                 as.Date(today()),
                                 by = 1))  %>% 
  
  mutate(dm = paste(dia = day(data), 
                    mes = month(data), 
                    sep ='/'))


##          2019
################################################################################

# filtrar a tabela que tem os dias
df_data19 <- df_data %>% 
  filter(year(data) == 2019)

#filtrar a tabela para 2019
tabela_19 <- ICMS_table20 %>% 
  filter(Ano == 2019) %>%  
  filter(Grupo == grupo) %>% 
  dplyr::select(c('data', 'Valor')) %>% 
  mutate(data = as.Date(data))

# cobinar as tabela
df_19 <- merge(df_data19,tabela_19,  
               by = 'data', 
               all = TRUE)

df_19[is.na(df_19)] <-  0


df_19 <- df_19 %>% 
  mutate(acumulado = cumsum(Valor))

##          2020
################################################################################


df_data20 <- df_data %>% 
  filter(year(data) == 2020)


#filtrar a tabela para 2020
tabela_20 <- ICMS_table20 %>% 
  filter(Ano == 2020) %>%  
  filter(Grupo == grupo) %>% 
  dplyr::select(c('data', 'Valor')) %>% 
  mutate(data = as.Date(data))



# cobinar as tabela
df_20 <- merge(df_data20,tabela_20,  
               by = 'data', 
               all = TRUE)


df_20[is.na(df_20)] <-  0


df_20 <- df_20 %>% 
  mutate(acumulado = cumsum(Valor))

#          UINÃO DAS TABELAS
################################################################################

df_19_20 <-df_19 %>%
  merge(df_20, by = 'dm',
        all.x = TRUE)

colnames(df_19_20) <- c('dm', 'data_2019', 'valor_2019','acumulado_2019',
                        'data_2020', 'valor_2020', 'acumulado_2020')
# remover as variaveis inuteis
rm(df_19,df_20, df_data19,df_data20)


df_data <-  data.frame(data =seq(as.Date("2021-01-01"),
                                 as.Date(today()),by = 1))  %>% 
  mutate(dm = paste(dia = day(data), 
                    mes = month(data), sep ='/'))


df_data21 <- df_data %>% 
  filter(year(data) == 2021)


tabela_21 <- ICMS_table20 %>% 
  filter(Ano == 2021) %>%  
  filter(Grupo == grupo) %>% 
  dplyr::select(c('data', 'Valor')) %>% 
  mutate(data = as.Date(data))

# união das tabelas
tabela_211 <- merge(df_data21,tabela_21,  
                    by = 'data', 
                    all = TRUE)

tabela_211[is.na(tabela_211)] <-  0

tabela_21a <- tabela_211 %>% 
  mutate(acumulado = cumsum(Valor)) 

################################################################################
#       Tabela 2022                                                            #
################################################################################

df_data22 <- df_data %>% 
  filter(year(data) ==2022)


tabela_22 <- ICMS_table20 %>% 
  filter(Ano == 2022) %>%  
  filter(Grupo == grupo) %>% 
  dplyr::select(c('data', 'Valor')) %>% 
  mutate(data = as.Date(data))

# união das tabelas
tabela_222 <- merge(df_data22,tabela_22,  
                    by = 'data', 
                    all = TRUE)

tabela_222[is.na(tabela_222)] <-  0

tabela_22a <- tabela_222 %>% 
  mutate(acumulado = cumsum(Valor))


df_21_22 <-tabela_21a %>%
  merge(tabela_22a, by = 'dm',
        all.x = TRUE)




colnames(df_21_22) <- c('dm', 'data_2021', 'valor_2021','acumulado_2021',
                        'data_2022', 'valor_2022', 'acumulado_2022')



df_21_22 <- df_21_22 %>% 
  mutate(valor_2022 = coalesce(valor_2022, 0), 
         acumulado_2021 = coalesce(acumulado_2021, 0), 
         variacao22_21 = ((acumulado_2022 - acumulado_2021)/acumulado_2021)*100) 

df_21_22 <- arrange(df_21_22, data_2021)

rm(df_data, df_data21,df_data22)

################################################################################

################################################################################

df <-df_19_20 %>%
  merge(df_21_22, by = 'dm',
        all.x = TRUE) %>% 
  arrange(data_2021)

rm(df_19_20, df_21_22, tabela_19, tabela_20, tabela_21, tabela_22,
   tabela_211, tabela_21a)


