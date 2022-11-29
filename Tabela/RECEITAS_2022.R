setwd("./../")
setwd('./Dados/')


# ICMS
################################################################################
ICMS_table <- read_excel("ICMS_TOTAL_2022.xlsx", 
                         sheet = "ICMS", 
                         col_types = c("text", "text", "numeric", "text", "numeric"))

#renomear a coluna
colnames(ICMS_table) <- c('Tipo', 'Grupo', 'data','Ano', 'Valor')

ICMS_table <- ICMS_table %>% 
  select(Tipo, data,Ano, Valor) 
# ADICIONAL 2%
################################################################################
#pegar modelos depois de 2020
ADD2 <- read_excel("ICMS_TOTAL_2022.xlsx", 
                           sheet = "Adicional_2_perc", 
                           col_types = c("text","text", "text", "numeric", "text", "numeric"))


#renomear a coluna
colnames(ADD2) <- c('receita', 'Tipo', 'Grupo', 'data','Ano', 'Valor')

ADD2 <- ADD2 %>% 
  select(Tipo, data,Ano, Valor) %>% 
  mutate(Tipo = 'Adicional 2%')

# PROTEGE
################################################################################
#pegar modelos depois de 2020
protege <- read_excel("ICMS_TOTAL_2022.xlsx", 
                           sheet = "PROTEGE", 
                           col_types = c("text", "text", "numeric", "text", "numeric"))


#renomear a coluna
colnames(protege) <- c( 'Tipo', 'Grupo', 'data','Ano', 'Valor')

protege <- protege %>% 
  select(Tipo, data, Ano, Valor) 

#IPVA
################################################################################
#pegar modelos depois de 2020
IPVA <- read_excel("ICMS_TOTAL_2022.xlsx", 
                           sheet = "IPVA", 
                           col_types = c("text", "numeric", "text", "numeric"))

IPVA <- drop_na(IPVA)

#renomear a coluna
colnames(IPVA) <- c('Tipo', 'data','Ano', 'Valor')

#ITCD
################################################################################
#pegar modelos depois de 2020
ITCD <- read_excel("ICMS_TOTAL_2022.xlsx", 
                           sheet = "ITCD", 
                           col_types = c("text", "numeric", "text", "numeric"))


ITCD <- drop_na(ITCD)


#renomear a coluna
colnames(ITCD) <- c('Tipo', 'data','Ano', 'Valor')





receitas <- ICMS_table %>% 
  rbind(ADD2) %>% 
  rbind(protege) %>% 
  rbind(IPVA) %>% 
  rbind(ITCD)

rm(ICMS_table, ADD2, protege, IPVA, ITCD)

receitas2 <- receitas
receitas <- receitas %>% 
  mutate(data = ymd(data), Ano = year(data)) %>% 
  group_by(data, Ano) %>% 
  summarise(Valor = sum(Valor))


df_data <-  data.frame(data =seq(as.Date("2019-01-01"),
                                 as.Date(today()),
                                 by = 1))  %>% 
  
  mutate(dm = paste(dia = day(data), 
                    mes = month(data), 
                    sep ='/'))


# filtrar a tabela que tem os dias
df_data19 <- df_data %>% 
  filter(year(data) == 2019)

#filtrar a tabela para 2019
tabela_19 <- receitas %>% 
  filter(Ano == 2019) %>%  
  #filter(Grupo == grupo) %>% 
  dplyr::select(c('data', 'Valor')) %>% 
  mutate(data = as.Date(data))

# cobinar as tabela
df_19 <- merge(df_data19,tabela_19,  
               by = 'data', 
               all = TRUE)

df_19[is.na(df_19)] <-  0

df_19 <- df_19 %>% 
  mutate(acumulado = cumsum(Valor))

rm(tabela_19, df_data19)


# 2020
################################################################################
# filtrar a tabela que tem os dias
df_data20 <- df_data %>% 
  filter(year(data) == 2020)

#filtrar a tabela para 2020
tabela_20 <- receitas %>% 
  filter(Ano == 2020) %>%  
  #filter(Grupo == grupo) %>% 
  dplyr::select(c('data', 'Valor')) %>% 
  mutate(data = as.Date(data))

# cobinar as tabela
df_20 <- merge(df_data20,tabela_20,  
               by = 'data', 
               all = TRUE)

df_20[is.na(df_20)] <-  0

df_20 <- df_20 %>% 
  mutate(acumulado = cumsum(Valor))

rm(tabela_20, df_data20)


# 2022
################################################################################
# filtrar a tabela que tem os dias
df_data22 <- df_data %>% 
  filter(year(data) == 2022)

#filtrar a tabela para 2022
tabela_22 <- receitas %>% 
  filter(Ano == 2022) %>%  
  #filter(Grupo == grupo) %>% 
  dplyr::select(c('data', 'Valor')) %>% 
  mutate(data = as.Date(data))

# cobinar as tabela
df_22 <- merge(df_data22,tabela_22,  
               by = 'data', 
               all = TRUE)

df_22[is.na(df_22)] <-  0

df_22 <- df_22 %>% 
  mutate(acumulado = cumsum(Valor))

rm(tabela_21, df_data21)


# 2021
################################################################################
# filtrar a tabela que tem os dias
df_data21 <- df_data %>% 
  filter(year(data) == 2021)

#filtrar a tabela para 2021
tabela_21 <- receitas %>% 
  filter(Ano == 2021) %>%  
  #filter(Grupo == grupo) %>% 
  dplyr::select(c('data', 'Valor')) %>% 
  mutate(data = as.Date(data))

# cobinar as tabela
df_21 <- merge(df_data21,tabela_21,  
               by = 'data', 
               all = TRUE)

df_21[is.na(df_21)] <-  0

df_21 <- df_21 %>% 
  mutate(acumulado = cumsum(Valor))

rm(tabela_21, df_data21)

df <- df_19 %>% 
  merge(df_20, by = 'dm',
        all.x = TRUE) %>%
  merge(df_21, by = 'dm',
        all.x = TRUE) %>%
  merge(df_22, by = 'dm',
        all.x = TRUE) 
  

colnames(df) <- c('dm', 'data_2019', 'valor_2019','acumulado_2019',
                        'data_2020', 'valor_2020', 'acumulado_2020',
                        'data_2021', 'valor_2021','acumulado_2021',
                        'data_2022', 'valor_2022', 'acumulado_2022')

df <- df %>% 
  mutate(valor_2022 = coalesce(valor_2022, 0), 
         acumulado_2021 = coalesce(acumulado_2021, 0),
         variacao22_21 = ((acumulado_2022 - acumulado_2021)/
                            acumulado_2021)*100) %>% 
  arrange(data_2021) 

rm(df_19, df_20, df_21, df_22, df_data, df_data22, tabela_22)

setwd("./../")