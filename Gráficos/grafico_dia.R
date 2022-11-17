

# variaveis para 

data <- ult_atu



mes1 <- month(data)

mes_1 <- mes1 -1



ano <- year(data)
ano_1 <- year(data) - 1



ano
df_data <-  data.frame(data =seq(as.Date("2022-01-01"),
                                as.Date(today()),
                                by = 1))  %>% 
  
  mutate(dm = paste(dia = day(data), 
                    mes = month(data), 
                    sep ='/'))


df_mes_ant <- df %>% 
  mutate(mes = month(data_2019))%>% 
  filter(mes == mes_1) %>%
  select(data_2021,valor_2022) %>% 
  rename(mes_passado = valor_2022)%>% 
  mutate(dm = day(data_2021)) %>% 
  drop_na()

  
df_mesA <- df %>% 
    mutate(mes = month(data_2019))%>% 
    filter(mes == mes1) %>%
    select(data_2022, valor_2022) %>% 
    drop_na() %>% 
    mutate(acu_mes = cumsum(valor_2022),
           dm = day(data_2022))

df_mes <- df %>% 
  mutate(mes = month(data_2019))%>% 
  filter(mes == mes1) %>%
  select(data_2021, valor_2021) %>% 
  mutate(dm = day(data_2021)) %>% 
  merge(df_mes_ant, by = 'dm',all.y = T) %>% 
  merge(df_mesA, by = 'dm',all.x = T) %>%
  mutate(acu_anoP = cumsum(valor_2021),
         acu_mesP = cumsum(mes_passado)) %>% 
  ungroup() %>% 
  select(dm, acu_mes,acu_mesP,acu_anoP)



grafico_dia <- df_mes %>% 
  ggplot() +
  geom_line(aes(x = dm, y = acu_mes, 
                color = 'Mês corrente', 
                linetype = 'Mês corrente'),size = 1)+
  
  geom_line(aes(x = dm, y = acu_mesP, 
                color = 'Mês anterior', 
                linetype = 'Mês anterior'),size = 1)+
  
  geom_line(aes(x = dm, y = acu_anoP, 
                color = 'Ano anterior', 
                linetype = 'Ano anterior'),size = 1)+
  
  labs(x = "Dias do mês", 
       y = " Valores em Reais (R$) ", 
       title = ' ',
       linetype = "Legenda:",
       color = "Legenda:")+
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  
  scale_color_manual(values = c('#e35959','#ffdc00','#1a9938'))+
  
  scale_linetype_manual(values = c('solid','solid','solid'))+
  theme_classic2()+
  theme(legend.key.size = unit(0.5,'cm'),
        legend.text = element_text((size=20)),
        panel.grid.major = element_line(colour = "#e3e2e1", linetype = 18,size = 0.2),
        panel.grid.minor = element_line(colour = "#e3e2e1", linetype = 18,size = 0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        #legend.box.background = element_rect(),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(1,1,1,1))


setwd("./../")

