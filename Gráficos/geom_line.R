
fig1 <- df %>%
  ggplot()+
  
  
  #2019
  geom_line(aes(x = data_2021, y = acumulado_2019, 
                color = "Acumulado 2019", 
                linetype = "Acumulado 2019"), 
            size=0.5) +
  #media movel 2019
  geom_ma(aes(x = data_2021, 
              y = acumulado_2019, 
              color = "Média Móvel (2019)", 
              linetype = "Média Móvel (2019)"), 
          ma_fun = EMA, 
          n = 10,
          size = 1)+  
  #2020
  geom_line(aes(x = data_2021, y = acumulado_2020, 
                color = "Acumulado 2020", 
                linetype = "Acumulado 2020"), 
            size=0.5) +
  #media movel 2020
  geom_ma(aes(x = data_2021, 
              y = acumulado_2020, 
              color = "Média Móvel (2020)", 
              linetype = "Média Móvel (2020)"), 
          ma_fun = EMA, 
          n = 10,
          size = 1)+
  
  #2021
  geom_line(aes(x = data_2021, 
                y = acumulado_2021, 
                color = "Acumulado 2021", 
                linetype = "Acumulado 2021"),
            size=0.5)+
  
  
  geom_ma(aes(x = data_2021, 
              y = acumulado_2021, 
              color = "Média Móvel (2021)",
              linetype = "Média Móvel (2021)"), 
          ma_fun = EMA, 
          n = 10,
          size = 1)+
  
  
  #2022
  geom_line(aes(x = data_2021, y = acumulado_2022, 
                color = "Acumulado 2022", 
                linetype = "Acumulado 2022"), 
            size=0.5) +
  
  #media movel 2022
  geom_ma(aes(x = data_2021, 
              y = acumulado_2022, 
              color = "Média Móvel (2022)", 
              linetype = "Média Móvel (2022)"), 
          ma_fun = EMA, 
          n = 10,
          size = 1)+
  
  
  
  labs(x = "  ", 
       y = " Valores em Reais (R$) ", 
       title = ' ',
                    linetype = "Variable",
                    color = "Variable")+
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  
  scale_color_manual(breaks = c('Acumulado 2019', "Média Móvel (2019)",
                                'Acumulado 2020', "Média Móvel (2020)",
                                'Acumulado 2021', "Média Móvel (2021)",
                                'Acumulado 2022', "Média Móvel (2022)"),
                     
                     values = c('Acumulado 2019' = '#e35959',
                                "Média Móvel (2019)" = "#e35959",
                                'Acumulado 2020' = '#5995e3',
                                "Média Móvel (2020)" = "#5995e3",
                                "Acumulado 2021"="#ffdc00",
                                "Média Móvel (2021)"="#ffdc00",
                                "Acumulado 2022" = "#1a9938", 
                                "Média Móvel (2022)" = "#1a9938"), 
                     name="Legenda:") +
  
  
  scale_linetype_manual(breaks = c('Acumulado 2019', "Média Móvel (2019)",
                                   'Acumulado 2020', "Média Móvel (2020)",
                                   'Acumulado 2021', "Média Móvel (2021)",
                                   'Acumulado 2022', "Média Móvel (2022)"),
                        
                        values = c('Acumulado 2019' = 'dotted',
                                   "Média Móvel (2019)" = 'solid',
                                   'Acumulado 2020' = 'dotted',
                                   "Média Móvel (2020)" = "solid",
                                   "Acumulado 2022" = "dotted", 
                                   "Média Móvel (2022)" = "solid", 
                                   "Acumulado 2021"="dotted",
                                   "Média Móvel (2021)"="solid"),
                        name="Legenda:") +
  
  labs(fill = "Title")+
  theme_classic2()+
  theme(legend.position = c(1.02, 0.05),
        legend.key.size = unit(0.5,'cm'),
        legend.text = element_text((size=20)),
        panel.grid.major = element_line(colour = "#e3e2e1", linetype = 18,size = 0.2),
        panel.grid.minor = element_line(colour = "#e3e2e1", linetype = 18,size = 0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        #legend.box.background = element_rect(),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(1,1,1,1))



################################################################################
# tabela do slide

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

setwd("./../")