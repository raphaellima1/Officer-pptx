fig2 <- df %>%
  ggplot()+
  #2022
  geom_line(aes(x = data_2021, 
                y = variacao22_21, 
                color = "Variação 2022/2021", 
                linetype = "Variação 2022/2021"),
            size= 0.7)+
  
  labs(x = "  ",
       y = "Em porcentagem (%)",
       title = '')+
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  
  geom_ma(aes(x = data_2021, 
              y = variacao22_21, 
              color = "10 por Média Móvel",
              linetype = "10 por Média Móvel"), 
          ma_fun = EMA, 
          n = 10,
          size = 1.2)+
  scale_color_manual(breaks = c('Variação 2022/2021',"10 por Média Móvel"),
                     values = c('Variação 2022/2021' = "#e03a3e",
                                "10 por Média Móvel" = '#e03a3e'), name="Legenda:")+
  
  scale_linetype_manual(breaks = c('Variação 2022/2021',"10 por Média Móvel"),
                        values = c('Variação 2022/2021' = "dotted",
                                   "10 por Média Móvel" = 'solid'), name="Legenda:")+
  
  ylim(-30, 100)+
  
  theme_classic2()+
  theme(legend.position=c(0.9, 0.9),
        panel.grid.major = element_line(colour = "#e3e2e1", linetype = 18,size = 0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

fig2

setwd("./../")