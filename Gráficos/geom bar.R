
#2019
df1 <- df %>% 
  dplyr::select(data_2019, valor_2019)

colnames(df1) <- c('data', 'Valor')

df2 <- df %>% 
  dplyr::select(data_2020, valor_2020)
colnames(df2) <- c('data', 'Valor')


df3 <- df %>% 
  dplyr::select(data_2021, valor_2021)

colnames(df3) <- c('data', 'Valor')

df4 <- df %>% 
  dplyr::select(data_2022, valor_2022)
colnames(df4) <- c('data', 'Valor')


month <- c('jan', 'fev','mar','abr','mai','jun','jul','ago','set','out','nov','dez')


dff <- df1 %>% 
  rbind(df2,df3,df4) %>% 
  mutate(mouth = month(data, label = TRUE), year = year(data)) %>% 
  group_by(mouth,year) %>% 
  summarise(valor = sum(Valor)) %>% 
  mutate(year = as.character(year),mouth = as.character(mouth)) %>% 
  drop_na()

rm(df1 ,df2, df3, df4)

fig3 <- ggplot() + 
  
  geom_bar(data = dff,aes(x=mouth, y=valor, group = year, fill = year),
           stat = "identity", position = position_dodge(preserve = "single"), 
           width =  0.7)+
  
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  
  scale_x_discrete(limits = month)+
  
  scale_fill_manual("legenda:", values = c("2019" = "#e35959", 
                                           "2020" =  "#5995e3",
                                           "2021" = "#ffdc00", 
                                           "2022" =  "#1a9938"))+
  
  labs(x = "", 
       y = "Valores em Reais (R$)", 
       title = "",
                    linetype = "Variable",
                    color = "Variable")+
  
  theme_classic2()+
  
  theme(legend.position="right")+
  theme(panel.grid.major = element_line(colour = "#e3e2e1", linetype = 18,size = 0.2),
        panel.grid.minor = element_line(colour = "#e3e2e1", linetype = 18,size = 0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
fig3
         
setwd("./../")