library(flextable)
library(janitor)


df_mes <- df %>% 
  mutate(Ano = month(data_2019, label = TRUE, abbr = F)) %>% 
  group_by(Ano) %>% 
  summarise(a = base::sum(valor_2019),
            b = base::sum(valor_2020),
            c = base::sum(valor_2021),
            d = base::sum(valor_2022))%>%
  adorn_totals("row") 




df_mes <- df_mes%>% 
  
  mutate(variacao = ((d-c)/c)*100)

              

colnames(df_mes) <- c('Mês', '2019', '2020','2021','2022','Variação 22/21') 
df_mes[df_mes == 0] <- NA
df_mes[df_mes == -100] <- NA
options(knir.kable.NA = '')




set_flextable_defaults(
  font.size = 11, theme_fun = theme_vanilla,
  padding.right =1.5,
  #padding = 5,
  background.color = "#ffffff")




mytable1 <- df_mes %>%
  flextable() %>% 
  autofit() %>% 
  color( ~ `Variação 22/21` < 0, ~ `Variação 22/21`,  color = 'red' ) %>% 
  bold( ~ `Variação 22/21` < 0, ~ `Variação 22/21`,  bold = TRUE ) %>% 
  width(width = 1.8) %>% 
  height(height = 0.25) %>% 
  colformat_double(big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  colformat_num(j = c('2019', '2020','2021','2022'), digits =0,big.mark="." ) %>% 

  bold(i = 13, bold = TRUE, part = "body") %>% 
  bg(., i= ~ `Mês` == "Total", part = "body", bg = "#ececec") 



mytable1 
  
