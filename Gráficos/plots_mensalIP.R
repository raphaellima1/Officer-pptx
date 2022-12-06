
p <- p+1
g <- g+1
my <- my %>% 
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext(glue('IPVA'), 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59))%>%
  
  ph_with(block_list(fpar(ftext(glue('GRÁFICO {s}.{g} - Comparativo entre o mês corrente, mês anterior e mês do ano anterior'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 8,
                                 height = 0.3)) %>% 
  # Nota de rodapé
  ph_with(block_list(fpar(ftext(glue("Ultima atualização - {ult_atu1}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          
          location = ph_location(left = 0, 
                                 top = 7,88,
                                 width = 6.5,
                                 height = 0.3)) %>% 
  
  # numero de slide
  ph_with(block_list(fpar(ftext(glue("Slide {p}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          location = ph_location(left = 12,10, 
                                 top = 7, 
                                 width = 0.9,
                                 height = 0.3))


for (i in 1:4) {
  mes1 <- i
  
  
  
  if (i == 1) {
    
    mes_1 <- 12
    df_mes_ant <- df %>% 
      mutate(mes = month(data_2019))%>% 
      filter(mes == mes_1) %>%
      select(data_2021,valor_2021) %>% 
      rename(mes_passado = valor_2021)%>% 
      mutate(dm = day(data_2021)) %>% 
      drop_na()
    
  }else{
    mes_1 <- i-1
    ano_1 <- 2022
    
    df_mes_ant <- df %>% 
      mutate(mes = month(data_2019))%>% 
      filter(mes == mes_1) %>%
      select(data_2022,valor_2022) %>% 
      rename(mes_passado = valor_2022)%>% 
      mutate(dm = day(data_2022)) %>% 
      drop_na()
  }  
  
  
  df_data <-  data.frame(data =seq(as.Date("2022-01-01"),
                                   as.Date(today()),
                                   by = 1))  %>% 
    
    mutate(dm = paste(dia = day(data), 
                      mes = month(data), 
                      sep ='/'))
  
  ########################################################
  df_mesA <- df %>% 
    mutate(mes = month(data_2019))%>% 
    filter(mes == mes1) %>%
    select(data_2021, valor_2021) %>% 
    drop_na() %>% 
    mutate(acu_anoP = cumsum(valor_2021),
           dm = day(data_2021))
  
  df_mes <- df %>% 
    mutate(mes = month(data_2019))%>% 
    filter(mes == mes1) %>%
    select(data_2022, valor_2022) %>% 
    mutate(dm = day(data_2022)) %>% 
    merge(df_mes_ant, by = 'dm',all.y = T) %>% 
    merge(df_mesA, by = 'dm',all.x = T) %>%
    mutate(acu_mes = cumsum(valor_2022),
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
         title = glue('{month(i, label = TRUE, abbr = FALSE)}'),
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
  
  
  ###################################################################################
  
  if(i == 1){
    
    
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 0.5, 
                                     top = 1, 
                                     width = 6,88,
                                     height = 3,34))
    
  }else if (i == 2) {
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 7.1, 
                                     top = 1, 
                                     width = 6,88,
                                     height = 3,34))  
  }else if(i == 3){
    
    
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 0.5, 
                                     top = 4.0, 
                                     width = 6,88,
                                     height = 3,34))
    
  }else {
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 7.1, 
                                     top = 4.0, 
                                     width = 6,88,
                                     height = 3,34))  
  }
}

################################################################################
p <- p+1
my <- my %>% 
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext(glue('IPVA'), 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  
  ph_with(block_list(fpar(ftext(glue('GRÁFICO {s}.{g} - Comparativo entre o mês corrente, mês anterior e mês do ano anterior'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 8,
                                 height = 0.3)) %>% 
  # Nota de rodapé
  ph_with(block_list(fpar(ftext(glue("Ultima atualização - {ult_atu1}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          
          location = ph_location(left = 0, 
                                 top = 7,88,
                                 width = 6.5,
                                 height = 0.3)) %>% 
  
  # numero de slide
  ph_with(block_list(fpar(ftext(glue("Slide {p}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          location = ph_location(left = 12,10, 
                                 top = 7, 
                                 width = 0.9,
                                 height = 0.3))



for (i in 5:8) {
  mes1 <- i
  
  
  
  if (i == 1) {
    
    mes_1 <- 12
    df_mes_ant <- df %>% 
      mutate(mes = month(data_2019))%>% 
      filter(mes == mes_1) %>%
      select(data_2021,valor_2021) %>% 
      rename(mes_passado = valor_2021)%>% 
      mutate(dm = day(data_2021)) %>% 
      drop_na()
    
  }else{
    mes_1 <- i-1
    ano_1 <- 2022
    
    df_mes_ant <- df %>% 
      mutate(mes = month(data_2019))%>% 
      filter(mes == mes_1) %>%
      select(data_2022,valor_2022) %>% 
      rename(mes_passado = valor_2022)%>% 
      mutate(dm = day(data_2022)) %>% 
      drop_na()
  }  
  
  
  
  
  df_data <-  data.frame(data =seq(as.Date("2022-01-01"),
                                   as.Date(today()),
                                   by = 1))  %>% 
    
    mutate(dm = paste(dia = day(data), 
                      mes = month(data), 
                      sep ='/'))
  
  ########################################################
  df_mesA <- df %>% 
    mutate(mes = month(data_2019))%>% 
    filter(mes == mes1) %>%
    select(data_2021, valor_2021) %>% 
    drop_na() %>% 
    mutate(acu_anoP = cumsum(valor_2021),
           dm = day(data_2021))
  
  df_mes <- df %>% 
    mutate(mes = month(data_2019))%>% 
    filter(mes == mes1) %>%
    select(data_2022, valor_2022) %>% 
    mutate(dm = day(data_2022)) %>% 
    merge(df_mes_ant, by = 'dm',all.y = T) %>% 
    merge(df_mesA, by = 'dm',all.x = T) %>%
    mutate(acu_mes = cumsum(valor_2022),
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
         title = glue('{month(i, label = TRUE, abbr = FALSE)}'),
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
  
  
  ###################################################################################
  
  if(i == 5){
    
    
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 0.5, 
                                     top = 1, 
                                     width = 6,88,
                                     height = 3,34))
    
  }else if (i == 6) {
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 7.1, 
                                     top = 1, 
                                     width = 6,88,
                                     height = 3,34))  
  }else if(i == 7){
    
    
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 0.5, 
                                     top = 4.0, 
                                     width = 6,88,
                                     height = 3,34))
    
  }else {
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 7.1, 
                                     top = 4.0, 
                                     width = 6,88,
                                     height = 3,34))  
  }
}
###############################################################################
p <- p+1
my <- my %>% 
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext(glue('IPVA'), 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59))%>%
  
  ph_with(block_list(fpar(ftext(glue('GRÁFICO {s}.{g} - Comparativo entre o mês corrente, mês anterior e mês do ano anterior'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 8,
                                 height = 0.3)) %>% 
  # Nota de rodapé
  ph_with(block_list(fpar(ftext(glue("Ultima atualização - {ult_atu1}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          
          location = ph_location(left = 0, 
                                 top = 7,88,
                                 width = 6.5,
                                 height = 0.3)) %>% 
  
  # numero de slide
  ph_with(block_list(fpar(ftext(glue("Slide {p}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          location = ph_location(left = 12,10, 
                                 top = 7, 
                                 width = 0.9,
                                 height = 0.3))


for (i in 9:12) {
  #print(i)
  mes1 <- i
  
  
  
  if (i == 1) {
    
    mes_1 <- 12
    df_mes_ant <- df %>% 
      mutate(mes = month(data_2019))%>% 
      filter(mes == mes_1) %>%
      select(data_2021,valor_2021) %>% 
      rename(mes_passado = valor_2021)%>% 
      mutate(dm = day(data_2021)) %>% 
      drop_na()
    
  }else{
    mes_1 <- i-1
    ano_1 <- 2022
    
    df_mes_ant <- df %>% 
      mutate(mes = month(data_2019))%>% 
      filter(mes == mes_1) %>%
      select(data_2022,valor_2022) %>% 
      rename(mes_passado = valor_2022)%>% 
      mutate(dm = day(data_2022)) %>% 
      drop_na()
  }  
  
  
  
  
  df_data <-  data.frame(data =seq(as.Date("2022-01-01"),
                                   as.Date(today()),
                                   by = 1))  %>% 
    
    mutate(dm = paste(dia = day(data), 
                      mes = month(data), 
                      sep ='/'))
  
  ########################################################
  df_mesA <- df %>% 
    mutate(mes = month(data_2019))%>% 
    filter(mes == mes1) %>%
    select(data_2021, valor_2021) %>% 
    drop_na() %>% 
    mutate(acu_anoP = cumsum(valor_2021),
           dm = day(data_2021))
  
  df_mes <- df %>% 
    mutate(mes = month(data_2019))%>% 
    filter(mes == mes1) %>%
    select(data_2022, valor_2022) %>% 
    mutate(dm = day(data_2022)) %>% 
    merge(df_mes_ant, by = 'dm',all.y = T) %>% 
    merge(df_mesA, by = 'dm',all.x = T) %>%
    mutate(acu_mes = cumsum(valor_2022),
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
         title = glue('{month(i, label = TRUE, abbr = FALSE)}'),
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
  
  
  ###################################################################################
  
  if(i == 9){
    
    
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 0.5, 
                                     top = 1, 
                                     width = 6,88,
                                     height = 3,34))
    
  }else if (i == 10) {
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 7.1, 
                                     top = 1, 
                                     width = 6,88,
                                     height = 3,34))  
  }else if(i == 11){
    
    
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 0.5, 
                                     top = 4.0, 
                                     width = 6,88,
                                     height = 3,34))
    
  }else {
    #figura 1
    my <- my %>%
      ph_with(dml(code = plot(grafico_dia)), 
              location = ph_location(left = 7.1, 
                                     top = 4.0, 
                                     width = 6,88,
                                     height = 3,34))  
  }
}


# sair da pasta
setwd("./../")
