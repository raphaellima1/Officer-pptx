IP <- 0.04
# carregar a tabela total chamada df
setwd('C:/Users/raphael.lima/Documents/Trabalhos/Trabalhos em R/1 - ICMS')
setwd("./Tabela/")

# rodar as tabela para totais
source('RECEITAS_2022.R')

# DESCOBRIR A ULTIMA ATUALIZAÇÃO
ult_atu <- max(receitas$data)
ult_atu1 <- format(ult_atu, "%d/%m/%y")
dia <- glue(('{year(ult_atu)}{month(ult_atu)}{day(ult_atu)}'))


# numero de paginas
p <- 1
# numero de seções
s <- 1
# Nº de graficos
g <- 1
# Nº de tabelas
t <- 1

############################ capa ##############################################

border <- fp_par(
  text.align = "center",
  shading.color = "#2d2e2d")



border1 <- fp_par(
  text.align = "left",
  shading.color = "#2d2e2d")


border2 <- fp_par(
  text.align = "justify",
  shading.color = "#2d2e2d")


# cria o pptx e pega o template

my <- read_pptx('blank.pptx') %>% 
  
  ph_with(block_list(fpar(ftext(glue("COMPORTAMENTO DIÁRIO DA RECEITAS TRIBUTÁRIAS"), 
                                prop = fp_text(font.size = 30, bold = TRUE,  
                                               color = "#2d2e2d"))
                          , fp_p = border)), 
          location = ph_location(left = 3, top = 2.36222, 
                                 width = 7.08,
                                 height = 1.18
          )) %>% 
  ph_with(block_list(fpar(ftext(glue("Ultima atualização - {ult_atu1}"), 
                                prop = fp_text(font.size = 20, 
                                               color = "#383838"))
                          , fp_p = border)), 
          location = ph_location(left = 5, top = 3.7, 
                                 width = 9,
                                 height = 0.8)) 

############################ tabela com a porcentagem de cada setor ############

setwd("./Flextable/")
source('table_setor_receitas.R')


my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  
  ph_with(block_list(fpar(ftext(glue('TABELA {s}.{t} - Participação percentual por atividade econômica.'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 6.5,
                                 height = 0.3)) %>%
  
  
  ph_with(table_setor, 
          location = ph_location(left = 0.9, top = 1)) %>% 
  
  ph_with(block_list(fpar(ftext(glue("Ultima atualização - {ult_atu1}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          
          location = ph_location(left = 0, 
                                 top = 7,88,
                                 width = 6.5,
                                 height = 0.3)) %>% 
  
  ph_with(block_list(fpar(ftext(glue("Slide {p}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          location = ph_location(left = 12,10, 
                                 top = 7, 
                                 width = 0.9,
                                 height = 0.3)) 


rm(table, table_setor)

################## 1 slide grafico acumulado  ##################################

# Entrar na pastas de graficos
setwd("./Gráficos/")
# rodar o grafico de linhas
source('geom_line.R')

p <- p+1

my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  
  ph_with(block_list(fpar(ftext(glue('GRÁFICO {s}.{g} - Comportamento da Arrecadação diária (Periodo acumulado).'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 8,
                                 height = 0.3)) %>%
  
  #figura 1
  ph_with(dml(code = plot(fig1)), 
          location = ph_location(left = 0.6, 
                                 top = 1, 
                                 width = 11.81,
                                 height = 5.5)) %>% 
  
  
  ph_with(tabela_dia, 
          location = ph_location(left = 2.6, 
                                 top = 6.5)) %>% 
  
  ph_with(block_list(fpar(ftext(glue("Ultima atualização - {ult_atu1}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          
          location = ph_location(left = 0, 
                                 top = 7,88,
                                 width = 6.5,
                                 height = 0.3)) %>% 
  
  ph_with(block_list(fpar(ftext(glue("Slide {p}"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          location = ph_location(left = 12,10, 
                                 top = 7, 
                                 width = 0.9,
                                 height = 0.3)) 

rm(df_dia, fig1, tabela_dia)

# 2 slide grafico var
########################## grafico variação ####################################
# Entrar na pastas de graficos
setwd("./Gráficos/")
source('geom_line_var.R')

p <- p+1  
g <- g+1
# Adicionar o slide
my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  # titulo
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  # Subtitulo  
  ph_with(block_list(fpar(ftext(glue('GRÁFICO {s}.{g} - Variação diária 2022/2021'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 6.5,
                                 height = 0.3)) %>%
  
  # Plot fig 2    
  ph_with(dml(code = plot(fig2)), 
          location = ph_location(left = 0.6, top = 1, 
                                 width = 11.811,
                                 height = 5.5)) %>% 
  
  
  # Nota de rodapé
  ph_with(block_list(fpar(ftext(glue("Última atualização - {ult_atu1}"), 
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



# 3 slide Grafico de barras      
########################## grafico barras ######################################
# Entrar na pastas de graficos
setwd("./Gráficos/")
source('geom bar.R')

p <- p+1
g <- g+1
my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  
  ph_with(block_list(fpar(ftext(glue('GRÁFICO {s}.{g} - Comportamento da Arrecadação (Periodo acumulado)'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 6.5,
                                 height = 0.3)) %>%
  
  
  # figura 3
  ph_with(dml(code = plot(fig3)), 
          location = ph_location(left = 0.6, top = 1, 
                                 width = 11.811,
                                 height = 5.5)) %>% 
  
  # Nota de rodapé
  ph_with(block_list(fpar(ftext(glue("Última atualização - {ult_atu1}"), 
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


################################################################################
setwd("./Flextable/")
source('tabela acumulada.R')

p <- p+1
t <- t+1
my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  
  ph_with(block_list(fpar(ftext(glue('TABELA {s}.{t} - Tabela acumulada em valores nominais R$'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 6.5,
                                 height = 0.3)) %>%
  
  # figura 4
  ph_with(mytable1, 
          location = ph_location(left = 0.9, top = 1.1)) %>% 
  
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


################################################################################
setwd("./Flextable/")
source('tabela real.R')

t <- t+1
p <- p+1

my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  
  ph_with(block_list(fpar(ftext(glue('TABELA {s}.{t} - Tabela acumulada em valores reais R$'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 6.5,
                                 height = 0.3)) %>%
  # Nota de rodapé
  ph_with(block_list(fpar(ftext(glue("¹Mês/Ano de referencia: Janeiro de 2019"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          
          location = ph_location(left = 0.9, 
                                 top = 6.1,
                                 width = 6.5,
                                 height = 0.3)) %>%
  
  # figura 4
  ph_with(tabela_real, 
          location = ph_location(left = 0.9, top = 1.1)) %>% 
  
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
################################################################################

setwd("./Flextable/")
source('tabela_variacao_receitas.R')


p <- p+1 
t <- t+1
my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  
  ph_with(block_list(fpar(ftext(glue('TABELA {s}.{t} - Comparativo entre o mês corrente, mês anterior e mês do ano anterior em valores nominais'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 10,
                                 height = 0.3)) %>%
  
  # figura 4
  ph_with(tabela_mes, 
          location = ph_location(left = 0.9, top = 1.1)) %>% 
  
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


################################################################################
setwd("./Flextable/")
source('cat_tab_real_receitas.R')


p <- p+1 
t <- t+1
my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
                                prop = fp_text(font.size = 18, 
                                               color = "#2d2e2d")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.15, top = 0.2, 
                                 width = 8,
                                 height = 0.59)) %>%
  
  ph_with(block_list(fpar(ftext(glue('TABELA {s}.{t} - Comparativo entre o mês corrente, mês anterior e mês do ano anterior em valores reais'), 
                                prop = fp_text(font.size = 14, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 0.25, top = 0.65, 
                                 width = 10,
                                 height = 0.3)) %>%
  # Nota de rodapé
  ph_with(block_list(fpar(ftext(glue("¹Mês/Ano de referencia: Janeiro de 2019"), 
                                prop = fp_text(font.size = 12, 
                                               color = "#B2B2B2")), 
                          fp_p = border1)),
          
          
          location = ph_location(left = 0.9, 
                                 top = 5.9,
                                 width = 6.5,
                                 height = 0.3)) %>%
  
  # figura 4
  ph_with(tabela_mes_r, 
          location = ph_location(left = 0.9, top = 1.1)) %>% 
  
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

################################################################################

setwd("./Gráficos/")
source('grafico_dia.R')


p <- p+1 
g <- g+1
my <- my %>%
  add_slide(layout = "Título e conteúdo", master = "Tema do Office") %>% 
  
  
  # Titulo e subtitulo
  ph_with(block_list(fpar(ftext('RECEITAS TRIBUTÁRIAS', 
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
  
  ph_with(dml(code = plot(grafico_dia)), 
          location = ph_location(left = 0.6, 
                                 top = 1, 
                                 width = 11.81,
                                 height = 5.5)) %>% 
  
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
###############################################################################

# são os graficos gerados em 4 plots

setwd("./Gráficos/")
source('plots_mensal_receitas.R')

################################################################################

my %>%
  print(target = glue("{dia} - RECEITAS TRIBUTÁRIAS.pptx"))%>% 
  browseURL()

