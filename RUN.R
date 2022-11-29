IP <-  0.004
#IP2 <- 0.0040

Sys.setlocale("LC_ALL", "pt_br.utf-8")

# esse script é dividido em 2 partes, a primeira que é o total do ICMS, e por 
# fim o loop para cada categoria 


pacman::p_load(
  tidyverse, readxl, lubridate, pdftools, rbcb,
  tsibble, fable, tidymodels, timetk, modeltime,
  workflowsets, stacks, vip, glmnet, kernlab, hts,
  tidyquant, glue, ggpubr,gridExtra,grid,lattice, 
  patchwork,ggthemes,officer, janitor,gt, flextable,
  kableExtra, magrittr,rvg, mschart, janitor)



# variavies para cada categoria
variaveis <- c('TOTAL', "COMBUSTÍVEL", "COMÉRCIO ATACADISTA E DISTRIBUIDOR",
               "COMÉRCIO VAREJISTA", 'COMUNICAÇÃO','ENERGIA ELÉTRICA',
               'EXTRATOR MINERAL OU FÓSSIL', 'INDÚSTRIA', 'OUTROS', 
               'PRESTAÇÃO DE SERVIÇO','PRODUÇÃO AGROPECUÁRIA')



setwd('C:/Users/raphael.lima/Documents/Trabalhos/Trabalhos em R/1 - ICMS')

setwd("./pptx/")

# ICMS total
source( encoding = 'UTF-8', file = 'ICMS_ACUMULADO.R')

setwd("./pptx/")
# ADICIONAL DE 2%
source( encoding = 'UTF-8', file = 'ADICIONAL_2%.R')

setwd("./pptx/")
# PROGETE
source( encoding = 'UTF-8', file = 'PROTEGE.R')

setwd("./pptx/")
# IPVA E ITCD
source( encoding = 'UTF-8', file = 'IPVA_ITCD.R')

setwd("./pptx/")
# IPVA E ITCD
source( encoding = 'UTF-8', file = 'RECEITA.R')