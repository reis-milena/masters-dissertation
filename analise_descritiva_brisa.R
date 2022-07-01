#######################################################
# Dissertacao Mestrado - Milena Villela
# Ano: 2021
# Objetivo do codigo: elaborar graficos e tabelas
# para analise descritiva
# Base de dados: Coorte Brisa 2010
#######################################################

#---- Pacotes ----

# library(dplyr)
# library(tidyverse)
library(ggplot2)
library(stargazer)
library(gridExtra)
library(kableExtra)

#---- Lendo a base ----
#-- Ribeirao Preto ----
# source("C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Programas R/brisa_filtrada.R")
source("G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/brisa_filtrada.R")

brisa_analise_descritiva = brisa_filtrada %>%
  filter(idade_mae_seguim >= 18,
         !is.na(raca_negra))  %>% 
  select(ocupada_seguim, ocupada_prentl,
         ocupada_formal_seguim, casada_marido_prentl,
         raca_negra, densid_morador_dormitor_prentl ,educacao_mae,
         casada_marido_seguimento, idade_mae_seguim, idade_mae_seguim_quadrado,
         ocupada_formal_prentl, vinculo_mae_bebe_prejudicada, depressao_epds,
         quantos_filhos_tem_nascim, idade_bebe_seguim, 
         faz_todo_trab_domestico, filho_creche, bayley_risco) %>%
  # para todos os modelos terem o mesmo N
  drop_na() #TEM QUE TER N=889  

#-- Sao Luis ----

# source("C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Programas R/brisa_filtrada_sl.R")
source("G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/brisa_filtrada_sl.R")

brisa_filtrada_sl = brisa_filtrada_sl %>%
  filter(idade_mae_seguim >= 18, 
         !is.na(raca_negra) )

brisa_analise_descritiva_sl = brisa_filtrada_sl %>% 
  select(ocupada_seguim, ocupada_prentl, 
         ocupada_formal_seguim, casada_marido_prentl,
         raca_negra, densid_morador_dormitor_prentl ,educacao_mae,
         casada_marido_seguimento, idade_mae_seguim,
         ocupada_formal_prentl, depressao_epds,
         quantos_filhos_tem_nascim, idade_bebe_seguim,
         faz_todo_trab_domestico, filho_creche,
         bayley_risco
         # bebe_planejado, 
         # educacao_companheiro, dummy_educacao_companheiro
  ) %>%
  # para todos os modelos terem o mesmo N
  drop_na() #TEM QUE TER N=856

#---- Transformando variaveis ----
#-- Ribeirao Preto----
# table(brisa_analise_descritiva$ocupada_formal_seguim, exclude = NULL)
brisa_analise_descritiva$ocupada_formal_seguim = ifelse(brisa_analise_descritiva$ocupada_formal_seguim==0,1,
                                                ifelse(brisa_analise_descritiva$ocupada_formal_seguim ==1,2,
                                                       3))
brisa_analise_descritiva$ocupada_seguim = factor(brisa_analise_descritiva$ocupada_seguim,
                                                levels = c(0,1),
                                                labels = c("Not working", "Working"))
brisa_analise_descritiva$ocupada_prentl = factor(brisa_analise_descritiva$ocupada_prentl,
                                         levels = c(0,1),
                                         labels = c("Not working", "Working"))
brisa_analise_descritiva$ocupada_formal_seguim = factor(brisa_analise_descritiva$ocupada_formal_seguim,
                                                levels = c(1,2,3),
                                                labels = c("Not working", "Formal working",
                                                           "Informal working"))
brisa_analise_descritiva$ocupada_formal_prentl = factor(brisa_analise_descritiva$ocupada_formal_prentl,
                                                levels = c(0,1,2),
                                                labels = c("Not working", "Formal working",
                                                           "Informal working"))
brisa_analise_descritiva$raca_negra = factor(brisa_analise_descritiva$raca_negra,
                                     levels = c(0,1),
                                     labels = c("Non-black", "Black"))

brisa_analise_descritiva$educacao_mae = factor(brisa_analise_descritiva$educacao_mae,
                                       levels = c(1,2,3),
                                       labels = c("Illiterate/elementary/middle school",
                                                  "High school",
                                                  "Undergraduate compl./incompl."))

# brisa_analise_descritiva$ipv_gravidez_recorrente = factor(brisa_analise_descritiva$ipv_gravidez_recorrente,
#                                                   levels = c(0,1,2),
#                                                   labels = c("No violence", "Once",
#                                                              "Recurrent"))

# brisa_analise_descritiva$viol_gravidez_domest_ex = factor(brisa_analise_descritiva$viol_gravidez_domest_ex,
#                                                   levels = c(0,1),
#                                                   labels = c("No violence", "Violence"))
# 
# brisa_analise_descritiva$hospital_particular = factor(brisa_analise_descritiva$hospital_particular,
#                                               levels = c(0,1),
#                                               labels = c("Public", "Private"))

brisa_analise_descritiva$filho_creche = factor(brisa_analise_descritiva$filho_creche,
                                       levels = c(0,1),
                                       labels = c("Not on daycare", "On daycare"))

# brisa_analise_descritiva$faz_todo_trab_domestico = factor(brisa_analise_descritiva$faz_todo_trab_domestico,
#                                                   levels = c(0,1,2),
#                                                   labels = c("Someone else does the housework",
#                                                              "Share the housework", 
#                                                              "Does all the housework"))
# brisa_analise_descritiva$faz_todo_trab_domestico = ifelse(
#   brisa_analise_descritiva$faz_todo_trab_domestico == 0,
#   NA_real_,
#   brisa_analise_descritiva$faz_todo_trab_domestico)
brisa_analise_descritiva$faz_todo_trab_domestico = factor(brisa_analise_descritiva$faz_todo_trab_domestico,
                                                          levels = c(0,1),
                                                          labels = c("Share the housework",
                                                                     "Does all the housework"))

brisa_analise_descritiva$casada_marido_seguimento = factor(brisa_analise_descritiva$casada_marido_seguimento,
                                                   levels = c(0,1),
                                                   labels = c("Not married", 
                                                              "Married after pregn."))
brisa_analise_descritiva$casada_marido_prentl = factor(brisa_analise_descritiva$casada_marido_prentl,
                                               levels = c(0,1),
                                               labels = c("Not married", 
                                                          "Married during pregn."))
# 
# brisa_analise_descritiva$vinculo_mae_bebe_prejudicada = factor(brisa_analise_descritiva$vinculo_mae_bebe_prejudicada,
#                                                        levels = c(0,1),
#                                                        labels = c("No broken bond with baby", 
#                                                                   "Broken bond with baby"))

# brisa_analise_descritiva$suporte_social_cuidados = factor(brisa_analise_descritiva$suporte_social_cuidados,
#                                                   levels = c(0,1),
#                                                   labels = c("None or rare social support", 
#                                                              
#                                                              "Have social support"))
# brisa_analise_descritiva$saude_crianca = factor(brisa_analise_descritiva$saude_crianca,
#                                         levels = c(0,1),
#                                         labels = c("Great/excellent baby's health", 
#                                                    "Bad/regular baby's health"))

brisa_analise_descritiva$depressao_epds = factor(brisa_analise_descritiva$depressao_epds,
                                         levels = c(0,1),
                                         labels = c("No postpartum depression", 
                                                    "Postpartum depression"))

brisa_analise_descritiva$bayley_risco = factor(brisa_analise_descritiva$bayley_risco,
                                       levels = c(0,1),
                                       labels = c("No risk in any domain", 
                                                  "At least one domain at risk"))

# brisa_analise_descritiva$dominio_cognitivo = factor(brisa_analise_descritiva$dominio_cognitivo,
#                                             levels = c(0,1),
#                                             labels = c("No risk at cognitive domain", 
#                                                        "Cognitive domain at risk"))
# 
# brisa_analise_descritiva$dominio_comunic_receptiva = factor(brisa_analise_descritiva$dominio_comunic_receptiva,
#                                                     levels = c(0,1),
#                                                     labels = c("No risk at receptive communic. domain", 
#                                                                "Receptive communic. domain at risk"))
# 
# brisa_analise_descritiva$dominio_comunic_expressiva = factor(brisa_analise_descritiva$dominio_comunic_expressiva,
#                                                      levels = c(0,1),
#                                                      labels = c("No risk at expressive communic. domain", 
#                                                                 "Expressive communic. domain at risk"))
# 
# brisa_analise_descritiva$dominio_motor_fino = factor(brisa_analise_descritiva$dominio_motor_fino,
#                                              levels = c(0,1),
#                                              labels = c("No risk at fine motor domain", 
#                                                         "Fine motor domain at risk"))
# 
# brisa_analise_descritiva$dominio_motor_grosso = factor(brisa_analise_descritiva$dominio_motor_grosso,
#                                                levels = c(0,1),
#                                                labels = c("No risk at gross motor domain", 
#                                                           "Gross motor domain at risk"))
# 
# brisa_analise_descritiva$bebe_planejado = factor(brisa_analise_descritiva$bebe_planejado,
#                                          levels = c(0,1),
#                                          labels = c("Not planned", 
#                                                     "Planned"))

brisa_analise_descritiva = brisa_analise_descritiva %>%
  mutate(fx_etaria_bebe = case_when(
    idade_bebe_seguim >= 12 & idade_bebe_seguim <= 14 ~ "12 a 14 meses",
    idade_bebe_seguim >  14 & idade_bebe_seguim <= 16 ~ "14 a 16 meses",
    idade_bebe_seguim >  16 & idade_bebe_seguim <= 18 ~ "16 a 18 meses",
    idade_bebe_seguim >  18 & idade_bebe_seguim <= 20 ~ "18 a 20 meses",
    idade_bebe_seguim >  20 & idade_bebe_seguim <= 22 ~ "20 a 22 meses",
    idade_bebe_seguim >  22 & idade_bebe_seguim <= 24 ~ "22 a 24 meses",
    idade_bebe_seguim >  24 & idade_bebe_seguim <= 26 ~ "24 a 26 meses",
    idade_bebe_seguim >  26 & idade_bebe_seguim <= 28 ~ "26 a 28 meses",
    idade_bebe_seguim >  28 & idade_bebe_seguim <= 30 ~ "28 a 30 meses",
    idade_bebe_seguim >  30 & idade_bebe_seguim <= 32 ~ "30 a 32 meses",
    idade_bebe_seguim >  32 & idade_bebe_seguim <= 34 ~ "32 a 34 meses",
    idade_bebe_seguim >  34 & idade_bebe_seguim <= 36 ~ "34 a 36 meses"
  ))

#-- Sao Luis ----
# table(brisa_estimacoes_sl$ocupada_formal_seguim, exclude = NULL)
# brisa_estimacoes_sl$ocupada_formal_seguim = ifelse(brisa_estimacoes_sl$ocupada_formal_seguim==0,1,
#                                                 ifelse(brisa_estimacoes_sl$ocupada_formal_seguim ==1,2,
#                                                        3))
brisa_analise_descritiva_sl$ocupada_formal_seguim = factor(brisa_analise_descritiva_sl$ocupada_formal_seguim,
                                                   levels = c(0,1,2),
                                                   labels = c("Not working", "Formal working",
                                                              "Informal working"))
brisa_analise_descritiva_sl$ocupada_formal_prentl = factor(brisa_analise_descritiva_sl$ocupada_formal_prentl,
                                                   levels = c(0,1,2),
                                                   labels = c("Not working", "Formal working",
                                                              "Informal working"))
brisa_analise_descritiva_sl$raca_negra = factor(brisa_analise_descritiva_sl$raca_negra,
                                        levels = c(0,1),
                                        labels = c("Non-black", "Black"))

brisa_analise_descritiva_sl$educacao_mae = factor(brisa_analise_descritiva_sl$educacao_mae,
                                          levels = c(1,2,3),
                                          labels = c("Illiterate/elementary/middle school",
                                                     "High school",
                                                     "Undergraduate compl./incompl."))

brisa_analise_descritiva_sl$filho_creche = factor(brisa_analise_descritiva_sl$filho_creche,
                                          levels = c(0,1),
                                          labels = c("Not on daycare", "On daycare"))

# brisa_analise_descritiva_sl$faz_todo_trab_domestico = factor(brisa_analise_descritiva_sl$faz_todo_trab_domestico,
#                                                      levels = c(0,1,2),
#                                                      labels = c("Someone else does the housework",
#                                                                 "Share the housework", 
#                                                                 "Does all the housework"))
brisa_analise_descritiva_sl$faz_todo_trab_domestico = factor(brisa_analise_descritiva_sl$faz_todo_trab_domestico,
                                                  levels = c(0,1),
                                                  labels = c("Share the housework",
                                                             "Does all the housework"))

brisa_analise_descritiva_sl$casada_marido_seguimento = factor(brisa_analise_descritiva_sl$casada_marido_seguimento,
                                                      levels = c(0,1),
                                                      labels = c("Not married", 
                                                                 "Married after pregn."))
brisa_analise_descritiva_sl$casada_marido_prentl = factor(brisa_analise_descritiva_sl$casada_marido_prentl,
                                                  levels = c(0,1),
                                                  labels = c("Not married", 
                                                             "Married after pregn."))

brisa_analise_descritiva_sl$depressao_epds = factor(brisa_analise_descritiva_sl$depressao_epds,
                                            levels = c(0,1),
                                            labels = c("No postpartum depression", 
                                                       "Postpartum depression"))

brisa_analise_descritiva_sl$bayley_risco = factor(brisa_analise_descritiva_sl$bayley_risco,
                                          levels = c(0,1),
                                          labels = c("No risk in any domain", 
                                                     "At least one domain at risk"))

brisa_analise_descritiva_sl = brisa_analise_descritiva_sl %>%
  mutate(fx_etaria_bebe = case_when(
    idade_bebe_seguim >= 12 & idade_bebe_seguim <= 14 ~ "12 a 14 meses",
    idade_bebe_seguim >  14 & idade_bebe_seguim <= 16 ~ "14 a 16 meses",
    idade_bebe_seguim >  16 & idade_bebe_seguim <= 18 ~ "16 a 18 meses",
    idade_bebe_seguim >  18 & idade_bebe_seguim <= 20 ~ "18 a 20 meses",
    idade_bebe_seguim >  20 & idade_bebe_seguim <= 22 ~ "20 a 22 meses",
    idade_bebe_seguim >  22 & idade_bebe_seguim <= 24 ~ "22 a 24 meses",
    idade_bebe_seguim >  24 & idade_bebe_seguim <= 26 ~ "24 a 26 meses",
    idade_bebe_seguim >  26 & idade_bebe_seguim <= 28 ~ "26 a 28 meses",
    idade_bebe_seguim >  28 & idade_bebe_seguim <= 30 ~ "28 a 30 meses",
    idade_bebe_seguim >  30 & idade_bebe_seguim <= 32 ~ "30 a 32 meses",
    idade_bebe_seguim >  32 & idade_bebe_seguim <= 34 ~ "32 a 34 meses",
    idade_bebe_seguim >  34 & idade_bebe_seguim <= 36 ~ "34 a 36 meses"
  ))

#----1. Tabelas ----

## Ocupacao ####
table(brisa_analise_descritiva$ocupada_prentl)
table(brisa_analise_descritiva$ocupada_prentl) %>% prop.table() %>% round(.,4)*100

table(brisa_analise_descritiva$ocupada_seguim)
table(brisa_analise_descritiva$ocupada_seguim) %>% prop.table() %>% round(.,4)*100

tabela_ocupacao = cbind(table(brisa_analise_descritiva$ocupada_prentl) %>% prop.table() %>% round(.,4)*100,
                        table(brisa_analise_descritiva$ocupada_seguim) %>% prop.table() %>% round(.,4)*100)
kable(tabela_ocupacao, "latex")
rm(tabela_ocupacao)

table(brisa_analise_descritiva$ocupada_prentl, 
      brisa_analise_descritiva$ocupada_seguim, exclude = NULL)
tabela_ocupacao_transicao = table(brisa_analise_descritiva$ocupada_prentl, 
                                  brisa_analise_descritiva$ocupada_seguim) %>% 
  prop.table() %>% round(.,4)*100

kable(tabela_ocupacao_transicao, "latex")
rm(tabela_ocupacao)

## Ocupacao raca####
table(brisa_analise_descritiva$ocupada_prentl, brisa_analise_descritiva$raca_negra)
table(brisa_analise_descritiva$ocupada_prentl, 
      brisa_analise_descritiva$raca_negra) %>% prop.table(2) %>% round(.,4)*100

table(brisa_analise_descritiva$ocupada_seguim, brisa_analise_descritiva$raca_negra)
table(brisa_analise_descritiva$ocupada_seguim,
      brisa_analise_descritiva$raca_negra) %>% prop.table(2) %>% round(.,4)*100

tabela_ocupacao = cbind(table(brisa_analise_descritiva$ocupada_prentl) %>% prop.table() %>% round(.,4)*100,
                        table(brisa_analise_descritiva$ocupada_seguim) %>% prop.table() %>% round(.,4)*100)
kable(tabela_ocupacao, "latex")
rm(tabela_ocupacao)

table(brisa_analise_descritiva$ocupada_prentl, 
      brisa_analise_descritiva$ocupada_seguim, exclude = NULL)
tabela_ocupacao_transicao = table(brisa_analise_descritiva$ocupada_prentl, 
                                  brisa_analise_descritiva$ocupada_seguim) %>% 
  prop.table() %>% round(.,4)*100

kable(tabela_ocupacao_transicao, "latex")
rm(tabela_ocupacao)

## Ocupacao formal ####
table(brisa_analise_descritiva$ocupada_formal_prentl)
table(brisa_analise_descritiva$ocupada_formal_prentl) %>% prop.table() %>% round(.,4)*100

table(brisa_analise_descritiva$ocupada_formal_seguim)
table(brisa_analise_descritiva$ocupada_formal_seguim) %>% prop.table() %>% round(.,4)*100

table(brisa_analise_descritiva$ocupada_formal_prentl, 
      brisa_analise_descritiva$ocupada_formal_seguim)
tabela_formal_transicao = table(brisa_analise_descritiva$ocupada_formal_prentl, 
                                  brisa_analise_descritiva$ocupada_formal_seguim) %>% 
  prop.table() %>% round(.,4)*100

kable(tabela_formal_transicao, "latex")
rm(tabela_formal_transicao)

## Ocupacao formal raca ####

#feito no stata!!!

## Outros aspectos ####

table_data = brisa_analise_descritiva %>% 
  select(depressao_epds,bayley_risco, 
         faz_todo_trab_domestico, raca_negra, filho_creche)

vars <- as.matrix(expand.grid(c("depressao_epds","bayley_risco", 
                                "faz_todo_trab_domestico","filho_creche"), c("raca_negra")))
tables <- lapply(seq(nrow(vars)), 
                 function(x) round(prop.table(table(table_data[, vars[x, ]]),2)*100,2) )
lbls <- apply(vars, 1, paste, collapse="_")
# names(tables) <- lbls

table_descr = rbind(tables[[1]] ,
                    tables[[2]] ,
                    tables[[3]] ,
                    tables[[4]]) %>% 
  as.data.frame() %>%
  setNames(c("Non-black (%)","Black (%)")) 
# %>%
#   mutate(Variavel= c("No postpartum depression","Postpartum depression",
#                "No risk in any domain",   "At least one domain at risk",
#                "Share the housework",     "Does all the housework"))
# table_descr = table_descr[,c(3,1,2)]

kable(table_descr, "latex")
rm(vars, tables, table_data, table_descr, lbls)
#----2. Graficos----

#-Educacao----

graph_data = brisa_analise_descritiva %>% 
  select(educacao_mae,raca_negra) %>%
  filter(!is.na(educacao_mae)) %>%
  group_by(raca_negra) %>%
  count(educacao_mae) %>%
  mutate(percent = round(prop.table(n)*100,1),
         # raca_negra = factor(raca_negra, 
         #                     levels = c(0,1), 
         #                     c("Non-black", "Black")),
         # educacao_mae = factor(educacao_mae,
         #                       levels = c(1,2,3),
         #                       labels = c("Illiterate/elementary/middle school",
         #                                  "High school",
         #                                  "Undergraduate compl./incompl."))
  )
ggplot(graph_data, aes(fill = educacao_mae, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 6
  )   + 
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Education") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 14),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow=2, byrow=TRUE))

ggsave(
  filename = "education_race_sl.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Dissertacao/Graficos/Sao Luis/")

#-Casada----
graph_data = brisa_analise_descritiva %>% 
  select(casada_marido_seguimento,raca_negra) %>%
  filter(!is.na(casada_marido_seguimento)) %>%
  group_by(raca_negra) %>%
  count(casada_marido_seguimento) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         casada_marido_seguimento = factor(casada_marido_seguimento,
                               levels = c(0,1),
                               c("Not married",
                                 "Married"))
  )

ggplot(graph_data, aes(fill = casada_marido_seguimento, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 5
  )   + 
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Marital status") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 

ggsave(
  filename = "married_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")


#-IPV----
graph_data = brisa_analise_descritiva %>% 
  select(ipv_gravidez_recorrente,raca_negra) %>%
  filter(!is.na(ipv_gravidez_recorrente)) %>%
  group_by(raca_negra) %>%
  count(ipv_gravidez_recorrente) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         ipv_gravidez_recorrente = factor(ipv_gravidez_recorrente,
                               levels = c(0,1,2),
                               c("No violence",
                                 "Once",
                                 "Frequent" ))
  )

ggplot(graph_data, aes(fill = ipv_gravidez_recorrente, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity")  +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.8,
    label = levels(graph_data$raca_negra), size = 6
  )   + 
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "IPV during gestation") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 15),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 
+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))


ggsave(
  filename = "ipv_frequent_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#No frequent

graph_data = brisa_analise_descritiva %>% 
  select(viol_gravidez_domest_ex,raca_negra) %>%
  filter(!is.na(viol_gravidez_domest_ex)) %>%
  group_by(raca_negra) %>%
  count(viol_gravidez_domest_ex) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         viol_gravidez_domest_ex = factor(viol_gravidez_domest_ex,
                                          levels = c(0,1),
                                          c("No violence",
                                            "Violence" ))
  )

ggplot(graph_data, aes(fill = viol_gravidez_domest_ex, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity")  +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.9,
    label = levels(graph_data$raca_negra), size = 5
  )   + 
  # n
  geom_text(
    aes(label = format(n)),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = 1.2
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "IPV during gestation") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 


ggsave(
  filename = "ipv_dummy_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#-Ocupacao----

graph_data = brisa_analise_descritiva %>% 
  select(ocupada_prentl ,raca_negra) %>%
  filter(!is.na(ocupada_prentl) ) %>%
  group_by(raca_negra) %>%
  count(ocupada_prentl) %>%
  mutate(percent = round(prop.table(n)*100,1),
         # raca_negra = factor(raca_negra, 
         #                     levels = c(0,1), 
         #                     c("Non-black", "Black")),
         # ocupada_prentl = factor(ocupada_prentl,
         #                         levels = c(0,1),
         #                         c("Not working",
         #                           "Working")),
         quando = 0

  ) %>%
  setNames(c("raca_negra","ocupada","n","percent","quando"))

g1 <- ggplot(graph_data,
             aes(fill = raca_negra, 
                 y = percent, 
                 x = ocupada)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity")+
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # )  +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Race") +
  ylim(0, 65) +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 14),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(hjust=0.5)
  )+
  ggtitle("Pregnancy")

graph_data2 = brisa_analise_descritiva %>% 
  select(ocupada_seguim ,raca_negra) %>%
  filter(!is.na(ocupada_seguim) ) %>%
  group_by(raca_negra) %>%
  count(ocupada_seguim) %>%
  mutate(percent = round(prop.table(n)*100,1),
         # raca_negra = factor(raca_negra, 
         #                     levels = c(0,1), 
         #                     c("Non-black", "Black")),
         # ocupada_seguim = factor(ocupada_seguim,
         #                         levels = c(0,1),
         #                         c("Not working",
         #                           "Working")),
         quando = 1
         
  )%>%
  setNames(c("raca_negra","ocupada","n","percent","quando"))

g2 <- ggplot(graph_data2,
             aes(fill = raca_negra, 
                 y = percent, 
                 x = ocupada)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity")+
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # )  +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Race") +
  ylim(0, 65) +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line    = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust=0.5)
  )+
  ggtitle("After birth")

grid.arrange(g1,g2,ncol=2)

ggsave(
  filename = "ocup_periodo_race_sl.png",
  plot  = grid.arrange(g1,g2,ncol=2),
  width = 8, height = 6, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Dissertacao/Graficos/Sao Luis/")
rm(graph_data2,g1,g2)

#formal
graph_data = brisa_analise_descritiva %>% 
  select(ocupada_formal_prentl ,raca_negra) %>%
  filter(!is.na(ocupada_formal_prentl),
         ocupada_formal_prentl != "Not working") %>%
  group_by(raca_negra) %>%
  count(ocupada_formal_prentl) %>%
  mutate(percent = round(prop.table(n)*100,1),
         # raca_negra = factor(raca_negra, 
         #                     levels = c(0,1), 
         #                     c("Non-black", "Black")),
         # ocupada_formal_prentl = factor(ocupada_formal_prentl,
         #                         levels = c(1,2),
         #                         c(
         #                           "Formal",
         #                           "Informal")),
         quando = 0
         
  ) %>%
  setNames(c("raca_negra","ocupada","n","percent","quando"))

g1 <- ggplot(graph_data,
             aes(fill = raca_negra, 
                 y = percent, 
                 x = ocupada)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity")+
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # )  +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Race") +
  ylim(0,80) + 
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 14),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(hjust=0.5)
  )+
  ggtitle("Pregnancy")

graph_data2 = brisa_analise_descritiva %>% 
  select(ocupada_formal_seguim ,raca_negra) %>%
  filter(!is.na(ocupada_formal_seguim),
         ocupada_formal_seguim != "Not working" ) %>%
  group_by(raca_negra) %>%
  count(ocupada_formal_seguim) %>%
  mutate(percent = round(prop.table(n)*100,1),
         # raca_negra = factor(raca_negra, 
         #                     levels = c(0,1), 
         #                     c("Non-black", "Black")),
         # ocupada_formal_seguim = factor(ocupada_formal_seguim,
         #                         levels = c(1,2),
         #                         c(
         #                           "Formal",
         #                           "Informal")),
         quando = 1
         
  )%>%
  setNames(c("raca_negra","ocupada","n","percent","quando"))

g2 <- ggplot(graph_data2,
             aes(fill = raca_negra, 
                 y = percent, 
                 x = ocupada)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity")+
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # )  +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Race") +
  ylim(0, 80) +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line    = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust=0.5)
  )+
  ggtitle("After birth")

grid.arrange(g1,g2,ncol=2)

ggsave(
  filename = "ocup_formal_periodo_race_sl.png",
  plot  = grid.arrange(g1,g2,ncol=2),
  width = 8, height = 6, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Dissertacao/Graficos/Sao Luis/")
rm(graph_data2,g1,g2)

#transicao
graph_data = brisa_analise_descritiva %>% 
  select(ocupada_formal_prentl, ocupada_formal_seguim ,raca_negra) %>%
  # filter(!is.na(ocupada_formal_prentl) ,
  #        !is.na(ocupada_formal_seguim)) %>%
  group_by(ocupada_formal_prentl,ocupada_formal_seguim) %>%
  count(raca_negra) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         ocupada_formal_prentl = factor(ocupada_formal_prentl,
                                          levels = c(0,1,2),
                                          c("Not working to",
                                            "Formal to",
                                            "Informal to")),
         ocupada_formal_seguim = factor(ocupada_formal_seguim,
                                        levels = c(0,1,2),
                                        c("not working",
                                          "formal",
                                          "informal"))
  )

ggplot(graph_data, aes(fill = raca_negra, 
                       y = percent, 
                       x = interaction(ocupada_formal_prentl, ocupada_formal_seguim))) + 
  geom_bar(width = 0.6, position = position_dodge(width = 1), stat = "identity") +
  # facet_wrap(~raca_negra)+
  geom_text(
    aes(label = paste0(percent,'%')),
    position = position_dodge(width = 0.9),
    size = 4.0,
    vjust = -0.35
  ) +
  geom_hline(yintercept = 0)  + 
  # n
  geom_text(
    aes(label = n),
    position = position_dodge(width = 1),
    size = 4.0,
    vjust = 1.2
  ) +
  # annotate(
  #   "text", x = c(1, 2), y = -1.9,
  #   label = levels(graph_data$ocupada_formal_seguim), size = 5
  # ) +
  # annotate(
  #   "text", x = c(3, 4), y = -1.9,
  #   label = levels(graph_data$ocupada_formal_seguim), size = 5
  # ) +
  # annotate(
  #   "text", x = c(1.5,3.5), y = -3.9,
  #   label = levels(graph_data$ocupada_formal_prentl), size = 5
  # ) + 
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Race") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, 
                               size = 11, 
                               # face = c("plain","bold","bold",
                               #          "italic","plain","italic",
                               #          "italic","bold","plain"),
                               colour = c("#6a737c","red","red",
                                          "black","#6a737c","black",
                                          "black","red","#6a737c")),
    axis.text.y = element_text(size = 12),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text = element_blank(),
    plot.caption = element_text(size = 10)
  ) +
  labs(
    caption = c("Note: Percentage related to transition and race. Ex: Formal during pregnancy and formal after pregnancy, 57% were non-black and 43% were black.
               X-axis colors related to the transition."))


ggsave(
  filename = "ocup_formal_tudo_trans_race22_n675.png",
  plot  = last_plot(),
  width = 9, height = 6.3, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#transicao2
graph_data = brisa_analise_descritiva %>% 
  select(ocupada_formal_prentl, ocupada_formal_seguim ,raca_negra) %>%
  filter(!is.na(ocupada_formal_prentl) ,
         !is.na(ocupada_formal_seguim)) %>%
  group_by(raca_negra) %>%
  count(ocupada_formal_prentl,ocupada_formal_seguim) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         ocupada_formal_prentl = factor(ocupada_formal_prentl,
                                        levels = c(0,1,2),
                                        c("Not working to",
                                          "Formal to",
                                          "Informal to")),
         ocupada_formal_seguim = factor(ocupada_formal_seguim,
                                        levels = c(0,1,2),
                                        c("not working",
                                          "formal",
                                          "informal"))
  )

ggplot(graph_data, aes(fill = raca_negra, 
                       y = n, 
                       x = interaction(ocupada_formal_prentl, ocupada_formal_seguim))) + 
  geom_bar(width = 0.6, position = position_dodge(width = 1), stat = "identity") +
  # facet_wrap(~raca_negra)+
  geom_text(
    aes(label = paste0(percent,'%')),
    position = position_dodge(width = 0.9),
    size = 4.0,
    vjust = -0.35
  ) +
  geom_hline(yintercept = 0)  + 
  # n
  geom_text(
    aes(label = n),
    position = position_dodge(width = 1),
    size = 4.0,
    vjust = 1.2
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Race") +
  ylab("Count") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, 
                               size = 11, 
                               # face = c("plain","bold","bold",
                               #          "italic","plain","italic",
                               #          "italic","bold","plain"),
                               colour = c("#6a737c","red","red",
                                          "black","#6a737c","black",
                                          "black","red","#6a737c")),
    axis.text.y = element_text(size = 12),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text = element_blank(),
    plot.caption = element_text(size = 10)
  ) +
  labs(
    caption = c("Note: Percentage related to race. X-axis colors related to the transition vulnerability."))


ggsave(
  filename = "ocup_formal_tudo_trans_race3_n675.png",
  plot  = last_plot(),
  width = 9, height = 6.3, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")


#-Idade mae----

graph_data = brisa_analise_descritiva %>% 
  select(idade_mae_seguim, raca_negra)# %>%
  # mutate(raca_negra = factor(raca_negra, 
  #                            levels = c(0,1), 
  #                            c("Non-black", "Black")))

ggplot(graph_data, aes(x = idade_mae_seguim, colour = raca_negra)) +
  geom_density(alpha = 0.1,size = 1) +
  theme_classic() +
  xlab("Mom's age") +
  ylab("Density") +
  scale_colour_brewer(palette = "Set2") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 15),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank(),
    legend.title = element_blank(),
    legend.text  = element_text(size = 14),
    legend.position = "bottom"
  )+
  scale_x_continuous(breaks = c(seq(15, 50, 5)))
ggsave(
  filename = "mom_age_race_sl.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Dissertacao/Graficos/Sao Luis/")


#histograma
ggplot(graph_data, aes(x = idade_mae_seguim, colour = raca_negra)) +
  geom_histogram(position = "identity", fill = "white", alpha = 0.4, size=1) +
  theme_classic() +
  xlab("Mom's age") +
  ylab("Count") +
  scale_colour_brewer(palette = "Set2") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank(),
    legend.title = element_blank(),
    legend.text  = element_text(size = 14),
    legend.position = "bottom"
  )+
  scale_x_continuous(breaks = c(seq(15, 50, 5)))

ggsave(
  filename = "mom_age_race2_sl.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Dissertacao/Graficos/Sao Luis/")

#teste t

idade_negras = graph_data %>% filter(raca_negra=="Black") %>% select(idade_mae_seguim)
idade_naonegras = graph_data %>% filter(raca_negra=="Non-black") %>% select(idade_mae_seguim)

t.test(x = idade_negras, y = idade_naonegras)
rm(idade_naonegras,idade_negras)
#-Idade bebe----

graph_data = brisa_analise_descritiva %>% 
  select(idade_bebe_seguim, raca_negra) #%>%
  # mutate(raca_negra = factor(raca_negra, 
  #                            levels = c(0,1), 
  #                            c("Non-black", "Black")))

ggplot(graph_data, aes(x = idade_bebe_seguim, colour = raca_negra)) +
  geom_density(alpha = 0.1,size = 1) +
  theme_classic() +
  xlab("Baby's age in months") +
  ylab("Density") +
  scale_colour_brewer(palette = "Set2", name = "Mom's race") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank(),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.position = "bottom",
  )+
  scale_x_continuous(breaks = c(seq(10, 36, 2) ) ) 
ggsave(
  filename = "baby_age_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")


#histograma
ggplot(graph_data, aes(x = idade_bebe_seguim, colour = raca_negra)) +
  geom_histogram(position = "identity", fill = "white", alpha = 0.4, size=1) +
  theme_classic() +
  xlab("Baby's age in months") +
  ylab("Count") +
  scale_colour_brewer(palette = "Set2", name = "Mom's race") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank(),
    legend.title = element_blank(),
    legend.text  = element_text(size = 14),
    legend.position = "bottom"
  )+
  scale_x_continuous(breaks = c(seq(10, 36, 2) ) ) 

ggsave(
  filename = "baby_age_race2_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#teste t

idade_negras = graph_data %>% filter(raca_negra=="Black") %>% select(idade_bebe_seguim)
idade_naonegras = graph_data %>% filter(raca_negra=="Non-black") %>% select(idade_bebe_seguim)
summary(idade_negras)
summary(idade_naonegras)

t.test(x = idade_negras, y = idade_naonegras)
rm(idade_naonegras,idade_negras)
#-Densidade morad/dormit----

graph_data = brisa_analise_descritiva %>% 
  select(densid_morador_dormitor_prentl, raca_negra) %>%
  mutate(raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")))

ggplot(graph_data, aes(x = densid_morador_dormitor_prentl, colour = raca_negra)) +
  geom_density(alpha = 0.1,size = 1) +
  theme_classic() +
  xlab("Number of residents per room") +
  ylab("Density") +
  scale_colour_brewer(palette = "Set2") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank(),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.position = "bottom",
  ) +
  scale_x_continuous(breaks = c(seq(1, 8, 1) ) ) 

ggsave(
  filename = "densidade_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#-Numero crianca casa----

graph_data = brisa_analise_descritiva %>% 
  select(quantos_filhos_tem_nascim, raca_negra) %>%
  filter(!is.na(quantos_filhos_tem_nascim)) %>%
  group_by(raca_negra) %>%
  mutate(quantos_filhos_tem_nascim = 
           case_when(
             quantos_filhos_tem_nascim == 1 ~ 1,
             quantos_filhos_tem_nascim == 2 ~ 2,
             quantos_filhos_tem_nascim >= 3 ~ 3  )
         ) %>%
  count(quantos_filhos_tem_nascim) %>%
  mutate(percent = round(prop.table(n)*100,1),
         # raca_negra = factor(raca_negra, 
         #                     levels = c(0,1), 
         #                     c("Non-black", "Black")),
         quantos_filhos_tem_nascim = factor(quantos_filhos_tem_nascim,
                                             levels = c(1,2,3), 
                                             c("1","2","3 or more"))
  )



ggplot(graph_data, aes(fill = quantos_filhos_tem_nascim, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.9), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.9),
    size = 5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 6
  )   + 
  # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.9),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Number of children") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 14),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 
+
  guides(fill = guide_legend(nrow=2, byrow=TRUE))

ggsave(
  filename = "number_kids_house_race_sl.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Dissertacao/Graficos/Sao Luis/")

#-Saude crianca----

graph_data = brisa_analise_descritiva %>% 
  select(saude_crianca, raca_negra) %>%
  filter(!is.na(saude_crianca)) %>%
  group_by(raca_negra) %>%
  count(saude_crianca) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         saude_crianca = factor(saude_crianca,
                                   levels = c(0,1),
                                   c("Excellent/Very good/Good","Regular/Bad"))
  )



ggplot(graph_data, aes(fill = saude_crianca, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 5
  )   + 
  # n
  geom_text(
    aes(label = format(n)),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = 1.2
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Baby's health") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 

ggsave(
  filename = "baby_health_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#-Hospital----

graph_data = brisa_analise_descritiva %>% 
  select(hospital_particular, raca_negra) %>%
  filter(!is.na(hospital_particular)) %>%
  group_by(raca_negra) %>%
  count(hospital_particular) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         hospital_particular = factor(hospital_particular,
                                      levels = c(0,1),
                                      c("Public","Private"))
  )

ggplot(graph_data, aes(fill = hospital_particular, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 6
  )   + 
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Baby's birth hospital") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 14),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 

ggsave(
  filename = "hospital_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#-Trabalho em casa----
graph_data = brisa_analise_descritiva %>% 
  select(faz_todo_trab_domestico,raca_negra) %>%
  filter(!is.na(faz_todo_trab_domestico)) %>%
  group_by(raca_negra) %>%
  count(faz_todo_trab_domestico) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         faz_todo_trab_domestico = factor(faz_todo_trab_domestico,
                                           levels = c(0,1),
                                           c("Share the housework",
                                             "Does all the housework"))
  )

ggplot(graph_data, aes(fill = faz_todo_trab_domestico, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 5
  )   + 
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Housework status") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 

ggsave(
  filename = "housework_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#-Creche----
graph_data = brisa_analise_descritiva %>% 
  select(filho_creche,raca_negra) %>%
  filter(!is.na(filho_creche)) %>%
  group_by(raca_negra) %>%
  count(filho_creche) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         filho_creche = factor(filho_creche,
                                          levels = c(0,1),
                                          c("Not on daycare",
                                            "On daycare"))
  )

ggplot(graph_data, aes(fill = filho_creche, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 5
  )   + 
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Kid on daycare") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 

ggsave(
  filename = "daycare_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")
#-Depressao EPDS----
graph_data = brisa_analise_descritiva %>% 
  select(depressao_epds,raca_negra) %>%
  filter(!is.na(depressao_epds)) %>%
  group_by(raca_negra) %>%
  count(depressao_epds) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         depressao_epds = factor(depressao_epds,
                               levels = c(0,1),
                               c("No depressive symptoms",
                                 "Depressive symptoms"))
  )

ggplot(graph_data, aes(fill = depressao_epds, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 5
  )   + 
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Postpartum depression") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 

ggsave(
  filename = "depression_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#-Bayley-risk----
graph_data = brisa_analise_descritiva %>% 
  select(bayley_risco,raca_negra) %>%
  filter(!is.na(bayley_risco)) %>%
  group_by(raca_negra) %>%
  count(bayley_risco) %>%
  mutate(percent = round(prop.table(n)*100,1),
         raca_negra = factor(raca_negra, 
                             levels = c(0,1), 
                             c("Non-black", "Black")),
         bayley_risco = factor(bayley_risco,
                                 levels = c(0,1),
                                 c("No risk in any domain",
                                   "At least one domain in risk"))
  )

ggplot(graph_data, aes(fill = bayley_risco, y = percent, x = raca_negra)) + 
  geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
  #percentual
  geom_text(
    aes(label = paste0(percent,"%")),
    position = position_dodge(width = 0.8),
    size = 4.5,
    vjust = -0.35
  )+
  geom_hline(yintercept = 0) +
  annotate(
    "text", x = c(1, 2), y = -1.7,
    label = levels(graph_data$raca_negra), size = 5
  )   + 
  # # n
  # geom_text(
  #   aes(label = format(n)),
  #   position = position_dodge(width = 0.8),
  #   size = 4.5,
  #   vjust = 1.2
  # ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Bayley III - Risk") +
  ylab("%") + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  ) 

ggsave(
  filename = "bayley_race_n675.png",
  plot  = last_plot(),
  width = 7, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

#----3. Diferenca de medias idade----

brisa_analise_descritiva_black = brisa_analise_descritiva%>% filter(raca_negra=="Black")
brisa_analise_descritiva_nonblack = brisa_analise_descritiva%>% filter(raca_negra!="Black")

#idade bebe
t.test(brisa_analise_descritiva_black$idade_bebe_seguim,
       brisa_analise_descritiva_nonblack$idade_bebe_seguim)

summary(brisa_analise_descritiva_black$idade_bebe_seguim)
summary(brisa_analise_descritiva_nonblack$idade_bebe_seguim)

#idade mae

t.test(brisa_analise_descritiva_black$idade_mae_seguim,
       brisa_analise_descritiva_nonblack$idade_mae_seguim)

summary(brisa_analise_descritiva_black$idade_mae_seguim)
summary(brisa_analise_descritiva_nonblack$idade_mae_seguim)
