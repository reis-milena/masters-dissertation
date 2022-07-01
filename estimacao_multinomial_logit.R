#######################################################
# Dissertacao Mestrado - Milena Villela
# Ano: 2021
# Objetivo do codigo: estimar multinomial logit
# Base de dados: Coorte Brisa 2010
#######################################################

#---- Pacotes ----
library("nnet") #multinomial logit
# library(dplyr)
# library(tidyverse)
library(margins)
# library(ggeffects)
library(stargazer)
library(ggplot2)
library(directlabels)

#---- Chamando a base com as variaveis criadas ----
#-- Ribeirao Preto ----

# source("C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Programas R/brisa_filtrada.R")
source("G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/brisa_filtrada.R")

brisa_filtrada = brisa_filtrada %>%
  filter(idade_mae_seguim >= 18, 
         !is.na(raca_negra) )

brisa_estimacoes = brisa_filtrada %>% 
  select(ocupada_seguim, ocupada_prentl,
         ocupada_formal_seguim, casada_marido_prentl,
         raca_negra, densid_morador_dormitor_prentl ,educacao_mae,
         casada_marido_seguimento, idade_mae_seguim, 
         ocupada_formal_prentl, depressao_epds,
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

brisa_estimacoes_sl = brisa_filtrada_sl %>% 
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

#---- Contando missings ----

for(coluna in 1:ncol(brisa_estimacoes)){
  
  print(paste(names(brisa_estimacoes)[coluna],": ",sum(is.na(brisa_estimacoes[,coluna]))
              ))
}

#variaveis que mais tem missing (utilizadas no modelo)
brisa_estimacoes$dummy_bayley_risco = ifelse(is.na(brisa_estimacoes$bayley_risco),1,0)
brisa_estimacoes$dummy_faz_todo_trab_domestico = ifelse(
  is.na(brisa_estimacoes$faz_todo_trab_domestico),1,0)

#corrigindo variaveis com muto missing para incluir na regressao
brisa_estimacoes$bayley_risco_alt = ifelse(
  is.na(brisa_estimacoes$bayley_risco),0,brisa_estimacoes$bayley_risco)
brisa_estimacoes$faz_todo_trab_domestico_alt = ifelse(
  is.na(brisa_estimacoes$faz_todo_trab_domestico),0,brisa_estimacoes$faz_todo_trab_domestico)

# table(brisa_estimacoes$faz_todo_trab_domestico, exclude = NULL)
# table(brisa_estimacoes$faz_todo_trab_domestico_alt, exclude = NULL)
# table(brisa_estimacoes$dummy_faz_todo_trab_domestico, exclude = NULL)
# 
# table(brisa_estimacoes$bayley_risco, exclude = NULL)
# table(brisa_estimacoes$bayley_risco_alt, exclude = NULL)
# table(brisa_estimacoes$dummy_bayley_risco, exclude = NULL)

#---- Transformando variaveis ----
#-- Ribeirao Preto ----
# table(brisa_estimacoes$ocupada_formal_seguim, exclude = NULL)
brisa_estimacoes$ocupada_formal_seguim = ifelse(brisa_estimacoes$ocupada_formal_seguim==0,1,
                                                ifelse(brisa_estimacoes$ocupada_formal_seguim ==1,2,
                                                       3))
brisa_estimacoes$ocupada_formal_seguim = factor(brisa_estimacoes$ocupada_formal_seguim,
                                              levels = c(1,2,3),
                                              labels = c("Not working", "Formal working",
                                                         "Informal working"))
brisa_estimacoes$ocupada_formal_prentl = factor(brisa_estimacoes$ocupada_formal_prentl,
                                              levels = c(0,1,2),
                                              labels = c("Not working", "Formal working",
                                                         "Informal working"))
brisa_estimacoes$raca_negra = factor(brisa_estimacoes$raca_negra,
                                   levels = c(0,1),
                                   labels = c("Non-black", "Black"))

brisa_estimacoes$educacao_mae = factor(brisa_estimacoes$educacao_mae,
                                     levels = c(1,2,3),
                                     labels = c("Illiterate/elementary/middle school",
                                                "High school",
                                                "Undergraduate compl./incompl."))

# brisa_estimacoes$ipv_gravidez_recorrente = factor(brisa_estimacoes$ipv_gravidez_recorrente,
#                                                 levels = c(0,1,2),
#                                                 labels = c("No violence", "Once",
#                                                            "Recurrent"))
# 
# brisa_estimacoes$viol_gravidez_domest_ex = factor(brisa_estimacoes$viol_gravidez_domest_ex,
#                                               levels = c(0,1),
#                                               labels = c("No violence", "Violence"))
# 
# brisa_estimacoes$hospital_particular = factor(brisa_estimacoes$hospital_particular,
#                                             levels = c(0,1),
#                                             labels = c("Public", "Private"))

brisa_estimacoes$filho_creche = factor(brisa_estimacoes$filho_creche,
                                     levels = c(0,1),
                                     labels = c("Not on daycare", "On daycare"))

brisa_estimacoes$faz_todo_trab_domestico = factor(brisa_estimacoes$faz_todo_trab_domestico,
                                                  levels = c(0,1),
                                                  labels = c("Share the housework",
                                                             "Does all the housework"))

brisa_estimacoes$casada_marido_seguimento = factor(brisa_estimacoes$casada_marido_seguimento,
                                                 levels = c(0,1),
                                                 labels = c("Not married", 
                                                            "Married after pregn."))
brisa_estimacoes$casada_marido_prentl = factor(brisa_estimacoes$casada_marido_prentl,
                                                   levels = c(0,1),
                                                   labels = c("Not married", 
                                                              "Married during pregn."))
# 
# brisa_estimacoes$vinculo_mae_bebe_prejudicada = factor(brisa_estimacoes$vinculo_mae_bebe_prejudicada,
#                                                      levels = c(0,1),
#                                                      labels = c("No broken bond with baby", 
#                                                                 "Broken bond with baby"))
# 
# brisa_estimacoes$suporte_social_cuidados = factor(brisa_estimacoes$suporte_social_cuidados,
#                                                 levels = c(0,1),
#                                                 labels = c("None or rare social support", 
# 
#                                                                                                                       "Have social support"))
# brisa_estimacoes$saude_crianca = factor(brisa_estimacoes$saude_crianca,
#                                       levels = c(0,1),
#                                       labels = c("Great/excellent baby's health", 
#                                                  "Bad/regular baby's health"))

brisa_estimacoes$depressao_epds = factor(brisa_estimacoes$depressao_epds,
                                       levels = c(0,1),
                                       labels = c("No postpartum depression", 
                                                  "Postpartum depression"))

brisa_estimacoes$bayley_risco = factor(brisa_estimacoes$bayley_risco,
                                         levels = c(0,1),
                                         labels = c("No risk in any domain", 
                                                    "At least one domain at risk"))

# brisa_estimacoes$dominio_cognitivo = factor(brisa_estimacoes$dominio_cognitivo,
#                                        levels = c(0,1),
#                                        labels = c("No risk at cognitive domain", 
#                                                   "Cognitive domain at risk"))
# 
# brisa_estimacoes$dominio_comunic_receptiva = factor(brisa_estimacoes$dominio_comunic_receptiva,
#                                           levels = c(0,1),
#                                           labels = c("No risk at receptive communic. domain", 
#                                                      "Receptive communic. domain at risk"))
# 
# brisa_estimacoes$dominio_comunic_expressiva = factor(brisa_estimacoes$dominio_comunic_expressiva,
#                                           levels = c(0,1),
#                                           labels = c("No risk at expressive communic. domain", 
#                                                      "Expressive communic. domain at risk"))
# 
# brisa_estimacoes$dominio_motor_fino = factor(brisa_estimacoes$dominio_motor_fino,
#                                           levels = c(0,1),
#                                           labels = c("No risk at fine motor domain", 
#                                                      "Fine motor domain at risk"))
# 
# brisa_estimacoes$dominio_motor_grosso = factor(brisa_estimacoes$dominio_motor_grosso,
#                                           levels = c(0,1),
#                                           labels = c("No risk at gross motor domain", 
#                                                      "Gross motor domain at risk"))
# 
# brisa_estimacoes$bebe_planejado = factor(brisa_estimacoes$bebe_planejado,
#                                                levels = c(0,1),
#                                                labels = c("Not planned", 
#                                                           "Planned"))

#-- Sao Luis ----
# table(brisa_estimacoes_sl$ocupada_formal_seguim, exclude = NULL)
# brisa_estimacoes_sl$ocupada_formal_seguim = ifelse(brisa_estimacoes_sl$ocupada_formal_seguim==0,1,
#                                                 ifelse(brisa_estimacoes_sl$ocupada_formal_seguim ==1,2,
#                                                        3))
brisa_estimacoes_sl$ocupada_formal_seguim = factor(brisa_estimacoes_sl$ocupada_formal_seguim,
                                                levels = c(0,1,2),
                                                labels = c("Not working", "Formal working",
                                                           "Informal working"))
brisa_estimacoes_sl$ocupada_formal_prentl = factor(brisa_estimacoes_sl$ocupada_formal_prentl,
                                                levels = c(0,1,2),
                                                labels = c("Not working", "Formal working",
                                                           "Informal working"))
brisa_estimacoes_sl$raca_negra = factor(brisa_estimacoes_sl$raca_negra,
                                     levels = c(0,1),
                                     labels = c("Non-black", "Black"))

brisa_estimacoes_sl$educacao_mae = factor(brisa_estimacoes_sl$educacao_mae,
                                       levels = c(1,2,3),
                                       labels = c("Illiterate/elementary/middle school",
                                                  "High school",
                                                  "Undergraduate compl./incompl."))

brisa_estimacoes_sl$filho_creche = factor(brisa_estimacoes_sl$filho_creche,
                                       levels = c(0,1),
                                       labels = c("Not on daycare", "On daycare"))

brisa_estimacoes_sl$faz_todo_trab_domestico = factor(brisa_estimacoes_sl$faz_todo_trab_domestico,
                                                  levels = c(0,1),
                                                  labels = c("Share the housework", 
                                                             "Does all the housework"))

brisa_estimacoes_sl$casada_marido_seguimento = factor(brisa_estimacoes_sl$casada_marido_seguimento,
                                                   levels = c(0,1),
                                                   labels = c("Not married", 
                                                              "Married after pregn."))
brisa_estimacoes_sl$casada_marido_prentl = factor(brisa_estimacoes_sl$casada_marido_prentl,
                                               levels = c(0,1),
                                               labels = c("Not married", 
                                                          "Married during pregn."))

brisa_estimacoes_sl$depressao_epds = factor(brisa_estimacoes_sl$depressao_epds,
                                         levels = c(0,1),
                                         labels = c("No postpartum depression", 
                                                    "Postpartum depression"))

brisa_estimacoes_sl$bayley_risco = factor(brisa_estimacoes_sl$bayley_risco,
                                       levels = c(0,1),
                                       labels = c("No risk in any domain", 
                                                  "At least one domain at risk"))



#---- Multinomial Logit----

# mnl_teste = multinom(ocupada_formal_seguim ~ 
#                       raca_negra                          +
#                       educacao_mae ,
#                     data = brisa_filtrada)


{#Ribeirao preto
mnl_1 = multinom(ocupada_formal_seguim ~ 
                   # informacoes depois do nasci.
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   # casada_marido_prentl                +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        
                 ,
                 data = brisa_estimacoes)
mnl_2 = multinom(ocupada_formal_seguim ~ 
                   #add. informacoes de antes do nascim.
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt
                   faz_todo_trab_domestico
                 ,
                 data = brisa_estimacoes)
#### informacoes de saude/desenvolvimento
mnl_3 = multinom(ocupada_formal_seguim ~
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt         +
                   faz_todo_trab_domestico             +
                   
                   #infos de desenvol. e saude
                   bayley_risco                        +
                   # dummy_bayley_risco                  +
                   # bayley_risco_alt                    +
                   depressao_epds
                 ,
                 data = brisa_estimacoes)
#interacoes
mnl_4 = multinom(ocupada_formal_seguim ~
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt         +
                   faz_todo_trab_domestico             +
                   
                   #infos de desenvol. e saude
                   bayley_risco                        +
                   # dummy_bayley_risco       +
                   # bayley_risco_alt         +
                   depressao_epds                      +
                   
                   #interacao raca e creche
                   raca_negra*filho_creche
                 ,
                 data = brisa_estimacoes)

mnl_5 = multinom(ocupada_formal_seguim ~
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt         +
                   faz_todo_trab_domestico             +
                   
                   #infos de desenvol. e saude
                   bayley_risco                        +
                   # dummy_bayley_risco       +
                   # bayley_risco_alt         +
                   depressao_epds                      +
                   
                   #interacao ocupacao e creche
                   ocupada_formal_prentl*depressao_epds
                 ,
                 data = brisa_estimacoes)

mnl_6 = multinom(ocupada_formal_seguim ~
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt         +
                   faz_todo_trab_domestico             +
                   
                   #infos de desenvol. e saude
                   bayley_risco                        +
                   # dummy_bayley_risco       +
                   # bayley_risco_alt         +
                   depressao_epds                      +
                   
                   #interacao depressao e creche
                   depressao_epds*filho_creche
                 ,
                 data = brisa_estimacoes)

} # Ribeirao Preto

{#Sao Luis
mnl_1 = multinom(ocupada_formal_seguim ~ 
                   # informacoes depois do nasci.
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   # casada_marido_prentl                +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        
                 ,
                 data = brisa_estimacoes_sl)
  
mnl_2 = multinom(ocupada_formal_seguim ~ 
                     #add. informacoes de antes do nascim.
                     raca_negra                          +
                     educacao_mae                        +
                     densid_morador_dormitor_prentl      +
                     casada_marido_seguimento            +
                     idade_mae_seguim                    +
                     idade_bebe_seguim                   +
                     quantos_filhos_tem_nascim           +
                     filho_creche                        +
                     
                     #infos antes do nasc
                     ocupada_formal_prentl               +
                     casada_marido_prentl                +
                     # dummy_faz_todo_trab_domestico       +
                     # faz_todo_trab_domestico_alt
                     faz_todo_trab_domestico
                   ,
                   data = brisa_estimacoes_sl)  

#### informacoes de saude/desenvolvimento
mnl_3 = multinom(ocupada_formal_seguim ~
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt         +
                   faz_todo_trab_domestico             +
                   
                   #infos de desenvol. e saude
                   bayley_risco                        +
                   # dummy_bayley_risco                  +
                   # bayley_risco_alt                    +
                   depressao_epds
                 ,
                 data = brisa_estimacoes_sl)
#interacoes
mnl_4 = multinom(ocupada_formal_seguim ~
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt         +
                   faz_todo_trab_domestico             +
                   
                   #infos de desenvol. e saude
                   bayley_risco                        +
                   # dummy_bayley_risco       +
                   # bayley_risco_alt         +
                   depressao_epds                      +
                   
                   #interacao raca e creche
                   raca_negra*filho_creche
                 ,
                 data = brisa_estimacoes_sl)

mnl_5 = multinom(ocupada_formal_seguim ~
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt         +
                   faz_todo_trab_domestico             +
                   
                   #infos de desenvol. e saude
                   bayley_risco                        +
                   # dummy_bayley_risco       +
                   # bayley_risco_alt         +
                   depressao_epds                      +
                   
                   #interacao ocupacao e creche
                   ocupada_prentl*filho_creche
                 ,
                 data = brisa_estimacoes_sl)

mnl_6 = multinom(ocupada_formal_seguim ~
                   raca_negra                          +
                   educacao_mae                        +
                   densid_morador_dormitor_prentl      +
                   casada_marido_seguimento            +
                   idade_mae_seguim                    +
                   idade_bebe_seguim                   +
                   quantos_filhos_tem_nascim           +
                   filho_creche                        +
                   
                   #infos antes do nasc
                   ocupada_formal_prentl               +
                   casada_marido_prentl                +
                   # dummy_faz_todo_trab_domestico       +
                   # faz_todo_trab_domestico_alt         +
                   faz_todo_trab_domestico             +
                   
                   #infos de desenvol. e saude
                   bayley_risco                        +
                   # dummy_bayley_risco       +
                   # bayley_risco_alt         +
                   depressao_epds                      +
                   
                   #interacao depressao e creche
                   depressao_epds*filho_creche
                 ,
                 data = brisa_estimacoes_sl)

}#Sao Luis



# summary(mnl_3)
#----Stargazer----
# The multinom package does not include p-value calculation for the regression 
# coefficients, so we calculate p-values using Wald tests (here z-tests)
z_mnl1 <- summary(mnl_1)$coefficients/summary(mnl_1)$standard.errors
z_mnl2 <- summary(mnl_2)$coefficients/summary(mnl_2)$standard.errors
z_mnl3 <- summary(mnl_3)$coefficients/summary(mnl_3)$standard.errors
z_mnl4 <- summary(mnl_4)$coefficients/summary(mnl_4)$standard.errors
z_mnl5 <- summary(mnl_5)$coefficients/summary(mnl_5)$standard.errors
z_mnl6 <- summary(mnl_6)$coefficients/summary(mnl_6)$standard.errors

p_mnl1 <- (1 - pnorm(abs(z_mnl1), 0, 1)) * 2
p_mnl2 <- (1 - pnorm(abs(z_mnl2), 0, 1)) * 2
p_mnl3 <- (1 - pnorm(abs(z_mnl3), 0, 1)) * 2
p_mnl4 <- (1 - pnorm(abs(z_mnl4), 0, 1)) * 2
p_mnl5 <- (1 - pnorm(abs(z_mnl5), 0, 1)) * 2
p_mnl6 <- (1 - pnorm(abs(z_mnl6), 0, 1)) * 2

coef_log_odds_mnl1 = exp(summary(mnl_1)$coefficients)
coef_log_odds_mnl2 = exp(summary(mnl_2)$coefficients)
coef_log_odds_mnl3 = exp(summary(mnl_3)$coefficients)
coef_log_odds_mnl4 = exp(summary(mnl_4)$coefficients)
coef_log_odds_mnl5 = exp(summary(mnl_5)$coefficients)
coef_log_odds_mnl6 = exp(summary(mnl_6)$coefficients)

log_lik_mnl1 = round(logLik(mnl_1),1) %>% rep(.,2) #ja tem no multinom (eh o value)
log_lik_mnl2 = round(logLik(mnl_2),1) %>% rep(.,2)
log_lik_mnl3 = round(logLik(mnl_3),1) %>% rep(.,2)
log_lik_mnl4 = round(logLik(mnl_4),1) %>% rep(.,2)
log_lik_mnl5 = round(logLik(mnl_5),1) %>% rep(.,2)
log_lik_mnl6 = round(logLik(mnl_6),1) %>% rep(.,2)

# log_lik_y =  logLik(multinom(ocupada_formal_seguim ~ 1, data = brisa_estimacoes_sl))
log_lik_y =  logLik(multinom(ocupada_formal_seguim ~ 1, data = brisa_estimacoes))

r2_mnl1 = round(as.numeric(1 - log_lik_mnl1/log_lik_y),2) #%>% rep(.,2)
r2_mnl2 = round(as.numeric(1 - log_lik_mnl2/log_lik_y),2) #%>% rep(.,2)
r2_mnl3 = round(as.numeric(1 - log_lik_mnl3/log_lik_y),2) #%>% rep(.,2)
r2_mnl4 = round(as.numeric(1 - log_lik_mnl4/log_lik_y),2) #%>% rep(.,2)
r2_mnl5 = round(as.numeric(1 - log_lik_mnl5/log_lik_y),2) #%>% rep(.,2)
r2_mnl6 = round(as.numeric(1 - log_lik_mnl6/log_lik_y),2) #%>% rep(.,2)
#mcfadden's R2

stargazer(mnl_1,mnl_2,mnl_3,
          mnl_4,mnl_5,mnl_6,
          type = "html",
          title = "Relative Risk Ratio",
          coef = c(list(coef_log_odds_mnl1),list(coef_log_odds_mnl2),
                   list(coef_log_odds_mnl3),list(coef_log_odds_mnl4),
                   list(coef_log_odds_mnl5),list(coef_log_odds_mnl6)
                   ),
          p = c(list(p_mnl1),list(p_mnl2),
                list(p_mnl3),list(p_mnl4),
                list(p_mnl5),list(p_mnl6)
          ),
          add.lines = list(c("$McFadden's R2$", r2_mnl1, r2_mnl2,r2_mnl3,
                             r2_mnl4,r2_mnl5,r2_mnl6
                             ),
                           c("Log Likelihood",log_lik_mnl1,log_lik_mnl2,
                             log_lik_mnl3,
                             log_lik_mnl4,log_lik_mnl5,log_lik_mnl6
                             ),
                           c("No. obs.",rep(nrow(mnl_1$fitted.values),2),
                             rep(nrow(mnl_2$fitted.values),2),rep(nrow(mnl_3$fitted.values),2),
                             rep(nrow(mnl_4$fitted.values),2),rep(nrow(mnl_5$fitted.values),2)
                             ,rep(nrow(mnl_6$fitted.values),2)
                             ))
          ,
          # out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_rel_risk_posquali_saoluis2.html"
          out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_rel_risk_posquali.html"
          
          )
stargazer(mnl_1,mnl_2,mnl_3,
          mnl_4,mnl_5,mnl_6,
          type = "html",
          title = "Coeficientes",
          add.lines = list(c("$McFadden's R2$", r2_mnl1, r2_mnl2,r2_mnl3,
                             r2_mnl4,
                             r2_mnl5,r2_mnl6
                             ),
                           c("Log Likelihood",log_lik_mnl1,log_lik_mnl2,
                             log_lik_mnl3,
                             log_lik_mnl4,log_lik_mnl5,log_lik_mnl6
                           ),
                           c("No. obs.",rep(nrow(mnl_1$fitted.values),2),
                             rep(nrow(mnl_2$fitted.values),2),rep(nrow(mnl_3$fitted.values),2),
                             rep(nrow(mnl_4$fitted.values),2),rep(nrow(mnl_5$fitted.values),2)
                             ,rep(nrow(mnl_6$fitted.values),2)
                           ))
          ,
          # out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_coef_posquali_saoluis2.html"
          out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_coef_posquali.html"
          
)
rm(log_lik_mnl1,log_lik_mnl2,log_lik_mnl3,log_lik_mnl4, log_lik_mnl5, log_lik_y,
   r2_mnl1,r2_mnl2,r2_mnl3,r2_mnl4,r2_mnl5,
   z_mnl1,z_mnl2,z_mnl3,z_mnl4,z_mnl5, p_mnl1,p_mnl2,p_mnl3,p_mnl4,p_mnl5)
# stargazer(mnl_6,
#           type = "latex",
#           coef = list(coef_log_odds_mnl6),
#           p    = list(p_mnl6),
#           add.lines = list(c("$McFadden's R2$",log_lik_mnl6),
#                            c("No. obs.",rep(nrow(brisa_estimacoes),2))))

# fitted_mnl3 = fitted(mnl_3)
# coef_mnl3 = exp(coef(mnl_3))          # log odds
# coef_mnl3 = (exp(coef(mnl_3))-1)*100  # odds ratio

# The relative risk ratio switching from black = 0 to 1 is 0.7816469 (exp(coef)) for 
# being in informal working after pregnancy vs. not working after pregn.

#---- Teste T de dif. de medias para ver diferencas na amostra entre modelos----

{base_1 = brisa_estimacoes %>% select(ocupada_formal_seguim,raca_negra,educacao_mae,
                                     densid_morador_dormitor_prentl,casada_marido_seguimento,
                                     idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
                                     filho_creche  ) %>%
  drop_na()

base_2 = brisa_estimacoes %>% select(ocupada_formal_seguim,raca_negra,educacao_mae,
                                     densid_morador_dormitor_prentl,casada_marido_seguimento,
                                     idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
                                     filho_creche,ocupada_formal_prentl,casada_marido_prentl,
                                     faz_todo_trab_domestico  ) %>%
  drop_na() %>% 
  select(ocupada_formal_seguim,raca_negra,educacao_mae,
         densid_morador_dormitor_prentl,casada_marido_seguimento,
         idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
         filho_creche )

base_3 = brisa_estimacoes %>% select(ocupada_formal_seguim,raca_negra,educacao_mae,
                                     densid_morador_dormitor_prentl,casada_marido_seguimento,
                                     idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
                                     filho_creche,ocupada_formal_prentl,casada_marido_prentl,
                                     faz_todo_trab_domestico,bayley_risco,depressao_epds
) %>%
  drop_na() %>% 
  select(ocupada_formal_seguim,raca_negra,educacao_mae,
         densid_morador_dormitor_prentl,casada_marido_seguimento,
         idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
         filho_creche ) %>%
  as.numeric()
} #ribeirao preto
{
  base_1 = brisa_estimacoes_sl %>% select(ocupada_formal_seguim,raca_negra,educacao_mae,
                                       densid_morador_dormitor_prentl,casada_marido_seguimento,
                                       idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
                                       filho_creche  ) %>%
    drop_na()
  
  base_2 = brisa_estimacoes_sl %>% select(ocupada_formal_seguim,raca_negra,educacao_mae,
                                       densid_morador_dormitor_prentl,casada_marido_seguimento,
                                       idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
                                       filho_creche,ocupada_formal_prentl,casada_marido_prentl,
                                       faz_todo_trab_domestico  ) %>%
    drop_na() %>% 
    select(ocupada_formal_seguim,raca_negra,educacao_mae,
           densid_morador_dormitor_prentl,casada_marido_seguimento,
           idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
           filho_creche )
  
  base_3 = brisa_estimacoes_sl %>% select(ocupada_formal_seguim,raca_negra,educacao_mae,
                                       densid_morador_dormitor_prentl,casada_marido_seguimento,
                                       idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
                                       filho_creche,ocupada_formal_prentl,casada_marido_prentl,
                                       faz_todo_trab_domestico,bayley_risco,depressao_epds
  ) %>%
    drop_na() %>% 
    select(ocupada_formal_seguim,raca_negra,educacao_mae,
           densid_morador_dormitor_prentl,casada_marido_seguimento,
           idade_mae_seguim,idade_bebe_seguim,quantos_filhos_tem_nascim,
           filho_creche ) 
} #sao luis
base_1 = lapply(base_1,as.numeric) %>% as.data.frame()
base_2 = lapply(base_2,as.numeric) %>% as.data.frame()
base_3 = lapply(base_3,as.numeric) %>% as.data.frame()

linha=1
text_teste_t = as.character()
for (coluna in 1:ncol(base_1)) {
  #base 1 e base 2
  pvalor = t.test(base_1[,coluna], base_2[,coluna])$p.value
  text_teste_t[linha] = paste("Modelo1 X Modelo2-","Variável: ",names(base_1)[coluna],
                              "p-valor: ",pvalor)
  linha = linha + 1
  #base 1 e base 3
  pvalor = t.test(base_1[,coluna], base_3[,coluna])$p.value
  text_teste_t[linha] = paste("Modelo1 X Modelo3-","Variável: ",names(base_1)[coluna],
                              "p-valor: ",pvalor)
  linha = linha + 1
  
}
writeLines(text_teste_t, 
           "G:/Meu Drive/Dissertação pc/Dissertação Mestrado/Programas R/teste_t_sl.txt"
           # "G:/Meu Drive/Dissertação pc/Dissertação Mestrado/Programas R/teste_t.txt"
           )
rm(base_1,base_2,base_3, coluna, linha, text_teste_t, pvalor)

#---- Multinomial Unindo RP e SL ----
brisa_estimacoes$munic    = 1 %>%
  factor(levels = 1,labels = "Ribeirão Preto")  # Ribeirao Preto
brisa_estimacoes_sl$munic = 0 %>%
  factor(levels = 0,labels = "São Luís")   # Sao Luis

brisa_unido_estimacoes = rbind(brisa_estimacoes, brisa_estimacoes_sl)

mnl3_unido = multinom(ocupada_formal_seguim ~
                        raca_negra                          +
                        educacao_mae                        +
                        densid_morador_dormitor_prentl      +
                        casada_marido_seguimento            +
                        idade_mae_seguim                    +
                        idade_bebe_seguim                   +
                        quantos_filhos_tem_nascim           +
                        filho_creche                        +
                        
                        #infos antes do nasc
                        ocupada_formal_prentl               +
                        casada_marido_prentl                +
                        # dummy_faz_todo_trab_domestico       +
                        # faz_todo_trab_domestico_alt         +
                        faz_todo_trab_domestico             +
                        
                        #infos de desenvol. e saude
                        bayley_risco                        +
                        # dummy_bayley_risco                  +
                        # bayley_risco_alt                    +
                        depressao_epds
                      ,
                      data = brisa_unido_estimacoes)
mnl3_unido_munic = multinom(ocupada_formal_seguim ~
                        raca_negra                          +
                        educacao_mae                        +
                        densid_morador_dormitor_prentl      +
                        casada_marido_seguimento            +
                        idade_mae_seguim                    +
                        idade_bebe_seguim                   +
                        quantos_filhos_tem_nascim           +
                        filho_creche                        +
                        
                        #infos antes do nasc
                        ocupada_formal_prentl               +
                        casada_marido_prentl                +
                        # dummy_faz_todo_trab_domestico       +
                        # faz_todo_trab_domestico_alt         +
                        faz_todo_trab_domestico             +
                        
                        #infos de desenvol. e saude
                        bayley_risco                        +
                        # dummy_bayley_risco                  +
                        # bayley_risco_alt                    +
                        depressao_epds                      +
                        munic
                      ,
                      data = brisa_unido_estimacoes)
mnl4_unido_munic = multinom(ocupada_formal_seguim ~
                              raca_negra                          +
                              educacao_mae                        +
                              densid_morador_dormitor_prentl      +
                              casada_marido_seguimento            +
                              idade_mae_seguim                    +
                              idade_bebe_seguim                   +
                              quantos_filhos_tem_nascim           +
                              filho_creche                        +
                              
                              #infos antes do nasc
                              ocupada_formal_prentl               +
                              casada_marido_prentl                +
                              # dummy_faz_todo_trab_domestico       +
                              # faz_todo_trab_domestico_alt         +
                              faz_todo_trab_domestico             +
                              
                              #infos de desenvol. e saude
                              bayley_risco                        +
                              # dummy_bayley_risco                  +
                              # bayley_risco_alt                    +
                              depressao_epds                      +
                              filho_creche*depressao_epds         +
                              munic
                            ,
                            data = brisa_unido_estimacoes)

z_mnl3       <- summary(mnl3_unido)$coefficients/summary(mnl3_unido)$standard.errors
z_mnl3_munic <- summary(mnl3_unido_munic)$coefficients/summary(mnl3_unido_munic)$standard.errors
z_mnl4_munic <- summary(mnl4_unido_munic)$coefficients/summary(mnl4_unido_munic)$standard.errors

p_mnl3       <- (1 - pnorm(abs(z_mnl3), 0, 1)) * 2
p_mnl3_munic <- (1 - pnorm(abs(z_mnl3_munic), 0, 1)) * 2
p_mnl4_munic <- (1 - pnorm(abs(z_mnl4_munic), 0, 1)) * 2

coef_log_odds_mnl3       = exp(summary(mnl3_unido)$coefficients)
coef_log_odds_mnl3_munic = exp(summary(mnl3_unido_munic)$coefficients)
coef_log_odds_mnl4_munic = exp(summary(mnl4_unido_munic)$coefficients)

log_lik_mnl3       = round(logLik(mnl3_unido),1) %>% rep(.,2) #ja tem no multinom (eh o value)
log_lik_mnl3_munic = round(logLik(mnl3_unido_munic),1) %>% rep(.,2) #ja tem no multinom (eh o value)
log_lik_mnl4_munic = round(logLik(mnl4_unido_munic),1) %>% rep(.,2) #ja tem no multinom (eh o value)

# log_lik_y =  logLik(multinom(ocupada_formal_seguim ~ 1, data = brisa_estimacoes_sl))
log_lik_y =  logLik(multinom(ocupada_formal_seguim ~ 1, data = brisa_estimacoes))

r2_mnl3       = round(as.numeric(1 - log_lik_mnl3/log_lik_y),2) #%>% rep(.,2)
r2_mnl3_munic = round(as.numeric(1 - log_lik_mnl3_munic/log_lik_y),2) #%>% rep(.,2)
r2_mnl4_munic = round(as.numeric(1 - log_lik_mnl4_munic/log_lik_y),2) #%>% rep(.,2)

#mcfadden's R2

stargazer(mnl3_unido,mnl3_unido_munic,mnl4_unido_munic,
          type = "html",
          title = "Relative Risk Ratio",
          coef = c(list(coef_log_odds_mnl3),list(coef_log_odds_mnl3_munic),
                   list(coef_log_odds_mnl4_munic)
          ),
          p = c(list(p_mnl3),list(p_mnl3_munic),
                list(p_mnl4_munic)
          ),
          add.lines = list(c("$McFadden's R2$", r2_mnl3,r2_mnl3_munic,
                             r2_mnl4_munic
          ),
          c("Log Likelihood",log_lik_mnl3,log_lik_mnl3_munic,log_lik_mnl4_munic
          ),
          c("No. obs.",rep(nrow(mnl3_unido$fitted.values),2),
            rep(nrow(mnl3_unido_munic$fitted.values),2),
            rep(nrow(mnl4_unido_munic$fitted.values),2)
          ))
          ,
          # out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_rel_risk_posquali_saoluis2.html"
          out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_rel_risk_unido.html"
          
)
stargazer(mnl3_unido,mnl3_unido_munic,mnl4_unido_munic,
          type = "html",
          title = "Coeficientes",
          add.lines = list(c("$McFadden's R2$", r2_mnl3,r2_mnl3_munic,
                             r2_mnl4_munic
          ),
          c("Log Likelihood",log_lik_mnl3,log_lik_mnl3_munic,log_lik_mnl4_munic
          ),
          c("No. obs.",rep(nrow(mnl3_unido$fitted.values),2),
            rep(nrow(mnl3_unido_munic$fitted.values),2),
            rep(nrow(mnl4_unido_munic$fitted.values),2)
          ))
          ,
          # out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_coef_posquali_saoluis2.html"
          out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_coef_unido.html"
          
)

#---- Multinomial - retorno- so as que trabalharam no pre natal----
brisa_estimacoes$munic    = 1 %>%
  factor(levels = 1,labels = "Ribeirão Preto")  # Ribeirao Preto
brisa_estimacoes_sl$munic = 0 %>%
  factor(levels = 0,labels = "São Luís")   # Sao Luis

brisa_unido_estimacoes_retorno = rbind(brisa_estimacoes, brisa_estimacoes_sl) %>%
  filter(ocupada_prentl == 1 )

mnl3_unido = multinom(ocupada_formal_seguim ~
                        raca_negra                          +
                        educacao_mae                        +
                        densid_morador_dormitor_prentl      +
                        casada_marido_seguimento            +
                        idade_mae_seguim                    +
                        idade_bebe_seguim                   +
                        quantos_filhos_tem_nascim           +
                        filho_creche                        +
                        
                        #infos antes do nasc
                        ocupada_formal_prentl               +
                        casada_marido_prentl                +
                        # dummy_faz_todo_trab_domestico       +
                        # faz_todo_trab_domestico_alt         +
                        faz_todo_trab_domestico             +
                        
                        #infos de desenvol. e saude
                        bayley_risco                        +
                        # dummy_bayley_risco                  +
                        # bayley_risco_alt                    +
                        depressao_epds
                      ,
                      data = brisa_unido_estimacoes_retorno)
mnl3_unido_munic = multinom(ocupada_formal_seguim ~
                              raca_negra                          +
                              educacao_mae                        +
                              densid_morador_dormitor_prentl      +
                              casada_marido_seguimento            +
                              idade_mae_seguim                    +
                              idade_bebe_seguim                   +
                              quantos_filhos_tem_nascim           +
                              filho_creche                        +
                              
                              #infos antes do nasc
                              ocupada_formal_prentl               +
                              casada_marido_prentl                +
                              # dummy_faz_todo_trab_domestico       +
                              # faz_todo_trab_domestico_alt         +
                              faz_todo_trab_domestico             +
                              
                              #infos de desenvol. e saude
                              bayley_risco                        +
                              # dummy_bayley_risco                  +
                              # bayley_risco_alt                    +
                              depressao_epds                      +
                              munic
                            ,
                            data = brisa_unido_estimacoes_retorno)
mnl4_unido_munic = multinom(ocupada_formal_seguim ~
                              raca_negra                          +
                              educacao_mae                        +
                              densid_morador_dormitor_prentl      +
                              casada_marido_seguimento            +
                              idade_mae_seguim                    +
                              idade_bebe_seguim                   +
                              quantos_filhos_tem_nascim           +
                              filho_creche                        +
                              
                              #infos antes do nasc
                              ocupada_formal_prentl               +
                              casada_marido_prentl                +
                              # dummy_faz_todo_trab_domestico       +
                              # faz_todo_trab_domestico_alt         +
                              faz_todo_trab_domestico             +
                              
                              #infos de desenvol. e saude
                              bayley_risco                        +
                              # dummy_bayley_risco                  +
                              # bayley_risco_alt                    +
                              depressao_epds                      +
                              filho_creche*depressao_epds         +
                              munic
                            ,
                            data = brisa_unido_estimacoes_retorno)

z_mnl3       <- summary(mnl3_unido)$coefficients/summary(mnl3_unido)$standard.errors
z_mnl3_munic <- summary(mnl3_unido_munic)$coefficients/summary(mnl3_unido_munic)$standard.errors
z_mnl4_munic <- summary(mnl4_unido_munic)$coefficients/summary(mnl4_unido_munic)$standard.errors

p_mnl3       <- (1 - pnorm(abs(z_mnl3), 0, 1)) * 2
p_mnl3_munic <- (1 - pnorm(abs(z_mnl3_munic), 0, 1)) * 2
p_mnl4_munic <- (1 - pnorm(abs(z_mnl4_munic), 0, 1)) * 2

coef_log_odds_mnl3       = exp(summary(mnl3_unido)$coefficients)
coef_log_odds_mnl3_munic = exp(summary(mnl3_unido_munic)$coefficients)
coef_log_odds_mnl4_munic = exp(summary(mnl4_unido_munic)$coefficients)

log_lik_mnl3       = round(logLik(mnl3_unido),1) %>% rep(.,2) #ja tem no multinom (eh o value)
log_lik_mnl3_munic = round(logLik(mnl3_unido_munic),1) %>% rep(.,2) #ja tem no multinom (eh o value)
log_lik_mnl4_munic = round(logLik(mnl4_unido_munic),1) %>% rep(.,2) #ja tem no multinom (eh o value)

# log_lik_y =  logLik(multinom(ocupada_formal_seguim ~ 1, data = brisa_estimacoes_sl))
log_lik_y =  logLik(multinom(ocupada_formal_seguim ~ 1, data = brisa_estimacoes))

r2_mnl3       = round(as.numeric(1 - log_lik_mnl3/log_lik_y),2) #%>% rep(.,2)
r2_mnl3_munic = round(as.numeric(1 - log_lik_mnl3_munic/log_lik_y),2) #%>% rep(.,2)
r2_mnl4_munic = round(as.numeric(1 - log_lik_mnl4_munic/log_lik_y),2) #%>% rep(.,2)

#mcfadden's R2

stargazer(mnl3_unido,mnl3_unido_munic,mnl4_unido_munic,
          type = "html",
          title = "Relative Risk Ratio",
          coef = c(list(coef_log_odds_mnl3),list(coef_log_odds_mnl3_munic),
                   list(coef_log_odds_mnl4_munic)
          ),
          p = c(list(p_mnl3),list(p_mnl3_munic),
                list(p_mnl4_munic)
          ),
          add.lines = list(c("$McFadden's R2$", r2_mnl3,r2_mnl3_munic,
                             r2_mnl4_munic
          ),
          c("Log Likelihood",log_lik_mnl3,log_lik_mnl3_munic,log_lik_mnl4_munic
          ),
          c("No. obs.",rep(nrow(mnl3_unido$fitted.values),2),
            rep(nrow(mnl3_unido_munic$fitted.values),2),
            rep(nrow(mnl4_unido_munic$fitted.values),2)
          ))
          ,
          # out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_rel_risk_posquali_saoluis2.html"
          out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_rel_risk_unido_retorno.html"
          
)
stargazer(mnl3_unido,mnl3_unido_munic,mnl4_unido_munic,
          type = "html",
          title = "Coeficientes",
          add.lines = list(c("$McFadden's R2$", r2_mnl3,r2_mnl3_munic,
                             r2_mnl4_munic
          ),
          c("Log Likelihood",log_lik_mnl3,log_lik_mnl3_munic,log_lik_mnl4_munic
          ),
          c("No. obs.",rep(nrow(mnl3_unido$fitted.values),2),
            rep(nrow(mnl3_unido_munic$fitted.values),2),
            rep(nrow(mnl4_unido_munic$fitted.values),2)
          ))
          ,
          # out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_coef_posquali_saoluis2.html"
          out = "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/mnl_coef_unido_retorno.html"
          
)



#---- Marginal Effect ----
margins(mnl_1)
cplot(mnl_1, what = "effect")
marg_effect_1 = marginal_effects(mnl_1) # = dy/dx e a media disso eh o AME
for (coluna in 1:ncol(marg_effect_1)) {
  print(colnames(marg_effect_1[coluna])) ; print(mean(marg_effect_1[,coluna]))
}
marg_effect_6 = marginal_effects(mnl_6)
for (coluna in 1:ncol(marg_effect_6)) {
  print(colnames(marg_effect_6[coluna])) ; print(mean(marg_effect_6[,coluna]))
}

#---- GGpredict----
library(ggeffects)
library("ggpubr")

#--- Extremes - all good/bad----
#Extreme good
ggpredict_4_extrm_good = ggpredict(mnl_4, 
          terms = c("raca_negra"),
          condition = c(casada_marido_seguimento = "Married after pregn.",
                        casada_marido_prentl  = "Married during pregn.",
                        educacao_mae = "Undergraduate compl./incompl.",
                        faz_todo_trab_domestico = "Share the housework",
                        filho_creche = "On daycare",
                        bayley_risco = "No risk in any domain",
                        ocupada_formal_prentl = "Formal working",
                        depressao_epds = "No postpartum depression"
                        )
          )

ggplot(ggpredict_4_extrm_good, aes(x = x, y = predicted)) + 
  geom_bar(size = 2, stat = "identity", 
           fill= "#44C2A5") + # cor rosa claro do brewer Set2 #FC8D42
  facet_wrap(~response.level) +
  ylab("Predicted probab.") +
  theme_classic() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15)
  )

ggsave(
  filename = "predic_prob_mnl4_extrm_good.png",
  plot  = last_plot(),
  width = 9, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")


#Extreme bad
ggpredict_4_extrm_bad = ggpredict(mnl_4, 
                                   terms = c("raca_negra"),
                                   condition = c(casada_marido_seguimento = "Not married",
                                                 casada_marido_prentl  = "Not married",
                                                 educacao_mae = "Illiterate/elementary/middle school",
                                                 faz_todo_trab_domestico = "Does all the housework",
                                                 filho_creche = "Not on daycare",
                                                 bayley_risco = "At least one domain at risk",
                                                 ocupada_formal_prentl = "Not working",
                                                 depressao_epds = "Postpartum depression"
                                   )
)

ggplot(ggpredict_4_extrm_bad, aes(x = x, y = predicted)) + 
  geom_bar(size = 2, stat = "identity", 
           fill= "#FC8D42") + # cor azul claro do brewer Set2 #44C2A5
  facet_wrap(~response.level) +
  ylab("Predicted probab.") +
  theme_classic() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15)
  )

ggsave(
  filename = "predic_prob_mnl4_extrm_bad.png",
  plot  = last_plot(),
  width = 9, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")
rm(ggpredict_4_extrm_bad,ggpredict_4_extrm_good)

#--- Extremes changing previous work----
#Extreme good
ggpredict_4_extrm_good_formal = ggpredict(mnl_4, 
                                          terms = c("raca_negra"),
                                          condition = c(casada_marido_seguimento = "Married after pregn.",
                                                        casada_marido_prentl  = "Married during pregn.",
                                                        educacao_mae = "Undergraduate compl./incompl.",
                                                        faz_todo_trab_domestico = "Share the housework",
                                                        filho_creche = "On daycare",
                                                        bayley_risco = "No risk in any domain",
                                                        ocupada_formal_prentl = "Formal working",
                                                        depressao_epds = "No postpartum depression"
                                   )
)%>% mutate(previous = "Formal working")
ggpredict_4_extrm_good_informal = ggpredict(mnl_4, 
                                            terms = c("raca_negra"),
                                            condition = c(casada_marido_seguimento = "Married after pregn.",
                                                          casada_marido_prentl  = "Married during pregn.",
                                                          educacao_mae = "Undergraduate compl./incompl.",
                                                          faz_todo_trab_domestico = "Share the housework",
                                                          filho_creche = "On daycare",
                                                          bayley_risco = "No risk in any domain",
                                                          ocupada_formal_prentl = "Informal working",
                                                          depressao_epds = "No postpartum depression"
                                            )
)%>% mutate(previous = "Informal working")
ggpredict_4_extrm_good_notwrk = ggpredict(mnl_4, 
                                          terms = c("raca_negra"),
                                          condition = c(casada_marido_seguimento = "Married after pregn.",
                                                        casada_marido_prentl  = "Married during pregn.",
                                                        educacao_mae = "Undergraduate compl./incompl.",
                                                        faz_todo_trab_domestico = "Share the housework",
                                                        filho_creche = "On daycare",
                                                        bayley_risco = "No risk in any domain",
                                                        ocupada_formal_prentl = "Not working",
                                                        depressao_epds = "No postpartum depression"
                                          )
) %>% mutate(previous = "Not working")

ggpredict_4_extrm_good= rbind(ggpredict_4_extrm_good_formal,
                              ggpredict_4_extrm_good_informal,
                              ggpredict_4_extrm_good_notwrk)


ggplot(ggpredict_4_extrm_good %>% filter(previous=="Informal working"), aes(x = x, y = predicted, fill = response.level)) + 
  geom_bar(width = 0.6, stat = "identity",  position = position_dodge(width = 0.8)) +
  # facet_wrap(~previous=="Formal working") +
  ylab("Predicted probab.") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Working status after pregn.")+ 
  ggtitle("Informal working during pregnancy") +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15)
  )

ggsave(
  filename = "predic_prob_mnl4_extrm_good_previous_work_formal.png",
  plot  = last_plot(),
  width = 9, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

ggplot(ggpredict_4_extrm_good, aes(x = x, y = predicted, fill = previous)) + 
  geom_bar(width = 0.6, stat = "identity",  position = position_dodge(width = 0.8)) +
  facet_wrap(~response.level) +
  ylab("Predicted probab.") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "During pregnancy")+ 
  ggtitle("Extreme good case")+ 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15),
    title = element_text(size = 15)
  )
ggsave(
  filename = "predic_prob_mnl4_extrm_good_previous_work.png",
  plot  = last_plot(),
  width = 9, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")


rm(ggpredict_4_extrm_good_formal,ggpredict_4_extrm_good_informal,
   ggpredict_4_extrm_good_notwrk, ggpredict_4_extrm_good)
#Extreme bad
ggpredict_4_extrm_bad_notwrk = ggpredict(mnl_4, 
                                  terms = c("raca_negra"),
                                  condition = c(casada_marido_seguimento = "Not married",
                                                casada_marido_prentl  = "Not married",
                                                educacao_mae = "Illiterate/elementary/middle school",
                                                faz_todo_trab_domestico = "Does all the housework",
                                                filho_creche = "Not on daycare",
                                                bayley_risco = "At least one domain at risk",
                                                ocupada_formal_prentl = "Not working",
                                                depressao_epds = "Postpartum depression"
                                  )
) %>% mutate(previous = "Not working")

ggpredict_4_extrm_bad_formal = ggpredict(mnl_4, 
                                         terms = c("raca_negra"),
                                         condition = c(casada_marido_seguimento = "Not married",
                                                       casada_marido_prentl  = "Not married",
                                                       educacao_mae = "Illiterate/elementary/middle school",
                                                       faz_todo_trab_domestico = "Does all the housework",
                                                       filho_creche = "Not on daycare",
                                                       bayley_risco = "At least one domain at risk",
                                                       ocupada_formal_prentl = "Formal working",
                                                       depressao_epds = "Postpartum depression"
                                         )
) %>% mutate(previous = "Formal working")

ggpredict_4_extrm_bad_informal = ggpredict(mnl_4, 
                                         terms = c("raca_negra"),
                                         condition = c(casada_marido_seguimento = "Not married",
                                                       casada_marido_prentl  = "Not married",
                                                       educacao_mae = "Illiterate/elementary/middle school",
                                                       faz_todo_trab_domestico = "Does all the housework",
                                                       filho_creche = "Not on daycare",
                                                       bayley_risco = "At least one domain at risk",
                                                       ocupada_formal_prentl = "Informal working",
                                                       depressao_epds = "Postpartum depression"
                                         )
) %>% mutate(previous = "Informal working")

ggpredict_4_extrm_bad= rbind(ggpredict_4_extrm_bad_formal,
                              ggpredict_4_extrm_bad_informal,
                              ggpredict_4_extrm_bad_notwrk)

ggplot(ggpredict_4_extrm_bad, aes(x = x, y = predicted, fill = previous)) + 
  geom_bar(width = 0.6, stat = "identity",  position = position_dodge(width = 0.8)) +
  facet_wrap(~response.level) +
  ylab("Predicted probab.") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "During pregnancy")+ 
  ggtitle("Extreme bad case")+ 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15),
    title = element_text(size = 15)
  )

ggsave(
  filename = "predic_prob_mnl4_extrm_bad_previous_work.png",
  plot  = last_plot(),
  width = 9, height = 5, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")
rm(ggpredict_4_extrm_bad, ggpredict_4_extrm_bad_formal,
   ggpredict_4_extrm_bad_informal, ggpredict_4_extrm_bad_notwrk)
#---Extremes together----

ggpredict_3_extrm_good_formal = ggpredict(mnl_3, 
                                          terms = c("raca_negra"),
                                          condition = c(casada_marido_seguimento = "Married after pregn.",
                                                        casada_marido_prentl  = "Married during pregn.",
                                                        # educacao_mae = "Undergraduate compl./incompl.",
                                                        faz_todo_trab_domestico = "Share the housework",
                                                        filho_creche = "On daycare",
                                                        bayley_risco = "No risk in any domain",
                                                        ocupada_formal_prentl = "Formal working",
                                                        depressao_epds = "No postpartum depression"
                                          )
)%>% mutate(previous = "Formal working")
ggpredict_3_extrm_good_informal = ggpredict(mnl_3, 
                                            terms = c("raca_negra"),
                                            condition = c(casada_marido_seguimento = "Married after pregn.",
                                                          casada_marido_prentl  = "Married during pregn.",
                                                          educacao_mae = "Undergraduate compl./incompl.",
                                                          faz_todo_trab_domestico = "Share the housework",
                                                          filho_creche = "On daycare",
                                                          bayley_risco = "No risk in any domain",
                                                          ocupada_formal_prentl = "Informal working",
                                                          depressao_epds = "No postpartum depression"
                                            )
)%>% mutate(previous = "Informal working")
ggpredict_3_extrm_good_notwrk = ggpredict(mnl_3, 
                                          terms = c("raca_negra"),
                                          condition = c(casada_marido_seguimento = "Married after pregn.",
                                                        casada_marido_prentl  = "Married during pregn.",
                                                        educacao_mae = "Undergraduate compl./incompl.",
                                                        faz_todo_trab_domestico = "Share the housework",
                                                        filho_creche = "On daycare",
                                                        bayley_risco = "No risk in any domain",
                                                        ocupada_formal_prentl = "Not working",
                                                        depressao_epds = "No postpartum depression"
                                          )
) %>% mutate(previous = "Not working")

ggpredict_3_extrm_good= rbind(ggpredict_3_extrm_good_formal,
                              ggpredict_3_extrm_good_informal,
                              ggpredict_3_extrm_good_notwrk)

ggpredict_3_extrm_bad_notwrk = ggpredict(mnl_3, 
                                         terms = c("raca_negra"),
                                         condition = c(casada_marido_seguimento = "Not married",
                                                       casada_marido_prentl  = "Not married",
                                                       educacao_mae = "Illiterate/elementary/middle school",
                                                       faz_todo_trab_domestico = "Does all the housework",
                                                       filho_creche = "Not on daycare",
                                                       bayley_risco = "At least one domain at risk",
                                                       ocupada_formal_prentl = "Not working",
                                                       depressao_epds = "Postpartum depression"
                                         )
) %>% mutate(previous = "Not working")

ggpredict_3_extrm_bad_formal = ggpredict(mnl_3, 
                                         terms = c("raca_negra"),
                                         condition = c(casada_marido_seguimento = "Not married",
                                                       casada_marido_prentl  = "Not married",
                                                       educacao_mae = "Illiterate/elementary/middle school",
                                                       faz_todo_trab_domestico = "Does all the housework",
                                                       filho_creche = "Not on daycare",
                                                       bayley_risco = "At least one domain at risk",
                                                       ocupada_formal_prentl = "Formal working",
                                                       depressao_epds = "Postpartum depression"
                                         )
) %>% mutate(previous = "Formal working")

ggpredict_3_extrm_bad_informal = ggpredict(mnl_3, 
                                           terms = c("raca_negra"),
                                           condition = c(casada_marido_seguimento = "Not married",
                                                         casada_marido_prentl  = "Not married",
                                                         educacao_mae = "Illiterate/elementary/middle school",
                                                         faz_todo_trab_domestico = "Does all the housework",
                                                         filho_creche = "Not on daycare",
                                                         bayley_risco = "At least one domain at risk",
                                                         ocupada_formal_prentl = "Informal working",
                                                         depressao_epds = "Postpartum depression"
                                           )
) %>% mutate(previous = "Informal working")

ggpredict_3_extrm_bad= rbind(ggpredict_3_extrm_bad_formal,
                             ggpredict_3_extrm_bad_informal,
                             ggpredict_3_extrm_bad_notwrk)

rm(ggpredict_3_extrm_bad_formal,
   ggpredict_3_extrm_bad_informal, ggpredict_3_extrm_bad_notwrk,
   ggpredict_3_extrm_good_formal,ggpredict_3_extrm_good_informal,
   ggpredict_3_extrm_good_notwrk)

# Para os tres graficos de previous work vai mudando em previous e
# salvando com nomes diferentes
gg_good = ggplot(ggpredict_3_extrm_good %>% filter(previous=="Not working"), aes(x = x, y = predicted, fill = response.level)) + 
  geom_bar(width = 0.6, stat = "identity",  position = position_dodge(width = 0.8)) +
  # facet_wrap(~previous=="Formal working") +
  ylab("Predicted probab.") +
  ylim(0,0.85) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Working status after pregn.")+ 
  ggtitle("Not working during pregnancy",subtitle = "Extreme good case") +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15),
    title = element_text(size = 15)
  )

gg_bad = ggplot(ggpredict_3_extrm_bad %>% filter(previous == "Not working"), 
                aes(x = x, y = predicted, fill = response.level)) + 
  geom_bar(width = 0.6, stat = "identity",  position = position_dodge(width = 0.8)) +
  # facet_wrap(~response.level) +
  ylab("Predicted probab.") +
  ylim(0,0.85) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Working status after pregn.")+
  ggtitle("",subtitle = "Extreme bad case") +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15),
    title = element_text(size = 15)
  )


ggarrange(gg_good, gg_bad, nrow = 2, heights = c(1, 1), common.legend = TRUE, legend="bottom")

# ATENCAO PARA MUDAR O NOME NA HORA DE SALVAR
ggsave(
    filename = "predic_prob_mnl3_extrm_bad_good_previous_notwrk.png",
  plot  = last_plot(),
  width = 8, height = 9, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Dissertacao/Graficos/")

rm(gg_good,gg_bad,ggpredict_3_extrm_bad,ggpredict_3_extrm_good)

#---Extremes bad changing characteristic----

## mudandos:
# casada_marido_seguimento - FEITO
# casada_marido_prentl     - FEITO
# educacao_mae             - FEITO
# faz_todo_trab_domestico  - FEITO
# filho_creche             - FEITO
# bayley_risco             - FEITO
# depressao_epds           - FEITO

{ #sem mudanca
ggpredict_3_extrm_bad_notwrk_sem_mud = ggpredict(mnl_3, 
                                         terms = c("raca_negra"),
                                         condition = c(casada_marido_seguimento = "Not married",
                                                       casada_marido_prentl  = "Not married",
                                                       educacao_mae = "Illiterate/elementary/middle school",
                                                       faz_todo_trab_domestico = "Does all the housework",
                                                       filho_creche = "Not on daycare",
                                                       bayley_risco = "At least one domain at risk",
                                                       ocupada_formal_prentl = "Not working",
                                                       depressao_epds = "Postpartum depression"
                                         )
) %>% mutate(previous = "Not working", mudanca = "Sem mudança")
ggpredict_3_extrm_bad_formal_sem_mud = ggpredict(mnl_3, 
                                                 terms = c("raca_negra"),
                                                 condition = c(casada_marido_seguimento = "Not married",
                                                               casada_marido_prentl  = "Not married",
                                                               educacao_mae = "Illiterate/elementary/middle school",
                                                               faz_todo_trab_domestico = "Does all the housework",
                                                               filho_creche = "Not on daycare",
                                                               bayley_risco = "At least one domain at risk",
                                                               ocupada_formal_prentl = "Formal working",
                                                               depressao_epds = "Postpartum depression"
                                                 )
) %>% mutate(previous = "Formal working", mudanca = "Sem mudança")
ggpredict_3_extrm_bad_informal_sem_mud = ggpredict(mnl_3, 
                                                   terms = c("raca_negra"),
                                                   condition = c(casada_marido_seguimento = "Not married",
                                                                 casada_marido_prentl  = "Not married",
                                                                 educacao_mae = "Illiterate/elementary/middle school",
                                                                 faz_todo_trab_domestico = "Does all the housework",
                                                                 filho_creche = "Not on daycare",
                                                                 bayley_risco = "At least one domain at risk",
                                                                 ocupada_formal_prentl = "Informal working",
                                                                 depressao_epds = "Postpartum depression"
                                                   )
) %>% mutate(previous = "Informal working", mudanca = "Sem mudança")

ggpredict_3_extrm_bad_sem= rbind(ggpredict_3_extrm_bad_formal_sem_mud,
                                 ggpredict_3_extrm_bad_informal_sem_mud,
                                 ggpredict_3_extrm_bad_notwrk_sem_mud)
} #sem mudanca

ggpredict_3_extrm_bad_notwrk_com_mud = ggpredict(mnl_3, 
                                                 terms = c("raca_negra"),
                                                 condition = c(casada_marido_seguimento = "Not married.",
                                                               casada_marido_prentl  = "Not married",
                                                               educacao_mae = "Illiterate/elementary/middle school",
                                                               faz_todo_trab_domestico = "Does all the housework",
                                                               filho_creche = "Not on daycare",
                                                               bayley_risco = "At least one domain at risk",
                                                               ocupada_formal_prentl = "Not working",
                                                               depressao_epds = "Postpartum depression"
                                                 )
) %>% mutate(previous = "Not working", mudanca = "Com mudança")

ggpredict_3_extrm_bad_formal_com_mud = ggpredict(mnl_3, 
                                                 terms = c("raca_negra"),
                                                 condition = c(casada_marido_seguimento = "Not married.",
                                                               casada_marido_prentl  = "Not married",
                                                               educacao_mae = "Illiterate/elementary/middle school",
                                                               faz_todo_trab_domestico = "Does all the housework",
                                                               filho_creche = "Not on daycare",
                                                               bayley_risco = "At least one domain at risk",
                                                               ocupada_formal_prentl = "Formal working",
                                                               depressao_epds = "Postpartum depression"
                                                 )
) %>% mutate(previous = "Formal working", mudanca = "Com mudança")

ggpredict_3_extrm_bad_informal_com_mud = ggpredict(mnl_3, 
                                                   terms = c("raca_negra"),
                                                   condition = c(casada_marido_seguimento = "Not married.",
                                                                 casada_marido_prentl  = "Not married",
                                                                 educacao_mae = "Illiterate/elementary/middle school",
                                                                 faz_todo_trab_domestico = "Does all the housework",
                                                                 filho_creche = "Not on daycare",
                                                                 bayley_risco = "At least one domain at risk",
                                                                 ocupada_formal_prentl = "Informal working",
                                                                 depressao_epds = "Postpartum depression"
                                                   )
) %>% mutate(previous = "Informal working", mudanca = "Com mudança")

ggpredict_3_extrm_bad_com= rbind(ggpredict_3_extrm_bad_formal_com_mud,
                                 ggpredict_3_extrm_bad_informal_com_mud,
                                 ggpredict_3_extrm_bad_notwrk_com_mud)

rm(ggpredict_3_extrm_bad_formal_sem_mud, ggpredict_3_extrm_bad_formal_com_mud,
   ggpredict_3_extrm_bad_informal_sem_mud,ggpredict_3_extrm_bad_informal_com_mud,
   ggpredict_3_extrm_bad_notwrk_sem_mud,ggpredict_3_extrm_bad_notwrk_com_mud)


gg_sem = ggplot(ggpredict_3_extrm_bad_sem %>% filter(previous=="Not working"), 
                aes(x = x, y = predicted, fill = response.level)) + 
  geom_bar(width = 0.6, stat = "identity",  position = position_dodge(width = 0.8)) +
  # facet_wrap(~previous=="Formal working") +
  ylab("Predicted probab.") +
  ylim(0,0.85) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Working status after pregn.")+ 
  ggtitle("Not working during pregnancy",subtitle = "Not changing") +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15),
    title = element_text(size = 15)
  )

gg_com = ggplot(ggpredict_3_extrm_bad_com %>% filter(previous == "Not working"), 
                aes(x = x, y = predicted, fill = response.level)) + 
  geom_bar(width = 0.6, stat = "identity",  position = position_dodge(width = 0.8)) +
  # facet_wrap(~response.level) +
  ylab("Predicted probab.") +
  ylim(0,0.85) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", name = "Working status after pregn.")+
  ggtitle("",subtitle = "Changing marital status after pregnancy") +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 15),
    title = element_text(size = 15)
  )


ggarrange(gg_sem, gg_com, nrow = 2, heights = c(1, 1), common.legend = TRUE, legend="bottom")


# ATENCAO PARA MUDAR O NOME NA HORA DE SALVAR
ggsave(
  filename = "pred_prob_mnl3_extrm_bad_previous_notwrk_chang_casad_seguim.png",
  plot  = last_plot(),
  width = 8, height = 9, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Dissertacao/Graficos/Mudanca_caract/")

rm(gg_com,gg_sem,ggpredict_3_extrm_bad_sem,ggpredict_3_extrm_bad_com)

#---- Predict Probabilities Extremes-----
options(scipen=999) # para não ter resultados com notacao cientifica
mnl_6$coefnames

min_densid_morador_dormitor_prentl = min(brisa_estimacoes$densid_morador_dormitor_prentl, na.rm = T)
mean_idade_mae_seguim = mean(brisa_estimacoes$idade_mae_seguim, na.rm = T)
min_quantos_filhos_tem_nascim = min(brisa_estimacoes$quantos_filhos_tem_nascim, na.rm = T)
mean_idade_bebe_seguim = mean(brisa_estimacoes$idade_bebe_seguim, na.rm = T)

less_vuln = predict(mnl_6, 
                    newdata = data.frame(raca_negra = c("Non-black"),
                                         densid_morador_dormitor_prentl = c(min_densid_morador_dormitor_prentl),
                                         educacao_mae = c("Undergraduate compl./incompl."),
                                         casada_marido_seguimento = c("Married"),
                                         idade_mae_seguim = c(mean_idade_mae_seguim),
                                         quantos_filhos_tem_nascim = c(min_quantos_filhos_tem_nascim),
                                         ocupada_formal_prentl = c("Formal working"),
                                         ipv_gravidez_recorrente = c("No violence"),
                                         suporte_social_cuidados = c("Have social support"),
                                         faz_todo_trab_domestico = c("Share the housework"),
                                         hospital_particular = c("Private"),
                                         idade_bebe_seguim = c(mean_idade_bebe_seguim),
                                         depressao_epds = c("No postpartum depression"),
                                         vinculo_mae_bebe_prejudicada = c("No broken bond with baby"),
                                         dominio_cognitivo = c("No risk at cognitive domain"),
                                         dominio_comunic_receptiva = c("No risk at receptive communic. domain"),
                                         dominio_comunic_expressiva = c("No risk at expressive communic. domain"),
                                         dominio_motor_fino = c("No risk at fine motor domain"),
                                         dominio_motor_grosso = c("No risk at gross motor domain"),
                                         filho_creche = c("On daycare"),
                                         saude_crianca = c("Great/excellent baby's health")
                                         
                    ), 
                    type = "probs")
less_vuln

max_densid_morador_dormitor_prentl = max(brisa_estimacoes$densid_morador_dormitor_prentl, na.rm = T)
max_quantos_filhos_tem_nascim = max(brisa_estimacoes$quantos_filhos_tem_nascim, na.rm = T)

more_vuln = predict(mnl_6, 
                    newdata = data.frame(raca_negra = c("Black"),
                                         densid_morador_dormitor_prentl = c(max_densid_morador_dormitor_prentl),
                                         educacao_mae = c("Illiterate/elementary/middle school"),
                                         casada_marido_seguimento = c("Not married"),
                                         idade_mae_seguim = c(mean_idade_mae_seguim),
                                         quantos_filhos_tem_nascim = c(max_quantos_filhos_tem_nascim),
                                         ocupada_formal_prentl = c("Not working"),
                                         ipv_gravidez_recorrente = c("Recurrent"),
                                         suporte_social_cuidados = c("None or rare social support"),
                                         faz_todo_trab_domestico = c("Does all the housework"),
                                         hospital_particular = c("Public"),
                                         idade_bebe_seguim = c(mean_idade_bebe_seguim),
                                         depressao_epds = c("Postpartum depression"),
                                         vinculo_mae_bebe_prejudicada = c("Broken bond with baby"),
                                         dominio_cognitivo = c("Cognitive domain at risk"),
                                         dominio_comunic_receptiva = c("Receptive communic. domain at risk"),
                                         dominio_comunic_expressiva = c("Expressive communic. domain at risk"),
                                         dominio_motor_fino = c("Fine motor domain at risk"),
                                         dominio_motor_grosso = c("Gross motor domain at risk"),
                                         filho_creche = c("Not on daycare"),
                                         saude_crianca = c("Bad/regular baby's health")
                                         
                    ), 
                    type = "probs")
more_vuln


#---- Predict Probabilities tabelona-----

#os dois abaixo sao iguais, 
# mas o predict precisa definir os tipos mais detalhadamente
# e o ggpredict so aguenta no maximo 4 termos

# ggpredict(mnl_3, 
#           terms = c("raca_negra", "educacao_mae"))
# 
# 
# table_predicts = predict(mnl_teste, 
#         newdata = data.frame(raca_negra = c("Black"), 
#                              educacao_mae = c(
#                                               "Illiterate/elementary/middle school")), 
#         type = "probs")

# for-loop para fazer o predic de todos os casos (por graus de vulnerabilidade)
# para fazer aquele grafico visto no webinario

#tem que ter todas as variáveis do modelo
mnl_2$coefnames

variaveis_vulnerabilidade = c("raca_negra", "densid_morador_dormitor_prentl",
                              "educacao_mae", "casada_marido_seguimento", 
                              "ocupada_formal_prentl",
                              "idade_mae_seguim",                                         
                              "idade_mae_seguim_quadrado" ,
                              "dominio_cognitivo"          ,                              
                              "dominio_comunic_receptiva"   ,                             
                              "dominio_comunic_expressiva"   ,                            
                              "dominio_motor_fino"            ,                           
                              "dominio_motor_grosso" ,
                              "depressao_epds", "quantos_filhos_tem_nascim",
                              "idade_bebe_seguim", "ipv_gravidez_recorrente", 
                              "suporte_social_cuidados",
                              "faz_todo_trab_domestico",
                              "filho_creche",
                              "vinculo_mae_bebe_prejudicada",
                              "saude_crianca",
                              "hospital_particular")  
levels = list() 
# length_levels = list()
for (no_variable in 1:length(variaveis_vulnerabilidade)) {
  
  variable = variaveis_vulnerabilidade[no_variable]
  
  if(is.factor(brisa_filtrada[,variable])){
    
    levels[[no_variable]] = levels(brisa_filtrada[,variable])
    # length_levels[[no_variable]] = length(levels[[no_variable]])
    
  }else if(names(brisa_filtrada[variable]) == "idade_mae_seguim" |
             names(brisa_filtrada[variable]) == "idade_mae_seguim_quadrado" ){
    
    levels[[no_variable]] = mean(brisa_filtrada[,variable], na.rm = T)
  }else{
    
    levels[[no_variable]] = c(min(brisa_filtrada[,variable], na.rm = T),
               mean(brisa_filtrada[,variable], na.rm = T),
               max(brisa_filtrada[,variable], na.rm = T))
    # length_levels[[no_variable]] = 3
  }
}
rm(no_variable, variable)

combinations = crossing(raca_negra                 = levels[[1]],
                        densid_morador_dormitor_prentl = levels[[2]],
                        educacao_mae               = levels[[3]],
                        casada_marido_seguimento   = levels[[4]],
                        ocupada_formal_prentl      = levels[[5]],
                        idade_mae_seguim           = levels[[6]],
                        idade_mae_seguim_quadrado  = levels[[7]],
                        dominio_cognitivo          = levels[[8]],
                        dominio_comunic_receptiva  = levels[[9]],
                        dominio_comunic_expressiva = levels[[10]],
                        dominio_motor_fino         = levels[[11]],
                        dominio_motor_grosso       = levels[[12]],
                        depressao_epds             = levels[[13]],
                        quantos_filhos_tem_nascim  = levels[[14]],
                        idade_bebe_seguim          = levels[[15]],
                        ipv_gravidez_recorrente    = levels[[16]],
                        suporte_social_cuidados    = levels[[17]],
                        faz_todo_trab_domestico    = levels[[18]],
                        filho_creche               = levels[[19]],
                        vinculo_mae_bebe_prejudicada = levels[[20]],
                        saude_crianca              = levels[[21]],
                        hospital_particular        = levels[[22]])

colunas_vulnerabilidade = paste0("vuln_",names(combinations))

for (no_new_col in 1:length(colunas_vulnerabilidade)) {
  
  combinations[,colunas_vulnerabilidade[no_new_col]] = NA
}
rm(no_new_col, colunas_vulnerabilidade)

combinations = combinations %>%
  mutate(
    vuln_raca_negra = case_when(
      raca_negra == "Non-black" ~ 0,
      raca_negra == "Black"     ~ 1
    ),
    vuln_densid_morador_dormitor_prentl = case_when(
      round(densid_morador_dormitor_prentl,6) == 0.400000 ~ 2,
      round(densid_morador_dormitor_prentl,6) == 1.963665 ~ 1,
      round(densid_morador_dormitor_prentl,6) == 8.000000 ~ 0,
    ),
    vuln_educacao_mae = case_when(
      educacao_mae == "Illiterate/elementary/middle school" ~ 2,
      educacao_mae == "High school"                         ~ 1,
      educacao_mae == "Undergraduate compl./incompl."       ~ 0
    ),
    vuln_casada_marido_seguimento = case_when(
      casada_marido_seguimento == "Not married" ~ 1,
      casada_marido_seguimento == "Married"     ~ 0,
    ),
    vuln_ocupada_formal_prentl = case_when(
      ocupada_formal_prentl == "Not working"      ~ 2,
      ocupada_formal_prentl == "Formal working"   ~ 0,
      ocupada_formal_prentl == "Informal working" ~ 1
    ),
    vuln_depressao_epds = case_when(
      depressao_epds == "No postpartum depression" ~ 0,
      depressao_epds == "Postpartum depression"    ~ 1
    ),
    vuln_quantos_filhos_tem_nascim = case_when(
      round(quantos_filhos_tem_nascim,6) == 1.000000 ~ 0,
      round(quantos_filhos_tem_nascim,6) == 1.777462 ~ 1,
      round(quantos_filhos_tem_nascim,6) == 7.000000 ~ 2
    ),
    vuln_idade_bebe_seguim = case_when(
      round(idade_bebe_seguim,5) == 12.12320 ~ 2,
      round(idade_bebe_seguim,5) == 22.5537 ~ 1,
      round(idade_bebe_seguim,5) == 36.46817 ~ 0
    ),
    vuln_ipv_gravidez_recorrente = case_when(
      ipv_gravidez_recorrente == "No violence" ~ 0,
      ipv_gravidez_recorrente == "Once"        ~ 1,
      ipv_gravidez_recorrente == "Recurrent"   ~ 2
    ),
    vuln_suporte_social_cuidados = case_when(
      suporte_social_cuidados == "None or rare social support" ~ 1,
      suporte_social_cuidados == "Have social support"         ~ 0
    ),
    vuln_faz_todo_trab_domestico = case_when(
      faz_todo_trab_domestico == "Share the housework"    ~ 0,
      faz_todo_trab_domestico == "Does all the housework" ~ 1
    ),
    vuln_filho_creche = case_when(
      filho_creche == "Not on daycare" ~ 1,
      filho_creche == "On daycare"     ~ 0
    )
    # ,
    # vuln_vinculo_mae_bebe_prejudicada = case_when(
    #   vinculo_mae_bebe_prejudicada == "Broken bond with baby" ~ 1,
    #   vinculo_mae_bebe_prejudicada == "No broken bond with baby"     ~ 0
    # ),
    # vuln_saude_crianca = case_when(
    #   saude_crianca == "Bad/regular baby's health" ~ 1,
    #   saude_crianca == "Great/excellent baby's health"     ~ 0
    # ),
    # vuln_hospital_particular = case_when(
    #   hospital_particular == "Public"   ~ 1,
    #   hospital_particular == "Private"  ~ 0
    # ),
    # vuln_dominio_cognitivo = case_when(
    #   dominio_cognitivo == "Cognitive domain at risk"     ~ 1,
    #   dominio_cognitivo == "No risk at cognitive domain"  ~ 0
    # ),
    # vuln_dominio_comunic_receptiva = case_when(
    #   dominio_comunic_receptiva == "Receptive communic.  domain at risk"   ~ 1,
    #   dominio_comunic_receptiva == "No risk at receptive communic. domain" ~ 0
    # ),
    # vuln_dominio_comunic_expressiva = case_when(
    #   dominio_comunic_expressiva == "Expressive communic.  domain at risk"   ~ 1,
    #   dominio_comunic_expressiva == "No risk at expressive communic. domain" ~ 0
    # ),
    # vuln_dominio_motor_fino = case_when(
    #   dominio_motor_fino == "Fine motor domain at risk"    ~ 1,
    #   dominio_motor_fino == "No risk at fine motor domain" ~ 0
    # ),
    # vuln_dominio_motor_grosso = case_when(
    #   dominio_motor_grosso  == "Gross motor domain at risk"    ~ 1,
    #   dominio_motor_grosso  == "No risk at gross motor domain" ~ 0
    # )
  )

n_first = which(names(combinations) == "vuln_raca_negra")
n_last = which(names(combinations) == "vuln_filho_creche")

combinations$grau_vulnerabilidade = rowSums(combinations[,n_first:n_last], na.rm = T)

data_predict = matrix(nrow = nrow(combinations), ncol = 3) %>% data.frame() %>%
  setNames(c("Not working",   "Formal working", "Informal working")) %>%
  mutate(grau_vulnerabilidade = combinations$grau_vulnerabilidade)

for (no_combination in 1:nrow(combinations)) {
  
  new_data = combinations[no_combination,1:(n_first-1)] %>%
    setNames(names(combinations))
  
  data_predict[no_combination,1:3] = predict(mnl_2,
                                    newdata = new_data,
                                    type = "probs")
}
rm(no_combination, new_data, n_first, n_last)


combinations$nome_vulnerabilidade = do.call(paste, 
                                            c(combinations[names(combinations)[c(1,3:6,9:12)]],
                                              sep=","))
row_menor_vulner = which(combinations$grau_vulnerabilidade == 0)
row_maior_vulner = which(combinations$grau_vulnerabilidade == 18)

data_predict_mean = data_predict %>% 
  aggregate(., by = list(data_predict$grau_vulnerabilidade), FUN = "mean") %>%
  select(names(data_predict)) %>%
  mutate(label_vunerabilidade = 
           c(paste0(str_sub(combinations$nome_vulnerabilidade[row_menor_vulner],end=62),"..."),
             rep(NA,17),
             paste0(str_sub(combinations$nome_vulnerabilidade[row_maior_vulner],end=65),"...")))
rm(row_maior_vulner,row_menor_vulner)

#-- Graph - tabelona ----
ggplot(data_predict_mean,
       aes(x = grau_vulnerabilidade, y = `Not working`)) +
  geom_line(size = 1, colour ="#66C2A5") +
  theme_classic() +
  ylab("Not working") + 
  xlab("Vulnerability") +
  theme(
    axis.title.x = element_text(size = 15),
    # axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  )+
  scale_x_discrete(limits = c( seq(0, 18, 2) ) ) +
  scale_y_continuous(breaks = c( seq(round(min(data_predict_mean$`Not working`),2), 
                                     round(max(data_predict_mean$`Not working`),2), 0.04) ),
                     limits = c(round(min(data_predict_mean$`Not working`),2), 
                                round(max(data_predict_mean$`Not working`)+0.01,2)) ) +
  geom_dl(aes(label = label_vunerabilidade), 
          method = list(dl.combine("first.points", "last.points")), cex = 0.8)

ggsave(
  filename = "multin_predic_prob_notworking.png",
  plot  = last_plot(),
  width = 7, height = 6, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")

ggplot(data_predict_mean,
       aes(x = grau_vulnerabilidade, y = `Formal working`)) +
  geom_line(size = 1, colour ="#FC8D62")+
  theme_classic() +
  ylab("Formal working") + 
  xlab("Vulnerability") +
  theme(
    axis.title.x = element_text(size = 15),
    # axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  )+
  scale_x_discrete(limits = c( seq(0, 18, 2) ) ) +
  scale_y_continuous(breaks = c( seq(round(min(data_predict_mean$`Formal working`),2), 
                                     round(max(data_predict_mean$`Formal working`),2), 0.04) ),
                     limits = c(round(min(data_predict_mean$`Formal working`)-0.01,2), 
                                round(max(data_predict_mean$`Formal working`)+0.01,2)) ) +
  geom_dl(aes(label = label_vunerabilidade), 
          method = list(dl.combine("first.points", "last.points")), cex = 0.8)

ggsave(
  filename = "multin_predic_prob_formal.png",
  plot  = last_plot(),
  width = 7, height = 6, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")


ggplot(data_predict_mean,
       aes(x = grau_vulnerabilidade, y = `Informal working`)) +
  geom_line(size = 1, colour ="#8DA0CB")+
  theme_classic() +
  ylab("Informal working") + 
  xlab("Vulnerability") +
  theme(
    axis.title.x = element_text(size = 15),
    # axis.ticks.x = element_blank(),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.line    = element_blank(),
    axis.title.y = element_text(size = 15)
  )+
  scale_x_discrete(limits = c( seq(0, 18, 2) ) ) +
  scale_y_continuous(breaks = c(seq(round(min(data_predict_mean$`Informal working`),2), 
                                    round(max(data_predict_mean$`Informal working`),2), 0.02) ),
                     limits = c(round(min(data_predict_mean$`Informal working`)-0.005,2), 
                                round(max(data_predict_mean$`Informal working`)+0.01,2)) ) +
  geom_dl(aes(label = label_vunerabilidade), 
          method = list(dl.combine("first.points", "last.points")), cex = 0.8)
ggsave(
  filename = "multin_predic_prob_informal.png",
  plot  = last_plot(),
  width = 7, height = 6, units = "in",
  path  = "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Resultados/Congresso/Graficos/")


#-- Graph - extremes ----
extremes_vuln = rbind(less_vuln,more_vuln) %>% as.data.frame()
extremes_vuln

barplot(extremes_vuln$`Not working`)

ggplot(extremes_vuln, aes(x = `Not working`)) +
  geom_bar(width = 0.6, position = position_dodge(width = 0.8))
