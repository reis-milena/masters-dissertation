##################################################
## Dissertacao Mestrado - Milena Villela
## Ano: 2021
## Objetivo do codigo: preparar base de dados
## Base de dados: Coorte Brisa 2010 Sao Luis
##################################################

###### Pacotes

library(dplyr)
library(tidyverse)
library(haven)
library(stringr)
library(lubridate)

####### Lendo a base e filtrando ######

### filtrando a base para conter somente as mulheres que responderam ao 
### pre-natal, nascimento e segmento 

# brisa_lepes_sl = read_dta(
#   "C:/Users/ville/OneDrive/Área de Trabalho/Dissertação Mestrado/Programas R/milena_brisaslzpn1_.dta")
brisa_lepes_sl = read_dta(
  "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/milena_brisaslzpn1_.dta")

brisa_filtrada_sl = brisa_lepes_sl %>% filter(dataentp != "",  #data da entrevista do pre-natal
                                              dataent != "",   #data da entrevista do nascimento
                                              !is.na(a3))      #data da entrevista do seguimento
rm(brisa_lepes_sl)

####### Violencia OMS - NAO FEITO ###########
##### Depressao Pos-parto EPDS #######################################

# referencias: 
#  https://openrit.grupotiradentes.com/xmlui/bitstream/handle/set/3269/MONIQUE.pdf?sequence=1

# escala_depressao_depois foi feito só somando direto as colunas tepds, 
# pois considera que o code-book só errou na descrição mas seguiu o questionário

# Questoes 1, 2, e 4:
#  Se voce marcou a primeira resposta, nao conte pontos.
#  Se voce marcou a segunda resposta, marque um ponto.
#  Se voce marcou a terceira resposta, marque dois pontos.
#  Se voce marcou a quarta resposta, marque tres pontos

# Questoes 3, 5, 6, 7, 8, 9 e 10:
#  Se voce marcou a primeira resposta, marque tres pontos.
#  Se voce marcou a segunda resposta, marque dois pontos.
#  Se voce marcou a terceira resposta, marque um ponto.
#  Se voce marcou a quarta resposta, nao conte pontos.


brisa_filtrada_sl = brisa_filtrada_sl %>% mutate(
  #mudando respostas para calcular score pois acho que estao errados(incidencia mt alta n_depres = 937)
  #acho que respostas estao seguindo codebook
  #ainda, no codebook a descricao das variaveis estao erradas (tudo igual mas sao diferentes entre si)
  g59_3_2 = case_when(
    g59_3 == "00" ~3,
    g59_3 == "01" ~2,
    g59_3 == "02" ~1,
    g59_3 == "03" ~0
  ),
  g59_5_2 = case_when(
    g59_5 == "00" ~3,
    g59_5 == "01" ~2,
    g59_5 == "02" ~1,
    g59_5 == "03" ~0
  ),
  g59_6_2 = case_when(
    g59_6 == "00" ~3,
    g59_6 == "01" ~2,
    g59_6 == "02" ~1,
    g59_6 == "03" ~0
  ),
  g59_7_2 = case_when(
    g59_7 == "00" ~3,
    g59_7 == "01" ~2,
    g59_7 == "02" ~1,
    g59_7 == "03" ~0
  ),
  g59_8_2 = case_when(
    g59_8 == "00" ~3,
    g59_8 == "01" ~2,
    g59_8 == "02" ~1,
    g59_8 == "03" ~0
  ),
  g59_9_2 = case_when(
    g59_9 == "00" ~3,
    g59_9 == "01" ~2,
    g59_9 == "02" ~1,
    g59_9 == "03" ~0
  ),
  g59_10_2 = case_when(
    g59_10 == "00" ~3,
    g59_10 == "01" ~2,
    g59_10 == "02" ~1,
    g59_10 == "03" ~0
  ),
  #corrigndo para numerico os itens 1,2 e 4
  g59_1_2 = case_when(
    g59_1 == "00" ~0,
    g59_1 == "01" ~1,
    g59_1 == "02" ~2,
    g59_1 == "03" ~3
  ),
  g59_2_2 = case_when(
    g59_2 == "00" ~0,
    g59_2 == "01" ~1,
    g59_2 == "02" ~2,
    g59_2 == "03" ~3
  ),
  g59_4_2 = case_when(
    g59_4 == "00" ~0,
    g59_4 == "01" ~1,
    g59_4 == "02" ~2,
    g59_4 == "03" ~3
  )
) %>% 
  mutate(
    score_depressao_epds = rowSums(.[c('g59_1_2', 'g59_2_2', 'g59_4_2', 
                                       # tepds3, tepds5, tepds6,
                                       # tepds7 ,tepds8 ,tepds9,
                                       # tepds10, 
                                       'g59_3_2', 'g59_5_2', 'g59_6_2',
                                       'g59_7_2' ,'g59_8_2' ,'g59_9_2',
                                       'g59_10_2')],
                                   na.rm = T)
  ) %>%
  #para transformar de volta a base em df
  data.frame()

#11 maes nao responderam esse questionario
#corrigindo o resultado do score delas (esta 0 mas tem que estar missing)

#pegando os numeros das colunas das variaveis de depressao epds para usar no loop
num_inicio = which( colnames(brisa_filtrada_sl) == "g59_1_2" )
num_final  = which( colnames(brisa_filtrada_sl) == "g59_10_2" )


for (linha in 1:nrow(brisa_filtrada_sl)) {
  
  if( all( is.na(brisa_filtrada_sl[linha,num_inicio:num_final]))  ){
    
    brisa_filtrada_sl$score_depressao_epds[linha] = NA
    
    # print(linha)
    # print(brisa_filtrada_sl$score_depressao_epds[linha])
    
  }else{
    
    brisa_filtrada_sl$score_depressao_epds[linha] = brisa_filtrada_sl$score_depressao_epds[linha]
    
  }
  
}

rm(num_final, num_inicio, linha)

brisa_filtrada_sl = brisa_filtrada_sl  %>%
  
  mutate(depressao_epds = case_when( 
    
    #considerou-se a presenca de sintomas depressivos pos-parto quando o escore foi ??? 12."(marizelia)
    score_depressao_epds >= 12 ~ 1,
    score_depressao_epds < 12  ~ 0
  )
  )

# View(brisa_filtrada_sl %>% select(score_depressao_epds,
#                                depressao_epds,
#                                g59_1, g59_2, g59_4,
#                                g59_1_2, g59_2_2, g59_4_2,
#                                # tepds3, tepds5, tepds6,
#                                # tepds7 ,tepds8 ,tepds9,
#                                # tepds10,
#                                g59_3_2, g59_5_2, g59_6_2,
#                                g59_7_2 ,g59_8_2 ,g59_9_2,
#                                g59_10_2))

# summary(brisa_filtrada_sl$score_depressao_epds)
# table(brisa_filtrada_sl$depressao_epds, exclude = NULL)
# table(brisa_filtrada_sl$depressao_epds, brisa_filtrada_sl$depressao_cesd)


##### Classe social - densidade ######################################

## Densidade da casa

#Pre natal
# table(brisa_filtrada_sl$pessoasp, exclude = NULL)
brisa_filtrada_sl$dormitor = ifelse(brisa_filtrada_sl$dormitor == 9, 
                                   NA_real_,
                                  brisa_filtrada_sl$dormitor)

brisa_filtrada_sl$densid_morador_dormitor_prentl = c(brisa_filtrada_sl$pessoasp / 
                                                       brisa_filtrada_sl$dormitor)
##### Ocupacao - mae #################################################

### Exerce atividade remunerada (empregada VS desempregada)

# # NASCIMENTO
# table(brisa_filtrada$ativrem, exclude = NULL)
# #parou de trabalhar pq ficou gravida 
# table(brisa_filtrada$paroutrab, exclude = NULL) 

### PRE-NATAL
# table(brisa_filtrada_sl$ativremp, exclude = NULL)
# table(brisa_filtrada_sl$relacaop, exclude = NULL)
# table(brisa_filtrada_sl$relacaop, brisa_filtrada_sl$ativremp)

brisa_filtrada_sl = brisa_filtrada_sl %>% 
  mutate( ocupada_prentl = 
            case_when(
              ativremp == 1 ~ 1,
              ativremp == 2 ~ 0
              # ,
              # pnativremp == 9 ~ NA_real_
            )
  )

### SEGUIMENTO
# table(brisa_filtrada_sl$f12, exclude = NULL)
# table(brisa_filtrada_sl$f14, brisa_filtrada_sl$f12)

brisa_filtrada_sl = brisa_filtrada_sl %>% 
  mutate( ocupada_seguim = 
            case_when(
              f12 == "01"   ~ 1,
              f12 == "02"   ~ 0
              # ,
              # is.na(tativrem) ~ NA_real_
            )
  )

# table(brisa_filtrada_sl$ocupada_seguim, exclude = NULL)
##### Ocupacao formal/informal #######################################

### PRE-NATAL

# Trabalha por conta própria=1
# Assalariado ou empregado=2
# Dono de empresa-empregador=3
# Faz bico=4
# Não se aplica=8
# Nao sabe=9
# table(brisa_filtrada_sl$relacaop, exclude = NULL)
# table(brisa_filtrada_sl$relacaop, brisa_filtrada_sl$ocupada_prentl)

brisa_filtrada_sl = brisa_filtrada_sl %>%
  mutate(ocupada_formal_prentl = 
           case_when(
             relacaop %in% c(2,3) ~ 1,  #formal
             relacaop %in% c(1,4) ~ 2,  #informal
             relacaop == 8        ~ 0,  #nao ocupada
             # pnrelacaop == 9        ~ NA_real_
           )
  )

# table(brisa_filtrada$ocupada_prentl, exclude = NULL)
# table(brisa_filtrada_sl$ocupada_formal_prentl, exclude = NULL)

### SEGUIMENTO

# Trabalha por conta própria=1
# Assalariado ou empregado=2
# Dono de empresa-empregador=3
# Faz bico=4
# Não se aplica=8
# Nao sabe=9
# table(brisa_filtrada_sl$f14, exclude = NULL)
# table(brisa_filtrada_sl$f14, brisa_filtrada_sl$ocupada_seguim)


brisa_filtrada_sl = brisa_filtrada_sl %>%
  mutate(ocupada_formal_seguim = 
           case_when(
             f14 %in% c("02","03") ~ 1,  #formal
             f14 %in% c("01","04") ~ 2,  #informal
             f14 == "08"           ~ 0,  #nao ocupada
             f14 == "09"           ~ NA_real_
           )
  )

# table(brisa_filtrada$ocupada_seguim, exclude = NULL)
# table(brisa_filtrada_sl$ocupada_formal_seguim, exclude = NULL)

##### Trabalho domestico #####################################

# table(brisa_filtrada_sl$trabcasa, exclude = NULL)

brisa_filtrada_sl = brisa_filtrada_sl %>%
  mutate(faz_todo_trab_domestico = case_when(
    trabcasa == 1 ~ 1, #faz todo o trabalho
    trabcasa == 2 ~ 0, #divide o trabalho
    trabcasa %in% c(3,9) ~ NA_real_
    
  )
  ) %>%
  mutate(faz_todo_trab_domestico2 = case_when(
    trabcasa == 1 ~ 2, #faz todo o trabalho
    trabcasa == 2 ~ 1, #divide o trabalho
    trabcasa == 3 ~ 0, #outra pessoa faz o trabalho
    trabcasa == 9 ~ NA_real_
    
  )
  )
# table(brisa_filtrada_sl$faz_todo_trab_domestico, exclude = NULL)
##### Desenvolvimento Infantil - Bayley III ##########################

# Criancas com classificacao Risco

# Classificacao criada:
# 0 = nao tem risco para nenhuma das classificacoes bayley
# 1 = tem uma classificacao bayley em risco
# 2 = tem entre 2 e 4 classificacoes bayley em risco
# 3 = tem todas as cinco classificacoes bayley em risco

variaveis_bayley = c("Cognitivo"   , "com_receptiva" ,
                     "com_expressiva"  , "motor_fino"  ,
                     "motor_grosso")
nome_dominio_bayley = c("cognitivo"        , "comunic_receptiva" ,
                        "comunic_expressiva", "motor_fino"        ,
                        "motor_grosso")

for (dominio_bayley in 1:length(variaveis_bayley)) {
  
  qual_dominio = variaveis_bayley[dominio_bayley]
  
  nome_variavel_dominio = paste0("dominio_",nome_dominio_bayley[dominio_bayley])
  
  brisa_filtrada_sl[,nome_variavel_dominio] = 0
  
  for (linha in 1:nrow(brisa_filtrada_sl)) {
    
    if(brisa_filtrada_sl[linha, qual_dominio] == "Risco"){
      
      brisa_filtrada_sl[linha, nome_variavel_dominio] = 1
    }
    
    if(brisa_filtrada_sl[linha, qual_dominio] == ""){
      
      brisa_filtrada_sl[linha, nome_variavel_dominio] = NA_real_
    }
    
  }
  
}
rm(linha, dominio_bayley, qual_dominio, nome_variavel_dominio, 
   nome_dominio_bayley, variaveis_bayley)

# table(brisa_filtrada_sl$Cognitivo, exclude = NULL)
# table(brisa_filtrada_sl$dominio_cognitivo, exclude = NULL)
# 
# table(brisa_filtrada_sl$com_receptiva, exclude = NULL)
# table(brisa_filtrada_sl$dominio_comunic_receptiva, exclude = NULL)
# 
# table(brisa_filtrada_sl$com_expressiva, exclude = NULL)
# table(brisa_filtrada_sl$dominio_comunic_expressiva, exclude = NULL)
# 
# table(brisa_filtrada_sl$motor_fino, exclude = NULL)
# table(brisa_filtrada_sl$dominio_motor_fino, exclude = NULL)
# 
# table(brisa_filtrada_sl$motor_grosso, exclude = NULL)
# table(brisa_filtrada_sl$dominio_motor_grosso, exclude = NULL)

### Variavel para pelo menos algum dominio em risco
brisa_filtrada_sl = brisa_filtrada_sl %>% 
  mutate(bayley_risco = case_when(
    dominio_cognitivo == 1 | dominio_comunic_expressiva == 1 |
      dominio_comunic_receptiva == 1 | dominio_motor_fino == 1 |
      dominio_motor_grosso == 1                                  ~ 1,
    dominio_cognitivo == 0 & dominio_comunic_expressiva == 0 &
      dominio_comunic_receptiva == 0 & dominio_motor_fino == 0 &
      dominio_motor_grosso == 0                                  ~ 0
  ))

# table(brisa_filtrada_sl$bayley_risco, exclude = NULL)
# View(brisa_filtrada_sl %>% select(bayley_risco,dominio_cognitivo,
#                                dominio_comunic_receptiva, dominio_comunic_expressiva,
#                                dominio_motor_fino,dominio_motor_grosso))

##### Numero de filhos que moram com a mae ###################
# table(brisa_filtrada_sl$qtfilhosp, exclude = NULL) #pre-natal
# table(brisa_filtrada_sl$qtfilhos, exclude = NULL)  #nascimento
# xtabs(~qtfilhosp + qtfilhos, brisa_filtrada_sl)
# xtabs(~qtfilhosp + morafilhop, brisa_filtrada_sl)
# xtabs(~morafilho + qtfilhos, brisa_filtrada_sl, exclude = NULL)
#correlacao de numero de filhos no nascim e prentl = 0.8442526


brisa_filtrada_sl = brisa_filtrada_sl %>% mutate(
  quantos_filhos_mora_prentl = case_when(
    qtfilhosp  < 88                     ~ as.numeric(qtfilhosp),
    qtfilhosp == 88 & morafilhop == 2   ~ 0,
    # qtfilhosp == 88 & morafilhop == 1   ~ NA_real_,
    # qtfilhosp == 99                     ~ NA_real_
  )
)

# table(brisa_filtrada_sl$quantos_filhos_mora_prentl, exclude = NULL)

brisa_filtrada_sl = brisa_filtrada_sl %>% mutate(
  quantos_filhos_mora_nascim = case_when(
    qtfilhos  < 88                     ~ as.numeric(qtfilhos),
    qtfilhos == 88 & morafilho == 2 ~ 0,
    qtfilhos == 88 & morafilho == 1 ~ NA_real_
  )
)

# table(brisa_filtrada_sl$quantos_filhos_mora_nascim, exclude = NULL)

##### Numero de filhos que a mulher tem ##############################

# table(brisa_filtrada_sl$nfilhos, exclude = NULL) #perg. feita no nascimento

brisa_filtrada_sl$quantos_filhos_tem_nascim = ifelse(brisa_filtrada_sl$nfilhos == 99,
                                                  NA_real_,
                                                  brisa_filtrada_sl$nfilhos)

# table(brisa_filtrada_sl$quantos_filhos_tem_nascim, exclude = NULL)

##### Educacao da mae ################################################

# Alfabetização de jovens e adultos=1 e Ensino fundamental ou 1º grau=2 ~1
# Ensino médio ou 2º grau=3 ~2
# Superior graduação incompleto=4 e Superior graduação completo=5 ~3

# table(brisa_filtrada_sl$cursog, exclude = NULL)

brisa_filtrada_sl = brisa_filtrada_sl %>%  mutate(
  educacao_mae = case_when(
    cursog == 1 | cursog == 2 ~ 1,
    cursog == 3               ~ 2,
    cursog == 4 | cursog == 5 ~ 3,
    cursog %in% c(8,9)        ~ NA_real_
  ),
  educacao_mae2 = case_when(
    cursog == 1 | cursog == 2 ~ 1,
    cursog == 3 | cursog == 4 ~ 2,
    cursog == 5               ~ 3,
    cursog %in% c(8,9)        ~ NA_real_
  )
)

# table(brisa_filtrada_sl$educacao_mae, exclude = NULL)

##### Idade da mae ###################################################
# dngest dnmae
# table(brisa_filtrada_sl$dngest==brisa_filtrada_sl$dnmae)
# View(brisa_filtrada_sl %>% select(dngest, dnmae) %>% 
#        cbind(.,brisa_filtrada_sl$dngest==brisa_filtrada_sl$dnmae))

#tem maes com data de nasc diferente mas vou seguir usando a data do pre-natal
brisa_filtrada_sl = brisa_filtrada_sl %>%
  # separando as variaveis de dia, mes e ano da data de nascim da mae para
  # calcular a idade no seguimento
  mutate(dataent_seguim = case_when(
    nchar(a3) == 8 ~
      paste0(str_sub(a3,start = -8, end=-7),
             "/",
             str_sub(a3,start = -6, end=-5),
             "/",
             str_sub(a3,start = -4)),
    nchar(a3) == 7 ~ 
      paste0("0",
             str_sub(a3,start = -8, end=-7),
             "/",
             str_sub(a3,start = -6, end=-5),
             "/",
             str_sub(a3,start = -4)))
  )
#TEM UMA MAE/CRIANCA QUE FOI ENTREVISTADA DIA 29 DE FEVEREIRO E RPECISA CORRIGIR
linha_29_fev = which(brisa_filtrada_sl$dataent_seguim == "29/02/2011")
#nao existe 29 de fev. em 2012, entao assumi que era de 2012
brisa_filtrada_sl[linha_29_fev,"dataent_seguim"] = "29/02/2012"

brisa_filtrada_sl = brisa_filtrada_sl %>%
  mutate(
    idade_mae_seguim = time_length(
      difftime(as.Date(dataent_seguim,"%d/%m/%Y"), 
               as.Date(dngest,"%d/%m/%Y")),
      "years"
    ),
    idade_mae_nascim = time_length(
      difftime(as.Date(dataent,"%d/%m/%Y"), 
               as.Date(dngest,"%d/%m/%Y")),
      "years"
    )
    
  )


#corrigindo duas maes que estao com idadeerrada por conta da data de nascimento

brisa_filtrada_sl$idade_mae_nascim[which(brisa_filtrada_sl$dngest == "09/09/9999")] = NA_real_
brisa_filtrada_sl$idade_mae_nascim[which(brisa_filtrada_sl$dngest == "13/05/2011")] = NA_real_
brisa_filtrada_sl$idade_mae_nascim[which(brisa_filtrada_sl$dngest == "02/09/2010")] = NA_real_

brisa_filtrada_sl$idade_mae_seguim[which(brisa_filtrada_sl$dngest == "09/09/9999")] = NA_real_
brisa_filtrada_sl$idade_mae_seguim[which(brisa_filtrada_sl$dngest == "13/05/2011")] = NA_real_
brisa_filtrada_sl$idade_mae_seguim[which(brisa_filtrada_sl$dngest == "02/09/2010")] = NA_real_



#correlacao entre as idades no nascim e no seguim =  0.9990373

##### Idade bebe #####################################################



brisa_filtrada_sl = brisa_filtrada_sl %>%
  mutate(
    idade_bebe_seguim = time_length(
      difftime(as.Date(dataent_seguim, "%d/%m/%Y"), #data da entrevista (seguim)
               as.Date(rn1dtnasc, "%d/%m/%Y")),     #data de nascimento (nasc)
      "months"
    )
  )
rm(linha_29_fev)
# View(brisa_filtrada_sl %>%
#        select(rn1dtnasc,a3, dataent_seguim, idade_bebe_seguim))
# summary(brisa_filtrada_sl$idade_bebe_seguim)

##### Cor/raca mae ################################################################

# Branca                      =1
# Preta/negra                 =2
# Parda/mulata/cabocla/morena =3
# Amarelo/oriental            =4
# Ind?gena                    =5
# N?o sabe                    =9


brisa_filtrada_sl = brisa_filtrada_sl %>% mutate(
  raca_negra = case_when(
    cormae == 2 |cormae == 3                ~ 1,
    cormae == 1 | cormae == 4 | cormae == 5 ~ 0
  )
)
# table(brisa_filtrada_sl$raca_negra, exclude=NULL)
# prop.table(table(brisa_filtrada_sl$raca_negra, exclude=NULL))

##### Casada ##########################
# Casada=1
# Uni?o consensual (Mora junto)=2
# Solteira=3
# Separada/desquitada/divorciada=4
# Vi?va=5
# N?o sabe=9


#antes
# table(brisa_filtrada_sl$sitcong, exclude = NULL)

brisa_filtrada_sl = brisa_filtrada_sl %>% mutate(
  casada_marido_prentl = case_when(
    sitcong == 1 | sitcong == 2                   ~ 1,
    sitcong == 3 | sitcong == 4 |  sitcong == 5   ~ 0
  )
)
# prop.table(table(brisa_filtrada_sl$casada_marido_prentl))
# table(brisa_filtrada_sl$casada_marido_prentl, exclude = NULL)

#depois
# table(brisa_filtrada_sl$f9, exclude = NULL)

brisa_filtrada_sl = brisa_filtrada_sl %>% mutate(
  casada_marido_seguimento = case_when(
    f9 == "01" | f9 == "02"           ~ 1,
    f9 == "03" | f9 == "04" | f9 == "05" ~ 0
  )
)
# prop.table(table(brisa_filtrada_sl$casada_marido_seguimento))

#correlacao entre os casamentos = 0.5807703

##### Crianca na creche/escola #######################################

# table(brisa_filtrada_sl$c2, exclude = NULL)

brisa_filtrada_sl$filho_creche = ifelse(brisa_filtrada_sl$c2 == "01",
                                     yes = 1,
                                     ifelse(brisa_filtrada_sl$c2 == "02",
                                            yes = 0,
                                            no = NA_real_))

# table(brisa_filtrada_sl$filho_creche, exclude = NULL)


##### Exportando a base ########################################

# A base esta sendo exportada pois as estimacoes serao feitas no stat.anova

#uses a comma (",") for the decimal point and a semicolon (";") for the separator.
# write.table(brisa_filtrada_sl,
#             "C:\\Users\\ville\\OneDrive\\Área de Trabalho\\Dissertação Mestrado\\Programas R\\brisa_final_sl.csv",
#             row.names = F,
#             dec = ".",
#             sep = ";",
#             na = ".")
