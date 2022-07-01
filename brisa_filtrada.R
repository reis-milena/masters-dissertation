##################################################
## Dissertacao Mestrado - Milena Villela
## Ano: 2021
## Objetivo do codigo: preparar base de dados
## Base de dados: Coorte Brisa 2010 Ribeirao Preto
##################################################

###### Pacotes

library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)

####### Lendo a base e filtrando ######

### filtrando a base para conter somente as mulheres que responderam ao 
### pre-natal, nascimento e segmento  


#dados "antigos"
# brisa_lepes = read.csv(
#   "C:\\Users\\ville\\OneDrive\\?rea de Trabalho\\Disserta??o Mestrado\\Programas R\\lepes-brisa.csv")
# brisa_lepes = read.csv(
#   "C:\\Users\\ville\\OneDrive\\Área de Trabalho\\Dissertação Mestrado\\Programas R\\Milena-lepes.csv")
brisa_lepes = read.csv(
  "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/Milena-lepes.csv")


brisa_filtrada = brisa_lepes %>% filter(gestante != "",
                                        mae != "",
                                        tnomemae != "")

# View(brisa_filtrada%>%select(gestante,mae,tnomemae))

rm(brisa_lepes)

####### Violencia OMS ###########

# (BLOCO T)

### Nesse bloco h? dois momentos em que perguntam sobre viol?ncia: "durante a gravidez" e 
### "12 meses antes da gravidez". Assim ser? calculados vari?veis de viol?ncia para cada
### momento. 
###
### Como ? constitu?do esse bloco?
### Tem 13 perguntas de viol?ncia (pnoms1:pnoms13 e pnoms17:pnoms29) e, logo depois destas
### 13 perguntas h? uma pergunta de quem perpetrou essa viol?ncia (pnoms14 e pnoms30).

# table( brisa_filtrada$pnoms14 ) # durante a gravidez
# table( brisa_filtrada$pnoms30 ) # 12 meses antes da gravidez

### O que significa cada valor da vari?vel do perpetrador (pnoms14 e pnoms30):

# Atual Marido / companheiro / namorado = 01          
# Ex-marido / companheiro / namorado = 02          
# Pai = 03       
# Padrasto = 04       
# M?e = 05
# Madrasta = 06  
# Irm?o, irm? ou outro familiar que mora na mesma resid?ncia que voc? = 07
# Familiar que n?o reside com voc? = 08
# Vizinho ou outra pessoa conhecida = 09  
# Outros:___________ = 10 
# Atual marido/companheiro/namorado e M?e = 11
# Ex-marido/companheiro/namorado e M?e = 12
# Irm?o e M?e = 13
# Pai e Marido = 14
# Pai e Ex-marido = 15
# Padrasto e irm?o = 16
# N?o quis responder = 77
# N?o houve viol?ncia = 88 

### O que significa cada valor das perguntas de viol?ncia (pnoms1:pnoms13 e pnoms17:pnoms29)

# N?o = 0
# Uma vez = 1    
# Poucas vezes = 2    
# Muitas vezes = 3  
# 7 e 9 n?o est?o definidos no dicion?rio

##### DURANTE A GRAVIDEZ ##################################

### Corrigindo Violacao de Fluxo ###

### H? viola??o de fluxo nesses dados de viol?ncia, ent?o eles ser?o corrigidos a seguir.

### Tem mulher que respondeu que sofreu algum dos tipos de viol?ncia nos itens pnoms1:pnoms13, 
### mas na pergunta do perpetrador (pnoms14) falou que n?o sofreu viol?ncia.

### Como ser? corrigido?
### Nesse caso ser? mudado o valor da pnoms14 para 77 (n?o quis responder)

### Loop para verificar quantos casos ocorrem essa viola??o de fluxo
quantos_casos = 0
quais_linhas = c()
for( linha in 1:nrow(brisa_filtrada) ){
  try(
    if(
       ( brisa_filtrada$pnoms1[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms2[ linha ] %in% c(1, 2, 3) |
        brisa_filtrada$pnoms3[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms4[ linha ] %in% c(1, 2, 3) |
        brisa_filtrada$pnoms5[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms6[ linha ] %in% c(1, 2, 3) |
        brisa_filtrada$pnoms7[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms8[ linha ] %in% c(1, 2, 3) | 
        brisa_filtrada$pnoms9[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms10[ linha ] %in% c(1, 2, 3) | 
        brisa_filtrada$pnoms11[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms12[ linha ] %in% c(1, 2, 3) | 
        brisa_filtrada$pnoms13[ linha ] %in% c(1, 2, 3) ) & 
       brisa_filtrada$pnoms14[ linha ] == 88 )
      {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
      })
}
quantos_casos #39
quais_linhas

### Corrigindo esses casos de viola??o
brisa_filtrada$pnoms14[quais_linhas] = 77


rm(quantos_casos, linha, quais_linhas)

### Observando os casos em que a pessoa respondeu que n?o sofreu viol?ncia nos itens 
### pnoms1:pnoms13, mas respondeu algu?m no item pnoms14

quantos_casos = 0
quais_linhas = c()
for( linha in 1:nrow(brisa_filtrada) ){
  try(
    if(
      ( brisa_filtrada$pnoms1[ linha ] == 0 & brisa_filtrada$pnoms2[ linha ] == 0 &
        brisa_filtrada$pnoms3[ linha ] == 0 & brisa_filtrada$pnoms4[ linha ] == 0 &
        brisa_filtrada$pnoms5[ linha ] == 0 & brisa_filtrada$pnoms6[ linha ] == 0 &
        brisa_filtrada$pnoms7[ linha ] == 0 & brisa_filtrada$pnoms8[ linha ] == 0 & 
        brisa_filtrada$pnoms9[ linha ] == 0 & brisa_filtrada$pnoms10[ linha ] == 0 & 
        brisa_filtrada$pnoms11[ linha ] == 0 & brisa_filtrada$pnoms12[ linha ] == 0 & 
        brisa_filtrada$pnoms13[ linha ] == 0 ) & 
      brisa_filtrada$pnoms14[ linha ] < 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos # casos - perpetradores: 10, 15 e 77
quais_linhas

# View( brisa_filtrada[quais_linhas,] %>% select(pnoms14, pnoms1:pnoms13) )

rm(quantos_casos, linha, quais_linhas)


#### Vari?vel Binaria - Violencia ###############################

### viol?ncia psicol?gica = pnoms1-pnoms4
### viol?ncia f?sica = pnoms5-pnoms10
### viol?ncia sexual = pnoms11-pnoms13

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_gravidez_psic = case_when(
    pnoms1 %in% c(1, 2, 3) | pnoms2 %in% c(1, 2, 3) | pnoms3 %in% c(1, 2, 3) | pnoms4 %in% c(1, 2, 3) ~ 1,
    
    pnoms1 == 0 & pnoms2 == 0 & pnoms3 == 0 & pnoms4 == 0 ~ 0
  ),
  viol_gravidez_fisica = case_when(
    pnoms5 %in% c(1, 2, 3) | pnoms6 %in% c(1, 2, 3) | pnoms7 %in% c(1, 2, 3) | pnoms8 %in% c(1, 2, 3) | 
      pnoms9 %in% c(1, 2, 3) | pnoms10 %in% c(1, 2, 3) ~ 1,
    
    pnoms5 == 0 & pnoms6 == 0 &pnoms7 == 0 & pnoms8 == 0 & pnoms9 == 0 & pnoms10 == 0 ~0
  ),
  viol_gravidez_sexual = case_when(
    pnoms11 %in% c(1, 2, 3) | pnoms12 %in% c(1, 2, 3) |pnoms13 %in% c(1, 2, 3) ~ 1,
    
    pnoms11 == 0 & pnoms12 == 0 & pnoms13 == 0 ~ 0
  ),
  viol_gravidez = case_when(
    viol_gravidez_psic==1 | viol_gravidez_fisica==1 | viol_gravidez_sexual==1 | pnoms14 < 77 ~ 1,
    
    ( viol_gravidez_psic==0 & viol_gravidez_fisica==0 & viol_gravidez_sexual==0 ) | pnoms14 > 77 ~ 0
  )
)
# View(brisa_filtrada %>% select(pnoms14, viol_gravidez, viol_gravidez_psic, viol_gravidez_fisica, viol_gravidez_sexual))
# View(brisa_filtrada %>% select(pnoms14, viol_gravidez_psic, pnoms1:pnoms4))
# View(brisa_filtrada %>% select(pnoms14, viol_gravidez_fisica, pnoms5:pnoms10))


# table(brisa_filtrada$viol_gravidez) ; sum(is.na(brisa_filtrada$viol_gravidez)) 
# table(brisa_filtrada$viol_gravidez, brisa_filtrada$pnoms14)
# table(brisa_filtrada$viol_gravidez_psic) ; table(brisa_filtrada$viol_gravidez_fisica) ; table(brisa_filtrada$viol_gravidez_sexual)
# sum(is.na(brisa_filtrada$viol_gravidez_psic)) ; sum(is.na(brisa_filtrada$viol_gravidez_fisica)) ; sum(is.na(brisa_filtrada$viol_gravidez_sexual))

### Interse??es

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_gravidez_psic_fisica = case_when(
    
    viol_gravidez_psic == 1 & viol_gravidez_fisica == 1 ~ 1,
    
    viol_gravidez_psic %in% c(0, 1) | viol_gravidez_fisica %in% c(0, 1) | viol_gravidez == 0 ~ 0
  ),
  viol_gravidez_psic_sexual = case_when(
    
    viol_gravidez_psic == 1 & viol_gravidez_sexual == 1 ~ 1,
    
    viol_gravidez_psic %in% c(0, 1) | viol_gravidez_sexual %in% c(0, 1) | viol_gravidez == 0 ~ 0
  ),
  viol_gravidez_sexual_fisica = case_when(
    
    viol_gravidez_sexual == 1 & viol_gravidez_fisica == 1 ~ 1,
    
    viol_gravidez_sexual %in% c(0, 1) | viol_gravidez_fisica %in% c(0, 1) | viol_gravidez == 0 ~ 0
  ),
  viol_gravidez_psic_sexual_fisica = case_when(
    
    viol_gravidez_sexual == 1 & viol_gravidez_fisica == 1  & viol_gravidez_psic == 1 ~ 1,
    
    viol_gravidez_sexual %in% c(0, 1) | viol_gravidez_fisica %in% c(0, 1) | 
      viol_gravidez_psic %in% c(0, 1) | viol_gravidez == 0 ~ 0
  )
)

# View(brisa_filtrada %>% select(pnoms14, viol_gravidez_psic_fisica, viol_gravidez_psic, viol_gravidez_fisica, viol_gravidez, pnoms1:pnoms13))
# View(brisa_filtrada %>% select(pnoms14, viol_gravidez_psic_sexual_fisica, viol_gravidez, viol_gravidez_psic, viol_gravidez_fisica, viol_gravidez_sexual, pnoms1:pnoms13))


# table(brisa_filtrada$viol_gravidez_psic_fisica) ; sum(is.na(brisa_filtrada$viol_gravidez_psic_fisica))
# table(brisa_filtrada$viol_gravidez_psic_sexual) ; sum(is.na(brisa_filtrada$viol_gravidez_psic_sexual))
# table(brisa_filtrada$viol_gravidez_sexual_fisica) ; sum(is.na(brisa_filtrada$viol_gravidez_sexual_fisica))
# table(brisa_filtrada$viol_gravidez_psic_sexual_fisica) ; sum(is.na(brisa_filtrada$viol_gravidez_psic_sexual_fisica))

#### Vari?vel BinAria - Violencia Domestica ########
# pnoms14 = 01
# Atual Marido / companheiro / namorado = 01  

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_gravidez_domest_psic = case_when(
    ( pnoms1 %in% c(1, 2, 3) | pnoms2 %in% c(1, 2, 3) | pnoms3 %in% c(1, 2, 3) | pnoms4 %in% c(1,2,3) ) &
      pnoms14 == 1 ~ 1,
    
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |    # psicol?gica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |  # psicol?gica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |  # psicol?gica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |  # psicol?gica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # f?sica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      pnoms14 %in% c(2:16, 88) ~ 0
  ),
  viol_gravidez_domest_fisica = case_when(
    ( pnoms5 %in% c(1, 2, 3) | pnoms6 %in% c(1, 2, 3) | pnoms7 %in% c(1, 2, 3) | pnoms8 %in% c(1,2,3) |
       pnoms9 %in% c(1, 2, 3) | pnoms10 %in% c(1,2,3) ) & pnoms14 == 1 ~1,
    
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |    # psicol?gica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) | # f?sica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      pnoms14 %in% c(2:16, 88) ~ 0
  ),
  viol_gravidez_domest_sexual = case_when(
    ( pnoms11 %in% c(1, 2, 3) | pnoms12 %in% c(1, 2, 3) |pnoms13 %in% c(1,2,3) ) & pnoms14 == 1 ~ 1,
    
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |    # psicol?gica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # f?sica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(2:16, 88) ) | # sexual
      pnoms14 %in% c(2:16, 88) ~ 0
  ),
  viol_gravidez_domest = case_when(
    viol_gravidez_domest_psic==1 | viol_gravidez_domest_fisica==1 | viol_gravidez_domest_sexual==1 ~ 1,
    
    viol_gravidez_domest_psic==0 & viol_gravidez_domest_fisica==0 & viol_gravidez_domest_sexual==0 ~ 0
  )
)
# View( brisa_filtrada %>% select(viol_gravidez_domest, viol_gravidez_domest_psic, viol_gravidez_domest_fisica, viol_gravidez_domest_sexual))
# View(brisa_filtrada %>% select(pnoms14, viol_gravidez_domest_psic, pnoms1:pnoms13) )
# View(brisa_filtrada %>% select(pnoms14,viol_gravidez_domest_fisica, pnoms5:pnoms10)%>% filter(pnoms14 %in% c(1:16)))
# View(brisa_filtrada %>% select(pnoms14,viol_gravidez_domest_sexual, pnoms11:pnoms13))

# table(brisa_filtrada$viol_gravidez_domest) ; sum(is.na(brisa_filtrada$viol_gravidez_domest)) 
# table(brisa_filtrada$viol_gravidez_domest_psic) ; table(brisa_filtrada$viol_gravidez_domest_fisica) ; table(brisa_filtrada$viol_gravidez_domest_sexual)
# sum(is.na(brisa_filtrada$viol_gravidez_domest_psic)) ; sum(is.na(brisa_filtrada$viol_gravidez_domest_fisica)) ; sum(is.na(brisa_filtrada$viol_gravidez_domest_sexual))

### Interse??es

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_gravidez_domest_psic_fisica = case_when(
    
    viol_gravidez_domest_psic == 1 & viol_gravidez_domest_fisica == 1 ~ 1,
    
    viol_gravidez_domest_psic %in% c(0, 1) | viol_gravidez_domest_fisica %in% c(0, 1) | viol_gravidez_domest == 0 ~ 0
  ),
  viol_gravidez_domest_psic_sexual = case_when(
    
    viol_gravidez_domest_psic == 1 & viol_gravidez_domest_sexual == 1 ~ 1,
    
    viol_gravidez_domest_psic %in% c(0, 1) | viol_gravidez_domest_sexual %in% c(0, 1) | viol_gravidez_domest == 0 ~ 0
  ),
  viol_gravidez_domest_sexual_fisica = case_when(
    
    viol_gravidez_domest_sexual == 1 & viol_gravidez_domest_fisica == 1 ~ 1,
    
    viol_gravidez_domest_sexual %in% c(0, 1) | viol_gravidez_domest_fisica %in% c(0, 1) | viol_gravidez_domest == 0 ~ 0
  ),
  viol_gravidez_domest_psic_sexual_fisica = case_when(
    
    viol_gravidez_domest_sexual == 1 & viol_gravidez_domest_fisica == 1  & viol_gravidez_domest_psic == 1 ~ 1,
    
    viol_gravidez_domest_sexual %in% c(0, 1) | viol_gravidez_domest_fisica %in% c(0, 1) | 
      viol_gravidez_domest_psic %in% c(0, 1) | viol_gravidez_domest == 0 ~ 0
  )
)

# View(brisa_filtrada %>% 
#        select(pnoms14, viol_gravidez_domest_psic_fisica, 
#               viol_gravidez_domest_psic, viol_gravidez_domest_fisica, 
#               viol_gravidez_domest, pnoms1:pnoms13))
# View(brisa_filtrada %>% 
#        select(pnoms14, viol_gravidez_domest_psic_sexual_fisica, 
#               viol_gravidez_domest, viol_gravidez_domest_psic, 
#               viol_gravidez_domest_fisica, viol_gravidez_domest_sexual, 
#               pnoms1:pnoms13))

# table(brisa_filtrada$viol_gravidez_domest_psic_fisica) ; sum(is.na(brisa_filtrada$viol_gravidez_domest_psic_fisica))
# table(brisa_filtrada$viol_gravidez_domest_psic_sexual) ; sum(is.na(brisa_filtrada$viol_gravidez_domest_psic_sexual))
# table(brisa_filtrada$viol_gravidez_domest_sexual_fisica) ; sum(is.na(brisa_filtrada$viol_gravidez_domest_sexual_fisica))
# table(brisa_filtrada$viol_gravidez_domest_psic_sexual_fisica) ; sum(is.na(brisa_filtrada$viol_gravidez_domest_psic_sexual_fisica))


#### Vari?vel Binaria - Violencia Domestica - incluindo o EX ####
# pnoms14 = 01
# Atual Marido / companheiro / namorado = 01  
# pnoms14 = 02
# Ex-Marido / companheiro / namorado = 02  

brisa_filtrada = brisa_filtrada %>% mutate(
  
  viol_gravidez_domest_ex_psic = case_when(
    ( pnoms1 %in% c(1, 2, 3) | pnoms2 %in% c(1, 2, 3) | 
        pnoms3 %in% c(1, 2, 3) | pnoms4 %in% c(1,2,3) ) &
      pnoms14 %in% c(1, 2)                                          ~ 1,
    
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |    # psicol?gica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # psicol?gica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # psicol?gica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # psicol?gica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # f?sica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      pnoms14 %in% c(3:16, 88)                                       ~ 0
  ),
  
  viol_gravidez_domest_ex_fisica = case_when(
    ( pnoms5 %in% c(1, 2, 3) | pnoms6 %in% c(1, 2, 3) | 
        pnoms7 %in% c(1, 2, 3) | pnoms8 %in% c(1,2,3) |
        pnoms9 %in% c(1, 2, 3) | pnoms10 %in% c(1,2,3) ) & pnoms14 %in% c(1, 2) ~1,
    
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |    # psicol?gica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) | # f?sica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      pnoms14 %in% c(3:16, 88) ~ 0
  ),
  viol_gravidez_domest_ex_sexual = case_when(
    ( pnoms11 %in% c(1, 2, 3) | pnoms12 %in% c(1, 2, 3) |
        pnoms13 %in% c(1,2,3) ) & pnoms14 %in% c(1, 2) ~ 1,
    
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |    # psicol?gica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # f?sica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) | # sexual
      pnoms14 %in% c(3:16, 88) ~ 0
  ),
  viol_gravidez_domest_ex = case_when(
    viol_gravidez_domest_ex_psic==1 | viol_gravidez_domest_ex_fisica==1 | 
      viol_gravidez_domest_ex_sexual==1                                     ~ 1,
    
    viol_gravidez_domest_ex_psic==0 & viol_gravidez_domest_ex_fisica==0 & 
      viol_gravidez_domest_ex_sexual==0                                     ~ 0
  )
)
# View( brisa_filtrada %>% select(viol_gravidez_domest_ex, viol_gravidez_domest_ex_psic, 
#                                 viol_gravidez_domest_ex_fisica, viol_gravidez_domest_ex_sexual))
# View(brisa_filtrada %>% select(pnoms14, viol_gravidez_domest_ex_psic, pnoms1:pnoms13) )
# View(brisa_filtrada %>% select(pnoms14,viol_gravidez_domest_ex_fisica,
#                                pnoms5:pnoms10) %>%
#        filter(pnoms14 %in% c(1:16)))
# View(brisa_filtrada %>% select(pnoms14,viol_gravidez_domest_ex_sexual, pnoms11:pnoms13))

# table(brisa_filtrada$viol_gravidez_domest_ex) ; sum(is.na(brisa_filtrada$viol_gravidez_domest_ex)) 
# table(brisa_filtrada$viol_gravidez_domest_ex_psic) ; table(
#   brisa_filtrada$viol_gravidez_domest_ex_fisica) ; table(
#     brisa_filtrada$viol_gravidez_domest_ex_sexual)
# sum(is.na(brisa_filtrada$viol_gravidez_domest_ex_psic)) ; sum(
#   is.na(brisa_filtrada$viol_gravidez_domest_ex_fisica)) ; sum(
#     is.na(brisa_filtrada$viol_gravidez_domest_ex_sexual))

### Interse??es

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_gravidez_domest_ex_psic_fisica = case_when(
    
    viol_gravidez_domest_ex_psic == 1 & viol_gravidez_domest_ex_fisica == 1 ~ 1,
    
    viol_gravidez_domest_ex_psic %in% c(0, 1) | viol_gravidez_domest_ex_fisica %in% c(0, 1) |
      viol_gravidez_domest_ex == 0 ~ 0
  ),
  viol_gravidez_domest_ex_psic_sexual = case_when(
    
    viol_gravidez_domest_ex_psic == 1 & viol_gravidez_domest_ex_sexual == 1 ~ 1,
    
    viol_gravidez_domest_ex_psic %in% c(0, 1) | viol_gravidez_domest_ex_sexual %in% c(0, 1) | 
      viol_gravidez_domest_ex == 0 ~ 0
  ),
  viol_gravidez_domest_ex_sexual_fisica = case_when(
    
    viol_gravidez_domest_ex_sexual == 1 & viol_gravidez_domest_ex_fisica == 1 ~ 1,
    
    viol_gravidez_domest_ex_sexual %in% c(0, 1) | viol_gravidez_domest_ex_fisica %in% c(0, 1) | 
      viol_gravidez_domest_ex == 0 ~ 0
  ),
  viol_gravidez_domest_ex_psic_sexual_fisica = case_when(
    
    viol_gravidez_domest_ex_sexual == 1 & viol_gravidez_domest_ex_fisica == 1  & 
      viol_gravidez_domest_ex_psic == 1 ~ 1,
    
    viol_gravidez_domest_ex_sexual %in% c(0, 1) | viol_gravidez_domest_ex_fisica %in% c(0, 1) | 
      viol_gravidez_domest_ex_psic %in% c(0, 1) | viol_gravidez_domest_ex == 0 ~ 0
  )
)

# View(brisa_filtrada %>% select(pnoms14, viol_gravidez_domest_ex_psic_fisica, 
#                                viol_gravidez_domest_ex_psic, viol_gravidez_domest_ex_fisica, 
#                                viol_gravidez_domest_ex, pnoms1:pnoms13))
# View(brisa_filtrada %>% select(pnoms14, viol_gravidez_domest_ex_psic_sexual_fisica, 
#                                viol_gravidez_domest_ex, viol_gravidez_domest_ex_psic, 
#                                viol_gravidez_domest_ex_fisica, viol_gravidez_domest_ex_sexual, 
#                                pnoms1:pnoms13))

# table(brisa_filtrada$viol_gravidez_domest_ex_psic_fisica) ; sum(
#   is.na(brisa_filtrada$viol_gravidez_domest_ex_psic_fisica))
# table(brisa_filtrada$viol_gravidez_domest_ex_psic_sexual) ; sum(
#   is.na(brisa_filtrada$viol_gravidez_domest_ex_psic_sexual))
# table(brisa_filtrada$viol_gravidez_domest_ex_sexual_fisica) ; sum(
#   is.na(brisa_filtrada$viol_gravidez_domest_ex_sexual_fisica))
# table(brisa_filtrada$viol_gravidez_domest_ex_psic_sexual_fisica) ; sum(
#   is.na(brisa_filtrada$viol_gravidez_domest_ex_psic_sexual_fisica))


##### Violencia recorrende - IPV ###########################

# IPV = viol. domestica (marido e analogos) +  viol. do ex

# violencia recorrente: quando a resposta foi "poucas vezes" ou "muitas vezes"

brisa_filtrada = brisa_filtrada %>% mutate(
  
  ipv_gravidez_recorrente_psic = case_when(
    #violencia 1 episodio
    ( pnoms1 %in% c(1) | pnoms2 %in% c(1) | 
        pnoms3 %in% c(1) | pnoms4 %in% c(1) ) &
      pnoms14 %in% c(1, 2)                                          ~ 1,
    
    #violencia recorrente
    ( pnoms1 %in% c(2, 3) | pnoms2 %in% c(2, 3) | 
        pnoms3 %in% c(2, 3) | pnoms4 %in% c(2, 3) ) &
      pnoms14 %in% c(1, 2)                                          ~ 2,
    
    #nao sofreu violencia
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |    # psicologica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # psicologica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # psicologica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # psicologica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # fisica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      pnoms14 %in% c(3:16, 88)                                       ~ 0
  ),
  
  ipv_gravidez_recorrente_fisica = case_when(
    #violencia 1 episodio
    (   pnoms5 %in% c(1) | pnoms6 %in% c(1) | 
          pnoms7 %in% c(1) | pnoms8 %in% c(1) |
          pnoms9 %in% c(1) | pnoms10 %in% c(1) ) & 
      pnoms14 %in% c(1, 2)                                          ~ 1,
    
    #violencia recorrente
    (   pnoms5 %in% c(2, 3) | pnoms6 %in% c(2, 3) | 
        pnoms7 %in% c(2, 3) | pnoms8 %in% c(2, 3) |
        pnoms9 %in% c(2, 3) | pnoms10 %in% c(2, 3) ) & 
      pnoms14 %in% c(1, 2)                                          ~ 2,
    
    #nao sofreu violencia
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |    # psicologica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicologica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicologica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicologica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # fisica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # fisica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # fisica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # fisica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) |  # fisica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) | # fisica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # sexual
      pnoms14 %in% c(3:16, 88)                                       ~ 0
  ),
  ipv_gravidez_recorrente_sexual = case_when(
    #violencia 1 episodio
    ( pnoms11 %in% c(1) | pnoms12 %in% c(1) |
        pnoms13 %in% c(1) ) & 
      pnoms14 %in% c(1, 2)                                          ~ 1,
    
    #violencia recorrente
    ( pnoms11 %in% c(2, 3) | pnoms12 %in% c(2, 3) |
        pnoms13 %in% c(2, 3) ) & 
      pnoms14 %in% c(1, 2)                                          ~ 2,
    
    #nao sofreu violencia
    ( pnoms1 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |    # psicologica
      ( pnoms2 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicologica
      ( pnoms3 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicologica
      ( pnoms4 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # psicologica
      ( pnoms5 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms6 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms7 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms8 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms9 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) |  # fisica
      ( pnoms10 %in% c(0, 1, 2, 3) & pnoms14 %in% c(1:16, 88) ) | # fisica
      ( pnoms11 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) | # sexual
      ( pnoms12 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) | # sexual
      ( pnoms13 %in% c(0, 1, 2, 3) & pnoms14 %in% c(3:16, 88) ) | # sexual
      pnoms14 %in% c(3:16, 88)                                    ~ 0
  ),
  ipv_gravidez_recorrente = case_when(
    #violencia 1 episodio
    ipv_gravidez_recorrente_psic == 1 | ipv_gravidez_recorrente_fisica == 1 | 
      ipv_gravidez_recorrente_sexual == 1                                      ~ 1,
    
    #violencia recorrente
    ipv_gravidez_recorrente_psic == 2 | ipv_gravidez_recorrente_fisica == 2 | 
      ipv_gravidez_recorrente_sexual == 2                                      ~ 2,
    
    #nao sofreu violencia
    ipv_gravidez_recorrente_psic == 0 & ipv_gravidez_recorrente_fisica == 0 & 
      ipv_gravidez_recorrente_sexual == 0                                      ~ 0
  )
)

# table(brisa_filtrada$ipv_gravidez_recorrente, exclude = NULL)
# table(brisa_filtrada$ipv_gravidez_recorrente_psic, exclude = NULL)
# table(brisa_filtrada$ipv_gravidez_recorrente_fisica, exclude = NULL)
# table(brisa_filtrada$ipv_gravidez_recorrente_sexual, exclude = NULL)


##### 12 MESES ANTES DA GRAVIDEZ ##########################

### Corrigindo Violacao de Fluxo ###

### H? viola??o de fluxo nesses dados de viol?ncia, ent?o eles ser?o corrigidos a seguir.

### Tem mulher que respondeu que sofreu algum dos tipos de viol?ncia nos itens pnoms17:pnoms29, 
### mas na pergunta do perpetrador (pnoms30) falou que n?o sofreu viol?ncia.

### Como ser? corrigido?
### Nesse caso ser? mudado o valor da pnoms30 para 77 (n?o quis responder)

### Loop para verificar quantos casos ocorrem essa viola??o de fluxo
quantos_casos = 0
quais_linhas = c()
for( linha in 1:nrow(brisa_filtrada) ){
  try(
    if(
      ( brisa_filtrada$pnoms17[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms18[ linha ] %in% c(1, 2, 3) |
        brisa_filtrada$pnoms19[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms20[ linha ] %in% c(1, 2, 3) |
        brisa_filtrada$pnoms21[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms22[ linha ] %in% c(1, 2, 3) |
        brisa_filtrada$pnoms23[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms24[ linha ] %in% c(1, 2, 3) | 
        brisa_filtrada$pnoms25[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms26[ linha ] %in% c(1, 2, 3) | 
        brisa_filtrada$pnoms27[ linha ] %in% c(1, 2, 3) | brisa_filtrada$pnoms28[ linha ] %in% c(1, 2, 3) | 
        brisa_filtrada$pnoms29[ linha ] %in% c(1, 2, 3) ) & 
      brisa_filtrada$pnoms30[ linha ] == 88 )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    })
}
quantos_casos #30
quais_linhas

### Corrigindo esses casos de viola??o
brisa_filtrada$pnoms30[quais_linhas] = 77


rm(quantos_casos, linha, quais_linhas)

### Observando os casos em que a pessoa respondeu que n?o sofreu viol?ncia nos itens 
### pnoms17:pnoms29, mas respondeu algu?m no item pnoms30

quantos_casos = 0
quais_linhas = c()
for( linha in 1:nrow(brisa_filtrada) ){
  try(
    if(
      ( brisa_filtrada$pnoms17[ linha ] == 0 & brisa_filtrada$pnoms18[ linha ] == 0 &
        brisa_filtrada$pnoms19[ linha ] == 0 & brisa_filtrada$pnoms20[ linha ] == 0 &
        brisa_filtrada$pnoms21[ linha ] == 0 & brisa_filtrada$pnoms22[ linha ] == 0 &
        brisa_filtrada$pnoms23[ linha ] == 0 & brisa_filtrada$pnoms24[ linha ] == 0 & 
        brisa_filtrada$pnoms25[ linha ] == 0 & brisa_filtrada$pnoms26[ linha ] == 0 & 
        brisa_filtrada$pnoms27[ linha ] == 0 & brisa_filtrada$pnoms28[ linha ] == 0 & 
        brisa_filtrada$pnoms29[ linha ] == 0 ) & 
      brisa_filtrada$pnoms30[ linha ] < 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos # casos - perpetradores: 1, 10, 11 e 77
quais_linhas

# View( brisa_filtrada[quais_linhas,] %>% select(pnoms30, pnoms17:pnoms29) )

rm(quantos_casos, linha, quais_linhas)


#### Variavel Binaria - Violencia ###############################
# table( brisa_filtrada$pnoms30 )

#violencia psicol?gica = pnoms17-pnoms20
#violencia f?sica = pnoms21-pnoms26
#violencia sexual = pnoms27-pnoms29

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_12meses_psic = case_when(
    pnoms17 %in% c(1, 2, 3) | pnoms18 %in% c(1, 2, 3) | pnoms19 %in% c(1, 2, 3) | pnoms20 %in% c(1, 2, 3) ~ 1,
    
    pnoms17 == 0 & pnoms18 == 0 & pnoms19 == 0 & pnoms20 == 0 ~ 0
  ),
  viol_12meses_fisica = case_when(
    pnoms21 %in% c(1, 2, 3) | pnoms22 %in% c(1, 2, 3) | pnoms23 %in% c(1, 2, 3) | pnoms24 %in% c(1, 2, 3) | 
      pnoms25 %in% c(1, 2, 3) | pnoms26 %in% c(1, 2, 3) ~ 1,
    
    pnoms21 == 0 & pnoms22 == 0 & pnoms23 == 0 & pnoms24 == 0 & pnoms25 == 0 & pnoms26 == 0 ~0
  ),
  viol_12meses_sexual = case_when(
    pnoms27 %in% c(1, 2, 3) | pnoms28 %in% c(1, 2, 3) | pnoms29 %in% c(1, 2, 3) ~ 1,
    
    pnoms27 == 0 & pnoms28 == 0 & pnoms29 == 0 ~ 0
  ),
  viol_12meses = case_when(
    viol_12meses_psic==1 | viol_12meses_fisica==1 | viol_12meses_sexual==1 | pnoms30 < 77 ~ 1,
    
    ( viol_12meses_psic==0 & viol_12meses_fisica==0 & viol_12meses_sexual==0 ) | pnoms30 > 77 ~ 0
  )
)
# View(brisa_filtrada %>% select(pnoms30, viol_12meses, viol_12meses_psic, viol_12meses_fisica, viol_12meses_sexual))
# View(brisa_filtrada %>% select(pnoms30, viol_12meses_psic, pnoms17:pnoms20))
# View(brisa_filtrada %>% select(pnoms30, viol_12meses_fisica, pnoms21:pnoms26))

# table(brisa_filtrada$viol_12meses) ; sum(is.na(brisa_filtrada$viol_12meses)) 
# table(brisa_filtrada$viol_12meses, brisa_filtrada$pnoms30)
# table(brisa_filtrada$viol_12meses_psic) ; table(brisa_filtrada$viol_12meses_fisica) ; table(brisa_filtrada$viol_12meses_sexual)
# sum(is.na(brisa_filtrada$viol_12meses_psic)) ; sum(is.na(brisa_filtrada$viol_12meses_fisica)) ; sum(is.na(brisa_filtrada$viol_12meses_sexual))

### Interse??es

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_12meses_psic_fisica = case_when(
    
    viol_12meses_psic == 1 & viol_12meses_fisica == 1 ~ 1,
    
    viol_12meses_psic %in% c(0, 1) | viol_12meses_fisica %in% c(0, 1) | viol_12meses == 0 ~ 0
  ),
  viol_12meses_psic_sexual = case_when(
    
    viol_12meses_psic == 1 & viol_12meses_sexual == 1 ~ 1,
    
    viol_12meses_psic %in% c(0, 1) | viol_12meses_sexual %in% c(0, 1) | viol_12meses == 0 ~ 0
  ),
  viol_12meses_sexual_fisica = case_when(
    
    viol_12meses_sexual == 1 & viol_12meses_fisica == 1 ~ 1,
    
    viol_12meses_sexual %in% c(0, 1) | viol_12meses_fisica %in% c(0, 1) | viol_12meses == 0 ~ 0
  ),
  viol_12meses_psic_sexual_fisica = case_when(
    
    viol_12meses_sexual == 1 & viol_12meses_fisica == 1  & viol_12meses_psic == 1 ~ 1,
    
    viol_12meses_sexual %in% c(0, 1) | viol_12meses_fisica %in% c(0, 1) | 
      viol_12meses_psic %in% c(0, 1) | viol_12meses == 0 ~ 0
  )
)

# View(brisa_filtrada %>% select(pnoms14, viol_12meses_psic_fisica, viol_12meses_psic, viol_12meses_fisica, viol_12meses, pnoms1:pnoms13))
# View(brisa_filtrada %>% select(pnoms14, viol_12meses_psic_sexual_fisica, viol_12meses, viol_12meses_psic, viol_12meses_fisica, viol_12meses_sexual, pnoms1:pnoms13))


# table(brisa_filtrada$viol_12meses_psic_fisica) ; sum(is.na(brisa_filtrada$viol_12meses_psic_fisica))
# table(brisa_filtrada$viol_12meses_psic_sexual) ; sum(is.na(brisa_filtrada$viol_12meses_psic_sexual))
# table(brisa_filtrada$viol_12meses_sexual_fisica) ; sum(is.na(brisa_filtrada$viol_12meses_sexual_fisica))
# table(brisa_filtrada$viol_12meses_psic_sexual_fisica) ; sum(is.na(brisa_filtrada$viol_12meses_psic_sexual_fisica))

#### Variavel Binaria - Violencia Domestica ########
# pnoms30 = 01
# Atual Marido / companheiro / namorado = 01  

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_12meses_domest_psic = case_when(
    ( pnoms17 %in% c(1, 2, 3) | pnoms18 %in% c(1, 2, 3) | pnoms19 %in% c(1, 2, 3) | 
        pnoms20 %in% c(1,2,3) ) &
      pnoms30 == 1                                                                     ~ 1,
    
    ( pnoms17 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |    # psicol?gica
      ( pnoms18 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |  # psicol?gica
      ( pnoms19 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |  # psicol?gica
      ( pnoms20 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |  # psicol?gica
      ( pnoms21 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms22 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms23 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms24 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms25 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms26 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # f?sica
      ( pnoms27 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      ( pnoms28 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      ( pnoms29 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      pnoms30 %in% c(2:16, 88)                                                       ~ 0
  ),
  viol_12meses_domest_fisica = case_when(
    ( pnoms21 %in% c(1, 2, 3) | pnoms22 %in% c(1, 2, 3) | pnoms23 %in% c(1, 2, 3) | 
        pnoms24 %in% c(1,2,3) |
        pnoms25 %in% c(1, 2, 3) | pnoms26 %in% c(1,2,3) ) & pnoms30 == 1             ~1,
    
    ( pnoms17 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |    # psicol?gica
      ( pnoms18 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms19 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms20 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms21 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms22 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms23 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms24 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms25 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) |  # f?sica
      ( pnoms26 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) | # f?sica
      ( pnoms27 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      ( pnoms28 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      ( pnoms29 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      pnoms30 %in% c(2:16, 88)                                                      ~ 0
  ),
  viol_12meses_domest_sexual = case_when(
    ( pnoms27 %in% c(1, 2, 3) | pnoms28 %in% c(1, 2, 3) |pnoms29 %in% c(1,2,3) ) & 
      pnoms30 == 1                                                                  ~ 1,
    
    ( pnoms17 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |    # psicol?gica
      ( pnoms18 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms19 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms20 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms21 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms22 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms23 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms24 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms25 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms26 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # f?sica
      ( pnoms27 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) | # sexual
      ( pnoms28 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) | # sexual
      ( pnoms29 %in% c(0, 1, 2, 3) & pnoms30 %in% c(2:16, 88) ) | # sexual
      pnoms30 %in% c(2:16, 88)                                                       ~ 0
  ),
  viol_12meses_domest = case_when(
    viol_12meses_domest_psic==1 | viol_12meses_domest_fisica==1 | viol_12meses_domest_sexual==1 ~ 1,
    
    viol_12meses_domest_psic==0 & viol_12meses_domest_fisica==0 & viol_12meses_domest_sexual==0 ~ 0
  )
)
# View( brisa_filtrada %>% select(viol_12meses_domest, viol_12meses_domest_psic,
#                                 viol_12meses_domest_fisica, viol_12meses_domest_sexual))
# View(brisa_filtrada %>% select(pnoms30, viol_12meses_domest_psic, pnoms17:pnoms20) )
# View(brisa_filtrada %>% select(pnoms30,viol_12meses_domest_fisica,
#                                pnoms21:pnoms26)%>%
#        filter(pnoms30 %in% c(1:16)))
# View(brisa_filtrada %>% select(pnoms30,viol_12meses_domest_sexual, pnoms27:pnoms29))

# table(brisa_filtrada$viol_12meses_domest) ; sum(
#   is.na(brisa_filtrada$viol_12meses_domest))
# table(brisa_filtrada$viol_12meses_domest_psic) ; table(
#   brisa_filtrada$viol_12meses_domest_fisica) ; table(
#     brisa_filtrada$viol_12meses_domest_sexual)
# sum(is.na(brisa_filtrada$viol_12meses_domest_psic)) ; sum(
#   is.na(brisa_filtrada$viol_12meses_domest_fisica)) ; sum(
#     is.na(brisa_filtrada$viol_12meses_domest_sexual))

### Interse??es

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_12meses_domest_psic_fisica = case_when(
    
    viol_12meses_domest_psic == 1 & viol_12meses_domest_fisica == 1                     ~ 1,
    
    viol_12meses_domest_psic %in% c(0, 1) | viol_12meses_domest_fisica %in% c(0, 1) | 
      viol_12meses_domest == 0                                                          ~ 0
  ),
  viol_12meses_domest_psic_sexual = case_when(
    
    viol_12meses_domest_psic == 1 & viol_12meses_domest_sexual == 1                     ~ 1,
    
    viol_12meses_domest_psic %in% c(0, 1) | viol_12meses_domest_sexual %in% c(0, 1) | 
      viol_12meses_domest == 0                                                          ~ 0
  ),
  viol_12meses_domest_sexual_fisica = case_when(
    
    viol_12meses_domest_sexual == 1 & viol_12meses_domest_fisica == 1                   ~ 1,
    
    viol_12meses_domest_sexual %in% c(0, 1) | viol_12meses_domest_fisica %in% c(0, 1) | 
      viol_12meses_domest == 0                                                          ~ 0
  ),
  viol_12meses_domest_psic_sexual_fisica = case_when(
    
    viol_12meses_domest_sexual == 1 & viol_12meses_domest_fisica == 1  & 
      viol_12meses_domest_psic == 1                                                     ~ 1,
    
    viol_12meses_domest_sexual %in% c(0, 1) | viol_12meses_domest_fisica %in% c(0, 1) | 
      viol_12meses_domest_psic %in% c(0, 1) | viol_12meses_domest == 0                  ~ 0
  )
)

# View(brisa_filtrada %>% select(pnoms14, viol_12meses_domest_psic_fisica, 
#                                viol_12meses_domest_psic, viol_12meses_domest_fisica, 
#                                viol_12meses_domest, pnoms1:pnoms13))
# View(brisa_filtrada %>% select(pnoms14, viol_12meses_domest_psic_sexual_fisica, 
#                                viol_12meses_domest, viol_12meses_domest_psic, 
#                                viol_12meses_domest_fisica, viol_12meses_domest_sexual, pnoms1:pnoms13))
# 
# table(brisa_filtrada$viol_12meses_domest_psic_fisica) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_psic_fisica))
# table(brisa_filtrada$viol_12meses_domest_psic_sexual) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_psic_sexual))
# table(brisa_filtrada$viol_12meses_domest_sexual_fisica) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_sexual_fisica))
# table(brisa_filtrada$viol_12meses_domest_psic_sexual_fisica) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_psic_sexual_fisica))


#### Variavel Binaria - Violencia Domestica - incluindo o EX ####
# pnoms30 = 01
# Atual Marido / companheiro / namorado = 01  
# pnoms30 = 02
# Ex-Marido / companheiro / namorado = 02  

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_12meses_domest_ex_psic = case_when(
    ( pnoms17 %in% c(1, 2, 3) | pnoms18 %in% c(1, 2, 3) | pnoms19 %in% c(1, 2, 3) | 
        pnoms20 %in% c(1,2,3) ) &
      pnoms30 %in% c(1, 2)                                                                 ~ 1,
    
    ( pnoms17 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |    # psicol?gica
      ( pnoms18 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |  # psicol?gica
      ( pnoms19 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |  # psicol?gica
      ( pnoms20 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |  # psicol?gica
      ( pnoms21 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms22 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms23 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms24 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms25 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms26 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # f?sica
      ( pnoms27 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      ( pnoms28 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      ( pnoms29 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      pnoms30 %in% c(3:16, 88) ~ 0
  ),
  viol_12meses_domest_ex_fisica = case_when(
    ( pnoms21 %in% c(1, 2, 3) | pnoms22 %in% c(1, 2, 3) | pnoms23 %in% c(1, 2, 3) | 
        pnoms24 %in% c(1,2,3) |
        pnoms25 %in% c(1, 2, 3) | pnoms26 %in% c(1,2,3) ) & pnoms30 %in% c(1, 2)            ~1,
    
    ( pnoms17 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |    # psicol?gica
      ( pnoms18 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms19 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms20 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms21 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms22 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms23 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms24 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms25 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) |  # f?sica
      ( pnoms26 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) | # f?sica
      ( pnoms27 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      ( pnoms28 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      ( pnoms29 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # sexual
      pnoms30 %in% c(3:16, 88) ~ 0
  ),
  viol_12meses_domest_ex_sexual = case_when(
    ( pnoms27 %in% c(1, 2, 3) | pnoms28 %in% c(1, 2, 3) |pnoms29 %in% c(1,2,3) ) 
    & pnoms30 %in% c(1, 2)                                                                ~ 1,
    
    ( pnoms17 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |    # psicol?gica
      ( pnoms18 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms19 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms20 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # psicol?gica
      ( pnoms21 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms22 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms23 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms24 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms25 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) |  # f?sica
      ( pnoms26 %in% c(0, 1, 2, 3) & pnoms30 %in% c(1:16, 88) ) | # f?sica
      ( pnoms27 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) | # sexual
      ( pnoms28 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) | # sexual
      ( pnoms29 %in% c(0, 1, 2, 3) & pnoms30 %in% c(3:16, 88) ) | # sexual
      pnoms30 %in% c(3:16, 88) ~ 0
  ),
  viol_12meses_domest_ex = case_when(
    viol_12meses_domest_ex_psic==1 | viol_12meses_domest_ex_fisica==1 | 
      viol_12meses_domest_ex_sexual==1                                                  ~ 1,
    
    viol_12meses_domest_ex_psic==0 & viol_12meses_domest_ex_fisica==0 & 
      viol_12meses_domest_ex_sexual==0                                                  ~ 0
  )
)
# View( brisa_filtrada %>% 
#         select(viol_12meses_domest_ex, viol_12meses_domest_ex_psic, 
#                viol_12meses_domest_ex_fisica, viol_12meses_domest_ex_sexual))
# View(brisa_filtrada %>% select(pnoms30, viol_12meses_domest_ex_psic, pnoms1:pnoms13) )
# View(brisa_filtrada %>% select(pnoms30,viol_12meses_domest_ex_fisica, pnoms5:pnoms10)
#      %>% filter(pnoms30 %in% c(1:16)))
# View(brisa_filtrada %>% select(pnoms30,viol_12meses_domest_ex_sexual, pnoms11:pnoms13))

# table(brisa_filtrada$viol_12meses_domest_ex) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_ex))
# table(brisa_filtrada$viol_12meses_domest_ex_psic) ; table(
#   brisa_filtrada$viol_12meses_domest_ex_fisica) ; table(brisa_filtrada$viol_12meses_domest_ex_sexual)
# sum(is.na(brisa_filtrada$viol_12meses_domest_ex_psic)) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_ex_fisica)) ; sum(is.na(
#     brisa_filtrada$viol_12meses_domest_ex_sexual))

### Interse??es

brisa_filtrada = brisa_filtrada %>% mutate(
  viol_12meses_domest_ex_psic_fisica = case_when(
    
    viol_12meses_domest_ex_psic == 1 & viol_12meses_domest_ex_fisica == 1                    ~ 1,
    
    viol_12meses_domest_ex_psic %in% c(0, 1) | viol_12meses_domest_ex_fisica %in% c(0, 1) | 
      viol_12meses_domest_ex == 0                                                            ~ 0
  ),
  viol_12meses_domest_ex_psic_sexual = case_when(
    
    viol_12meses_domest_ex_psic == 1 & viol_12meses_domest_ex_sexual == 1                    ~ 1,
    
    viol_12meses_domest_ex_psic %in% c(0, 1) | viol_12meses_domest_ex_sexual %in% c(0, 1) |
      viol_12meses_domest_ex == 0                                                            ~ 0
  ),
  viol_12meses_domest_ex_sexual_fisica = case_when(
    
    viol_12meses_domest_ex_sexual == 1 & viol_12meses_domest_ex_fisica == 1                  ~ 1,
    
    viol_12meses_domest_ex_sexual %in% c(0, 1) | viol_12meses_domest_ex_fisica %in% c(0, 1) |
      viol_12meses_domest_ex == 0                                                            ~ 0
  ),
  viol_12meses_domest_ex_psic_sexual_fisica = case_when(
    
    viol_12meses_domest_ex_sexual == 1 & viol_12meses_domest_ex_fisica == 1  & 
      viol_12meses_domest_ex_psic == 1                                                       ~ 1,
    
    viol_12meses_domest_ex_sexual %in% c(0, 1) | viol_12meses_domest_ex_fisica %in% c(0, 1) | 
      viol_12meses_domest_ex_psic %in% c(0, 1) | viol_12meses_domest_ex == 0                 ~ 0
  )
)

# View(brisa_filtrada %>% 
#        select(pnoms14, viol_12meses_domest_ex_psic_fisica, 
#               viol_12meses_domest_ex_psic, viol_12meses_domest_ex_fisica, 
#               viol_12meses_domest_ex, pnoms1:pnoms13))
# View(brisa_filtrada %>% 
#        select(pnoms14, viol_12meses_domest_ex_psic_sexual_fisica, 
#               viol_12meses_domest_ex, viol_12meses_domest_ex_psic, 
#               viol_12meses_domest_ex_fisica, viol_12meses_domest_ex_sexual, pnoms1:pnoms13))

# table(brisa_filtrada$viol_12meses_domest_ex_psic_fisica) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_ex_psic_fisica))
# table(brisa_filtrada$viol_12meses_domest_ex_psic_sexual) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_ex_psic_sexual))
# table(brisa_filtrada$viol_12meses_domest_ex_sexual_fisica) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_ex_sexual_fisica))
# table(brisa_filtrada$viol_12meses_domest_ex_psic_sexual_fisica) ; sum(is.na(
#   brisa_filtrada$viol_12meses_domest_ex_psic_sexual_fisica))



##### Bloco Q - Discriminacao ########################################

#### Discriminacao por ra?a/cor ###################################

## pneod1:pneod9 lugares e frequ?ncia para discrimina??o por RACA/COR
## respostas:
# 0 = N?o
# 1 = uma
# 2 = duas ou tr?s
# 3 = quatro ou mais
# 7 = n?o quis responder
# 9 = n?o sabe

# table(brisa_filtrada$pneod1, exclude=NULL)
# table(brisa_filtrada$pneod2, exclude=NULL)
# table(brisa_filtrada$pneod9, exclude=NULL)

brisa_filtrada = brisa_filtrada %>%
  mutate(
    discrim_raca_escola = case_when(
      pneod1 %in% c(1, 2, 3) ~ 1,
      pneod1 == 0            ~ 0
    ),
    discrim_raca_obter_emprego = case_when(
      pneod2 %in% c(1, 2, 3) ~ 1,
      pneod2 == 0            ~ 0
    ),
    discrim_raca_trabalho = case_when(
      pneod3 %in% c(1, 2, 3) ~ 1,
      pneod3 == 0            ~ 0
    ),
    discrim_raca_proc_casa = case_when(
      pneod4 %in% c(1, 2, 3) ~ 1,
      pneod4 == 0            ~ 0
    ),
    discrim_raca_medico = case_when(
      pneod5 %in% c(1, 2, 3) ~ 1,
      pneod5 == 0            ~ 0
    ),
    discrim_raca_loja = case_when(
      pneod6 %in% c(1, 2, 3) ~ 1,
      pneod6 == 0            ~ 0
    ),
    discrim_raca_credito = case_when(
      pneod7 %in% c(1, 2, 3) ~ 1,
      pneod7 == 0            ~ 0
    ),
    discrim_raca_rua = case_when(
      pneod8 %in% c(1, 2, 3) ~ 1,
      pneod8 == 0            ~ 0
    ),
    discrim_raca_policia = case_when(
      pneod9 %in% c(1, 2, 3) ~ 1,
      pneod9 == 0            ~ 0
    ),
    discrim_raca = case_when(
      discrim_raca_escola == 1     | discrim_raca_obter_emprego == 1 |
        discrim_raca_trabalho == 1 | discrim_raca_proc_casa == 1     |
        discrim_raca_medico == 1   | discrim_raca_loja == 1          |
        discrim_raca_credito == 1  | discrim_raca_rua == 1           |
        discrim_raca_policia == 1                              ~ 1,
      
      discrim_raca_escola ==0     & discrim_raca_obter_emprego ==0 &
        discrim_raca_trabalho ==0 & discrim_raca_proc_casa ==0     &
        discrim_raca_medico ==0   & discrim_raca_loja ==0          &
        discrim_raca_credito ==0  & discrim_raca_rua ==0           &
        discrim_raca_policia ==0                              ~ 0
    ),
    discrim_raca_relacionado_ocupacao = 
      case_when(
        discrim_raca_obter_emprego == 1 | discrim_raca_trabalho == 1 ~ 1,
        discrim_raca_obter_emprego == 0 & discrim_raca_trabalho == 0 ~ 0
      )
  )


# table(brisa_filtrada$discrim_raca, exclude=NULL)
# table(brisa_filtrada$discrim_raca_relacionado_ocupacao, exclude=NULL)
# table(brisa_filtrada$discrim_raca, brisa_filtrada$discrim_raca_relacionado_ocupacao)
# prop.table(table(brisa_filtrada$discrim_raca, exclude=NULL))

#### Discriminacao em outras questoes

### Corrigindo violacao de fluxo ###

### H? viola??o de fluxo nesses dados de discriminacao, entao eles ser?o corrigidos a seguir.

### Tem mulher que respondeu que sofreu discriminacao em algum lugar nos itens pnwq1, pnwq4 
### pnwq7, pnwq10 e pnwq13 
### mas na pergunta da causa de discriminacao respondeu que nao sofreu (88).

### Como ser? corrigido?
### Nesse caso ser? mudado o valor dos itens acima para 77 (n?o quis responder)

### Loop para verificar quantos casos ocorrem essa viola??o de fluxo
quantos_casos= 0
quais_linhas = c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      (brisa_filtrada$pnwq1[linha] == 1  & brisa_filtrada$pnwq2[linha]   == 88 ) |
      (brisa_filtrada$pnwq4[linha] == 1  & brisa_filtrada$pnwq5[linha]   == 88 ) |
      (brisa_filtrada$pnwq7[linha] == 1  & brisa_filtrada$pnwq8[linha]   == 88 ) |
      (brisa_filtrada$pnwq10[linha] == 1 & brisa_filtrada$pnwq11[linha]  == 88 ) |
      (brisa_filtrada$pnwq13[linha] == 1 & brisa_filtrada$pnwq14[linha]  == 88 ) 
      
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #9
quais_linhas

### Corrigindo esses casos de viola??o caso a caso

### PNWQ1 e PNWQ2
quantos_casos= 0
quais_linhas = c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq1[linha] == 1  & brisa_filtrada$pnwq2[linha]   == 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #4
quais_linhas

brisa_filtrada$pnwq2[quais_linhas] = 77

### PNWQ4 e PNWQ5
quantos_casos= 0
quais_linhas = c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq4[linha] == 1  & brisa_filtrada$pnwq5[linha]   == 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #1
quais_linhas

brisa_filtrada$pnwq5[quais_linhas] = 77
# 
# ### PNWQ7 e PNWQ8
# quantos_casos= 0
# quais_linhas = c()
# for (linha in 1:nrow(brisa_filtrada)) {
#   try(
#     if(
#       brisa_filtrada$pnwq7[linha] == 1  & brisa_filtrada$pnwq8[linha]   == 88 
#     )
#     {
#       quantos_casos = quantos_casos + 1
#       quais_linhas = c( quais_linhas,  linha  )
#     }) 
# }
# quantos_casos #0
# quais_linhas
# 
# brisa_filtrada$pnwq8[quais_linhas] = 77

### PNWQ10 e PNWQ11
quantos_casos= 0
quais_linhas = c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq10[linha] == 1  & brisa_filtrada$pnwq11[linha]   == 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #3
quais_linhas

brisa_filtrada$pnwq11[quais_linhas] = 77

### PNWQ13 e PNWQ14
quantos_casos= 0
quais_linhas = c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq13[linha] == 1  & brisa_filtrada$pnwq14[linha]   == 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #1
quais_linhas

brisa_filtrada$pnwq14[quais_linhas] = 77

rm(quantos_casos, linha, quais_linhas)

### Observando os casos em que a pessoa respondeu que n?o sofreu discriminacao nos itens 
### pnwq1, pnwq4, pnwq7, pnwq10 e pnwq13, mas respondeu a causa nos item pnwq2, pnwq5, 
### pnwq8, pnwq11 e pnwq14

quantos_casos= 0
quais_linhas= c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      (brisa_filtrada$pnwq1[linha] == 2  & brisa_filtrada$pnwq2[linha]   != 88 ) |
      (brisa_filtrada$pnwq4[linha] == 2  & brisa_filtrada$pnwq5[linha]   != 88 ) |
      (brisa_filtrada$pnwq7[linha] == 2  & brisa_filtrada$pnwq8[linha]   != 88 ) |
      (brisa_filtrada$pnwq10[linha] == 2 & brisa_filtrada$pnwq11[linha]  != 88 ) |
      (brisa_filtrada$pnwq13[linha] == 2 & brisa_filtrada$pnwq14[linha]  != 88 )
      
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #pelo menos 52 (essa violacao acontece para cada par de item na mesma linha)
quais_linhas

### PNWQ1 e PNWQ2
quantos_casos= 0
quais_linhas= c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq1[linha] == 2  & brisa_filtrada$pnwq2[linha]   != 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #36
quais_linhas

brisa_filtrada$pnwq1[quais_linhas] = 1

### PNWQ4 e PNWQ5
quantos_casos= 0
quais_linhas= c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq4[linha] == 2  & brisa_filtrada$pnwq5[linha] != 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #25
quais_linhas

brisa_filtrada$pnwq4[quais_linhas] = 1

### PNWQ7 e PNWQ8
quantos_casos= 0
quais_linhas= c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq7[linha] == 2  & brisa_filtrada$pnwq8[linha]   != 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #16
quais_linhas

brisa_filtrada$pnwq7[quais_linhas] = 1

### PNWQ10 e PNWQ11
quantos_casos= 0
quais_linhas= c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq10[linha] == 2  & brisa_filtrada$pnwq11[linha]   != 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #13
quais_linhas

brisa_filtrada$pnwq10[quais_linhas] = 1

### PNWQ13 e PNWQ14
quantos_casos= 0
quais_linhas= c()
for (linha in 1:nrow(brisa_filtrada)) {
  try(
    if(
      brisa_filtrada$pnwq13[linha] == 2  & brisa_filtrada$pnwq14[linha]   != 88 
    )
    {
      quantos_casos = quantos_casos + 1
      quais_linhas = c( quais_linhas,  linha  )
    }) 
}
quantos_casos #14
quais_linhas

brisa_filtrada$pnwq13[quais_linhas] = 1

rm(linha, quais_linhas, quantos_casos)

#### Discriminacao por varios motivos #############################

# variaveis que perguntam por qual motivo vc sofreu discrimina??o em tal
# local: pnwq2 (local de trabalho), pnwq5 (moradia)   , pnwq8 (pol?cia),
#        pnwq11 (locais p?blicos) , pnwq14 (col?gio/faculdade)

# motivos: 
# 01. Sua cor ou ra?a=01
# 02. Ser mulher=02
# 03. Sua religi?o ou culto=03
# 04. Doen?a ou defici?ncia f?sica=04
# 05. Sua op??o ou prefer?ncia sexual=05
# 06. Sua condi??o econ?mica, instru??o ou fun??o=06
# 07. Sua atividade pol?tica=07
# 08. Sua idade=08
# 09. Sua apar?ncia f?sica=09
# 10. Outra  -> Qual? ______________________________=10
# 77. N?o quis responder=77
# 88. N?o se aplica=88
# 99. N?o sabe=99

# table(brisa_filtrada$pnwq2) # local de trabalho
# table(brisa_filtrada$pnwq5) # moradia

brisa_filtrada = brisa_filtrada %>%
  mutate(
    discrim_raca2 = case_when(
      pnwq2 == 1 | pnwq5 == 1 | pnwq8 == 1 | 
        pnwq11 == 1 | pnwq14 == 1                           ~ 1,
      
      pnwq2 %in% c(2:10,88) | pnwq5 %in% c(2:10,88) |
        pnwq8 %in% c(2:10,88) | pnwq11 %in% c(2:10,88) | 
        pnwq14 %in% c(2:10,88)                            ~ 0
    ),
    discrim_mulher = case_when(
      pnwq2 == 2 | pnwq5 == 2 | pnwq8 == 2 | 
        pnwq11 == 2 | pnwq14 == 2                           ~ 1,
      
      pnwq2 %in% c(1,3:10,88) | pnwq5 %in% c(1,3:10,88) |
        pnwq8 %in% c(1,3:10,88) | pnwq11 %in% c(1,3:10,88) | 
        pnwq14 %in% c(1,3:10,88)                            ~ 0
    ),
    discrim_religiao = case_when(
      pnwq2 == 3 | pnwq5 == 3 | pnwq8 == 3 | 
        pnwq11 == 3 | pnwq14 == 3                           ~ 1,
      
      pnwq2 %in% c(1,2,4:10,88) | pnwq5 %in% c(1,2,4:10,88) |
        pnwq8 %in% c(1,2,4:10,88) | pnwq11 %in% c(1,2,4:10,88) | 
        pnwq14 %in% c(1,2,4:10,88)                          ~ 0
    ),
    
    discrim_econ_educ = case_when(
      pnwq2 == 6 | pnwq5 == 6 | pnwq8 == 6 | 
        pnwq11 == 6 | pnwq14 == 6                           ~ 1,
      
      pnwq2 %in% c(1:5,7:10,88) | pnwq5 %in% c(1:5,7:10,88) |
        pnwq8 %in% c(1:5,7:10,88) | pnwq11 %in% c(1:5,7:10,88) | 
        pnwq14 %in% c(1:5,7:10,88)                          ~ 0
    )
  )

# table(brisa_filtrada$discrim_raca2, exclude = NULL)
# prop.table(table(brisa_filtrada$discrim_raca, brisa_filtrada$discrim_raca2),2 )#ta batendo
# table(brisa_filtrada$discrim_mulher, exclude = NULL)
# prop.table(table(brisa_filtrada$pnreve10, brisa_filtrada$discrim_mulher),1 )#nao ta batendo
# table(brisa_filtrada$discrim_religiao, exclude = NULL)
# prop.table(table(brisa_filtrada$pnreve11, brisa_filtrada$discrim_religiao),1 )#nao ta batendo
# table(brisa_filtrada$discrim_econ_educ, exclude = NULL)
# prop.table(table(brisa_filtrada$pnreve15, brisa_filtrada$discrim_econ_educ),1 )#nao ta batendo mt

#### Discriminacao por local #############################

# variaveis que perguntam por qual motivo vc sofreu discrimina??o em tal
# local: pnwq2 (local de trabalho), pnwq5 (moradia)   , pnwq8 (pol?cia),
#        pnwq11 (locais p?blicos) , pnwq14 (col?gio/faculdade)

# motivos: 
# 01. Sua cor ou ra?a=01
# 02. Ser mulher=02
# 03. Sua religi?o ou culto=03
# 04. Doen?a ou defici?ncia f?sica=04
# 05. Sua op??o ou prefer?ncia sexual=05
# 06. Sua condi??o econ?mica, instru??o ou fun??o=06
# 07. Sua atividade pol?tica=07
# 08. Sua idade=08
# 09. Sua apar?ncia f?sica=09
# 10. Outra  -> Qual? ______________________________=10
# 77. N?o quis responder=77
# 88. N?o se aplica=88
# 99. N?o sabe=99


brisa_filtrada = brisa_filtrada %>%
  mutate(
    discrim_local_trabalho = case_when(
      pnwq1 == 1                      ~ 1,
      pnwq1 == 2                      ~ 0
    ),
    discrim_moradia = case_when(
      pnwq4 == 1                      ~ 1,
      pnwq4 == 2                      ~ 0
    ),
    discrim_policia = case_when(
      pnwq7 == 1                      ~ 1,
      pnwq7 == 2                      ~ 0
    ),
    discrim_local_publico = case_when(
      pnwq10 == 1                      ~ 1,
      pnwq10 == 2                      ~ 0
    ),
    discrim_coleg_facul = case_when(
      pnwq13 == 1                      ~ 1,
      pnwq13 == 2                      ~ 0
    )
  )
 
# table(brisa_filtrada$discrim_local_trabalho, exclude = NULL)
# table(brisa_filtrada$discrim_moradia, exclude = NULL)
# table(brisa_filtrada$discrim_policia, exclude = NULL)
# table(brisa_filtrada$discrim_local_publico, exclude = NULL)
# table(brisa_filtrada$discrim_coleg_facul, exclude = NULL)




##### Depressao CES-D ######################################

### Referencias para definicao de corte do score:

### depressao severa socre >= 22
### https://link.springer.com/article/10.1186/1742-4755-11-79
### depressao score > 15
### https://www.scielo.br/scielo.php?script=sci_arttext&pid=S0101-81082008000400008

### Para cada um dos 20 itens pncesd1:pncesd20 apresenta as seguintes respostas
# Raramente (menos que 1 dia)                 = 0                                                                    
# Durante pouco tempo (1 ou 2 dias)           = 1            
# Durante um tempo moderado (3 a 4 dias)      = 2      
# Durante a maior parte do tempo (5 a 7 dias) = 3

## Os itens pncesd1, pncesd2, pncesd3, pncesd5, pncesd6, pncesd7, pncesd9,
## pncesd10, pncesd11, pncesd13, pncesd14, pncesd15, pncesd17, pncesd18, pncesd19
## sao positivos, ou seja, sua pontuacao "procede"

## Ja os itens pncesd4, pncesd8, pncesd12, pncesd16 possuem pontuacao inversa

## O escore final varia de 0 a 60 pontos e corresponde ? soma da pontua??o de todas as respostas


brisa_filtrada = brisa_filtrada %>% 
  mutate(
    #MUDANDO QUESTOES QUE FOGEM DO SUPORTE E QUE PRECISAM SER INVERTIDAS
    
    #PRIMEIRO AS QUE PRECISAM SER INVERTIDAS
    pncesd4_2 = case_when(
      pncesd4 == 0 ~ 3,
      pncesd4 == 1 ~ 2,
      pncesd4 == 2 ~ 1,
      pncesd4 == 3 ~ 0,
      #caso em que foge do suporte (0-3)
      pncesd4 %in% c(7,9) ~ NA_real_
    ),
    pncesd8_2 = case_when(
      pncesd8 == 0 ~ 3,
      pncesd8 == 1 ~ 2,
      pncesd8 == 2 ~ 1,
      pncesd8 == 3 ~ 0,
      #caso em que foge do suporte (0-3)
      pncesd8 %in% c(7,9) ~ NA_real_
    ),
    pncesd12_2 = case_when(
      pncesd12 == 0 ~ 3,
      pncesd12 == 1 ~ 2,
      pncesd12 == 2 ~ 1,
      pncesd12 == 3 ~ 0,
      #caso em que foge do suporte (0-3)
      pncesd12 %in% c(7,9) ~ NA_real_
    ),
    pncesd16_2 = case_when(
      pncesd16 == 0 ~ 3,
      pncesd16 == 1 ~ 2,
      pncesd16 == 2 ~ 1,
      pncesd16 == 3 ~ 0,
      #caso em que foge do suporte (0-3)
      pncesd16 %in% c(7,9) ~ NA_real_
    ),
    
    #AGORA AS QUE FOGEM DO SUPORTE
    pncesd1_2  = ifelse( pncesd1  %in% c(7,9), NA_real_, pncesd1),
    pncesd2_2  = ifelse( pncesd2  %in% c(7,9), NA_real_, pncesd2),
    pncesd3_2  = ifelse( pncesd3  %in% c(7,9), NA_real_, pncesd3),
    pncesd5_2  = ifelse( pncesd5  %in% c(7,9), NA_real_, pncesd5),
    pncesd6_2  = ifelse( pncesd6  %in% c(7,9), NA_real_, pncesd6),
    pncesd7_2  = ifelse( pncesd7  %in% c(7,9), NA_real_, pncesd7),
    pncesd9_2  = ifelse( pncesd9  %in% c(7,9), NA_real_, pncesd9),
    pncesd10_2 = ifelse( pncesd10 %in% c(7,9), NA_real_, pncesd10),
    pncesd11_2 = ifelse( pncesd11 %in% c(7,9), NA_real_, pncesd11),
    pncesd13_2 = ifelse( pncesd13 %in% c(7,9), NA_real_, pncesd13),
    pncesd14_2 = ifelse( pncesd14 %in% c(7,9), NA_real_, pncesd14),
    pncesd15_2 = ifelse( pncesd15 %in% c(7,9), NA_real_, pncesd15),
    pncesd17_2 = ifelse( pncesd17 %in% c(7,9), NA_real_, pncesd17),
    pncesd18_2 = ifelse( pncesd18 %in% c(7,9), NA_real_, pncesd18),
    pncesd19_2 = ifelse( pncesd19 %in% c(7,9), NA_real_, pncesd19),
    pncesd20_2 = ifelse( pncesd20 %in% c(7,9), NA_real_, pncesd20)
  ) %>%
  
  mutate(
    score_cesd = rowSums(.[c('pncesd1_2', 'pncesd2_2', 'pncesd3_2',
                             'pncesd5_2', 'pncesd6_2', 'pncesd7_2',
                             'pncesd9_2', 'pncesd10_2', 'pncesd11_2',
                             'pncesd13_2', 'pncesd14_2', 'pncesd15_2',
                             'pncesd17_2', 'pncesd18_2', 'pncesd19_2',
                             'pncesd20_2',
                             'pncesd4_2', 'pncesd8_2', 'pncesd12_2',
                             'pncesd16_2')],
                         na.rm = T)
    )  %>%
  
  mutate(
    depressao_cesd = case_when( 
      #pontuacao >= 22 pontos no score indica a presenca de sintomas depressivos (conforme marizelia)
      score_cesd >= 22  ~ 1,
      score_cesd < 22   ~ 0
    )
  ) %>%
  #para transformar de volta a base em df
  data.frame() 

# View(brisa_filtrada %>% select(depressao_cesd, score_cesd,
#                                pncesd1, pncesd2, pncesd3,
#                                pncesd5, pncesd6, pncesd7,
#                                pncesd9, pncesd10, pncesd11,
#                                pncesd13, pncesd14, pncesd15,
#                                pncesd17, pncesd18, pncesd19,
#                                pncesd20, pncesd4_2, pncesd8_2,
#                                pncesd12_2, pncesd16_2))

# summary(brisa_filtrada$score_cesd)
# table(brisa_filtrada$depressao_cesd, exclude = NULL)


##### Depressao Pos-parto EPDS #######################################

# referencias: 
#  https://openrit.grupotiradentes.com/xmlui/bitstream/handle/set/3269/MONIQUE.pdf?sequence=1

# escala_depressao_depois foi feito s? somando direto as colunas tepds, 
# pois considera que o code-book s? errou na descri??o mas seguiu o question?rio

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


brisa_filtrada = brisa_filtrada %>% mutate(
  #mudando respostas para calcular score pois acho que estao errados(incidencia mt alta n_depres = 937)
  #acho que respostas estao seguindo codebook
  #ainda, no codebook a descricao das variaveis estao erradas (tudo igual mas sao diferentes entre si)
  tepds3_2 = case_when(
    tepds3 == 0 ~3,
    tepds3 == 1 ~2,
    tepds3 == 2 ~1,
    tepds3 == 3 ~0
  ),
  tepds5_2 = case_when(
    tepds5 == 0 ~3,
    tepds5 == 1 ~2,
    tepds5 == 2 ~1,
    tepds5 == 3 ~0
  ),
  tepds6_2 = case_when(
    tepds6 == 0 ~3,
    tepds6 == 1 ~2,
    tepds6 == 2 ~1,
    tepds6 == 3 ~0
  ),
  tepds7_2 = case_when(
    tepds7 == 0 ~3,
    tepds7 == 1 ~2,
    tepds7 == 2 ~1,
    tepds7 == 3 ~0
  ),
  tepds8_2 = case_when(
    tepds8 == 0 ~3,
    tepds8 == 1 ~2,
    tepds8 == 2 ~1,
    tepds8 == 3 ~0
  ),
  tepds9_2 = case_when(
    tepds9 == 0 ~3,
    tepds9 == 1 ~2,
    tepds9 == 2 ~1,
    tepds9 == 3 ~0
  ),
  tepds10_2 = case_when(
    tepds10 == 0 ~3,
    tepds10 == 1 ~2,
    tepds10 == 2 ~1,
    tepds10 == 3 ~0
  )
) %>% 
  mutate(
    score_depressao_epds = rowSums(.[c('tepds1', 'tepds2', 'tepds4', 
                                       # tepds3, tepds5, tepds6,
                                       # tepds7 ,tepds8 ,tepds9,
                                       # tepds10, 
                                       'tepds3_2', 'tepds5_2', 'tepds6_2',
                                       'tepds7_2' ,'tepds8_2' ,'tepds9_2',
                                       'tepds10_2')],
                                   na.rm = T)
    ) %>%
  #para transformar de volta a base em df
  data.frame()

#8 maes nao responderam esse questionario
#corrigindo o resultado do score delas (esta 0 mas tem que estar missing)

#pegando os numeros das colunas das variaveis de depressao epds para usar no loop
num_inicio = which( colnames(brisa_filtrada) == "tepds1" )
num_final  = which( colnames(brisa_filtrada) == "tepds10" )


for (linha in 1:nrow(brisa_filtrada)) {
  
  if( all( is.na(brisa_filtrada[linha,num_inicio:num_final]) )  ){
    
    brisa_filtrada$score_depressao_epds[linha] = NA
    
    # print(linha)
    # print(brisa_filtrada$score_depressao_epds[linha])
    
  }else{
    
    brisa_filtrada$score_depressao_epds[linha] = brisa_filtrada$score_depressao_epds[linha]
    
  }
  
}

rm(num_final, num_inicio, linha)

brisa_filtrada = brisa_filtrada  %>%
  
  mutate(depressao_epds = case_when( 
    
    #considerou-se a presenca de sintomas depressivos pos-parto quando o escore foi ??? 12."(marizelia)
    score_depressao_epds >= 12 ~ 1,
    score_depressao_epds < 12  ~ 0
  )
  )

# View(brisa_filtrada %>% select(score_depressao_epds,
#                                depressao_epds,
#                                tepds1, tepds2, tepds4,
#                                # tepds3, tepds5, tepds6,
#                                # tepds7 ,tepds8 ,tepds9,
#                                # tepds10,
#                                tepds3_2, tepds5_2, tepds6_2,
#                                tepds7_2 ,tepds8_2 ,tepds9_2,
#                                tepds10_2))

# summary(brisa_filtrada$score_depressao_epds)
# table(brisa_filtrada$depressao_epds, exclude = NULL)
# table(brisa_filtrada$depressao_epds, brisa_filtrada$depressao_cesd)

##### Bolsa Familia ##################################################

brisa_filtrada = brisa_filtrada %>%
  mutate(bolsa_familia_transfer = case_when(
    tbolsafam == 1   ~ 1,
    tbolsafam == 2   ~ 0,
    is.na(tbolsafam) ~ NA_real_
  )
  )

##### Empregada mensalista ###########################################

brisa_filtrada = brisa_filtrada %>%
  mutate( empregada_mensal = 
            case_when(
              tempregada > 0 & tempregada != 7 ~ 1,
              tempregada == 0                  ~ 0,
              is.na(tempregada)                ~ NA_real_
            )
          )

##### Classe social -seguim- A/B/C/D/E #############################

## Calculando os pontos por partes

##Agua encanada

# brisa_filtrada = brisa_filtrada %>%
#   mutate( pontos_agua = 
#             case_when(
#               pnaguabeber == 1                   ~ 4,
#               pnaguabeber > 1 & pnaguabeber != 9 ~ 0,
#               pnaguabeber == 9                   ~ NA_real_
#               )
#           )
# 
# ##Grau de intrucao chefe de familia
# # Analfabeto Primario Incompleto  Ate a 3 s?rie Fundamental =0  
# # Primario completo Ate a 4 s?rie Fundamental  Ginasial incompleto =1   
# # Ginasial completo  Fundamental completo  Colegial incompleto=2     
# # Colegial completo  M?dio completo  Superior incompleto =4         
# # Superior completo=8 
# brisa_filtrada = brisa_filtrada %>%
#   mutate( pontos_instrucao_chefe = 
#             case_when(
#               tgrauinschefe == 0    ~ 0,
#               tgrauinschefe == 1    ~ 1,
#               tgrauinschefe == 2    ~ 2,
#               tgrauinschefe == 4    ~ 4,
#               tgrauinschefe == 8    ~ 7,
#               is.na(tgrauinschefe)  ~ NA_real_
#               
#             )
#   )
# ##Bens/Itens 
# # exemplo: valor ponto (=quantid)
# # Banheiro           : 0(=0) 3(=1) 7(=2) 10(=3) 14(=4+)
# # autom?vel          : 0(=0) 3(=1) 5(=2) 8(=3) 11(=4+)
# # Empregada          : 0(=0) 3(=1) 7(=2) 10(=3) 13(=4+)
# # M?quina de lavar   : 0(=0) 2(=1) 4(=2) 6(=3) 6(=4+)
# # Videocassete ou DVD: 0(=0) 1(=1) 3(=2) 4(=3) 6(=4+)
# # Geladeira          : 0(=0) 2(=1) 3(=2) 5(=3) 5(=4+)
# # Freezer            : 0(=0) 2(=1) 4(=2) 6(=3) 6(=4+)
# 
# brisa_filtrada = brisa_filtrada %>%
#   mutate( ponto_banheiro = 
#             case_when(
#               is.na(tbanheiro)                ~ NA_real_,
#               tbanheiro == 0                  ~ 0,
#               tbanheiro == 1                  ~ 3,
#               tbanheiro == 2 | tbanheiro == 5 ~ 7,
#               tbanheiro == 3 | tbanheiro == 6 ~ 10,
#               tbanheiro == 4                  ~ 14
#             ),
#           ponto_automovel = 
#             case_when(
#               is.na(tautom)             ~ NA_real_,
#               tautom == 0               ~ 0,
#               tautom == 1               ~ 3,
#               tautom == 2 | tautom == 7 ~ 5,
#               tautom == 3               ~ 8,
#               tautom == 4               ~ 11
#             ),
#           ponto_empregada = 
#             case_when(
#               is.na(tempregada) ~ NA_real_,
#               tempregada == 0   ~ 0,
#               tempregada == 1   ~ 3,
#               tempregada == 2   ~ 7,
#               tempregada == 3   ~ 10
#             ),
#           ponto_maquina_lavar = 
#             case_when(
#               is.na(tmaquina) ~ NA_real_,
#               tmaquina == 0   ~ 0,
#               tmaquina == 1   ~ 2,
#               tmaquina == 2   ~ 4,
#               tmaquina == 3   ~ 6
#             ),
#           ponto_dvd = 
#             case_when(
#               is.na(tdvd) ~ NA_real_,
#               tdvd == 0   ~ 0,
#               tdvd == 1   ~ 1,
#               tdvd == 2   ~ 3,
#               tdvd == 3   ~ 4,
#               tdvd == 4   ~ 6
#             ),
#           ponto_geladeira = 
#             case_when(
#               is.na(tgelad) ~ NA_real_,
#               tgelad == 0   ~ 0,
#               tgelad == 1   ~ 2,
#               tgelad == 2   ~ 3,
#               tgelad == 3   ~ 5,
#               tgelad == 4   ~ 5
#             ),
#           ponto_freezer = 
#             case_when(
#               is.na(tfreezer) ~ NA_real_,
#               tfreezer == 0   ~ 0,
#               tfreezer == 1   ~ 2,
#               tfreezer == 2   ~ 4,
#               tfreezer == 3   ~ 6
#             )
#   ) %>%
#   mutate(
#     pontos_itens = rowSums(.[c('ponto_automovel', 'ponto_banheiro', 'ponto_empregada',
#                                'ponto_maquina_lavar', 'ponto_dvd', 'ponto_geladeira',
#                                'ponto_freezer')],
#                            na.rm = T)
#   ) %>% 
#   # somando para calcular o "score" de classe social
#   mutate(
#     pontos_somados_classe = sum( pontos_instrucao_chefe,
#                                  pontos_agua,
#                                  pontos_itens,
#                                  na.rm = T),
#     # clases sociais
#     # A   = 1
#     # B1  = 2
#     # B2  = 3
#     # C1  = 4
#     # C2  = 5
#     # D-E = 6
#     classe_social = case_when(
#       pontos_somados_classe >= 45 & pontos_somados_classe <= 100 ~ 1,
#       pontos_somados_classe >= 38 & pontos_somados_classe <= 44  ~ 2,
#       pontos_somados_classe >= 29 & pontos_somados_classe <= 37  ~ 3,
#       pontos_somados_classe >= 23 & pontos_somados_classe <= 28  ~ 4,
#       pontos_somados_classe >= 17 & pontos_somados_classe <= 22  ~ 5,
#       pontos_somados_classe >= 0  & pontos_somados_classe <= 16  ~ 6
#     ),
#     classe_social2 = case_when(
#       pontos_somados_classe >= (45-2) & pontos_somados_classe <= (100-2) ~ 1,
#       pontos_somados_classe >= (38-2) & pontos_somados_classe <= (44-2)  ~ 2,
#       pontos_somados_classe >= (29-2) & pontos_somados_classe <= (37-2)  ~ 3,
#       pontos_somados_classe >= (23-2) & pontos_somados_classe <= (28-2)  ~ 4,
#       pontos_somados_classe >= (17-2) & pontos_somados_classe <= (22-2)  ~ 5,
#       pontos_somados_classe >= (0)  & pontos_somados_classe <= (16-2)  ~ 6
#     )
#     )%>%
#   #para transformar de volta a base em df
#   data.frame() %>%
#   #excluindo as variaveis auxiliares de pontos por item
#   select( - starts_with("ponto_"))%>%
#   #excluindo as variaveis auxiliares de pontos por categoria
#   select( - starts_with("ponto_"))
# # %>%
# #   #excluindo as variaveis auxiliares de pontos somados
# #   select( - starts_with("pontos_somados"))
# 
# brisa_filtrada = brisa_filtrada %>% 
#   mutate( classe_social_menor = 
#             case_when(
#               # clases sociais
#               # A     = 1
#               # B1-B2 = 2
#               # C1-C2 = 3
#               # D-E   = 4
#               classe_social == 1                      ~ 1,
#               classe_social == 2 | classe_social == 3 ~ 2,
#               classe_social == 4 | classe_social == 5 ~ 3,
#               classe_social == 6                      ~ 4,
#             ),
#           classe_social2_menor = 
#             case_when(
#               # clases sociais
#               # A     = 1
#               # B1-B2 = 2
#               # C1-C2 = 3
#               # D-E   = 4
#               classe_social2 == 1                       ~ 1,
#               classe_social2 == 2 | classe_social2 == 3 ~ 2,
#               classe_social2 == 4 | classe_social2 == 5 ~ 3,
#               classe_social2 == 6                       ~ 4,
#             ),
#           )
##### Classe social - densidade ######################################

## Densidade da casa

#Pre natal
brisa_filtrada$pnpessoasp = ifelse(brisa_filtrada$pnpessoasp == 99, 
                                   NA_real_,
                                   brisa_filtrada$pnpessoasp)

brisa_filtrada$densid_morador_dormitor_prentl = c(brisa_filtrada$pnpessoasp / brisa_filtrada$pndormitor)
#tem uma mae com densidade inf, colocando essa mae com densidade = pnpessoasp
#pq ela ta com num de dormitorios=0
linha_mae = which(brisa_filtrada$densid_morador_dormitor_prentl==Inf)

brisa_filtrada$densid_morador_dormitor_prentl[linha_mae] = brisa_filtrada$pnpessoasp[linha_mae]
rm(linha_mae)


# summary(brisa_filtrada$densid_morador_dormitor_prentl)

# #outra forma de capturar informacao da renda/classe social
# # apenas com algumas variaveis do construto
# 
# # table(brisa_filtrada$pnbanheirop, exclude = NULL)
# # table(brisa_filtrada$pntelevisaop, exclude = NULL)
# table(brisa_filtrada$pnempregadap, exclude = NULL)
# 
# #corrigindo valor pra tv
# brisa_filtrada$classe_tv = ifelse(brisa_filtrada$pntelevisaop == 77,
#                                   NA_real_,
#                                   brisa_filtrada$pntelevisaop)
# 
# #corrigindo valor pra banheiro
# brisa_filtrada = brisa_filtrada %>% 
#   mutate( classe_banheiro = 
#             case_when(
#               pnbanheirop == 0   ~ 0,
#               pnbanheirop == 4   ~ 1,
#               pnbanheirop == 5   ~ 2,
#               pnbanheirop == 6   ~ 3,
#               pnbanheirop == 7   ~ 4
#               
#             )
#           )
# 
# #corrigindo valor pra empregada domestica
# brisa_filtrada = brisa_filtrada %>% 
#   mutate( classe_empregada_doms = 
#             case_when(
#               pnempregadap == 0        ~ 0,
#               pnempregadap %in% c(3,4) ~ 1,
#               pnempregadap == 77       ~ NA_real_
#               
#             )
#   )

# table(brisa_filtrada$classe_tv, exclude = NULL)
# table(brisa_filtrada$classe_banheiro, exclude = NULL)

##### Renda domiciliar per capita na gesta??o ########################

brisa_filtrada$renda_domic_prentl = ifelse(brisa_filtrada$pnrendafp==99999,
                                           yes = NA_real_,
                                           no  = brisa_filtrada$pnrendafp)

brisa_filtrada$quantos_moram_casa_prentl = ifelse(brisa_filtrada$pnpessoasp == 99,
                                                  yes = NA_real_,
                                                  no  = brisa_filtrada$pnpessoasp)

brisa_filtrada$renda_domic_percapita_prentl = c(
  brisa_filtrada$renda_domic_prentl/brisa_filtrada$quantos_moram_casa_prentl )

##### Ocupacao - mae #################################################

### Exerce atividade remunerada (empregada VS desempregada)

# # NASCIMENTO
# table(brisa_filtrada$ativrem, exclude = NULL)
# #parou de trabalhar pq ficou gravida 
# table(brisa_filtrada$paroutrab, exclude = NULL) 

### PRE-NATAL
# table(brisa_filtrada$pnativremp, exclude = NULL)
# table(brisa_filtrada$pnrelacaop, brisa_filtrada$pnativremp)
# tem uma mae que disse que nao trabalha em pnativaremp mas disse sua ocupacao
# ou o que faz atualmente no trabalho

mae_ocup_errado = which(brisa_filtrada$pnativremp == 2 & 
                       brisa_filtrada$pnrelacaop == 2)

brisa_filtrada = brisa_filtrada %>% 
  mutate( ocupada_prentl = 
            case_when(
              pnativremp == 1 ~ 1,
              pnativremp == 2 ~ 0,
              pnativremp == 9 ~ NA_real_
            )
  )

brisa_filtrada[mae_ocup_errado, 'ocupada_prentl'] = 1
rm(mae_ocup_errado)

### SEGUIMENTO
# table(brisa_filtrada$tativrem, exclude = NULL)
# table(brisa_filtrada$treltrab, brisa_filtrada$tativrem)
# tem duas maes que disse que nao trabalha em pnativaremp mas disse sua ocupacao
# ou o que faz atualmente no trabalho

mae_ocup_errado = which(brisa_filtrada$tativrem == 2 & 
                          brisa_filtrada$treltrab %in% c(2,3))


brisa_filtrada = brisa_filtrada %>% 
  mutate( ocupada_seguim = 
            case_when(
              tativrem == 1   ~ 1,
              tativrem == 2   ~ 0,
              is.na(tativrem) ~ NA_real_
            )
          )

brisa_filtrada[mae_ocup_errado, 'ocupada_seguim'] = 1
rm(mae_ocup_errado)

# table(brisa_filtrada$ocupada_seguim, exclude = NULL)


### Interacao

# PRE-NATAL
# table( brisa_filtrada$pnativremp,brisa_filtrada$pnchefep)
# prop.table(table( brisa_filtrada$pnativremp,brisa_filtrada$pnchefep), 1)

##### Ocupacao formal/informal #######################################

### PRE-NATAL

# Trabalha por conta pr?pria=1
# Assalariado ou empregado=2
# Dono de empresa-empregador=3
# Faz bico=4
# N?o se aplica=8
# Nao sabe=9
# table(brisa_filtrada$pnrelacaop, exclude = NULL)
# table(brisa_filtrada$pnrelacaop, brisa_filtrada$ocupada_prentl)

brisa_filtrada = brisa_filtrada %>%
  mutate(ocupada_formal_prentl = 
           case_when(
             pnrelacaop %in% c(2,3) ~ 1,  #formal
             pnrelacaop %in% c(1,4) ~ 2,  #informal
             pnrelacaop == 8        ~ 0,  #nao ocupada
             pnrelacaop == 9        ~ NA_real_
           )
         )

# table(brisa_filtrada$ocupada_prentl, exclude = NULL)
# table(brisa_filtrada$ocupada_formal_prentl, exclude = NULL)

### SEGUIMENTO

# Trabalha por conta pr?pria=1
# Assalariado ou empregado=2
# Dono de empresa-empregador=3
# Faz bico=4
# N?o se aplica=8
# Nao sabe=9
# table(brisa_filtrada$treltrab, exclude = NULL)
# table(brisa_filtrada$treltrab, brisa_filtrada$ocupada_seguim)

brisa_filtrada = brisa_filtrada %>%
  mutate(ocupada_formal_seguim = 
           case_when(
             treltrab %in% c(2,3)    ~ 1,  #formal
             treltrab %in% c(1,4)    ~ 2,  #informal
             treltrab == 8  &
               ocupada_seguim == 0   ~ 0,  #nao ocupada
             treltrab == 8  &
               ocupada_seguim == 1   ~ NA_real_,
             treltrab == 9           ~ 0, #essa pessoa esta com ocupada_seguim = 0
             is.na(treltrab) &
               ocupada_seguim == 0   ~ 0,
             is.na(treltrab) &
               is.na(ocupada_seguim) ~ NA_real_
           )
  )

# table(brisa_filtrada$ocupada_seguim, exclude = NULL)
# table(brisa_filtrada$ocupada_formal_seguim, exclude = NULL)

##### Ocupacao - companheiro - MUITO RUIM 960 NAs #################################

# table(brisa_filtrada$pntrabcompp, exclude = NULL) #pre-natal
# table(brisa_filtrada$tcomptrab, exclude = NULL) #seguim 


# table(brisa_filtrada$ocupada_seguim, exclude = NULL)

##### Trabalho domestico #####################################

# table(brisa_filtrada$trabcasa, exclude = NULL)

brisa_filtrada$faz_todo_trab_domestico = ifelse(brisa_filtrada$trabcasa == 1,
                                                1,
                                                ifelse(brisa_filtrada$trabcasa == 2,
                                                       0,
                                                       NA_real_))

#ps: no dicioario nao tinha o que eh a categoria 3, e soh fui descobrir quando
#    peguei nos dados de sao luis
# brisa_filtrada = brisa_filtrada %>%
#   mutate(faz_todo_trab_domestico = case_when(
#     trabcasa == 1 ~ 2, #faz todo o trabalho
#     trabcasa == 2 ~ 1, #divide o trabalho
#     trabcasa == 3 ~ 0  #outra pessoa faz o trabalho
#   )
#   )
# table(brisa_filtrada$faz_todo_trab_domestico, exclude = NULL)

##### Apoio social/suporte em cuidados ##############################

# Se precisar, com que frequencia conta com alguem para
# table(brisa_filtrada$pnmos19 , exclude = NULL) #prepara suas refeicoes, se nao puder prep
# table(brisa_filtrada$pnmos22 , exclude = NULL) #tarefa diaria, se vc ficar doente
# xtabs(~ pnmos22 +pnmos19, brisa_filtrada, addNA = TRUE)

brisa_filtrada = brisa_filtrada %>%
  mutate(suporte_social_cuidados = 
           case_when(
             pnmos19 %in% c(0,1) & pnmos22 %in% c(0,1)     ~ 0,
             pnmos19 %in% c(2,3,4) | pnmos22 %in% c(2,3,4) ~ 1,
             pnmos19 %in% c(7,9)   | pnmos22 %in% c(7,9)   ~ NA_real_
           )
         )

# table(brisa_filtrada$suporte_social_cuidados, exclude = NULL)
# xtabs(~ pnmos22 +pnmos19 + suporte_social_cuidados, brisa_filtrada, addNA = TRUE)

##### Desenvolvimento Infantil - Bayley III ##########################

# Criancas com classificacao Risco

# Classificacao criada:
# 0 = nao tem risco para nenhuma das classificacoes bayley
# 1 = tem uma classificacao bayley em risco
# 2 = tem entre 2 e 4 classificacoes bayley em risco
# 3 = tem todas as cinco classificacoes bayley em risco

variaveis_bayley = c("cog_classifica__o"   , "com_recep_classif" ,
                       "com_expre_classif" , "mot_fino_classif"  ,
                       "mot_grosso_classif")
nome_dominio_bayley = c("cognitivo"        , "comunic_receptiva" ,
                        "comunic_expressiva", "motor_fino"        ,
                        "motor_grosso")

for (dominio_bayley in 1:length(variaveis_bayley)) {
  
  qual_dominio = variaveis_bayley[dominio_bayley]
  
  nome_variavel_dominio = paste0("dominio_",nome_dominio_bayley[dominio_bayley])
  
  brisa_filtrada[,nome_variavel_dominio] = 0
  
  for (linha in 1:nrow(brisa_filtrada)) {
    
    if(brisa_filtrada[linha, qual_dominio] == "Risco"){
      
      brisa_filtrada[linha, nome_variavel_dominio] = 1
    }
    
    if(brisa_filtrada[linha, qual_dominio] == ""){
      
      brisa_filtrada[linha, nome_variavel_dominio] = NA_real_
    }
      
  }

}
rm(linha, dominio_bayley, qual_dominio, nome_variavel_dominio, 
   nome_dominio_bayley, variaveis_bayley)

# table(brisa_filtrada$cog_classifica__o, exclude = NULL)
# table(brisa_filtrada$dominio_cognitivo, exclude = NULL)
# 
# table(brisa_filtrada$com_recep_classif, exclude = NULL)
# table(brisa_filtrada$dominio_comunic_receptiva, exclude = NULL)
# 
# table(brisa_filtrada$com_expre_classif, exclude = NULL)
# table(brisa_filtrada$dominio_comunic_expressiva, exclude = NULL)
# 
# table(brisa_filtrada$mot_fino_classif, exclude = NULL)
# table(brisa_filtrada$dominio_motor_fino, exclude = NULL)
# 
# table(brisa_filtrada$mot_grosso_classif, exclude = NULL)
# table(brisa_filtrada$dominio_motor_grosso, exclude = NULL)

### Variavel para pelo menos algum dominio em risco
brisa_filtrada = brisa_filtrada %>% 
  mutate(bayley_risco = case_when(
    dominio_cognitivo == 1 | dominio_comunic_expressiva == 1 |
      dominio_comunic_receptiva == 1 | dominio_motor_fino == 1 |
      dominio_motor_grosso == 1                                  ~ 1,
    dominio_cognitivo == 0 & dominio_comunic_expressiva == 0 &
      dominio_comunic_receptiva == 0 & dominio_motor_fino == 0 &
      dominio_motor_grosso == 0                                  ~ 0
  ))

# table(brisa_filtrada$bayley_risco, exclude = NULL)
# 
# View(brisa_filtrada %>% select(bayley_risco,dominio_cognitivo,
#                                dominio_comunic_receptiva, dominio_comunic_expressiva,
#                                dominio_motor_fino,dominio_motor_grosso))


# 
# brisa_filtrada$bayley_quantos_risco = apply(brisa_filtrada[,variaveis_bayley],
#                                             1,
#                                             function(x) length(which(x=="Risco")))
# 
# 
# brisa_filtrada$bayley_risco = ifelse(brisa_filtrada$bayley_quantos_risco == 5,
#                                      3,
#                                      ifelse(brisa_filtrada$bayley_quantos_risco >= 2 &
#                                               brisa_filtrada$bayley_quantos_risco < 5,
#                                             2,
#                                             ifelse(brisa_filtrada$bayley_quantos_risco == 1,
#                                                      1,
#                                                    0
#                                             )
#                                      )
#                                      )
# 
# brisa_filtrada$bayley_risco = ifelse(
#     apply(brisa_filtrada[,variaveis_bayley] == "", 1, all), #1= linha
#     NA_real_, brisa_filtrada$bayley_risco
#    )
# 
# # table(brisa_filtrada$bayley_risco, exclude = NULL)
# # View(brisa_filtrada %>% select(c(variaveis_bayley, bayley_risco)))
# 
# #Bayley risco dummy
# 
# brisa_filtrada = brisa_filtrada %>%
#   mutate( bayley_risco_dummy = 
#             case_when(
#               bayley_risco == 3            ~ 1,
#               bayley_risco %in% c(0,1,2)   ~ 0,
#               is.na(bayley_risco)          ~ NA_real_
#             )
#           )
# # table(brisa_filtrada$bayley_risco_dummy)
# 
# #Bayley bem desenvolvido
# 
# brisa_filtrada$bayley_quantos_competente = apply(brisa_filtrada[,variaveis_bayley],
#                                             1,
#                                             function(x) length(which(x=="Competente")))
# 
# 
# brisa_filtrada$bayley_competente = ifelse(brisa_filtrada$bayley_quantos_competente == 5,
#                                      3,
#                                      ifelse(brisa_filtrada$bayley_quantos_competente >= 2 &
#                                               brisa_filtrada$bayley_quantos_competente < 5,
#                                             2,
#                                             ifelse(brisa_filtrada$bayley_quantos_competente == 1,
#                                                    1,
#                                                    0
#                                             )
#                                      )
# )
# 
# brisa_filtrada$bayley_competente = ifelse(
#   apply(brisa_filtrada[,variaveis_bayley] == "", 1, all), #1= linha
#   NA_real_, brisa_filtrada$bayley_competente
# )
# # table(brisa_filtrada$bayley_competente, exclude = NULL)
# 
# rm(variaveis_bayley)

##### Saude da crianca ###############################################

#Como a mae considera a saude da crianca
# Excelente=01   
# Muito boa=02
# Boa      =03
# Regular  =04
# Ruim     =05                                                                                                           
# N?o sabe= 09

brisa_filtrada = brisa_filtrada %>%
  mutate(saude_crianca = 
           case_when(
             tsaude == 1 | tsaude == 2 | tsaude == 3 ~ 0,
             tsaude == 5 | tsaude == 4 ~ 1
           )
         )

##### Crianca na creche/escola #######################################

# table(brisa_filtrada$tescola, exclude = NULL)

brisa_filtrada$filho_creche = ifelse(brisa_filtrada$tescola == 1,
                                     yes = 1,
                                     ifelse(brisa_filtrada$tescola == 2,
                                            yes = 0,
                                            no = NA_real_))

# table(brisa_filtrada$filho_creche, exclude = NULL)

##### Vinculo mae-bebe ###############################################

brisa_filtrada = brisa_filtrada %>%
  #mudando algumas variaveis para depois poder calcular o score
  #(algumas variaveis tem a ordem 0-5 invertida)
  #as mudancas foram feitas conforme apresentado no anexo em Brockington et al (2006)
  mutate(
    tpbq2_2 = case_when(
      tpbq2 == 0 ~ 5,
      tpbq2 == 1 ~ 4,
      tpbq2 == 2 ~ 3,
      tpbq2 == 3 ~ 2,
      tpbq2 == 4 ~ 1,
      tpbq2 == 5 ~ 0,
    ),
    tpbq3_2 = case_when(
      tpbq3 == 0 ~ 5,
      tpbq3 == 1 ~ 4,
      tpbq3 == 2 ~ 3,
      tpbq3 == 3 ~ 2,
      tpbq3 == 4 ~ 1,
      tpbq3 == 5 ~ 0,
    ),
    tpbq5_2 = case_when(
      tpbq5 == 0 ~ 5,
      tpbq5 == 1 ~ 4,
      tpbq5 == 2 ~ 3,
      tpbq5 == 3 ~ 2,
      tpbq5 == 4 ~ 1,
      tpbq5 == 5 ~ 0,
    ),
    tpbq6_2 = case_when(
      tpbq6 == 0 ~ 5,
      tpbq6 == 1 ~ 4,
      tpbq6 == 2 ~ 3,
      tpbq6 == 3 ~ 2,
      tpbq6 == 4 ~ 1,
      tpbq6 == 5 ~ 0,
    ),
    tpbq7_2 = case_when(
      tpbq7 == 0 ~ 5,
      tpbq7 == 1 ~ 4,
      tpbq7 == 2 ~ 3,
      tpbq7 == 3 ~ 2,
      tpbq7 == 4 ~ 1,
      tpbq7 == 5 ~ 0,
    ),
    tpbq10_2 = case_when(
      tpbq10 == 0 ~ 5,
      tpbq10 == 1 ~ 4,
      tpbq10 == 2 ~ 3,
      tpbq10 == 3 ~ 2,
      tpbq10 == 4 ~ 1,
      tpbq10 == 5 ~ 0,
    ),
    tpbq12_2 = case_when(
      tpbq12 == 0 ~ 5,
      tpbq12 == 1 ~ 4,
      tpbq12 == 2 ~ 3,
      tpbq12 == 3 ~ 2,
      tpbq12 == 4 ~ 1,
      tpbq12 == 5 ~ 0,
    ),
    tpbq13_2 = case_when(
      tpbq13 == 0 ~ 5,
      tpbq13 == 1 ~ 4,
      tpbq13 == 2 ~ 3,
      tpbq13 == 3 ~ 2,
      tpbq13 == 4 ~ 1,
      tpbq13 == 5 ~ 0,
    ),
    tpbq14_2 = case_when(
      tpbq14 == 0 ~ 5,
      tpbq14 == 1 ~ 4,
      tpbq14 == 2 ~ 3,
      tpbq14 == 3 ~ 2,
      tpbq14 == 4 ~ 1,
      tpbq14 == 5 ~ 0,
    ),
    tpbq15_2 = case_when(
      tpbq15 == 0 ~ 5,
      tpbq15 == 1 ~ 4,
      tpbq15 == 2 ~ 3,
      tpbq15 == 3 ~ 2,
      tpbq15 == 4 ~ 1,
      tpbq15 == 5 ~ 0,
    ),
    tpbq17_2 = case_when(
      tpbq17 == 0 ~ 5,
      tpbq17 == 1 ~ 4,
      tpbq17 == 2 ~ 3,
      tpbq17 == 3 ~ 2,
      tpbq17 == 4 ~ 1,
      tpbq17 == 5 ~ 0,
    ),
    tpbq18_2 = case_when(
      tpbq18 == 0 ~ 5,
      tpbq18 == 1 ~ 4,
      tpbq18 == 2 ~ 3,
      tpbq18 == 3 ~ 2,
      tpbq18 == 4 ~ 1,
      tpbq18 == 5 ~ 0,
    ),
    tpbq19_2 = case_when(
      tpbq19 == 0 ~ 5,
      tpbq19 == 1 ~ 4,
      tpbq19 == 2 ~ 3,
      tpbq19 == 3 ~ 2,
      tpbq19 == 4 ~ 1,
      tpbq19 == 5 ~ 0,
    ),
    tpbq20_2 = case_when(
      tpbq20 == 0 ~ 5,
      tpbq20 == 1 ~ 4,
      tpbq20 == 2 ~ 3,
      tpbq20 == 3 ~ 2,
      tpbq20 == 4 ~ 1,
      tpbq20 == 5 ~ 0,
    ),
    tpbq21_2 = case_when(
      tpbq21 == 0 ~ 5,
      tpbq21 == 1 ~ 4,
      tpbq21 == 2 ~ 3,
      tpbq21 == 3 ~ 2,
      tpbq21 == 4 ~ 1,
      tpbq21 == 5 ~ 0,
    ),
    tpbq23_2 = case_when(
      tpbq23 == 0 ~ 5,
      tpbq23 == 1 ~ 4,
      tpbq23 == 2 ~ 3,
      tpbq23 == 3 ~ 2,
      tpbq23 == 4 ~ 1,
      tpbq23 == 5 ~ 0,
    ),
    tpbq24_2 = case_when(
      tpbq24 == 0 ~ 5,
      tpbq24 == 1 ~ 4,
      tpbq24 == 2 ~ 3,
      tpbq24 == 3 ~ 2,
      tpbq24 == 4 ~ 1,
      tpbq24 == 5 ~ 0,
    ),
  )%>% 
  mutate(
    score_vinculo_mae_bebe = rowSums(.[c('tpbq1',    'tpbq2_2',  'tpbq3_2', 
                                         'tpbq4',    'tpbq5_2',  'tpbq6_2',
                                         'tpbq7_2',  'tpbq8',    'tpbq9',
                                         'tpbq10_2', 'tpbq11',   'tpbq12_2',
                                         'tpbq13_2', 'tpbq14_2', 'tpbq15_2',
                                         'tpbq16',   'tpbq17_2', 'tpbq18_2',
                                         'tpbq19_2', 'tpbq20_2', 'tpbq21_2',
                                         'tpbq22',   'tpbq23_2', 'tpbq24_2',
                                         'tpbq25')],
                                     na.rm = T)
  )  %>%
  #para transformar de volta a base em df
  data.frame()

#7 maes nao responderam esse questionario
#corrigindo o resultado do score delas (esta 0 mas tem que estar missing)

#pegando os numeros das colunas das variaveis de vinculo maternos para usar no loop
num_inicio = which( colnames(brisa_filtrada) == "tpbq1" )
num_final  = which( colnames(brisa_filtrada) == "tpbq25" )


for (linha in 1:nrow(brisa_filtrada)) {
  
  if( all( is.na(brisa_filtrada[linha,num_inicio:num_final]) )  ){
    
    brisa_filtrada$score_vinculo_mae_bebe[linha] = NA
    
    # print(linha)
    # print(brisa_filtrada$score_vinculo_mae_bebe[linha])
    
  }else{
    
    brisa_filtrada$score_vinculo_mae_bebe[linha] = brisa_filtrada$score_vinculo_mae_bebe[linha]
    
  }
  
}

rm(num_final, num_inicio, linha)

brisa_filtrada = brisa_filtrada %>%
  mutate(vinculo_mae_bebe_boa = case_when( 
    #"A pontuacao maxima atingida pelo questionario eh de 125 pontos. 
    # A relacao mae/filho foi classificada em boa (<= 25 pontos) ou prejudicada (> 25 pontos)" 
    # (marizelia)
    score_vinculo_mae_bebe <= 25 ~ 1,
    score_vinculo_mae_bebe >  25 ~ 0
    
  ),
  vinculo_mae_bebe_prejudicada = case_when(
    score_vinculo_mae_bebe > 25  ~ 1,
    score_vinculo_mae_bebe <= 25 ~ 0
  )
  )


# View(brisa_filtrada %>% select(vinculo_mae_bebe_boa, vinculo_mae_bebe_prejudicada, 
#                                score_vinculo_mae_bebe,
#                                tpbq1,    tpbq2_2,  tpbq3_2, 
#                                tpbq4,    tpbq5_2,  tpbq6_2,
#                                tpbq7_2,  tpbq8,    tpbq9,
#                                tpbq10_2, tpbq11,   tpbq12_2,
#                                tpbq13_2, tpbq14_2, tpbq15_2,
#                                tpbq16,   tpbq17_2, tpbq18_2,
#                                tpbq19_2, tpbq20_2, tpbq21_2,
#                                tpbq22,   tpbq23_2, tpbq24_2,
#                                tpbq25))

# summary(brisa_filtrada$score_vinculo_mae_bebe)
# table(brisa_filtrada$vinculo_mae_bebe_boa, exclude = NULL)
# table(brisa_filtrada$vinculo_mae_bebe_prejudicada, exclude = NULL)

##### Nascimento planejado ###################################

# table(brisa_filtrada$plangest, exclude = NULL)

brisa_filtrada$bebe_planejado = ifelse(brisa_filtrada$plangest == 1,
                                       1,
                                       ifelse(brisa_filtrada$plangest == 2,
                                              0,
                                              NA_real_))
# table(brisa_filtrada$bebe_planejado, exclude = NULL)

##### Numero de filhos que moram com a mae ###################
# table(brisa_filtrada$pnqtfilhosp, exclude = NULL)
# table(brisa_filtrada$qtfilhos, exclude = NULL)
# xtabs(~pnqtfilhosp + qtfilhos, brisa_filtrada)
# xtabs(~pnqtfilhosp + pnmorafilhop, brisa_filtrada)
# xtabs(~morafilho + qtfilhos, brisa_filtrada, exclude = NULL)
#correlacao de numero de filhos no nascim e prentl = 0.9451053

brisa_filtrada = brisa_filtrada %>% mutate(
  quantos_filhos_mora_prentl = case_when(
    pnqtfilhosp  < 88                     ~ as.numeric(pnqtfilhosp),
    pnqtfilhosp == 88 & pnmorafilhop == 2 ~ 0,
    pnqtfilhosp == 88 & pnmorafilhop == 1 ~ NA_real_,
    pnqtfilhosp == 99                     ~ NA_real_
  )
)

# table(brisa_filtrada$quantos_filhos_mora_prentl, exclude = NULL)

brisa_filtrada = brisa_filtrada %>% mutate(
  quantos_filhos_mora_nascim = case_when(
    qtfilhos  < 88                     ~ as.numeric(qtfilhos),
    qtfilhos == 88 & morafilho == 2 ~ 0,
    qtfilhos == 88 & morafilho == 1 ~ NA_real_
  )
)

# table(brisa_filtrada$quantos_filhos_mora_nascim, exclude = NULL)

##### Numero de filhos que a mulher tem ##############################

# table(brisa_filtrada$nfilhos, exclude = NULL) #perg. feita no nascimento

brisa_filtrada$quantos_filhos_tem_nascim = ifelse(brisa_filtrada$nfilhos == 88,
                                                  NA_real_,
                                                  brisa_filtrada$nfilhos)

# table(brisa_filtrada$quantos_filhos_tem_nascim, exclude = NULL)

##### Mora com filho crianca #########################################
# table(brisa_filtrada$moracri, exclude = NULL)
# xtabs(~morafilho + moracri, brisa_filtrada, exclude = NULL)

brisa_filtrada = brisa_filtrada %>% mutate(
  mora_filho_crianca_nasc = 
    case_when(
      morafilho == 1 & moracri == 1      ~ 1,
      moracri == 2 |
       (morafilho == 1 & moracri == 2) |
        (morafilho == 2)                 ~ 0
    )
)

# table(brisa_filtrada$mora_filho_crianca_nasc, exclude = NULL)

### crianca ate 3 anos
# table(brisa_filtrada$criate3, exclude = NULL)
# xtabs(~morafilho + criate3, brisa_filtrada, exclude = NULL)

brisa_filtrada = brisa_filtrada %>% mutate(
  mora_filho_3anos_nasc = 
    case_when(
      morafilho == 1 & criate3 == 1      ~ 1,
      criate3 %in% c(2, 8) |
        (morafilho == 1 & criate3 == 2) |
        (morafilho == 2)                 ~ 0
    )
)

# table(brisa_filtrada$mora_filho_3anos_nasc, exclude = NULL)

##### Educacao da mae ################################################

# Alfabetiza??o de jovens e adultos=1 e Ensino fundamental ou 1? grau=2 ~1
# Ensino m?dio ou 2? grau=3 ~2
# Superior gradua??o incompleto=4 e Superior gradua??o completo=5 ~3

brisa_filtrada = brisa_filtrada %>%  mutate(
  educacao_mae = case_when(
    pncursog == 1 | pncursog == 2 ~ 1,
    pncursog == 3                 ~ 2,
    pncursog == 4 | pncursog == 5 ~ 3,
    pncursog %in% c(8,9)          ~ NA_real_
    ),
  educacao_mae2 = case_when(
    pncursog == 1 | pncursog == 2 ~ 1,
    pncursog == 3 | pncursog == 4 ~ 2,
    pncursog == 5                 ~ 3,
    pncursog %in% c(8,9)          ~ NA_real_
  )
  )

# table(brisa_filtrada$educacao_mae, exclude = NULL)

##### Educacao companheiro - MUITO RUIM 871 NAs  ########################

# Alfabetiza??o de jovens e adultos=1 e Ensino fundamental ou 1? grau=2 ~1
# Ensino m?dio ou 2? grau=3 ~2
# Superior gradua??o incompleto=4 e Superior gradua??o completo=5 ~3
# table(brisa_filtrada$tcompcurso, exclude = NULL)

brisa_filtrada = brisa_filtrada %>%  mutate(
  educacao_companheiro = case_when(
    pncursocompp == 1 | pncursocompp == 2 ~ 1,
    pncursocompp == 3                     ~ 2,
    pncursocompp == 4 | pncursocompp == 5 ~ 3,
    pncursocompp %in% c(8,9)              ~ NA_real_
  )
)
brisa_filtrada$dummy_educacao_companheiro = ifelse(is.na(brisa_filtrada$educacao_companheiro),1,0)


# table(brisa_filtrada$educacao_companheiro, exclude = NULL)

##### Educacao da chefe de familia prentl ##############################

# table(brisa_filtrada$pninstrucaop, exclude = NULL)
# table(brisa_filtrada$pncursochefp, exclude = NULL)

brisa_filtrada = brisa_filtrada %>%  mutate(
  educacao_chefe = case_when(
    # Alfabetiza??o de jovens e adultos=1 e Ensino fundamental ou 1? grau=2 ~1
    # Ensino m?dio ou 2? grau=3 ~2
    # Superior gradua??o incompleto=4 e Superior gradua??o completo=5 ~3
    pncursochefp == 1 | pncursochefp == 2 ~ 1,
    pncursochefp == 3                     ~ 2,
    pncursochefp == 4 | pncursochefp == 5 ~ 3,
    pncursochefp %in% c(8,9)          ~ NA_real_
  ),
  educacao_chefe2 = case_when(
    # Antes
    
    # Analfabeto Primario Incompleto  Ate a 3 s?rie Fundamental =0  
    # Primario completo Ate a 4 s?rie Fundamental  Ginasial incompleto =1   
    # Ginasial completo  Fundamental completo  Colegial incompleto=2     
    # Colegial completo  M?dio completo  Superior incompleto =4         
    # Superior completo=8 
    
    # Depois
    
    # 0 ou 1 ~ 1 (analf. ou fund. completo ou medio incompleto)
    # 2 ou 4 ~ 2 (medio completo ou superior incompleto)
    # 8      ~ 3 (superior incompleto)
    pninstrucaop == 0 | pninstrucaop == 1 ~ 1,
    pninstrucaop == 2 | pninstrucaop == 4 ~ 2,
    pninstrucaop == 8                     ~ 3,
    pninstrucaop == 9                     ~ NA_real_
  )
)

# table(brisa_filtrada$educacao_chefe, exclude = NULL)
# table(brisa_filtrada$educacao_chefe2, exclude = NULL)

##### Idade da mae ###################################################

brisa_filtrada = brisa_filtrada %>%
  # separando as variaveis de dia, mes e ano da data de nascim da mae para
  # calcular a idade no seguimento
  mutate(
    dia_nascim_mae = as.numeric(str_sub(pndngest, end   = 2)),
    ano_nascim_mae = as.numeric(str_sub(pndngest, start = -4)),
    mes_nascim_mae = str_sub(pndngest, start = 3, end = 5),
    dia_nascim_mae2 = as.numeric(str_sub(pndnmae, end   = 2)),
    ano_nascim_mae2 = as.numeric(str_sub(pndnmae, start = -4)),
    mes_nascim_mae2 = str_sub(pndnmae, start = 3, end = 5)
  ) %>%
  mutate(
    mes_nascim_mae =
      case_when(
        mes_nascim_mae == "JAN" ~ 01,
        mes_nascim_mae == "FEB" ~ 02,
        mes_nascim_mae == "MAR" ~ 03,
        mes_nascim_mae == "APR" ~ 04,
        mes_nascim_mae == "MAY" ~ 05,
        mes_nascim_mae == "JUN" ~ 06,
        mes_nascim_mae == "JUL" ~ 07,
        mes_nascim_mae == "AUG" ~ 08,
        mes_nascim_mae == "SEP" ~ 09,
        mes_nascim_mae == "OCT" ~ 10,
        mes_nascim_mae == "NOV" ~ 11,
        mes_nascim_mae == "DEC" ~ 12
      ),
    mes_nascim_mae2 =
      case_when(
        mes_nascim_mae2 == "JAN" ~ 01,
        mes_nascim_mae2 == "FEB" ~ 02,
        mes_nascim_mae2 == "MAR" ~ 03,
        mes_nascim_mae2 == "APR" ~ 04,
        mes_nascim_mae2 == "MAY" ~ 05,
        mes_nascim_mae2 == "JUN" ~ 06,
        mes_nascim_mae2 == "JUL" ~ 07,
        mes_nascim_mae2 == "AUG" ~ 08,
        mes_nascim_mae2 == "SEP" ~ 09,
        mes_nascim_mae2 == "OCT" ~ 10,
        mes_nascim_mae2 == "NOV" ~ 11,
        mes_nascim_mae2 == "DEC" ~ 12
      ),
    
    data_nascim_mae = as.Date( paste0(dia_nascim_mae, "/",
                                      mes_nascim_mae, "/",
                                      ano_nascim_mae),
                               "%d/%m/%Y"
                               ),
    data_nascim_mae2 = as.Date( paste0(dia_nascim_mae2, "/",
                                      mes_nascim_mae2, "/",
                                      ano_nascim_mae2),
                               "%d/%m/%Y"
    ),
    idade_mae_seguim = time_length(
      difftime(as.Date(tdataent, "%d/%m/%Y"), 
               data_nascim_mae),
      "years"
      )
      
  )
# View(brisa_filtrada %>%
#        select(pndnmae, pndngest, pnidadeg,
#                                pndataentp, tdataent, idade_mae_seguim,
#               dif_idade_intermediaria) %>%
#        cbind(.,
#              c(as.character(brisa_filtrada$pndnmae) == as.character(brisa_filtrada$pndngest)),
#              c(brisa_filtrada$idade_mae_seguim -brisa_filtrada$pnidadeg)
#              ))
# View(brisa_filtrada %>%
#        select(data_nascim_mae,pndataentp, pndnmae, pnidadeg, tdataent, idade_mae_seguim)%>%
#        filter(idade_mae_seguim < 5))

#corrigindo duas maes que estao com idade errada por conta da data de nascimento

brisa_filtrada$dif_idade_intermediaria = 
  brisa_filtrada$idade_mae_seguim - brisa_filtrada$pnidadeg

for (linha in 1:nrow(brisa_filtrada)) {
  
  if (brisa_filtrada$dif_idade_intermediaria[linha] < 1) {
    
    brisa_filtrada$idade_mae_seguim[linha] = time_length(
      
      difftime(as.Date(brisa_filtrada$tdataent[linha], "%d/%m/%Y"), 
               brisa_filtrada$data_nascim_mae2[linha]),
      "years"
    )
    
  }
  
}
rm(linha)

brisa_filtrada$idade_mae_seguim_quadrado = brisa_filtrada$idade_mae_seguim^2

#excluindo colunas intermediarias criadas para o calculo de idade e correcao desta
brisa_filtrada = brisa_filtrada %>%
  subset(., select = - c(dif_idade_intermediaria,
                         data_nascim_mae2, data_nascim_mae,
                         mes_nascim_mae2,  mes_nascim_mae,
                         dia_nascim_mae,   dia_nascim_mae2,
                         mes_nascim_mae,   mes_nascim_mae2,
                         ano_nascim_mae,   ano_nascim_mae2))
##### Idade bebe #####################################################

brisa_filtrada = brisa_filtrada %>% 
  mutate(
    idade_bebe_seguim = time_length(
      difftime(as.Date(tdataent, "%d/%m/%Y"),   #data da entrevista (seguim)
               as.Date(rn1dtnasc, "%d/%m/%Y")), #data de nascimento (nasc)
      "months"
    )
  )

# View(brisa_filtrada %>% 
#        select(rn1dtnasc, tdataent, idade_bebe_seguim))
# summary(brisa_filtrada$idade_bebe_seguim)

##### Amamentacao exclusiva ##########################################

# Variaveis

# taleitexcm  : ate que idade o aleitamento foi exclusivo (em meses)
#
#   quando inseriu na rotina alimentar 
#
# tleitemes   : leite (liquido ou po)
# tleitformes : leite tipo formula
# tliqmes     : outros liquidos (cha, suco)
# tsolidomes  : semi-solidos ou solidos


colunas_interesse = c(which( colnames(brisa_filtrada) == "tleitemes" ),
                      which( colnames(brisa_filtrada) == "tleitformes" ),
                      which( colnames(brisa_filtrada) == "tliqmes" ),
                      which( colnames(brisa_filtrada) == "tsolidomes" ))

for (linha in 1:nrow(brisa_filtrada)) {
  
  if( is.na(brisa_filtrada$taleitexcm[linha] )){
    
    brisa_filtrada$amamentacao_exclusiva[linha] = NA_real_
    
  }else{
    
    if( brisa_filtrada$taleitexcm[linha] == 0 ) {
    
    brisa_filtrada$amamentacao_exclusiva[linha] = 0
    
    } else{
      brisa_filtrada$amamentacao_exclusiva[linha] = apply(brisa_filtrada[linha,colunas_interesse], 1, 
                                                   function(x) {
                                                     min(x[ x > 0 ], na.rm = TRUE) 
                                                   }
      )

                                                 }

  }
  
}
rm(linha)

# View(brisa_filtrada %>% select(
#   amamentacao_exclusiva,
#   # tidleitemeses,
#   taleitexcm,  taleitexcd,
#   tleitemes,   tleitedias,
#   tleitformes, tleitefordia,
#   tliqmes,     tliqdia,
#   tsolidomes,  tsolidodia)
# )

# MAES MISSING (N=57)
# maes com NA em taleitexcm pelo oq se observou utilizaram outros meios (sem ser aleitamento materno)
# para alimentar o nenem logo no in?cio de vida deste
# entao foi visto caso a caso e observado quais sao possiveis recuperar (sendo amamentacao_exclusiva=0)

maes_missing_taleitexcm = which( is.na(brisa_filtrada$taleitexcm))

# View(brisa_filtrada[maes_missing_taleitexcm,] %>% select(  taleitexcd,
#   tleitemes,   tleitedias,
#   tleitformes, tleitefordia,
#   tliqmes,     tliqdia,
#   tsolidomes,  tsolidodia)
# )

colunas_interesse_dias = c(which( colnames(brisa_filtrada) == "tleitedias" ),
                           which( colnames(brisa_filtrada) == "tleitefordia" ),
                           which( colnames(brisa_filtrada) == "tliqdia" ),
                           which( colnames(brisa_filtrada) == "tsolidodia" ))

# maes com missing em toda as variaveis de alimentacao
maes_missing_em_tudo = which(
  rowSums(
    is.na(
      brisa_filtrada[,c(colunas_interesse,colunas_interesse_dias)]))
  #numero de NA == numero de colunas
  ==ncol(brisa_filtrada[,c(colunas_interesse,colunas_interesse_dias)])
  )

#removendo as maes que devem permanecer missing em amamentacao_exclusiva
# obs: lembrando que foi visto caso a caso
maes_mudar_missing_amamentacao_exclusiva = maes_missing_taleitexcm[ 
  !maes_missing_taleitexcm %in% c(maes_missing_em_tudo, 493, 746, 678, 710)]

#mudando de NA para 0, as maes que fizeram uso de outra alimentacao alem do aleitam. materno
#desde os primeiros dias (em geral foram formula ou leite)

brisa_filtrada$amamentacao_exclusiva[maes_mudar_missing_amamentacao_exclusiva] = 0

rm(maes_missing_em_tudo, maes_missing_taleitexcm, maes_mudar_missing_amamentacao_exclusiva,
   colunas_interesse, colunas_interesse_dias)

### Criando variavel de amamentacao exclusiva por pelo menos X messes

# 6 meses

brisa_filtrada = brisa_filtrada %>% mutate(
  amamentacao_exclusiva_6meses = case_when(
    amamentacao_exclusiva >= 6   ~ 1,
    amamentacao_exclusiva <  6   ~ 0,
    is.na(amamentacao_exclusiva) ~ NA_real_
  )
)

# View(brisa_filtrada %>% select( amamentacao_exclusiva_6meses, amamentacao_exclusiva))
# table(brisa_filtrada$amamentacao_exclusiva_6meses, exclude = NULL)
# prop.table(table(brisa_filtrada$amamentacao_exclusiva_6meses))

##### Hospital de nascimento do bebe ##############################################

#Criando dummy para cada hospital
# table(brisa_filtrada$pnhospital, exclude = NULL)

# Hospital das Clinicas 12
# Hospital Ribeirania 13
# Hospital Santa Lydia 15
# Hospital Santa Casa 16
# Mater 17
# H. Sinha Junqueira 18
# Hospital S?o Paulo 19

## Corrindo maes com hospital = 20 que nao foram encontradas informacoes desse hospital

# a mae 733 tem o hospital = 18

brisa_filtrada$pnhospital[733] = 18

# criando dummies de hospital
hospitais = unique(brisa_filtrada$pnhospital) 

for (numero_hospital in 1:length(hospitais)) {
  qual_hospital = hospitais[numero_hospital]
  
  nome_hospital = paste0("hospital_",qual_hospital)
  
  if(qual_hospital != 20){
    
    brisa_filtrada[,nome_hospital] = 0
    
    for (linha in 1:nrow(brisa_filtrada)) {
      
      if(brisa_filtrada$pnhospital[linha] == qual_hospital)
        
        brisa_filtrada[linha,nome_hospital] = 1
    }
    
  }


}
rm(qual_hospital, nome_hospital, linha, hospitais, numero_hospital)

brisa_filtrada$pnhospital = ifelse(brisa_filtrada$pnhospital == 20,
                                   NA_real_,
                                   brisa_filtrada$pnhospital)

# table(brisa_filtrada$hospital_12, exclude = NULL)
# table(brisa_filtrada$hospital_13, exclude = NULL)
# table(brisa_filtrada$hospital_15, exclude = NULL)
# table(brisa_filtrada$hospital_16, exclude = NULL)
# table(brisa_filtrada$hospital_17, exclude = NULL)
# table(brisa_filtrada$hospital_18, exclude = NULL)
# table(brisa_filtrada$hospital_19, exclude = NULL)



## Dummy para hospital particular/publico

# Publicos  : 12 15 16 17
# Particular: 13 14 18 19

brisa_filtrada$hospital_particular = ifelse(brisa_filtrada$pnhospital %in% c(13, 14, 18, 19),
                                            yes = 1, 
                                            ifelse(
                                              is.na(brisa_filtrada$pnhospital),
                                              NA_real_,
                                              0)
                                            )

#mae que tava com hospital =20 mas conseguimos saber o hospital
# que eh de Jundiai-SP e eh publico
brisa_filtrada$hospital_particular[761] = 0

# table(brisa_filtrada$hospital_particular, exclude = NULL)

##### Cor/raca mae ################################################################

# Branca                      =1
# Preta/negra                 =2
# Parda/mulata/cabocla/morena =3
# Amarelo/oriental            =4
# Ind?gena                    =5
# N?o sabe                    =9


brisa_filtrada = brisa_filtrada %>% mutate(
  raca_negra = case_when(
    cormae == 2 |cormae == 3                ~ 1,
    cormae == 1 | cormae == 4 | cormae == 5 ~ 0
  )
)
# table(brisa_filtrada$raca_negra, exclude=NULL)
# prop.table(table(brisa_filtrada$raca_negra, exclude=NULL))


##### Religiao #################################################

# table(brisa_filtrada$religiao, exclude = NULL)
# Catolica=01
# Evang?lica=02
# Esp?rita/Kardecista=03
# Umbanda/Candombl?=04
# Judaica=05
# Orientais=06
# Outra=07
# N?o se aplica=88
# N?o sabe=99

religioes = unique(brisa_filtrada$religiao) 

for (qual_religiao in religioes) {
  
  nome_religiao = paste0("religiao_",qual_religiao)
  
  if(qual_religiao != 88 & qual_religiao != 99){
    
    brisa_filtrada[,nome_religiao] = 0
    
    for (linha in 1:nrow(brisa_filtrada)) {
      
      if(brisa_filtrada$religiao[linha] == qual_religiao){
        
        brisa_filtrada[linha,nome_religiao] = 1
      }else if(brisa_filtrada$religiao[linha] == 88 | brisa_filtrada$religiao[linha] == 99){
        
        brisa_filtrada[linha,nome_religiao] = NA_real_
      }
        
    }
    
  }
  
}
rm(qual_religiao, nome_religiao, linha, religioes)

# table(brisa_filtrada$religiao_1,exclude = NULL)
# table(brisa_filtrada$religiao_2,exclude = NULL)
# table(brisa_filtrada$religiao_3,exclude = NULL)
# table(brisa_filtrada$religiao_4,exclude = NULL)
# table(brisa_filtrada$religiao_6,exclude = NULL)


##### Escala de Estresse Percebido (PSS-14) #######################################
# referencias: https://www.scielo.br/scielo.php?script=sci_arttext&pid=S0034-89102007000400015#:~:text=Em%20rela??o%20ao%20resultado%20da,variando%20de%205%20a%2033.
#


#antes (no pre-natal)
brisa_filtrada = brisa_filtrada %>% mutate(
  #mudando respostas pq a pontuacao eh invertida
  pnpss4_2 = case_when(
    pnpss4 == 0 ~4,
    pnpss4 == 1 ~3,
    pnpss4 == 2 ~2,
    pnpss4 == 3 ~1,
    pnpss4 == 4 ~0
  ),
  pnpss5_2 = case_when(
    pnpss5 == 0 ~4,
    pnpss5 == 1 ~3,
    pnpss5 == 2 ~2,
    pnpss5 == 3 ~1,
    pnpss5 == 4 ~0
  ),
  pnpss6_2 = case_when(
    pnpss6 == 0 ~4,
    pnpss6 == 1 ~3,
    pnpss6 == 2 ~2,
    pnpss6 == 3 ~1,
    pnpss6 == 4 ~0
  ),
  pnpss7_2 = case_when(
    pnpss7 == 0 ~4,
    pnpss7 == 1 ~3,
    pnpss7 == 2 ~2,
    pnpss7 == 3 ~1,
    pnpss7 == 4 ~0
  ),
  pnpss9_2 = case_when(
    pnpss9 == 0 ~4,
    pnpss9 == 1 ~3,
    pnpss9 == 2 ~2,
    pnpss9 == 3 ~1,
    pnpss9 == 4 ~0
  ),
  pnpss10_2 = case_when(
    pnpss10 == 0 ~4,
    pnpss10 == 1 ~3,
    pnpss10 == 2 ~2,
    pnpss10 == 3 ~1,
    pnpss10 == 4 ~0
  ),
  pnpss13_2 = case_when(
    pnpss13 == 0 ~4,
    pnpss13 == 1 ~3,
    pnpss13 == 2 ~2,
    pnpss13 == 3 ~1,
    pnpss13 == 4 ~0
  )
) %>% 
  mutate(
  escala_stress_prentl = rowSums(.[c('pnpss4_2', 'pnpss5_2', 'pnpss6_2', 'pnpss7_2',
                                     'pnpss9_2', 'pnpss10_2', 'pnpss13_2',
                                     'pnpss1', 'pnpss2', 'pnpss3', 'pnpss8',
                                     'pnpss11', 'pnpss12', 'pnpss14')],
                                 na.rm = T) ) %>%
  data.frame()
# summary(brisa_filtrada$escala_stress_prentl)

#depois (no seguimento)
brisa_filtrada = brisa_filtrada %>% mutate(
  #mudando respostas pq a pontuacao eh invertida
  tpss4_2 = case_when(
    tpss4 == 0 ~4,
    tpss4 == 1 ~3,
    tpss4 == 2 ~2,
    tpss4 == 3 ~1,
    tpss4 == 4 ~0
  ),
  tpss5_2 = case_when(
    tpss5 == 0 ~4,
    tpss5 == 1 ~3,
    tpss5 == 2 ~2,
    tpss5 == 3 ~1,
    tpss5 == 4 ~0
  ),
  tpss6_2 = case_when(
    tpss6 == 0 ~4,
    tpss6 == 1 ~3,
    tpss6 == 2 ~2,
    tpss6 == 3 ~1,
    tpss6 == 4 ~0
  ),
  tpss7_2 = case_when(
    tpss7 == 0 ~4,
    tpss7 == 1 ~3,
    tpss7 == 2 ~2,
    tpss7 == 3 ~1,
    tpss7 == 4 ~0
  ),
  tpss9_2 = case_when(
    tpss9 == 0 ~4,
    tpss9 == 1 ~3,
    tpss9 == 2 ~2,
    tpss9 == 3 ~1,
    tpss9 == 4 ~0
  ),
  tpss10_2 = case_when(
    tpss10 == 0 ~4,
    tpss10 == 1 ~3,
    tpss10 == 2 ~2,
    tpss10 == 3 ~1,
    tpss10 == 4 ~0
  ),
  tpss13_2 = case_when(
    tpss13 == 0 ~4,
    tpss13 == 1 ~3,
    tpss13 == 2 ~2,
    tpss13 == 3 ~1,
    tpss13 == 4 ~0
  )
) %>% 
  mutate(
  escala_stress_seguim = rowSums(.[c("tpss4_2", "tpss5_2", "tpss6_2", "tpss7_2",
                                     "tpss9_2", "tpss10_2", "tpss13_2",
                                     "tpss1", "tpss2", "tpss3", "tpss8",
                                     "tpss11", "tpss12", "tpss14")],
                                 na.rm = T) 
  ) 
  data.frame()
# summary(brisa_filtrada$escala_stress_seguim)


##### Relato de vida estressante - vitima de agressao fisica  ####
# 
# ## teste diferen?a entre PNAAS2 e PNREVE8
# table(brisa_filtrada$pnaas2)
# table(brisa_filtrada$pnreve8)
# 
# brisa_filtrada = brisa_filtrada %>% mutate(
#   teste_pnaas2_pnreve8 = case_when(
#     pnreve8==1 & pnaas2==2 ~"n?o agred. pnaas2 mas agred. pnreve8",
#     pnreve8==1 & pnaas2==1 ~"agredida nos dois",
#     pnreve8==2 & pnaas2==2 ~"nao agredida nos dois",
#     pnreve8==2 & pnaas2==1 ~"agred. pnreve8 mas n?o agred. pnaas2"
#   ))
# table(brisa_filtrada$teste_pnaas2_pnreve8)
# 
# ## teste diferen?a entre Viol.Domestica e PNREVE8
# table(brisa_filtrada$viol_dom)
# table(brisa_filtrada$pnreve8)
# 
# brisa_filtrada = brisa_filtrada %>% mutate(
#   teste_viol_dom_pnreve8 = case_when(
#     pnreve8==1 & viol_dom==0 ~"n?o sofreu viol. dom. mas foi agred. pnreve8",
#     pnreve8==1 & viol_dom==1 ~"agredida nos dois",
#     pnreve8==2 & viol_dom==0 ~"nao agredida nos dois",
#     pnreve8==2 & viol_dom==1 ~"agred. pnreve8 mas n?o sofreu viol. dom."
#   ))
# table(brisa_filtrada$teste_viol_dom_pnreve8)
# ##
# table(brisa_filtrada$pnreve8)


#### Mora com o marido (ver se mudou de antes-pre natal e depois-segmento) ####

#antes
# prop.table(table(brisa_filtrada$pnmoracompp))
#table(is.na(brisa_filtrada$pnmoracompp))# n?o tem NA

brisa_filtrada = brisa_filtrada %>% mutate(
  mora_marido_prentl = case_when(
    pnmoracompp==1 ~1,
    pnmoracompp==2  ~0
  )
)
# prop.table(table(brisa_filtrada$mora_marido_antes))


#depois
# prop.table(table(brisa_filtrada$tmarido))
#table(is.na(brisa_filtrada$tmarido))# tem tr?s NA's


brisa_filtrada = brisa_filtrada %>% mutate(
  mora_marido_seguim = case_when(
    tmarido==1 ~1,
    tmarido==2  ~0
  )
)
# prop.table(table(brisa_filtrada$mora_marido_depois))
# 
# #mudan?a de estado de morar com o marido antes e depois
# 
# brisa_filtrada = brisa_filtrada %>% mutate(
#   mudanca_mora_marido = c(mora_marido_antes==mora_marido_depois)) %>%
#   mutate(
#     mudanca_mora_marido_separou = case_when(
#     mudanca_mora_marido==FALSE & mora_marido_antes==1 ~"separou",
#     mudanca_mora_marido==FALSE  & mora_marido_antes==0 ~"juntou",
#     mudanca_mora_marido==TRUE ~"nao_mudou"
#   ))
# 
# table(brisa_filtrada$mudanca_mora_marido) #TRUE= n?o mudou de estado
# table(brisa_filtrada$mudanca_mora_marido_separou)

#filtrando somente para as mulheres que sofreram viol?ncia dom?stica
# brisa_viol_domestica = brisa_filtrada %>% filter(viol_dom==1)
# 
# View(brisa_viol_domestica%>%select(mora_marido_antes,mora_marido_depois,mudanca_mora_marido,mudanca_mora_marido_separou)) 
# 
# table(brisa_viol_domestica$mudanca_mora_marido) #TRUE= n?o mudou de estado
# prop.table(table(brisa_viol_domestica$mudanca_mora_marido))
# 
# table(brisa_viol_domestica$mudanca_mora_marido_separou)
# prop.table(table(brisa_viol_domestica$mudanca_mora_marido_separou))


##### Casada ##########################
# Casada=1
# Uni?o consensual (Mora junto)=2
# Solteira=3
# Separada/desquitada/divorciada=4
# Vi?va=5
# N?o sabe=9


#antes
# table(brisa_filtrada$pnsitcong, exclude = NULL)

brisa_filtrada = brisa_filtrada %>% mutate(
  casada_marido_prentl = case_when(
    pnsitcong == 1 | pnsitcong == 2                     ~ 1,
    pnsitcong == 3 | pnsitcong == 4 |  pnsitcong == 5   ~ 0
  )
)
# prop.table(table(brisa_filtrada$casada_marido_antes))
# table(brisa_filtrada$casada_marido_prentl, exclude = NULL)

#depois
# table(brisa_filtrada$tsitconj, exclude = NULL)

brisa_filtrada = brisa_filtrada %>% mutate(
  casada_marido_seguimento = case_when(
    tsitconj == 1 | tsitconj == 2                 ~ 1,
    tsitconj == 3 | tsitconj == 4 | tsitconj == 5 ~ 0
  )
)
# prop.table(table(brisa_filtrada$casada_marido_seguimento))

#mudan?a de estado de estar casada antes e depois

# brisa_filtrada = brisa_filtrada %>% mutate(mudanca_casada_marido = c(casada_marido_antes==casada_marido_depois))%>%
#   mutate(mudanca_casada_marido_separou = case_when(
#     mudanca_casada_marido==FALSE & casada_marido_antes==1 ~"separou",
#     mudanca_casada_marido==FALSE  & casada_marido_antes==0 ~"juntou",
#     mudanca_casada_marido==TRUE ~"nao_mudou"
#   ))
# 
# table(brisa_filtrada$mudanca_casada_marido) #TRUE= n?o mudou de estado
# table(brisa_filtrada$mudanca_casada_marido_separou)
# 
# #filtrando somente para as mulheres que sofreram viol?ncia dom?stica
# brisa_viol_domestica = brisa_filtrada %>% filter(viol_dom==1)
# 
# View(brisa_viol_domestica%>%select(casada_marido_antes,casada_marido_depois,mudanca_casada_marido,mudanca_casada_marido_separou)) 
# 
# table(brisa_viol_domestica$mudanca_casada_marido) #TRUE= n?o mudou de estado
# prop.table(table(brisa_viol_domestica$mudanca_casada_marido))
# 
# table(brisa_viol_domestica$mudanca_casada_marido_separou)
# prop.table(table(brisa_viol_domestica$mudanca_casada_marido_separou))

##### Datas da entrevista #########################################

brisa_filtrada = brisa_filtrada %>%
  mutate(ano_entrev_prenatal   = as.numeric(str_sub(pndataentp, start= -4)),
         ano_entrev_nascim     = as.numeric(str_sub(pndataentnasc, start= -4)),
         ano_entrev_seguimento = as.numeric(str_sub(tdataent,   start= -4)),
         diferenca_nasc        = c(ano_entrev_seguimento - ano_entrev_nascim)
  )


##### Bloco R - Violencia #########################################
### Neste ?ltimo ano (12 meses), algu?m lhe bateu, esbofeteou, chutou ou machucou fisicamente?
# table(brisa_filtrada$pnaas2) #Neste ?ltimo ano (12 meses), algu?m lhe bateu, esbofeteou, chutou ou machucou fisicamente?
# prop.table(table(brisa_filtrada$pnaas2))
# 
# #pnaas3, pnaas4, pnaas5: marido, ex-marido e namorado, respectivamente
# #77 = n?o quis responder; 88= n?o se aplica
# table(brisa_filtrada$pnaas3) #47 #caso sim acima, numero de vezes que o MARIDO fez
# table(brisa_filtrada$pnaas4) #23 #caso sim acima, numero de vezes que o EX-MARIDO fez
# table(brisa_filtrada$pnaas5) #17 #caso sim acima, numero de vezes que o NAMORADO fez
# 
# ##existem viola??es de fluxo nesses dados de viol?ncia, ent?o ser?o corrigidas a seguir
# # uma mulher respondeu que sim, sofreu viol., na pnaas2 mas respondeu tudo 88 nas pnaas3:7 seguintes
# # ent?o o 88 ser? mudado para 77
# 
# conta=0 #confirmando quantas existem nessa situa??o e identificando a linha
# for(x in 1:nrow(brisa_filtrada)){
#   #print(x)
#   try(
#     if(brisa_filtrada$pnaas2[x]==1 & brisa_filtrada$pnaas3[x]==88 &
#        brisa_filtrada$pnaas4[x]==88 & brisa_filtrada$pnaas5[x]==88 &
#        brisa_filtrada$pnaas6[x]==88 & brisa_filtrada$pnaas7[x]==88
#     ){
#       conta = conta + 1
#       linha = x 
#     })
# }
# conta
# linha
# rm(conta,x)
# 
# which( colnames(brisa_filtrada)=="pnaas3" ) ; which( colnames(brisa_filtrada)=="pnaas7" )
# 
# brisa_filtrada[linha,276:280] = 77
# 
# rm(linha)
# ##outro caso de viola??o
# # mulher que respondeu que n?o, n?o sofreu viol., na pnaas2 mas respondeu o quanto sofreu para 
# # algum dos perpetradores
# # ent?o sera mudado pnaas2 para 1
# conta=0
# linhas = c()
# for(x in 1:nrow(brisa_filtrada)){
#   #print(x)
#   try(
#     if(brisa_filtrada$pnaas2[x]==2 & (brisa_filtrada$pnaas3[x]<77 |
#        brisa_filtrada$pnaas4[x]<77 | brisa_filtrada$pnaas5[x]<77 |
#        brisa_filtrada$pnaas6[x]<77 | brisa_filtrada$pnaas7[x]<77)
#     ){
#       conta = conta + 1
#       linhas = c(linhas,x)
#     })
# }
# conta
# linhas
# 
# brisa_filtrada$pnaas2[linhas]= 1
# 
# rm(conta,linhas,x)
# 
# ##note ainda que existem casos em que a mulher respondeu que n?o, n?o sofreu viol.,
# #mas nas respostas seguintes est? 77
# 
# #mas ainda nada ser? feito a respeito desse caso
# 
# 
# ## criando as vari?veis de viol?ncia
# brisa_filtrada = brisa_filtrada %>% mutate(
#   viol_dom = case_when(
#     (pnaas3 <77 | pnaas4 <77 |pnaas5 <77 ) & pnaas2==1 ~ 1,
#     pnaas3 ==88 & pnaas4 ==88 & pnaas5 ==88            ~ 0
#   ),
#   viol_dom_so_marido = case_when(
#     (pnaas3 <77) & pnaas2==1   ~ 1,
#     pnaas3 ==88                ~ 0
#   ),
#   viol_dom_so_ex = case_when(
#     (pnaas4 <77) & pnaas2==1   ~ 1,
#     pnaas4 ==88                ~ 0
#   ),
#   viol_dom_so_namorado = case_when(
#     (pnaas5 <77 ) & pnaas2==1  ~ 1,
#     pnaas5 ==88                ~ 0
#   ),
#   viol_dom_marido_namorado = case_when(
#     viol_dom_so_marido==1 | viol_dom_so_namorado==1 ~ 1,
#     viol_dom_so_marido==0 & viol_dom_so_namorado==0 ~ 0
#   ),
#   viol = case_when(
#     pnaas2==1 | pnaas3 <77 | pnaas4 <77 | pnaas5 <77 |pnaas6 <77 | pnaas7 <77 ~ 1,
#     pnaas3 ==88 & pnaas4 ==88 & pnaas5 ==88 &pnaas6 ==88 & pnaas7 ==88        ~ 0
#   )
# )
# 
# View(brisa_filtrada %>% select(pnaas2:pnaas7,viol_dom,viol_dom_so_marido,viol_dom_so_ex,viol_dom_so_namorado))
# View(brisa_filtrada %>% select(pnaas3:pnaas5,viol_dom,viol_dom_so_marido,viol_dom_so_ex,viol_dom_so_namorado))
# 
# table(brisa_filtrada$viol)
# table(brisa_filtrada$viol_dom)
# table(brisa_filtrada$viol_dom_so_marido)
# table(brisa_filtrada$viol_dom_so_ex)
# table(brisa_filtrada$viol_dom_so_namorado)
# table(brisa_filtrada$viol_dom_marido_namorado)

# prop.table(table(brisa_filtrada$viol_dom))
# prop.table(table(brisa_filtrada$viol_dom_so_marido))
# prop.table(table(brisa_filtrada$viol)) 

#como tem mulher que respondeu que n?o sofreu viol (pnaas2) mas respondeu as perguntas seguintes de perpetrador (pnaas3-7)

# `%notin%` <- Negate(`%in%`)
# 
# brisa_filtrada = brisa_filtrada %>% mutate(
#   viol2 = case_when(
#     pnaas2==1 | pnaas3 %notin% c(77,88) | pnaas4 %notin% c(77,88) |
#       pnaas5 %notin% c(77,88) |pnaas6 %notin% c(77,88) | pnaas7 %notin% c(77,88) ~1,
#     pnaas2==2 | pnaas3 %in% c(88) | pnaas4 %in% c(88) |
#       pnaas5 %in% c(88) |pnaas6 %in% c(88) | pnaas7 %in% c(88) ~0
#   )
# )
# 
# prop.table(table(brisa_filtrada$viol2)) 
#### comparacao bloco T e bloco R para violencia ####
# table(brisa_filtrada$viol_dom) #bloco R
# table(brisa_filtrada$viol_dom_marido_namorado) #bloco R
# table(brisa_filtrada$viol_gravidez_domest) #bloco T
# table(brisa_filtrada$viol_12meses_domest) #bloco T

#usar viol_dom e viol_dom_marido_namorado da no mesmo n?emro de viol. dom em comum
# em_commum_viol_dom = 0
# for(mulher in 1:nrow(brisa_filtrada)){
#   print(mulher)
#   try(
#     if(brisa_filtrada$viol_dom_marido_namorado[mulher] ==1 & 
#        brisa_filtrada$viol_gravidez_domest[mulher] ==1 ){
#       
#       em_commum_viol_dom = em_commum_viol_dom + 1
#     })
# }
# em_commum_viol_dom
# 
# View(brisa_filtrada %>% select(viol_dom,viol_gravidez_domest))

##### Exportando a base ########################################

# A base esta sendo exportada pois as estimacoes serao feitas no stat.anova

#uses a comma (",") for the decimal point and a semicolon (";") for the separator.
# write.table(brisa_filtrada,
#            #"C:\\Users\\ville\\OneDrive\\Área de Trabalho\\Dissertação Mestrado\\Programas R\\brisa_final.csv",
#            "G:/Meu Drive/Dissertacao pc/Dissertacao Mestrado/Programas R/brisa_final.csv",
#            row.names = F,
#            dec = ".",
#            sep = ";",
#            na = ".")

