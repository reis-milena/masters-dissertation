/*****************************************************************
Dissertacao Mestrado - Milena Villela
Ano: 2022
Objetivo do codigo: estimar efeitos marginais do multinomial logit
Base de dados: Coorte Brisa 2010
******************************************************************/

/* PACOTES USADOS*/
{
* ssc install estout, replace
}

/* LENDO A BASE FINAL GERADA NO R */
{
import delimited "G:\Meu Drive\Dissertacao pc\Dissertacao Mestrado\Programas R\brisa_estimacoes_total.csv", clear

import delimited "G:\Meu Drive\Dissertacao pc\Dissertacao Mestrado\Programas R\brisa_estimacoes.csv", clear

import delimited "G:\Meu Drive\Dissertacao pc\Dissertacao Mestrado\Programas R\brisa_estimacoes_sl.csv", clear
}

/* ARRUMANDO AS VARIAVEIS */
{

/*foreach variavel of var ocupada_formal_seguim ocupada_formal_prentl educacao_mae raca_negra casada_marido_seguimento casada_marido_prentl faz_todo_trab_domestico depressao_epds filho_creche bayley_risco municipio { 
                encode `variavel', gen(`variavel'_num) 
        }
*/
		
label define educacao_mael                    1 "Illiterate/elementary/middle school" 2 "High school"     3 "Undergraduate compl./incompl.", add
label values educacao_mae educacao_mael

label define raca_negral                      0 "Non-black"           1  "Black" , add
label values raca_negra raca_negral

label define ocupada_formal_prentll           0 "Not working "   1 "Formal working" 2 "Informal working", add
label values ocupada_formal_prentl ocupada_formal_prentll

label define ocupada_formal_seguiml           0 "Not working"         1 "Formal working" 2 "Informal working", add
label values ocupada_formal_seguim ocupada_formal_seguiml

*label define casada_marido_prentll            0 "Not married during preg."         1  "Married during preg." , add
*label values casada_marido_prentl casada_marido_prentll

label define casada_marido_seguimentol        0 "Not married after preg."         1  "Married after preg." , add
label values casada_marido_seguimento casada_marido_seguimentol

label define filho_crechel                    0 "Not on daycare"      1  "On daycare" , add
label values filho_creche filho_crechel

label define faz_todo_trab_domesticol         0 "Share the housework"     1  "Does all the housework" , add 
label values faz_todo_trab_domestico faz_todo_trab_domesticol

label define depressao_epdsl                  0 "No postpartum depression"     1  "Postpartum depression" , add 
label values depressao_epds depressao_epdsl

label define bayley_riscol                    0 "No risk in any domain"     1  "At least one domain at risk" , add 
label values bayley_risco bayley_riscol		

* esse so roda para full sample
label define municipiol                    0 "Ribeirão Preto"     1  "São Luís" , add 
label values municipio municipiol	

}
/*ESTIMANDO MULTINOMIAL E EFEITO MARGINAL*/
{
				   
/*TheXmatrix, which represents the economic aspects, includes work status during pregnancy, a proxy for household income, and education. TheVmatrix, whichrepresents demographic aspects, includes marriage during and after pregnancy, age, number of children, and division of household labor. TheZmatrix, which represents mothers’ health, inlcudes postpartum depression. Finally, theWmatrix, which represents infor-mation about the babies, includes daycare, development at risk, and the age of the baby*/

* MULTINOMIAL
{
*** Primeiro: full sample
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.municipio i.filho_creche##i.depressao_epds, b(0)
eststo mnl1


*** Segundo: Ribeirão Preto
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.filho_creche##i.depressao_epds, b(0)
eststo mnl2

*** Terceiro: São Luís
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.filho_creche##i.depressao_epds, b(0)
eststo mnl3

*** Quarto: return to work
drop if ocupada_formal_prentl == 0
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.municipio i.filho_creche##i.depressao_epds, b(0)
eststo mnl4

*** Quinto: return to work para Ribeirao Preto
drop if ocupada_formal_prentl == 0
mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.municipio i.filho_creche##i.depressao_epds, b(0)
eststo mnl5

*** Sexto: return to work para Sao Luis
drop if ocupada_formal_prentl == 0
mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.municipio i.filho_creche##i.depressao_epds, b(0)
eststo mnl6

* Exportando
esttab mnl1 mnl2 mnl3, se label unstack nobaselevels nomtitle nonumber pr2 star(* 0.1 ** 0.05 *** 0.01) noomitted tex
esttab mnl4 mnl5 mnl6, se label unstack nobaselevels nomtitle nonumber pr2 star(* 0.1 ** 0.05 *** 0.01) noomitted tex
eststo clear
}

* ESTIMACAO EFEITO MARGINAL
{
** para visualizar
mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.municipio i.filho_creche##i.depressao_epds, b(0)

margins, dydx(*)
{/*mtable, dydx(*)
mtable, dydx(*) stat(se) // para standar error (se)*/
}
*eststo stores a copy of the active estimation results for later tabulation

** para exportar

*** Primeiro: full sample
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.municipio i.filho_creche##i.depressao_epds, b(0)
eststo mlogit
foreach dep_var_type in 1 2 {
     quietly margins, dydx(*) predict(outcome(`dep_var_type')) post
     eststo margin1_`dep_var_type', title(Outcome `dep_var_type')
     estimates restore mlogit
}
eststo drop mlogit

*** Segundo: Ribeirão Preto
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.filho_creche##i.depressao_epds, b(0)
eststo mlogit
foreach dep_var_type in 1 2 {
     quietly margins, dydx(*) predict(outcome(`dep_var_type')) post
     eststo margin2_`dep_var_type', title(Outcome `dep_var_type')
     estimates restore mlogit
}
eststo drop mlogit

*** Terceiro: São Luís
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.filho_creche##i.depressao_epds, b(0)
eststo mlogit
foreach dep_var_type in 1 2 {
     quietly margins, dydx(*) predict(outcome(`dep_var_type')) post
     eststo margin3_`dep_var_type', title(Outcome `dep_var_type')
     estimates restore mlogit
}
eststo drop mlogit

*** Quarto: return to work FULL SAMPLE
drop if ocupada_formal_prentl == 0
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.municipio i.filho_creche##i.depressao_epds, b(0)
eststo mlogit
foreach dep_var_type in 1 2 {
     quietly margins, dydx(*) predict(outcome(`dep_var_type')) post
     eststo margin4_`dep_var_type', title(Outcome `dep_var_type')
     estimates restore mlogit
}
eststo drop mlogit

*** Quinto: return to work RIBEIRAO PRETO
drop if ocupada_formal_prentl == 0
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.filho_creche##i.depressao_epds, b(0)
eststo mlogit
foreach dep_var_type in 1 2 {
     quietly margins, dydx(*) predict(outcome(`dep_var_type')) post
     eststo margin5_`dep_var_type', title(Outcome `dep_var_type')
     estimates restore mlogit
}
eststo drop mlogit

*** Sexto: return to work SAO LUIS
drop if ocupada_formal_prentl == 0
quietly mlogit ocupada_formal_seguim i.ocupada_formal_prentl densid_morador_dormitor_prentl i.educacao_mae i.raca_negra i.casada_marido_seguimento idade_mae_seguim quantos_filhos_tem_nascim i.faz_todo_trab_domestico i.depressao_epds idade_bebe_seguim i.filho_creche i.bayley_risco i.filho_creche##i.depressao_epds, b(0)
eststo mlogit
foreach dep_var_type in 1 2 {
     quietly margins, dydx(*) predict(outcome(`dep_var_type')) post
     eststo margin6_`dep_var_type', title(Outcome `dep_var_type')
     estimates restore mlogit
}
eststo drop mlogit

* Exportando
esttab margin1_1 margin1_2 margin2_1 margin2_2 margin3_1 margin3_2, se label mtitles nobaselevels nonumber pr2 star(* 0.1 ** 0.05 *** 0.01) tex
esttab margin4_1 margin4_2 margin5_1 margin5_2 margin6_1 margin6_2, se label mtitles nobaselevels nonumber pr2 star(* 0.1 ** 0.05 *** 0.01) tex
eststo clear
}
}