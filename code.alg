algoritmo "Algoritmo em VisuAlg para cálculo de condutores elétricos de entrada e disjuntores"
// Função : Calcular condutores elétricos de entrada e disjuntores.
// Autor : Roberto Luiz Debarba
// Data : 08/07/2013
//Informações=============================================
procedimento informacoes
inicio
     escreval ("INFORMAÇÕES")
     escreval ("------------------------------------------------------------------------------------------------")
     escreval
     escreva ("Digite o número de comodos na casa (max. 20): ")
     leia (numcomodos)
     escreva ("Digite a tensão elétrica da casa (127 ou 220): ")
     leia (tensao)
     calc <- 0
     limpatela
     escreval ("INFORMAÇÕES")
     escreval ("------------------------------------------------------------------------------------------------")
     escreval
     repita
          calc <- calc + 1
          escreval ("Comodo ", calc)
          escreva ("Digite o nome do comodo: ")
          leia (comodonome[calc])
          comodo[calc,2] <- 0
          se (comodonome[calc] = "Sala") ou (comodonome[calc] = "Dispensa") ou (comodonome[calc] = "Quarto") ou (comodonome[calc] = "Hall") ou (comodonome[calc] = "Corredor") ou (comodonome[calc] = "Closet") entao
               comodo[calc,2] <- 1
          fimse
          se (comodonome[calc] = "Copa") ou (comodonome[calc] = "Cozinha") ou (comodonome[calc] = "Lavanderia") ou (comodonome[calc] = "Lavação") ou (comodonome[calc] = "Área de serviço") ou (comodonome[calc] = "Banheiro") entao
               comodo[calc,2] <- 2
          fimse
          se (comodo[calc,2] = 0) entao
               escreval ("Tipo de comodo não identificado!")
               escreval ("1 - Sala, Quarto, Hall, Corredor, Closet, semelhantes;")
               escreval ("2 - Copa, Cozinha, Lavanderia, Banheiro, semelhantes.")
               escreva ("Digite o número do grupo no qual o comodo digitado pertence: ")
               leia (comodo[calc,2])
               se (comodo[calc,2] <> 1) e (comodo[calc,2] <> 2) entao
                    repita
                         escreval ("Número inválido!")
                         escreva ("Digite novamente: ")
                         leia (comodo[calc,2])
                    ate (comodo[calc,2] = 1) ou (comodo[calc,2] = 2)
               fimse
          fimse
          escreva ("Digite a área do comodo (m²): ")
          leia (comodo[calc,1])
          escreva ("Digite o perimetro do comodo (m): ")
          leia (comodo[calc,6])
          escreval
     ate calc = numcomodos
fimprocedimento
//FIMPROCEDIMENTO=========================================
//Iluminação==============================================
procedimento iluminacao
var
     resto : inteiro
inicio
     limpatela
     calc <- 0
     repita
          calc <- calc + 1
          resto <- (int((comodo[calc,1] - 6) / 4))* 60
          comodo[calc,3] <- 100 + resto
     ate calc = numcomodos
fimprocedimento
//FIMROCEDIMENTO==========================================
//TUGS====================================================
procedimento tugs
var
     num1, totaltomadas, sobra, num2 : real
inicio
     calc <- 0
     repita
          limpatela
          calc <- calc + 1
          se (comodo[calc,2] = 1) entao
               num1 <- (int(comodo[calc,6] / 5))
               num2 <- ((comodo[calc,6] / 5) - num1)
               se (num2 <> 0) entao
                    num2 <- 1
               senao
                    num2 <- 0
               fimse
               comodo[calc,4] <- (num1 + num2) * 100
          senao
               num1 <- (int(comodo[calc,6] / 3.5))
               num2 <- ((comodo[calc,6] / 3.5) - num1)
               se (num2 <> 0) entao
                    num2 <- 1
               senao
                    num2 <- 0
               fimse
               totaltomadas <- num1 + num2
               se (totaltomadas <= 3) entao
                    comodo[calc,4] <- totaltomadas * 600
               senao
                    sobra <- (totaltomadas - 3) * 100
                    comodo[calc,4] <- (1800 + sobra)
               fimse
          fimse
     ate calc = numcomodos
fimprocedimento
//FIMPROCEDIMENTO=========================================
//tues====================================================
procedimento tues
var
     aquecedor, paquecedor, chuveiro, pchuveiro, ar, par, forno, pforno, numtues, agua, pagua : inteiro
     demanda : real
inicio
     limpatela
     escreval ("INFORMAÇÕES")
     escreval ("------------------------------------------------------------------------------------------------")
     escreval
     escreval ("Digite a quantidade dos seguintes aparelhos elétricos presentes na casa...")
     escreva ("Aquecedores de Ar: ")
     leia (aquecedor)
     paquecedor <- aquecedor * 1300
     escreva ("Chuveiros Elétricos: ")
     leia (chuveiro)
     pchuveiro <- chuveiro * 7000
     escreva ("Ar Condicionado: ")
     leia (ar)
     par <- ar * 1300
     escreva ("Forno Elétrico: ")
     leia (forno)
     pforno <- forno * 1200
     escreva ("Aquecedor de Água: ")
     leia (agua)
     pagua <- agua * 5000
     numtues <- aquecedor + chuveiro + ar + forno + agua
     escolha numtues
          caso 1
               demanda <- 1
          caso 2
               demanda <- 1
          caso 3
               demanda <- 0.84
          caso 4
               demanda <- 0.76
          caso 5
               demanda <- 0.7
          caso 6
               demanda <- 0.65
          caso 7
               demanda <- 0.6
          caso 8
               demanda <- 0.57
          caso 9
               demanda <- 0.54
          caso 10
               demanda <- 0.52
          caso 11
               demanda <- 0.49
          caso 12
               demanda <- 0.48
          caso 13
               demanda <- 0.46
          caso 14
               demanda <- 0.45
          caso 15
               demanda <- 0.44
          caso 16
               demanda <- 0.43
          outrocaso
               demanda <- 0.4
     fimescolha
     pespecificos <- (paquecedor + pchuveiro + par + pforno + pagua) * demanda
     limpatela
     escreval ("INFORMAÇÕES")
     escreval ("------------------------------------------------------------------------------------------------")
     escreval
     escreval ("Informações inseridas com sucesso!")
     escreval ("Pressione ENTER para continuar...")
     leia (varinutil)
fimprocedimento
//FIMPROCEDIMENTO=========================================
//pporcomodo==============================================
procedimento pporcomodo
inicio
     calc <- 0
          repita
               calc <- calc + 1
               comodo[calc,5] <- comodo[calc,3] + comodo[calc,4]
          ate calc = numcomodos
fimprocedimento
//FIMPROCEDIMENTO==========================================
//Potencia Geral =========================================
procedimento potenciageral
var
     luz, tug, num1, num2 : real
inicio
     calc <- 0
     tug <- 0
     luz <- 0
     repita
          calc <- calc + 1
          luz <- comodo[calc,3] + luz
          tug <- comodo[calc,4] + tug
     ate calc = numcomodos
     num1 <- luz + (tug * 0.8)
     se (num1 <= 1000) entao
          num2 <- num1 * 0.86
     fimse
     se (num1 > 1000) e (num1 <= 2000) entao
          num2 <- num1 * 0.75
     fimse
     se (num1 > 2000) e (num1 <= 3000) entao
          num2 <- num1 * 0.66
     fimse
     se (num1 > 3000) e (num1 <= 4000) entao
          num2 <- num1 * 0.59
     fimse
     se (num1 > 4000) e (num1 <= 5000) entao
          num2 <- num1 * 0.52
     fimse
     se (num1 > 5000) e (num1 <= 6000) entao
          num2 <- num1 * 0.45
     fimse
     se (num1 > 6000) e (num1 <= 7000) entao
          num2 <- num1 * 0.4
     fimse
     se (num1 > 7000) e (num1 <= 8000) entao
          num2 <- num1 * 0.35
     fimse
     se (num1 > 8000) e (num1 <= 9000) entao
          num2 <- num1 * 0.31
     fimse
     se (num1 > 9000) e (num1 <= 10000) entao
          num2 <- num1 * 0.27
     fimse
     se (num1 > 10000) entao
          num2 <- num1 * 0.24
     fimse
     ptotal <- num2 + pespecificos
fimprocedimento
//FIMPROCEDIMENTO=========================================
//Disjuntor===============================================
procedimento disjuntor
inicio
     se (ptotal < 15000) entao
          tipo <- "Monofásico"
          corrente <- ptotal / tensao
     fimse
     se (ptotal >= 15000) e (ptotal < 25000) entao
          tipo <- "Bifásico"
          corrente <- (ptotal / 2) / tensao
     fimse
     se (ptotal >= 25000) e (ptotal < 75000) entao
          tipo <- "Trifásico"
          corrente <- (ptotal / 3) / tensao
     fimse
     se (ptotal >= 75000) entao
          tipo <- "Transformador"
          corrente <- 0
     fimse
     se (corrente > 0) e (corrente < 1) entao
          pdisjuntor <- 1
     fimse
     se (corrente >= 1) e (corrente < 2) entao
          pdisjuntor <- 2
     fimse
     se (corrente >= 2) e (corrente < 6) entao
          pdisjuntor <- 6
     fimse
     se (corrente >= 6) e (corrente < 10) entao
          pdisjuntor <- 10
     fimse
     se (corrente >= 10) e (corrente < 16) entao
          pdisjuntor <- 16
     fimse
     se (corrente >= 16) e (corrente < 20) entao
          pdisjuntor <- 20
     fimse
     se (corrente >= 20) e (corrente < 25) entao
          pdisjuntor <- 25
     fimse
     se (corrente >= 25) e (corrente < 32) entao
          pdisjuntor <- 32
     fimse
     se (corrente >= 32) e (corrente < 40) entao
          pdisjuntor <- 40
     fimse
     se (corrente >= 40) e (corrente < 50) entao
          pdisjuntor <- 50
     fimse
     se (corrente >= 50) e (corrente < 60) entao
          pdisjuntor <- 60
     fimse
     se (corrente >= 60) e (corrente < 75) entao
          pdisjuntor <- 75
     fimse
     se (corrente >= 75) e (corrente < 90) entao
          pdisjuntor <- 90
     fimse
     se (corrente >= 90) e (corrente < 100) entao
          pdisjuntor <- 100
     fimse
     se (corrente >= 100) e (corrente < 125) entao
          pdisjuntor <- 125
     fimse
     se (corrente >= 125) e (corrente < 150) entao
          pdisjuntor <- 150
     fimse
     se (corrente >= 150) e (corrente < 175) entao
          pdisjuntor <- 175
     fimse
     se (corrente >= 175) e (corrente <= 200) entao
          pdisjuntor <- 200
     fimse
     se (corrente = 0) ou (corrente > 200) entao
          pdisjuntor <- 0
     fimse
fimprocedimento
//FIMPROCEDIMENTO=========================================
//Cabos===================================================
procedimento cabos
inicio
     se (tipo = "Monofásico") entao
          se (pdisjuntor > 0) e (pdisjuntor < 69) entao
               cabo <- 10
          fimse
          se (pdisjuntor >= 69) e (pdisjuntor < 90) entao
               cabo <- 16
          fimse
          se (pdisjuntor >= 90) e (pdisjuntor < 111) entao
               cabo <- 25
          fimse
          se (pdisjuntor >= 111) e (pdisjuntor < 133) entao
               cabo <- 35
          fimse
          se (pdisjuntor >= 133) e (pdisjuntor < 168) entao
               cabo <- 50
          fimse
          se (pdisjuntor >= 168) e (pdisjuntor < 201) entao
               cabo <- 70
          fimse
          se (pdisjuntor >= 201) ou (pdisjuntor = 0) entao
               cabo <- 0
          fimse
     senao           //-----Bifásico e Trifásico---------------
          se (pdisjuntor > 0) e (pdisjuntor < 62) entao
               cabo <- 10
          fimse
          se (pdisjuntor >= 62) e (pdisjuntor < 80) entao
               cabo <- 16
          fimse
          se (pdisjuntor >= 80) e (pdisjuntor < 99) entao
               cabo <- 25
          fimse
          se (pdisjuntor >= 99) e (pdisjuntor < 118) entao
               cabo <- 35
          fimse
          se (pdisjuntor >= 118) e (pdisjuntor < 149) entao
               cabo <- 50
          fimse
          se (pdisjuntor >= 149) e (pdisjuntor < 179) entao
               cabo <- 70
          fimse
          se (pdisjuntor >= 179) ou (pdisjuntor = 0) entao
               cabo <- 0
          fimse
     fimse
fimprocedimento
//FIMPROCEDIMENTO=========================================
//Resultados==============================================
procedimento resultado
inicio
     escreval ("RESULTADO")
     escreval ("------------------------------------------------------------------------------------------------")
     escreval
     calc <- 0
     escreval ("Comodo   /    Área    /    Iluminação    /    Tugs    /    Potência Elétrica Geral")
     escreval
     repita
          calc <- calc + 1
          escreval (comodonome[calc], "        ", comodo[calc,1], " m²        ", comodo[calc,3]," W        ", comodo[calc,4]," W          ", comodo[calc,5], " W")
     ate calc = numcomodos
     escreval
     escreval ("Potência Elétrica das tomadas especificas: ", pespecificos, " W")
     escreval
     escreval ("-----------------------------------------------------")
     escreval
     escreval ("Potência Elétrica total da casa (Considerado Fator de Demanda): ", ptotal, " W")
     escreval
     escreval ("-----------------------------------------------------")
     escreval
     se (cabo <> 0) entao
          escreval ("Espessura de cabos indicada: ", cabo, " mm²")
     senao
          escreval ("Espessura de cabos indicada: ERRO!")
          escreval ("Corrente muito alta. Espessura indicada não consta em nosso banco de dados!")
     fimse
     escreval
     se (pdisjuntor <> 0) entao
          escreval ("Disjuntor indicado: ", tipo, " de ", pdisjuntor, " A")
     senao
          escreval ("Disjuntor indicado: ", tipo)
          escreval ("Potência elétrica muito alta!")
          escreval ("A especificação do equipamento não consta em nosso banco de dados!")
     fimse
fimprocedimento
//FIMPROCEDIMENTO=========================================
var
     comodo : vetor [1..20,1..6] de real
     comodonome : vetor [1..20] de caractere
     corrente, cabo, pespecificos, numcomodos, tensao, ptotal, opcao, pdisjuntor : real
     calc : inteiro
     varinutil, tipo, continuar : caractere
inicio
     escreval ("Calculadora de condutores elétricos de entrada e disjuntores")
     escreval ("-----------------------------------------------------------------------------------------------")
     escreval
     escreval ("|---------Menu---------|")
     escreval ("|                      |")
     escreval ("|     1 - Iniciar      |")
     escreval ("|      0 - Sair        |")
     escreval ("|                      |")
     escreval ("|----------------------|")
     escreval
     escreval
     escreva ("Digite uma opção: ")
     leia (opcao)
     limpatela
     se (opcao = 1) entao
          continuar <- "s"
          enquanto (continuar = "s") faca
               limpatela
               informacoes
               iluminacao
               tugs
               tues
               potenciageral
               disjuntor
               cabos
               pporcomodo
               limpatela
               resultado
               escreval
               escreva ("Calcular novamente (s/n): ")
               leia (continuar)
          fimenquanto
     fimse
fimalgoritmo
