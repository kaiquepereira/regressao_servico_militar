##############################################################################
#                                  Pacotes                                   #
##############################################################################

#Instalação de Pacotes
install.packages("caret")
install.packages("fastDummies")
install.packages("jtools")
install.packages("kableExtra")
install.packages("knitr")
install.packages("lmtest")
install.packages("plotly")
install.packages("pROC")
install.packages("readr")
install.packages("ROCR")
install.packages("tidyverse")

#Carregamento de Pacotes
library(caret)
library(fastDummies)
library(jtools)
library(kableExtra)
library(knitr)
library(lmtest)
library(plotly)
library(pROC)
library(readr)
library(ROCR)
library(tidyverse)

##############################################################################
#                                  Dataset                                   #
##############################################################################

#Carregando a Base de Dados
sermil2022 <- read_csv("sermil2022.csv")

##############################################################################
#                              Data Wrangling                                #
##############################################################################

#Remover observações com missing values
sermil2022 <- na.omit(sermil2022)

#Remover observações cujo UF de residência não seja SP
sermil2022 <- sermil2022[sermil2022$UF_RESIDENCIA == "SP", ]

#Remover variável UF_RESIDENCIA, pois todos são de SP
sermil2022 <- subset(sermil2022, select = -UF_RESIDENCIA)

#Remover observações cujo país de residência não seja Brasil
sermil2022 <- sermil2022[sermil2022$PAIS_RESIDENCIA == "BRASIL", ]

#Remover variável PAIS_RESIDENCIA, pois todos são Brasil
sermil2022 <- subset(sermil2022, select = -PAIS_RESIDENCIA)

#Remover variável município de nascimento
sermil2022 <- subset(sermil2022, select = -c(MUN_NASCIMENTO))

#Remover Ano de vinculação, pois todos são 2022
sermil2022 <- subset(sermil2022, select = -c(VINCULACAO_ANO))

#Remover observações de mulheres
sermil2022 <- sermil2022[sermil2022$SEXO != "F", ]

#Remover variável SEXO, pois todos são homens
sermil2022 <- subset(sermil2022, select = -SEXO)

#Remover variável MUN_RESIDENCIA, pois é redundante com a jsm
sermil2022 <- subset(sermil2022, select = -MUN_RESIDENCIA)

#Remover variável MUN_JSM, pois é redundante com a jsm
sermil2022 <- subset(sermil2022, select = -MUN_JSM)

#Remover observações cujo UF da jsm não seja SP
sermil2022 <- sermil2022[sermil2022$UF_JSM == "SP", ]

#Remover variável UF_JSM, pois é redundante com a jsm
sermil2022 <- subset(sermil2022, select = -UF_JSM)

#Converter as observações em letras minúsculas
sermil2022 <- sermil2022 %>%
  mutate_all(tolower)

#Converter as variáveis em letras minúsculas
names(sermil2022) <- tolower(names(sermil2022))

#Trocar nome da variável Y
sermil2022 <- rename(sermil2022, dispensado = dispensa)

#Trocar com dispensa para sim e sem dispensa para não
sermil2022$dispensado <- ifelse(sermil2022$dispensado == "com dispensa", "sim", "nao")

#Corrigir strings incorretas
sermil2022$escolaridade <- gsub("pós-graduaç╞o", "pós-graduação", sermil2022$escolaridade)
sermil2022$pais_nascimento <- gsub("jap╟o", "japão", sermil2022$pais_nascimento)
sermil2022$pais_nascimento <- gsub("bol╓via", "bolívia", sermil2022$pais_nascimento)
sermil2022$pais_nascimento <- gsub("it╡lia", "itália", sermil2022$pais_nascimento)
sermil2022$pais_nascimento <- gsub("l╓bano", "líbano", sermil2022$pais_nascimento)
sermil2022$pais_nascimento <- gsub("panam╡", "panamá", sermil2022$pais_nascimento)
sermil2022$escolaridade <- gsub("4° ano - ensino médio (profissionalizante)",
                                "4° ano - ensino médio", sermil2022$escolaridade)
sermil2022$escolaridade <- gsub("(profissionalizante)", "", sermil2022$escolaridade)
sermil2022$escolaridade <- gsub("[()]", "", sermil2022$escolaridade)

# Converter variáveis quantitativas para integer
sermil2022$ano_nascimento <- as.integer(sermil2022$ano_nascimento)
sermil2022$peso <- as.integer(sermil2022$peso)
sermil2022$altura <- as.integer(sermil2022$altura)
sermil2022$cabeca <- as.integer(sermil2022$cabeca)
sermil2022$cintura <- as.integer(sermil2022$cintura)
sermil2022$calcado <- as.integer(sermil2022$calcado)

# Converter variáveis qualitativas para factor
sermil2022$uf_nascimento <- factor(sermil2022$uf_nascimento)
sermil2022$pais_nascimento <- factor(sermil2022$pais_nascimento)
sermil2022$jsm <- factor(sermil2022$jsm)
sermil2022$zona_residencial <- factor(sermil2022$zona_residencial)
sermil2022$estado_civil <- factor(sermil2022$estado_civil)
sermil2022$escolaridade <- factor(sermil2022$escolaridade)
sermil2022$dispensado <- factor(sermil2022$dispensado)

#Criar variável IMC
sermil2022 <- sermil2022 %>%
  mutate(
    imc = peso / (altura / 100) ^ 2
  )

#Deixar apenas uma casa decimal na variável IMC
sermil2022$imc <- round(sermil2022$imc, 1)

#Reordenar variáveis
sermil2022 <- sermil2022[, c("dispensado", "ano_nascimento", "peso", "altura", "imc", "cabeca",
                             "cintura", "calcado", "pais_nascimento", "uf_nascimento",
                             "zona_residencial", "jsm", "estado_civil", "escolaridade")]

##############################################################################
#                          Verificação do Dataset                            #
##############################################################################

#Informações sobre o dataset
summary(sermil2022)

#Tabela de frequências absolutas da variável 'dispensado'
table(sermil2022$dispensado) 

#Informações sobre o dataset
glimpse(sermil2022)

#Informações sobre uma variável factor
levels(glimpse(sermil2022$uf_nascimento))
table(sermil2022$jsm)

##############################################################################
#                                    Dummy                                   #
##############################################################################

#Dummyficar variáveis qualitativas
sermil2022 <- dummy_columns(.data = sermil2022,
                            select_columns = "pais_nascimento",
                            remove_selected_columns = T,
                            remove_most_frequent_dummy = T)

sermil2022 <- dummy_columns(.data = sermil2022,
                            select_columns = "uf_nascimento",
                            remove_selected_columns = T,
                            remove_most_frequent_dummy = T)

sermil2022 <- dummy_columns(.data = sermil2022,
                            select_columns = "jsm",
                            remove_selected_columns = T,
                            remove_most_frequent_dummy = T)

sermil2022 <- dummy_columns(.data = sermil2022,
                            select_columns = "zona_residencial",
                            remove_selected_columns = T,
                            remove_most_frequent_dummy = T)

sermil2022 <- dummy_columns(.data = sermil2022,
                            select_columns = "estado_civil",
                            remove_selected_columns = T,
                            remove_most_frequent_dummy = T)

sermil2022 <- dummy_columns(.data = sermil2022,
                            select_columns = "escolaridade",
                            remove_selected_columns = T,
                            remove_most_frequent_dummy = T)

##############################################################################
#                              Modelo Militar                                #
##############################################################################

modelo_militar <- glm(formula = dispensado ~ . ,
                      data = sermil2022, 
                      family = "binomial")

options(max.print = 1500)
summary(modelo_militar)

#Extração do valor de Log-Likelihood (LL)
logLik(modelo_militar)

#Output do modelo
summ(modelo_militar, confint = T, digits = 3, ci.width = .95)

# Adicionando os valores previstos de probabilidade na base de dados
sermil2022$phat <- modelo_militar$fitted.values

#Mover phat para o começo
sermil2022 <- sermil2022 %>%
  select(phat, everything())

##############################################################################
#         Gráfico de Sensitividade e Especificidade VS Cutoffs               #
##############################################################################

#Coletando os fitted.values
predicoes <- prediction(predictions = modelo_militar$fitted.values, 
                        labels = as.factor(sermil2022$dispensado))

#Salvando no objeto dados_curva_roc
dados_curva_roc <- performance(predicoes, measure = "sens") 

#Extraindo sensitividade
sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 

#Extraindo especificidade
especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

#Extraindo os cutoffs:
cutoffs <- dados_curva_roc@x.values[[1]] 

#Dataframe para lot
dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

#Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

##############################################################################
#                           Matriz de Confusão                               #
##############################################################################

#Matriz de confusão
confusionMatrix(
  table(predict(modelo_militar, type = "response") >= 0.645,
        sermil2022$dispensado == "sim")[2:1, 2:1]
)

##############################################################################
#                                  Curva ROC                                 #
##############################################################################

#função roc do pacote pROC
ROC <- roc(response = sermil2022$dispensado, 
           predictor = modelo_militar$fitted.values)

#Plotagem da curva ROC
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )

#Extraindo valores da Curva ROC e índice de gini
ROC$auc

#Extraindo índice de gini
2 * ROC$auc - 1

##############################################################################
#                         Modelo Completo com Step-Wise                      #
##############################################################################
step_modelo_militar <- step(object = modelo_militar,
                            k = qchisq(p = 0.05, df = 1, lower.tail = FALSE),
                            steps = 1)  # Defina o número desejado de etapas

options(max.print = 1500)
summary(step_modelo_militar)

#Extração do valor de Log-Likelihood (LL)
logLik(step_modelo_militar)

#Output do modelo
summ(step_modelo_militar, confint = T, digits = 3, ci.width = .95)

# Adicionando os valores previstos de probabilidade na base de dados
sermil2022$phat <- step_modelo_militar$fitted.values

#Mover phat para o começo
sermil2022 <- sermil2022 %>%
  select(phat, everything())

##############################################################################
#         Gráfico de Sensitividade e Especificidade VS Cutoffs               #
##############################################################################

#Coletando os fitted.values
predicoes <- prediction(predictions = step_modelo_militar$fitted.values, 
                        labels = as.factor(sermil2022$dispensado))

#Salvando no objeto dados_curva_roc
dados_curva_roc <- performance(predicoes, measure = "sens") 

#Extraindo sensitividade
sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 

#Extraindo especificidade
especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

#Extraindo os cutoffs:
cutoffs <- dados_curva_roc@x.values[[1]] 

#Dataframe para lot
dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

#Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

##############################################################################
#                           Matriz de Confusão                               #
##############################################################################

#Matriz de confusão
confusionMatrix(
  table(predict(step_modelo_militar, type = "response") >= 0.645,
        sermil2022$dispensado == "sim")[2:1, 2:1]
)

##############################################################################
#                                  Curva ROC                                 #
##############################################################################

#função roc do pacote pROC
ROC <- roc(response = sermil2022$dispensado, 
           predictor = step_modelo_militar$fitted.values)

#Plotagem da curva ROC
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )

#Extraindo valores da Curva ROC e índice de gini
ROC$auc

#Extraindo índice de gini
2 * ROC$auc - 1

##############################################################################
#                            Comparação dos Modelos                          #
##############################################################################
#Comparando os modelos
lrtest(modelo_militar, step_modelo_militar)