#-----------------------------------------------------------------------------------------------------------------------------
# ANALISE DE BASE DE IMOVEIS P/ SAO PAULO
# Hermes LUIZ BOLINELLI JUNIOR
# Abril/2018
#------------------------------------------------------------------------------------------------------------------------------

# INICIO ----------------------------------------------------------------------------------------------------------------------

#
# Bibliotecas Utilizadas
#
library(arules)
library(class) 
library(caret)
library(datasets)
# Para os gráficos
library(ggmap)
library(ggplot2)


set.seed(1234)

# ETAPA 01 --------------------------------------------------------------------------------------------------------------------
# nesta etapa vamos obter o modelo, analisá-lo, fazer o traatamento dos dados e
# processar o modelo base: com todos as features e registros
#------------------------------------------------------------------------------------------------------------------------------


# Obtem dados e criação da base de treinamento "train"  e "test"
#
base<- read.csv("C:/BASE_IMOVEIS_SP_V7.csv", sep=";")
allBase <- base  # recebe toda a base para k-means

# Bases Treinamento e teste para Knn e Regressão Logistica
idx = sample(1:nrow(base),round(0.7*nrow(base)))  # já vou criar base treinamento e teste para os metodos Knn e egressao Logistica

train=base[idx,]   
head(train); tail(train)

test=base[-idx,]
head(test);tail(test)

nrow(train)
nrow(test)

# Estrutura/Analise
#
str(train)
summary(train)
cor(train)
#
# Features mais correlacionadas com a variavel dependente:
# AREA (80%)  BANHEIROS (67%)   QUARTOS (30%)   PISCINA (32%) SALAOFESTAS (20%) ...
#-----------------------------------------------------------------------------------------------------------------------------
# GRAFICOS
# library(car)
# scatterplotMatrix(train, spread = FALSE, smoother.args = list(lty = 2),
#                  main = "Scatter Plot Matrix")
# DEMORA MUIIITOOOO!!! Teve que ser comentado e utilizado apenas o cor(train)
#------------------------------------------------------------------------------------------------------------------------------


# TRATAMENTO: -----------------------------------------------------------------------------------------------------------------
# 
nrow(train)              # Há 13265 regsitros originais
nrow(test)               # Há 5685 regsitros originais 
train <- na.omit(train)  # Elimino registros com NA´s
test <- na.omit(test)
allBase <- na.omit((allBase))
nrow(train)              # Foram eliminados 61 registros (0,54599% do total) devido a NA´s, resultando em 13204 registros
nrow(test)               # Foram eliminados  44 regsitros (0,2990% do total) devido a NA´s, resultados em  5668 regsitros

# apenas para manter o original em "train" e "test"
#
t <- train               
tst <- test


# Normalização ----------------------------------------------------------------------------------------------------------------
#
meanTrainFeatures = colMeans(t[,1:13])            # Média de cada feature 
stdTrainFeatures  = apply(t[,1:13], 2, sd)        # DP de cada feature
(meanTrainFeatures); (stdTrainFeatures)

t[,1:13] = sweep(t[,1:13], 2, meanTrainFeatures,"-") 
t[,1:13] = sweep(t[,1:13], 2, stdTrainFeatures,"/")

test[,1:13]  = sweep(test[,1:13],  2, meanTrainFeatures,"-") 
test[,1:13]  = sweep(test[,1:13], 2, stdTrainFeatures,"/")

head(train); head(test)


# Normalizacao: Toda a Base
meanAllBase = colMeans(allBase[,1:13])            # Média de cada feature da Base toda (allBase)
stdallBase  = apply(allBase[,1:13], 2, sd)        # DP de cada feature da base toda allBase
meanAllBase;stdallBase

allBaseN = sweep(allBase[,1:13], 2, meanAllBase,"-") 
allBaseN = sweep(allBaseN[,1:13], 2, stdallBase,"/")

allBaseN <- cbind(allBaseN, Valor = allBase$VALOR)
head(allBaseN)
str(allBaseN)

# allBase: toda base lida  e  allBaseN toda base lida normalizada
# train e tst : base treinamento e teste.  Bases t e test: bases treinamento e teste normalizadas
#------------------------------------------------------------------------------------------------------------------------------


#######################################################################################################################
############## GRAFICO SAO PAULO ##########
# Vamos gerar gráfico com os pontos da Base geral. Vamos separar utilizar a coluna Padrão
# e da tabela com a quantidade de padrões de nossa base.
#
base <- allBase[,1:2]
head(base)
base <- cbind(base, Grupo = allBase$PADRAO)
head(base)
df_imoveis <- as.data.frame(base)

imoveis_pd_1 <- subset(df_imoveis,Grupo == 1)
imoveis_pd_2 <- subset(df_imoveis,Grupo == 2)
imoveis_pd_3 <- subset(df_imoveis,Grupo == 3)

new_df_pd1 <- imoveis_pd_1[c("LONGITUDE", "LATITUDE")]
names(new_df_pd1) <- c("lon", "lat")

new_df_pd2 <- imoveis_pd_2[c("LONGITUDE", "LATITUDE")]
names(new_df_pd2) <- c("lon", "lat")

new_df_pd3 <- imoveis_pd_3[c("LONGITUDE", "LATITUDE")]
names(new_df_pd3) <- c("lon", "lat")

map_sampa <- get_googlemap(center = c(lon = -46.6388, lat =  -23.5489), zoom = 11, size = c(640, 640), scale = 2,maptype ="terrain", color = "color")

ggmap(map_sampa) + geom_point(data=new_df_pd1, aes(x=lon, y=lat),  size=2,  col="black", shape=19, alpha=0.4 ) + geom_point(data=new_df_pd2, aes(x=lon, y=lat),  size=3,  col="red", shape=15 , alpha=0.4 ) + geom_point(data=new_df_pd3, aes(x=lon, y=lat),  size=3,  col="blue", shape=17, alpha=0.4)

table(allBase$PADRAO)
#     1     2     3 
#  3860 14808   204

# Observamos tanto pelo gráfico gerado como pela tabela que há os dados estão 
# desbalanceados, o que não é adequado. Há muito mais itens do Padrão 2 (Médio) do que o Padrão Baixo (1) e 
# do Padrão Alto (204)

#### FIM GRAFICO SAO PAULO ################
#######################################################################################################################

# Vamos gerar alguns gráficos para noosa análise:

hist(allBase$PADRAO)    # Observa-se o já discutido anteriormente: Maioria padrão 2.
hist(allBase$AREA, breaks = 3)
densityplot(allBase$VALOR)
with(allBase, plot(AREA, VALOR))
ggplot(allBase) + geom_histogram(aes(VALOR), bins = 100)

# É posivel observar que os imóveis majoritariamente estão entre 70..80 metros quadrados e valores entorno de
# 300.000 reais. Ha um desbalanceamento, os valores mais altos são bem poucos e dispersos a direita do gafico.
# As metragens acima de 200..250 m2 são em menor quantidade e vão se dispersando a direita.
#######################################################################################################################


# MODELO 01 BASE ------------------------------------------------------------------------------------------------------
# 01 : todas as features
# Vamos usar a base de treino e fazer uma Regressoa. Vamos aplicar na base test
# e ter o Valor Medio Modelo Base.
# Vamos usar este valor para futuras comparações.
#

ylm <- lm(formula =VALOR ~ LONGITUDE + LATITUDE + PADRAO + QUARTOS + BANHEIROS + 
         CHURRASQUEIRA + PISCINA + PLAYGROUND + PORTARIA + SALAO_FESTAS + QUADRA_POLIESPORTIVA + VAGAS_GARAGEM  
         + AREA , data = t)

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$VALOR) / length(Preditor))
MAE
# MAE  = 88482.06  # MODELO BASE

# Valor Medio
VMMB <- mean(Preditor)
VMMB                   # VMMB = R$ 368.175.6

coefficients(ylm)     
summary(ylm)         
#
# todas variaveis signifivas a 0,1%, exceção apenas para
# vagas de garagem (10%) e latitude longiutde.
# POr opção, mantivemos todas.
#######################################################################################################################



#######################################################################################################################
# ETAPA 02 - Knn
# Aqui vamos utilizar o Knn para comparar a classficação deste metodo com a coluna Padrao da base.
# Há uma discussão no texto do relatório sobre isso.
# Vaos criar a nova base

#summary(t_cl)
#idx = sample(1:nrow(t_cl),round(0.7*nrow(t_cl)))

trainKnn = t                              #t_cl[idx,]
head(trainKnn);tail(trainKnn);nrow(train)

testknn = test #t_cl[-idx,]
head(testknn);tail(testknn);nrow(testknn)


# Vamos classificar a base com o metodo knn
#
fit <- knn(trainKnn, testknn, trainKnn$PADRAO, k = 3)
head(fit);length(fit)
result = table(knn = fit, kmeans = testknn$PADRAO)
result


# vamos verificar a acurácia da nova classificação
# ou seja, comparar o agrupamento feito com kmeans com o feito aqui com o knn
#
acertos = sum(diag(result))/sum(result)
cat("percentual de acerto: ", acertos)  # 94,63656 %


# FIM ETAPA 02: KNN
###############################################################################################################################


###############################################################################################################################
# ETAPA 03: REGRESSÃO LOGISTICA
# Como a Regressao Logsitica dá resultados entre 0-1
# Vamos utilizar a estrategia "One x All"
# Cada Padrao a ser analisado será 1 e os demais 0


# Grupo A
head(train)
head(t)
tg1 <- ifelse(train$PADRAO == 1, 1,0)
tg1 <- cbind (t,tg1)
tg1 <- tg1[,-14]
head(tg1)

tstg1 <- ifelse(tst$PADRAO == 1, 1,0)
tstg1 <- cbind (test,tstg1)
tstg1 <- tstg1[,-14]
head(tstg1)


fitl = glm(tg1 ~.,data=tg1, family=binomial("logit"))
fitl

predict_test = predict(fitl, newdata=tstg1, type="response")>0.5
predict_test

c_matrix=table(tstg1$tstg1,predict_test)
c_matrix

cat('Accuracy: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %')
# aCURACIA GRUPO A: 100  %

#-----------------------------------------------------------------
# Grupo B
# 
tg2 <- ifelse(train$PADRAO == 2, 1,0)
tg2 <- cbind (t,tg2)
tg2 <- tg2[,-14]
tg2

tstg2 <- ifelse(tst$PADRAO == 2, 1,0)
tstg2 <- cbind (test,tstg2)
tstg2 <- tstg2[,-14]
tstg2


fitl2 = glm(tg2 ~.,data=tg2, family=binomial("logit"))
fitl2

predict_test2 = predict(fitl2, newdata=tstg2, type="response")>0.5
predict_test2

c_matrix=table(tstg2$tstg2,predict_test2)
c_matrix

cat('Accuracy: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %')
# ACURACIA = 98.74735 %

#-------------------------------------------------------------------------
# GRUPO C
# 
tg3 <- ifelse(train$PADRAO == 3, 1,0)
tg3 <- cbind (t,tg3)
tg3 <- tg3[,-14]
tg3

tstg3 <- ifelse(tst$PADRAO == 3, 1,0)
tstg3 <- cbind (test,tstg3)
tstg3 <- tstg3[,-14]
tstg3


fitl3 = glm(tg3 ~.,data=tg3, family=binomial("logit"))
fitl3

predict_test = predict(fitl3, newdata=tstg3, type="response")>0.5
predict_test

c_matrix=table(tstg3$tstg3,predict_test)
c_matrix

cat('Accuracy: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %')
# ACURACIA = 100 %

# FIM ETAPA 03: REGRESSAO LOGISTICA
#######################################################################################################################



#######################################################################################################################
# ETAPA 02 --------------------------------------------------------------------------------------------------------------------
# KMEANS
# vou separar o MODELO BASE (ETAPA01) em 3 agrupamentos utilizando k-means
# a ideia aqui é um ver os resultado para um metodo não supervisonado, o que no caso aqui presente seria comum.
# Depois vamos comparar cada um dos erros medios (MAE) de cada grupo do k-means com o Modelo Base (VMMB e MAE)
# O esperado seria o modelo de cada grupo ter erro menor do que o do modelo base.
# ----------------------------------------------------------------------------------------------------------------------------

set.seed(2234)
# KMEANS
#
head(t)
cl <- kmeans(t[,1:13], 3)   # K-MEANS: dividindo em 3 grupos
cl
# 
cl$cluster
table(cl$cluster)
#     1    2    3 
#  5991 3544 3669


# Preparar uma base com outro nome, para preservar os dados originais
#
t_cl <- t
t_cl <- cbind(t_cl, Grupo = cl$cluster)
head(t_cl);head(train)
str(t_cl);str(train)

# Compara o agrupamento feito com o padrão #########
res2PG<- table((t_cl$Grupo),(train$PADRAO))
res2PG


# Vamos comparar o agrupamento feito aqui com kmeans com a feature PADRAO
#
acertosres2PG = sum(diag(res2PG))/sum(res2PG)
cat("percentual de acerto: ", acertosres2PG) 
# 34.07301 % Percetnual baixo.

###### Fim compara agrupamemto com o PADRÃO ########


# GRUPO A ---------------------------------------------------------------------------------------------------------------------
#
A <- t_cl[t_cl$Grupo == 1,]
head(A)
summary(A)
str(A)

# MODELO GRUPO A 
#
ylmA <- lm(formula =VALOR ~ LONGITUDE + LATITUDE + PADRAO + QUARTOS + BANHEIROS + 
             CHURRASQUEIRA + PISCINA + PLAYGROUND + PORTARIA + SALAO_FESTAS + QUADRA_POLIESPORTIVA + VAGAS_GARAGEM  
           + AREA , data = A)
ylmA


# PREDITOR
Preditor <- predict (ylmA, test )

# TABELA
table(head(Preditor,10),head(test$VALOR,10))

# ERRO MÉDIO ABSOLUTO (MAE)
MAE <- sum(abs(Preditor - test$VALOR) / length(Preditor))
MAE                   # MAE  = 87998.49     # MAE  = 88482.06 (MODELO BASE) 

# Valor Medio
VMA <- mean(Preditor)
VMA                   # VMA = R$ 354.328.80     # VMMB = R$ 368.175.6 
#------------------------------------------------------------------------------------------------------------------------------


# GRUPO B ---------------------------------------------------------------------------------------------------------------------
#
B <- t_cl[t_cl$Grupo == 2,]
head(B)
summary(B)
str(B)

# MODELO GRUPO B 
#
ylmB <- lm(formula =VALOR ~ LONGITUDE + LATITUDE + PADRAO + QUARTOS + BANHEIROS + 
             CHURRASQUEIRA + PISCINA + PLAYGROUND + PORTARIA + SALAO_FESTAS + QUADRA_POLIESPORTIVA + VAGAS_GARAGEM  
           + AREA , data = B)
ylmB


# PREDITOR
Preditor <- predict (ylmB, test)

# TABELA
table(head(Preditor,10),head(test$VALOR,10))

# ERRO MÉDIO ABSOLUTO (MAE)
MAE <- sum(abs(Preditor - test$VALOR) / length(Preditor))
MAE                          # MAE  = 97441.43      # MAE  = 88482.06 (MODELO BASE)

# Valor Medio
VMB <- mean(Preditor)
VMB                          # VMB = R$ 405.611.20     # VMMB = R$ 368.175.6
#------------------------------------------------------------------------------------------------------------------------------


# GRUPO C ---------------------------------------------------------------------------------------------------------------------
#
C <- t_cl[t_cl$Grupo == 3,]
head(C)
summary(C)

# MODELO GRUPO C 
#
ylmC <- lm(formula =VALOR ~ LONGITUDE + LATITUDE + PADRAO + QUARTOS + BANHEIROS + 
             CHURRASQUEIRA + PISCINA + PLAYGROUND + PORTARIA + SALAO_FESTAS + QUADRA_POLIESPORTIVA + VAGAS_GARAGEM  
           + AREA , data = C)
ylmC


# PREDITOR
Preditor <- predict (ylmC, test )

# TABELA
table(head(Preditor,10),head(test$VALOR,10))


# ERRO MÉDIO ABSOLUTO (MAE)
MAE <- sum(abs(Preditor - test$VALOR) / length(Preditor))
MAE                      # MAE  = 168419.8        # MAE  = 88482.06 (MODELO BASE)

# Valor Medio
VMC <- mean(Preditor)
VMC                      # VMC = R$ 274.088.80     # VMMB = R$ 368.175.60

#------------------------------------------------------------------------------------------------------------------------------
# FIM ETAPA 02:  k-means e lm
#==============================================================================================================================

#RESUMO02 #####################################
# MODELO    VALOR R$        MAE
# BASE      368.175.60      88.482.06 
# A         405.611.20      97.441.43
# B         274.088.80     168.419.80
# C         354.328.80      87.998.49
############################################



############## GRAFICO SAO PAULO ###############################
# Grafico da distribuição em SP com os grupos obtidos no k-means

base <- train[,1:2]
head(base)
base <- cbind(base, Grupo = t_cl$Grupo)
head(base)
df_imoveis <- as.data.frame(base)

imoveis_pd_1 <- subset(df_imoveis,Grupo == 1)
imoveis_pd_2 <- subset(df_imoveis,Grupo == 2)
imoveis_pd_3 <- subset(df_imoveis,Grupo == 3)

new_df_pd1 <- imoveis_pd_1[c("LONGITUDE", "LATITUDE")]
names(new_df_pd1) <- c("lon", "lat")

new_df_pd2 <- imoveis_pd_2[c("LONGITUDE", "LATITUDE")]
names(new_df_pd2) <- c("lon", "lat")

new_df_pd3 <- imoveis_pd_3[c("LONGITUDE", "LATITUDE")]
names(new_df_pd3) <- c("lon", "lat")

map_sampa <- get_googlemap(center = c(lon = -46.6388, lat =  -23.5489), zoom = 11, size = c(640, 640), scale = 2,maptype ="terrain", color = "color")

ggmap(map_sampa) + geom_point(data=new_df_pd1, aes(x=lon, y=lat),  size=2,  col="black", shape=19, alpha=0.4 ) + geom_point(data=new_df_pd2, aes(x=lon, y=lat),  size=3,  col="red", shape=15 , alpha=0.4 ) + geom_point(data=new_df_pd3, aes(x=lon, y=lat),  size=3,  col="blue", shape=17, alpha=0.4)

#### FIM GRAFICO SAO PAULO ##################################


# FIM TRABALHO DISCIPLINA
#################################################################################################################################
