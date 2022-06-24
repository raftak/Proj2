#ETAPA 1 EDA
library(dplyr)
library(ggplot2)

#Dados de 2014-2018 (data), 2009-2013 (data2)
data <- read.csv("2014_2018.csv", sep=",")
data2 <- read.csv("2009_2013.csv", sep=",") 

#Inspecionando os dados
length(data)
nrow(data)
str(data)

#Determinando qtd de NA's e eliminando os
sum(is.na(data$Chegadas))
sum(is.na(data2$Chegadas))

cleandata <- na.omit(data)
cleandata2 <- na.omit(data2)

#Medidas de tendencia central e dispersao
mean(cleandata$Chegadas)
median(cleandata$Chegadas)
var(cleandata$Chegadas)
sd(fulldata$Chegadas)
summary(cleandata)

#analise de frequencias e outliers
plot(density(fulldata$Chegadas))
hist(fulldata$Chegadas, xlim=c(0,30), breaks = 200000)
boxplot(Chegadas~Ano, data=fulldata)

ggplot(cleandata) + 
  geom_boxplot(aes(x = as.factor(Ano), y = Chegadas)) +
  ylim(0,30)

#Full data 2009 - 2018
fulldata <- merge(x=cleandata,y=cleandata2, all=TRUE)

#Tabelas sumarizadas
sumtable1 = fulldata %>% filter(Ano=="2014") %>%group_by(Pa??s)  %>% summarize(Chegadas = sum(Chegadas)) %>% arrange(desc(Chegadas))
write.table(sumtable1, file="Pais.csv")

sumtable2 = fulldata %>% group_by(UF) %>% summarize(Chegadas = sum(Chegadas)) %>% arrange(desc(Chegadas))
write.table(sumtable2, file="UF.csv")

sumtable3 = fulldata %>% group_by(Continente) %>% summarize(Chegadas = sum(Chegadas)) %>% arrange(desc(Chegadas))
write.table(sumtable3, file="Continente.csv")

sumtable4 = fulldata %>% group_by(Via.de.acesso) %>% summarize(Chegadas = sum(Chegadas)) %>% arrange(desc(Chegadas))
write.table(sumtable4, file="Via.csv")

sumtable5 = fulldata %>% group_by(Pa???s, Ano) %>% summarize(Chegadas = sum(Chegadas))

sumtable6 = cleandata %>% group_by(Ano, Ordem.m????s) %>% summarize(Chegadas = sum(Chegadas))
sumtable7 = cleandata2 %>% group_by(Ano, Ordem.m????s) %>% summarize(Chegadas = sum(Chegadas))

sumtable8 = cleandata2 %>% group_by(Ano) %>% summarize(Chegadas = sum(Chegadas))
sumtable9 = cleandata %>% group_by(Ano) %>% summarize(Chegadas = sum(Chegadas))

sumtable10 <- merge(x=sumtable6,y=sumtable7, all=TRUE)

sumtable11 <- fulldata %>% group_by(Ano, Ordem.m?s) %>% filter(Pa?s=="Argentina") %>% summarize(Chegadas = sum(Chegadas)) 
sumtable12 <- fulldata %>% group_by(Ano, Ordem.m?s) %>% filter(Pa?s=="Estados Unidos") %>% summarize(Chegadas = sum(Chegadas)) 

sumtable13 <- cleandata %>% group_by(Ano, Ordem.m?s, UF,Via.de.acesso ,Pa?s) %>% filter(Ano=="2014") %>% summarize(Chegadas = sum(Chegadas))

#Grafico em ggplot

ggplot(cleandata, aes(fill=Continente, y=Chegadas, x=UF)) + 
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(breaks = seq(from = 0, to = 12e+06, by = 1e+06))  

ggplot(sumtable1, aes(y=Chegadas, x=Pa??s, fill=Pa??s)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(from = 0, to = 10e+06, by = 1e+06)) +
  ylim (0e+00,12e+06)

ggplot(sumtable2, aes(y=Chegadas, x=UF, fill=UF)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(from = 0, to = 10e+06, by = 1e+06)) +
  ylim (0e+00,12e+06)

ggplot(sumtable3, aes(y=Chegadas, x=Continente, fill=Continente)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(from = 0, to = 19e+06, by = 1e+06)) +
  ylim (0e+00,19e+06)

ggplot(sumtable4, aes(y=Chegadas, x=Via.de.acesso, fill=Via.de.acesso )) + 
  geom_bar(position="dodge",stat="identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(from = 0, to = 22e+06, by = 1e+06)) +
  ylim (0e+00,22e+06)

#Etapa 2 predicao TS Chegadas
library(forecast)
library(fpp2)
library(DMwR)
library(readxl)

#carregando os dados
write.csv(sumtable10, "sumtable10.csv")
sumtable10 <- read.csv("sumtable10.csv", sep=",", colClasses=c("NULL", NA, NA, NA))
sumtable10 <- read.csv("sumtable10.csv", sep=",")

visarrival <- read_excel("visarrival.xlsx", sheet = "Sheet1")

#Passando de df para ts

ts_data = ts(data= visarrival$Visitors, start = c(2009,1), frequency = 12)
visarrivalteste <- visarrival[97:120,]
ts_data_treino = ts(data= visarrival$Visitors, start = c(2009,1), end = c(2016,12), frequency = 12)
ts_data_teste = ts(data= visarrivalteste$Visitors, start = c(2017,1), end = c(2018,12), frequency = 12)


#Plotagens da ts
autoplot(ts_data)

ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Chegadas") +
  ggtitle("Seasonal plot: Turistas")

#funcao decompose() , classical decomposition


ts_data %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Decomposition_ts_chegadas")

#Autocorrelacao
ggAcf(ts_data, lag.max=96)

#Treino e Teste do metodo de predicao
ts_data
ts_data_treino
ts_data_teste

fit1 <- hw(ts_data_treino,seasonal="additive", h=24)
fit2 <- hw(ts_data_treino,seasonal="multiplicative", h=24)

auto.arima(ts_data, stepwise=FALSE, approximation=FALSE)
fit3 <- forecast(arima(ts_data_treino,order = c(2,0,1), seasonal = c(0,1,1)), h=24)

accuracy(fit1, ts_data_teste)
accuracy(fit2, ts_data_teste)
accuracy(fit3, ts_data_teste)

checkresiduals(fit21)
checkresiduals(fit3)
checkresiduals(fit2)

#Visualizando os resultados dos Testes
autoplot(ts_data_teste) +
  
  autolayer(fit1, series="HW forecasts additive", PI=FALSE) +
  autolayer(fit2, series="HW forecasts multiplicative", PI=FALSE) +
  autolayer(fit3, series="ARIMA", PI=FALSE) +
  xlab("Ano") +
  ylab("Chegadas (*1000)") +
  ggtitle("Conjunto Teste") +
  guides(colour=guide_legend(title="Legenda"))

#previsao: metodo HW mult. na base (em ts)

fit21 <- hw(ts_data,seasonal="multiplicative", h=60)

autoplot(ts_data) +
  
  autolayer(fit21, series="HW multiplicative",
            PI=FALSE) +
  xlab("Ano") +
  scale_x_continuous(breaks=c(2009:2023)) +
  ylab("Chegadas") +
  ggtitle("Previsao") +
  guides(colour=guide_legend(title="Legenda"))

autoplot(fit21) +
  ylab("Chegadas (*1000)") +
  scale_x_continuous(breaks=c(2009:2023))

#Contando os numeros finais
fit21
finalnum<-as.data.frame(fit21$mean)
finalnum<-as.data.frame(fit21)

library(reshape2)
library(timetk)

finalnum <-tk_tbl(fit21)
str(finalnum)
write.csv(finalnum, file ="forecast.csv")

#ETAPA 3 predicao receitas
#EDA
visarrival<-read_xlsx("visarrival.xlsx")

plot(density(visarrival$Revenue))
hist(visarrival$Revenue)
boxplot(Visitors~ano, data=visarrival)
boxplot(visarrival$Revenue)

summary(visarrival)
sd(visarrival$Visitors)

#Passando de df para ts
ts_data = ts(data= visarrival$Revenue, start = c(2009,1), frequency = 12)

#Conjntos de Treino e teste
visarrivalteste <- visarrival[97:120,]
ts_data_treino = ts(data= visarrival$Revenue, start = c(2009,1), end = c(2016,12), frequency = 12)
ts_data_teste = ts(data= visarrivalteste$Revenue, start = c(2017,1), end = c(2018,12), frequency = 12)

#Plotagens da ts
autoplot(ts_data)

ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Revenue") +
  ggtitle("Seasonal plot: revenue")

#funcao decompose() , classical decomposition
ts_data %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Decomposition_revenue")

#Autocorrelacao
ggAcf(ts_data, lag.max=96)

#Treino e Teste do metodo de predicao

fit1 <- hw(ts_data_treino,seasonal="additive", h=24)
fit2 <- hw(ts_data_treino,seasonal="multiplicative", h=24)

auto.arima(ts_data)
fit3 <- forecast(arima(ts_data_treino,order = c(1,0,0), seasonal = c(1,0,0)), h=24)

accuracy(fit1, ts_data_teste)
accuracy(fit2, ts_data_teste)
accuracy(fit3, ts_data_teste)

#Visualizando os resultados dos Testes
autoplot(ts_data_teste) +
  
  autolayer(fit1, series="HW forecasts additive", PI=FALSE) +
  autolayer(fit2, series="HW forecasts multiplicative", PI=FALSE) +
  autolayer(fit3, series="ARIMA", PI=FALSE) +
  xlab("Ano") +
  ylab("Revenue") +
  ggtitle("Previsao") +
  guides(colour=guide_legend(title="Legenda"))

#Predicao com Hw additive
fit21 <- hw(ts_data,seasonal="additive", h=60)

autoplot(ts_data) +
  autolayer(fit21, series="HW additive",
            PI=FALSE) +
  xlab("Ano") +
  scale_x_continuous(breaks=c(2009:2023)) +
  ylab("Receitas") +
  guides(colour=guide_legend(title="Legenda"))

autoplot(fit21) +
  ylab("Chegadas (*1000)") +
  scale_x_continuous(breaks=c(2009:2023))

#Contando os numeros finais
fit21$mean
finalnum<-as.data.frame(fit21$mean)
finalnum<-as.data.frame(fit21)

library(reshape2)
library(timetk)

finalnum <-tk_tbl(fit21)
str(finalnum)
write.csv(finalnum, file ="forecast_revenue.csv")

#ETAPA 4: REGRESSAO LINEAR
#EDA
scatter.smooth(x=visarrival$Revenue, y=visarrival$Visitors, main="Visitors x Revenue")  # scatterplot
plot(density(visarrival$Revenue)) #density plot
plot(density(visarrival$Visitors))

# divide graph area in 2 columns
par(mfrow=c(1, 2)) 
boxplot(visarrival$Revenue, main="Revenue", sub=paste("Outlier rows: ", boxplot.stats(visarrival$Revenue)$out))  # box plot for 'speed'
boxplot(visarrival$Visitors, main="Visitors", sub=paste("Outlier rows: ", boxplot.stats(visarrival$Visitors)$out))  # box plot for 'distance'

cor(visarrival$Visitors, visarrival$Revenue)  # calculate correlation between x and y

# Create Training and Test data -
set.seed(100)  
trainingRowIndex <- sample(1:nrow(visarrival), 0.8*nrow(visarrival))  # row indices for training data
trainingData <- visarrival[trainingRowIndex, ]  # model training data
testData  <- visarrival[-trainingRowIndex, ]   # test data

# Build the model on training data
lmMod <- lm(Visitors ~ Revenue, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

summary (lmMod)  # model summary
plot(lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$Visitors, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

#Model evaluation accuracy
regr.eval(actuals_preds$actuals, actuals_preds$predicteds)

shapiro.test(resid(lmMod))
plot(density(resid(lmMod)), main="Histograma dos res??duos", #verify normal distr
     xlab="Res??duos")

plot(lmMod, which=c(2,2)) #grafico Normal QQ
plot(rstudent(lmMod) ~ fitted(lmMod), pch = 19)
abline(h = 0, lty = 2)
# build linear regression model on full data
linearMod <- lm(Visitors ~ Revenue, data=visarrival)  
print(linearMod)
plot(linearMod)

summary(linearMod)

#plotting

plot(visarrival$Visitors~visarrival$Revenue)
abline(linearMod, lty=1)
