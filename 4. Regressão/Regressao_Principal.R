
library(tidyverse)

library(readxl)

library(forecast)

library(urca)

library(vars)

library(mFilter)

library(seasonal)

library(seasonalview)


setwd("C:\\Users\\Caio Forcione\\Desktop\\Academic Projects\\01. Economics\\01. Master\\06. Time Series\\3. Trabalho Final\\2. Dados")

getwd()


cambio <- ts(scan("CAMBIO.csv"), start=c(2000,1), frequency=12)

ipca <- ts(scan("IPCA.csv"), start=c(2000,1), frequency=12)

juros <- ts(scan("djuros.csv"), start=c(2000,1), frequency=12)

PIB <- ts(scan("PIB.csv"), start=c(2000,1), frequency=12)


tsdisplay(cambio)
tsdisplay(ipca)
tsdisplay(juros)
tsdisplay(PIB)


######## CAMBIO ################

# RETIRAR OUTLIERS

dummy <- cambio*0
dummy[31:33] <- 1
cambio_du <- residuals(lm(cambio~dummy))
plot.ts(cambio_du)

length(cambio_du)

M.cambio <- lm(cambio_du ~ seq(1,242))
summary(M.cambio)

CE.cambio <- residuals(M.cambio)
tsdisplay(CE.cambio)

M1.cambio <- Arima(CE.cambio, order=c(13,0,0), seasonal=list(order=c(0,0,0),period=0), include.mean=FALSE)

coeftest(M1.cambio)
tsdisplay(residuals(M1.cambio))




########### IPCA ##############

# RETIRAR OUTLIERS

dummy2 <- ipca*0
dummy2[35:37] <- 1
ipca_du <- residuals(lm(ipca~dummy2))
plot.ts(ipca_du)

length(ipca)

M.ipca <- lm(ipca_du ~ seq(1,242))
summary(M.ipca)

CE.ipca <- residuals(M.ipca)
tsdisplay(CE.ipca)            # S?rie foi Estacionarizada?

M1.ipca <- Arima(CE.ipca, order=c(13,0,0), seasonal=list(order=c(0,0,0),period=0), include.mean=FALSE)

coeftest(M1.ipca)
tsdisplay(residuals(M1.ipca))

df.ipca <- ur.df(ipca, type="trend", lags=13)

plot(df.ipca)

summary(df.ipca)



########### JUROS ##############


length(juros)

M.juros <- lm(juros ~ seq(1,242))
summary(M.juros)

CE.juros <- residuals(M.juros)
tsdisplay(CE.juros)       

M1.juros <- Arima(CE.juros, order=c(13,0,0), seasonal=list(order=c(1,0,0),period=12), include.mean=FALSE)

coeftest(M1.juros)
tsdisplay(residuals(M1.juros))

df.ct <- ur.df(juros, type="trend", lags=13)

plot(df.ct)

summary(df.ct)

df.c <- ur.df(juros, type="drift", lags=13)

plot(df.c)

summary(df.c)

########### PIB ##############

# pib_hp <- hpfilter(PIB)

# write.csv(cpib_hp, "cpib_hp.csv", row.names = FALSE)

# cpib_hp <- pib_hp$cycle



plot(PIB)
length(PIB)

M.PIB <- lm(PIB ~ seq(1,242))
summary(M.PIB)


CE.PIB <- residuals(M.PIB)
tsdisplay(CE.PIB)            # S?rie foi Estacionarizada?

M1.PIB <- Arima(CE.PIB, order=c(17,0,0), seasonal=list(order=c(1,0,0),period=13), include.mean=FALSE)

coeftest(M1.PIB)
tsdisplay(residuals(M1.PIB))


######## VAR ##########

data <- data.frame(cbind((juros), (PIB), (ipca_du), (cambio_du)))

colnames(data) <- c("juros", "PIB", "ipca", "cambio")

plot.ts(data)





acf(data,36)

?VARselect

VARselect(data, lag.max=16, type = "none")


Var.Est <- VAR(data, p=1, season = NULL, exogen = NULL)
Var.Est <- VAR(data, p=4, season = NULL, exogen = NULL)

acf(residuals(Var.Est),36)


spib <- seas(PIB)
summary(spib)

sjuros <- seas(juros)
summary(sjuros)

names(spib)
spib$data
plot(spib$data[,1])
spib <- spib$data[,1]

sjuros <- sjuros$data[,1]

data <- data.frame(cbind((sjuros), (spib), (ipca_du), (cambio_du)))

colnames(data) <- c("juros", "PIB", "ipca", "cambio")

plot.ts(data)


VARselect(data, lag.max=12, type = "none")

Var.Est <- VAR(data, p=1, type = "none", season = NULL, exogen = NULL)

acf(residuals(Var.Est),36)

summary(Var.Est)


Amat <- diag(4)
Amat[2,1] <-NA
Amat[3,1] <-NA
Amat[3,2] <-NA 
Amat[4,1] <-NA 
Amat[4,2] <-NA 
Amat[4,3] <-NA 
Amat

AEst <- SVAR(Var.Est, Amat=Amat, Bmat=NULL, hessian=TRUE, estmethod ="direct")
AEst

######## IMPULSE JUROS ############

irf.1 <- irf(AEst, n.ahead=20,  impulse = "juros", response = c("PIB"))


irf.2<- irf(AEst, n.ahead=20, impulse = "juros", response = "ipca")     


irf.3 <- irf(AEst, n.ahead=20, impulse = "juros", response = "cambio")     


irf.4 <- irf(AEst, n.ahead=20, impulse = "juros", response = "juros")     


######## IMPULSE IPCA ############

irf.5 <- irf(AEst, n.ahead=20,  impulse = "ipca", response = c("PIB"))


irf.6<- irf(AEst, n.ahead=20, impulse = "ipca", response = "ipca")     


irf.7 <- irf(AEst, n.ahead=20, impulse = "ipca", response = "cambio")     


irf.8 <- irf(AEst, n.ahead=20, impulse = "ipca", response = "juros")     


######## IMPULSE CAMBIO ############

irf.9 <- irf(AEst, n.ahead=20,  impulse = "cambio", response = c("PIB"))


irf.10<- irf(AEst, n.ahead=20, impulse = "cambio", response = "ipca")     


irf.11 <- irf(AEst, n.ahead=20, impulse = "cambio", response = "cambio")     


irf.12 <- irf(AEst, n.ahead=20, impulse = "cambio", response = "juros")     


 ######## IMPULSE PIB ############

irf.13 <- irf(AEst, n.ahead=20,  impulse = "PIB", response = c("PIB"))


irf.14<- irf(AEst, n.ahead=20, impulse = "PIB", response = "ipca")     


irf.15 <- irf(AEst, n.ahead=20, impulse = "PIB", response = "cambio")     


irf.16 <- irf(AEst, n.ahead=20, impulse = "PIB", response = "juros")     


######### GRÁFICOS ##############

plot(irf.1)

plot(irf.2)

plot(irf.3)

plot(irf.4)

plot(irf.5)

plot(irf.6)

plot(irf.7)

plot(irf.8)

plot(irf.9)

plot(irf.10)

plot(irf.11)

plot(irf.12)

plot(irf.13)

plot(irf.14)

plot(irf.15)

plot(irf.16)


