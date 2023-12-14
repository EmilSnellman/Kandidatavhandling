#Paket som användes i avhandlingen
library(readxl)
library(dplyr)
library(broom)
library(PeerPerformance)
library(lmtest)
library(sandwich)
library(tseries)
library(car)

#Regressioner
#Fama French 5-faktormodell
data <- read_excel(#Data som innehåller avkastningsdata och faktorkoefficienter)
colnames(data) <- c("DATE", "Mkt_RF", "SMB", "HML", "RMW", "CMA", "RET")
data$DATE <- as.Date(as.character(data$DATE), format = "%Y%m")
model <- lm( RET ~ Mkt_RF + SMB + HML + RMW + CMA, data = data)
summary(model)

#Carhart 4-faktormodell
data <- read_excel(#Data som innehåller avkastningsdata och faktorkoefficienter)
colnames(data) <- c("DATE", "Mkt_RF", "SMB", "HML", "WML", "RET")
data$DATE <- as.Date(as.character(data$DATE), format = "%Y%m")
model <- lm( RET ~ Mkt_RF + SMB + HML + WML, data = data)
summary(model)

#Fama French 3-faktormodell
data <- read_excel(#Data som innehåller avkastningsdata och faktorkoefficienter)
colnames(data) <- c("DATE", "Mkt_RF", "SMB", "HML", "RET")
data$DATE <- as.Date(as.character(data$DATE), format = "%Y%m")
model <- lm( RET ~ Mkt_RF + SMB + HML, data = data)
summary(model)

#Fama French 3-faktormodell
data <- read_excel(#Data som innehåller avkastningsdata)
colnames(data) <- c("DATE", "Mkt_RF", "RET")
data$DATE <- as.Date(as.character(data$DATE), format = "%Y%m")
model <- lm( RET ~ Mkt_RF, data = data)
summary(model)

#Regressioner med robusta standardfel
coeftest(model, vcov = vcovHC(model, type = 'HC0'))

#White's test:
#5 faktor
bptest(model, ~ Mkt_RF*SMB*HML*RMW*CMA + I(Mkt_RF^2) + I(SMB^2)+ I(HML^2)+ I(RMW^2)+ I(CMA^2) , data = data)
#4 faktor
bptest(model, ~ Mkt_RF*SMB*HML*WML + I(Mkt_RF^2) + I(SMB^2)+ I(HML^2)+ I(WML^2) , data = data)
#3 faktor
bptest(model, ~ Mkt_RF*SMB*HML*WML + I(Mkt_RF^2) + I(SMB^2)+ I(HML^2)+ , data = data)
#CAPM
bptest(model, ~ Mkt_RF + I(Mkt_RF^2) , data = data)

#Durbin-Watson test
dwtest(formula = model,  alternative = "two.sided")

#VIF
vif(model)
vif_values <- vif(model)
summary(vif_values)

#Sharpe-talet och signifikans test
data <- read_excel(#data som innehåller avkastningsdata över riskfra räntan för två prtföljer)
x <- data[[1]]
y <- data[[2]]
set.seed(1234)
ctr = list(type = 2)
out = sharpeTesting(x, y, control = ctr)
print(out)
  
#Jarque-Bera test:
data <- read_excel(#data som innehåller avkastningsdata)
returns <- data[[1]]
jarque.bera.test(returns)

  