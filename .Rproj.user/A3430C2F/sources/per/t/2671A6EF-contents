
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

#Loading the Dataset
macro <- read_csv(file.choose())
head(macro)


o <- ts(macro$`OilPrice`, start = c(2005,1), frequency = 12)
u <- ts(macro$`UnemployementRate`, start = c(2005,1), frequency = 12)
p <- ts(macro$`CPI`, start = c(2005,1), frequency = 12)
r <- ts(macro$`PolicyRate`, start = c(2005,1), frequency = 12)
e <- ts(macro$`ExchangeRate`, start = c(2005,1), frequency = 12)

##############################################################
## ROBUSTNESS CHECK 3 : IGREA YoY % change
#################################################################
a <- ts(macro$`IGREA`, start = c(2005,1), frequency = 12)




ts_plot(a, title = "IGREA", Xtitle = "Time", Ytitle = "IGREA")




amat <- diag(6)
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat[4,1] <- NA
amat[4,2] <- NA
amat[4,3] <- NA
amat[5,1] <- NA
amat[5,2] <- NA
amat[5,3] <- NA
amat[5,4] <- NA

amat[6,1] <- NA
amat[6,2] <- NA
amat[6,3] <- NA
amat[6,4] <- NA
amat[6,5] <- NA



b.mat <- diag(6)
diag(b.mat) <- NA

svR3 <- cbind(a,o,u,p,e,r)
colnames(svR3) <- cbind("IGREA","OilPrices", "UnemploymentRate", "InflationRate","ExchangeRate", "PolicyRate")
plot.ts(svR3)

#Max Lag - 12

lagselect <- VARselect(svR3, lag.max = 10, type = "both")
lagselect$selection
lagselect$criteria


Model1R3 <- VAR(svR3, p = 9, season = NULL, exog = NULL, type = "both")
SVARMod1R3 <- SVAR(Model1R3, Amat = amat, Bmat = b.mat, hessian = TRUE, max.iter=10000)



#Impulse Response Functions
#Elasticity of Unemployement
SVARup1R3 <- irf(SVARMod1R3, impulse = "InflationRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup2R3 <- irf(SVARMod1R3, impulse = "PolicyRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup3R3 <- irf(SVARMod1R3, impulse = "ExchangeRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup4R3 <- irf(SVARMod1R3, impulse = "OilPrices", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
plot(SVARup1R3)
plot(SVARup2R3)
plot(SVARup3R3)
plot(SVARup4R3)


#Elasticity of Policy Rate
SVARpr1R3 <- irf(SVARMod1R3, impulse = "UnemploymentRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr2R3 <- irf(SVARMod1R3, impulse = "InflationRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr3R3 <- irf(SVARMod1R3, impulse = "ExchangeRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr4R3 <- irf(SVARMod1R3, impulse = "OilPrices", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
plot(SVARpr1R3)
plot(SVARpr2R3)
plot(SVARpr3R3)
plot(SVARpr4R3)


#Elasticity of Exchange Rate
SVARer1R3 <- irf(SVARMod1R3, impulse = "UnemploymentRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer2R3 <- irf(SVARMod1R3, impulse = "InflationRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer3R3 <- irf(SVARMod1R3, impulse = "PolicyRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer4R3 <- irf(SVARMod1R3, impulse = "OilPrices", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
plot(SVARer1R3)
plot(SVARer2R3)
plot(SVARer3R3)
plot(SVARer4R3)




#Forecast Error Variance Decomposition
SVARfevdR3 <- fevd(SVARMod1R3, n.ahead = 12)
SVARfevdR3

