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



amat <- diag(5)
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


b.mat <- diag(5)
diag(b.mat) <- NA


#################################################################
#ROBUSTNESS CHECK 1 - CHANGING ORDER OF VARIABLES -> OUPER
#################################################################

sv_robust1 <- cbind(o,u,p,e,r)
colnames(sv_robust1) <- cbind("OilPrices", "UnemploymentRate", "InflationRate", "ExchangeRate","PolicyRate")
plot.ts(sv_robust1)

#Max Lag - 12

lagselect <- VARselect(sv_robust1, lag.max = 10, type = "both")
lagselect$selection
lagselect$criteria


Model1R <- VAR(sv_robust1, p = 9, season = NULL, exog = NULL, type = "both")
SVARMod1R <- SVAR(Model1R, Amat = amat, Bmat = b.mat, hessian = TRUE, max.iter=10000)




#Impulse Response Functions
#Elasticity of Unemployement
SVARup1R <- irf(SVARMod1R, impulse = "InflationRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup2R <- irf(SVARMod1R, impulse = "PolicyRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup3R <- irf(SVARMod1R, impulse = "ExchangeRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup4R <- irf(SVARMod1R, impulse = "OilPrices", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
plot(SVARup1R)
plot(SVARup2R)
plot(SVARup3R)
plot(SVARup4R)


#Elasticity of Policy Rate
SVARpr1R <- irf(SVARMod1R, impulse = "UnemploymentRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr2R <- irf(SVARMod1R, impulse = "InflationRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr3R <- irf(SVARMod1R, impulse = "ExchangeRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr4R <- irf(SVARMod1R, impulse = "OilPrices", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
plot(SVARpr1R)
plot(SVARpr2R)
plot(SVARpr3R)
plot(SVARpr4R)


#Elasticity of Exchange Rate
SVARer1R <- irf(SVARMod1R, impulse = "UnemploymentRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer2R <- irf(SVARMod1R, impulse = "InflationRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer3R <- irf(SVARMod1R, impulse = "PolicyRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer4R <- irf(SVARMod1R, impulse = "OilPrices", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
plot(SVARer1R)
plot(SVARer2R)
plot(SVARer3R)
plot(SVARer4R)




#Forecast Error Variance Decomposition
SVARfevdR <- fevd(SVARMod1R, n.ahead = 12)
SVARfevdR
