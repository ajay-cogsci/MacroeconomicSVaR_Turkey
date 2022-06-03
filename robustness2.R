
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
## ROBUSTNESS CHECK 2 : PRODUCTION INDUSTRY YoY % change
#################################################################
g <- ts(macro$`TotalProduction`, start = c(2005,1), frequency = 12)




ts_plot(g, title = "TotalIndustryProduction", Xtitle = "Time", Ytitle = "Total Production")




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

svR2 <- cbind(o,g,u,p,e,r)
colnames(svR2) <- cbind("OilPrices", "IndustryProduction", "UnemploymentRate", "InflationRate","ExchangeRate", "PolicyRate")
plot.ts(svR2)

#Max Lag - 12

lagselect <- VARselect(svR2, lag.max = 100, type = "both")
lagselect$selection
lagselect$criteria


Model1R2 <- VAR(svR2, p = 17, season = NULL, exog = NULL, type = "both")
SVARMod1R2 <- SVAR(Model1R2, Amat = amat, Bmat = b.mat, hessian = TRUE, max.iter=10000)



#Impulse Response Functions
#Elasticity of Unemployement
SVARup1R2 <- irf(SVARMod1R2, impulse = "InflationRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup2R2 <- irf(SVARMod1R2, impulse = "PolicyRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup3R2 <- irf(SVARMod1R2, impulse = "ExchangeRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup4R2 <- irf(SVARMod1R2, impulse = "OilPrices", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
plot(SVARup1R2)
plot(SVARup2R2)
plot(SVARup3R2)
plot(SVARup4R2)


#Elasticity of Policy Rate
SVARpr1R2 <- irf(SVARMod1R2, impulse = "UnemploymentRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr2R2 <- irf(SVARMod1R2, impulse = "InflationRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr3R2 <- irf(SVARMod1R2, impulse = "ExchangeRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr4R2 <- irf(SVARMod1R2, impulse = "OilPrices", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
plot(SVARpr1R2)
plot(SVARpr2R2)
plot(SVARpr3R2)
plot(SVARpr4R2)


#Elasticity of Exchange Rate
SVARer1R2 <- irf(SVARMod1R2, impulse = "UnemploymentRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer2R2 <- irf(SVARMod1R2, impulse = "InflationRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer3R2 <- irf(SVARMod1R2, impulse = "PolicyRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer4R2 <- irf(SVARMod1R2, impulse = "OilPrices", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
plot(SVARer1R2)
plot(SVARer2R2)
plot(SVARer3R2)
plot(SVARer4R2)




#Forecast Error Variance Decomposition
SVARfevdR2 <- fevd(SVARMod1R2, n.ahead = 12)
SVARfevdR2

