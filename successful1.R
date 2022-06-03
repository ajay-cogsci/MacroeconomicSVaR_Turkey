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

o <- ts(macro$OilPrice, start = c(2005,1), frequency = 12)
u <- ts(macro$`UnemployementRate`, start = c(2005,1), frequency = 12)
p <- ts(macro$`CPI`, start = c(2005,1), frequency = 12)
r <- ts(macro$`PolicyRate`, start = c(2005,1), frequency = 12)
e <- ts(macro$`ExchangeRate`, start = c(2005,1), frequency = 12)

ts_plot(o, title = "Oil Price", Xtitle = "Time", Ytitle = "Oil Price")
ts_plot(u, title = "Unemp Rate", Xtitle = "Time", Ytitle = "Unemp Rate")
ts_plot(p, title = "Inflation", Xtitle = "Time", Ytitle = "Inflation")
ts_plot(r, title = "Policy Rate", Xtitle = "Time", Ytitle = "Policy Rate")
ts_plot(e, title = "Exchange Rate", Xtitle = "Time", Ytitle = "Exchange Rate")



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



sv <- cbind(o,u,p,r,e)
colnames(sv) <- cbind("OilPrices", "UnemploymentRate", "InflationRate", "PolicyRate","ExchangeRate")
plot.ts(sv)

#Max Lag - 12

lagselect <- VARselect(sv, lag.max = 10, type = "both")
lagselect$selection
lagselect$criteria


Model1 <- VAR(sv, p = 9, season = NULL, exog = NULL, type = "both")
SVARMod1 <- SVAR(Model1, Amat = amat, Bmat = b.mat, hessian = TRUE, max.iter=10000)



#Elasticity of Inflation with Respect to Alternative Variables


#Impulse Response Functions
#Elasticity of Unemployement
SVARup1 <- irf(SVARMod1, impulse = "InflationRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup2 <- irf(SVARMod1, impulse = "PolicyRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup3 <- irf(SVARMod1, impulse = "ExchangeRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)
SVARup4 <- irf(SVARMod1, impulse = "OilPrices", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)

SVARup5 <- irf(SVARMod1, impulse = "UnemploymentRate", response = "UnemploymentRate", cumulative = TRUE, ci = 0.68)

passthrough = SVARup1$irf$InflationRate/SVARup5$irf$UnemploymentRate


plot(SVARup1)
plot(SVARup2)
plot(SVARup3)
plot(SVARup4)


#Elasticity of Policy Rate
SVARpr1 <- irf(SVARMod1, impulse = "UnemploymentRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr2 <- irf(SVARMod1, impulse = "InflationRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr3 <- irf(SVARMod1, impulse = "ExchangeRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)
SVARpr4 <- irf(SVARMod1, impulse = "OilPrices", response = "PolicyRate", cumulative = TRUE, ci = 0.68)

SVARpr5 <- irf(SVARMod1, impulse = "PolicyRate", response = "PolicyRate", cumulative = TRUE, ci = 0.68)

passthrough_pr = SVARpr2$irf$InflationRate/SVARpr5$irf$PolicyRate


plot(SVARpr1)
plot(SVARpr2)
plot(SVARpr3)
plot(SVARpr4)


#Elasticity of Exchange Rate
SVARer1 <- irf(SVARMod1, impulse = "UnemploymentRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer2 <- irf(SVARMod1, impulse = "InflationRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer3 <- irf(SVARMod1, impulse = "PolicyRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
SVARer4 <- irf(SVARMod1, impulse = "OilPrices", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)

SVARer5 <- irf(SVARMod1, impulse = "ExchangeRate", response = "ExchangeRate", cumulative = TRUE, ci = 0.68)
passthrough_er = SVARer2$irf$InflationRate/SVARer5$irf$ExchangeRate


plot(SVARer1)
plot(SVARer2)
plot(SVARer3)
plot(SVARer4)

#Elasticity of Oil Prices
SVARop1 <- irf(SVARMod1, impulse = "InflationRate", response = "OilPrices", cumulative = TRUE, ci = 0.68)
SVARop5 <- irf(SVARMod1, impulse = "OilPrices", response = "OilPrices", cumulative = TRUE, ci = 0.68)
passthrough_op = SVARop1$irf$InflationRate/SVARop5$irf$OilPrices




#Forecast Error Variance Decomposition
SVARfevd <- fevd(SVARMod1, n.ahead = 12)
SVARfevd

