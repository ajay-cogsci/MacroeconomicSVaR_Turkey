# SVaR Implementation for Macroeconomic Indicators of Inflation for Turkey
 Implementation of Structural Vector Autoregression and corresponding robustness checks for the Turkish Inflation and chosen macroeconomic indicators
 
# Objectives – 
1.	Investigation of Drivers for Turkish Inflation using SVAR
2.	Investigation of 7 primary models – 2 models with 5 variables, 4 models with 6 variables and 1 model with 7 variables
	
# Macroeconomic Models tested - 
1. OUPRE
2. OUPER
3. O(G)UPER
4. (A)OUPER
5. (D,OS)OUPER
6. O(F)UPER
7. O(V)UPER

where, 


	O is the Year-on-Year Log changes in Global Brend Crude Oil Prices
	U is the Year-on-Year percent changes in the Turkish Unemployement Rate
	P is the the Year-on-Year log changes Consumer Price Index as measure of Turkish Inflation
	E is the Year-on-Year log changes in the exchange rate of USD-TRY (Turkish lira)
	R is the Year-on-Year changes in the policy rate
	G is the Year-on-Year percent changes in the “Production of Total Industry in Turkey”
	A is the Year-on-Year percent changes in the “Global Real Economic Activity”
	F is the Year-on-Year percent changes in the “Wu-Xia shadow US federal rates”
	V is the Year-on-Year percent changes in the “Chicago Global Options Exchange Volatility Index” (VIX)
	D, and OS is the Year percent changes ins the global demand, and regression residuals fitted in the change in commodity change
 
 ### Reference Paper - 
 Yilmazkuday, Hakan. "Drivers of Turkish inflation." The Quarterly Review of Economics and Finance 84 (2022): 315-323.
