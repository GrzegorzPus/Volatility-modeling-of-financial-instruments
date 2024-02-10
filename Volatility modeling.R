##################################################################
#                        Import data                             #
##################################################################

install.packages("quantmod")
install.packages("xts")
install.packages("fGarch")
install.packages("PerformanceAnalytics")
#install.packages("Closexlsx")
install.packages("rugarch")
install.packages("readxl")

library(quantmod)
library(xts)
library(fGarch)
library(PerformanceAnalytics)
#library(Closexlsx) 
library(rugarch) 
library(readxl)

data <- read_excel("C:\\Users\\GRZEGORZ\\OneDrive\\Pulpit\\Studia\\Analiza finansowych szeregów czasowych\\Analiza zmiennosci-dane.xlsx", col_names=TRUE)
data_a <- read_excel("C:\\Users\\GRZEGORZ\\OneDrive\\Pulpit\\Studia\\Analiza finansowych szeregów czasowych\\Analiza zmiennosci-dane.xlsx", sheet = 2, col_names = TRUE)
data$Date <- as.Date(data$Date)
data_a$Date <- as.Date(data_a$Date)

head(data_a)
names(data_a)
data_a[c("Close")]


price <- data_a$Close
price <- as.matrix(price)


returns <- 100*diff(log(price))
returns <- as.matrix(returns)
return_date <- data[, 1:2]

##################################################################
#                            Plots                               #
##################################################################

chartSeries(data_a)
chartSeries(subset(data_a, format(Date, "%Y-%m") == "2024-01"))

# Charts for rates of return
plot(returns, type = "l")

# Histogram
chart.Histogram(100*diff(log(data_a$Close)),
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'red', 'green'))
legend("topright", legend = "Normal density", fill = "green", cex=0.5)

# ACF
acf(returns, plot = TRUE)

# PACF = Partial ACF
pacf(returns, plot = TRUE)

# ACF
acf(returns^2, plot = TRUE)


####################################################################################################################################
#                                              AR(1)-GARCH(1,1)            
####################################################################################################################################
                          #              Conditional normal distribution                   #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

ar1.garch11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.garch11.norm)

                          ##################################################################
                          #              Conditional t-Student distribution                #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

ar1.garch11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.garch11.std)

####################################################################################################################################
#                                              AR(1)-GJR-GARCH(1,1)            
####################################################################################################################################
                          #                Conditional normal distribution                 #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

ar1.gjrgarch11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.gjrgarch11.norm)

                          ##################################################################
                          #                 Conditional t-Student distribution             #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

ar1.gjrgarch11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.gjrgarch11.std)


####################################################################################################################################
#                                                AR(1)-EGARCH(1,1)            
####################################################################################################################################
                          #               Conditional normal distribution                 #
                         ##################################################################

specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

ar1.egarch11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.egarch11.norm)


                          ##################################################################
                          #                Conditional t-Student distribution              #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

ar1.egarch11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.egarch11.std)

par(mar = c(5, 5, 2, 2))
plot(ar1.egarch11.std, which = 'all')

####################################################################################################################################
#                                           AR(1)-GARCH(1, 1)-in-Mean            
####################################################################################################################################
                          #                Conditional normal distribution                 #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

ar1.garchm11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.garchm11.norm)

                          ##################################################################
                          #                 Conditional t-Student distribution             #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

ar1.garchm11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.garchm11.std)


####################################################################################################################################
#                                          AR(1)-GJR-GARCH(1,1)-in-Mean            
####################################################################################################################################
                          #                Conditional normal distribution                 #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

ar1.gjrgarchm11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.gjrgarchm11.norm)

                          ##################################################################
                          #               Conditional t-Student distribution               #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

ar1.gjrgarchm11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.gjrgarchm11.std)


####################################################################################################################################
#                                           AR(1)-EGARCH(1,1)-in-Mean            
####################################################################################################################################
                          #                Conditional normal distribution                 #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

ar1.egarchm11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.egarchm11.norm)

                          ##################################################################
                          #                 Conditional t-Student distribution             #
                          ##################################################################

specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraine),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

ar1.egarchm11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

show(ar1.egarchm11.std)


####################################################################################################################################
#                                           Information criteria            
####################################################################################################################################

information.criteria <- t(cbind(
  infocriteria(ar1.garch11.norm),
  infocriteria(ar1.garch11.std),
  infocriteria(ar1.gjrgarch11.norm),  
  infocriteria(ar1.gjrgarch11.std),
  infocriteria(ar1.egarch11.norm),
  infocriteria(ar1.egarch11.std),
  infocriteria(ar1.garchm11.norm),
  infocriteria(ar1.garchm11.std),
  infocriteria(ar1.gjrgarchm11.norm),
  infocriteria(ar1.gjrgarchm11.std),
  infocriteria(ar1.egarchm11.norm),
  infocriteria(ar1.egarchm11.std)
))

rownames(information.criteria) <- c("GARCH(1,1)-n", "GARCH(1,1)-t", "GJR-GARCH(1,1)-n", "GJR-GARCH(1,1)-t", "EGARCH(1,1)-n", "EGARCH(1,1)-t", "GARCH-M(1,1)-n", "GARCH-M(1,1)-t", "GJR-GARCH-M(1,1)-n", "GJR-GARCH-M(1,1)-t", "EGARCH-M(1,1)-n", "EGARCH-M(1,1)-t")
information.criteria

