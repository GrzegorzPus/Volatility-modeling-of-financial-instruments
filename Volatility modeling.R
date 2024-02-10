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

data <- read_excel("C:\\Users\\GRZEGORZ\\Downloads\\Analiza_zmiennosci.xlsx", col_names=TRUE)


head(data)
names(data)
data[c("Close")]


price <- data$Close
price <- as.matrix(price)


returns<-100*diff(log(price))
returns<-as.matrix(returns)

##################################################################
#                            Plots                               #
##################################################################

data_plot <- read_excel("C:\\Users\\GRZEGORZ\\Downloads\\Analiza_zmiennosci.xlsx", sheet = 2, col_names = TRUE)
data_plot$Date <- as.Date(data_plot$Date)

chartSeries(data_plot)
chartSeries(subset(data_plot, format(Date, "%Y-%m") == "2024-01"))

# Charts for rates of return
plot(returns, type = "l")

# Histogram
chart.Histogram(100*diff(log(data$Close)),
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

# Ad 1) Specyfikacja MODELU AR(1)-GARCH(1, 1) z normalnym rozkładem warunkowym   
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())
# print(specification)
# Ad 2) Estymacja
ar1.garch11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
show(ar1.garch11.norm)

##################################################################
#              Conditional t-Student distribution                #
##################################################################

specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.garch11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
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
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.gjrgarch11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
show(ar1.gjrgarch11.norm)

##################################################################
#                 Conditional t-Student distribution             #
##################################################################

# Ad 1) Specyfikacja MODELU AR(1)-GJR-GARCH(1, 1) z rozkładem warunkowym t-Studenta
specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.gjrgarch11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
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
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.egarch11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
show(ar1.egarch11.norm)


##################################################################
#                Conditional t-Student distribution              #
##################################################################

specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.egarch11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
show(ar1.egarch11.std)

par(mar = c(5, 5, 2, 2))
plot(ar1.egarch11.std, which = 'all')

####################################################################################################################################
#                                           AR(1)-GARCH(1, 1)-in-Mean            
####################################################################################################################################
#                Conditional normal distribution                 #
##################################################################

# Ad 1) Specyfikacja MODELU AR(1)-GARCH(1, 1)-in-Mean z normalnym rozkładem warunkowym   
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())
print(specification)
# Ad 2) Estymacja
ar1.garchm11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
show(ar1.garchm11.norm)

##################################################################
#                 Conditional t-Student distribution             #
##################################################################

# Ad 1) Specyfikacja MODELU AR(1)-GARCH(1, 1)-in-Mean z rozkładem warunkowym t-Studenta  
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.garchm11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
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
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.gjrgarchm11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
show(ar1.gjrgarchm11.norm)

##################################################################
#               Conditional t-Student distribution              #
##################################################################

specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.gjrgarchm11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
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
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "norm", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.egarchm11.norm <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
show(ar1.egarchm11.norm)

##################################################################
#                 Conditional t-Student distribution             #
##################################################################

specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE,
                                              archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(data$Brexit, data$Covid, data$Ukraina),
                                              archex = FALSE), 
                            distribution.model = "std", 
                            start.pars = list(), fixed.pars = list())

# Ad 2) Estymacja
ar1.egarchm11.std <- ugarchfit(data = returns, spec = specification, solver = 'hybrid')

# Ad 3) Analiza wynikow
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