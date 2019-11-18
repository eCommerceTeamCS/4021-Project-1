datadir <- "C:/Users/student.DESKTOP-JKQBVN5.000/Documents/Data/"
sourcedir <-"C:/Users/student.DESKTOP-JKQBVN5.000/Documents/RCode"
setwd(datadir)
gnp <- read.table("gnp96.dat")
setwd(sourcedir)

install.packages("forecast")
library(forecast)

gnp.ts<-ts(gnp$V2)
plot(gnp.ts)

time<-gnp$V1
gnp.ts.lm <- lm(gnp.ts~time)
summary(gnp.ts.lm)
plot(time, gnp.ts, type = "l")
gnp.trend.seasonal <- lm(gnp.ts ~ time + sin(2*pi*time/12) + cos(2*pi*time/12))
summary(gnp.trend.seasonal)
plot(time, gnp.ts, type = "l")
lines(time, gnp.trend.seasonal$fitted.values, col = "red")

# Model diagnostics for temp.trend.seasonal
par(mfrow=c(2,2))
plot(gnp.trend.seasonal, labels.id = NULL)
par(mfrow=c(1,1))

# Get the residuals from the gnp.trend.seasonal model above and store in e.ts.temp:
e.ts.temp <- ts(gnp.trend.seasonal$residuals)

# Plot the residuals for the temp.trend model
plot(e.ts.temp)
# Plot acf and pacf side by side
par(mfrow=c(1,2))
acf(e.ts.temp, main="ACF of Residuals\nfrom gnp.trend.seasonal")
pacf(e.ts.temp,main="PACF of Residuals\nfrom gnp.trend.seasonal")
par(mfrow=c(1,1))

#first order difference of our residuals
par(mfrow=c(1,2))
acf(diff(e.ts.temp), main="Diff ACF of Residuals\nfrom gnp.trend.seasonal")
pacf(diff(e.ts.temp),main="Diff PACF of Residuals\nfrom gnp.trend.seasonal")
par(mfrow=c(1,1))
