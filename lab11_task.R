gnp96 <- read.table("gnp96.txt")
gnp <- ts(gnp96$V2, start=1947, f=4)
plot(gnp)
# rosnący ->różnicujemy
dgnp = diff(gnp)
plot(dif)
abline(h=0)
ddgnp = diff(dgnp)
plot(ddif)
abline(h=0)
lgnp <- log(gnp)
dlgnp = diff(lgnp)
plot(dlgnp)

par(mfrow=c(1,1))
acf(dlgnp)
pacf(dlgnp)

?arima
gnp.ar<-arima(dlgnp,order=c(1,0,0))
gnp.ar

gnp.ma<-arima(dlgnp,order=c(0,0,2))
gnp.ma

acf(gnp.ar$res,100)
?Box.test
Box.test(gnp.ar$res, type="Ljung")
qqnorm(gnp.ar$res); qqline(gnp.ar$res)

Box.test(gnp.ar$res, 10, type="Ljung")
qqnorm(gnp.ar$res); qqline(gnp.ar$res)
tsdiag(gnp.ar)
tsdiag(gnp.ma)

gnp.arma<-arima(dlgnp,order=c(1,0,1))
tsdiag(gnp.arma)
