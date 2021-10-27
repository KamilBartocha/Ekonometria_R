cafeteria <- read.table("cafeteria.txt")
View(cafeteria)
attach(cafeteria)












library(car)
library(ellipse)
zarthan <- read.table("zarthan.txt")
View(zarthan)
attach(zarthan)

plot(zarthan)
cor(zarthan)
summary(zarthan)

#~. - all data
# or reg <- lm(sales~population + income, zarthan)
reg <- lm(sales~., zarthan)
summary(reg)
plot(reg$res ~ reg$fit)
plot(reg$res ~ population)
plot(reg$res ~ income)

boxplot(reg$res)
qqnorm(reg$res)
qqline(reg$res)
avPlots(reg)

# wykres zależności reszt od składnika interakcji
plot(reg$res~I(population*income))
reg2<-lm(sales~. + I(population*income), zarthan) 
summary(reg2)
anova(reg2)


reg0<-lm(sales~1,zarthan)
anova(reg0,reg)

# A - przedział ufności pokrywa \beta_1
# B - przedział ufności pokrywa \beta_2
# chcemy P(a iloczyn b) >= 1-\alfa, rozpisując wychodzi
# \alpha_0 = \frac{\alpha}{2}

?confint
confint(reg,c("population","income"),level=0.95)
plot(ellipse(reg,c("population","income"),level=0.9),type="l")
rect(0.482813482,0.007089742,0.50919647,0.01130842)
?predict
predict(reg,data.frame(population=220,income=2500),interval="confidence",level=0.95)
predict(reg,data.frame(population=c(220,375),income=c(2500,3500)),
        interval="prediction",level=0.95)
library(SemiPar)
data(janka)
View(janka)
# hardness dependent, dens independent
plot(janka$hardness~janka$dens)

reg<-lm(janka$hardnes~janka$dens)
summary(reg)
abline(reg)
plot(reg$res~reg$fit)
boxplot(reg$res)
qqnorm(reg$res)
qqline(reg$res)

# data is not linear, we need to make transformation
# Y' = sqrt(Y) or Y' = log(Y), we choose log

plot(log(janka$hardness)~janka$dens)
reg_logY<-lm(log(janka$hardness)~janka$dens)
summary(reg_logY)
plot(reg_logY$res~reg_logY$fit)
abline(h=0)
boxplot(reg_logY$res)
qqnorm(reg_logY$res)
qqline(reg_logY$res)

# we need to make transformation of X, coz we can see
# that data looks like log
# X' = sqrt(X) or X' = log(X), we choose log

plot(log(janka$hardness)~log(janka$dens))
reg_logXY<-lm(log(janka$hardness)~log(janka$dens))
summary(reg_logXY)
plot(reg_logXY$res~reg_logXY$fit)
abline(h=0)
boxplot(reg_logXY$res)
qqnorm(reg_logXY$res)
qqline(reg_logXY$res)

# Shapiro-Wilk test
shapiro.test(reg_logXY$res)
#p-value = 0.5177 