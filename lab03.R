library(car)

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
