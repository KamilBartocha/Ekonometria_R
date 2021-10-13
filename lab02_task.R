library(SemiPar)
data(janka)
View(janka)
plot(janka$dens~janka$hardness)
boxplot(janka$hardness)
plot(janka$hardness,type="o")

reg<-lm(janka$dens~janka$hardness)
summary(reg)
abline(reg)

plot(janka$hardness,reg$res)
abline(h=0)
plot(janka$hardness~reg$fit)

plot(reg$res, type="o")
abline(h=0)
boxplot(reg$res)

qqnorm(reg$res)
qqline(reg$res)
plot(reg$res~reg$fit)

identify(LotSize,reg$res)