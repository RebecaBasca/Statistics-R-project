data(Seatbelts)

Seatbelts <- data.frame(as.matrix(Seatbelts), date=time(Seatbelts)) 

dataset = Seatbelts
View(dataset)

attach(dataset)

str(dataset)
head(dataset)

plot(date, DriversKilled, col=(law+2))

mean(kms)
max(PetrolPrice)


library(moments)
skewness(drivers)
kurtosis(drivers)


hist(PetrolPrice,breaks = 20)
plot(density(PetrolPrice))


Law0 = subset(dataset,law == 0)
Law1 = subset(dataset,law != 0)

summary(Law0)
summary(Law1)

View(Law0)

mean(Law0$DriversKilled)

library(ggplot2)


law_comparison <-ggplot(Seatbelts, aes(x=factor(law), y =DriversKilled)) +geom_boxplot(fill = "skyblue")
                  +theme_grey()+ylab ("Number of drivers killed")+xlab("Before and after seatbelt law")
law_comparison


t.test(Law0$DriversKilled, Law1$DriversKilled, mu = 0, alternative = "greater")

shapiro.test(Seatbelts$DriversKilled)
shapiro.test(Law0$kms)
shapiro.test(Law1$kms)



plot(Law0$DriversKilled, Law0$kms, pch=16, col= "Blue")
cor(Law0$DriversKilled, Law0$kms)

model0 = lm(Law0$DriversKilled ~ Law0$kms, data= Law0)
model0

# rezulta ca  DriversKilled ~~ (-0.001744)*kms + 151.091397

summary(model0)


rez=model0$residuals
plot(model0$fitted,model0$residuals)


#datenoi = data.frame(kms = 10000)

#predict(model0, datenoi, interval="predict", level=0.95)



