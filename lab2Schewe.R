#---zadanie 2.1b
library(MASS)
battery.times<-c(423,705,2623,347,620,2719,1035,482)
#estymator najwiekszej wiarygodnosci z wyprowadzenia 1/mean(x)
my.lambda<-1/mean(battery.times)
r.lambda<-fitdistr(battery.times,densfun = "exponential")
#---zadanie 2.1c
mean<-1/my.lambda
prob.1000h<-exp(-my.lambda*1000)
#---zadanie2.2


#zadanie 2.3
hundred.observations<-rgamma(100,2,1)
print(fitdistr(hundred.observations,densfun="gamma"))

thousand.observations<-rgamma(1000,2,1)
print(fitdistr(thousand.observations,densfun="gamma"))





