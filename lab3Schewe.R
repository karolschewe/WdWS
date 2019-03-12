#quantiles function (qnorm) - odwrotna dystrybuanta
#dystrybuanta mówi z jakim prawdopodobieństwem wylosowana wartosc bedzie mniejsza badz rowna x
#odwrotna dystrybuanta mówi na odwrót x są prawdopodobieństwa y wartość mniejsza badz rowna

#potrzebujemy wiedzieć jakie wartosci x bedziemy podstawiac do funkcji gestosci aby miec 
# 0.001 i 0.999 kwantyl. Idealna do tego bedzie qnorm

poczatek<-qnorm(0.001)
koniec<-qnorm(0.999)
wektor.iksow<-seq(poczatek,koniec,0.001)
plot(x=wektor.iksow, y=dnorm(wektor.iksow),xlim = c(-3.5,3.5),type = 'l')
#rozklad studenta: gęstość dt (rt - losuje z rozkladu itd.)
lines(x=wektor.iksow,y=dt(wektor.iksow,df = 5), col = "red" )
lines(x=wektor.iksow,y=dt(wektor.iksow,df = 10), col = "green" )
lines(x=wektor.iksow,y=dt(wektor.iksow,df = 20), col = "blue" )

legend("topright", legend=c("gauss", "t-Student;5","t-Student;10","t-Student;20"),
       col=c("black", "red","green","blue"), lty=1, cex=0.6,
       title="Distributions", text.font=4)

#------zadanie 3.2-------


df.kozy<-read.table("goats.txt",header = TRUE)
#uzyjemy t.test
print(t.test(df.kozy$WeightInitial,mu=23,alternative = "greater",conf.level = 0.05))

