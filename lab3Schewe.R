#quantiles function (qnorm) - odwrotna dystrybuanta
#dystrybuanta mówi z jakim prawdopodobieñstwem wylosowana wartosc bedzie mniejsza badz rowna x
#odwrotna dystrybuanta mówi na odwrót x s¹ prawdopodobieñstwa y wartoœæ mniejsza badz rowna

#potrzebujemy wiedzieæ jakie wartosci x bedziemy podstawiac do funkcji gestosci aby miec 
# 0.001 i 0.999 kwantyl. Idealna do tego bedzie qnorm



poczatek<-qnorm(0.001)
koniec<-qnorm(0.999)
wektor.iksow<-seq(poczatek,koniec,0.001)
plot(x=wektor.iksow, y=dnorm(wektor.iksow),xlim = c(-3.5,3.5),type = 'l',ylab = "gestosc")
#rozklad studenta: gêstoœæ dt (rt - losuje z rozkladu itd.)
lines(x=wektor.iksow,y=dt(wektor.iksow,df = 5), col = "red" )
lines(x=wektor.iksow,y=dt(wektor.iksow,df = 10), col = "green" )
lines(x=wektor.iksow,y=dt(wektor.iksow,df = 20), col = "blue" )


legend("topright", legend=c("gauss", "t-Student;5","t-Student;10","t-Student;20"),
       col=c("black", "red","green","blue"), lty=1, cex=0.6,
       title="Distributions", text.font=4)

#------zadanie 3.2-------

#a
df.kozy<-read.table("goats.txt",header = TRUE)
#uzyjemy t.test
print(t.test(df.kozy$WeightInitial,mu=23,alternative = "greater",conf.level = 0.05))
# p-wartosc wyniosla 0,39 > 0,05 
#nie ma podstawy aby odrzuciæ hipotezê zerow¹ (H_0 = œrednia masa jest równa 23kg)

#b
#prawdopodobienstwo, ze nie odrzucimy H_0 pod warunkiem ze mu jest rowne 24kg wynosi:
#1- prawdopodobienstwo ze odrzucimy H_0
#w R: 1 - power.t.test
#uwaga: bierzemy estymator odchylenia std.
prawd.odrzucenia<-power.t.test(n=40,delta=1,sd=sd(df.kozy$WeightInitial),
                               sig.level = 0.05,type="one.sample",alternative = "one.sided")
prawd.przyjecia<-1-prawd.odrzucenia$power
print(prawd.przyjecia)


#c



#----zadanie3.6----
#H_0: p = 0,35 H_1: p < 0.35
#n*p = ?
n = 400
p = 0.35
q = 1-p
if (n*p >= 5 | n*q>=5 )
{
  print("prop.test")
} else
{
  print("binom test")
}
#robimy prop.test:
print(prop.test(x=128,n=400,p=0.35,alternative = "less"))
#p-value= 0.114 > 0.05 nie ma podstawy aby odrzucic hipoteze zerowa (p = 0,35)