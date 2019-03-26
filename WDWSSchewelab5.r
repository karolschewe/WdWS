this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Zadanie 3.9
# a)
# X = 1 osoba robiąca zakupy regularnie w Warszawie
# X = 0 osoba, która nie robi zakupów regularnie w Warszawie
# Y = 1 osoba robiąca zakupy regularnie w Krakowie
# Y = 0 osoba, która nie robi zakupów regularnie w Krakowie
# Rozkłady X i Y są dwupunktowe 
# H0: pw = pk
# H1: pw > pk
# nw*pw - liczba klientów regularnych z Warszawy = 40 > 5
# nw *  (1 - pw) - liczba klientów nieregularnych z Warszawy = 193 > 5
# nk*pk - liczba klientów regularnych z Krakowa = 31 > 5
# nk *  (1 - pk) - liczba klientów nieregularnych z Krakowa = 189 > 5
# Możemy stosować przybliżenie rozkładem normalnym:
prop.test(x = c(40, 31), n = c(233, 220), alternative = "greater")
# p-value = 0.2204 > alfa = 0.05
fisher.test(matrix(c(40, 31, 193, 189), nrow = 2), alternative = "greater")
# p-value = 0.2206 > alfa = 0.05
# Odp.: Nie ma podstaw do odrzucenia hipotezu H0. Odsetek klientów w obu sklepach jest taki sam

#-----zad3.10-----
df.plony<-read.table('yields.txt')
gatunek1<-df.plony$yield[df.plony$variety=='v1']
gatunek2<-df.plony$yield[df.plony$variety=='v2']
print(t.test())


#------------zad 3.11------------?????????????????
df.domy<-read.table("domy.txt",TRUE)
#poziom istotnosci - 0,05
#badamy laczny rozklad stezenia dwutlenku wegla w obu domach
#mamy pary niezaleznych pomiarow z dwoch rozkladow
#odpowiednim testem bedzie paired t test????????
#H0: mu1=mu2
#H1: mu1 > mu2
#mu1: srednie stezenie w domu energooszczednym
#mu2: stezenie w normalnym domu

print(t.test(df.domy$domE,df.domy$domS, alternative = "greater", paired = TRUE)) #p-value = 0.003 < 0.05
#odrzucamy H0
#Odp.: Stezenie w domu energooszczednym jest wieksze.
# --------zad 3.11 b-----------
#jaka jest moc testu zakladajac ze mu1 = mu2+50?
#czyli jakie jest prawdopodobienstwo ze odrzucimy H0 kiedy mu1 jest wieksze od mu2 o 50?
print(power.t.test(12,50,sd=sd(df.domy$domE-df.domy$domS),type = "paired",alternative = "one.sided"))
#0.84
#Odp.: Prawd. ze odrzucimy H0 kiedy H1=H0+50 wynosi 84%.

#-----zadanie 3.12-----
przyrzad.a<-c(144, 165, 125, 149, 128, 159)
przyrzad.b<-c(147, 167, 124, 152, 127, 160)
#przyjmuje ze dane maja rozklad normalny
#mamy pary niezaleznych pomiarow z dwoch rozkladow
#odpowiednim testem bedzie paired t test
#H0: mu1=mu2
#H1: mu1 != mu2
print(t.test(przyrzad.a,przyrzad.b,alternative = "two.sided",paired = TRUE))
#p-value 0.18 > 0.01
#nie ma podstawy do odrzucenia hipotezy zerowej
#Odp.: nie ma istotnej roznicy w pomiarach cisnienia
#------zadanie 3.12b-------
#liczymy moc testu 
#p = 0.8
# delta = ?
print(power.t.test(n=6, sd=sd(przyrzad.a - przyrzad.b), sig.level=0.01,
             power=0.8, type="paired"))
#delta = 3.87
#----zadanie 3.12c----------
#n=?
print(power.t.test(sd=sd(przyrzad.a-przyrzad.b),sig.level=0.01,
                      power=0.8,type="paired",delta=1.2))#30.7
#Odp.: Aby z prawdopodobienstwem 80% wykryc roznice o 1.2 nalezy miec 30 probek

#------zadanie 3.13----------

#### Zad 3.13 ####
gatA <- c(26.4,22.5,24.9,23.7,21.5)
gatB <- c(25.1,29.0,23.4,27.6,22.3)
#a
# alfa = 0.05
# H0: muA=muB
# H1: muA < muB
# Zalozenie: zawartosc nikotyny ma rozklad normalny
# Zastosujemy unpaired t.test, poniewaz pomiary obu gatunkow 
# mamy dwa rozne gatunki kiepow
# Musimy sprawdzic czy sigA=sigB.
# Uzyjemy var.test
# H0: sigA=sigB
# H1: sigA != sigB

print(var.test(gatA,gatB,alternative = 'two.sided'))
# p-val=0.48>alfa=0.1, nie ma podstawy do odrzucenia hipotezy, wiec przyjmujemy H0, czyli sigA=sigB
# uzyjemy unpaired t.test z rownymi wariancjami

print(t.test(gatA, gatB, alternative = "less", mu=0, paired=FALSE, 
       var.equal = TRUE))
#p- value=0.15 nie ma podstawy do odrzucenia hipotezy H0 - 
#Odp.: Szlugi maja tyle samo nikotyny

#------zadanie 3.13b------???????/
# p = ?
#delta = 2 
#szukane: 1 - power t.test

#odchylenie z dysku: std<-sqrt((4*var(gatA)+4*var(gatB))/8)
p.pomocnicze<- power.t.test(n=5, delta=2, sd=sd(gatA-gatB),type ="two.sample", alternative = "one.sided")$power

print(1-p.pomocnicze)

#(c) Jak liczne musz? by? pr?by testowe aby test z pkt. (a)
# dawa? poprawn? odpowied? z prawd. nie mniejszym ni? 0.75, 
# w sytuacji gdy gatunek B zawiera ?rednio o 2 mg nikotyny
# wi?cej ni? gatunek A.
# P(poprawnej odpowiedzi|muB=muA+2) = 
# = P(odrzucenia H0| muB-muA=2) = moc.testu(delta=2) >= 0.75
power.t.test(delta=2,sd=sd(gatA-gatB),type = "two.sample",
             alternative="one.sided", power=0.75)$n
# Pr?by testowe musz? liczy? co najmniej po 17 sztuk
# papieros?w





