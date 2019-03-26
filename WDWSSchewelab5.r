
#------------zad 3.11------------
df.domy<-read.table("domy.txt",TRUE)
#poziom istotnosci - 0,05
#badamy laczny rozklad stezenia dwutlenku wegla w obu domach
#mamy pary niezaleznych pomiarow z dwoch rozkladow
#odpowiednim testem bedzie paired t test
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

