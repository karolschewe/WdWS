library(faraway)
#zadanie 1a
data(pima) #wczytanie ramki pima z biblioteki faraway
pima$test<-factor(pima$test)#zamiana wektora na factor
levels(pima$test)<-c("brak objawow", "sa objawy") #sposob na przypisanie etykiet do naszego factora
pima$diastolic[pima$diastolic==0]<-NA #wybieramy te rekordy, ktore maja zerowa zmienna diastolic
pima$glucose[pima$glucose==0]<-NA #przypisujemy im potem wartosc NA
pima$triceps[pima$triceps==0]<-NA #robimy to dla wszystkich zmiennych, gdzie nie moze byc zero
pima$insulin[pima$insulin==0]<-NA
pima$bmi[pima$bmi==0]<-NA
#zadanie 1b
srednia<-mean(pima$diastolic,na.rm = TRUE)
mediana<-median(pima$diastolic,na.rm = TRUE)
kwartyl.dolny<-quantile(pima$diastolic,probs = 0.25,na.rm = TRUE)
kwartyl.gorny<-quantile(pima$diastolic,probs = 0.75,na.rm = TRUE)
rozstep<-range(pima$diastolic,na.rm = TRUE)
zmienna2<-kwartyl.gorny-kwartyl.dolny#mozna tak
rozstep.miedzykwartylowy<-IQR(pima$diastolic,na.rm = TRUE)#lub funkcja
stdev<-sd(pima$diastolic,na.rm = TRUE)
#zadanie1c
babki.cisnienie<-pima$diastolic[pima$test=="sa objawy"]#wybranie babek z cukrzyca
srednia.cukrzyca<-mean(babki.cisnienie,na.rm = TRUE)
stdev.cukrzyca<-sd(babki.cisnienie,na.rm = TRUE)
#zadanie1d
print(boxplot(pima$pregnant))#wykres skrzynkowy
#zadanie1e
print(length(babki.cisnienie))#dlugosc wektora jest liczba kobiet z objawami cukrzycy
#zadanie1f
print(hist(pima$diastolic))
print(plot(density(pima$diastolic,na.rm = TRUE)))


#zadanie2a
dane.o.wyspach <- read.table(file.choose(),header=TRUE)
#zadanie2b
srednia.zolwie <- mean(dane.o.wyspach$Species)
mediana.zolwie<- median(dane.o.wyspach$Species)
dolny.zolwie<-quantile(dane.o.wyspach$Species,0.25)
gorny.zolwie<-quantile(dane.o.wyspach$Species,0.75)
range.zolwie<-range(dane.o.wyspach$Species)
IQR.zolwie<-IQR(dane.o.wyspach$Species)
stdev.zolwie<-sd(dane.o.wyspach$Species)
wariancja.zolwie<-var(dane.o.wyspach$Species)
#zadanie2c
histogram.zolwie<-hist(dane.o.wyspach$Area, breaks = 5,main = "Powierzchnia wysp",xlab = "powierzchnia[km^2]",ylab = "liczba zliczen")
#zadanie2d
gatunki<-dane.o.wyspach$Species[dane.o.wyspach$Area<25]
boxplot(gatunki,main="Wykres skrzynkowy dla gatunkow")
#zadanie3
wektor<-c((-1-sqrt(10))/4,-1/4,-1/4,(-1+sqrt(10))/4,1)
skosnosc<-skewness(wektor)
#zadanie4
wektor2<-c(5,8,9,3,8,7)
median(wektor2)
quantile(wektor2,0.25)
quantile(wektor2,0.75)
