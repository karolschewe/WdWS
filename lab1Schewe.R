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

