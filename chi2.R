#1

#H0: proporcje zatrudnionych są takie same jak w całym kraju
#H1: proporcje zatrudnionych różnią się od tych w kraju

kraj = c(0.38, 0.32, 0.23, 0.07) #expected
 
allegheny = c(122, 85, 76, 17) #observed

chisq.test(allegheny, p = kraj)

#chi2 = 3.2939
#df = 3

#alfa = 0.1 < p-val = 0.3485
#Brak podstaw do odrzucenia H0

#Na poziomie istotności 10% dane potwierdzają, że
#proporcje zatrudnionych w Allegheny są takie same
#jak ich odpowiedniki w skali całego kraju

#2

#H0: Odsetki zgonów są takie same jak w artykule
#H1: Odstetki zgonów różnią od tych w artykule

artykul = c(0.74, 0.16, 0.10)
okreg = c(68, 27, 5)

chisq.test(okreg, p = artykul)

#alfa = 0.1 > p-value = 0.005
#Odrzucamy H0

#Na poziomie istotności 10% dane nie potwierdzają, że
#odsetki poszczególnych zgonów w okręgu są takie same,
#co przedstawione w artykule

#3

#H0: rozkład smaków w cukierkach Skittles wynosi 20%
#H1: ~H0

#Smaki: cytrynowy, limonkowy, pomarańczowy, truskawkowy, winogronowy

observedf = c(43, 50, 44, 44, 52)
expectedp = c(0.2, 0.2, 0.2, 0.2, 0.2)

chisq.test(observedf, p = expectedp)

#chi2 = 1.4421, p-value = 0.8369

#alfa = 0.05

#alfa < p-value -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% dane potwierdzają, że
#rozkład smaków w cukierkach Skittles wynosi 20%.

#Zadanie 4

ozon = read.csv("normalnosc_ozon.csv", sep=";", dec=",", head=TRUE)
ozon = ozon$ozon

#H0: stężenie ozonu ma rozkład normalny
#H1: stężenie ozonu nie ma rozkładu normalnego
install.packages("nortest")
library("nortest")

alfa = 0.05

pearson.test(ozon, adjust=FALSE)$p.value
#p-value = 0.2689319 -> brak podstaw do odrzucenia H0
pearson.test(ozon, adjust=TRUE)$p.value
#p-value = 0.145961 -> brak podstaw do odrzucenia H0
lillie.test(ozon)$p.value
#p-value = 0.2774425 -> brak podstaw do odrzucenia H0
shapiro.test(ozon)
#p-value = 0.1097949 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% dane nie potwierdzają
#hipotezy, że stężenie ozonu nie ma rozkładu
#normalnego.

###################
#Zad 4 - test normalności dla szeregu rozdzielczego

file=read.csv("normalnosc_ozon.csv",sep=";",dec=",")
ozon=file$ozon
br=c(0,2,4,6,8,10,12)
k=length(br)-1
series=cut(ozon,br)
y=table(series)

m=mean(ozon)
s=sd(ozon)
n=length(ozon)

hist(ozon,br,freq=F)
curve(dnorm(x,m,s),xlim=c(br[1],br[k+1]),col=3, add=TRUE)

observedf=c();
for (i in 1:dim(y)){
  observedf=c(observedf,y[[i]])
}
observedf

normprobabilities=c();
for (i in 1:length(br)-1) {
  normprobabilities=c(normprobabilities,pnorm(br[i+1],m,s)-pnorm(br[i],m,s))
}
normprobabilities[1]=pnorm(br[2],m,s)
normprobabilities[length(br)-1]=1-pnorm(br[length(br)-1],m,s)

normalfreq=normprobabilities*n
normalfreq

#Zauważmy, że w dwóch ostatnich klasach mamy mniej niż 5 obserwacji. Dlatego musimy połączyć te klasy.

normalfreq[k-1]=normalfreq[k-1]+normalfreq[k]
normalfreq=normalfreq[-c(k)]

observedf[k-1]=observedf[k-1]+observedf[k]
observedf=observedf[-c(k)]

normalfreq
observedf

k=k-1

expectedp=normalfreq/sum(normalfreq)
expectedp

chisq.test(observedf,p=expectedp)

#Zadanie 5

observed = c(10, 25, 35, 20, 10)
n = sum(observed)
breaks = c(0, 2, 4, 6, 8, 10)

# Dane
observed_counts <- c(10, 25, 35, 20, 10)
total_counts <- sum(observed_counts)

# Przedziały
breaks <- c(0, 2, 4, 6, 8, 10)

# Średnia i odchylenie standardowe
mean_value <- sum((breaks[-length(breaks)] + breaks[-1]) / 2 * observed_counts) / total_counts
std_dev <- sqrt(sum(((breaks[-length(breaks)] + breaks[-1]) / 2 - mean_value)^2 * observed_counts) / total_counts)

# Wartości oczekiwane dla każdego przedziału na podstawie rozkładu normalnego
expected_counts <- diff(pnorm(breaks, mean = mean_value, sd = std_dev)) * total_counts

# Wykonanie testu chi-kwadrat
chi_square_test <- chisq.test(observed_counts, p = expected_counts, rescale.p = TRUE)

# Wyświetlenie wyników
chi_square_test


pearson.test(x = rep(1:length(observed), observed), n.class = length(observed))

#Zadanie 6

#H0: rozkład ocen jest normalny
#H1: rozkład ocen nie jest normalny

file = read.csv("normalnosc_punkty.csv", sep=";", dec=",", head=TRUE)
punkty = file$punkty

alfa = 0.01

shapiro.test(punkty)$p.value

#alfa = 0.01 > p-value = 0.0006
#Odrzucamy H0

#Na poziomie istotności 0.01 dane potwierdzają hipotezę,
#że oceny w tej grupie nie mają rozkładu normalnego.


#Zadanie 7

miejski = c(15, 12, 8)
podmiejski = c(8, 15, 9)
wiejski = c(6, 8, 7)

TK = data.frame(miejski, podmiejski, wiejski)

chisq.test(TK)
alfa = 0.05
#p-value = 0.5569 > alfa -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% dane nie potwierdzają
#hipotezy, że poziom wykształcenia jest zależny
#od miejsca zamieszkania

#Zadanie 8

tak = c(10, 7, 4)
nie = c(90, 93, 96)

TK = data.frame(tak, nie)

#H0: odsetek nie zależy od linii lotniczych
#H1: odsetek zależy od linii lotniczych

chisq.test(TK)

alfa = 0.05

#p-value = 0.251 > alfa -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% dane nie potwierdzają,
#że odsetek pasażerów, którzy zagubili bagaż w trakcie
#lotu zależy od linii lotniczej.

#Zadanie 9

#H0: opinia nie zależy od wieku
#H1: opinia zależy od wieku

za = c(96, 96, 90, 36)
przeciw = c(201, 189, 195, 234)
niewiem = c(3, 15, 15, 30)

tabela = data.frame(za, przeciw, niewiem)

chisq.test(tabela)
alfa = 0.05

#p-value = 2.511e-11 < alfa -> odrzucamy H0

#Na poziomie istotności 5% dane potwierdzają 
#hipotezę, że odczucia danej osoby w związku
#z ograniczeniami dot. anten satelitarnych
#są powiazane z jej wiekiem.
