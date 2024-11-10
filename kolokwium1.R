#Jakub Bilski, 155865, Grupa L8, Informatyka (ST)

#Zadanie 1

wiek = na.omit(read.csv("dane.csv", sep=";", dec=",", head=TRUE)$wiek)

#(a)

#Średnia:
mean(wiek)
#Interpretacja: Średnia wieku wśród 32 sprawdzonych uczestników kursu wynosi 36.09375

quantile(wiek, 0.25)
#Interpretacja: Kwantyl pierwszy wieku uczestników kursu wynosi 36, tzn. wiek 25% uczestników jest mniejszy
#lub równy 36 oraz wiek 75% uczestników jest większy lub równy 36

#(b)
przedzialy=seq(25, 45, length=5)
szereg=table(cut(wiek, przedzialy))

#(c)
hist(wiek, breaks=przedzialy, freq=TRUE)

#Zadanie 2
h = 5
lambda = 1/h

#Prawdopodobieństwo:
p = 1 - pexp(3, lambda)

#Prawdopodobieństwo, że w kolejnym pomiarze dekoder zarejestruje co najmniej
#4 impulsy wynosi 0.5488.

#Wykres:
curve(dexp(x, lambda), 0, 50, main="Rozkład wykładniczy rozpadów promieniotwórczych", xlab="Liczba rozpadów", ylab="Gęstość prawdopodobieństwa")

#Zadanie 3
n = 150
p = 0.1
q = 1 - p

#(a)
#Przybliżony rozkład zmiennej losowej P - przybliżenie rozkładu dwumianowego rozkładem normalnym:
# P ~ N(n*p, sqrt(n*p*q))

#(b)
pnorm(20, n*p, sqrt(n*p*q))

#Prawdopodobieństwo, że mniej niż 20 studentów w próbie 150 studentów pali wynosi 91,32%

#Zadanie 4
n = 200
mu = 35
s = 5
h = 5
alfa = 0.01*h

install.packages("BSDA")
library("BSDA")

zsum.test(mu, s, n, conf.level=1-alfa)

#Na poziomie ufności 95% średnia zawartość witaminy C
#w soku pomarańczowym mieści się w przedziale
#[34.31; 35.69] - zatem można stwierdzić, że właściciel
#nie podaje prawdziwych informacji nt. zawartości witaminy C
#w soku pomarańczowym.


