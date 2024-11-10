dane = read.csv("DwiePopulacje.csv", sep=";", dec=".", head=TRUE)

#Zadanie 1

reg1=na.omit(dane$cel1)
reg2=na.omit(dane$cel2)
n1=length(reg1)
n2=length(reg2)
x_bar1=mean(reg1)
x_bar2=mean(reg2)
var1=var(reg1)
var2=var(reg2)
Sp2=((n1-1)*var1+(n2-1)*var2)/(n1+n2-2)
#1. H0: mu1=mu2 H1: mu1!=mu2
#2
t=(x_bar1-x_bar2)/sqrt(Sp2*(1/n1+1/n2))
#t=-1.539823
alfa=0.02
qt(1-alfa/2, n1+n2-2)
#3. R=(-inf; -2.47266) suma (2.47266; inf)
#4. t nie należy do R -> zatem nie mamy podstaw
#do odrzucenia H0
#5. Na poziomie istotności alfa=0.02 dane
#nie potwierdzają hipotezy, że przeciętna
#zawartość celulozy dla regionu I różni się
#istotnie od przeciętnej zawartości celulozy
#dla regionu II.

t.test(reg1, reg2, var.equal=TRUE, conf.level=1-alfa)
#p-value = 0.1352
#alfa = 0.02 < p-value = 0.1352 -> brak podstaw do odrzucenia H0


#b)
#1. H0: sig^2_1 - sig^2_2=0
# H1: sig^2_1 - sig^2_2 != 0
#2
F=var1/var2
#F=0.4786012
#3.
qf(alfa/2, n1-1, n2-1)
qf(1-alfa/2, n1-1, n2-1)
#R=(0; 0.162458) suma (3.69874; inf)
#4. F nie należy do R -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie istotności 0.02 dane nie potwierdzają hipotezy
#o różności wariancji. Zatem możemy założyć, że wariancje
#są jednorodne

var.test(reg1, reg2, alternative="two.sided")
# alfa = 0.02 < p-value = 0.3225 -> nie mamy podstaw do odrzucenia H0

#t
t.test(reg1, reg2, var.equal=TRUE, conf.level=0.98)
#Na poziomie ufności 98% przedział
#(-13.52; 3.15) pokrywa nieznaną
#prawdziwą różnicę średnich zawartości
#celulozy w dwóch regionach.
#Ponieważ przedział (-13.52; 3.15) pokrywa 
#wartość 0, zatem nie mamy podstaw
#do odrzucenia H0.

#Zadanie 2

tradycyjna = na.omit(dane$tradycyjna)
nowa = na.omit(dane$nowa)

n1 = length(tradycyjna)
n2 = length(nowa)

#H0: mu1 <= mu2
#H1: mu1 > mu2

alfa = 0.1

t.test(tradycyjna, nowa, var.equal=FALSE, alternative="greater", conf.level = 1-alfa)
#p-value > alfa, brak podstaw do odrzucenia H0

var.test(tradycyjna, nowa, conf.level=1-alfa)
#p-value > alfa, brak podstaw do odrzucenia H0

#Zadanie 3
publiczny=na.omit(dane$publiczny)
prywatny=na.omit(dane$prywatny)
#1. H0:sig^2_1 = sig^2_2
#H1: sig^2_1 != sig^2_2
#2.
var.test(publiczny, prywatny, alternative="two.sided")
#3. p-value = 0.08687
#4. alfa =0.1 > p-value = 0.08687 -> odrzucamy H0
#5. Na poziomie istotności 0.1 dane
# potwierdzają hipotezę o różności wariancji

#Zakładamy, że wariancje są różne

#1. H0: mu1 - mu2 >= 0 
#H1: mu1-mu2 < 0
t.test(publiczny, prywatny, var.equal=FALSE, alternative="less")
#p-value = 0.023
#alfa = 0.1 > p-value = 0.023 -> odrzucamy H0
#Na poziomie istotności 0.1 dane
#potwierdzają hipotezę, że publiczne
#źródła finansowania udzielają,
#przeciętnie rzecz biorąc, mniejszych kredytów

#Zadanie 4

zm1=na.omit(dane$zawodnik1)
zm2=na.omit(dane$zawodnik2)

#1. H0: sig^2_1-sig^2_2>=0 
#H1: sig^2_1-sig^2_2 < 0
#2.
var.test(zm1, zm2, alternative="less")
#3. p-value = 0.2108
#4. alfa=0.05 < p-value = 0.2108 -> brak podstaw do odrzucenia H0
#5. Na poziomie istotności 0.05 dane nie potwierdzają hipotezy
#o większej regularności wyników pierwszego zawodnika

#Zadanie 5

#H0: mu1 <= mu2
#H1: mu1 > mu2

L1 = na.omit(dane$L1)
L2 = na.omit(dane$L2)

alfa = 0.1

var.test(L1, L2, conf.level=1-alfa)
#p-value > alfa -> brak podstaw do odrzucenia H0

t.test(L1, L2, var.equal=FALSE, alternative = "greater", conf.level=1-alfa)
#p-value < alfa -> odrzucamy H0


#Zadanie 6
p1 = 0.78
p2 = 0.8
T1 = p1*n1
T2 = p2*n2
n1=1200
n2=2000
alfa = 0.1

przedzial = function (p1, n1, p2, n2, alfa){
  p_diff = p1-p2
  se_diff = sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
  z = qnorm(1-alfa/2)
  lower = p_diff - z* se_diff
  upper = p_diff + z* se_diff
  return(c(lower, upper))
}

przedzial(p1, n1, p2, n2, alfa)

#Na poziomie ufności 90% przedział (-4.446%; 0.46%)
#pokrywaw nieznaną prawdziwą różnicę proporcji zadowolonych
#Polaków i Amerykanów z pracy

prop.test(c(T1, T2), c(n1,n2), conf.level=0.9)$conf.int


#Na poziomie ufności 90% przedział (-4.53%;0.53%)
#pokrywa nieznaną prawdziwą różnicę propocji zadowolonych 
#Polaków i Amerykanów z pracy

#b)
alfa = 0.1
#H0: p1-p2>=0
#H1: p1-p2<0
prop.test(c(T1,T2), c(n1,n2), alternative="less", conf.level=0.9)
#p-value = 0.09583
# alfa = 0.1 > p-value = 0.09583 -> odrzucamy H0
# Na poziomie istotności 0.1 dane potwierdzają
# hipotezę, że proporcja zadowolonych POlaków jest mniejsza niż
# zadowolonych Amerykanów

Z = (p1 - p2)/sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2)) #-1.339
z = qnorm(1-alfa/2) #1.644

#H1 greater, więc obszar krytyczny: (z, +inf) -> statystyka Z w obszarze krytycznym, więc odrzucamy H0

#(c)

#H0: p0 <= 0.75
#H1: p0 > 0.75

p0 = 0.75
Z = (p1 - p0) / sqrt((p0*(1-p0)/n1)) #2.4
z = qnorm(1-alfa/2) #1.64
#H1 greater, więc obszar krytyczny: (z, +inf) -> statystyka Z w obszarze krytycznym, więc odrzucamy H0

prop.test(T1, n1, p = p0, alternative = "greater", conf.level = 1 - alfa)
#p-value = 0.009 < alfa = 0.1 -> odrzucamy H0
binom.test(T1, n1, p = p0, alternative="greater", conf.level = 1 - alfa)
#p-value = 0.008 < alfa = 0.1 -> odrzucamy H0

#Zadanie 7

afryka = c(313, 28)
azja = c(145, 56)

n1 = sum(afryka)
n2 = sum(azja)

p1a = afryka[1]/n1
p1b = afryka[2]/n1

p2a = azja[1]/n2
p2b = azja[2]/n2

phat = (afryka[1]+azja[1])/(n1+n2)

#(a)

#H0: p1a = p2a
#H1: p1a != p2a
alfa = 0.05
Z = (p1a - p2a) / sqrt(phat*(1-phat)*(1/n1 + 1/n2)) #6.11
z = qnorm(1-alfa/2) #1.96
#H1 two-sided, więc obszar krytyczny: (-inf, -z) U (z, +inf) -> statystyka Z w obszarze krytycznym, więc
#odrzucamy H0

prop.test(c(afryka[1], azja[1]), c(n1, n2), alternative="two.sided", conf.level=1-alfa)
#p-value = 0 < alfa = 0.05 -> odrzucamy H0

#Na poziomie istotności 0.05 dane potwierdzają hipotezę, że częstość występowania malarii typu A
#zależy od regionu.

#(b)

przedzial = function (p1, n1, p2, n2, alfa){
  p_diff = p1-p2
  se_diff = sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
  z = qnorm(1-alfa/2)
  lower = p_diff - z* se_diff
  upper = p_diff + z* se_diff
  return(c(lower, upper))
}

przedzial(p1a, n1, p2a, n2, alfa)

#Na poziomie ufności 95% przedział (12.8%; 26.5%) pokrywa prawdziwą nieznaną różnicę
#częstości występowania malarii typu A w Afryce i Azji.

prop.test(c(afryka[1], azja[1]), c(n1, n2), conf.level=1-alfa)$conf.int


#Na poziomie ufności 95% przedział (12.41%; 26.9%) pokrywa prawdziwą nieznaną różnicę
#częstości występowania malarii typu A w Afryce i Azji.


#Zadanie 8

#temperatura 11 stopni

T1 = 73
n1 = 105
T2 = 102
n2 = 110
p1 = T1/n1
p2 = T2/n2
phat = (T1+T2)/(n1+n2)
alfa = 0.05

#(a)
#H0: p1 = p2
#H1: p1 != p2
Z = (p1 - p2) / sqrt(phat*(1-phat)*(1/n1 + 1/n2)) #-4.37
z = qnorm(1-alfa/2) #1.96

#H1 two-sided, więc obszar krytyczny: (-inf, -z) U (z, +inf) -> statystyka Z w obszarze krytycznym, więc
#odrzucamy H0

prop.test(c(T1, T2), c(n1, n2), alternative = "two.sided", conf.level = 1-alfa)
#p-value = 0 < alfa = 0.05 -> odrzucamy H0

#Na poziomie istotności 0.05 dane potwierdzają hipotezę, że propocja przeżywalności zależy od temperatury.

#(b)

przedzial = function (p1, n1, p2, n2, alfa){
  p_diff = p1-p2
  se_diff = sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
  z = qnorm(1-alfa/2)
  lower = p_diff - z* se_diff
  upper = p_diff + z* se_diff
  return(c(lower, upper))
}

przedzial(p1, n1, p2, n2, alfa)

#Na poziomie ufności 95% przedział (-33.3%; -13.2%) pokrywa prawdziwą nieznaną różnicę
#przeżywalności w badanych temperaturach.

prop.test(c(T1, T2), c(n1, n2), conf.level=1-alfa)

#Na poziomie ufności 95% przedział (-34.2%; -12.2%) pokrywa prawdziwą nieznaną różnicę
#przeżywalności w badanych temperaturach.


#temperatura 30 stopni


#Zadanie 9

przed=c(15,4,9,9,10,10,12,17,14)
po=c(14,4,10,8,10,9,10,15,14)
roznica=przed-po
alfa = 0.05

#1. H0: mu=0 
#H1: mu!=0

#2. 
t.test(roznica, conf.level=0.05)
#3. p-value = 0.08052
#4. alfa = 0.05 < p-value = 0.08052 -> brak podstaw do odrzucenia H0
#5. Na poziomie istotności 0.05 dane nie 
# potwierdzają hipotezy, że rodzaj
# leku zmienia wartości określonego
#parametru biochemicznego

#Zadanie 10

ph_15cm = c(6.55, 5.98, 5.59, 6.17, 5.92, 6.18, 6.43, 5.68)
ph_100cm = c(6.78, 6.14, 5.80, 5.91, 6.10, 6.01, 8.18, 5.88)
d = ph_15cm - ph_100cm
alfa = 0.1

#(a)
t.test(d, conf.level = 1 - alfa)

#p-value = 0.2312 > alfa = 0.01 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 0.1 dane nie potwierdzają hipotezy, że pH zależy
#od głębokości.

#(b)

odchylenie = sd(d)
srednia = mean(d)
n = length(d)
alfa = 0.1

przedzial = function(D, S, n, alfa){
  t = qt(1- alfa/2, df= n-1)
  x = t * (S/sqrt(n))
  lower = D - x
  upper = D + x
  return(c(lower, upper))
}

przedzial(srednia, odchylenie, n, alfa)

#Na poziomie 90% ufności przedział (-70.3%; 12.8%) pokrywa nieznaną prawdziwą
#nieznaną różnicę w poziomie pH wody w różnych głębokościach.

