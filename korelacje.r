dane = read.csv("Reg_chemikalia.csv", sep=";", dec=",", head=TRUE)

X = dane$surowiec
Y = dane$produkt

#a
plot(X, Y, main="Zależność wielkości produkcji od ilości zużytego surowca", xlab="Ilość zużytego surowca (X)", ylab = "Wielkość produkcji (Y)", pch=19)

#b
cov(X, Y) #S_XY = 138.4889
#Jest różna od 0, zatem istnieje liniowa zależność
#między ilością zużytego surowca, a końcową wielkością
#produkcji środków chemicznych

#Ponieważ kowariancja jest dodatnia, zatem wraz ze
#wzrostem ilości zużytego surowca wzrasta końcowa
#wielkość produkcji środków chemicznych

#c
cor(X, Y) #r = 0.8953468
#Współczynnik korelacji r jest w wartości bezwględnej
#większy lub równy 0,8, zatem istnieje bardzo silny związek
#liniowy między ilością zużytego surowca
#a końcową wielkością produkcji środków chemicznych

#Kowariancja to kierunek zależności, a korelacja to siła zależności

#d
#y = b0 + b1x
lm(Y~X) #y = 22,405 + 3,619x
#Równanie prostej regresji liniowej

#e
abline(lm(Y~X), col = "red")

#f
#Jeśli ilość surowca wzrośnie o 1 litr, to końcowa
#wielkość produkcji środków chemicznych wzrośnie
#o 3,619 kg (interpretacja współczynnika regresji
#liniowej)

#g, h
predict(lm(Y~X), data.frame(X=c(20,15)))
#Jeśli zużyjemy do produkcji 20 litrów surowca, to 
#końcowa wielkość produkcji wyniesie 94,79 kg
#Jeśli zużyjemy do produkcji 15 litrów surowca, to
#końcowa wielkość produkcji wyniesie 76,69 kg

#i
summary(lm(Y~X))$r.squared
#Ocena dopasowania prostej regresji: 0.8016458
#Prosta regresji liniowej jest dobrze dopasowana do danych

#Końcowa wielkość produkcji środków chemicznych
#jest wyjaśniona w ok. 80% przez ilości
#zużytego surowca

#j

#H0: b1=0 (regresja liniowa jest nieistotna)
#H1: b1!=0 (regresja liniowa jest istotna)

anova(lm(Y~X))
#1 sposób
alfa = 0.05
n=length(X)
qf(1-alfa, 1, n-2)
#F = 32.332 > F_t = 5.317655 -> odrzucamy H0

#2 sposób
#alfa = 0.05 > p-value = 0.0004617 -> odrzucamy H0

#Na poziomie istotności alfa=0.05 dane potwierdzają
#hipotezę, że regresja liniowa jest istotna.

#2

dane = read.csv("Reg_urzadzenie.csv", sep=";", dec=",", head=TRUE)

X = dane$efektywnosc
Y = dane$zywotnosc

#a

plot(X, Y, main="Zależność żywotności urządzenia od liczby wyprodukowanych elementów przez urządzenie", xlab="Liczba wyprodukowanych elementów (X)", ylab="Żywotność urządzenia w miesiącach  (Y)", pch=19)

#b

cov(X, Y) #S_XY = -8.652778
#Jest różna od zera, zatem istnieje liniowa zależność

#c

cor(X, Y) #r = -0.9094164
#Wartość bezwględna większa od 0.8, zatem istnieje bardzo
#silny związek liniowy

#d

model = lm(Y~X) # y = 18.8823 - 0.8629x
abline(lm(Y~X), col="red")

#e

#Zmaleje o 0.8629 miesiąca

#f, g
predict(model, data.frame(X=c(11, 19)))

#9.390582
#2.487535

#h
summary(model)$r.squared

#82.7% - prosta regresji liniowej jest dobrze dopasowana

#i

alfa = 0.01
#H0: b1=0 (regresja liniowa jest nieistotna)
#H1: b1!=0 (regresja liniowa jest istotna)

anova(model)

#alfa = 0.01 > p-value = 0.00067 -> odrzucamy H0

#Na poziomie istotności 0.01 dane potwierdzają hipotezę, 
#że regresja liniowa jest istotna

#3

dane = read.csv("Reg_arszenik.csv", sep=";", dec=",", head=TRUE)

#a

X = dane$pH
Y = dane$arszenik

plot(X, Y, main="Zależność usuniętego arszeniku od zakwaszenia gleby")

#b
cov(X,Y) #-18.32, kowariancja różna od zera, zatem istnieje zależność liniowa
cor(X, Y) #-0.95, zatem istnieje silna zależność liniowa

#c
model = lm(Y~X)
abline(model, col="red")

#d

#Maleje o 18.03%

#e

predict(model, data.frame(X=c(7.5, 9)))
#55
#28

#f, g
summary(model)$r.squared
#90.34%

#h
alfa = 0.01
anova(model)

#H0: b1 = 0
#H0: b1 != 0

#alfa = 0.01 > p=value = 1.552e-09
#Zatem odrzucamy H0

#Na poziomie istotności 1% dane potwierdzają 
#hipotezę, że regresja liniowa jest istotna
