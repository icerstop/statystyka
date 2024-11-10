loty = read.csv('loty.csv', sep=';', head = TRUE)
loty
class(loty)
min(loty)

loty$X1956
loty[,2]

for (i in 1:6){
  print("Średnia" + names(loty)[i])
  print(mean(loty[,i]))
}

quantile(loty[,2])
Q1 = quantile(loty[,2])[3]

przedzialy=seq(200, 650, length=10)
przedzialy
nazwy = names(loty)
kolory = c("blue", "red", "yellow", "black", "green", "pink")

for (i in 1:6){
  print("Summary dla kolumny: ")
  print(summary(loty[,i]))
  print("Wspolczynnik zmiennosci: ") #słabe zróżnicowanie liczby pasażerów
  print(sd(loty[,i])/mean(loty[,i])*100)
  hist(loty[,i], main="Wykres", breaks = przedzialy)
}

par(mfrow=c(2,3))
for (j in 1:6){
  hist(loty[,j], main=paste('Loty', nazwy[j]), breaks=przedzialy, col=kolory[j])
}

for (k in 1:6){
  boxplot(loty[,k])
}
boxplot(loty[,1], loty[,2], loty[,3], loty[,4], loty[,5], loty[,6], xlab='lata')

floor(mean(loty[,2])) #Średnia liczba pasażerów wynosiła w  1956 roku 328 osób

sd(loty[,2])

Q1

summary(loty) #kolumnami
lata = colSums(loty) #suma kolumnami
rowSums(loty) #suma wierszami

loty_t = t(loty)
loty_t
summary(loty_t)

srednia = mean(unlist(loty), na.rm=TRUE) #ze wszystkich
srednia

srednie_kol = colMeans(loty, na.rm = TRUE)
srednie_kol

srednie_wier = rowMeans(loty, na.rm = TRUE)
srednie_wier


oceny = read.csv("oceny.csv", sep=";", dec=",", head = TRUE)
class(oceny)

grupy = names(oceny)
par(mfrow=c(2,2))

mean(na.omit(oceny[,2]))
apply(na.omit(oceny), 2, mean)
apply(na.omit(oceny), 2, quantile)

for(j in 1:4){
  title=paste("hist", grupy[j])
  discrete.histogram(oceny[,j], freq=TRUE, main=title)
}

boxplot(oceny[,1], oceny[,2], oceny[,3], oceny[,4])

table(oceny[,1])
pie(table(oceny[,1]))

for (i in 1:4){
  title=paste("wykres kołowy", grupy[i])
  pie(table(oceny[,i]), main = title)
}

truskawki = read.csv("truskawki.csv", sep=";", head = TRUE)
truskawki

class(truskawki)

tr <- na.omit(truskawki, plon.2010)

max(na.omit(truskawki))
min(na.omit(truskawki))

par(mfrow=c(1,2))
nazwy = names(truskawki)
truskawki

par(mfrow=c(1,2))
for (j in 1:2){
  title=paste("histogram", nazwy[j])
  hist(truskawki[,j], breaks=ceiling(sqrt(length(truskawki[,j]))), freq=FALSE, main=title)
}

breaks <- seq(0, 1000, by=100)
truskawki$plon.2000_przedzialy <- cut(truskawki$plon.2000, breaks, include.lowest=TRUE)


summary(na.omit(truskawki[,2]))
summary(truskawki[,2])

k <- ceiling(sqrt(length(truskawki$plon.2000)))
data <- cut(truskawki$plon.2000, breaks=k)
table(data)
pie(table(data), main = 'plon 2000')

par(mfrow=c(1,2))
boxplot(truskawki[,1], truskawki[,2])
