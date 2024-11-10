liczby = c(1, 2, 3, 4 ,5, 6)
tabela = table(liczby)
pie(tabela)
ozon = read.csv("ozon.csv", sep = ";", head = TRUE)
pie(ozon)

sufit <- data.frame(sufit = ceiling(ozon$ozon))
par(mfrow=c(1,2))
plot(sufit$sufit, type = 'l', xlab = "Dni", ylab = "Ilość ozonu", main = "Ilość ozonu w poszczególnej jednostce (zaokrąglenie w górę)")
plot(ozon$ozon, type = 'l', xlab = "Dni", ylab = "Ilość ozonu", main = "Ilość ozonu w poszczególnej jednostce")
par(mfrow=c(1,1))
mean(ozon$ozon, na.rm = TRUE)


# Obliczenie średniej dla kolumny ozon
srednia_ozonu <- mean(ozon$ozon, na.rm = TRUE)

# Tworzenie wykresu
plot(ozon$ozon, type='l', xlab="Dni", ylab="Ilość ozonu", main="Ozon w poszczególnych dniach")

# Dodanie linii średniej
abline(h=srednia_ozonu, col="red", lwd=3)

# Dodanie legendy dla jasności
legend("topright", legend=paste("Średnia:", round(srednia_ozonu, 2)), col="red", lwd=2)

quantile(ozon$ozon)

summary(ozon$ozon)

grupaA = c(3.0, 3.0, 3.0, 5.0, 5.0)
grupaB = c(3.0, 3.5, 4.5, 4.5, 5.0)
boxplot(grupaA, grupaB)

sin(pi)
result <- sin(pi)
if(abs(result) < 1e-15) {
  result <- 0
}
result

#długość euklidesowa (norma)
euklides <- sqrt(sum(x^2))
euklides

x_t = t(x)
y_t = t(y)

x = seq(2, 20, 2)
y = rev(x)
#x_t*y
x_t_y <- t(x)%*%y
x_t_y
#x*y_t
x_y_t <- x%*%t(y)
x_y_t

z = seq(5, 10, length=13)

z1 <- rep(c(1,2), each = 5)
z1
z2 <- rep(c(1,2), times = 5)
z2

z1 + 4

z3 <- z2[-length(z2)]
z3

c = z1 + z3
c
#różne długości wektorów, dodanie niemożliwe

z1_1 <- z1[z1 > 1]
z1_1

#macierz
A <- matrix(c(2, 3, 0, 1, -1, 2, 1, 1, -1), nrow=3, ncol=3, byrow=TRUE) 
A

A*A
A%*%A

t(A)
det(A)
solve(A)

b = A[3,]
b

#a i b zamiast x i y
a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
b = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

plot(a, b, main="Wykres punktowy", xlab = "a", ylab="b", pch = 19)

df <- data.frame(a, b)
df
plot(df, main = "wykres data frame", xlab = "a", ylab="b", pch = 5)

cbind_df <- cbind(a, b)
plot(cbind_df[, "a"], cbind_df[, "b"], main="Wykres cbind", xlab = "a", ylab = "b", pch = 10, col="red")
rbind_df <- rbind(a, b)

plot(1:10, rbind_df[1,], main="Wykres rbind(a)", xlab="indeks", ylab="a", pch=11, col="blue")
plot(1:10, rbind_df[2,], main="Wykres rbind(b)", xlab="indeks", ylab="b", pch=12, col="pink")

curve(x^2 + 3*x - 5, from = -3, to = 4, xlab = "x", ylab = "y", zlab = "f(x,y)", main = "Wykres funkcji y = f(x)")

x <- seq(-10, 10, length = 50)
y <- seq(-10, 10, length = 50)

grid <- expand.grid(x=x, y=y)

z <- matrix(sin(grid$x)+(cos(grid$x)+sin(grid$y)+cos(grid$y)), nrow = 50, ncol = 50)

colors <- colorRampPalette(c("green", "yellow", "orange", "red"))(100)
zlim <- range(z)
zcol <- colors[cut(z, breaks = 100)]


persp(x, y, z, theta = 0, phi = 90, expand = 0.5, col = zcol, zlim = zlim, xlab = "x", ylab = "y", zlab = "f(x,y)")

dane = read.csv("ozon.csv", sep=";")

oz = dane$ozon


length(oz)
min(oz)
max(oz)


przedzialy=seq(0, 12, length=8)
hist(oz, breaks=przedzialy)

ceiling(oz)
floor(oz)

sort(oz)

summary(dane$ozon)

a = c(4, 5, 7, 100, 101)
b = c(21, 30, 37, 88)

quantile(b)

quantile(a)

summary(a)


A=c(3,3,4,4.5,4.5)
#dominanta={3, 4.5}
B=c(2,3.5,4,4.5,5)
#dominanty brak
mean(A)
quantile(A)

var(A)
sd(A)
R=max(A)-min(A)
RQ=quantile(A, prob=0.75)-quantile(A, prob=0.25)
v=sd(A)/mean(A)*100

par(mfrow=c(1,2))
boxplot(A)
boxplot(B)
boxplot(A,B)


dbinom(0, 3, 0.6)
dbinom(2, 3, 0.6)+dbinom(3, 3 ,0.6)
1-pbinom(1,3, 0.6)

x=seq(0,3)
p=c(dbinom(0, 3, 0.6), dbinom(1, 3, 0.6), dbinom(2, 3, 0.6), dbinom(3, 3, 0.6))
sum(p)

rozklad=rbind(x,p)
rozklad

expect = 0
for (i in 1:4){
  expect=expect+rozklad[1,i]*rozklad[2,i]
}
print("expect")
print(expect)
getwd()