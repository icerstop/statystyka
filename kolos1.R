#KOLOKWIUM 1

#Zadanie 1

energia = na.omit(read.csv("dane.csv", sep=";", dec=",", head=TRUE)$energia)

#a
quantile(energia, 0.75)
sd(energia)

#Interpretacja: 75% wartości ze zbioru danych energia jest mniejszych od wartości 317.75 
                # 25% wartości ze zbioru danych energia jest większych od wartości 317.75
                # Średnia wartość waha się o 53.08

#b
breaks = seq(180, 420, length=6)
szereg = table(cut(energia, breaks))
szereg

#c
pie(szereg)

#Zadanie 2
#a
h = 5
lambda = 1/h
curve(dexp(x, lambda), 0, 50, xlab="Liczba rozpadów na sekundę", ylab="Gęstość prawdopodobieństwa")

#b
#P(X<=3)
pexp(3, lambda)
#P(X>=3)
1 - pexp(3, lambda)

#Zadanie 3
h = 5
sigma = 5
mu = 160+h
n = 40

#a
#Średnia próby jest przybliżona rozkładem normalnym, parametry: mu, sigma/sqrt(n)

#b P(164 <= X <= 167)
wynik = pnorm(167, mu, sigma/sqrt(n)) - pnorm(164, mu, sigma/sqrt(n))

#Zadanie 4

moc = na.omit(read.csv("dane.csv", sep=";", dec=",", head=TRUE)$moc)
t.test(moc, conf.level=0.95)

#Na poziomie ufności 95% możemy stwierdzić, że 
# średnia oczekiwana wartość uzyskanej mocy
# mieści się w przedziale [3.76; 4.31]
n = length(moc)
S = sd(moc)
error = 0.1

# error = qt(0.975, n-1)*sd(moc)/sqrt(n)
n1 = ceiling((qt(0.975, n-1)*S/error)^2)

#KOLOKWIUM 2

#1

#a
energia = na.omit(read.csv("dane.csv", sep=";", dec=",", head=TRUE)$energia)
median(energia)
quantile(energia, 0.5)

#b
przedzialy = seq(190, 400, length=5)
szereg = table(cut(energia, przedzialy))
szereg

#c
discrete.histogram(szereg, freq=FALSE)

#2 
p = 0.4
h = 5

#a
zepsute = 0:h
prawdop = dbinom(zepsute, h, p)
tabela = data.frame(x = zepsute, P_X = prawdop)
tabela

#b 
#P(X>1) => 1 - P(X<=1)
wynik = 1 - pbinom(1, h, p)

#c
e = h * p


#3
h = 5
mu = 170+h
sigma = 6
n = 50

#a
# T ~ N(mu*n, sqrt(n)*sigma)

#b
print(pnorm(8550, n*mu, sqrt(n)*sigma))

#4

n = 120
T = 23
h = 5
#Nie znamy średniej i odchylenia
zsum.test(mean(probka), sd(probka), n, conf.level=0.95)

#Asystent inżyniera przemysłowego przeprowadził 120 przypadkowych obserwacji zespołu monterów tapicerek w 
#zakładzie montażu samochodów. W 24 przypadkach zaobserwował, że pracownicy układali materiały poza 
#swoim stanowiskiem pracy (co może stwarzać niebezpieczeństwo dla innych pracowników zakładu, a więc 
#jest niezgodne z przepisami BHP). Oceń z ufnością 90% prawdziwy odsetek monterów nie przestrzegających 
#wspomnianych przepisów BHP. Zinterpretuj wynik.

#zad11
n = 120
blad = 24
alfa = 0.1
phat = 24/120
L = phat - qnorm(1-alfa/2)*sqrt(phat*(1-phat))/sqrt(n)
P = phat + qnorm(1-alfa/2)*sqrt(phat*(1-phat))/sqrt(n)
