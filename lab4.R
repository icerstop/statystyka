p = 0.25
n = 1000
T = 232
pnorm(T/n, p, sqrt(p*(1-p)/n))

dane = read.csv("dane_est.csv", sep = ";", dec=",")

srednia = mean(diamenty)
alfa = 0.05


diamenty = na.omit(dane$diamenty)
mu_est = function(srednia, ufnosc, liczebnosc, odchylenie)
n = length(diamenty)
liczebnosc = n
ufnosc = alfa
odchylenie = sd(diamenty)
z = qnorm(1-alfa/2)


przedzial_ufnosci <- function(dane, poziom_ufnosci){
  n <- length(dane)
  s <- sd(dane)
  srednia <- mean(dane)
  blad_standardowy <- s/sqrt(n)
  t_wartosc <- qt((1+poziom_ufnosci)/2, df=n-1)
  dolny_limit <- srednia - t_wartosc*blad_standardowy
  gorny_limit <- srednia + t_wartosc*blad_standardowy
  return(c(dolny_limit, gorny_limit))
}

przedzial_ufnosci(diamenty, 0.95)
t.test(diamenty, conf.level = 0.95)


szacowanie = function(diamenty, sigma = NULL) {
  n <- length(diamenty)
  srednia <- mean(diamenty)

  
  if(!is.null(sigma)){
    
  
  if(n < 30){
    t <- qt(1-alfa/2, df = n-1)
    error <- t*(sd/sqrt(n))
  }
  else{
    z <- qnorm(1-alfa/2)
    error <- z*(sd/sqrt(n))
  }
  
  }
  dol <- srednia - error
  gora <- srednia + error
  
  return(c(dol, gora))
}

przedzial <- szacowanie(diamenty)

przedzial

# Dla wariancji (przykładowy współczynnik ufności 95%)
n <- length(diamenty)
wariancja <- var(diamenty)
chi_dolne <- qchisq(0.025, df = n-1)
chi_gorne <- qchisq(0.975, df = n-1)
dolny_limit_var <- ((n-1) * wariancja) / chi_gorne
gorny_limit_var <- ((n-1) * wariancja) / chi_dolne

# Dla odchylenia standardowego, bierzemy pierwiastek kwadratowy z wartości dla wariancji
dolny_limit_sd <- sqrt(dolny_limit_var)
gorny_limit_sd <- sqrt(gorny_limit_var)

# Wyniki
dolny_limit_var
gorny_limit_var
dolny_limit_sd
gorny_limit_sd

#Zadanie 2
#Populacja - wszystkie kobiety karmiące piersią
#Próbka - 20 kobiet karmiących piersią
#Badana zmienna - poziom PCB

mleko = dane$mleko
mean(mleko)



# Przedział ufności dla średniej
n <- length(mleko)
srednia <- mean(mleko)
sd <- sd(mleko)
blad_standardowy <- sd / sqrt(n)
t_wartosc <- qt(0.975, df = n-1) # Dla 95% przedziału ufności

przedzialufnosci(mean(mleko), sd(mleko), FALSE, n, 0.95)
t.test(mleko, conf.level=0.95)
#Na poziomie ufności 95% przedział (3.42; 8.18) pokrywwa rzeczywistą nieznaną 
#średnią zawartość PCB w mleku WSZYSTKICH matek karmiących piersią

dolny_limit <- srednia - t_wartosc * blad_standardowy
gorny_limit <- srednia + t_wartosc * blad_standardowy

dolny_limit
gorny_limit

# Przedział ufności dla wariancji i odchylenia standardowego
chi_dolny <- qchisq(0.025, df = n-1)
chi_gorny <- qchisq(0.975, df = n-1)

dolny_limit_var <- ((n-1) * var(mleko)) / chi_gorny
gorny_limit_var <- ((n-1) * var(mleko)) / chi_dolny

# Przeliczenie na odchylenie standardowe
dolny_limit_sd <- sqrt(dolny_limit_var)
gorny_limit_sd <- sqrt(gorny_limit_var)

dolny_limit_var
gorny_limit_var
dolny_limit_sd
gorny_limit_sd

a = sigma.test(mleko, conf.level = 0.95)$conf.int
sqrt(a)

#Z ufnośćią 95% przedział (3.86; 7.43) pokrywa rzeczywistą nieznaną
#wartość odchylenia standardowego zawartości PCB w mleku WSZYSTKICH matek
#karmiących piersią 

#Zadanie 3

# Dane
papierosy <- na.omit(dane$papierosy)
sigma <- 0.7

# Średnia
srednia <- mean(papierosy)

# Rozmiar próbki
n <- length(papierosy)

# Błąd standardowy
blad_standardowy <- sigma / sqrt(n)

# Wartości krytyczne dla normalnego rozkładu (z) dla 95% przedziału ufności
z <- qnorm(0.975)

# Granice przedziału ufności
dolny_limit <- srednia - z * blad_standardowy
gorny_limit <- srednia + z * blad_standardowy

dolny_limit
gorny_limit

z.test(papierosy, sigma.x=0.7, conf.level=0.95)

#Z ufnością 95% przedział (1,45; 2.17) pokrywa rzeczywistą nieznaną
#średnią zawartość nikotyny we WSZYSTKICH papierosach nowej marki

L <- 0.3
n <- ceiling((2*z*sigma/L)^2)
n

#84 

sd(papierosy)
sigma.test(papierosy, sigma.x=0.7, conf.level=0.95)

#Zadanie 4

wodorosty <- na.omit(dane$wodorosty)

sd(wodorosty)
var(wodorosty)
mean(wodorosty)

t.test(wodorosty, conf.level=0.90)
sigma.test(wodorosty, conf.level=0.90)


sygnaly <- c(17, 21, 20, 18, 19, 22, 20, 21, 16, 19)
t.test(sygnaly, conf.level = 0.95)
z.test(sygnaly, sigma.x=3, conf.level = 0.95)


n = 1200
mu = 4.7
sigma = 2.2
alfa = 0.05
Z = qnorm(1-alfa/2)

error <- Z*sigma/sqrt(n)

przedzial <- c(mu-error, mu+error)
przedzial



zsum.test(mu, sigma, n, conf.level=0.95)

#Zad 6

# Dane
srednia <- 4.7  # średnia próby
odch_std <- 2.2  # odchylenie standardowe próby
n <- 1200  # liczba obserwacji
alpha <- 0.05  # poziom istotności dla 95% ufności

# A) Średnia długość trwania wszystkich połączeń
Z <- qnorm(1 - alpha / 2)
margin_error <- Z * (odch_std / sqrt(n))
lower_mean <- srednia - margin_error
upper_mean <- srednia + margin_error

# B) Odchylenie standardowe wszystkich połączeń
chi2_lower <- qchisq(alpha / 2, n - 1)
chi2_upper <- qchisq(1 - alpha / 2, n - 1)
lower_sd <- sqrt((n - 1) * odch_std^2 / chi2_upper)
upper_sd <- sqrt((n - 1) * odch_std^2 / chi2_lower)

# Wyniki
cat("A) Średnia długość trwania wszystkich połączeń wynosi od", lower_mean, "do", upper_mean, "minut\n")
cat("B) Odchylenie standardowe wszystkich połączeń wynosi od", lower_sd, "do", upper_sd, "minut\n")

#Zad 7
n = 365
mu = 102
v = 81
s = sqrt(v)

zsum.test(mu, s, n, conf.level = 0.98)

z <- qnorm(0.99) #zarówno średnia i odchylenie nie są znane - obliczone na podstawie dużej próbki
error <- z*(s/sqrt(n))
wynik <- c(mu-error, mu+error)
wynik

#Zadanie 8
v = 25
s = sqrt(v)
e = 1
z = qnorm(0.975)
#e=z*s/sqrt(n)
#sqrt(n)=(z*s)/e
#n = (z^2*s^2)/e^2

n = ceiling((z*s/e)^2)
n

#Zadanie 9
s = 0.3
z_90 = qnorm(0.95)
e = 0.1
n = ceiling((z_90*s/e)^2)
n

z_99 = qnorm(0.995)
n = ceiling((z_99*s/e)^2)
n

#Zadanie 10
x <- 4
n <- 100
conf.level = 0.95
alpha = 1 - conf.level

funkcja <- function(x, n, conf.level){
  p_hat <- x/n
  alpha <- 1 - conf.level
  Z <- qnorm(1-alpha/2)
  error <- Z*sqrt(p_hat*(1-p_hat)/n)
  lower <- p_hat - error
  upper <- p_hat + error
  return (c(lower, upper))
}

funkcja(x, n, conf.level)

binom.test(x, n, p=0.5, conf.level=0.95)
prop.test(x, n, p=0.5, conf.level=0.95)

#Zadanie 11
n = 120
x = 24
p_hat = x/n
z = qnorm(0.95) 
se = sqrt(p_hat*(1-p_hat)/n)

lower = p_hat - z*se
upper = p_hat + z*se
cat(c(lower, upper))

#Zadanie 12
E = 0.05
z = qnorm(0.99)

#a - znana proporcja, p = 0.3

p_a = 0.3
cat("n_a = ", n_a = ceiling((z^2*p_a*(1-p_a))/E^2))

#b - nieznana proporcja, np. p = 0.5
p_b = 0.5
cat("n_b = ", n_b = ceiling((z^2*p_b*(1-p_b))/E^2))

#Ponieważ wielkość próby potrzebna do oszacowania proporcji 
#z daną dokładnością jest odwrotnie proporcjonalna
#do wariancji proporcji (większa wariancja wymaga większej próby, 
#aby osiągnąć tę samą dokładność oszacowania), 
#największa próba będzie potrzebna, 
#gdy wariancja jest największa. 
#Dlatego przyjmujemy p = 0.5 jako najgorszy przypadek, 
#zakładając, że nie znamy dokładnej proporcji 
#p, ponieważ maksymalizuje to wymaganą wielkość próby, 
#zapewniając, że oszacowanie będzie wystarczająco dokładne
#niezależnie od rzeczywistej wartości p.