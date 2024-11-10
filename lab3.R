#1 - rozkład dwumianowy B(n=5, p=0.3)

k <- c(0:5)  # Możliwe wartości S
probabilities <- dbinom(k, size = 5, prob = 0.3)  # Prawdopodobieństwa dla tych wartości


rbind(k, probabilities)
# Rysowanie wykresu
plot(k, probabilities, type = "h", lwd = 2, col = "blue", main = "Rozkład prawdopodobieństwa S", xlab = "Liczba zanieczyszczonych studni", ylab = "Prawdopodobieństwo", ylim = c(0, max(probabilities) * 1.2))
points(k, probabilities, pch = 19, col = "red")

# (i) Prawdopodobieństwo, że dokładnie 3 studnie są zanieczyszczone
P_3 <- dbinom(3, size = 5, prob = 0.3)

# (ii) Prawdopodobieństwo, że co najmniej 3 studnie są zanieczyszczone
# Można to obliczyć jako dopełnienie prawdopodobieństwa, że mniej niż 3 studnie są zanieczyszczone
P_at_least_3 <- 1 - pbinom(2, size = 5, prob = 0.3) #1 - dystrybuanta dla X=2

# (iii) Prawdopodobieństwo, że mniej niż 3 studnie są zanieczyszczone
P_less_than_3 <- pbinom(2, size = 5, prob = 0.3)

#2 - rozkład dwumianowy B(n=8, p=0.9)
x=c(0:8)

# (a) P(B=8)
p_b_8 <- dbinom(8, size = 8, prob = 0.9)
p_all <- dbinom(x, 8, 0.9)
rbind(x, p_all)

# (b) P(B=7)
p_b_7 <- dbinom(7, size = 8, prob = 0.9)

# (c) P(B>5)
p_b_5 <- 1 - dbinom(5, size = 8, prob = 0.9)

# (d) E(B) - wartość oczekiwana rozkładu dwumianowego: n*p
e_b <- 8 * 0.9 #średnia liczba żarówek z 8 wylosowanych, która będzie świecić przez 500 godzin
#przeciętnie możemy spodziewać się n*p żarówek przekraczających żywotność 500 godzin
e_b <- sum(x*p_all) #inna wersja
# (e) SD(B) - odchylenie standardowe rozkładu dwumianowego: sqrt(n*p*(1-p)) - 
sd_b <- sqrt(8 * 0.9 * (1 - 0.9)) #jak bardzo liczba żarówek, które będą świecić dłużej niż 500 godzin, może różnić się od średniej
#możemy spodziewać się, że przeciętnie żywotność powyżej 500 godzin będzie odchylała się od średniej o 1 żarówkę

#3 - rozkład wykładniczy
lambda <- 0.01 #parametr określający tempo występowania zdarzenia

curve(dexp(x, rate = lambda), from = 0, to = 600, xlab = "Dni", ylab = "Gęstość",
      main = "Funkcja gęstości rozkładu wykładniczego")

# (a) Prawdopodobieństwo, że pojedyncze ogniwo przeżyje co najmniej 200 dni
p_a <- 1 - pexp(200, rate = lambda)

# (b) Prawdopodobieństwo, że pojedyncze ogniwo ulegnie awarii przed upływem 100 dni
p_b <- pexp(100, rate = lambda)

# (c) Prawdopodobieństwo, że pojedyncze ogniwo ulegnie awarii przed upływem 500 dni
p_c <- pexp(500, rate = lambda)

#4 - rozkład wykładniczy
lambda <- 1/2.4 #ponieważ lambda jest odwrotnością średniej w rozkładzie wykładniczym

curve(dexp(x, rate=lambda), from=0, to=10, main="Funkcja gęstości rozkładu wykładniczego", xlab="Stopnie w skali Richtera", ylab="Gęstość")

#trzęsienie ziemi większe niż 3 w skali richtera
p_gt_3 <- 1 - pexp(3, rate=lambda)

#trzęsienie ziemi w przedziale 2-3
p_23 <- pexp(3, rate=lambda) - pexp(2, rate=lambda)

#sprawdzenie za pomocą całki
f <- function(x) x*lambda*exp(-lambda*x)
result <- integrate(f, lower=0, upper = Inf)
result$value

#5 - rozkład normalny

avg <- 0.13
sigma <- 0.005

curve(dnorm(x, mean=avg, sd=sigma), from=avg - 4 * sigma, to=avg + 4 * sigma, main="Funkcja gęstości rezystancji przewodów", xlab="Rezystancja (omy)", ylab="Gęstość")

# Dolny i górny limit rezystancji
dolny_limit <- 0.12
gorny_limit <- 0.14

# Obliczanie prawdopodobieństwa spełnienia wymagań
p_ohm <- pnorm(gorny_limit, mean=avg, sd=sigma) - pnorm(dolny_limit, mean=avg, sd=sigma)
p_ohm

#6 - rozkład normalny (zmieniamy godziny na minuty)
e_6 = 120
sd_6  = 15

curve(dnorm(x, e_6, sd_6), from = e_6 - 4*sd_6, to = e_6 + 4 * sd_6)

p_151 <- pnorm(135, e_6, sd_6) - pnorm(111, e_6, sd_6)

#7 - rozkład normalny
e_7 = 46.8
sd_7 = 1.75
# (a) wynosi co najwyżej 50km/h

pr7_a = pnorm(50, e_7, sd_7)

# (b) wynosi co najmniej 48km/h
pr7_b = 1 - pnorm(48, e_7, sd_7)


#8 - rozkład dokładny zmiennej losowej = rozkład dwumianowy, który możemy przybliżyć za pomocą rozkładu normalnego z powodu dużej próby
# próba: 100, 

n = 100
p = 0.25

#P(X<=15)
pbinom(15, n, p)
#przybliżony rozkład normalny (aproksymacyjny)
pnorm(15, n*p, sqrt(n*p*(1-p)))

#9 
mu = 200
sigma = 10
n = 25
# (a)
#avR ma rozkład N(mu, sig/sqrt(n))
#P(199<avR<202)

p_9_a = pnorm(202, mu, sigma/sqrt(n)) - pnorm(199, mu, sigma/sqrt(n))

# (b)

# T = X1 + X2 + ... + Xn ~`N(np*mu sqrt(n)*sigma) -> tj. rozkład T

#P(T<=5100)
p_9_b = pnorm(5100, n*mu, sqrt(n)*sigma)

#10 - znów używamy przybliżenia za pomocą rozkładu normalnego
#avR ma rozkład N(mu, sig/sqrt(n))
#P(198<avR<206)

mu = 202
sigma = 14
n = 64

p_10 = pnorm(206, mu, sigma/sqrt(n)) - pnorm(198, mu, sigma/sqrt(n))

#11 

mu = 0.5
sigma = 0.2
n = 100
e = mu*n

p_11 = 1 - pnorm(47, e, sqrt(n)*sigma)



time = c(8, 12, 26, 10, 23, 21, 16, 22, 18, 17, 36, 9)
mean(time)