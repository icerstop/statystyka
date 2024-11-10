dane = read.csv("dane_hip.csv", sep=";", dec=",", head=TRUE)

#Zadanie 1
wiatr = na.omit(dane$wiatr)

#H0: mu <= 4
#H1: mu > 4
alpha = 0.05
mu_0 = 4
test_result = t.test(wiatr, alternative="greater", mu=mu_0, conf.level=1-alpha)
test_result

#t=2.4186
#p-value=0.01705 < alpha=0.05 -> jest podstawa do odrzucenia H0
#Na poziomie istotności 0.05 dane potwierdzają
#hipotezę, że okolice Darłowa nadają się do budowy
#elektrowni wiatrowych.

#Zadanie 2
pompa = na.omit(dane$pompa)

#H0: mu >= 3.5
#H1: mu < 3.5
alpha = 0.01
mu_0 = 3.5
test_result = t.test(pompa, alternative="less", mu=mu_0, conf.level=1-alpha)
test_result

#t=-1.0898
#p-value=0.1521 > alpha=0.01 -> nie ma podstaw do odrzucenia H0
#Na poziomie istotności 0.01 dane nie potwierdzają
#hipotezy, że średni współczynnik COP jest
#istotnie statystycznie mniejszy niż 3.5

#Zadanie 3
morze = na.omit(dane$morze)

#H0: mu = 870
#H1: mu != 870
alpha = 0.05
mu_0 = 870
sigma_0 = 5
test_result = z.test(morze, alternative="two.sided", mu=mu_0, sd=sigma_0, conf.level=1-alpha)
test_result

#z=-0.44721
#p-value=0.6547 > alpha=0.01 -> nie ma podstaw do odrzucenia H0
#Na poziomie istotności 0.01 dane nie potwierdzają
#hipotezy, że średnia głębokość jest różna od 870 metrów.

#Zadanie 4
blaszki = na.omit(dane$blaszki)

#H0: mu <= 0.04
#H1: mu > 0.04
alpha = 0.02
mu_0 = 0.04
test_result = z.test(blaszki, alternative="greater", mu=mu_0, sd=sd(blaszki), conf.level=1-alpha)
test_result

#z=1.6409
#p-value=0.05041 > alpha=0.02 -> nie ma podstaw do odrzucenia H0
#Na poziomie istotności 0.02 dane nie potwierdzają
#hipotezy, że produkowane przez automat blaszki
#są grubsze niż 0.04 mm.

#Zadanie 5
mleko = na.omit(dane$mleko)
alpha = 0.05

#(a)

#H0: mu = 1.7%
#H1: mu != 1.7!

mu_0 = 1.7
test_result = t.test(mleko, alternative="two.sided", mu=mu_0, conf.level=1-alpha)

#t=-1.765
#p-value = 0.1114 > alpha=0.05 -> nie ma podstaw do odrzucenia H0
#Na poziomie istotności 0.05 dane nie potwierdzają 
#hipotezy, że średnia zawartość tłuszczu w mleku
#nie wynosi 1.7%.

#(b)
#H0: var >= 0.02
#H1: var < 0.02

sigmasq_0 = 0.02
test_result = sigma.test(mleko, sigmasq=sigmasq_0, alternative="less")
test_result

#X-squared = 5.2
#p-val = 0.1835 > alpha=0.05 -> nie ma podstaw do odrzucenia H0
#Na poziomie istotności 0.05 dane nie potwierdzają 
#hipotezy, że wariancja zawartości tłuszczu
#jest mniejsza niż 0.02%

#Zadanie 6

dane = read.csv("dane_hip.csv", sep=";", dec=",", head=TRUE)
kukulki = na.omit(dane$kukulki)
alpha = 0.05

sd = 2.5
mu = 17
var = sd^2

mean(kukulki)

#(a)

#(i)
#H0: mu = 17
#H1: mu != 17

t.test(kukulki, mu=mu, alternative="two.sided", conf.level=1-alpha)

#p-value < alfa, więc odrzucamy H0

#(ii)

#H0: var = 6.25
#H1: var != 6.25

sigma.test(kukulki, sigmasq = var, alternative = "two.sided", conf.level = 1-alpha)

#p-value > alpha, więc nie mamy podstaw do odrzucenia H0


#(b)

t.test(kukulki, conf.level=1-alpha)$conf.int

#Średnia (17) nie zawiera się w przedziale ufności, zatem odrzucamy hipotezę H0


#zad 7

mu = 55
sd = 18
n = 100

srednia = 60
odchylenie = 20

alfa = 0.01

#H0: mu <= 55
#H1: mu > 55

zsum.test(srednia, odchylenie, n, mu=mu, alternative="greater", conf.level=1-alfa)

#p-value < alfa, zatem odrzucamy H0

#zad 8

n = 2500
p = 1600
alfa = 0.05
p_0 = 0.6

#H0: p = 0.6
#H1: p != 0.6

binom.test(p, n, p=p_0, alternative="two.sided", conf.level=1-alfa)

#p-value mniejsze of alfy, zatem odrzucamy H0

p_hat = 1600/2500

Z = (p_hat - p_0)/(sqrt(p_0*(1-p_0))/sqrt(n))

left = -qnorm(1-alfa/2)
right = qnorm(1-alfa/2)

#Ponieważ abs(Z) > qnorm(1-alfa/2), zatem zawiera się w obszarze krytycznym, a co za tym idzie
#odrzucamy hipotezę H0

#zad 9

p_0 = 0.02
n = 1200
T = 16
p_hat = T/n
alfa = 0.05

z = qnorm(1-alfa) #1.64485
Z = (p_hat - p_0)/(sqrt(p_0*(1-p_0))/sqrt(n)) #-1.6495

#Z < -qnorm(1-alfa), zatem odrzucamy H0

binom.test(T, n, p=p_0, alternative="less", conf.level = 1-alfa)
#p-value > alfa -> brak podstaw do odrzucenia H0

#zad 10

#H0: p <= 0.9
#H1: p > 0.9
n = 1100
T = 1000
p_hat = T/n
alfa= 0.05
p_0 = 0.9

binom.test(T, n, p=p_0, alternative="greater", conf.level=1-alfa)

#alfa < p-value -> brak podstaw do odrzucenia H0

qnorm(1-alfa/2)
Z = (p_hat - p_0)/(sqrt(p_0*(1-p_0))/sqrt(n)) #1.005 -> poza obszarem krytycznym, więc brak podstaw
#do odrzucenia H0
