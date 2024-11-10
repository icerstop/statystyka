#######################################
############## HIPOTEZY ###############
#######################################

# 1. Aby mogły pracować urządzenia prądotwórcze elektrowni wiatrowej średnia 
# prędkość wiatru powinna przekraczać 4 m/s. W celu stwierdzenia czy sensowna
# jest budowa elektrowni wiatrowej mierzono przez okres roku każdego miesiąca 
# przeciętną prędkość wiatru w okolicach Darłowa uzyskując wyniki (w m/s): 
# 5,9  4,4 5,4 3,8 4,0 4,2 3,4 3,6 4,6 6,5 5,6 4,8. 
# Zakładając, że prędkość wiatru jest zmienną losową o rozkładzie normalnym 
# oraz przyjmując poziom istotności α=0,05 sprawdź, czy okolice Darłowa 
# nadają się do budowy elektrowni wiatrowych. W tym celu skonstruuj odpowiednią 
# procedurę testującą. Porównaj otrzymane wyniki z uzyskanymi po zastosowaniu
# odpowiedniej funkcji z pakietu R.    


dane = read.csv("dane_hip.csv", sep=";", dec=",", head=TRUE)

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

# 2. Przyjęto, że współczynnik efektywności pompy cieplnej COP jest zadawalający, 
# gdy jego średnia wartość wynosi co najmniej 3,5 (co oznacza, 
# że ponad 70% dostarczonego przez pompę ciepła pochodzi z naturalnego 
# źródła ciepła, a reszta pochodzi z pracy sprężarki). 
# Potencjalny nabywca pompy ma wątpliwości i wysunął przepuszczenie, 
# że współczynnik efektywności pompy cieplnej w jego gospodarstwie domowym 
# jest znacznie mniejszy niż 3,5. Przez pewien okres mierzono współczynnik COP 
# w tym gospodarstwie otrzymując następujące wyniki:  
# 3,5 3,2 3,6 3,0 3,3 3,8 2,5 3,0 3,7 3,9. 
# Zakładając, że zmienna opisująca wartości współczynnika COP jest zmienną losową 
# o rozkładzie normalnym i na podstawie powyższych wyników 
# (przyjmując poziom istotności α=0,01) sprawdź, czy wątpliwości nabywcy są słuszne.  

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

# 3. Wiadomo, że rozkład wyników pomiarów głębokości morza w pewnym rejonie 
# jest normalny z odchyleniem standardowym 5 m. Dokonano 5 niezależnych pomiarów
# głębokości morza w pewnym rejonie i otrzymano następujące wyniki (w m):  
# 862 870 876 866 871. Przyjmując poziom istotności α=0,05 zweryfikuj hipotezę, 
# że średnia głębokość morza w tym rejonie jest różna od 870 m.  

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


# 4. Automat produkuje blaszki określonych wymiarów o nominalnej grubości 0,04 mm. 
# Wylosowana próba 40 blaszek dała następujące wyniki: 
# 0,048   0,028   0,037   0,033   0,054   0,046   0,041   0,043   0,044   0,05   0,047   0,052   
# 0,053   0,048 0,027   0,056   0,058   0,039   0,026   0,034   0,043   0,042   0,047   0,022   
# 0,046   0,04   0,036 0,043   0,041   0,044   0,043   0,044   0,038   0,046   0,041   0,038   
# 0,047   0,03   0,041   0,049. 
# Przyjmując poziom istotności α=0,02 sprawdź poprawność twierdzenia, że produkowane przez ten 
# automat blaszki są grubsze niż nominalna grubość.  

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

# 5. W próbce laboratoryjnej mleka spożywczego wykonano 10 oznaczeń (w %) 
# zawartości tłuszczu i uzyskano:  1,5  1,8 1,5 1,7 1,6 1,6 1,8 1,6 1,7 1,6. 
# Przyjmując, że rozkład zawartości tłuszczu w mleku spożywczym jest normalny, 
# odpowiedź na poniższe pytania (przyjmij poziom istotności α = 0,05): 

# (a) Czy obserwacje przeczą hipotezie, że średnia zawartość tłuszczu w mleku wynosi 1,7 %? 

#H0: mu = 1.7%
#H1: mu != 1.7!

mu_0 = 1.7
test_result = t.test(mleko, alternative="two.sided", mu=mu_0, conf.level=1-alpha)

#t=-1.765
#p-value = 0.1114 > alpha=0.05 -> nie ma podstaw do odrzucenia H0
#Na poziomie istotności 0.05 dane nie potwierdzają 
#hipotezy, że średnia zawartość tłuszczu w mleku
#nie wynosi 1.7%.


# (b) Czy można twierdzić, że wariancja zawartości tłuszczu w mleku jest mniejsza niż 0,02 (%)

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


# 6. Kukułki podrzucają swoje jaja różnym ptakom, między innymi małym strzyżykom.
# Obserwacje przyrodników wskazują, że kukułka potrafi znieść jajo wielkości podobnej
# do jaj „adopcyjnych rodziców”. Zmierzono długość [w mm] 21 jaj podrzuconych strzyżykom
# otrzymując wyniki: 17,93 19,82 16,74 18,52 21,40 14,93 19,66 16,54 18,56 14,30 18,64 15,43 
# 17,52 17,62 15,19 20,76 20,79 21,05 20,26 19,14 20,79. 
# Wiadomo, że średnia długość jaj strzyżyka wynosi 17 mm, a odchylenie standardowe 2,5 mm. 
# Zakładamy, że badana cecha ma rozkład normalny. 

dane = read.csv("dane_hip.csv", sep=";", dec=",", head=TRUE)
kukulki = na.omit(dane$kukulki)
alpha = 0.05

sd = 2.5
mu = 17
var = sd^2

mean(kukulki)

# (a) Na poziomie istotności 0,05 zweryfikuj stawianą przez przyrodników hipotezę dotyczącą: 

# (i) średniej długości podrzuconych jaj;            

t.test(kukulki, mu=mu, alternative="two.sided", conf.level=1-alpha)

#p-value < alfa, więc odrzucamy H0

# (ii) wariancji długości podrzuconych jaj.

#H0: var = 6.25
#H1: var != 6.25

sigma.test(kukulki, sigmasq = var, alternative = "two.sided", conf.level = 1-alpha)

#p-value > alpha, więc nie mamy podstaw do odrzucenia H0


# (b) Zbuduj 95% przedział ufności dla średniej długości jaj podrzucanych strzyżykom. 
# Jaki jest związek między skonstruowanym przedziałem ufności a decyzją podjętą przy testowaniu hipotez?

t.test(kukulki, conf.level=1-alpha)$conf.int

#Średnia (17) nie zawiera się w przedziale ufności, zatem odrzucamy hipotezę H0

# 7. Agencja Ochrony Środowiska ustaliła dopuszczalne średnie zanieczyszczenie
# na terenach przemysłowych jako 55 miligramów na m3, (w promieniu 2 km od fabryki), 
# przy odchyleniu standardowym 18 (miligramów na m3). Ekolog zmierzył poziom 
# zanieczyszczeń na terenie przemysłowym 100 razy, w różnych dniach i nocach, 
# a następnie obliczył średnią i odchylenie standardowe pomiarów: 
# odpowiednio 60 i 20 miligramów na m3. Przyjmując poziom istotności 0,01, zweryfikuj, 
# czy dane potwierdzają, że fabryka działa niezgodnie z prawem (w celu weryfikacji 
# hipotezy dotyczącej wariancji należy przyjąć normalność poziomu zanieczyszczeń). 

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

# 8. Sondaż opinii publicznej na temat frekwencji oczekiwanej na wyborach wykazał, 
# że w losowo wybranej grupie 2500 osób 1600 zamierza uczestniczyć w głosowaniu. 
# Czy na poziomie istotności równym 0,05 próba przeczy twierdzeniu, że 60% ogółu 
# osób zamierza wziąć udział w wyborach? Skonstruuj odpowiednią procedurę testującą. 
# Porównaj otrzymane wyniki z uzyskanymi po zastosowaniu testu dokładnego dostępnego w pakiecie R. 

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


# 9. Przeprowadzono badanie jakości jaj kurzych pochodzących z pewnej fermy. 
# Zakłada się z góry, że 2% jaj jest złej jakości. Wylosowano 1200 jaj do 
# zbadania i wśród nich 16 okazało się złej jakości. Czy obserwacje przeczą 
# przyjętemu założeniu i potwierdzają, że frakcja ta w badanej fermie jest mniejsza? 
# Wnioskuj na poziomie istotności 0,05. W tym celu skonstruuj odpowiednią 
# procedurę testującą. Porównaj otrzymane wyniki z uzyskanymi po zastosowaniu 
# testu dokładnego dostępnego w pakiecie R. 

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

# 10.  W sondażu przeprowadzonym przez pracownię badania opinii społecznych
# wśród 1100 dorosłych Polaków, 1000 ankietowanych odpowiedziało, 
# że w ubiegłym miesiącu nie przeczytało żadnej książki. Pozostali 
# twierdzili, że przeczytali przynajmniej jedną książkę. 
# Na podstawie tych danych, na poziomie 0,05, stwierdzić, czy opinia, 
# że procent Polaków, którzy nie przeczytali żadnej książki jest w
# iększy niż 90 jest uzasadniona? 

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

#######################################
########### DWIE POPULACJE ############
#######################################

# 1. Badano zawartość procentową celulozy w drewnie pewnego gatunku pochodzącego z dwóch 
# różnych regionów Polski. Dla regionu I poddano analizie 8 próbek drewna natomiast
# dla regionu II przebadano 21 próbek drewna. Otrzymane wyniki zapisane zostały 
# w pliku DwiePopulacje.csv. Przyjmując normalność rozkładu zawartości celulozy 
# w drewnie i poziom istotności 0,02: 

dane = read.csv("DwiePopulacje.csv", sep=";", dec=".", head=TRUE)

reg1=na.omit(dane$cel1)
reg2=na.omit(dane$cel2)
n1=length(reg1)
n2=length(reg2)
x_bar1=mean(reg1)
x_bar2=mean(reg2)
var1=var(reg1)
var2=var(reg2)

# (a) zweryfikuj hipotezę, że przeciętna zawartość celulozy dla regionu I 
# różni się istotnie od przeciętnej zawartości celulozy dla regionu II. 
# Przyjmij jednorodność wariancji populacji i normalność rozkładu badanej cechy;

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

# (b) sprawdź, czy założenie o równości wariancji było słuszne 

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


# (c) oceń metodą przedziałową, ze współczynnikiem ufności 0,98, 
# różnicę średnich. Zinterpretuj wynik i na jego podstawie 
# podejmij decyzję dotyczącą hipotezy z punktu (a). 

t.test(reg1, reg2, var.equal=TRUE, conf.level=0.98)
#Na poziomie ufności 98% przedział
#(-13.52; 3.15) pokrywa nieznaną
#prawdziwą różnicę średnich zawartości
#celulozy w dwóch regionach.
#Ponieważ przedział (-13.52; 3.15) pokrywa 
#wartość 0, zatem nie mamy podstaw
#do odrzucenia H0.


# 2. W budownictwie mieszkaniowym założono, że czas budowy metodą 
# tradycyjną jest dłuższy niż czas budowy nową technologią. 
# Wylosowano 10 obiektów wybudowanych metodą tradycyjną oraz 
# 11 nową metodą i otrzymane czasy budowy zapisano w pliku DwiePopulacje.csv. 
# Przyjmując, że rozkład czasu budowy zarówno metodą tradycyjną 
# jak i nową jest normalny, zweryfikuj hipotezę, że średni czas
# budowy metodą tradycyjną jest dłuższy od średniego czasu budowy nową metodą. 
# Przyjmij poziom istotności 0,1. 

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

# 3. Bank chce sprawdzić, która metoda pozyskiwania pieniędzy 
# (ze źródeł publicznych czy prywatnych) prowadzi do pozyskania 
# większego funduszu. W tym celu bank pobrał losową próbę 11 firm, 
# które zaciągnęły kredyt tylko ze źródeł publicznych oraz próbę 16 firm, 
# które zaciągnęły kredyt tylko ze źródeł prywatnych. Otrzymane wyniki 
# zapisano w pliku DwiePopulacje.csv. Zakładając, że wysokość kredytów 
# prywatnych i publicznych ma rozkład normalny czy można stwierdzić, 
# że publiczne źródła finansowania udzielają, przeciętnie rzecz biorąc, 
# mniejszych kredytów? Wnioskuj na poziomie istotności 0,1.  

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

# 4. Dla porównania regularności uzyskiwanych wyników sportowych 
# dwóch zawodników w pewnym okresie, wylosowano 12 wyników pierwszego 
# zawodnika i 9 drugiego. Otrzymano następujące rezultaty (w m): 
# dla pierwszego:    7,6;  7,81;  8,01;  7,95;  7,15;  8,06;  7,9;  7,91;  7,56;  7,62;  7,85;  8,02; 
# dla drugiego:  7,5;  7,9;  8,0;  7,17;  7,28;  7,35;  7,73;  7,2;  7,98. 
# Na poziomie istotności 0,05 zweryfikuj hipotezę o większej regularności 
# wyników pierwszego zawodnika. 

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


# 5. Testowano działanie dwóch leków przeciwbólowych, przy czym spodziewano się, 
# że pierwszy lek będzie działał dłużej. Podano 10 losowym pacjentom lek L1, 
# a innej losowej grupie 15 pacjentów skarżących się również na bóle, 
# lek L2. Otrzymane czasy działania (w godzinach) zapisano w pliku DwiePopulacje.csv. 
# Czy można twierdzić, że średni czas działania leku L1 jest istotnie dłuższy niż 
# dla leku L2? Testuj na poziomie istotności 10%. 

#H0: mu1 <= mu2
#H1: mu1 > mu2

L1 = na.omit(dane$L1)
L2 = na.omit(dane$L2)

alfa = 0.1

var.test(L1, L2, conf.level=1-alfa)
#p-value > alfa -> brak podstaw do odrzucenia H0

t.test(L1, L2, var.equal=FALSE, alternative = "greater", conf.level=1-alfa)
#p-value < alfa -> odrzucamy H0


# 6. W oparciu o badania przeprowadzone w Polsce i USA, dotyczące zadowolenia 
# z pracy, uzyskano wyniki: spośród 1200 badanych Polaków 78% potwierdziło 
# zadowolenie z pracy, natomiast spośród 2000 Amerykanów 20% było niezadowolonych. 

# (a) Porównaj za pomocą 90% przedziału ufności procent zadowolonych Polaków i Amerykanów. 
# Napisz swoją własną funkcję, a następnie porównaj wyniki z otrzymanymi po zastosowaniu funkcji R.     

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



# (b) Czy opinia, że proporcja zadowolonych Polaków jest mniejsza niż zadowolonych Amerykanów jest słuszna? 
# Testuj na poziomie istotności 0,1.     

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


# (c) Socjolodzy twierdzili, że procent Polaków zadowolonych z pracy jest większy niż 75. 
# Czy próba potwierdza to przypuszczenie? Wnioskuj na poziomie istotności 0,1.  


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


# 7. Badano czy częstość występowania malarii zależy od regionu. 
# Przypadki występowania malarii w tropikalnych regionach były następujące: 
# w Azji zanotowano 313 przypadków malarii typu A i 28 przypadków malarii typu B; 
# w Afryce zanotowano 145 przypadków malarii typu A i 56 typu B. 

afryka = c(313, 28)
azja = c(145, 56)

n1 = sum(afryka)
n2 = sum(azja)

p1a = afryka[1]/n1
p1b = afryka[2]/n1

p2a = azja[1]/n2
p2b = azja[2]/n2

phat = (afryka[1]+azja[1])/(n1+n2)

# (a) Czy częstość występowania malarii typu A zależy od regionu? 
# Przyjmij poziom istotności 0,05.


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

# (b) Oceń metodą przedziałową, przyjmując współczynnik ufności 0,95, 
# różnicę badanych częstości występowania malarii typu A. 


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



# 8. Doświadczenie ma określić efekt temperatury na przeżywalność jajeczek owadów. 
# W temperaturze 11°C przeżyły do następnego etapu rozwoju 73 z 105 jajeczek. 
# W temperaturze 30°C przetrwały102 z 110 jajeczek. 

T1 = 73
n1 = 105
T2 = 102
n2 = 110
p1 = T1/n1
p2 = T2/n2
phat = (T1+T2)/(n1+n2)
alfa = 0.05

# (a) Czy wyniki doświadczenia potwierdzają przypuszczenie, że proporcja przeżywalności 
# zależy od temperatury? Przyjmij poziom istotności 0,05.    

#H0: p1 = p2
#H1: p1 != p2
Z = (p1 - p2) / sqrt(phat*(1-phat)*(1/n1 + 1/n2)) #-4.37
z = qnorm(1-alfa/2) #1.96

#H1 two-sided, więc obszar krytyczny: (-inf, -z) U (z, +inf) -> statystyka Z w obszarze krytycznym, więc
#odrzucamy H0

prop.test(c(T1, T2), c(n1, n2), alternative = "two.sided", conf.level = 1-alfa)
#p-value = 0 < alfa = 0.05 -> odrzucamy H0

#Na poziomie istotności 0.05 dane potwierdzają hipotezę, że propocja przeżywalności zależy od temperatury.

# (b) Oceń metodą przedziałową różnicę proporcji przeżywalności w badanych temperaturach. 
# Przyjmij współczynnik ufności 0,95.  

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


# 9. Lekarz podejrzewa, że dany rodzaj leku zmienia wartości określonego parametru 
# biochemicznego. I tak, u 9 pacjentów zmierzono poziom tego parametru przed i po podaniu leku: 
# Przed: 15 4 9 9 10 10 12 17 14
# Po: 14 4 10 8 10 9 10 17 15 14 
# Zakładając poziom istotności 5%, zweryfikuj podejrzenia lekarza. 
# Załóż normalność poziomu parametru biochemicznego.

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


# 10. Zmierzono pH wody w 8 jeziorach na dwóch różnych głębokościach (15 cm i 100 cm). 
# Wyniki przedstawiono w poniższej tabeli: 
# głebokość 1 2 3 4 5 6 7 8 
# 15cm 6.55 5.98 5.59 6.17 5.92 6.18 6.43 5.68 
# 100cm 6.78 6.14 5.80 5.91 6.10 6.01 8.18 5.88 

ph_15cm = c(6.55, 5.98, 5.59, 6.17, 5.92, 6.18, 6.43, 5.68)
ph_100cm = c(6.78, 6.14, 5.80, 5.91, 6.10, 6.01, 8.18, 5.88)
d = ph_15cm - ph_100cm
alfa = 0.1

# (a) Czy pH wody zależy od głębokości? Załóż 10% istotności i normalności rozkładu pH.                                                                                        

t.test(d, conf.level = 1 - alfa)

#p-value = 0.2312 > alfa = 0.01 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 0.1 dane nie potwierdzają hipotezy, że pH zależy
#od głębokości.


# (b) Sprawdź (a) używając 90% przedziału ufności. 


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

#######################################
############### ANOVA #################
#######################################


# 1. Inżynier chemiczny chce sprawdzić, czy różne warunki ciśnieniowe mają
# wpływ na średnią produkcję pewnego wyrobu z danego typu surowca. 
# Poniższa tabela zawiera wyniki przeprowadzonego eksperymentu (w g/l): 
# Ciśnienie                   Replikacje
# Niskie 28 26 29 30 28 31 26 32 25 29 
# Średnie 30 29 30 30 28 32 29 32 28 30
# Silne  31 29 33 33 29 33 28 32 27 32 
# Bardzo silne 29 27 30 31 27 32 27 32 27 30
# Czy ciśnienie ma wpływ na wielkość produkcji? 
# Wykorzystaj funkcję pakietu R o nazwie anova. 
# Wnioskuj na poziomie istotności 0,05. Analizę wariancji poprzedź testem 
# równości wariancji, np. z wykorzystaniem funkcji bartlett.test.      

cisnienie = read.csv("anova_cisnienie.csv", sep=";")
alfa = 0.05

obiekty = rep(names(cisnienie), each=length(cisnienie$Niskie))

wyniki = c(na.omit(cisnienie$Niskie),
           na.omit(cisnienie$Srednie),
           na.omit(cisnienie$Silne),
           na.omit(cisnienie$BardzoSilne))

cisnienieTest = data.frame(obiekty, wyniki)

#średnie próbkowe
srednie = sapply(split(cisnienieTest$wyniki, cisnienieTest$obiekty), mean)

#H0: sig_1^2 = sig_2^2 = sig_3^2 = sig_4^2
#H1: -H0
#H0: wariancje są jednorodne H1: -H0

bartlett.test(wyniki~obiekty, cisnienieTest)

#alfa = 0.05< p-value = 0.5009 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% nie mamy podstaw
#do odrzucenia H0, zatem zakładamy jednorodność
#wariancji i możemy przeprowadzić ANOVA

# H0: mu1 = mu2 = mu3 = mu4 H1: -H0
anova(lm(wyniki~obiekty))

#1 sposób: F=2.2665 <qf(1-alfa, 3, 36) =F_tabl = 2.866 -> brak podstaw do odrzucenia H0
qf(1-alfa, 3, 36)


#2 sposób: alfa=0.05 < p-value=0.09735 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% nie mamy podstaw
#do odrzucenia H0. Stwierdzamy zatem, że ciśnienie
#nie ma istotnego wpływu na wielkość produkcji



# 2. W doświadczeniu badano zawartość popiołu (części niepalnych) 
# dla ekogroszku wyprodukowanego na bazie węgla wysokogatunkowego 
# pochodzącego z pięciu różnych kopalni. Otrzymano następujące wyniki:  
# Obiekty (kopalnie) 
# K1 6,5 7,8 6,9 6,4
# K2 7,2 8,5 7,3 7,0
# K3 7,2 7,5 7,1 7,5
# K4 7,1 7,0 7,1 7,2 
# K5 7,2 6,6 7,4 7,5
# Czy średnie zawartości popiołu dla ekogroszku produkowanego w pięciu 
# kopalniach można uznać za jednakowe? Wykonaj analizę wariancji na 
# poziomie istotności 0,01. 

kopalnie = read.csv("Anova_kopalnie.csv", sep=";", dec=",", head = TRUE)

obiekty = rep(names(kopalnie), each=length(kopalnie$K1))

wyniki = c(na.omit(kopalnie$K1),
           na.omit(kopalnie$K2),
           na.omit(kopalnie$K3),
           na.omit(kopalnie$K4),
           na.omit(kopalnie$K5))

alfa = 0.01

kopalnieTest = data.frame(obiekty, wyniki)

#H0: sig_1^2 = sig_2^2 = sig_3^2 = sig_4^2 = sig_5^2
#H1: -H0

srednie = sapply(split(kopalnieTest$wyniki, kopalnieTest$obiekty), mean)

bartlett.test(wyniki~obiekty, kopalnieTest)

#p-value = 0.03188 > alfa = 0.01 -> brak podstaw do odrzucenia H0, zatem zakładamy
#jednorodność wariancji i możemy przeprowadzić ANOVA.

# H0: mu1 = mu2 = mu3 = mu4 = mu5
#H1: -H0

anova(lm(wyniki~obiekty))

#F = 0.9563
#p-value = 0.4594 > alfa = 0.01 -> brak podstaw do odrzucenia H0


# 3. Każdym z trzech mikrometrów zmierzono kilkukrotnie ten sam przedmiot i 
# uzyskano wyniki:
# Mikrometr I:  4,5;   4,7;   4,8;   4,7 
# Mikrometr II:  4,7;   4,8;   4,5;   4,7;  4,4;   4,8
# Mikrometr III:  4,8;   4,9;   4,8;   4,9;   4,8 
# Zakładając, że błędy pomiarów mają rozkłady normalne o takiej 
# samej wariancji, na poziomie istotności 0,05 zweryfikuj hipotezę, 
# że wybór mikrometru ma wpływ na uzyskane wyniki. 

mikrometry = read.csv("anova_mikrometr.csv", sep=";")
obiekty = rep(names(mikrometry), c(length(na.omit(mikrometry$mikrometrI)), length(na.omit(mikrometry$mikrometrII)), length(na.omit(mikrometry$mikrometrIII))))
wyniki=c(na.omit(mikrometry$mikrometrI), na.omit(mikrometry$mikrometrII), na.omit(mikrometry$mikrometrIII))
mikrometryTest=data.frame(obiekty, wyniki)

#H0: mu1=mu2=mu3, H1: -H0

srednie = sapply(split(mikrometryTest$wyniki, mikrometryTest$obiekty), mean)

anova(lm(wyniki~obiekty))

#alfa = 0.05 < p-value = 0.06859 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% nie mamy podstaw do odrzucenia H0
# Stwierdzamy zatem, że wybór mikrometru nie ma wpływu
# na uzyskane wyniki.


# 4. Populacja sportowców została ostrzeżona, że palenie papierosów 
# opóźnia rozwój. Jedną z miar wpływu palenia na rozwój jest 
# badanie rytmu zatokowego serca. Przeprowadzono eksperyment, w którym zbadano rytm
# zatokowy serca u 24 sportowców po zadanym wysiłku fizycznym i 5-min. 
# Otrzymano wyniki:  
# Niepalący 69 52 71 58 59 65
# Lekko-palący 91 72 81 67 95 84
# Średnio-palący 55 60 78 58 62 66
# Dużo-palący 66 81 70 77 57 79

sportowcy = read.csv("anova_sportowcy.csv", sep=";", dec=",")
obiekty = rep(names(sportowcy), each=length(na.omit(sportowcy$Niepalacy)))
wyniki = c(na.omit(sportowcy$Niepalacy),
           na.omit(sportowcy$Lekkopalacy),
           na.omit(sportowcy$Sredniopalacy),
           na.omit(sportowcy$Duzopalacy))

sportowcyTest = data.frame(obiekty, wyniki)


# (a) Zakładając, że rytm serca u każdego rodzaju palaczy ma rozkład normalny,
# na poziomie istotności 0,01, sprawdź czy palenie papierosów może 
# wpływać na rytm zatokowy serca.        

bartlett.test(wyniki~obiekty, sportowcyTest)


alfa = 0.01

#alfa = 0.01 < p-value = 0.8517 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 1% nie mamy podstaw do
#odrzucenia H0, zatem zakładamy jednorodność
#wariancji i możemy przeprowadzić ANOVA

#H0: mu1=mu2=mu3=mu4 H1: -H0

anova(lm(wyniki~obiekty))

#alfa = 0.01 > p-value = 0.003979 -> Odrzucamy H0

#Na poziomie istotności 1% odrzucamy H0
#Stwierdzamy zatem, że palenie papierosów
#może wpłynąć na rytm zatokowy serca.

# (b) Zastosuj test uczciwych istotnych różnic 
# (honest significant differences) Tukey’a do wyznaczenia grup 
# jednorodnych porównywanych średnich obiektowych.  

# Sprawdzimy, które poziomy palenia są podobne (nie różnią się między sobą istotnie)

plot(TukeyHSD(aov(wyniki~obiekty)))

#Grupy jednorodne
#(obiekty w grupach jednorodnych nie różnią się między sobą istotnie)

# (N-D), (Ś-D), (S-N) -> (N-Ś-D)
# (L-D)


# 5. Badano masę tarczycy chomików w zależności od ich poziomu 
# homozygotyczności (inbredu), wyróżniając cztery grupy 
# (I – osobniki niezinbredowane, II – osobniki o poziomie inbredu z przedziału <0,01 – 0,10>, 
# III – osobniki o poziomie inbredu z przedziału <0,11 – 0,20>, 
# IV – osobniki o poziomie inbredu od 0,21). 
# Uzyskano następujące wyniki: 
# I: 78 65 71 79 64 53 62 
# II: 67 88 76 86 76 79  
# III: 86 90 78 76 89   
# IV: 57 98 89 95 93 

inbred = read.csv("anova_chomiki.csv", sep=";", dec=",")
obiekty = rep(names(inbred), c(length(na.omit(inbred$I)), length(na.omit(inbred$II)), length(na.omit(inbred$III)), length(na.omit(inbred$IV))))
wyniki = c(na.omit(inbred$I), na.omit(inbred$II), na.omit(inbred$III), na.omit(inbred$IV))
inbredTest = data.frame(obiekty, wyniki)

srednie = sapply(split(inbredTest$wyniki, inbredTest$obiekty), mean)

# (a) Sprawdź (przyjmując poziom istotności 0,05) czy słuszne jest przypuszczenie, 
# że masa gruczołu tarczycowego zależy od poziomu inbredu.     


#H0: sig_1^2 = sig_2^2 
#H1: -H0

bartlett.test(wyniki~obiekty, inbredTest)
alfa=0.05

#alfa=0.05 < p-value = 0.2139 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% nie mamy podstaw
#do odrzucenia H0, zatem zakładamy jednorodność
#wariancji i możemy przeprowadzić ANOVA

anova(lm(wyniki~obiekty))

#alfa = 0.05 > p-value = 0.02398 -> odrzucamy H0

#Na poziomie istotności 5% odrzucamy H0
#Stwierdzamy zatem, że masa gruczołu tarczycowego
#zależy od poziomu inbredu.

# (b) Zastosuj test HSD Tukey’a do wyznaczenia grup jednorodnych 
# porównywanych średnich obiektowych.


#Sprawdzamy, które poziomy inbredu są podobne (nie różnią się między sobą istotnie)

TukeyHSD(aov(wyniki~obiekty))

#grupy jednorodne:

#pierwsza grupa jednorodna: II-I, III-I, III-II -> I-II-III
#druga grupa jednorodna: III-II, IV-II, IV-III -> II-III-IV

plot(TukeyMSD(aov(wyniki~obiekty)))


# 6. Student inżynierii przemysłowej pomógł zespołowi badawczemu ocenić 
# różne strategie lokalizacji pułapek zapachowych na ćmy cygańskie. 
# Uzyskano następujące dane (w %):  Strategia lokalizacji pułapek 
# obserwacje 1 2 3 4 5
# rozproszony 90 92 94 93 92
# skoncentrowany 99 97 98 98 99
# roślina żywicielka 95 96 97 97 96
# powietrzny 98 98 99 99 98
# gruntowy 87 93 90 91 89

# Zmienną odpowiedzi jest szacunkowy odsetek uwięzionej rodzimej 
# populacji płci męskiej.  

pulapki = read.csv("C:/Users/klemm/Desktop/SADkolos2/Anova_pulapki.csv", sep = ";")


obiekty = rep(names(pulapki), each = length(na.omit(pulapki$rozsiany)))


wyniki = c(na.omit(pulapki$rozsiany),
           na.omit(pulapki$skoncentrowany),
           na.omit(pulapki$roslina.zywicielka),
           na.omit(pulapki$powietrzny),
           na.omit(pulapki$gruntowy))


pulapkiTest = data.frame(obiekty, wyniki)

# (a) Czy strategia lokalizacji może mieć wpływ na liczbę uwięzionych ciem cygańskich? 
# Przyjmij poziom istotności 0,05 i normalność proporcji uwięzionych ciem cygańskich. 
# Zweryfikuj założenie dotyczące jednorodności wariancji przed wykonaniem ANOVA. 

### H0: sig_1^2 = sig_2^2 = sig_3^2 = sig_4^2 = sig_5^2
### H1: !HO

alfa = 0.05

bartlett.test(wyniki~obiekty, pulapkiTest)

### Wariancje są jednorodne (p-value > alfa)


### H0: mu1 = mu2
### H1: !H0

anova(lm(wyniki~obiekty))

### Odrzucamy H0 (p-value < alfa)



# (b) Zastosuj test HSD Tukey’a do wyznaczenia grup jednorodnych porównywanych średnich obiektowych. 


TukeyHSD(aov(wyniki~obiekty))

### Grupy jednorodne: r-g, rz-p, s-p, s-rz => s-rz-p, r-g


#######################################
############# REGRESJA ################
#######################################

# 1. 1. Poniższa tabela przedstawia wyniki eksperymentu, w którym inżynier 
# chce wyznaczyć relację miedzy końcową wielkością produkcji środków 
# chemicznych Y (w kg) w zależności od ilości zużytego surowca X (w litrach): 
# X 14 23 9 17 10 22 5 12 6 16 
# Y 68 105 40 79 81 95 31 72 45 93 

dane = read.csv("Reg_chemikalia.csv", sep=";", dec=",", head=TRUE)

X = dane$surowiec
Y = dane$produkt


# (a) Narysuj wykres punktowy przedstawiający zależność 
# wielkości produkcji od ilości zużytego surowca (scatter plot). 

plot(X, Y, main="Zależność wielkości produkcji od ilości zużytego surowca", xlab="Ilość zużytego surowca (X)", ylab = "Wielkość produkcji (Y)", pch=19)


# (b) Wyznacz i zinterpretuj kowariancję próbkową między ilością 
# zużytego surowca a wielkością produkcji. 

cov(X, Y) #S_XY = 138.4889
#Jest różna od 0, zatem istnieje liniowa zależność
#między ilością zużytego surowca, a końcową wielkością
#produkcji środków chemicznych

#Ponieważ kowariancja jest dodatnia, zatem wraz ze
#wzrostem ilości zużytego surowca wzrasta końcowa
#wielkość produkcji środków chemicznych

# (c) Wyznacz i zinterpretuj współczynnik korelacji.

cor(X, Y) #r = 0.8953468
#Współczynnik korelacji r jest w wartości bezwględnej
#większy lub równy 0,8, zatem istnieje bardzo silny związek
#liniowy między ilością zużytego surowca
#a końcową wielkością produkcji środków chemicznych

#Kowariancja to kierunek zależności, a korelacja to siła zależności

# (d) Wyznacz ocenę prostej regresji między wielkością
# produkcji a ilością zużytego surowca. 

#y = b0 + b1x
lm(Y~X) #y = 22,405 + 3,619x
#Równanie prostej regresji liniowej


# (e) Dodaj do wykresu punktowego prostą regresji. 

abline(lm(Y~X), col = "red")

# (f) W jaki sposób zmieni się wielkość produkcji, 
#jeśli ilość surowca wzrośnie o 1 litr?

#Jeśli ilość surowca wzrośnie o 1 litr, to końcowa
#wielkość produkcji środków chemicznych wzrośnie
#o 3,619 kg (interpretacja współczynnika regresji
#liniowej)

# (g) Jaka będzie wielkość produkcji, jeśli zużyjemy do produkcji 20 litrów surowca?
# (h) Jaka będzie wielkość produkcji, jeśli zużyjemy do produkcji 15 litrów surowca? 

predict(lm(Y~X), data.frame(X=c(20,15)))
#Jeśli zużyjemy do produkcji 20 litrów surowca, to 
#końcowa wielkość produkcji wyniesie 94,79 kg
#Jeśli zużyjemy do produkcji 15 litrów surowca, to
#końcowa wielkość produkcji wyniesie 76,69 kg


# (i) Oceń dopasowanie prostej regresji do danych.

summary(lm(Y~X))$r.squared
#Ocena dopasowania prostej regresji: 0.8016458
#Prosta regresji liniowej jest dobrze dopasowana do danych

#Końcowa wielkość produkcji środków chemicznych
#jest wyjaśniona w ok. 80% przez ilości
#zużytego surowca

# (j) Zweryfikuj test o istotności regresji. Przyjmij poziom istotności 5%. Zinterpretuj wynik.

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


# 2. Żywotność pewnego urządzenia (w miesiącach) zależy od liczby wyprodukowanych 
# przez to urządzenie elementów (efektywność urządzenia). Dla próby 9 urządzeń
# tego samego typu otrzymano następujące wyniki: 
# Efektywność (X) 18 20 18 17 15 15 14 12 10 
# Żywotność (Y) 2 3 3 4 5 6 7 11 9 

dane = read.csv("Reg_urzadzenie.csv", sep=";", dec=",", head=TRUE)

X = dane$efektywnosc
Y = dane$zywotnosc

# (a) Narysuj wykres punktowy przedstawiający zależność żywotności 
# od efektywności (scatter plot). 

plot(X, Y, main="Zależność żywotności urządzenia od liczby wyprodukowanych elementów przez urządzenie", xlab="Liczba wyprodukowanych elementów (X)", ylab="Żywotność urządzenia w miesiącach  (Y)", pch=19)


# (b)  Oblicz i zinterpretuj kowariancję między żywotnością i efektywnością.

cov(X, Y) #S_XY = -8.652778
#Jest różna od zera, zatem istnieje liniowa zależność

# (c) Oblicz i zinterpretuj współczynnik korelacji. 

cor(X, Y) #r = -0.9094164
#Wartość bezwględna większa od 0.8, zatem istnieje bardzo
#silny związek liniowy


# (d) Wyznacz ocenę prostej regresji żywotności urządzenia od jego efektywności.

model = lm(Y~X) # y = 18.8823 - 0.8629x
abline(lm(Y~X), col="red")


# (e) Jak zmieni się żywotność urządzenia jeśli efektywność wzrośnie o 1 element?

#Zmaleje o 0.8629 miesiąca

# (f) Oszacuj żywotność urządzenia przy efektywności 11 elementów.  

# (g) Oszacuj żywotność urządzenia przy efektywności 19 elementów.

predict(model, data.frame(X=c(11, 19)))

#9.390582
#2.487535

# (h) Oceń dopasowanie prostej regresji.             

summary(model)$r.squared

#82.7% - prosta regresji liniowej jest dobrze dopasowana

# (i) Zweryfikuj test istotności regresji. Przyjmij poziom istotności 1%. Zinterpretuj otrzymany wynik.  

alfa = 0.01
#H0: b1=0 (regresja liniowa jest nieistotna)
#H1: b1!=0 (regresja liniowa jest istotna)

anova(model)

#alfa = 0.01 > p-value = 0.00067 -> odrzucamy H0

#Na poziomie istotności 0.01 dane potwierdzają hipotezę, 
#że regresja liniowa jest istotna


# 3. 3. Przeprowadzono proces usuwania arszeniku z wód gruntowych. 
# Poniższa tabela przedstawia procentowe ilości usuniętego przez proces
# w arszeniku w zależności od zakwaszenia (pH) gleby:    
# pH 7,01 7,11 7,12 7,24 7,94 7,94 8,04 8,05 8,07 
# % ilość arszeniku 60 67 66 52 50 45 52 48 40 
# pH 8,90 8,94 8,95 8,97 8,98 9,85 9,86 9,86 9,87 
# % ilość arszeniku 23 20 40 31 26 9 22 13 7 

# (a) Narysuj diagram punktowy ilości usuniętego arszeniku w zależności od zakwaszenia gleby.

dane = read.csv("Reg_arszenik.csv", sep=";", dec=",", head=TRUE)

X = dane$pH
Y = dane$arszenik

plot(X, Y, main="Zależność usuniętego arszeniku od zakwaszenia gleby")


# (b) Oblicz i zinterpretuj kowariancję i współczynnik korelacji między zakwaszeniem 
# gleby a ilością usuniętego arszeniku.                 

cov(X,Y) #-18.32, kowariancja różna od zera, zatem istnieje zależność liniowa
cor(X, Y) #-0.95, zatem istnieje silna zależność liniowa


# (c) Wyznacz prostą regresji zależności ilości usuniętego arszeniku i zakwaszenia gleby. 

model = lm(Y~X)
abline(model, col="red")


# (d) W jaki sposób zmieni się ilość usuniętego przez proces arszeniku 
# jeśli pH gleby wzrośnie o 1?           

#Maleje o 18.03%

# (e) Ile arszeniku zostanie usunięte, jeśli pH gleby wyniesie 7,5? 
# (f) Ile arszeniku zostanie usunięte, jeśli pH gleby wyniesie 9?  

predict(model, data.frame(X=c(7.5, 9)))
#55
#28


# (g) Jak dobra jest ocena liniowa regresji?

summary(model)$r.squared
#90.34%

# (h) Zweryfikuj test istotności regresji. Przyjmij poziom istotności 1%. 

alfa = 0.01
anova(model)

#H0: b1 = 0
#H0: b1 != 0

#alfa = 0.01 > p=value = 1.552e-09
#Zatem odrzucamy H0

#Na poziomie istotności 1% dane potwierdzają 
#hipotezę, że regresja liniowa jest istotna.



#######################################
############ CHI-KWADRAT ##############
#######################################

# 1. Stowarzyszenie Russela Reynolda przeprowadziło ankietę wśród 
# emerytowanych menedżerów wyższego szczebla, którzy wrócili do pracy. 
# Ustalili, że po powrocie do pracy 38% było zatrudnionych w innej organizacji, 
# 32% samozatrudnionych, 23% było freelancerami lub konsultantami, 
# a 7% założyło własne firmy. Aby sprawdzić, czy te wartości procentowe 
# są zgodne z odsetkami mieszkańców hrabstwa Allegheny, lokalny badacz 
#3 przeprowadził ankietę wśród 300 emerytowanych menedżerów, którzy 
# wrócili do pracy, i odkrył, że 122 pracowało dla innej firmy, 
# 85 prowadziło działalność na własny rachunek, 76 pracowało jako 
# freelancer lub doradzało, a 17 założyło własne firmy. Czy przy 
# istotności 10% dane potwierdzają, że odsetki poszczególnych zatrudnionych
# w hrabstwie Allegheny różnią się od ich odpowiedników w skali całego kraju?

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

# 2. Badacz przeczytał w artykule, że liczba zgonów związanych
# z bronią palną wśród osób w wieku od 1 do 18 lat rozkładała 
# się następująco: 74% to wypadki, 16% to zabójstwa, a 10% to samobójstwa. 
# W jej okręgu w ubiegłym roku doszło do 68 wypadków śmiertelnych, 
# 27 zabójstw i 5 samobójstw. Czy na poziomie istotności 10% dane
# potwierdzają, że odsetki poszczególnych zgonów różnią się od 
# przedstawionych w artykule? 

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


# 3. M&M/Mars, producent cukierków Skittles, twierdzi, że mieszanka smakowa
# wynosi 20% dla każdego smaku. Skittles to połączenie cukierków 
# o smaku cytrynowym, limonkowym, pomarańczowym, truskawkowym 
# i winogronowym. Poniższe dane przedstawiają wyniki czterech 
# losowo wybranych torebek Skittles i ich mieszanek smakowych. 

# Smaki 
# Torebka 
# Cytrynowy 7 20 4 12
# Limonkowy 20 5 16 9
# Pomarańczowy 10 5 13 16
# Truskawkowy 7 13 21 3
# Winogronowy 14 17 4 17

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


# 4. Poniższe przykładowe dane przedstawiają stężenie ozonu 
# (mierzone w częściach na 100 milionów) w powietrzu 
# w centrum miasta przez 78 kolejnych letnich dni w 2004 roku: 
# 3.5 1.7 3.1 4.5 3.0 3.7 4.1 9.4 2.5 5.7 1.4 4.7 6.1 6.8 
# 1.1 5.8 4.2 6.0 7.6 3.5 5.3 3.0 4.4 3.9 1.6 8.1 2.4 7.5 
# 4.7 5.4 1.4 6.6 5.9 4.7 5.1 4.7 4.4 6.8 2.0 6.8 5.8 5.7 
# 6.5 2.8 4.1 6.0 6.7 6.2 5.8 3.4 5.4 6.2 5.5 3.4 6.0 7.4 
# 2.5 3.7 5.6 1.4 7.6 3.3 9.4 5.6 5.6 6.2 3.1 4.4 5.5 3.7 
# 5.8 6.6 6.6 3.8 5.3 6.6 11.7 4.0 

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

# Powyższe dane można pogrupować w przedziały w następujący sposób: 
# stężenie ozonu 0-2 2-4 4-6 6-8 8-10 10-12 
# częstotliwość 7 19 31 17 3 1 
# Należy pamiętać, że do ostatnich klas kwalifikuje się mniej niż 5 pomiarów. 
# Na poziomie istotności 0,05 sprawdź, czy stężenie ozonu ma rozkład normalny.

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

# 5. 5. Poniższe dane dotyczą objętości guza (w mm^3) w grupie losowo wybranych 100 myszy: 
# Objętość guza (0; 2] (2; 4] (4; 6] (6; 8] (8; 10] 
# Numer myszy 10 25 35 20 10 
# Na poziomie istotności 0.1 zweryfikuj czy objętość  guza ma rozkład normalny. 

przedzial = function(dane1, dane2, alfa){
  diff = mean(dane1) - mean(dane2)
  
  n1 = length(dane1)
  n2 = length(dane2)
  
  s1 = var(dane1)
  s2 = var(dane2)
  
  sp = ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
  
  t = qt(1-alfa/2, df=n1+n2-2)
  
  L = diff - t* sqrt(sp*(1/n1 + 1/n2))
  P = diff + t*sqrt(sp*(1/n1 + 1/n2))
  
  return(c(L,P))
  
}

przedzial(cel1, cel2, alfa)




# 6. 6. Poniższe dane przedstawiają liczbę punktów uzyskanych przez grupę
# studentów na koniec semestru: 
# 23 60 79 32 57 74 52 70 82 36 80 77 81 95 41 65 92 85 55 76 52 10 
# 64 75 78 25 80 98 81 67 41 71 83 54 64 72 88 62 74 43 60 78 89 76
# 84 48 90 15 79 84 34 67 17 82 69 74 63 80 85 61        
# Na poziomie istotności 0,01 sprawdź, czy oceny tej grupy uczniów mają rozkład normalny. 


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


# 7. 7. Socjolog pragnie sprawdzić, czy liczba lat nauki danej osoby 
# ma związek z jej miejscem zamieszkania. Wybrano próbę 88 osób i sklasyfikowano, 
# jak pokazano. 
# Obszar        Brak Uczelni  4-letnie studia  Stopień zaawansowany
# Miejski           15            12                8
# Podmiejski        8             15                9
# Wiejski           6             8                 7

# Czy na poziomie istotności 0,05 socjolog może 
# stwierdzić, że miejsce zamieszkania danej osoby zależy od liczby lat studiów?  


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


# 8. Badacz wybrał 100 pasażerów każdej z 3 linii lotniczych 
# i zapytał ich, czy linia lotnicza zgubiła ich bagaż podczas 
# ostatniego lotu. Dane przedstawiono w tabeli. Czy na poziomie 
# istotności 0,05 dane potwierdzają, że odsetek pasażerów, którzy 
# zagubili bagaż w trakcie lotu, zależy od linii lotniczej? 

#       Linia 1     Linia 2     Linia 3
# Tak     10          7           4
# Nie     90          93          96


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


# 9. W Senacie planowane jest głosowanie nad projektem 
# ustawy zezwalającej na instalowanie anten satelitarnych 
# dowolnej wielkości na obszarach objętych ograniczeniami wykonawczymi. 
# Podobną ustawę przyjęła Izba. Przeprowadzono badanie opinii publicznej, 
# aby sprawdzić, czy odczucia danej osoby w związku z ograniczeniami 
# dotyczącymi anten satelitarnych są powiązane z jej wiekiem. 
# Uzyskano następujące dane: 

#           18-29       30-49       50-64     65 i więcej
# Za          96          96          90        36
# Przeciw     201         189         195       234
# Nie wiem    3           15          15        30

# Czy dane potwierdzają, że opinia zależy od wieku? 
# Załóżmy poziom istotności 0,05. 

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

