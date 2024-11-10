#Zadanie 1

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

#Zadanie 2

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

#Zadanie 3

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


#Zadanie 4
sportowcy = read.csv("anova_sportowcy.csv", sep=";", dec=",")
obiekty = rep(names(sportowcy), each=length(na.omit(sportowcy$Niepalacy)))
wyniki = c(na.omit(sportowcy$Niepalacy),
           na.omit(sportowcy$Lekkopalacy),
           na.omit(sportowcy$Sredniopalacy),
           na.omit(sportowcy$Duzopalacy))

sportowcyTest = data.frame(obiekty, wyniki)

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

# Sprawdzimy, które poziomy palenia są podobne (nie różnią się między sobą istotnie)

plot(TukeyHSD(aov(wyniki~obiekty)))

#Grupy jednorodne
#(obiekty w grupach jednorodnych nie różnią się między sobą istotnie)

# (N-D), (Ś-D), (S-N) -> (N-Ś-D)
# (L-D)


#Zadanie 5

inbred = read.csv("anova_chomiki.csv", sep=";", dec=",")
obiekty = rep(names(inbred), c(length(na.omit(inbred$I)), length(na.omit(inbred$II)), length(na.omit(inbred$III)), length(na.omit(inbred$IV))))
wyniki = c(na.omit(inbred$I), na.omit(inbred$II), na.omit(inbred$III), na.omit(inbred$IV))
inbredTest = data.frame(obiekty, wyniki)

srednie = sapply(split(inbredTest$wyniki, inbredTest$obiekty), mean)

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

#Sprawdzamy, które poziomy inbredu są podobne (nie różnią się między sobą istotnie)

TukeyHSD(aov(wyniki~obiekty))

#grupy jednorodne:

#pierwsza grupa jednorodna: II-I, III-I, III-II -> I-II-III
#druga grupa jednorodna: III-II, IV-II, IV-III -> II-III-IV

plot(TukeyMSD(aov(wyniki~obiekty)))

#a




