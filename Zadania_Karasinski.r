install.packages("nortest")
library(nortest)

# =============================================================================
# Zadanie 1: Porownanie srednich dwoch zmiennych (test t dla prob niezaleznych)
# Porownujemy miesieczne stopy zwrotu akcji PKO BP i CD Projekt z GPW
# =============================================================================

# ustawiamy ziarno dla powtarzalnosci wynikow
set.seed(67)

# symulujemy miesieczne stopy zwrotu (w %) dla 48 miesiecy (4 lata)
# PKO BP - bank, stabilniejsze stopy zwrotu
# CDR - sektor gier, bardziej zmienne stopy zwrotu

pko_returns <- rnorm(48, mean = 0.8, sd = 4.2)
cdr_returns <- rnorm(48, mean = 1.5, sd = 8.5)

# tworzymy ramke danych
df_task1 <- data.frame(PKO = pko_returns, CDR = cdr_returns)

# statystyki opisowe
cat("Srednia PKO:", mean(df_task1$PKO), "\n")
cat("Srednia CDR:", mean(df_task1$CDR), "\n")
cat("Odch. std. PKO:", sd(df_task1$PKO), "\n")
cat("Odch. std. CDR:", sd(df_task1$CDR), "\n")

# WYNIKI STATYSTYK OPISOWYCH:
# Srednia PKO: 0.198% - niewielka dodatnia srednia stopa zwrotu
# Srednia CDR: 1.191% - wyzsza srednia stopa zwrotu niz PKO
# Odch. std. PKO: 4.19% - umiarkowana zmiennosc typowa dla sektora bankowego
# Odch. std. CDR: 8.10% - prawie dwukrotnie wyzsza zmiennosc, typowa dla sektora gier
# Juz na tym etapie widac, ze CDR ma wyzsza srednia, ale takze znacznie wiecej ryzyka

# test normalnosci Lilliefors (Kolmogorov-Smirnov) dla obu zmiennych
# H0: rozklad jest normalny
# H1: rozklad nie jest normalny

lillie.test(df_task1$PKO)
lillie.test(df_task1$CDR)

# WYNIKI TESTU NORMALNOSCI LILLIEFORS:
# PKO: D = 0.0638, p-value = 0.895 -> p > 0.05, nie odrzucamy H0
#   Rozklad stop zwrotu PKO jest zgodny z rozkladem normalnym
# CDR: D = 0.0687, p-value = 0.827 -> p > 0.05, nie odrzucamy H0
#   Rozklad stop zwrotu CDR jest zgodny z rozkladem normalnym
# Wniosek: Zalozenie normalnosci jest spelnione dla obu zmiennych,
# mozemy stosowac testy parametryczne (test t, test F)

# test rownosci wariancji (test F)
# H0: wariancje sa rowne
# H1: wariancje nie sa rowne

var.test(df_task1$PKO, df_task1$CDR)

# WYNIKI TESTU F (ROWNOSC WARIANCJI):
# F = 0.267, p-value = 1.348e-05 -> p < 0.05, odrzucamy H0
# Wariancje stop zwrotu PKO i CDR sa istotnie rozne
# CDR ma znacznie wyzsza zmiennosc (SD=8.10) niz PKO (SD=4.19)
# Stosunek wariancji F=0.267 oznacza, ze wariancja PKO stanowi ok. 27% wariancji CDR
# Konsekwencja: musimy uzyc testu t-Welcha (zamiast klasycznego testu t),
# poniewaz nie jest spelnione zalozenie o rownosci wariancji

# test t-Studenta dla prob niezaleznych
# H0: srednie sa rowne (mu_PKO = mu_CDR)
# H1: srednie nie sa rowne (mu_PKO != mu_CDR)
# var.equal = FALSE bo wariancje sa rozne (test Welcha)

t.test(df_task1$PKO, df_task1$CDR, var.equal = FALSE)

# WYNIKI TESTU T-WELCHA:
# t = -0.754, df = 70.44, p-value = 0.4533
# p-value = 0.4533 > 0.05, zatem NIE odrzucamy H0
# Nie ma istotnej statystycznie roznicy miedzy srednimi stopami zwrotu PKO i CDR
# na poziomie istotnosci alfa = 0.05
#
# INTERPRETACJA KONCOWA ZADANIA 1:
# Mimo ze CDR ma wyzsza srednia stope zwrotu (1.19% vs 0.20%), roznica ta
# nie jest statystycznie istotna (p = 0.453). Wynika to z duzej zmiennosci
# stop zwrotu CDR (SD = 8.10%), ktora sprawia, ze przedzial ufnosci jest
# bardzo szeroki i obejmuje zero. Innymi slowy - roznica miedzy srednimi
# moze byc przypadkowa i wynikac z losowej zmiennosci danych, a nie z
# rzeczywistej przewagi CDR nad PKO pod wzgledem sredniej stopy zwrotu.


# =============================================================================
# Zadanie 2: Porownanie srednich kilku zmiennych (ANOVA)
# Porownujemy srednie stopy zwrotu spolek z 4 sektorow GPW:
# banki, energetyka, gry, media
# =============================================================================

set.seed(6767)

# symulujemy miesieczne stopy zwrotu (w %) dla 36 miesiecy w kazdym sektorze
# kazdy sektor ma inna charakterystyke ryzyka i zwrotu

returns_banks <- rnorm(36, mean = 0.5, sd = 3.8)
returns_energy <- rnorm(36, mean = 0.3, sd = 4.5)
returns_games <- rnorm(36, mean = 1.8, sd = 9.2)
returns_media <- rnorm(36, mean = 0.2, sd = 5.1)

# tworzymy ramke danych w formacie dlugim (potrzebne do ANOVA)
df_task2 <- data.frame(
  value = c(returns_banks, returns_energy, returns_games, returns_media),
  sector = factor(rep(c("banks", "energy", "games", "media"), each = 36))
)

# statystyki opisowe dla kazdego sektora
by(df_task2$value, df_task2$sector, mean)
by(df_task2$value, df_task2$sector, sd)

# WYNIKI STATYSTYK OPISOWYCH:
# Srednie stopy zwrotu: banks=0.195%, energy=0.181%, games=1.578%, media=0.804%
# Odchylenia standardowe: banks=4.41%, energy=5.08%, games=9.86%, media=5.84%
# Sektor gier wyroznia sie najwyzsza srednia (1.578%) ale tez najwyzsza zmiennoscia (9.86%)
# Sektory banki i energetyka maja bardzo zblizone srednie (ok. 0.2%)

# test normalnosci Lilliefors dla kazdej grupy
# H0: rozklad jest normalny
# H1: rozklad nie jest normalny

by(df_task2$value, df_task2$sector, lillie.test)

# WYNIKI TESTU NORMALNOSCI LILLIEFORS DLA KAZDEJ GRUPY:
# banks:  p-value = 0.385 > 0.05 -> nie odrzucamy H0, rozklad normalny
# energy: p-value = 0.468 > 0.05 -> nie odrzucamy H0, rozklad normalny
# games:  p-value = 0.161 > 0.05 -> nie odrzucamy H0, rozklad normalny
# media:  p-value = 0.653 > 0.05 -> nie odrzucamy H0, rozklad normalny
# Wniosek: Wszystkie grupy maja rozklad normalny - zalozenie normalnosci
# dla ANOVA jest spelnione

# test jednorodnosci wariancji Bartletta
# H0: wariancje we wszystkich grupach sa rowne
# H1: co najmniej jedna wariancja jest rozna

bartlett.test(df_task2$value, df_task2$sector)

# WYNIKI TESTU BARTLETTA (JEDNORODNOSC WARIANCJI):
# K-squared = 28.537, df = 3, p-value = 2.802e-06
# p-value < 0.05, odrzucamy H0 - wariancje NIE sa jednorodne
# Sektor gier ma znacznie wyzsza zmiennosc (SD=9.86%) niz pozostale sektory
#
# UWAGA: Pomimo naruszenia zalozenia jednorodnosci wariancji, ANOVA jest
# odporny (robust) na to naruszenie, gdy grupy sa rowne liczebnie.
# W naszym przypadku kazda grupa ma dokladnie 36 obserwacji, wiec mozemy
# kontynuowac analize ANOVA. Przy nierownych grupach nalezaloby uzyc
# testu Welcha (oneway.test) lub testu nieparametrycznego Kruskala-Wallisa.

# jednoczynnikowa analiza wariancji (ANOVA)
# H0: srednie we wszystkich grupach sa rowne
# H1: co najmniej jedna srednia jest rozna

anova <- aov(df_task2$value ~ df_task2$sector)
summary(anova)

# WYNIKI ANOVA:
# F = 0.355, p-value = 0.785
# p-value = 0.785 > 0.05, NIE odrzucamy H0
# Nie stwierdzono istotnych statystycznie roznic miedzy srednimi stopami
# zwrotu w poszczegolnych sektorach
# Wartosc F = 0.355 jest bardzo niska (bliska 0), co oznacza, ze zmiennosc
# miedzy grupami jest mniejsza niz zmiennosc wewnatrz grup

# analiza post-hoc Tukeya - sprawdzamy ktore pary sektorow sie roznia
TukeyHSD(anova)

# WYNIKI TESTU POST-HOC TUKEYA (HSD):
# Zadna para sektorow nie wykazuje istotnej roznicy (wszystkie p adj > 0.80):
# energy-banks:  p adj > 0.80, roznica nieistotna
# games-banks:   p adj > 0.80, roznica nieistotna
# media-banks:   p adj > 0.80, roznica nieistotna
# games-energy:  p adj > 0.80, roznica nieistotna
# media-energy:  p adj > 0.80, roznica nieistotna
# media-games:   p adj > 0.80, roznica nieistotna
#
# INTERPRETACJA KONCOWA ZADANIA 2:
# Nie stwierdzono statystycznie istotnych roznic w srednich stopach zwrotu
# miedzy sektorami banki, energetyka, gry i media na poziomie istotnosci 0.05.
# Mimo ze sektor gier ma najwyzsza srednia (1.578%), jego bardzo wysoka zmiennosc
# (SD=9.86%) powoduje, ze roznica ta nie jest statystycznie istotna.
# Duza wariancja wewnatrz grup "zaslania" ewentualne roznice miedzy grupami.


# =============================================================================
# Zadanie 3: Test A/B dla prob zaleznych (parowy test t)
# Porownujemy stopy zwrotu indeksu WIG20 przed i po wprowadzeniu
# programu Pracowniczych Planow Kapitalowych (PPK) - dane sparowane miesieczne
# =============================================================================

set.seed(67)

# symulujemy miesieczne stopy zwrotu WIG20 (w %) dla 30 miesiecy
# przed PPK - nieco nizsze stopy zwrotu
# po PPK - oczekujemy wyzszych stop zwrotu (dodatkowy kapital na rynku)

n_obs <- 30

before_ppk <- rnorm(n_obs, mean = 0.4, sd = 4.0)
# po PPK dodajemy maly efekt (ok. 0.5 pp wyzsze stopy)
after_ppk <- before_ppk + rnorm(n_obs, mean = 0.5, sd = 2.0)

# tworzymy ramke danych
df_task3 <- data.frame(
  before = before_ppk,
  after = after_ppk
)

# obliczamy roznice (after - before)
df_task3$diff <- df_task3$after - df_task3$before

# statystyki opisowe
cat("Srednia przed PPK:", mean(df_task3$before), "\n")
cat("Srednia po PPK:", mean(df_task3$after), "\n")
cat("Srednia roznic:", mean(df_task3$diff), "\n")

# WYNIKI STATYSTYK OPISOWYCH:
# Srednia przed PPK: -0.210% - ujemna srednia stopa zwrotu przed wprowadzeniem PPK
# Srednia po PPK: 0.091% - niewielka dodatnia srednia stopa zwrotu po PPK
# Srednia roznic: 0.300 punktu procentowego - niewielki wzrost po wprowadzeniu PPK
# Na pierwszy rzut oka widac poprawe, ale czy jest ona statystycznie istotna?

# test normalnosci roznic (wymagany dla parowego testu t)
# H0: roznice maja rozklad normalny
# H1: roznice nie maja rozkladu normalnego

lillie.test(df_task3$diff)

# WYNIKI TESTU NORMALNOSCI LILLIEFORS NA ROZNICACH:
# D = 0.0857, p-value = 0.831
# p-value = 0.831 > 0.05, nie odrzucamy H0
# Roznice maja rozklad normalny - mozemy stosowac parowy test t-Studenta
# Zalozenie normalnosci roznic jest kluczowe dla testu parowego

# parowy test t-Studenta
# H0: srednia roznic = 0 (brak efektu PPK)
# H1: srednia roznic != 0 (jest efekt PPK)

t.test(df_task3$after, df_task3$before, paired = TRUE)

# WYNIKI PAROWEGO TESTU T-STUDENTA:
# t = 0.952, df = 29, p-value = 0.349
# p-value = 0.349 > 0.05, NIE odrzucamy H0
# Nie stwierdzono statystycznie istotnego efektu wprowadzenia PPK
# na stopy zwrotu WIG20

# alternatywnie: test nieparametryczny Wilcoxona dla prob zaleznych
# stosujemy gdy roznice nie maja rozkladu normalnego

wilcox.test(df_task3$after, df_task3$before, paired = TRUE)

# WYNIKI TESTU WILCOXONA (NIEPARAMETRYCZNY):
# V = 288, p-value = 0.262
# p-value = 0.262 > 0.05, NIE odrzucamy H0
# Test nieparametryczny potwierdza wynik testu parametrycznego -
# brak istotnego efektu PPK na stopy zwrotu WIG20
#
# INTERPRETACJA KONCOWA ZADANIA 3:
# Nie stwierdzono statystycznie istotnego wplywu wprowadzenia PPK na stopy
# zwrotu WIG20 (p = 0.349 dla testu t, p = 0.262 dla testu Wilcoxona).
# Srednia roznica 0.30 punktu procentowego jest zbyt mala w stosunku do
# zmiennosci danych, aby uznac ja za istotna statystycznie.
# Oba testy (parametryczny i nieparametryczny) daja zgodne wyniki, co
# wzmacnia nasz wniosek o braku istotnego efektu.
# Mozliwe przyczyny: efekt PPK moze byc zbyt maly do wykrycia przy tej
# wielkosci proby (30 obs.) lub rzeczywiscie nie wplywa istotnie na indeks.
