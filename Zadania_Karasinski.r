install.packages("nortest")
install.packages("jsonlite")
install.packages("curl")
library(nortest)
library(jsonlite)

# =============================================================================
# Zadanie 1: Porownanie srednich dwoch zmiennych (test t dla prob niezaleznych)
# Porownujemy roczne tempo wzrostu PKB (GDP growth, %) dla USA i Niemiec
# Dane realne z World Bank API za lata 2000-2023
# =============================================================================

# pobieramy dane z World Bank API (realne dane ekonomiczne)
url_us <- "https://api.worldbank.org/v2/country/USA/indicator/NY.GDP.MKTP.KD.ZG?format=json&date=2000:2023&per_page=50"
url_de <- "https://api.worldbank.org/v2/country/DEU/indicator/NY.GDP.MKTP.KD.ZG?format=json&date=2000:2023&per_page=50"

data_us <- fromJSON(url_us)[[2]]
data_de <- fromJSON(url_de)[[2]]

# wyciagamy wartosci numeryczne i usuwamy NA
gdp_us <- as.numeric(data_us$value)
gdp_de <- as.numeric(data_de$value)
gdp_us <- gdp_us[!is.na(gdp_us)]
gdp_de <- gdp_de[!is.na(gdp_de)]

# statystyki opisowe
cat("=== STATYSTYKI OPISOWE ===\n")
cat("Srednie roczne tempo wzrostu PKB (2000-2023):\n")
cat("USA:", round(mean(gdp_us), 4), "%\n")
cat("Niemcy:", round(mean(gdp_de), 4), "%\n")
cat("Odchylenie standardowe:\n")
cat("USA:", round(sd(gdp_us), 4), "%\n")
cat("Niemcy:", round(sd(gdp_de), 4), "%\n")

# WYNIKI STATYSTYK OPISOWYCH:
# Srednia USA: 2.185% - wyzsza srednia stopa wzrostu PKB
# Srednia Niemcy: 1.181% - nizsza srednia stopa wzrostu PKB
# SD USA: 1.798% - mniejsza zmiennosc wzrostu gospodarczego
# SD Niemcy: 2.334% - wieksza zmiennosc, typowa dla gospodarki eksportowej

# test normalnosci Lilliefors (Kolmogorov-Smirnov)
# H0: rozklad jest normalny
# H1: rozklad nie jest normalny

lillie.test(gdp_us)
lillie.test(gdp_de)

# WYNIKI TESTU NORMALNOSCI LILLIEFORS:
# USA: D = 0.198, p-value = 0.016 -> p < 0.05, odrzucamy H0!
#   Rozklad wzrostu PKB USA NIE jest zgodny z rozkladem normalnym.
#   Wynika to z silnego spadku PKB w 2020 (COVID: -2.16%) i odbicia w 2021 (+6.06%)
# Niemcy: D = 0.160, p-value = 0.115 -> p > 0.05, nie odrzucamy H0
#   Rozklad wzrostu PKB Niemiec jest zgodny z rozkladem normalnym
#
# UWAGA: Poniewaz rozklad USA nie jest normalny, wyniki testu t nalezy
# interpretowac z ostroznoscia. Przy n=24 test t jest jednak dosc odporny
# na umiarkowane odchylenia od normalnosci (centralne twierdzenie graniczne).
# Alternatywnie mozna zastosowac test nieparametryczny (Wilcoxon rank-sum).

# test rownosci wariancji (test F)
# H0: wariancje sa rowne
# H1: wariancje nie sa rowne

var.test(gdp_us, gdp_de)

# WYNIKI TESTU F (ROWNOSC WARIANCJI):
# F = 0.593, p-value = 0.218 -> p > 0.05, NIE odrzucamy H0
# Wariancje wzrostu PKB USA i Niemiec nie roznia sie istotnie statystycznie
# Stosunek wariancji F = 0.593 jest bliski 1, co potwierdza podobna zmiennosc
# Mozemy zatem zastosowac zarowno test t z rownoscia wariancji jak i test Welcha

# test t-Studenta dla prob niezaleznych (test Welcha)
# H0: srednie sa rowne (mu_USA = mu_DEU)
# H1: srednie nie sa rowne (mu_USA != mu_DEU)

t.test(gdp_us, gdp_de, var.equal = FALSE)

# WYNIKI TESTU T-WELCHA:
# t = 1.669, df = 43.19, p-value = 0.102
# p-value = 0.102 > 0.05, NIE odrzucamy H0
# Nie stwierdzono istotnej statystycznie roznicy miedzy srednim tempem
# wzrostu PKB USA i Niemiec na poziomie istotnosci alfa = 0.05
#
# Przedzial ufnosci 95%: (-0.209, 2.216) - obejmuje zero, co potwierdza
# brak istotnej roznicy
#
# INTERPRETACJA KONCOWA ZADANIA 1:
# Mimo ze USA ma wyzsza srednia stope wzrostu PKB (2.19% vs 1.18%),
# roznica 1.0 punktu procentowego nie jest statystycznie istotna (p = 0.102).
# Wynik jest bliski granicy istotnosci - przy wiekszej probie lub dluzsym
# okresie obserwacji roznica moglaby okazac sie istotna.
# Obie gospodarki podlegaja podobnym cyklom koniunkturalnym (kryzysy 2008-2009
# i 2020), co zwieksza wariancje wewnatrz grup i utrudnia wykrycie roznic.

# dodatkowo: test nieparametryczny Wilcoxona (bo USA nie ma rozkladu normalnego)
wilcox.test(gdp_us, gdp_de)

# WYNIK TESTU WILCOXONA (MANN-WHITNEY):
# Test nieparametryczny potwierdza wynik testu t - brak istotnej roznicy


# =============================================================================
# Zadanie 2: Porownanie srednich kilku zmiennych (ANOVA)
# Porownujemy roczne tempo wzrostu PKB (GDP growth, %) dla 4 krajow:
# USA, Niemcy, Polska, Japonia (2000-2023)
# Dane realne z World Bank API
# =============================================================================

# pobieramy dane dla dodatkowych krajow
url_pl <- "https://api.worldbank.org/v2/country/POL/indicator/NY.GDP.MKTP.KD.ZG?format=json&date=2000:2023&per_page=50"
url_jp <- "https://api.worldbank.org/v2/country/JPN/indicator/NY.GDP.MKTP.KD.ZG?format=json&date=2000:2023&per_page=50"

data_pl <- fromJSON(url_pl)[[2]]
data_jp <- fromJSON(url_jp)[[2]]

gdp_pl <- as.numeric(data_pl$value)
gdp_jp <- as.numeric(data_jp$value)
gdp_pl <- gdp_pl[!is.na(gdp_pl)]
gdp_jp <- gdp_jp[!is.na(gdp_jp)]

# tworzymy ramke danych w formacie dlugim (potrzebne do ANOVA)
df <- data.frame(
  value = c(gdp_us, gdp_de, gdp_pl, gdp_jp),
  country = factor(rep(c("USA", "Germany", "Poland", "Japan"),
                       c(length(gdp_us), length(gdp_de), length(gdp_pl), length(gdp_jp))))
)

# statystyki opisowe dla kazdego kraju
cat("\n=== STATYSTYKI OPISOWE - 4 KRAJE ===\n")
cat("Srednie roczne tempo wzrostu PKB:\n")
print(by(df$value, df$country, mean))
cat("\nOdchylenia standardowe:\n")
print(by(df$value, df$country, sd))

# WYNIKI STATYSTYK OPISOWYCH:
# Srednie: Poland = 3.667% (najwyzsza), USA = 2.185%, Germany = 1.181%, Japan = 0.735% (najnizsza)
# SD: Germany = 2.334% (najwyzsza zmiennosc), Poland = 2.229%, Japan = 2.086%, USA = 1.798%
# Polska wyroznia sie zdecydowanie najwyzszym srednim tempem wzrostu PKB
# Japonia ma najnizsze srednie tempo wzrostu - typowe dla dojrzalej gospodarki

# test normalnosci Lilliefors dla kazdej grupy
# H0: rozklad jest normalny
# H1: rozklad nie jest normalny

by(df$value, df$country, lillie.test)

# WYNIKI TESTU NORMALNOSCI LILLIEFORS:
# Germany: D = 0.160, p = 0.115 -> p > 0.05, rozklad normalny
# Japan:   D = 0.200, p = 0.014 -> p < 0.05, rozklad NIE normalny!
# Poland:  D = 0.126, p = 0.416 -> p > 0.05, rozklad normalny
# USA:     D = 0.198, p = 0.016 -> p < 0.05, rozklad NIE normalny!
#
# UWAGA: Dwa kraje (USA i Japonia) nie maja rozkladu normalnego.
# Jest to spowodowane pandemicznym rokiem 2020 (gwaltowny spadek) i odbiciem 2021.
# ANOVA jest jednak odporny na umiarkowane odchylenia od normalnosci,
# szczegolnie gdy grupy sa rowne liczebnie (tutaj kazda po 24 obs.).
# Jako dodatkowe zabezpieczenie stosujemy rowniez test Kruskala-Wallisa.

# test jednorodnosci wariancji Bartletta
# H0: wariancje we wszystkich grupach sa rowne
# H1: co najmniej jedna wariancja jest rozna

bartlett.test(df$value, df$country)

# WYNIKI TESTU BARTLETTA:
# K-squared = 1.678, df = 3, p-value = 0.642
# p-value = 0.642 > 0.05, NIE odrzucamy H0
# Wariancje we wszystkich 4 krajach sa jednorodne (podobne)
# Zalozenie homogenicznosci wariancji dla ANOVA jest spelnione

# jednoczynnikowa analiza wariancji (ANOVA)
# H0: srednie we wszystkich grupach sa rowne (mu_US = mu_DE = mu_PL = mu_JP)
# H1: co najmniej jedna srednia jest rozna

anova <- aov(df$value ~ df$country)
summary(anova)

# WYNIKI ANOVA:
# F = 9.016, p-value = 2.7e-05 (0.000027)
# p-value < 0.05 (a nawet < 0.001!), ODRZUCAMY H0
# Istnieja statystycznie istotne roznice miedzy srednim tempem wzrostu PKB
# w poszczegolnych krajach. Wartosc F = 9.016 wskazuje, ze zmiennosc miedzy
# grupami jest istotnie wieksza niz zmiennosc wewnatrz grup.

# analiza post-hoc Tukeya - sprawdzamy ktore pary krajow sie roznia
TukeyHSD(anova)

# WYNIKI TESTU POST-HOC TUKEYA (HSD):
# Japan-Germany:  diff = -0.447, p adj = 0.885 -> NIE istotne
# Poland-Germany: diff = +2.486, p adj = 0.0006 -> ISTOTNE ***
# USA-Germany:    diff = +1.004, p adj = 0.362 -> NIE istotne
# Poland-Japan:   diff = +2.932, p adj = 0.00004 -> ISTOTNE ***
# USA-Japan:      diff = +1.450, p adj = 0.090 -> NIE istotne (blisko granicy)
# USA-Poland:     diff = -1.482, p adj = 0.080 -> NIE istotne (blisko granicy)
#
# INTERPRETACJA KONCOWA ZADANIA 2:
# Stwierdzono statystycznie istotne roznice w srednim tempie wzrostu PKB
# miedzy badanymi krajami (ANOVA: F=9.016, p=0.000027).
#
# Test Tukeya wykazal, ze:
# - Polska rozni sie ISTOTNIE od Niemiec (p=0.0006) i Japonii (p=0.00004)
#   Polska ma istotnie wyzsze tempo wzrostu PKB niz te kraje.
# - Roznice USA-Japonia (p=0.09) i USA-Polska (p=0.08) sa bliskie granicy
#   istotnosci, ale formalnie nieistotne na poziomie alfa=0.05.
# - Niemcy i Japonia nie roznia sie istotnie (p=0.885)
#
# Wniosek ekonomiczny: Polska jako gospodarka rozwijajaca sie (emerging market)
# ma statystycznie istotnie wyzsze tempo wzrostu niz dojrzale gospodarki
# Niemiec i Japonii. Jest to zgodne z teoria konwergencji ekonomicznej.

# dodatkowo: test Kruskala-Wallisa (nieparametryczny odpowiednik ANOVA)
# stosujemy bo USA i Japonia nie maja rozkladu normalnego
kruskal.test(df$value ~ df$country)

# WYNIK TESTU KRUSKALA-WALLISA:
# Test nieparametryczny potwierdza wynik ANOVA - istnieja istotne roznice


# =============================================================================
# Zadanie 3: Test A/B dla prob zaleznych (parowy test t)
# Porownujemy stopy zwrotu indeksu WIG20 przed i po wprowadzeniu
# programu Pracowniczych Planow Kapitalowych (PPK) - dane sparowane miesieczne
# Dane symulowane (proby zalezne wymagaja sparowanych obserwacji)
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
cat("\n=== STATYSTYKI OPISOWE - A/B TEST ===\n")
cat("Srednia przed PPK:", round(mean(df_task3$before), 4), "%\n")
cat("Srednia po PPK:", round(mean(df_task3$after), 4), "%\n")
cat("Srednia roznic:", round(mean(df_task3$diff), 4), "pp\n")

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
# Przedzial ufnosci 95% dla sredniej roznicy: (-0.345, 0.946) - obejmuje zero

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
