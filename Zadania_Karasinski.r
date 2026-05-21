install.packages("nortest")
library(nortest)

# =============================================================================
# Zadanie 1: Porownanie srednich dwoch zmiennych (test t dla prob niezaleznych)
# Porownujemy miesieczne stopy zwrotu akcji PKO BP i CD Projekt z GPW
# =============================================================================

# ustawiamy ziarno dla powtarzalnosci wynikow
set.seed(42)

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

# test normalnosci Lilliefors (Kolmogorov-Smirnov) dla obu zmiennych
# H0: rozklad jest normalny
# H1: rozklad nie jest normalny

lillie.test(df_task1$PKO)
lillie.test(df_task1$CDR)

# jesli p-value > 0.05 to nie odrzucamy H0 - rozklad jest normalny

# test rownosci wariancji (test F)
# H0: wariancje sa rowne
# H1: wariancje nie sa rowne

var.test(df_task1$PKO, df_task1$CDR)

# jesli p-value < 0.05 to odrzucamy H0 - wariancje sa rozne
# CDR ma wyzsza zmiennosc niz PKO, wiec spodziewamy sie odrzucenia H0

# test t-Studenta dla prob niezaleznych
# H0: srednie sa rowne (mu_PKO = mu_CDR)
# H1: srednie nie sa rowne (mu_PKO != mu_CDR)
# var.equal = FALSE bo wariancje sa rozne (test Welcha)

t.test(df_task1$PKO, df_task1$CDR, var.equal = FALSE)

# interpretacja: jesli p-value < 0.05 to odrzucamy H0
# oznacza to statystycznie istotna roznice w srednich stopach zwrotu PKO i CDR


# =============================================================================
# Zadanie 2: Porownanie srednich kilku zmiennych (ANOVA)
# Porownujemy srednie stopy zwrotu spolek z 4 sektorow GPW:
# banki, energetyka, gry, media
# =============================================================================

set.seed(123)

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

# test normalnosci Lilliefors dla kazdej grupy
# H0: rozklad jest normalny
# H1: rozklad nie jest normalny

by(df_task2$value, df_task2$sector, lillie.test)

# test jednorodnosci wariancji Bartletta
# H0: wariancje we wszystkich grupach sa rowne
# H1: co najmniej jedna wariancja jest rozna

bartlett.test(df_task2$value, df_task2$sector)

# jesli p-value > 0.05 to nie odrzucamy H0 - wariancje sa jednorodne
# mozemy stosowac ANOVA

# jednoczynnikowa analiza wariancji (ANOVA)
# H0: srednie we wszystkich grupach sa rowne
# H1: co najmniej jedna srednia jest rozna

anova <- aov(df_task2$value ~ df_task2$sector)
summary(anova)

# jesli p-value < 0.05 to odrzucamy H0 - istnieja istotne roznice miedzy sektorami

# analiza post-hoc Tukeya - sprawdzamy ktore pary sektorow sie roznia
TukeyHSD(anova)

# interpretacja: porownania par z p adj < 0.05 wskazuja na istotne roznice
# spodziewamy sie, ze sektor gier rozni sie od pozostalych (wyzsza srednia i wariancja)


# =============================================================================
# Zadanie 3: Test A/B dla prob zaleznych (parowy test t)
# Porownujemy stopy zwrotu indeksu WIG20 przed i po wprowadzeniu
# programu Pracowniczych Planow Kapitalowych (PPK) - dane sparowane miesieczne
# =============================================================================

set.seed(2024)

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

# test normalnosci roznic (wymagany dla parowego testu t)
# H0: roznice maja rozklad normalny
# H1: roznice nie maja rozkladu normalnego

lillie.test(df_task3$diff)

# jesli p-value > 0.05 to nie odrzucamy H0 - roznice sa normalne
# mozemy stosowac parowy test t

# parowy test t-Studenta
# H0: srednia roznic = 0 (brak efektu PPK)
# H1: srednia roznic != 0 (jest efekt PPK)

t.test(df_task3$after, df_task3$before, paired = TRUE)

# alternatywnie: test nieparametryczny Wilcoxona dla prob zaleznych
# stosujemy gdy roznice nie maja rozkladu normalnego

wilcox.test(df_task3$after, df_task3$before, paired = TRUE)

# interpretacja: jesli p-value < 0.05 to odrzucamy H0
# oznacza to, ze wprowadzenie PPK mialo statystycznie istotny wplyw
# na stopy zwrotu WIG20
