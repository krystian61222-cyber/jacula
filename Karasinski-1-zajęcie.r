install.packages("nortest")
library(nortest)
library(readxl)

returns <- read_excel("Data 1.xlsx", sheet = "Returns")

rf <- read_excel("Data 1.xlsx", sheet = "Risk_free")

avg_return <- colMeans(returns[-1], na.rm = T)
avg_rf <- colMeans(rf[-1], na.rm = T)
sd_return <- apply(returns[-1], 2, sd, na.rm = T)


sharpe <- (avg_return - avg_rf)/sd_return
sharpe

sharpe_table <- data.frame(stock = names(sharpe), value = sharpe)
row.names(sharpe_table) <- NULL

list_banks = list("ALR", "BHW", "BNP", "BOS", "GTN", "ING", "MBK", "MIL", "PEO", "PKO", "SAN", "SPL", "UCG")
list_energy = list("BDZ", "CEZ", "CLC", "ENA", "KGN", "MLS", "NVG", "OND", "PEN", "PEP", "PGE", "RAE", "TPE", "ZEP")
list_games = list("3RG", "11B", "ART", "BBT", "BCS", "BLO", "CDR", "CIG", "CRJ", "DGE", "GIF", "GOP", "HUG", "MOV", "PCF", "PLW", "RND", "SIM", "TEN", "ULG", "VVD")
list_media = list("AGO", "ATG", "CPL", "DIG", "GPP", "IMS", "KCI", "KPL", "IRQ", "MZA", "PGM", "PTW", "WPL")


df_banks <- data.frame(stock = unlist(list_banks), sector = "banks")
df_energy <- data.frame(stock = unlist(list_energy), sector = "energy")
df_games <- data.frame(stock = unlist(list_games), sector = "games")
df_media <- data.frame(stock = unlist(list_media), sector = "media")

df_sector <- rbind(df_banks, df_energy, df_games, df_media)

final <- merge(sharpe_table, df_sector, by="stock")

by(final$value, final$sector, lillie.test)

bartlett.test(final$value, final$sector)

anova <- aov(final$value ~ final$sector)

summary(anova)

TukeyHSD(anova)