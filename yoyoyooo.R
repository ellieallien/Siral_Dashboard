### Voici le code qu'il faudra executer 
library(lubridate)
library(composr)
library(quantmod)

data = read.csv2("./input/siral_data_Eliora.csv", stringsAsFactors = F)

data$mois = month(dmy(data$Date.du.rapport))

data$trim = data$mois %>% recode(`4` = "Deuxieme",
                                 `5` = "Deuxieme",
                                 `6` = "Deuxieme",
                                 `7` = "Troisieme",
                                 `8` = "Troisieme",
                                 `9` = "Troisieme",
                                 `10` = "Quatrieme",
                                 `11` = "Quatrieme",
                                 `12` = "Quatrieme")


# UTILISATION 
#nettoyge de la colonne devise
data$Devise.du.projet %<>% gsub("[\\$,]","",.)
data$Devise[data$Devise == ""] <- data$Devise.du.projet[data$Devise == ""]
data$Devise %<>% gsub("_","",.)


# Definition des codes forex
forex_codes <- c(us = "USD", 
                 USprojet = "USD",
                 euro = "EUR",
                 livresterling = "GBP")

# Mettre tous les prix en dollars 
data$prix_USD <- harmonisation_devises(vecteur_devise = data$Devise,
                      devise_finale = "USD",
                      prix = data$Montant.total.du.projet,
                      forex_codes= forex_codes)
