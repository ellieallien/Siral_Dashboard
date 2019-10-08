### Voici le code qu'il faudra executer pour preparer 
library(lubridate) #pour un traitement plus smooth des dates
library(composr) #pour recoder
library(quantmod) #pour les taux dechange
library(magrittr) #pour la lisibilite
library(tidyr) #tidyverse
library(dplyr) #tidyverse
library(purrr) #tidyverse
library("RColorBrewer") #couleurs plus cool 

#Chargement des donnees
data = read.csv2("./input/logements_donnes.csv", stringsAsFactors = F)
questions = read.csv2("./input/questions.csv", stringsAsFactors = F)
names(questions)[1] = "type"
choices = read.csv2("./input/choices.csv", stringsAsFactors = F)
names(choices)[1] = "list_name"

questionnaire = koboquest::load_questionnaire(data = data, 
                                              questions = questions, 
                                              choices = choices,
                                              choices.label.column.to.use = "label_french")

targets = read.csv2("./input/targets.csv", stringsAsFactors = F)
names(targets)[1] = "Province"
targets$target_hno_2019 %<>% gsub(" ", "", ., fixed = T) %>% as.numeric
targets$Province %<>% gsub("-", " ", ., fixed = T) %>% tolower
targets %<>% select(Province, target_hno_2019) 
 
source(file = "./devises.R")
source(file = "./construction_carto.R")
source(file = "./geodata.R")

data$mois = month(dmy(data$today))

# Definition des trimestres pour desagreger plus tard
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
data$devise_projet %<>% gsub("[\\$,]","",.)
# data$Devise[data$Devise == ""] <- data$Devise.du.projet[data$Devise == ""]
data$devise_projet %<>% gsub("_","",.)

#rapporter cout du projet pour ceux ou le cout de l'activite est vide
data$Montant.total.couvrant.l.activite.rapportee[is.na(data$Montant.total.couvrant.l.activite.rapportee)] <- data$Montant.total.du.projet[is.na(data$Montant.total.couvrant.l.activite.rapportee)] 

# Definition des codes forex
forex_codes <- c(us = "USD", 
                 USprojet = "USD",
                 europrojet = "EUR",
                 lsprojet = "GBP")

# Mettre tous les prix en dollars 
data$prix_USD <- harmonisation_devises(vecteur_devise = data$devise_projet,
                      devise_finale = "USD",
                      prix = data$montant,
                      forex_codes= forex_codes)


#Fonction pour les plus beaux labels
map_to_labels_outside <- function(vector, variable.name , questionnaire){
  vector %<>% lapply(function(x){
    questionnaire$question_get_choice_labels(x, 
                                             variable.name = variable.name)
    }%>% paste(collapse = ","))  %>% unlist %>% as.character
  return(vector)
}
