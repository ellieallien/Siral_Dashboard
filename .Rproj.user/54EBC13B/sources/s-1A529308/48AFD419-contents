#' Fonction qui renvoie les dernieres mises a jour FOREX pour une paire de devises
#' @param currency_s la devise de depart, indiquee par un vecteur charactere de 3 lettres
#' @param currency_f la devise d'arrivee, indiquee par un vecteur charactere de 3 lettres
#' @return un taux d'echange
taux_dechange <- function(currency_s, currency_f){
  if(!length(paste0(currency_s, currency_f) == 6))stop("Les deux devises doivent etre specifiees utilisant un code a trois lettres. Par ex: EUR ou USD")
  rate <- getQuote(paste0(currency_s,currency_f, "=X"))$Last
}

#' Fonction qui renvoie les dernieres mises a jour FOREX pour une liste de devises 
#' @param vecteur_devise un vecteur avec la devise de chaque element
#' @param currency_f la devise d'arrivee, indiquee par un vecteur charactere de 3 lettres
#' @param forex_codes une liste qui relie les devises dans vecteur_devise aux standards trois lettres (si ils ne le sont pas deja)
#' @return 
taux_dechanges <- function(vecteur_devise, currency_f, forex_codes = NULL){
  if(!is.null(forex_codes)){
    vecteur_devise <- recode(vecteur_devise, !!!forex_codes)
  }
  #Telecharger les taux dechange uniques
  rate = c()
  for(i in unique(vecteur_devise)){
    if(!is.na(i) & !(i == "")){
      rate[i] <- getQuote(paste0(i,currency_f, "=X"))$Last
      }
    }
  taux_dechange = recode(vecteur_devise, !!!rate)
  return(rate)
}


#' Fonction qui convertit des prix de differentes devises a une devise clef, en utilisant les dernieres mises a jour FOREX
#' @param vecteur_devise un vecteur avec la devise de chaque prix
#' @param devise_finale la devise d'arrivee, indiquee par un vecteur charactere de 3 lettres
#' @param prix un vecteur numerique des prix
#' @return un vecteur de la meme longueur que prix converti dans la devise devise_finale
harmonisation_devises <- function(vecteur_devise, devise_finale, prix, forex_codes){
  if(!is.numeric(prix))stop("L'argument prix comporte des elements non-numeriques")
  taux = taux_dechanges(vecteur_devise, devise_finale, forex_codes) 
  vecteur_devise <- recode(vecteur_devise, !!!forex_codes)
  vecteur_final <- c()
  for(i in c(1:length(vecteur_devise))){
  vecteur_final[i] = taux[vecteur_devise[i]] * as.numeric(prix[i])
  }
  return(vecteur_final)
}



