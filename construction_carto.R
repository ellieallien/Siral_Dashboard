comptage_par_admin  <- function(df, 
                                   nom.admin = province,
                                   nom.partenaires = "Partenaires"){
  quadmin = enquo(nom.admin)
  qu_admin = quo_name(enquo(nom.admin))
  variables = c(qu_admin, nom.partenaires)
  partenaires_p = df %>%  dplyr::group_by(!! quadmin) %>% 
    nest() %>%  
    mutate(!! nom.partenaires := map(data, function(df)
      length(unique(df[[nom.partenaires]])))  %>% unlist) %>% select(!! variables) %>% as.data.frame

  return(partenaires_p)
}

#' Fonction qui renvoie une base de données à cartographier collée à une base de données de format "sf" 
#' @param donnees_groupees base de donnees contenant les moyennes/pourcentages/valeurs pour chaque element a cartographier
#' @param shapefile multipolygone contenant une colonne permettant de la relier aux donnees groupees
#' @param nom.admin.donnees le nom de la colonne qui donne les lieux dans 'donnees_groupees'
#' @param nom.admin.shapefile le nom de la colonne qui donne les lieux dans 'shapefile'
#' @param variable le nom de/des variables a cartographier dans 'donnees_groupees'
#' @return une base de donnees jointes; prete a etre convertie en carte avec ggplot ou arcgis
carte_complete <- function(donnees_groupees, 
                           shapefile,
                         nom.admin.donnees = "Province",
                         nom.admin.shapefile = "NOM",
                         variable = "Partenaires"){

  #check que tous les noms de colonnes dans les objets corr. 
  if(sum(donnees_groupees[nom.admin.donnees] %in% shapefile[nom.admin.shapefile])>0){
    stop("Certaines valeurs dans les donnees ne sont pas dans les bases geographiques. Merci de verifier l'orthographe")
  }
  
  variables = c(variable, 
               # nom.admin.shapefile, 
                "geometry")
  
  Geometrie_groupee = merge(y = donnees_groupees, 
                            x = shapefile, 
                            by.y = nom.admin.donnees, 
                            by.x = nom.admin.shapefile, 
                            all.x = T)  %>%  select(!! variables)
 
  # Geometrie_groupee[[variable]][is.na(Geometrie_groupee[[variable]])] = 0
  
  return(Geometrie_groupee)
}

couleurs_pour_carte <- function(gradient, nombre, zero = "#D3D3D3"){
  my.cols <- brewer.pal(nombre, gradient)
  my.cols[1] = zero
  return(my.cols)
}


