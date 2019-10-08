comptage_par_admin  <- function(df, 
                                   nom.admin = Province,
                                   nom.partenaires = "Partenaires"){
  quadmin = enquo(nom.admin)
  partenaires_p = df %>%  dplyr::group_by(!! quadmin) %>% 
    nest() %>%  
    mutate(Partenaires = map(data, function(df)
      length(unique(df[[nom.partenaires]])))  %>% unlist) %>% select(Partenaires, Province) %>% as.data.frame

  return(partenaires_p)
}


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
 
  Geometrie_groupee[[variable]][is.na(Geometrie_groupee[[variable]])] = 0
  
  return(Geometrie_groupee)
}

couleurs_pour_carte <- function(gradient, nombre, zero = "#D3D3D3"){
  my.cols <- brewer.pal(nombre, gradient)
  my.cols[1] = zero
  return(my.cols)
}


