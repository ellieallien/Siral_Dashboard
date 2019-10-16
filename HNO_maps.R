# CARTES HNO 

chiffres_carto = read.csv2("./Eliora Henzler/GitHub/Siral_dashboard/input/dashboard_n.csv", stringsAsFactors = F)
source("./Eliora Henzler/GitHub/Siral_dashboard/nettoyage.R")

names(chiffres_carto)[1] = "Province"
names(chiffres_carto)[5] = "Zone de Sante"
names(chiffres_carto)[6] = "Pcode_ZS"

carta = carte_complete(donnees_groupees = chiffres_carto, 
                       shapefile = Zone_sante, 
                       nom.admin.donnees = "Pcode_ZS",
                       nom.admin.shapefile = "Pcode", 
                       variable = c("Besoins_345", "CIBLE_345", "Besoins_total", "Cible_totale", "Province") )


couleurs <- couleurs_pour_carte("Oranges", nombre = 6)


carta$Besoins_345 <- cut(carta$Besoins_345, breaks =  c(0, 25000, 500000, 75000, 100000,125000, 150000),  
                         labels = c("0", "25000", "500000", "75000", "100000","125000"),
                         right = FALSE)


Provincez  <- cbind(Province, st_coordinates(st_centroid(Province)))
texto = geom_text(data = Provincez, aes(X, Y, label = NOM))

ggplot(carta) + 
  geom_sf(aes(fill = Besoins_345, color = Besoins_345)) +
  labs(fill = "Personnes dans le besoin", color = NA) +
  scale_fill_manual(
    values = couleurs) +
  scale_color_manual(values = couleurs) +
  geom_sf(data = Provincez, fill = NA) +
  geom_sf_text(aes(label = NOM))
  
  

keep_unique_rows = function(carta, vars_to_check){
  slim_jim = carta %>% select(!! vars_to_check) %>% distinct %>% as.data.frame
  return(slim_jim)
}


b = keep_unique_rows(carta, "Besoins_345")
  
carta$Besoins_total <- cut(carta$Besoins_total, breaks =  c(0, 25000, 500000, 75000, 100000,125000, 150000),  
                           labels = c("0", "25000", "500000", "75000", "100000","125000"),
                           right = FALSE)

Besoins = ggplot(carta) + 
    geom_sf(aes(fill = Besoins_total, color = Besoins_total)) +
    labs(fill = "Personnes dans le besoin")
  scale_fill_manual(
    values = couleurs) +
    scale_color_manual(values = couleurs)
  
