# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("devtools")
# install.packages("sf")
# install.packages("shiny")
# devtools::install_github("ellieallien/hypegrammaR")

library(hypegrammaR)
library(devtools)
library("sf")

zname <- "C:/Users/Admin/Documents/Eliora Henzler/GitHub/SIRAL_dashboard/input/zone_sante.shp"
pname <- "C:/Users/Admin/Documents/Eliora Henzler/GitHub/SIRAL_dashboard/input/Province26.shp"
## [1] "/home/travis/R/Library/sf/shape/nc.shp"
Zone_sante <- st_read(zname, stringsAsFactors = F)
Province <- st_read(pname, stringsAsFactors = F)

Province$NOM <- tolower(gsub("[^a-zA-Z0-9_]", "\\ ", Province$NOM ))
Province$NOM
