
rm(list=ls())
require(here)
library(ncdf4)
source("mapa_obs.R")
#source("r_modelos.R")
source("r2_modelos.R")

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]


nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")
r = c(10, 28, 12, 12, 4, 28, 10, 10) # NCEP es tiene 24 miembros pero aparecen 28 ...?, para que la funcion no tenga problemas es necesario que r = 28
                                     # GDFL-CM2P1 tiene 28 miembros para pp, poniendo r 28 no afecta a los calculos de de temp con 10 miembros (NA)    
for( i in 1:length(nombres)){
  modelos_rx2(nombres[i], r[i])
}


probando=mean_sd(nombres[2])


#modelos mm por dia!! multiplicar por 30


mapa(lista = T1, titulo = "prueba", nombre_fig = "prueba", escala = c(-5,40)
