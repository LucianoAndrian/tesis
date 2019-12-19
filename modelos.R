
rm(list=ls())
require(here)
library(ncdf4)
source("mapa_obs.R")
#source("r_modelos.R")
source("r2_modelos.R")

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]


nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPv2")
r = c(10, 10, 12, 12, 4, 28, 10, 10) # NCEP es tiene 24 miembros pero aparecen 28 ...?, para que la funcion no tenga problemas es necesario que r = 28

for( i in 1:length(nombres)){
  modelo = modelos_rx(nombres[i], r[i])
}


#modelos mm por dia!! multiplicar por 30

CM4i =modelos_rx2("CMC-CanCM4i", lon2, lat2, 10)

mapa_obs(lista = CM4i[[1]], titulo = "prono CM4i", nombre_fig = "CM4i_T", escala = c(-5,40)
         , label_escala = "°C", resta = 273, brewer = "Spectral", revert = "si", niveles = 9, contour = "si", lon2, lat2)

mapa_obs(lista = CM4i[[2]], titulo = "prono CM4i - sd-T", nombre_fig = "CM4i_sd_0_t_rx", escala = c(0,3)
         , label_escala = "mm", resta = 0, brewer = "Spectral", revert = "no", niveles = 9, contour = "si", lon2, lat2)

mapa_obs(lista = CM4i[[3]], titulo = "prono CM4i - PP", nombre_fig = "CM4i_pp2", escala = c(0,500)
         , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2)

mapa_obs(lista = CM4i[[4]], titulo = "prono CM4i - sd -PP", nombre_fig = "CM4i_sd_pp", escala = c(0,138)
         , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2)


NCEP = modelos_rx2("NCEP-CFSv2", lon2, lat2, 28)


mapa(lista = NCEP[[1]], titulo = "prono NCEP  T", nombre_fig = "NCEP_T3", escala = c(-5,40)
         , label_escala = "ºC", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si", lon2, lat2)



ccsm4 = modelos_rx("COLA-CCSM4", lon2, lat2, 10)
