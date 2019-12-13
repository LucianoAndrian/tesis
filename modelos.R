
rm(list=ls())
require(here)
library(ncdf4)
source("mapa_obs.R")
source("r_modelos.R")

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

#modelos mm por dia!! multiplicar por 30

CM4i =modelos_r10("CMC-CanCM4i", lon2, lat2)

mapa_obs(lista = CM4i[[1]], titulo = "prono CM4i", nombre_fig = "CM4i", escala = c(-5,40)
         , label_escala = "°C", resta = 273, brewer = "Spectral", revert = "si", niveles = 9, contour = "si", lon2, lat2)

mapa_obs(lista = CM4i[[2]], titulo = "prono CM4i - sd-T", nombre_fig = "CM4i_sd_t", escala = c(0,3)
         , label_escala = "mm", resta = 0, brewer = "Spectral", revert = "no", niveles = 9, contour = "si", lon2, lat2)

mapa_obs(lista = CM4i[[3]], titulo = "prono CM4i - PP", nombre_fig = "CM4i_pp", escala = c(0,500)
         , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2)

mapa_obs(lista = CM4i[[4]], titulo = "prono CM4i - sd -PP", nombre_fig = "CM4i_sd_pp", escala = c(0,138)
         , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2)


