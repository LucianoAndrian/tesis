
rm(list=ls())
require(here)
library(ncdf4)
source("mapa_obs.R")
source("r2_modelos.R")

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]


nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")
r = c(10, 28, 12, 12, 4, 28, 10, 20) # NCEP es tiene 24 miembros pero aparecen 28 ...?, para que la funcion no tenga problemas es necesario que r = 28
                                     # GDFL-CM2P1 tiene 28 miembros para pp, poniendo r 28 no afecta a los calculos de de temp con 10 miembros (NA)    

# seleccion y guardado de datos NC en carpeta ncfiles
for( i in 1:length(nombres)){
  modelos_rx2(nombres[i], r[i])
}

for(i in 1:length(nombres)){
  v = mean_sd(nombres[i])
  
  # imagenes
  # T
  mapa(lista = v[[1]], titulo = paste("Promedio Temperatura 1982 - 2010 - ", nombres[i], sep = ""), nombre_fig = paste("temp_",nombres[i], sep = ""), escala = c(-5,40 ) 
       ,label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11, contour = "si", lon2, lat2, c(-5,0,5,10,15,20,25,30,35,40),"/salidas/ensemble/")
  
  # SD - T
  mapa(lista = v[[2]], titulo = paste(" SD Temperatura 1982 - 2010 - ", nombres[i], sep = ""), nombre_fig = paste("SD-temp_",nombres[i], sep = ""), escala = c(0,3) 
       ,label_escala = "°C", resta = 0, brewer = "RdYlBu", revert = "si", niveles = 9, contour = "si", lon2, lat2, c(0,0.5,1,1.5,2,2.5,3),"/salidas/ensemble/")
  
  # PP
  
  mapa(lista = v[[3]], titulo = paste("Promedio Precipitaciòn 1982 - 2010 - ", nombres[i], sep = ""), nombre_fig = paste("pp_",nombres[i], sep = ""), escala = c(0, 500) 
       ,label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2, c(0, 50, 100, 150, 200, 250, 350, 300,350, 400, 450, 500),"/salidas/ensemble/")
  
  # SD
  
  mapa(lista = v[[4]], titulo = paste("SD Precipitaciòn 1982 - 2010 - ", nombres[i], sep = ""), nombre_fig = paste("SD-pp_",nombres[i], sep = ""), escala = c(0, 150) 
       ,label_escala = "mm", resta = 0, brewer = "Spectral", revert = "si", niveles = 9, contour = "si", lon2, lat2, c(0, 25, 50, 75, 100, 125, 150 ),"/salidas/ensemble/")
 
}

# escalas, colores, VER. 
# T ok
# SD_T valores muy distinos para cada ensamble
# PP ven gral, valores muy altos en la cordillera
# SD_PP valores puntuales muy altos atenuan el resto, diferencia entre ensambles

#GFDL FLOR A06 vs B01

gfdl = c("GFDL-FLOR-A06", "GFDL-FLOR-B01")

A06 = mean_sd(gfdl[1])
B01 = mean_sd(gfdl[2])

dif_t = A06[[1]] - B01[[1]]

dif_pp = A06[[3]] - B01[[3]]


# imagenes
# T

mapa(lista = v[[1]], titulo = paste("Promedio Temperatura 1982 - 2010 - GFDL-FLOR-A06", sep = ""), nombre_fig = paste("TEMP_GFDL_FLOR_A06", sep = ""), escala = c(-5,40 ) 
     ,label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11, contour = "si", lon2, lat2, c(-5,0,5,10,15,20,25,30,35,40),"/salidas/ensemble/GFDL/")

mapa(lista = v2[[1]], titulo = paste("Promedio Temperatura 1982 - 2010 - GFDL-FLOR-B01", sep = ""), nombre_fig = paste("TEMP_GFDL_FLOR_B01", sep = ""), escala = c(-5,40 ) 
     ,label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11, contour = "si", lon2, lat2, c(-5,0,5,10,15,20,25,30,35,40),"/salidas/ensemble/GFDL/")



mapa(lista = dif_t, titulo = paste("Diferencia Promedio Temperatura GFDL-FLOR A06 - B01", sep = ""), nombre_fig = paste("dif_temp_gfdl", sep = ""), escala = c(-0.3, 0.4) 
     ,label_escala = "°C", resta = 0, brewer = "RdBu", revert = "si", niveles = 11, contour = "si", lon2, lat2, c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),"/salidas/ensemble/GFDL/")
  
# PP
  
mapa(lista = dif_pp, titulo = paste("Diferencia Promedio PP GFDL-FLOR A06 - B01 - ", sep = ""), nombre_fig = paste("dif_pp_gfdl", sep = ""), escala = c(-15, 15) 
     ,label_escala = "mm", resta = 0, brewer = "BrBG", revert = "no", niveles = 11, contour = "si", lon2, lat2, c(-15, -10, -5, 0, 5, 10, 15),"/salidas/ensemble/GFDL/")


mapa(lista = v[[3]], titulo = paste("Promedio Temperatura 1982 - 2010 - GFDL-FLOR-A06", sep = ""), nombre_fig = paste("PP_GFDL_FLOR_A06", sep = ""), escala = c(0, 500) 
     ,label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2, c(0, 50, 100, 150, 200, 250, 350, 300,350, 400, 450, 500),"/salidas/ensemble/GFDL/")


mapa(lista = v2[[3]], titulo = paste("Promedio Temperatura 1982 - 2010 - GFDL-FLOR-B01", sep = ""), nombre_fig = paste("PP_GFDL_FLOR_B01", sep = ""), escala = c(0, 500) 
     ,label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2, c(0, 50, 100, 150, 200, 250, 350, 300,350, 400, 450, 500),"/salidas/ensemble/GFDL/")
