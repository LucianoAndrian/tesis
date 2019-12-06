rm(list=ls()) 
require(here)
#setwd("/home/auri/Facultad/")
library(ncdf4)

source("mapa_obs.R")
ruta = "/datos/osman/nmme/monthly"

tref = nc_open(paste(ruta,"tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
names(tref$var)
temp = ncvar_get(tref, "tref")

# serie desde enero '82 -> noviembre 2012 
# NO FALTAN MESES, SOLO LO LEE RARO NCVIEW ---> USAR NCDUMP POR TERMINAL
# TOMAR DESDE MARZO DEL '82 HASTA NOV DEL 2011?? ---> uso feb del 2012 asi me quedan misma cantidad de estaciones.  ES IMPORTANTE O DA IGUAL??
lat = ncvar_get(tref, "Y")
lon = ncvar_get(tref, "X")

temp = temp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:371] 

lon2 = lon[which(lon==275):which(lon==330)]
lat2 = lat[which(lat==-60):which(lat==15)]

temp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12)) # CON ESTA DIMENCION YA CUMPLE LO DE ARRIBA.

for(j in 1:12){
  for (i in 0:29){
    temp_estaciones[,,1+i,j] = temp[ , , j+12*i]
  }
}


# Estaciones

estaciones_p_a_t = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
i=1
while(i<=4){
  estaciones_p_a_t[,,,i] = apply(temp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}


estaciones_prom_t = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom_t[,,i] = apply(estaciones_p_a_t[,,,i], c(1,2), mean)
}

#estaciones_prom_t[which(estaciones_prom_t>310)] = NA
#estaciones_prom_t[which(estaciones_prom_t<265)] = NA


mapa_obs(lista = estaciones_prom_t, titulo = "Temperatura", nombre_fig = "temp", escala = c(-5,35 ) 
         ,label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11, contour = "si", lon2, lat2)

mask = estaciones_prom_t[,,1]  
mask[which(!is.na(mask))]=1



# sd
standar_d_t = array(NA, dim = c(length(lon2), length(lat2), 4))
for( i in 1:4 ){
  standar_d_t[,,i] = apply(estaciones_p_a_t[,,,i], c(1,2), sd)
}


mapa_obs(lista = standar_d_t, titulo = "sd - TEMP", nombre_fig = "sd_TEMP", escala = c(0,1.8)
         , label_escala = "mm", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9, contour = "si", lon2, lat2)



### PP ###

prec = nc_open(paste(ruta, "prec_monthly_nmme_cpc.nc", sep = "/"))
names(prec$var)
pp = ncvar_get(prec, "prec")

# serie desde enero '82 -> noviembre 2012 
# NO FALTAN MESES, SOLO LO LEE RARO NCVIEW ---> USAR NCDUMP POR TERMINAL
# TOMAR DESDE MARZO DEL '82 HASTA NOV DEL 2011?? ---> uso feb del 2012 asi me quedan misma cantidad de estaciones.  ES IMPORTANTE O DA IGUAL??
lat = ncvar_get(prec, "Y")
lon = ncvar_get(prec, "X")

pp = pp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:350] 

lon2 = lon[which(lon==275):which(lon==330)]
lat2 = lat[which(lat==-60):which(lat==15)]

pp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 28, 12)) # CON ESTA DIMENCION YA CUMPLE LO DE ARRIBA.

for(j in 1:12){
  for (i in 0:27){
    pp_estaciones[,,1+i,j] = pp[ , , j+12*i]
  }
}


# Estaciones

estaciones_p_a_pp = array(NA, dim = c(length(lon2), length(lat2), 28, 4))
i=1
while(i<=4){
  estaciones_p_a_pp[,,,i] = apply(pp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}


estaciones_prom_pp = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom_pp[,,i] = apply(estaciones_p_a_pp[,,,i], c(1,2), mean)*mask
}



mapa_obs(lista = estaciones_prom_pp, titulo = "PP", nombre_fig = "pp", escala = c(0,500)
         , label_escala = "mm", resta = 0, brewer = "GnBu",revert = 0, niveles = 9, contour = "si", lon2, lat2)


## sd
standar_d_pp = array(NA, dim = c(length(lon2), length(lat2), 4))
for( i in 1:4 ){
  standar_d_pp[,,i] = apply(estaciones_p_a_pp[,,,i], c(1,2), sd)*mask
}


satandar_d_pp = standar_d_pp[which(standar_d_pp>=100)]=90   # veeer # contour fill parece que requiere valores mas altos del maximos para completarel contorno

mapa_obs(lista = standar_d_pp, titulo = "sd - PP", nombre_fig = "sd_pp", escala = c(0,100)
         , label_escala = "mm", resta = 0, brewer = "YlOrRd",revert = "no", niveles = 9, contour = "si", lon2, lat2)


