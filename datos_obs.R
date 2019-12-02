rm(list=ls()) 
require(here)
#setwd("/home/auri/Facultad/")
library(ncdf4)

source("mapa_obs.R")


tref = nc_open(paste("tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
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

estaciones_p_a = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
i=1
while(i<=4){
  estaciones_p_a[,,,i] = apply(temp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}


estaciones_prom = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom[,,i] = apply(estaciones_p_a[,,,i], c(1,2), mean)
}
 

mapa_obs(lista = estaciones_prom, titulo = "Temperatura", nombre_fig = "temp", escala = c(-15,55 ) 
        ,label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11, contour = "si")

mask = estaciones_prom[,,1]  
mask[which(!is.na(mask))]=1


### PP ###

prec = nc_open(paste("prec_monthly_nmme_cpc.nc", sep = "/"))
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

estaciones_p_a = array(NA, dim = c(length(lon2), length(lat2), 28, 4))
i=1
while(i<=4){
  estaciones_p_a[,,,i] = apply(pp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}


estaciones_prom = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom[,,i] = apply(estaciones_p_a[,,,i], c(1,2), mean)*mask
}



mapa_obs(lista = estaciones_prom, titulo = "PP", nombre_fig = "pp", escala = c(0,500)
         , label_escala = "mm", resta = 0, brewer = "GnBu",revert = 0, niveles = 9, contour = "si")


# sd
s= seq(1:10)
for( i in 1:10){
  print(sqrt(sum((s-5.5)^2/9)))
  
}


