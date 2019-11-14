rm(list=ls()) 
require(here)
#setwd("/home/auri/Facultad/")
PATH <- "/home/luciano/V"
setwd(PATH)
library(ncdf4)
#path<-"/home/auri/Descargas/"

tref = nc_open(paste(PATH, "tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
names(tref$var)
temp = ncvar_get(tref, "tref")

# serie desde enero '82 -> noviembre 2012 
# NO FALTAN MESES, SOLO LO LEE RARO NCVIEW ---> USAR NCDUMP POR TERMINAL
# TOMAR DESDE MARZO DEL '82 HASTA NOV DEL 2011?? ---> uso feb del 2012 asi me quedan misma cantidad de estaciones.  ES IMPORTANTE O DA IGUAL??
lat = ncvar_get(tref, "Y")
lon = ncvar_get(tref, "X")

temp = temp[,,3:371] 

lon2 = c(which(lon==275):which(lon==330))
lat2 = c(which(lat==-60):which(lat==15))

temp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12)) # CON ESTA DIMENCION YA CUMPLE LO DE ARRIBA.

for(j in 1:12){
  for (i in 0:29){
    temp_estaciones[,,1+i,j] = temp[lon2, lat2, j+12*i]
  }
}


estaciones_p_a = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
for(i in 1:9){
  estaciones_p_a[,,,i-ceiling(i/2)] = apply(temp_estaciones[,,,1:i+3], c(1,2,3), mean)
  i = i + 3
}

estaciones_prom = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom[,,i] = apply(estaciones_p_a[,,,i], c(1,2), mean)
}
 
# probar graficos
