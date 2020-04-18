rm(list=ls()) 
#require(here)
#setwd("/home/auri/Facultad/")
library(ncdf4)

source("funciones.R")
ruta = "/pikachu/datos/osman/nmme/monthly"

tref = nc_open(paste(ruta,"tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
names(tref$var)
temp = ncvar_get(tref, "tref")

# serie desde enero '82 -> noviembre 2012 
# NO FALTAN MESES, SOLO LO LEE RARO NCVIEW ---> USAR NCDUMP POR TERMINAL
# TOMAR DESDE MARZO DEL '82 HASTA NOV DEL 2011?? ---> uso feb del 2012 asi me quedan misma cantidad de estaciones.  ES IMPORTANTE O DA IGUAL??
lat = ncvar_get(tref, "Y")
lon = ncvar_get(tref, "X")
nc_close(tref)
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

mapa(lista = estaciones_prom_t, titulo = "Temperatura - CPC ", nombre_fig = "temp_cpc", escala = c(-5,40 ) 
     , label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si"
     , niveles = 11, contour = "si", lon2, lat2, seq(-5, 40, by = 5), seq(-5, 40, by = 5), 15, 1500,  "/salidas/observado/")

mask = estaciones_prom_t[,,1]  
mask[which(!is.na(mask))]=1



# sd
standar_d_t = array(NA, dim = c(length(lon2), length(lat2), 4))
for( i in 1:4 ){
  standar_d_t[,,i] = apply(estaciones_p_a_t[,,,i], c(1,2), sd)
}


mapa(lista = standar_d_t, titulo = "SD - TEMP - CPC", nombre_fig = "sd_TEMP_cpc", escala = c(0,2)
     , label_escala = "ºC", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 2, by = 0.2), seq(0, 2, by = 0.1), 15, altura = 1500, "/salidas/observado/")



### PP ###

#prec = nc_open(paste(ruta, "prec_monthly_nmme_cpc.nc", sep = "/"))
prec = nc_open("/home/luciano.andrian/tesis/ncfiles/cpc_pp.1x1.nc")  # nuevo
names(prec$var)
pp = ncvar_get(prec, "precip")
lat = ncvar_get(prec, "lat")
lon = ncvar_get(prec, "lon")
nc_close(prec)

pp = pp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:362]   

pp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12)) # le pongo 30 igual. va tener NA pero sino complica los calculos mas adelante

for(j in 1:12){
  for (i in 0:29){
    pp_estaciones[,,1+i,j] = pp[ , , j+12*i]
  }
}


# Estaciones

estaciones_p_a_pp = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
i=1
while(i<=4){
  estaciones_p_a_pp[,,,i] = apply(pp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}


estaciones_prom_pp = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom_pp[,,i] = apply(estaciones_p_a_pp[,,,i], c(1,2), mean)*mask
}



mapa(lista = estaciones_prom_pp, titulo = "PP - CPC", nombre_fig = "pp-CPC", escala = c(0,500)
     , label_escala = "mm", resta = 0, brewer = "PuBuGn",revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 500, by = 50), seq(0, 500, by = 50),  15, altura = 1500, "/salidas/observado/")



## sd
standar_d_pp = array(NA, dim = c(length(lon2), length(lat2), 4))
for( i in 1:4 ){
  standar_d_pp[,,i] = apply(estaciones_p_a_pp[,,,i], c(1,2), sd)*mask
}


satandar_d_pp = standar_d_pp[which(standar_d_pp>=100)]=150   # veeer # contour fill parece que requiere valores mas altos del maximos para completarel contorno

mapa(lista = standar_d_pp, titulo = "sd - PP - CPC", nombre_fig = "sd_PP_CPC", escala = c(0,160)
     , label_escala = "mm", resta = 0, brewer = "PuBuGn",revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 160, by = 20), seq(0, 160, by = 10), 15, altura = 1500, "/salidas/observado/")




# pp2

require(fields)
library(ncdf4)

aux = nc_open("/home/luciano.andrian/tesis/X157.92.36.193.339.11.29.13.nc")
#aux2 = ncvar_get(aux, "precip")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),]
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[which(lon==275.25):which(lon==330.25), which(lat==-60.25):which(lat==15.25),27:386]
require(fields)


lon3 = lon[which(lon==275.25):which(lon==330.25)]
lat3 = lat[which(lat==-60.25):which(lat==15.25)]

pp2_int = array(NA, dim = c(56, 76, 360))

for(i in 1:360){
  
  mod = list(x = lon3, y = lat3, z = aux2[,,i])
  
  grid = list(x=seq(min(lon3), max(lon3), by = 1), y = seq(min(lat3), max(lat3), by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp2_int[,,i] = pp_aux$z
}

pp2_estaciones = array(NA, dim = c(56, 76, 30, 12))

for(j in 1:12){
  for (i in 0:29){
    pp2_estaciones[,,1+i,j] = pp2_int[ , , j+12*i]
  }
}


estaciones_p_a_pp2 = array(NA, dim = c(56, 76, 30, 4))
i=1
while(i<=4){
  estaciones_p_a_pp2[,,,i] = apply(pp2_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}

estaciones_prom_pp2 = array(NA, dim = c(56, 76, 4))

for( i in 1:4){
  estaciones_prom_pp2[,,i] = apply(estaciones_p_a_pp2[,,,i], c(1,2), mean)
}


mapa(lista = estaciones_prom_pp2, titulo = "PP - GPCC", nombre_fig = "pp_gpcc", escala = c(0,500) 
     , label_escala = "mm", resta = 0, brewer = "YlOrRd",revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 500, by = 50), seq(0, 500, by = 50),  15, altura = 1500, "/salidas/observado/")




## sd
standar_d_pp2 = array(NA, dim = c(56, 76, 4))
for( i in 1:4 ){
  standar_d_pp2[,,i] = apply(estaciones_p_a_pp2[,,,i], c(1,2), sd)*mask
}


satandar_d_pp2 = standar_d_pp2[which(standar_d_pp2>=100)]=150   # veeer # contour fill parece que requiere valores mas altos del maximos para completarel contorno

mapa(lista = standar_d_pp2, titulo = "sd - PP - GPCC", nombre_fig = "sd_PP_gpcc", escala = c(0,160)
     , label_escala = "mm", resta = 0, brewer = "PuBuGn",revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 160, by = 20), seq(0, 160, by = 10),  15, altura = 1500, "/salidas/observado/")





# pp3 cmap
# el nc va desde enero '82 hasta dic 2012

aux = nc_open("/home/luciano.andrian/tesis/X190.191.242.210.56.5.48.49.nc")
#aux2 = ncvar_get(aux, "precip")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),]
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[,,27:386]
nc_close(aux)

lon4 = lon
lat4 = lat

pp3_int = array(NA, dim = c(58, 78, 360)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:360){
  
  mod = list(x = lon4, y = lat4, z = aux2[,,i])
  
  grid = list(x=seq(min(lon4), max(lon4), by = 1), y = seq(min(lat4), max(lat2), by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp3_int[,,i] = pp_aux$z  
}


pp3_estaciones = array(NA, dim = c(58, 78, 30, 12))

for(j in 1:12){
  for (i in 0:29){
    pp3_estaciones[,,1+i,j] = pp3_int[1:58 , 1:78, j+12*i]
  }
}


estaciones_p_a_pp3 = array(NA, dim = c(58, 78, 30, 4))
i=1
while(i<=4){
  estaciones_p_a_pp3[,,,i] = apply(pp3_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)*30 # esta en mm/day
  i = i + 1
}

# mascara de 58,78.
mask2 = mask

a1 = array(NA, dim = c(76,1))
mask2 = rbind(t(a1),mask2,t(a1))

c1 = array(NA, dim = c(58,1))
mask2 = cbind(c1,mask2,c1)


estaciones_prom_pp3 = array(NA, dim = c(58, 78, 4))

for( i in 1:4){
  estaciones_prom_pp3[,,i] = apply(estaciones_p_a_pp3[,,,i], c(1,2), mean)*mask2
}


mapa(lista = estaciones_prom_pp3[2:57, 2:77,], titulo = "PP - CMAP", nombre_fig = "pp_cmap", escala = c(0,500)
     , label_escala = "mm", resta = 0, brewer = "YlOrRd",revert = "no", niveles = 9
     , contour = "si", lon2[276:331], lat2[31:106], seq(0, 500, by = 50), seq(0, 500, by = 50),  15, altura = 1500, "/salidas/observado/")




## sd
standar_d_pp3 = array(NA, dim = c(58, 78, 4))
for( i in 1:4 ){
  standar_d_pp3[,,i] = apply(estaciones_p_a_pp3[,,,i], c(1,2), sd)*mask2
}


mapa(lista = standar_d_pp3[2:57, 2:77,], titulo = "sd - PP - CMAP", nombre_fig = "sd_PP_cmap", escala = c(0,160)
     , label_escala = "mm", resta = 0, brewer = "PuBuGn",revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 160, by = 20), seq(0, 160, by = 10),  15, altura = 1500, "/salidas/observado/")






dif_pp1_2 = estaciones_prom_pp -  estaciones_prom_pp2
dif_pp1_3 = estaciones_prom_pp -  estaciones_prom_pp3[2:57,2:77,]  
# ver esto, ya que tienen distinta dimenciones!!! habria q probar descagar otra vez y ver si se consigue la misma dim. 
                                                       # no era facil debido a la grilla.
dif_pp2_3 = estaciones_prom_pp2 -  estaciones_prom_pp3[2:57,2:77,]

mapa(lista = dif_pp1_2, titulo = "PP - CPC vs GPCC", nombre_fig = "dif_pp_cpc-gpcc", escala = c(-100, 100),
     label_escala = "mm", resta = 0, brewer = "BrBG", revert = "no", niveles = 9
     , contour = "si", pp_aux$x[2:57], pp_aux$y[2:77], seq(-100,100, by = 20),  seq(-100,100, by = 10),  15, altura = 1500, "/salidas/observado/")


mapa(lista = dif_pp1_3, titulo = "PP - CPC vs CMAP", nombre_fig = "dif_pp_cpc-cmap", escala = c(-100, 100),
     label_escala = "mm", resta = 0, brewer = "BrBG", revert = "no", niveles = 9
     , contour = "si", pp_aux$x[2:57], pp_aux$y[2:77], seq(-100,100, by = 20),  seq(-100,100, by = 10),  15, altura = 1500, "/salidas/observado/")


mapa(lista = dif_pp2_3, titulo = "PP - GPCC vs CMAP", nombre_fig = "dif_pp_gpcc-cmap", escala = c(-100, 100),
     label_escala = "mm", resta = 0, brewer = "BrBG", revert = "no", niveles = 9
     , contour = "si", pp_aux$x[2:57], pp_aux$y[2:77], seq(-100,100, by = 20),  seq(-100,100, by = 10),  15, altura = 1500, "/salidas/observado/")

