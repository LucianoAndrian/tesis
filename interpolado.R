require(fields)
library(ncdf4)

aux = nc_open("/home/luciano.andrian/X157.92.36.193.339.11.29.13.nc")
#aux2 = ncvar_get(aux, "precip")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),]
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[which(lon==275.25):which(lon==330.25), which(lat==-60.25):which(lat==15.25),]
require(fields)


lon2 = lon[which(lon==275.25):which(lon==330.25)]
lat2 = lat[which(lat==-60.25):which(lat==15.25)]

pp2_int = array(NA, dim = c(56, 76, 360))

for(i in 13:(396-2*12)){
  
  mod = list(x = lon2, y = lat2, z = aux2[,,i])
  
  grid = list(x=seq(min(lon2), max(lon2), by = 1), y = seq(min(lat2), max(lat2), by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp2_int[,,i-12] = pp_aux$z
}

pp2_estaciones = array(NA, dim = c(56, 76, 30, 12))

for(j in 1:12){
  for (i in 0:29){
    pp2_estaciones[,,1+i,j] = pp2_int[ , , j+12*i]
  }
}


pp2_p_a_t = array(NA, dim = c(56, 76, 30, 4))
i=1
while(i<=4){
  pp2_p_a_t[,,,i] = apply(pp2_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}

pp2_prom_t = array(NA, dim = c(56, 76, 4))

for( i in 1:4){
  pp2_prom_t[,,i] = apply(pp2_p_a_t[,,,i], c(1,2), mean)
}

source("mapa_obs.R")


mapa(lista = pp2_prom_t, titulo = "PP2", nombre_fig = "pp", escala = c(0,500) 
     ,label_escala = "mm", resta = 0, brewer = "Blues", revert = "no", niveles = 9, contour = "si", pp_aux$x, pp_aux$y, c(0,100,200,300,400,500))


## sd
standar_d_pp2 = array(NA, dim = c(56, 76, 4))
for( i in 1:4 ){
  standar_d_pp2[,,i] = apply(pp2_p_a_t[,,,i], c(1,2), sd)*mask
}


satandar_d_pp2 = standar_d_pp2[which(standar_d_pp2>=100)]=90   # veeer # contour fill parece que requiere valores mas altos del maximos para completarel contorno

mapa(lista = standar_d_pp2, titulo = "sd - PP", nombre_fig = "sd_pp", escala = c(0,100)
     , label_escala = "mm", resta = 0, brewer = "YlOrRd",revert = "no", niveles = 9, contour = "si", pp_aux$x, pp_aux$y,c(0,25,50,75,100))

