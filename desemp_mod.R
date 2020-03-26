# desempeño de modelos.
# Anomaly Correlation


# observaciones. O(j,m) j años, m estaciones.
# necesito "estaciones_p_a_t" de datos_obs.R  (ahora se va a llamar prom_est)
# los años y latitudes se mantienen igual que en datos_obs.R

# DUDAS: temp y pp?? 

library(ncdf4)
source("funciones.R")
mask = as.matrix(read.table("mascara.txt"))

## CPC ## #sin mascara
# Temp
ruta = "/pikachu/datos/osman/nmme/monthly"

tref = nc_open(paste(ruta,"tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
names(tref$var)
temp = ncvar_get(tref, "tref")
lat = ncvar_get(tref, "Y")
lon = ncvar_get(tref, "X")
nc_close(tref)

temp = temp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:371] 

lon2 = lon[which(lon==275):which(lon==330)]
lat2 = lat[which(lat==-60):which(lat==15)]

temp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12))

for(j in 1:12){
  for (i in 0:29){
    temp_estaciones[,,1+i,j] = temp[ , , j+12*i]
  }
}

# Estaciones

prom_est_cpc_t = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
i=1
while(i<=4){
  prom_est_cpc_t[,,,i] = apply(temp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}

# PP

prec = nc_open(paste(ruta, "prec_monthly_nmme_cpc.nc", sep = "/"))
names(prec$var)
pp = ncvar_get(prec, "prec")
lat = ncvar_get(prec, "Y")
lon = ncvar_get(prec, "X")
nc_close(prec)

pp = pp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:350]    # aca falta un año.

lon2 = lon[which(lon==275):which(lon==330)]
lat2 = lat[which(lat==-60):which(lat==15)]

pp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12)) # le pongo 30 igual. va tener NA pero sino complica los calculos mas adelante

for(j in 1:12){
  for (i in 0:28){
    pp_estaciones[,,1+i,j] = pp[ , , j+12*i]
  }
}


# Estaciones

prom_est_cpc_pp = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
i=1
while(i<=4){
  prom_est_cpc_pp[,,,i] = apply(pp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean, na.rm = T)
  i = i + 1
}


## GPCC ## #con mascara
# solo pp

aux = nc_open("/home/luciano.andrian/tesis/X157.92.36.193.339.11.29.13.nc")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[which(lon==275.25):which(lon==330.25), which(lat==-60.25):which(lat==15.25),27:386]
nc_close(aux)

lon2 = lon[which(lon==275.25):which(lon==330.25)]
lat2 = lat[which(lat==-60.25):which(lat==15.25)]

pp2_int = array(NA, dim = c(56, 76, 360))

require(fields) 
for(i in 1:360){ # interpolado (no guarde los interpolados en datos_obs.R)
  
  mod = list(x = lon2, y = lat2, z = aux2[,,i])
  
  grid = list(x=seq(min(lon2), max(lon2), by = 1), y = seq(min(lat2), max(lat2), by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp2_int[,,i] = pp_aux$z
}

pp2_estaciones = array(NA, dim = c(56, 76, 30, 12))

for(j in 1:12){
  for (i in 0:29){
    pp2_estaciones[,,1+i,j] = pp2_int[ , , j+12*i]
  }
}

prom_est_gpcc_pp = array(NA, dim = c(56, 76, 30, 4))
i=1
while(i<=4){
  prom_est_gpcc_pp[,,,i] = apply(pp2_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}

## CMAP ## # sin mascara
# solo pp

aux = nc_open("/home/luciano.andrian/tesis/X190.191.242.210.56.5.48.49.nc")
#aux2 = ncvar_get(aux, "precip")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),]
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[,,27:386]
nc_close(aux)

lon2 = lon
lat2 = lat

pp3_int = array(NA, dim = c(58, 78, 360)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:360){  #interpolado
  
  mod = list(x = lon2, y = lat2, z = aux2[,,i])
  
  grid = list(x=seq(min(lon2), max(lon2), by = 1), y = seq(min(lat2), max(lat2), by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp3_int[,,i] = pp_aux$z  
}


pp3_estaciones = array(NA, dim = c(58, 78, 30, 12))

for(j in 1:12){
  for (i in 0:29){
    pp3_estaciones[,,1+i,j] = pp3_int[1:58 , 1:78, j+12*i]
  }
}


prom_est_cmap_pp = array(NA, dim = c(58, 78, 30, 4))
i=1
while(i<=4){
  prom_est_cmap_pp[,,,i] = apply(pp3_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)*30 # esta en mm/day
  i = i + 1
}


# para hacer cross validation.
# para cada año tengo q tener promedio de todos los años menos ese año.

aux = diag(30)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 30, 4, 30))

aux2_cpc_t = array(data = 1, dim = c(56, 76, 30, 4, 30))
aux2_gpcc_pp = array(data = 1, dim = c(56, 76, 30, 4, 30))
aux2_cmap_pp = array(data = 1, dim = c(56, 76, 30, 4, 30))
aux2_cpc_pp = array(data = 1, dim = c(56, 76, 30, 4, 30))

c_v_cpc_t = array(data = NA, dim = c(56, 76, 30, 4))
c_v_cpc_pp = array(data = NA, dim = c(56, 76, 30, 4))
c_v_gpcc_pp = array(data = NA, dim = c(56, 76, 30, 4))
c_v_cmap_pp = array(data = NA, dim = c(56, 76, 30, 4))

for(i in 1:30){
 aux2[,,i,,i] = aux2[,,i,,i]*aux[i,i]  # una especie de matriz identidad inversa con NA y 1 pero en 4 dim.
 
 aux2_cpc_t[,,,,i] = aux2[,,,,i]*prom_est_cpc_t 
 aux2_cpc_pp[,,,,i] = aux2[,,,,i]*prom_est_cpc_pp 
 aux2_gpcc_pp[,,,,i] = aux2[,,,,i]*prom_est_cpc_t 
 aux2_cmap_pp[,,,,i] =aux2[,,,,i]*prom_est_cmap_pp[2:57,2:77,,] 
 
 # promedio sacando cada año.
 c_v_cpc_t[,,i,] = apply(aux2_cpc_t[,,,,i], c(1,2,4), mean, na.rm = T)
 c_v_cpc_pp[,,i,] = apply(aux2_cpc_pp[,,,,i], c(1,2,4), mean, na.rm = T)
 c_v_gpcc_pp[,,i,] = apply(aux2_gpcc_pp[,,,,i], c(1,2,4), mean, na.rm = T)
 c_v_cmap_pp[,,i,] = apply(aux2_cmap_pp[,,,,i], c(1,2,4), mean, na.rm = T)
}






