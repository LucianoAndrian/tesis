# desempeño de modelos.
# Anomaly Correlation


###################################################
### Observaciones. O(j,m) j años, m estaciones. ###
###################################################
# necesito "estaciones_p_a_t" de datos_obs.R  (ahora se va a llamar prom_est)
# los años y latitudes se mantienen igual que en datos_obs.R

library(ncdf4)
source("funciones.R")
mask = as.matrix(read.table("mascara.txt"))


# O == prom_est-...
# O' == O - c_v_....


##------------------------ CPC ------------------------ ## 
#sin mascara

# Temp

ruta = "/pikachu/datos/osman/nmme/monthly"

tref = nc_open(paste(ruta,"tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
names(tref$var)
temp = ncvar_get(tref, "tref")
lat = ncvar_get(tref, "Y")
lon = ncvar_get(tref, "X")
nc_close(tref)

temp = temp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:371] 

lon2 = lon[which(lon==275):which(lon==330)]  # se usan las mismas en PP
lat2 = lat[which(lat==-60):which(lat==15)]   #

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

#prec = nc_open(paste(ruta, "prec_monthly_nmme_cpc.nc", sep = "/"))
prec = nc_open("/home/luciano.andrian/tesis/ncfiles/cpc_pp.1x1.nc")  # nuevo
names(prec$var)
pp = ncvar_get(prec, "precip")
lat = ncvar_get(prec, "lat")
lon = ncvar_get(prec, "lon")
nc_close(prec)

pp = pp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:362]    # aca falta un año.

pp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12)) # le pongo 30 igual. va tener NA pero sino complica los calculos mas adelante

for(j in 1:12){
  for (i in 0:29){
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



## ------------------------ GPCC ------------------------ ## #con mascara
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



## ------------------------ CMAP ------------------------ ## # sin mascara
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

# O
prom_est_obs = array(data = NA, dim = c(56, 76, 30, 4, 4))
prom_est_obs[,,,,1] = prom_est_cpc_t
prom_est_obs[,,,,2] = prom_est_cpc_pp
prom_est_obs[,,,,3] = prom_est_gpcc_pp
prom_est_obs[,,,,4] = prom_est_cmap_pp[2:57,2:77,,]



########################## Cross Validation ##########################
# 
# para cada año tengo q tener promedio de todos los años menos ese año.

aux = diag(30)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 30, 4, 30, 4))
aux2_obs = array(data = 1, dim = c(56, 76, 30, 4, 30, 4))

c_v_obs = array(data = NA, dim = c(56, 76, 30, 4, 4))

for(i in 1:30){
  
  aux2[,,i,,i,] = aux2[,,i,,i,]*aux[i,i] # como matriz identidad inversa con NA en la diagonal y 1 pero en 4 dimenciones.
  
  aux2_obs[,,,,i,] = aux2[,,,,i,]*prom_est_obs[,,,,] 
 
  # promedio sacando cada año.
  
  c_v_obs[,,i,,] = apply(aux2_obs[,,,,i,], c(1,2,4,5), mean, na.rm = T)
  
}



### O' 
Op_obs = array(data = NA, dim = c(56, 76, 30, 4, 4))
for(i in 1:4){
  
  Op_obs[,,,,i] = prom_est_obs[,,,,i]-c_v_obs[,,,,i]
  
}



###################################################
###    Modelos. F(j,m) j años, m estaciones.    ###
###################################################

# necesito el array intermedio para crear sd que tiene la funcion mean_sd. 
# modificada la funcion, devuelve lista que en las dim [[5]] = se encuetnra la temp y  [[6]] la pp. h
# la funcion funciona de a un modelo, ya que estaba pensada para usar en conjunto con la funcion de graficado.
# la funcion necesita, estas lon y lat (56, 76)
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2") 

# uso misma denominacion que para las obserbaciones.
# esto es F 
prom_est_mods_t = array(data = NA, dim = c(56, 76, 29, 4, 8)) # recordar, los modelos 1982-2010 (29 años)
prom_est_mods_pp = array(data = NA, dim = c(56, 76, 29, 4, 8))
for(i in 1:length(modelos)){
  v = mean_sd(modelos[i])
  prom_est_mods_t[,,,,i] = v[[5]]
  prom_est_mods_pp[,,,,i] = v[[6]]
}  


########################## Cross Validation ##########################
# T y PP

aux = diag(30)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 29, 4, 29, 8))
aux2_pp = array(data = 1, dim = c(56, 76, 29, 4, 29, 8))

aux2_mod_t = array(data = 1, dim = c(56, 76, 29, 4, 29, 8))
aux2_mod_pp = array(data = 1, dim = c(56, 76, 29, 4, 29, 8))

c_v_mod_t = array(data = NA, dim = c(56, 76, 29, 4, 8))
c_v_mod_pp = array(data = NA, dim = c(56, 76, 29, 4, 8))

for(i in 1:29){
  aux2[,,i,,i,] = aux2[,,i,,i,]*aux[i,i]  # una especie de matriz identidad inversa con NA y 1 pero en 4 dim.
  
  aux2_mod_t[,,,,i,] = aux2[,,,,i,]*prom_est_mods_t 
  aux2_mod_pp[,,,,i,] = aux2[,,,,i,]*prom_est_mods_pp
  
  
  # promedio sacando cada año.
  c_v_mod_t[,,i,,] = apply(aux2_mod_t[,,,,i,], c(1, 2, 4, 5), mean, na.rm = T)
  c_v_mod_pp[,,i,,] = apply(aux2_mod_pp[,,,,i,], c(1, 2, 4, 5), mean, na.rm = T)
}

Fp_t = prom_est_mods_t - c_v_mod_t
Fp_pp = prom_est_mods_pp - c_v_mod_pp

##--- OK ---##





### ec 3 becker ### 

# revisar, dan valores medios raros. sobre todo AC_pp


# AC. Temp

AC_t = array(data = NA, dim = c(56, 76, 4, 8))

for(i in 1:8){
  
  #numerador
  aux = Op_obs[,,1:29,,1]*Fp_t[,,,,i] # Op_obs[,,,,,1] solo temp
  num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
  #denominador
  den = ((apply(Op_obs[,,1:29,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_t**2, c(1, 2, 4), sum, na.rm = T))/29) 
  
  AC_t[,,,i] = num/den
  
}



# AC PP

AC_pp = array(data = NA, dim = c(56, 76, 4, 8, 3))

for(i in 1:8){
  for(j in 2:4){
    aux = Op_obs[,,1:29,,j]*Fp_pp[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    
    den = ((apply(Op_obs[,,1:29,,j]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_pp**2, c(1, 2, 4), sum, na.rm = T))/29)
    
    AC_pp[,,,i,j-1] = num/den
  }
}

#
