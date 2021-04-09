#RMSE corregido

#### Apertura base de datos ####
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
## ------------------------ CMAP ------------------------ ## # sin mascara
# solo pp

aux = nc_open("/home/luciano.andrian/tesis/ncfiles/X190.191.242.210.56.5.48.49.nc")
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
  
  pp_aux = fields::interp.surface.grid(obj = mod, grid.list = grid)
  
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
datos.obs = array(data = NA, dim = c(56, 76, 29, 4, 2)) # uso misma cantidad de años que los modelos
datos.obs[,,,,1] = prom_est_cpc_t[,,1:29,] 
datos.obs[,,,,2] = prom_est_cmap_pp[2:57,2:77,1:29,]  # este tenia + lats y lons por el grillado






#### Apertura de los modelos ####
#-------------------------------------------------#
###    Modelos. F(j,m) j años, m estaciones.    ###
#-------------------------------------------------#

# necesito el array intermedio para crear sd que tiene la funcion mean_sd. 
# modificada la funcion, devuelve lista que en las dim [[5]] = se encuetnra la temp y  [[6]] la pp. h
# ESTAS LISTAS SON EL ENSAMBLE DE LOS MIEMBROS DE CADA MODELO --> OK

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}

resultados= list()

modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO") 

# uso misma denominacion que para las obserbaciones.
# esto es F 
t.mods = array(data = NA, dim = c(56, 76, 29, 4, 8)) # recordar, los modelos 1982-2010 (29 años)
pp.mods = array(data = NA, dim = c(56, 76, 29, 4, 8))
for(i in 1:length(modelos)){
  aux = mean_sd(modelos[i])
  t.mods[,,,,i] = aux[[5]]
  pp.mods[,,,,i] = aux[[6]]
}  


aux = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8, 29))
t.rmse = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8))

for(i in 1:8){
  
  for(t in 1:29){
    años = seq(1,29)
    años[t] = NA
    mp = apply(t.mods[,,años,,i], c(1,2,4), mean, na.rm = T)
    mo = apply(datos.obs[,,años,,1], c(1,2,4), mean, na.rm = T)
    aux[,,,i,t] = (t.mods[,,t,,i]- mp + mo - datos.obs[,,t,,1]) 
  }
  
  t.rmse[,,,i] = sqrt((apply(aux[,,,i,]**2, c(1,2,3), sum, na.rm = T)/29))

}

t.rmse_mean = apply(t.rmse, c(1,2,3), mean, na.rm = T)*mask_arr


rmse = list()
rmse[[1]] = t.rmse_mean


aux = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8, 29))
pp.rmse = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8))

for(i in 1:8){
  
  for(t in 1:29){
    años = seq(1,29)
    años[t] = NA
    mp = apply(pp.mods[,,años,,i], c(1,2,4), mean, na.rm = T)
    mo = apply(datos.obs[,,años,,2], c(1,2,4), mean, na.rm = T)
    aux[,,,i,t] = (pp.mods[,,t,,i]- mp + mo - datos.obs[,,t,,2]) 
  }
  
  pp.rmse[,,,i] = sqrt((apply(aux[,,,i,]**2, c(1,2,3), sum, na.rm = T)/29))
  
}

pp.rmse_mean = apply(pp.rmse, c(1,2,3), mean, na.rm = T)*mask_arr

rmse[[2]] = pp.rmse_mean



aux = array(data = NA, dim = c(length(lon2), length(lat2), 4, 29))
for(t in 1:29){
  años = seq(1,29)
  años[t] = NA
  mp = apply(datos.obs[,,años,,1], c(1,2,4), mean, na.rm = T)
  aux[,,,t] = (datos.obs[,,t,,1]- mp)**2
}

sd_t = sqrt(apply(aux, c(1,2,3), sum, na.rm = T)/29)*mask_arr



aux = array(data = NA, dim = c(length(lon2), length(lat2), 4, 29))
for(t in 1:29){
  años = seq(1,29)
  años[t] = NA
  mp = apply(datos.obs[,,años,,2], c(1,2,4), mean, na.rm = T)
  aux[,,,t] = (datos.obs[,,t,,2]- mp)**2
}

sd_pp = sqrt(apply(aux, c(1,2,3), sum, na.rm = T)/29)*mask_arr



NRMSE = list()
NRMSE[[1]] = 1 - rmse[[1]]/sd_t
NRMSE[[2]] = 1 - rmse[[2]]/sd_pp


return(NRMSE)