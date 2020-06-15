# Desempeño de modelos... otra vez.


#### Apertura base de datos ####
#-------------------------------------------------#
### Observaciones. O(j,m) j años, m estaciones. ###
#-------------------------------------------------#
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

pp = pp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:362]   

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
datos.obs = array(data = NA, dim = c(56, 76, 29, 4, 4)) # uso misma cantidad de años que los modelos
datos.obs[,,,,1] = prom_est_cpc_t[,,1:29,] 
datos.obs[,,,,2] = prom_est_cpc_pp[,,1:29,]
datos.obs[,,,,3] = prom_est_gpcc_pp[,,1:29,]
datos.obs[,,,,4] = prom_est_cmap_pp[2:57,2:77,1:29,]  # este tenia + lats y lons por el grillado



########################## Cross Validation  datos.obs ##########################
# 
# para cada año tengo q tener promedio de todos los años menos ese año.

aux = diag(29)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 29, 4, 29, 4))  
aux2.obs = array(data = 1, dim = c(56, 76, 29, 4, 29, 4))

cv.obs = array(data = NA, dim = c(56, 76, 29, 4, 4)) # para las 4 base de datos, la 1era temp y las otras pp

for(i in 1:29){
  
  aux2[,,i,,i,] = aux2[,,i,,i,]*aux[i,i] # como matriz identidad inversa con NA en la diagonal y 1 pero en 4 dimenciones.
  
  aux2.obs[,,,,i,] = aux2[,,,,i,]*datos.obs
  
  # promedio sacando cada año.
  
  cv.obs[,,i,,] = apply(aux2.obs[,,,,i,], c(1,2,4,5), mean, na.rm = T)
  
}



### O' 
Op = datos.obs - cv.obs


#### Apertura de los modelos ####
#-------------------------------------------------#
###    Modelos. F(j,m) j años, m estaciones.    ###
#-------------------------------------------------#

# necesito el array intermedio para crear sd que tiene la funcion mean_sd. 
# modificada la funcion, devuelve lista que en las dim [[5]] = se encuetnra la temp y  [[6]] la pp. h
# ESTAS LISTAS SON EL ENSAMBLE DE LOS MIEMBROS DE CADA MODELO --> OK


lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2") 

# uso misma denominacion que para las obserbaciones.
# esto es F 
t.mods = array(data = NA, dim = c(56, 76, 29, 4, 8)) # recordar, los modelos 1982-2010 (29 años)
pp.mods = array(data = NA, dim = c(56, 76, 29, 4, 8))
for(i in 1:length(modelos)){
  aux = mean_sd(modelos[i])
  t.mods[,,,,i] = aux[[5]]
  pp.mods[,,,,i] = aux[[6]]
}  


########################## Cross Validation modelos ##########################

aux = diag(29)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29))

aux3 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29)) # T
aux4 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29)) # PP

aux5 = array(data = NA, dim = c(56, 76, 29, 4, 8))
aux6 = array(data = NA, dim = c(56, 76, 29, 4, 8))

for(i in 1:29){
  aux2[,,i,,,i] = aux2[,,i,,,i]*aux[i,i]  # una especie de matriz identidad inversa con NA y 1 pero en 4 dim.

  aux3[,,,,,i] = aux2[,,,,,i]*t.mods
  aux4[,,,,,i] = aux2[,,,,,i]*pp.mods
  
  # promedio sacando cada anio
  # 
  aux5[,,i,,] = apply(aux3[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)   
  aux6[,,i,,] = apply(aux4[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)
  
  
}





##### ACC ####

### F'
t.Fp = t.mods - aux5
pp.Fp = pp.mods - aux6


#!!!!!!!!
t.Fp_ens = apply(t.Fp, c(1,2,3,4), mean, na.rm = T)
pp.Fp_ens = apply(pp.Fp, c(1,2,3,4), mean, na.rm = T)


# recordar que en Op[,,,,1] esta la temp

#for(i in 1:8){
  aux = Op[,,1:29,,1]*t.Fp_ens#[,,,,i] 
  num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
  den = ((apply(Op[,,,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(t.Fp_ens**2, c(1, 2, 4), sum, na.rm = T))/29) 

t.ACC = num/sqrt(den)


# AC PP

pp.ACC = array(data = NA, dim = c(56, 76, 4, 3))
#for(i in 1:8){
  
  for(j in 2:4){ # las 3 bases de datos
    
    aux = Op[,,1:29,,j]*pp.Fp_ens#[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    
    den = ((apply(Op[,,,,j]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(pp.Fp_ens**2, c(1, 2, 4), sum, na.rm = T))/29)
    
    pp.ACC[,,,j-1] = num/sqrt(den)
    
  }
#}


##### Ensamble y Graficos  #### 

#t.ACC_ensamble = apply(t.ACC, c(1,2,3), mean, na.rm = T)
#pp.ACC_ensamble = apply(pp.ACC, c(1,2,3,5), mean, na.rm = T)

# para testear
rc = qt(p = 0.95,df = 29-1)/sqrt((29-1)+qt(p = 0.95,df = 29-1))


#--- Graficos ----#
source("funciones.R")

topo = metR::GetTopography(-85 + 359.5, -29 + 359.5, 15.5,  -60.5, resolution = 1/15) # mapa topografia
topo2 = topo #
topo2[which(topo2$h<1500)]=NA # altura para la cual tapa el grafico
save(topo, file = "topo.RData")
load("topo.RData")


mapa_topo3(variable = t.ACC, colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
           , titulo = paste("ACC Temp Ensamble y CPC", sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/desemp_mods/", nombre.fig = paste("t.ACC_ensamble", sep = ""), na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1, v.sig = rc, variable.sig = t.ACC
           , lon = lon2, lat = lat2, type.sig = "point2")



nombres2 = c("CPC", "GPCC", "CMAP")

mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}



i = 3
#for(i in 1:3){
mapa_topo3(variable = pp.ACC[,,,i]*mask_arr, colorbar = "PuBuGn", revert = F, escala = seq(0, 1, by = 0.1)
           , titulo = paste("ACC PP MODS y ", nombres2[i], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/desemp_mods/", nombre.fig = paste("AC_pp_", nombres2[i], sep = ""), na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1, v.sig = rc, variable.sig = pp.ACC[,,,i]*mask_arr
           , lon = lon2, lat = lat2, type.sig = "point2")

#}

##### Ensamble sacando modelos ####

t.Fp_oneless = array(data = NA, dim = c(length(lon2), length(lat2), 29, 4, 8))
pp.Fp_oneless = array(data = NA, dim = c(length(lon2), length(lat2), 29, 4, 8))

for(i in 1:8){
  
  aux = t.Fp
  aux[,,,,i] = NA
  
  t.Fp_oneless[,,,,i] = apply(aux, c(1,2,3,4), mean, na.rm = T)  # obtengo 8 ensambles en los que falta un modelo
  
  
  
  aux2 = pp.Fp
  aux2[,,,,i] = NA
  
  pp.Fp_oneless[,,,,i] = apply(aux2, c(1,2,3,4), mean, na.rm = T)
}  
  


# ACC T


den = array(data = NA, dim = c(56, 76, 4, 8))
num = array(data = NA, dim = c(56, 76, 4, 8))

for(i in 1:8){
  aux = Op[,,1:29,,1]*t.Fp_oneless[,,,,i] 
  num[,,,i] = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  den[,,,i] = ((apply(Op[,,,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(t.Fp_oneless[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 

}

t.ACC_oneless = num/sqrt(den)


# AC PP

pp.ACC_oneless = array(data = NA, dim = c(56, 76, 4, 8, 3))

den = array(data = NA, dim = c(56, 76, 4, 8, 3))
num = array(data = NA, dim = c(56, 76, 4, 8, 3))

for(i in 1:8){
  
  for(j in 2:4){ # las 3 bases de datos
  
    aux = Op[,,1:29,,j]*pp.Fp_oneless[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    
    den = ((apply(Op[,,,,j]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(pp.Fp_oneless[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29)
    
    pp.ACC_oneless[,,,i,j-1] = num/sqrt(den)
    
    
  }
  
}
  
  
  
#t.ACC_oneless = array(data = NA, dim = c(length(lon2), length(lat2), 4, 8))
#pp.ACC_oneless = array(data = NA, dim = c(length(lon2), length(lat2), 4, 3, 8))

#for(i in 1:8){
#  aux = t.ACC
#  aux[,,,i] = NA
#  t.ACC_oneless[,,,i] =  apply(aux, c(1,2,3), mean, na.rm = T)
#  
#  
#  aux2 = pp.ACC
#  aux2[,,,i,] = NA
#  pp.ACC_oneless[,,,,i] = apply(aux2, c(1,2,3,5), mean, na.rm = T)
#
#}


#--- Graficos ---#

for(i in 1:8){
  
  mapa_topo3(variable = t.ACC_oneless[,,,i], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
             , titulo = paste("ACC Temp MODS y CPC sin ", modelos[i], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/desemp_mods/AC_sin_mods/", nombre.fig = paste("t.ACC_sin_", modelos[i], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1, v.sig = rc, variable.sig = t.ACC_oneless[,,,i]
             , lon = lon2, lat = lat2, type.sig = "point2")
  
}


nombres2 = c("CPC", "GPCC", "CMAP")

mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}


for(j in 1:8){
  
  #for(i in 1:3){
   i = 3 
    
    mapa_topo3(variable = pp.ACC_oneless[,,,j,i]*mask_arr, colorbar = "PuBuGn", revert = F, escala = seq(0, 1, by = 0.1)
               , titulo = paste("ACC PP MODS y ", nombres2[i], " sin ", modelos[j],  sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
               , salida = "/salidas/desemp_mods/AC_sin_mods/", nombre.fig = paste("pp.ACC_", nombres2[i],"sin_", modelos[j], sep = ""), na.fill = -1000
               , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1, v.sig = rc, variable.sig = pp.ACC_oneless[,,,j,i]*mask_arr
               , lon = lon2, lat = lat2, type.sig = "point2")
  #}
  
}


#### ACC teorico ####

t.ACC_ens_vs_mods = array(data = NA, c(length(lon2), length(lat2), 4, 8))
pp.ACC_ens_vs_mods = array(data = NA, c(length(lon2), length(lat2), 4, 8))

for(i in 1:8){
  
  aux = t.Fp_ens*t.Fp[,,,,i]
  
  num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
  den = ((apply(t.Fp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(t.Fp_ens**2, c(1, 2, 4), sum, na.rm = T))/29) 
  
  t.ACC_ens_vs_mods[,,,i] = num/sqrt(den)
  
  aux = pp.Fp_ens*pp.Fp[,,,,i]
  
  num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
  den = ((apply(pp.Fp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(pp.Fp_ens**2, c(1, 2, 4), sum, na.rm = T))/29) 
  
  pp.ACC_ens_vs_mods[,,,i] = num/sqrt(den)
  
}


#--- promedio de los ACC teo ----#
t.ACC_teo_prom = apply(t.ACC_ens_vs_mods, c(1,2,3), mean, na.rm = T)
pp.ACC_teo_prom = apply(pp.ACC_ens_vs_mods, c(1,2,3), mean, na.rm = T)


#--- Graficos ----#
source("funciones.R")

#topo = metR::GetTopography(-85 + 359.5, -29 + 359.5, 15.5,  -60.5, resolution = 1/15) # mapa topografia
load("topo.RData")
topo2 = topo #
topo2[which(topo2$h<1500)]=NA # altura para la cual tapa el grafico
#save(topo, file = "topo.RData")

rc = qt(p = 0.95,df = 29-1)/sqrt((29-1)+qt(p = 0.95,df = 29-1))

mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}



for(i in 1:8){
  
  mapa_topo3(variable =  t.ACC_ens_vs_mods[,,,i]*mask_arr, colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
             , titulo = paste("ACC Temp Ensamble y ", modelos[i], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/desemp_mods/AC_con_mods/", nombre.fig = paste("t.ACC_ens_", modelos[i], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1, v.sig = rc, variable.sig = t.ACC_ens_vs_mods[,,,i]*mask_arr
             , lon = lon2, lat = lat2, type.sig = "point2")
  
  mapa_topo3(variable =  pp.ACC_ens_vs_mods[,,,i]*mask_arr, colorbar = "PuBuGn", revert = F, escala = seq(0, 1, by = 0.1)
             , titulo = paste("ACC PP Ensamble y ", modelos[i], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/desemp_mods/AC_con_mods/", nombre.fig = paste("pp.ACC_ens_", modelos[i], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1, v.sig = rc, variable.sig = pp.ACC_ens_vs_mods[,,,i]*mask_arr
             , lon = lon2, lat = lat2, type.sig = "point2")
  
}

mapa_topo3(variable =  t.ACC_teo_prom*mask_arr, colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
           , titulo = "ACC Teorico Temperatura", label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/desemp_mods/AC_con_mods/", nombre.fig = "t.ACC_teo", na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1, v.sig = rc, variable.sig = t.ACC_teo_prom*mask_arr
           , lon = lon2, lat = lat2, type.sig = "point2")

mapa_topo3(variable =  pp.ACC_teo_prom*mask_arr, colorbar = "PuBuGn", revert = F, escala = seq(0, 1, by = 0.1)
           , titulo = "ACC Teorico Precipitacion", label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/desemp_mods/AC_con_mods/", nombre.fig = "pp.ACC_teo", na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1, v.sig = rc, variable.sig = pp.ACC_teo_prom*mask_arr
           , lon = lon2, lat = lat2, type.sig = "point2")

#### Fig10 ####

t.ACC_mod = array(data = NA, dim = c(length(lon2), length(lat2), 4, 8))
pp.ACC_mod = array(data = NA, dim = c(length(lon2), length(lat2), 4, 8, 3))

for(i in 1:8){
  aux = Op[,,1:29,,1]*t.Fp[,,,,i] 
  num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 

  den = ((apply(Op[,,,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(t.Fp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 
  
  t.ACC_mod[,,,i] = num/sqrt(den)
}


for(i in 1:8){
  for(j in 2:4){
    
    aux = Op[,,1:29,,j]*pp.Fp[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    den = ((apply(Op[,,,,j]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(pp.Fp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 
    
  pp.ACC_mod[,,,i,j-1] = num/sqrt(den)
  }
}


cajas_lat = list()
cajas_lat[[1]] =  seq(which(lat2 == -13), which(lat2 == 2), by = 1); cajas_lat[[2]] = seq(which(lat2 == -16), which(lat2 == 4), by = 1)
cajas_lat[[3]] = seq(which(lat2 == -16), which(lat2 == 2), by = 1); cajas_lat[[4]] = seq(which(lat2 == -26), which(lat2 == -17), by = 1)
cajas_lat[[5]] = seq(which(lat2 == -39), which(lat2 == -24), by = 1)

cajas_lon = list()
cajas_lon[[1]] =  seq(which(lon2 == 291), which(lon2 == 304), by = 1); cajas_lon[[2]] = seq(which(lon2 == 301), which(lon2 == 316), by = 1)
cajas_lon[[3]] = seq(which(lon2 == 313), which(lon2 == 326), by = 1); cajas_lon[[4]] = seq(which(lon2 == 308), which(lon2 == 321), by = 1)
cajas_lon[[5]] = seq(which(lon2 == 296), which(lon2 == 309), by = 1)

t.ACC_box = list()
pp.ACC_box = list()
t.ACC_ens_box = list()
pp.ACC_ens_box = list()
for(i in 1:5){
  
  t.ACC_ens_box[[i]] = apply(t.ACC[cajas_lon[[i]], cajas_lat[[i]],], c(3), mean, na.rm = T)
  pp.ACC_ens_box[[i]] = apply(pp.ACC[cajas_lon[[i]], cajas_lat[[i]],,], c(3, 4), mean, na.rm = T)
  
  t.ACC_box[[i]] = apply(t.ACC_mod[cajas_lon[[i]], cajas_lat[[i]],,], c(3, 4), mean, na.rm = T)
  
  pp.ACC_box[[i]] = apply(pp.ACC_mod[cajas_lon[[i]], cajas_lat[[i]],,,], c(3, 4, 5), mean, na.rm = T)
  
}
cajas = c("Am", "SAM", "NeB", "SACZ", "LPB")
cajas_num = seq( 1, 5)


t.data = fig10(prom_cajas = t.ACC_box, prom_ensamble = t.ACC_ens_box, variable = "temp")

pp.data = list()
for( i in 1:3){
  pp.data[[i]] = fig10(prom_cajas = pp.ACC_box, prom_ensamble = pp.ACC_ens_box, variable = "pp", base_datos = i)
}

# Grafico
est = c("MAM", "JJA", "SON", "DJF")
g = list()
for(i in 2:5){
  
  g = ggplot() + theme_minimal()+
    geom_text(data = t.data[[1]], aes(x = cajas, y = t.data[[1]][,i], label = value), color = "green", size = 4) + 
    geom_point(data = t.data[[2]], aes(x = cajas, y = t.data[[2]][,i]), color = "darkgreen", shape = "*" , size = 11) + 
    geom_text(data = pp.data[[1]][[1]], aes(x = cajas, y = pp.data[[1]][[1]][,i], label = value), color = "blue", size = 4) + 
    geom_point(data = pp.data[[1]][[2]], aes(x = cajas , y = pp.data[[1]][[2]][,i]), color = "navyblue", shape = "*" , size = 11) + 
    geom_text(data = pp.data[[2]][[1]], aes(x = cajas, y = pp.data[[2]][[1]][,i], label = value), color = "purple", size = 4) + 
    geom_point(data = pp.data[[2]][[2]], aes(x = cajas , y = pp.data[[2]][[2]][,i]), color = "purple4", shape = "*" , size = 11) + 
    geom_text(data = pp.data[[3]][[1]], aes(x = cajas, y = pp.data[[3]][[1]][,i], label = value), color = "deepskyblue", size = 4) + 
    geom_point(data = pp.data[[3]][[2]], aes(x = cajas , y = pp.data[[3]][[2]][,i]), color = "deepskyblue4", shape = "*" , size = 11) + 
    
    geom_hline(yintercept = rc, color = "grey", size = 1) +
    geom_hline(yintercept = 0, color = "black")+
    ggtitle(paste("ACC - ", est[i-1], sep = ""))+
    scale_y_continuous(limits = c(-0.2, 0.8), breaks = seq(-0.2,0.8, by = 0.2)) + 
    #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0,0.8, by = 0.2)) + 
    scale_x_continuous(labels=c("1" = "Am", "2" = "SAM", "3" = "NeB", "4" = "SACZ", "5" = "LPB"),breaks = seq(1, 5, by = 1))+
    xlab(label = "COLA-CCSM4(1), GFDL-CM2p1(2), GFDL-FLOR-A06(3), GFDL-FLOR-B01(4), NASA-GEOS5(5), NCEP-CFSv2(6) CMC-CanCM4i(7), CMC-CanSIPSv2(8)" )+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black", face = "bold"), axis.title.y  = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5)) 
  ggsave(paste("/home/luciano.andrian/tesis/salidas/desemp_mods/AC_con_mods/", "ACC", "_", est[i-1], ".jpg",sep =""), plot = g, width = 30, height = 15  , units = "cm")
  
}


#### Bias ####

t.bias = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8))
for(i in 1:8){

    t.bias[,,,i] = apply(t.mods[,,,,i] - datos.obs[,,,,1], c(1,2,4), sum, na.rm = T)/29
    
}
t.bias_mean = apply(t.bias, c(1,2,3), mean, na.rm = T)*mask_arr



pp.bias = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8, 3))

for(i in 1:8){
  for(j in 2:4){
    
    pp.bias[,,,i,j-1] = apply(pp.mods[,,,,i] - datos.obs[,,,,j], c(1,2,4), sum, na.rm = T)/29
    
  }
  
}
pp.bias_mean = apply(pp.bias, c(1,2,3,5), mean, na.rm = T)



#--- graficos ---#

mapa_topo3(variable =  t.bias_mean*mask_arr, colorbar = "RdBu", revert = T, escala = seq(-5, 5, by = 1)
           , titulo = paste("Bias T - CPC ", sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/desemp_mods/", nombre.fig = paste("bias_t_cpc", sep = ""), na.fill = -1000
           , r = 4, estaciones = T, altura.topo = 1500, size.point = 1
           , lon = lon2, lat = lat2)



nombres2 = c("CPC", "GPCC", "CMAP")

for(i in 1:3){
  
  mapa_topo3(variable =  pp.bias_mean[,,,i]*mask_arr, colorbar = "BrBG", revert = F, escala = seq(-100, 100, by = 20)
             , titulo = paste("Bias PP - ", nombres2[i], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/desemp_mods/", nombre.fig = paste("pp.bias_",nombres2[i], sep = ""), na.fill = -1000
             , r = 4, estaciones = T, altura.topo = 1500, size.point = 1
             , lon = lon2, lat = lat2)
}




#### MAE ####

t.mae = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8))

for(i in 1:8){
  
  t.mae[,,,i] = apply(abs(t.mods[,,,,i] - datos.obs[,,,,1]), c(1,2,4), sum, na.rm = T)/29
  
}
t.mae_mean = apply(t.mae, c(1,2,3), mean, na.rm = T)*mask_arr



pp.mae = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8, 3))

for(i in 1:8){
  for(j in 2:4){
    
    pp.mae[,,,i,j-1] = apply(abs(pp.mods[,,,,i] - datos.obs[,,,,j]), c(1,2,4), sum, na.rm = T)/29
    
  }
  
}
pp.mae_mean = apply(pp.mae, c(1,2,3,5), mean, na.rm = T)



#--- graficos ---#


mapa_topo3(variable = t.mae_mean*mask_arr, colorbar = "Reds", revert = F, escala = seq(0, 5, by = 1)
           , titulo = paste("MAE T - CPC ", sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/desemp_mods/", nombre.fig = paste("t.mae_cpc", sep = ""), na.fill = -1000
           , r = 4, estaciones = T, altura.topo = 1500, size.point = 1
           , lon = lon2, lat = lat2)


for(i in 1:3){
  
  mapa_topo3(variable = pp.mae_mean[,,,i]*mask_arr, colorbar = "PuBuGn", revert = F, escala = seq(0, 100, by = 20)
             , titulo = paste("MAE PP - ", nombres2[i], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/desemp_mods/", nombre.fig = paste("pp.mae_",nombres2[i], sep = ""), na.fill = -1000
             , r = 4, estaciones = T, altura.topo = 1500, size.point = 1
             , lon = lon2, lat = lat2)
}



#### RMSE ####

t.rmse = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8))

for(i in 1:8){
  
  t.rmse[,,,i] = sqrt(apply((t.mods[,,,,i] - datos.obs[,,,,1])**2, c(1,2,4), sum, na.rm = T)/29)
  
}
t.rmse_mean = apply(t.rmse, c(1,2,3), mean, na.rm = T)*mask_arr


pp.rmse = array(data = NA, dim =c(length(lon2), length(lat2), 4, 8, 3))

for(i in 1:8){
  for(j in 2:4){
    
    pp.rmse[,,,i,j-1] = sqrt(apply((pp.mods[,,,,i] - datos.obs[,,,,j])**2, c(1,2,4), sum, na.rm = T)/29)
    
  }
  
}
pp.rmse_mean = apply(pp.rmse, c(1,2,3,5), mean, na.rm = T)



#--- graficos ---#
mapa(lista = t.rmse_mean*mask_arr, titulo = paste("RMSE T - CPC ", sep = ""), nombre_fig = paste("t.rmse_cpc", sep = ""), escala = c(0,5) 
     , label_escala = "", resta = 0, brewer = "Reds", revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 5, by = 1), seq(0, 5, by = 1), topo2, "/salidas/desemp_mods/")

mapa_topo3(variable = t.rmse_mean*mask_arr, colorbar = "Reds", revert = F, escala = seq(0, 5, by = 1)
           , titulo = paste("RMSE T - CPC ", sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/desemp_mods/", nombre.fig = paste("t.rmse_cpc", sep = ""), na.fill = -1000
           , r = 4, estaciones = T, altura.topo = 1500, size.point = 1
           , lon = lon2, lat = lat2)


for(i in 1:3){
  
  mapa_topo3(variable = pp.rmse_mean[,,,i]*mask_arr, colorbar = "PuBuGn", revert = F, escala = seq(0, 100, by = 20)
             , titulo = paste("RMSE PP - ", nombres2[i], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/desemp_mods/", nombre.fig = paste("pp.rmse_",nombres2[i], sep = ""), na.fill = -1000
             , r = 4, estaciones = T, altura.topo = 1500, size.point = 1
             , lon = lon2, lat = lat2)
}

#### Fig.8 ####



fig8(v1 = t.ACC, v2 = t.ACC_teo_prom, lon = lon2, lat = lat2, titulo = "Temperatura: ACC Observado vs ACC Teorico", color = "firebrick", y.name = "ACC", x.name = "ACC Teorico"
     , nombre.fig = "t.fig8_accvsteo", salida = "/salidas/desemp_mods/")

fig8(v1 = pp.ACC[,,,3], v2 = pp.ACC_teo_prom, lon = lon2, lat = lat2, titulo = "Precipitación: ACC Observado vs ACC Teorico", color = "royalblue4", y.name = "ACC", x.name = "ACC Teorico"
     , nombre.fig = "pp.fig8_accvsteo", salida = "/salidas/desemp_mods/")
