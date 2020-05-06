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

num = array(data = NA, dim = c(56, 76, 4, 8))
den_out = array(data = NA, dim = c(56, 76, 4, 8))
#numerador
for(i in 1:8){
  aux = Op_obs[,,1:29,,1]*Fp_out_t[,,,,i] 
  num_out[,,,i] = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
  den_out[,,,i] = ((apply(Op_obs[,,,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_out_t[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 
}


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




mapa_sig2(lista = t.ACC, titulo = paste("ACC Temp Ensamble y CPC", sep = ""), nombre_fig = paste("t.ACC_ensamble", sep = ""), escala = c(0, 1) 
          , label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 9
          , contour = "si", lon2, lat2, seq(0, 1, by = 0.1), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/")




nombres2 = c("CPC", "GPCC", "CMAP")

mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}


#for(i in 1:3){
i = 3 # solo CMAP
mapa_sig2(lista = pp.ACC[,,,i]*mask_arr, titulo = paste("ACC PP MODS y ", nombres2[i], sep = ""), nombre_fig = paste("AC_pp_", nombres2[i], sep = ""), escala = c(0, 1) 
         , label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 11
         , contour = "si", lon2, lat2, seq(0, 1, by = 0.1), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/")
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
  
  mapa_sig2(lista = t.ACC_oneless[,,,i], titulo = paste("ACC Temp MODS y CPC sin ", modelos[i], sep = ""), nombre_fig = paste("t.ACC_sin_", modelos[i], sep = ""), escala = c(0, 1) 
            , label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 9
            , contour = "si", lon2, lat2, seq(0, 1, by = 0.2), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/AC_sin_mods/")
  
}


nombres2 = c("CPC", "GPCC", "CMAP")

mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}


for(j in 1:8){
  
  #for(i in 1:3){
   i = 3 
    mapa_sig2(lista = pp.ACC_oneless[,,,j,i]*mask_arr, titulo = paste("ACC PP MODS y ", nombres2[i], " sin ", modelos[j],  sep = ""), nombre_fig = paste("pp.ACC_", nombres2[i],"sin_", modelos[j], sep = ""), escala = c(0, 1) 
              , label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 11
              , contour = "si", lon2, lat2, seq(0, 1, by = 0.2), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/AC_sin_mods/")
  #}
  
}


#### ACC teorico ####

