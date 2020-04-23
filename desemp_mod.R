# desempeño de modelos.

### Anomaly Correlation ###


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

#descargar topografia para mapas.

topo = metR::GetTopography(-85 + 359.5, -29 + 359.5, 15.5,  -60.5, resolution = 1/15) # mapa topografia
topo2 = topo #
topo2[which(topo2$h<1500)]=NA # altura para la cual tapa el grafico


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
prom_est_obs = array(data = NA, dim = c(56, 76, 29, 4, 4)) # uso misma cantidad de años que los modelos
prom_est_obs[,,,,1] = prom_est_cpc_t[,,1:29,] 
prom_est_obs[,,,,2] = prom_est_cpc_pp[,,1:29,]
prom_est_obs[,,,,3] = prom_est_gpcc_pp[,,1:29,]
prom_est_obs[,,,,4] = prom_est_cmap_pp[2:57,2:77,1:29,]  # este tenia + lats y lons por el grillado



########################## Cross Validation ##########################
# 
# para cada año tengo q tener promedio de todos los años menos ese año.

aux = diag(29)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 29, 4, 29, 4))  
aux2_obs = array(data = 1, dim = c(56, 76, 29, 4, 29, 4))

c_v_obs = array(data = NA, dim = c(56, 76, 29, 4, 4)) # para las 4 base de datos, la 1era temp y las otras pp

for(i in 1:29){
  
  aux2[,,i,,i,] = aux2[,,i,,i,]*aux[i,i] # como matriz identidad inversa con NA en la diagonal y 1 pero en 4 dimenciones.
  
  aux2_obs[,,,,i,] = aux2[,,,,i,]*prom_est_obs
 
  # promedio sacando cada año.
  
  c_v_obs[,,i,,] = apply(aux2_obs[,,,,i,], c(1,2,4,5), mean, na.rm = T)
  
}



### O' 
Op_obs = prom_est_obs - c_v_obs
  


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
prom_mods_t = array(data = NA, dim = c(56, 76, 29, 4, 8)) # recordar, los modelos 1982-2010 (29 años)
prom_mods_pp = array(data = NA, dim = c(56, 76, 29, 4, 8))
for(i in 1:length(modelos)){
  v = mean_sd(modelos[i])
  prom_mods_t[,,,,i] = v[[5]]
  prom_mods_pp[,,,,i] = v[[6]]
}  


########################## Sacando modelos  ##########################

prom_est_mods_out_t = array(data = NA, dim = c(56, 76, 29, 4, 8)) # 8 promedios sacando cada modelo
prom_est_mods_out_pp = array(data = NA, dim = c(56, 76, 29, 4, 8))

for(i in 1:8){
  aux_t = prom_mods_t
  aux_t[,,,,i] = NA
  prom_est_mods_out_t[,,,,i] = apply(aux_t, c(1, 2, 3, 4), mean, na.rm = T)
  
  aux_pp = prom_mods_pp
  aux_pp[,,,,i] = NA
  prom_est_mods_out_pp[,,,,i] = apply(aux_pp, c(1, 2, 3, 4), mean, na.rm = T)
}

########################## Ensamble completo #########################

prom_est_mods_t  = apply(prom_mods_t, c(1, 2, 3, 4), mean, na.rm = T)
prom_est_mods_pp = apply(prom_mods_pp, c(1, 2, 3, 4), mean, na.rm = T)




########################## Cross Validation ##########################
# T y PP

aux = diag(29)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 29, 4, 29))
aux2_pp = array(data = 1, dim = c(56, 76, 29, 4, 29))

aux2_mod_t = array(data = 1, dim = c(56, 76, 29, 4, 29))
aux2_mod_pp = array(data = 1, dim = c(56, 76, 29, 4, 29))

c_v_mod_t = array(data = NA, dim = c(56, 76, 29, 4))
c_v_mod_pp = array(data = NA, dim = c(56, 76, 29, 4))

for(i in 1:29){
  aux2[,,i,,i] = aux2[,,i,,i]*aux[i,i]  # una especie de matriz identidad inversa con NA y 1 pero en 4 dim.

  aux2_mod_t[,,,,i] = aux2[,,,,i]*prom_est_mods_t 
  aux2_mod_pp[,,,,i] = aux2[,,,,i]*prom_est_mods_pp
  
  
  # promedio sacando cada año.
  c_v_mod_t[,,i,] = apply(aux2_mod_t[,,,,i], c(1, 2, 4), mean, na.rm = T)
  c_v_mod_pp[,,i,] = apply(aux2_mod_pp[,,,,i], c(1, 2, 4), mean, na.rm = T)
}

### F'
Fp_t = prom_est_mods_t - c_v_mod_t
Fp_pp = prom_est_mods_pp - c_v_mod_pp

# AC. Temp

AC_t = array(data = NA, dim = c(56, 76, 4))

  
#numerador
aux = Op_obs[,,1:29,,1]*Fp_t # Op_obs[,,,,,1] solo temp
num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
#denominador
den = ((apply(Op_obs[,,,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_t**2, c(1, 2, 4), sum, na.rm = T))/29) 
  
AC_t = num/sqrt(den)
  


# AC PP

AC_pp = array(data = NA, dim = c(56, 76, 4, 3))

for(j in 2:4){
  aux = Op_obs[,,1:29,,j]*Fp_pp
  num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    
  den = ((apply(Op_obs[,,,,j]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_pp**2, c(1, 2, 4), sum, na.rm = T))/29)
    
  AC_pp[,,,j-1] = num/sqrt(den)
}

#r critico
# de t -student
rc = qt(p = 0.95,df = 29-1)/sqrt((29-1)+qt(p = 0.95,df = 29-1))


#####-----------------------------######

source("funciones.R")

mapa_sig2(lista = AC_t, titulo = paste("ACC Temp MODS y CPC", sep = ""), nombre_fig = paste("AC_temp", sep = ""), escala = c(0, 1) 
     , label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 1, by = 0.2), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, 15, 1500, "/salidas/desemp_mods/")


nombres2 = c("CPC", "GPCC", "CMAP")

mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}

AC_pp[,,,3] = AC_pp[,,,3]*mask_arr
for(i in 1:3){

mapa_sig2(lista = AC_pp[,,,i], titulo = paste("ACC PP MODS y ", nombres2[i], sep = ""), nombre_fig = paste("AC_pp_", nombres2[i], sep = ""), escala = c(0, 1) 
     , label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 11
     , contour = "si", lon2, lat2, seq(0, 1, by = 0.2), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, 15, 1500, "/salidas/desemp_mods/")
}
####################################################################################################################################################################
########################## Cross Validation sacando modelos y con modelos individuales ##########################
# T y PP

aux = diag(29)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29))

aux2_mod_out_t = array(data = 1, dim = c(56, 76, 29, 4, 8, 29))
aux2_mod_out_pp = array(data = 1, dim = c(56, 76, 29, 4, 8, 29))

aux2_mod_sin_t = array(data = 1, dim = c(56, 76, 29, 4, 8, 29))
aux2_mod_sin_pp = array(data = 1, dim = c(56, 76, 29, 4, 8, 29))

c_v_mod_sin_t = array(data = NA, dim = c(56, 76, 29, 4, 8))
c_v_mod_sin_pp = array(data = NA, dim = c(56, 76, 29, 4, 8))

c_v_mod_out_t = array(data = NA, dim = c(56, 76, 29, 4, 8))
c_v_mod_out_pp = array(data = NA, dim = c(56, 76, 29, 4, 8))

for(i in 1:29){
  aux2[,,i,,,i] = aux2[,,i,,,i]*aux[i,i]  # una especie de matriz identidad inversa con NA y 1 pero en 4 dim.

  aux2_mod_out_t[,,,,,i] = aux2[,,,,,i]*prom_est_mods_out_t
  aux2_mod_out_pp[,,,,,i] = aux2[,,,,,i]*prom_est_mods_out_pp
  
  # promedio sacando cada anio
  # recordar: aca hay 8 ensambles a los que se les saco un modelo en cada caso
  c_v_mod_out_t[,,i,,] = apply(aux2_mod_out_t[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)   
  c_v_mod_out_pp[,,i,,] = apply(aux2_mod_out_pp[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)
  

  # AC teorico ---> calculo en ## AC teorico ##
  # recordar: aca hay 8 modelos
  aux2_mod_sin_t[,,,,,i] = aux2[,,,,,i]*prom_mods_t
  aux2_mod_sin_pp[,,,,,i] = aux2[,,,,,i]*prom_mods_pp
  
  
  c_v_mod_sin_t[,,i,,] = apply(aux2_mod_sin_t[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)
  c_v_mod_sin_pp[,,i,,] = apply(aux2_mod_sin_pp[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)
  
  
}

### F'
Fp_out_t = prom_est_mods_out_t - c_v_mod_out_t
Fp_out_pp = prom_est_mods_out_pp - c_v_mod_out_pp

# AC. Temp

AC_out_t = array(data = NA, dim = c(56, 76, 4, 8))

num_out = array(data = NA, dim = c(56, 76, 4, 8))
den_out = array(data = NA, dim = c(56, 76, 4, 8))
#numerador
for(i in 1:8){
  aux = Op_obs[,,1:29,,1]*Fp_out_t[,,,,i] 
  num_out[,,,i] = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
  den_out[,,,i] = ((apply(Op_obs[,,,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_out_t[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 
}


AC_out_t = num_out/sqrt(den_out)


# AC PP

AC_out_pp = array(data = NA, dim = c(56, 76, 4, 8, 3))
for(i in 1:8){
  
  for(j in 2:4){
    
    aux = Op_obs[,,1:29,,j]*Fp_out_pp[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    
    den = ((apply(Op_obs[,,,,j]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_out_pp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29)
    
    AC_out_pp[,,,i,j-1] = num/sqrt(den)
    
  }
}

#r critico
# de t -student
rc = qt(p = 0.95,df = 29-1)/sqrt((29-1)+qt(p = 0.95,df = 29-1))

#####-----------------------------######
source("funciones.R")
for(i in 1:8){
  
  mapa_sig2(lista = AC_out_t[,,,i], titulo = paste("ACC Temp MODS y CPC sin ", modelos[i], sep = ""), nombre_fig = paste("AC_temp_sin_", modelos[i], sep = ""), escala = c(0, 1) 
            , label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 9
            , contour = "si", lon2, lat2, seq(0, 1, by = 0.2), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/AC_sin_mods/")
  
}


nombres2 = c("CPC", "GPCC", "CMAP")

mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}

#AC_out_pp[,,,3] = AC_pp[,,,,3]*mask_arr
for(j in 1:8){
  
  for(i in 1:3){
    
    mapa_sig2(lista = AC_out_pp[,,,j,i]*mask_arr, titulo = paste("ACC PP MODS y ", nombres2[i], " sin ", modelos[j],  sep = ""), nombre_fig = paste("AC_pp_", nombres2[i],"sin_", modelos[j], sep = ""), escala = c(0, 1) 
              , label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 11
              , contour = "si", lon2, lat2, seq(0, 1, by = 0.2), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/AC_sin_mods/")
  }
  
}

####################################################################################################################################################################
########################## AC teorico ##########################


### F'
Fp_sin_t = prom_mods_t - c_v_mod_sin_t
Fp_sin_pp = prom_mods_pp - c_v_mod_sin_pp

# AC. Temp

AC_sin_t = array(data = NA, dim = c(56, 76, 4, 8))

num_sin = array(data = NA, dim = c(56, 76, 4, 8))
den_sin = array(data = NA, dim = c(56, 76, 4, 8))
#numerador
for(i in 1:8){ # pasando por cada modelo
  aux = Fp_t*Fp_sin_t[,,,,i]   # AC ensamble vs AC modelo (originalmente obs * F)
  num_sin[,,,i] = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
  den_sin[,,,i] = ((apply(Fp_t**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_sin_t[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 
}


AC_sin_t = num_sin/sqrt(den_sin)


# AC PP

AC_sin_pp = array(data = NA, dim = c(56, 76, 4, 8))
for(i in 1:8){

    aux = Fp_pp*Fp_sin_pp[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 

    den = ((apply(Fp_pp**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_sin_pp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29)
    
    AC_sin_pp[,,,i] = num/sqrt(den)
  
}

AC_teo_prom_t = apply(AC_sin_t, c(1,2,3), mean, na.rm = T)
AC_teo_prom_pp = apply(AC_sin_pp, c(1,2,3), mean, na.rm = T)
#r critico
# de t -student
rc = qt(p = 0.95,df = 29-1)/sqrt((29-1)+qt(p = 0.95,df = 29-1))

#####-----------------------------######
source("funciones.R")


mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}



for(i in 1:8){
  
  
  mapa_sig2(lista = AC_sin_t[,,,i]*mask_arr, titulo = paste("ACC Temp Ensamble y ", modelos[i], sep = ""), nombre_fig = paste("AC_temp_con_", modelos[i], sep = ""), escala = c(0, 1) 
            , label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 9
            , contour = "si", lon2, lat2, seq(0, 1, by = 0.1), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/AC_con_mods/")
 
  mapa_sig2(lista = AC_sin_pp[,,,i]*mask_arr, titulo = paste("ACC PP  y ", modelos[j],  sep = ""), nombre_fig = paste("AC_pp_con_", modelos[i], sep = ""), escala = c(0, 1) 
            , label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 11
            , contour = "si", lon2, lat2, seq(0, 1, by = 0.1), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/AC_con_mods/")
  
}

mapa_sig2(lista = AC_teo_prom_t*mask_arr, titulo = paste("ACC Teorico Temperatura", sep = ""), nombre_fig = paste("AC_teo_temp", sep = ""), escala = c(0, 1) 
          , label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 9
          , contour = "si", lon2, lat2, seq(0, 1, by = 0.1), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/AC_con_mods/")

mapa_sig2(lista = AC_teo_prom_pp*mask_arr, titulo = paste("ACC Teorico Precipitación", sep = ""), nombre_fig = paste("AC_teo_pp", sep = ""), escala = c(0, 1) 
          , label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9
          , contour = "si", lon2, lat2, seq(0, 1, by = 0.1), seq(0, 1, by = 0.1), alpha = 0.3, size = 1, color = "black", v = rc, topo2, "/salidas/desemp_mods/AC_con_mods/")

####################################################################################################################################################################
############################ Fig 10 ##################################
# AC modelos vs obs

# Ac temp

AC_mods_obs_t = array(data = NA, dim = c(56, 76, 4, 8))

num_mods = array(data = NA, dim = c(56, 76, 4, 8))
den_mods = array(data = NA, dim = c(56, 76, 4, 8))
#numerador
for(i in 1:8){ # pasando por cada modelo
  aux = Op_obs[,,1:29,,1]*Fp_sin_t[,,,,i]   # AC ensamble vs AC modelo (originalmente obs * F)
  num_mods[,,,i] = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  
  den_mods[,,,i] = ((apply(Op_obs[,,1:29,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_sin_t[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 
}

AC_mods_obs_t = num_mods/sqrt(den_mods)


# AC pp


AC_mods_obs_pp = array(data = NA, dim = c(56, 76, 4, 8, 3))
for(j in 2:4){

  for(i in 1:8){
    
    aux = Op_obs[,,1:29,,j]*Fp_sin_pp[,,,,i]
    num_mods_pp = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    
    den_mods_pp = ((apply(Op_obs[,,,,j]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(Fp_sin_pp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29)
    
    AC_mods_obs_pp[,,,i,j-1] = num_mods_pp/sqrt(den_mods_pp)
    
  }
    
}


cajas = c("Am", "SAM", "NeB", "SACZ", "LPB")
cajas_num = seq( 1, 5)


cajas_lat = list()
cajas_lat[[1]] =  seq(which(lat2 == -13), which(lat2 == 2), by = 1); cajas_lat[[2]] = seq(which(lat2 == -16), which(lat2 == 4), by = 1)
cajas_lat[[3]] = seq(which(lat2 == -16), which(lat2 == 2), by = 1); cajas_lat[[4]] = seq(which(lat2 == -26), which(lat2 == -17), by = 1)
cajas_lat[[5]] = seq(which(lat2 == -39), which(lat2 == -24), by = 1)

cajas_lon = list()
cajas_lon[[1]] =  seq(which(lon2 == 291), which(lon2 == 304), by = 1); cajas_lon[[2]] = seq(which(lon2 == 301), which(lon2 == 316), by = 1)
cajas_lon[[3]] = seq(which(lon2 == 313), which(lon2 == 326), by = 1); cajas_lon[[4]] = seq(which(lon2 == 308), which(lon2 == 321), by = 1)
cajas_lon[[5]] = seq(which(lon2 == 296), which(lon2 == 309), by = 1)

prom_cajas_t = list()
prom_cajas_pp = list()
prom_ensamble_t = list()
prom_ensamble_pp = list()
for(i in 1:5){
  
  prom_ensamble_t[[i]] = apply(AC_t[cajas_lon[[i]], cajas_lat[[i]],], c(3), mean, na.rm = T)
  prom_ensamble_pp[[i]] = apply(AC_pp[cajas_lon[[i]], cajas_lat[[i]],,], c(3, 4), mean, na.rm = T)
  
  prom_cajas_t[[i]] = apply(AC_mods_obs_t[cajas_lon[[i]], cajas_lat[[i]],,], c(3, 4), mean, na.rm = T)
  
  prom_cajas_pp[[i]] = apply(AC_mods_obs_pp[cajas_lon[[i]], cajas_lat[[i]],,,], c(3, 4, 5), mean, na.rm = T)
  
}


data_t = fig10(prom_cajas = prom_cajas_t, prom_ensamble = pprom_ensamble_t, variable = "temp")

ggplot() + theme_minimal() +
  geom_text(data = data_t[[1]], aes(x = cajas, y = SON, label = value), color = "green", size = 4) + 
  geom_point(data = data_t[[2]], aes(x = cajas, y = SON), color = "red", shape = "*" , size = 8) + 
  geom_hline(yintercept = rc, color = "grey") +
  ggtitle(" ACC T ")+
  scale_y_continuous(limits = c(0, 0.8)) + 
  scale_x_continuous(labels=c("1" = "Am", "2" = "SAM", "3" = "NeB", "4" = "SACZ", "5" = "LPB"))+
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 3),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5))


## pp
source("funciones.R")
data_pp = list()
for( i in 1:3){
  data_pp[[i]] = fig10(prom_cajas = prom_cajas_pp, prom_ensamble = prom_ensamble_pp, variable = "pp", base_datos = i)
}
est = c("MAM", "JJA", "SON", "DJF")
g = list()
for(i in 2:5){
  
  g = ggplot() + theme_minimal()+
    geom_text(data = data_t[[1]], aes(x = cajas, y = data_t[[1]][,i], label = value), color = "green", size = 4) + 
    geom_point(data = data_t[[2]], aes(x = cajas, y = data_t[[2]][,i]), color = "darkgreen", shape = "*" , size = 11) + 
    geom_text(data = data_pp[[1]][[1]], aes(x = cajas, y = data_pp[[1]][[1]][,i], label = value), color = "blue", size = 4) + 
    geom_point(data = data_pp[[1]][[2]], aes(x = cajas , y = data_pp[[1]][[2]][,i]), color = "navyblue", shape = "*" , size = 11) + 
    geom_text(data = data_pp[[2]][[1]], aes(x = cajas, y = data_pp[[2]][[1]][,i], label = value), color = "purple", size = 4) + 
    geom_point(data = data_pp[[2]][[2]], aes(x = cajas , y = data_pp[[2]][[2]][,i]), color = "purple4", shape = "*" , size = 11) + 
    geom_text(data = data_pp[[3]][[1]], aes(x = cajas, y = data_pp[[3]][[1]][,i], label = value), color = "deepskyblue", size = 4) + 
    geom_point(data = data_pp[[3]][[2]], aes(x = cajas , y = data_pp[[3]][[2]][,i]), color = "deepskyblue4", shape = "*" , size = 11) + 
    
    geom_hline(yintercept = rc, color = "grey", size = 1) +
    #geom_hline(yintercept = 0, color = "black")+
    ggtitle(paste("ACC - ", est[i-1], sep = ""))+
    #scale_y_continuous(limits = c(-0.2, 0.8), breaks = seq(-0.2,0.8, by = 0.2)) + 
    scale_y_continuous(limits = c(0, 0.8), breaks = seq(0,0.8, by = 0.2)) + 
    scale_x_continuous(labels=c("1" = "Am", "2" = "SAM", "3" = "NeB", "4" = "SACZ", "5" = "LPB"),breaks = seq(1, 5, by = 1))+
    xlab(label = "COLA-CCSM4(1), GFDL-CM2p1(2), GFDL-FLOR-A06(3), GFDL-FLOR-B01(4), NASA-GEOS5(5), NCEP-CFSv2(6) CMC-CanCM4i(7), CMC-CanSIPSv2(8)" )+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black", face = "bold"), axis.title.y  = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5)) 
    ggsave(paste("/home/luciano.andrian/tesis/salidas/desemp_mods/AC_con_mods/", "ACC", "_", est[i-1], ".jpg",sep =""), plot = g, width = 30, height = 15  , units = "cm")
  
}
  


####################################################################################################################################################################

### BIAS ###

b_t = apply((prom_est_mods_t - prom_est_obs[,,,,1]), c(1,2,4), sum, na.rm = T)/29

mapa(lista = b_t*mask_arr, titulo = paste("Bias T - CPC ", sep = ""), nombre_fig = paste("bias_t_cpc", sep = ""), escala = c(-10,10) 
     , label_escala = "", resta = 0, brewer = "RdBu", revert = "si", niveles = 11
     , contour = "si", lon2, lat2, seq(-10, 10, by = 2), seq(-10, 10, by = 1), 15, 1500, "/salidas/desemp_mods/")

nombres2 = c("CPC", "GPCC", "CMAP")

b_pp = array(data = NA, dim = c(56,76,4,3))

for(i in 2:4){
  
  b_pp[,,,i-1] = (apply(prom_est_mods_pp - prom_est_obs[,,,,i], c(1,2,4), sum, na.rm = T)/29)
  
}

for(i in 1:3){
 
   mapa(lista = b_pp[,,,i]*mask_arr, titulo = paste("Bias PP - ", nombres2[i], sep = ""), nombre_fig = paste("bias_pp_",nombres2[i], sep = ""), escala = c(-100,100) 
       , label_escala = "", resta = 0, brewer = "BrBG", revert = "no", niveles = 11
       , contour = "si", lon2, lat2, seq(-100, 100, by = 20), seq(-100, 100, by = 10), 15, 1500, "/salidas/desemp_mods/")
}




### MAE ###

mae_t = (apply(abs(prom_est_mods_t - prom_est_obs[,,,,1]), c(1,2,4), sum, na.rm = T)/29) 

mapa(lista = mae_t*mask_arr, titulo = paste("MAE T - CPC ", sep = ""), nombre_fig = paste("mae_t_cpc", sep = ""), escala = c(0,5) 
     ,label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 9
     , contour = "si", lon2, lat2, seq(0, 5, by = 0.5), seq(0, 5, by = 0.25),15, 1500, "/salidas/desemp_mods/")


mae_pp = array(data = NA, dim = c(56,76,4,3))

for(i in 2:4){
  
  mae_pp[,,,i-1] = apply(abs(prom_est_mods_pp - prom_est_obs[,,,,i]), c(1,2,4), sum, na.rm = T)/29
  
}

for(i in 1:3){
  
  mapa(lista = mae_pp[,,,i]*mask_arr, titulo = paste("MAE PP -  ", nombres2[i], sep = ""), nombre_fig = paste("mae_pp_", nombres2[i], sep = ""), escala = c(0,100) 
       ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 11
       , contour = "si", lon2, lat2, seq(0, 100, by = 10), seq(0, 100, by = 5), 15, 1500, "/salidas/desemp_mods/")
}





### RMSE ###

rmse_t = sqrt(apply(( prom_est_mods_t - prom_est_obs[,,,,1] )**2, c(1,2,4), sum, na.rm = T)/29)

mapa(lista = rmse_t*mask_arr, titulo = paste("RMSE T - CPC", sep = ""), nombre_fig = paste("rmse_t_cpc", sep = ""), escala = c(0,6) 
     ,label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 11
     , contour = "si", lon2, lat2, seq(0, 6, by = 1), seq(0, 6, by = 0.5), 15, 1500, "/salidas/desemp_mods/")


rmse_pp = array(data = NA, dim = c(56,76,4,3))

for(i in 2:4){
  
  rmse_pp[,,,i-1] = sqrt(apply((prom_est_mods_pp - prom_est_obs[,,,,i] )**2, c(1,2,4), sum, na.rm = T)/29)
  
}

for(i in 1:3){
  
  mapa(lista = rmse_pp[,,,i]*mask_arr, titulo = paste("RMSE PP - ", nombres2[i], sep = ""), nombre_fig = paste("rmse_pp_", nombres2[i], sep = ""), escala = c(0,160) 
       ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9
       , contour = "si", lon2, lat2, seq(0, 160, by = 20), seq(0, 160, by = 10) ,15, 1500, "/salidas/desemp_mods/")
  
}



  
  
  
  
  
  









