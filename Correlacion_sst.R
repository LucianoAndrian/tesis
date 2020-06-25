# Correlacion SST
library(ncdf4)
source("funciones.R")
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

#### SST ####
aux = nc_open("X140.172.38.222.142.12.42.20.nc")
sst = ncvar_get(aux, "sst")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
nc_close(aux)
# rotando el array
sst = sst[,ncol(sst):1,]

#### mascara ####
aux = nc_open("X190.191.246.159.142.12.48.11.nc")
w.mask = ncvar_get(aux, "mask")
nc_close(aux)
# rotando..
w.mask = w.mask[,ncol(w.mask):1] 

#### meses de ci ####
sst.ci = array(data = NA, dim = c(360, 180, 29, 4))
ci = c(2, 5, 8, 11)
for(i in 1:4){
  for(j in 0:28)
    sst.ci[,,j + 1, i] = sst[,,ci[i]+12*j]*w.mask
}


####  Modelos ####
aux = list()
nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2") 
for(i in 1:8){
  aux[[i]] = mean_sd(nombres[i]) ## para obterner T2 y PP2 de la funcion
}

aux2 = array(data = NA, dim = c(56,76,29,4,8))
aux3 = array(data = NA, dim = c(56,76,29,4,8))
for(i in 1:8){
  aux2[,,,,i] = aux[[i]][[5]]
  aux3[,,,,i] = aux[[i]][[6]]
}
####
#---- ensambles ----#
t.ens = apply(aux2, c(1,2,3,4), mean, na.rm = T)
pp.ens = apply(aux3, c(1,2,3,4), mean, na.rm = T)

#### cajas ####
#----falta alguna? -----#
lats = list()
lats[[1]] =  seq(which(lat2 == -13), which(lat2 == 2), by = 1); lats[[2]] = seq(which(lat2 == -16), which(lat2 == 4), by = 1)
lats[[3]] = seq(which(lat2 == -16), which(lat2 == 2), by = 1); lats[[4]] = seq(which(lat2 == -26), which(lat2 == -17), by = 1)
lats[[5]] = seq(which(lat2 == -39), which(lat2 == -24), by = 1); lats[[6]] = seq(which(lat2 == -7), which(lat2 == 5), by = 1)
lats[[7]] = seq(which(lat2 == -24), which(lat2 == -15), by = 1); lats[[8]] = seq(which(lat2 == -15), which(lat2 == 2), by = 1)
lats[[9]] = seq(which(lat2 == -32), which(lat2 == -22), by = 1); lats[[10]] = seq(which(lat2 == -45), which(lat2 == -27), by = 1)
lats[[11]] = seq(which(lat2 == -37), which(lat2 == -20), by = 1)

lons = list()
lons[[1]] =  seq(which(lon2 == 291), which(lon2 == 304), by = 1); lons[[2]] = seq(which(lon2 == 301), which(lon2 == 316), by = 1)
lons[[3]] = seq(which(lon2 == 313), which(lon2 == 326), by = 1); lons[[4]] = seq(which(lon2 == 308), which(lon2 == 321), by = 1)
lons[[5]] = seq(which(lon2 == 296), which(lon2 == 309), by = 1); lons[[6]] = seq(which(lon2 == 295), which(lon2 == 325), by = 1)
lons[[7]] = seq(which(lon2 == 305), which(lon2 == 330), by = 1); lons[[8]] = seq(which(lon2 == 305), which(lon2 == 320), by = 1)
lons[[9]] = seq(which(lon2 == 297), which(lon2 == 310), by = 1); lons[[10]] = seq(which(lon2 == 292), which(lon2 == 307), by = 1)
lons[[11]] = seq(which(lon2 == 298), which(lon2 == 313), by = 1)

lat = rev(lat)
lats.area = list() # un poco distintas debido al grillado del mapa. 1x1 pero 1.5 2.5 3.5
lats.area[[1]] =  seq(which(lat == -13.5), which(lat == 2.5), by = 1); lats.area[[2]] = seq(which(lat == -16.5), which(lat == 4.5), by = 1)
lats.area[[3]] = seq(which(lat == -16.5), which(lat == 2.5), by = 1); lats.area[[4]] = seq(which(lat == -26.5), which(lat == -17.5), by = 1)
lats.area[[5]] = seq(which(lat == -39.5), which(lat == -24.5), by = 1); lats.area[[6]] = seq(which(lat == -7.5), which(lat == 5.5), by = 1)
lats.area[[7]] = seq(which(lat == -27.5), which(lat == -15.5), by = 1); lats.area[[8]] = seq(which(lat == -15.5), which(lat == 2.5), by = 1)
lats.area[[9]] = seq(which(lat == -32.5), which(lat == -22.5), by = 1);lats.area[[10]] = seq(which(lat == -45.5), which(lat == -27.5), by = 1)
lats.area[[11]] = seq(which(lat == -37.5), which(lat == -20.5), by = 1)

lons.area = list()
lons.area[[1]] =  seq(which(lon == 291.5), which(lon == 304.5), by = 1); lons.area[[2]] = seq(which(lon == 301.5), which(lon == 316.5), by = 1)
lons.area[[3]] = seq(which(lon == 313.5), which(lon == 326.5), by = 1); lons.area[[4]] = seq(which(lon == 308.5), which(lon == 321.5), by = 1)
lons.area[[5]] = seq(which(lon == 296.5), which(lon == 309.5), by = 1); lons.area[[6]] = seq(which(lon == 295.5), which(lon == 325.5), by = 1)
lons.area[[7]] = seq(which(lon == 300.5), which(lon == 315.5), by = 1); lons.area[[8]] = seq(which(lon == 305.5), which(lon == 320.5), by = 1)
lons.area[[9]] = seq(which(lon == 297.5), which(lon == 310.5), by = 1); lons.area[[10]] = seq(which(lon == 292.5), which(lon == 307.5), by = 1)
lons.area[[11]] = seq(which(lon == 298.5), which(lon == 313.5), by = 1)
#### graficos ####

# temp
region = c("Amazonia", "South American Monsoon", "North-estern Brazil", "SACZ", "La Plata Basin", "Norte Amazonia", "South Brazil",  "North-estern Brazil 2", "SESA 2", "SESA", "SESA 3")
region.fig = c("Am", "SAM", "NeB", "SACZ", "LPB", "NA", "SB", "NeB2", "SESA_2", "SESA", "SESA_3")
estaciones = c("MAM", "JJA", "SON", "DJF")

for(i in 1:11){
  area = w.mask
  area[lons.area[[i]], lats.area[[i]]] = 2
  
  aux.prom = apply(t.ens[lons[[i]], lats[[i]],,], c(3,4), mean, na.rm = T)
  
  for(j in 1:4){
    
    aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
    
    # prueba mapa_topo3
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
               , titulo = paste("Correlación Temperatura en ", region[i], " con SST - ", estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
               , r = 1, width = 30, height = 15, salida =  "/salidas/corr/", nombre.fig = paste("t.corr_", region.fig[i],"_", estaciones[j], sep = ""), na.fill = 0
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, fill.mapa = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
    
  }
}



# pp
for(i in 1:11){
  area = w.mask
  area[lons.area[[i]], lats.area[[i]]] = 2
  
  aux.prom = apply(pp.ens[lons[[i]], lats[[i]],,], c(3,4), mean, na.rm = T)
  
  for(j in 1:4){
    
    aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
    
    # prueba mapa_topo3
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
               , titulo = paste("Correlación Precipitación en ", region[i], " con SST - ", estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
               , r = 1, width = 30, height = 15, salida =  "/salidas/corr/", nombre.fig = paste("pp.corr_", region.fig[i],"_", estaciones[j], sep = ""), na.fill = 0
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, fill.mapa = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
    
  }
}






for(i in 1:11){ # i = 5 LPB, i = 8 NeB2, i = 9 SESA 2, i = 10 SESA
  for(m in 1 :8){ #esto va hacer 320 graficos... usar m = 2 para gdfl-cm2p1
    area = w.mask
    area[lons.area[[i]], lats.area[[i]]] = 2
    aux.prom = apply(aux3[lons[[i]], lats[[i]],,,m], c(3,4), mean, na.rm = T)
    
    for(j in 1:4){
      
      aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
      
      aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
      aux.sig = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
      auxa = array(area, dim = c(dim(area), 1))
      
      mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
                 , titulo = paste("Correlación Precipitación con SST - ", nombres[m], "  ", estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
                 , r = 1, width = 30, height = 15, salida =  "/salidas/corr/", nombre.fig = paste("pp.corr_",region.fig[i], nombres[m],"_", estaciones[j], sep = ""), na.fill = 0
                 , sig = T, variable.sig = aux.sig, color.vsig = "white", alpha.vsig = 1, fill.mapa = T
                 , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
     }
  }
}








# Correlacion datos cmap con sst
# cambio de periodo para los dos, un año menos.

#### SST ####

aux = nc_open("X140.172.38.222.142.12.42.20.nc")
sst = ncvar_get(aux, "sst")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
nc_close(aux)
# rotando el array
sst = sst[,ncol(sst):1,1:336]

#### mascara ####
aux = nc_open("X190.191.246.159.142.12.48.11.nc")
w.mask = ncvar_get(aux, "mask")
nc_close(aux)
# rotando..
w.mask = w.mask[,ncol(w.mask):1] 

#### meses de ci ####
sst.ci = array(data = NA, dim = c(360, 180, 28, 4))
ci = c(2, 5, 8, 11)
for(i in 1:4){
  for(j in 0:27)
    sst.ci[,,j + 1, i] = sst[,,ci[i]+12*j]*w.mask
}


# pp cmap
require(fields)
aux = nc_open("/home/luciano.andrian/tesis/X190.191.242.210.56.5.48.49.nc")

lon4 = ncvar_get(aux, "lon")
lat4 = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[,,27:363]
nc_close(aux)



pp3_int = array(NA, dim = c(58, 78, 336)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:336){
  
  mod = list(x = lon4, y = lat4, z = aux2[,,i])
  
  grid = list(x=seq(min(lon4), max(lon4), by = 1), y = seq(min(lat4), max(lat2)+1, by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp3_int[,,i] = pp_aux$z  # algo esta mal con esta
}


pp3_estaciones = array(NA, dim = c(58, 78, 28, 12))

for(j in 1:12){
  for (i in 0:27){
    pp3_estaciones[,,1+i,j] = pp3_int[1:58 , 1:78, j+12*i]
  }
}


estaciones_p_a_pp3 = array(NA, dim = c(58, 78, 28, 4))
i=1
while(i<=4){
  estaciones_p_a_pp3[,,,i] = apply(pp3_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)*30 # esta en mm/day
  i = i + 1
}

pp.obs = estaciones_p_a_pp3


for(i in 1:11){
  area = w.mask
  area[lons.area[[i]], lats.area[[i]]] = 2
  
  aux.prom = apply(pp.obs[lons[[i]], lats[[i]],,], c(3,4), mean, na.rm = T)
  
  for(j in 1:4){
    
    aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
    
    # prueba mapa_topo3
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
               , titulo = paste("Correlación Precipitación en ", region[i], " con SST - ", estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
               , r = 1, width = 30, height = 15, salida =  "/salidas/corr/", nombre.fig = paste("pp.corr.cmap_", region.fig[i],"_", estaciones[j], sep = ""), na.fill = 0
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, fill.mapa = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
    
  }
}











# pp cpc

aux = nc_open("/home/luciano.andrian/tesis/X157.92.36.193.339.11.29.13.nc")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[which(lon==275.25):which(lon==330.25), which(lat==-60.25):which(lat==15.25),27:363]
require(fields)


lon3 = lon[which(lon==275.25):which(lon==330.25)]
lat3 = lat[which(lat==-60.25):which(lat==15.25)]

pp2_int = array(NA, dim = c(56, 76, 336))

for(i in 1:336){
  
  mod = list(x = lon3, y = lat3, z = aux2[,,i])
  
  grid = list(x=seq(min(lon3), max(lon3), by = 1), y = seq(min(lat3), max(lat3), by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp2_int[,,i] = pp_aux$z
}

pp2_estaciones = array(NA, dim = c(56, 76, 28, 12))

for(j in 1:12){
  for (i in 0:27){
    pp2_estaciones[,,1+i,j] = pp2_int[ , , j+12*i]
  }
}


estaciones_p_a_pp2 = array(NA, dim = c(56, 76, 28, 4))
i=1
while(i<=4){
  estaciones_p_a_pp2[,,,i] = apply(pp2_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}

estaciones_prom_pp2 = array(NA, dim = c(56, 76, 4))

for( i in 1:4){
  estaciones_prom_pp2[,,i] = apply(estaciones_p_a_pp2[,,,i], c(1,2), mean)
}




pp.obs = estaciones_p_a_pp2


for(i in 1:11){
  area = w.mask
  area[lons.area[[i]], lats.area[[i]]] = 2
  
  aux.prom = apply(pp.obs[lons[[i]], lats[[i]],,], c(3,4), mean, na.rm = T)
  
  for(j in 1:4){
    
    aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
    
    # prueba mapa_topo3
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
               , titulo = paste("Correlación Precipitación en ", region[i], " con SST - ", estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
               , r = 1, width = 30, height = 15, salida =  "/salidas/corr/", nombre.fig = paste("pp.corr.cpc_", region.fig[i],"_", estaciones[j], sep = ""), na.fill = 0
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, fill.mapa = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
    
  }
}

# da casi igual









#


# correlacion nino3.4 y pp SA
# observado. SST vs PP cmap, usando lo de arriba.



mask2 = as.matrix(read.table("mascara.txt"))
mask2 = array(mask2, dim = c(56,76,28,4))

pp.obs = pp.obs[2:57,2:77,,]*mask2
pp.obs = pp.obs*mask2

lats.n34 = seq(which(lat == -4.5), which(lat == 5.5))
lons.n34 = seq(which(lon == 190.5), which(lon == 240.5))

area = w.mask
area[lons.n34, lats.n34] = 2

aux.prom = apply(sst.ci[lons.n34, lats.n34,,], c(3,4), mean, na.rm = T)

j = 3
for(j in 1:4){
  aux.corr = corr(mod = aux.prom[,j], obs = pp.obs[,,,j], lon = 56, lat = 76, cf = 0.95)
  
  aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
  aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
  auxa = array(area, dim = c(dim(area), 1))
  
  mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
             , titulo = paste("Correlación Precipitación CMAP con SST Niño3.4 - " , estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "SA"
             , r = 1, width = 20, height = 20, salida =  "/salidas/corr/", nombre.fig = paste("pp.cmap.corr.SA_", estaciones[j], sep = ""), na.fill = -1000
             , sig = T, variable.sig = aux2, color.vsig = "black", alpha.vsig = 0.3, altura.topo = 1500, type.sig = "point", size.point = 1)
  
}




##### Con pp del ensamble ####
#### SST ####
aux = nc_open("X140.172.38.222.142.12.42.20.nc")
sst = ncvar_get(aux, "sst")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
nc_close(aux)
# rotando el array
sst = sst[,ncol(sst):1,]

#### mascara ####
aux = nc_open("X190.191.246.159.142.12.48.11.nc")
w.mask = ncvar_get(aux, "mask")
nc_close(aux)
# rotando..
w.mask = w.mask[,ncol(w.mask):1] 

#### meses de ci ####
sst.ci = array(data = NA, dim = c(360, 180, 29, 4))
ci = c(2, 5, 8, 11)
for(i in 1:4){
  for(j in 0:28)
    sst.ci[,,j + 1, i] = sst[,,ci[i]+12*j]*w.mask
}


area = w.mask
area[lons.n34, lats.n34] = 2
  
aux.prom = apply(sst.ci[lons.n34, lats.n34,,], c(3,4), mean, na.rm = T)

mask = array(as.matrix(read.table("mascara.txt")), dim = c(dim(pp.ens)))

pp.ens = pp.ens*mask
  
for(j in 1:4){
    
  aux.corr = corr(mod = aux.prom[,j], obs = pp.ens[,,,j], lon = 56, lat = 76, cf = 0.95)
    
    # prueba mapa_topo3
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
               , titulo = paste("Correlación Precipitación ensamble con SST Niño3.4 -  ", estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "SA"
               , r = 1, width = 20, height = 20, salida =  "/salidas/corr/", nombre.fig = paste("pp.corr.ens.SA_",estaciones[j], sep = ""), na.fill = -1000
               , sig = T, variable.sig = aux2, color.vsig = "black", alpha.vsig = 0.3, altura.topo = 1500, type.sig = "point", size.point = 1 )
  }


### contra pp de cada modelo (por ahora con gfdl-cm2p1)

pp = aux3[,,,,2]*mask

for(j in 1:4){
  
  aux.corr = corr(mod = aux.prom[,j], obs = pp[,,,j], lon = 56, lat = 76, cf = 0.95)
  
  # prueba mapa_topo3
  
  aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
  aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
  auxa = array(area, dim = c(dim(area), 1))
  
  mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
             , titulo = paste("Correlación Precipitación GFDL-CM2p1 con SST Niño3.4 - ", estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "SA"
             , r = 1, width = 20, height = 20, salida =  "/salidas/corr/", nombre.fig = paste("pp.corr.CM2p1.SA_",estaciones[j], sep = ""), na.fill = -1000
             , sig = T, variable.sig = aux2, color.vsig = "black", alpha.vsig = 0.3, altura.topo = 1500, type.sig = "point", size.point = 1 )
}




