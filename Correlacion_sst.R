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

lons = list()
lons[[1]] =  seq(which(lon2 == 291), which(lon2 == 304), by = 1); lons[[2]] = seq(which(lon2 == 301), which(lon2 == 316), by = 1)
lons[[3]] = seq(which(lon2 == 313), which(lon2 == 326), by = 1); lons[[4]] = seq(which(lon2 == 308), which(lon2 == 321), by = 1)
lons[[5]] = seq(which(lon2 == 296), which(lon2 == 309), by = 1); lons[[6]] = seq(which(lon2 == 295), which(lon2 == 325), by = 1)
lons[[7]] = seq(which(lon2 == 305), which(lon2 == 330), by = 1); lons[[8]] = seq(which(lon2 == 305), which(lon2 == 320), by = 1)

lat = rev(lat)
lats.area = list() # un poco distintas debido al grillado del mapa. 1x1 pero 1.5 2.5 3.5
lats.area[[1]] =  seq(which(lat == -13.5), which(lat == 2.5), by = 1); lats.area[[2]] = seq(which(lat == -16.5), which(lat == 4.5), by = 1)
lats.area[[3]] = seq(which(lat == -16.5), which(lat == 2.5), by = 1); lats.area[[4]] = seq(which(lat == -26.5), which(lat == -17.5), by = 1)
lats.area[[5]] = seq(which(lat == -39.5), which(lat == -24.5), by = 1); lats.area[[6]] = seq(which(lat == -7.5), which(lat == 5.5), by = 1)
lats.area[[7]] = seq(which(lat == -27.5), which(lat == -15.5), by = 1); lats.area[[8]] = seq(which(lat == -15.5), which(lat == 2.5), by = 1)

lons.area = list()
lons.area[[1]] =  seq(which(lon == 291.5), which(lon == 304.5), by = 1); lons.area[[2]] = seq(which(lon == 301.5), which(lon == 316.5), by = 1)
lons.area[[3]] = seq(which(lon == 313.5), which(lon == 326.5), by = 1); lons.area[[4]] = seq(which(lon == 308.5), which(lon == 321.5), by = 1)
lons.area[[5]] = seq(which(lon == 296.5), which(lon == 309.5), by = 1); lons.area[[6]] = seq(which(lon == 295.5), which(lon == 325.5), by = 1)
lons.area[[7]] = seq(which(lon == 300.5), which(lon == 315.5), by = 1); lons.area[[8]] = seq(which(lon == 305.5), which(lon == 320.5), by = 1)

#### graficos ####

# temp
region = c("Amazonia", "South American Monsoon", "North-estern Brazil", "SACZ", "La Plata Basin", "Norte Amazonia", "South Brazil")
region.fig = c("Am", "SAM", "NeB", "SACZ", "LPB", "NA", "SB")
estaciones = c("MAM", "JJA", "SON", "DJF")

for(i in 1:7){
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
for(i in 1:7){
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









# nueva zona NeB para PP en SON, SSgamma es significativo en casi todo el cont.
# en esta region tiene el maximo del ensamble y se mantiene en todos los miembros menos uno.
area = w.mask
area[lons.area[[8]], lats.area[[8]]] = 2
for(m in 1:8){
  aux.prom = apply(aux3[lons[[8]], lats[[8]],,,m], c(3,4), mean, na.rm = T)
  #for(j in 1:4){
    j = 3
    aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux.sig = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
               , titulo = paste("Correlación Precipitación con SST - ", nombres[m], "  ", estaciones[j], sep = ""), label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
               , r = 1, width = 30, height = 15, salida =  "/salidas/corr/", nombre.fig = paste("pp.corr2_NeB2_", nombres[m],"_", estaciones[j], sep = ""), na.fill = 0
               , sig = T, variable.sig = aux.sig, color.vsig = "white", alpha.vsig = 1, fill.mapa = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
    
  #}
}













