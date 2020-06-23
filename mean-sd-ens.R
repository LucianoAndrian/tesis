
rm(list = ls())
library(ncdf4)
ruta =  "/home/luciano.andrian/tesis/ncfiles/"
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
anios = seq(from = 1982, to = 2010, by = 1)
variable = c("temp", "pp")
modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")
V.mean = array(data = NA, dim = c(length(lon2), length(lat2), 4, 8, 2))
V.sd1 = array(data = NA, dim = c(length(lon2), length(lat2), 4, 8, 2))


for(v in 1:2){
  print(v)
  for(j in 1:8){ 
  print(paste("inicio j = ", j))
    
    nc = nc_open(paste(ruta, modelos[j], "-", variable[v], ".nc",  sep = ""))
    
    var = ncvar_get(nc, variable[v])
    nc_close(nc)
    
    V1 =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
    for(i in 1:4){
      V1[,,i] = apply(var[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask  
    }
    
   V.mean[,,,j,v] = V1
    
    # SD1
    V2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
    for(i in 1:4){
      V2[,,,i] =  apply(var[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
    }
  
     
    sd = array(NA, dim = c(length(lon2), length(lat2),4)) 
    for(i in 1:4){
      sd[,,i] = apply(V2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
    }
    
    V.sd1[,,,j,v] = sd
    
    print(paste("fin j = ", j))
  }
}

v.mean = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.mean[,,,1] = apply(V.mean[,,,,1], c(1,2,3), mean, na.rm = T)
v.mean[,,,2] = apply(V.mean[,,,,2], c(1,2,3), mean, na.rm = T)

v.sd1 = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.sd1[,,,1] = apply(V.sd1[,,,,1], c(1,2,3), mean, na.rm = T)
v.sd1[,,,2] = apply(V.sd1[,,,,2], c(1,2,3), mean, na.rm = T)

v.sd2 = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.sd2[,,,1] = apply(V.mean[,,,,1], c(1,2,3), sd, na.rm = T)
v.sd2[,,,2] = apply(V.mean[,,,,2], c(1,2,3), sd, na.rm = T)

source("funciones.R")


color = c("YlOrRd", "PuBuGn")
escala = list(); escala[[1]] = seq(0, 1.5, by = 0.1); escala[[2]] = seq(0, 50, by = 5)  
variable = c("Temperatura", "Precipitación")
lab.escala = c("ºC", "mm")
nombre.fig = c("t", "pp")

for(v in 1:2){
  
  aux = v.sd1[,,,v]; aux2 = array(mask,c(dim(mask),4))
  
  mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = color[v], revert = F, escala = escala[[v]]
             , titulo = paste("Desvio Estandar del Ensamble  - ", variable[v], " ", sep = ""), label.escala = lab.escala[v], x.label = NULL, y.label = NULL, mapa = "SA"
             , width = 20, height = 20, salida =  "/salidas/ensemble/sd_mean/", nombre.fig = paste(nombre.fig[v], ".sd_", sep = ""), na.fill = 0
             , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, r = 4, estaciones = T, altura.topo = 1500)

}



# sd2
color = c("YlOrRd", "YlGnBu")
escala = list(); escala[[1]] = seq(0, 3, by = 0.25); escala[[2]] = seq(0, 150, by = 10)  
variable = c("Temperatura", "Precipitación")
lab.escala = c("ºC", "mm")
nombre.fig = c("t", "pp")

for(v in 1:2){
  
  aux = v.sd2[,,,v]; aux2 = array(mask,c(dim(mask),4))
  
  mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = color[v], revert = F, escala = escala[[v]]
             , titulo = paste("Desvio Estandar 2 del Ensamble  - ", variable[v], " ", sep = ""), label.escala = lab.escala[v], x.label = NULL, y.label = NULL, mapa = "SA"
             , width = 20, height = 20, salida =  "/salidas/ensemble/", nombre.fig = paste(nombre.fig[v], ".sd2_", sep = ""), na.fill = 0
             , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, r = 4, estaciones = T, altura.topo = 1500)
  
}





color = c("Spectral", "YlGnBu")
escala = list(); escala[[1]] = seq(0, 30, by = 2.5); escala[[2]] = seq(0, 250, by = 25)  
variable = c("Temperatura", "Precipitación")
lab.escala = c("ºC", "mm")
nombre.fig = c("t", "pp")
revert = c(T,F )
resta = c(273,0)
for(v in 1:2){
  
  aux = v.mean[,,,v]; aux2 = array(mask,c(dim(mask),4))
  
  mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = color[v], revert = revert[v], escala = escala[[v]]
             , titulo = paste("Media del Ensamble  - ", variable[v], " ", sep = ""), label.escala = lab.escala[v], x.label = NULL, y.label = NULL, mapa = "SA"
             , width = 20, height = 20, salida =  "/salidas/ensemble/", nombre.fig = paste(nombre.fig[v], ".mean_", sep = ""), na.fill = 0
             , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, r = 4, estaciones = T, altura.topo = 1500, resta = resta[v])
  
}








