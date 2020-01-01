# ANOVA

library(ncdf4)

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")

r = c(10, 28, 12, 12, 4, 28, 10, 10)

anios = seq(from = 1982, to = 2010, by = 1)

temp_mods = array(NA, dim = c(length(lon2), length(lat2), 3, 28, length(anios), 4, 8))

for(i in 1:8){
  nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/", nombres[i], "-temp.nc", sep = ""))
  temp = ncvar_get(nc, "temp")
  temp_mods[,,,1:r[i],,,i] = temp
  nc_close(nc)
}

temp_seasons = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
for(i in 1:8){
  temp_seasons[,,,,,i] = apply(temp_mods[,,,,,,i],c(1,2,4,5,6),mean, na.rm = T) 
}


temp_mean = array(NA, dim = c(length(lon2), length(lat2), 4))
temp_y = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4))
temp_m = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
temp_ym = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))

for(i in 1:4){
  temp_mean[,,i] = apply(temp_seasons[,,,,i,],c(1,2),mean, na.rm = T)         #ver
  temp_y[,,,i] = apply(temp_seasons[,,,,i,],c(1,2,4),mean, na.rm = T)
  temp_m[,,i,] = apply(temp_seasons[,,,,i,], c(1,2,5),mean, na.rm = T) 
  temp_ym[,,,i,] = apply(temp_seasons[,,,,i,],c(1,2,4,5),mean, na.rm = T)
}


aux = array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
for(i in 1:length(anios)){
    aux[,,i,] = (temp_y[,,i,]-temp_mean)**2
}

SSa = apply(aux, c(1,2,4), sum)

aux = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
for(i in 1:8){
  aux[,,,i] = (temp_m[,,,i]-temp_mean)**2
}

SSb = apply(aux, c(1,2,3), sum)

aux = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
for(i in 1:28){
  aux[,, i,,,] = (temp_seasons[,,i,,,]-temp_ym)**2
}

SSe = apply(aux, c(1,2,5), sum, na.rm = T)

#SSg  ( ver dimensiones. )

aux = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
for(i in 1:8){
  aux[,,,,i] = temp_ym[,,,,i] - temp_y
}

aux2 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
for(i in 1:length(anios)){
  aux2[,,i,,] = aux[,,i,,] - temp_m
}

aux3 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
for(i in 1:length(anios)){
  for(j in 1:8){
    aux3[,,i,,j] = aux2[,,i,,j] - temp_mean
  }
}

SSg = apply(aux3, c(1,2,4), sum, na.rm = T)

### ?? ###
# estimadores insesgados
f1 = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
for( i in 1:8){
  f1[,,,i] = SSb/SSe*((length(anios)*8*(r[i]-1))/(8-1))   # uno para cada modelo
}


f1 = SSb/SSe*((length(anios)*8*(r[i]-1))/(8-1))   # uno para cada modelo


