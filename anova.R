# ANOVA
rm(list=ls())
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


#londim = ncdim_def("lon", "grados_este", as.double(lon2))
#latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
#timedim = ncdim_def("years", "Years", as.double(anios))
#mesesdim = ncdim_def("meses", "meses", as.double(1:3))
#rdim = ncdim_def("r","miembros", as.double(1:28))
#mdim = ncdim_def("m", "modelos", as.double(1:8))
#tempdim = ncdim_def("temp", "°C", as.double(temp_seasons))
#seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))


#fillvalue = NA
#dlname = "temperatura"
#temp_def = ncvar_def("temp", "Kelvin", list(londim, latdim, rdim, timedim, seasondim, mdim ), fillvalue, dlname, prec="single")

#ncfname = paste("pre_anova-temp", ".nc", sep = "")

#ncout = nc_create(ncfname, list(temp_def), force_v4=T)

#ncvar_put(ncout, temp_def, temp_seasons)

#ncatt_put(ncout, "lon", "lat", "meses", "r", "anios", "season") #esto nse que hace pero no anda

#nc_close(ncout) #verificar donde guarda los nc


#################################################################################################################################
nc2 = nc_open("/home/luciano.andrian/tesis/ncfiles/pre_anova-temp.nc")
temp_seasons2 = ncvar_get(nc2, "temp") 
nc_close(nc2)

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

TSS = SSa + SSb + SSg + SSe

r = 14 #promedio???
f1 = SSb/SSe*((length(anios)*8*(r-1))/(8-1))   # uno para cada modelo?

aux_f2 = (8-1)/(29*8+(14-1))
f2 = (SSb - aux_f2*SSe)/(TSS)  # da muuuyy chico e-6
