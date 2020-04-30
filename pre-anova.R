# ANOVA
# seleccion y orden de los nc en un solo archivo
library(ncdf4)

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")

r = c(10, 10, 12, 12, 4, 28, 10, 20)

anios = seq(from = 1982, to = 2010, by = 1)

temp_mods = array(NA, dim = c(length(lon2), length(lat2), 3, 28, length(anios), 4, 8))

for(i in 1:8){
  nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/", nombres[i], "-temp.nc", sep = ""))
  temp = ncvar_get(nc, "temp")
  temp_mods[,,,1:r[i],,,i] = temp
  nc_close(nc)
}

# promedio de las seasons

temp_seasons = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
for(i in 1:8){
  temp_seasons[,,,,,i] = apply(temp_mods[,,,,,,i],c(1,2,4,5,6),mean, na.rm = T) 
}

# guardado del array en formato nc

londim = ncdim_def("lon", "grados_este", as.double(lon2))
latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
timedim = ncdim_def("years", "Years", as.double(anios))
mesesdim = ncdim_def("meses", "meses", as.double(1:3))
rdim = ncdim_def("r","miembros", as.double(1:28))
mdim = ncdim_def("m", "modelos", as.double(1:8))
tempdim = ncdim_def("temp", "°C", as.double(temp_seasons))
seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))


fillvalue = NA
dlname = "temperatura"
temp_def = ncvar_def("temp", "Kelvin", list(londim, latdim, rdim, timedim, seasondim, mdim ), fillvalue, dlname, prec="single")

ncfname = paste("pre_anova-temp", ".nc", sep = "")

ncout = nc_create(ncfname, list(temp_def), force_v4=T)

ncvar_put(ncout, temp_def, temp_seasons)

nc_close(ncout) #verificar donde guarda los nc

###### PP ######

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")

r = c(10, 10, 12, 12, 4, 28, 10, 20)

anios = seq(from = 1982, to = 2010, by = 1)

pp_mods = array(NA, dim = c(length(lon2), length(lat2), 3, 28, length(anios), 4, 8))

for(i in 1:8){
  nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/", nombres[i], "-pp.nc", sep = ""))
  pp = ncvar_get(nc, "pp")
  pp_mods[,,,1:r[i],,,i] = pp
  nc_close(nc)
}

# promedio de las seasons

pp_seasons = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
for(i in 1:8){
  pp_seasons[,,,,,i] = apply(pp_mods[,,,,,,i],c(1,2,4,5,6),mean, na.rm = T) 
}

# guardado del array en formato nc

londim = ncdim_def("lon", "grados_este", as.double(lon2))
latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
timedim = ncdim_def("years", "Years", as.double(anios))
mesesdim = ncdim_def("meses", "meses", as.double(1:3))
rdim = ncdim_def("r","miembros", as.double(1:28))
mdim = ncdim_def("m", "modelos", as.double(1:8))
tempdim = ncdim_def("pp", "mm x month", as.double(pp_seasons))
seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))


fillvalue = NA
dlname = "precipitacion"
temp_def = ncvar_def("pp", "mm", list(londim, latdim, rdim, timedim, seasondim, mdim ), fillvalue, dlname, prec="single")

ncfname = paste("pre_anova-pp", ".nc", sep = "")

ncout = nc_create(ncfname, list(temp_def), force_v4=T)

ncvar_put(ncout, temp_def, pp_seasons)

nc_close(ncout) #verificar donde guarda los nc
