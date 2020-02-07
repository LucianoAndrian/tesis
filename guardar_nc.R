prueba = temp_estaciones[,,1,1]
lon = read.table("lon2.txt")[,1]
lat = read.table("lat2.txt")[,1]

londim = ncdim_def("lon", "grados_este", as.double(lon))
latdim = ncdim_def("lat", "grados_norte", as.double(lat))
tempdim = ncdim_def("temp", "°C", as.double(prueba))

fillvalue = 1e32
dlname = "temperatura"
temp_def = ncvar_def("temp", "°C", list(londim, latdim), fillvalue, dlname, prec="single")

ncfname = "prueba.nc"
ncout = nc_create(ncfname, list(temp_def), force_v4=T)

ncvar_put(ncout, temp_def, prueba)

ncatt_put(ncout, "lon", "lat", "temp")

nc_close(ncout)



tref = nc_open("prueba.nc")
names(tref$var)
temp2 = ncvar_get(tref, "temp")
