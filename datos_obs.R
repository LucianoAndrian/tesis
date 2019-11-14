rm(list=ls()) 
setwd("/home/auri/Facultad/")
library(ncdf4)
path<-"/home/auri/Descargas/"

tref = nc_open(paste(path, "tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
names(tref$var)
temp = ncvar_get(tref, "tref")

# la serie desde  mayo del '82 hasta agosto 2013
# faltan feb '86, dic '91, sep '97, jul 2003 y may 2009 cada vez que los dias alcanzan el 30 salta un mes. ver ncview
# los dias se mueven 1, 1, 2, 2, 3, 3. arranca con el mayo '82 con 11
# saltea meses en los tiempos 45, 114, 182, 251 y 320
lat = ncvar_get(tref, "Y")
lon = ncvar_get(tref, "X")

# tomo desde Dec '82 hasta NOV '2012 
temp = temp[,,8:362] # cambian los tiempos que se salta.


temp_m = array(NA, dim = c(360, 181, 30, 12)) # 3 dim = anios, 4dim = meses

for(j in 1:12){
  for (i in 0:29){
    if(j+12*i==45-6){
      temp_m[,,1+i,j] = NA
    } else if(j == 1 & i == 114)
    temp_m[,,1+i,j] = temp[, ,j+12*i]
  }
}