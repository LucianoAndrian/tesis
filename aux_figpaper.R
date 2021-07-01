source("funciones.R")
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}


# aux, fig_paper

# Correlacion SST
library(ncdf4)

#### SST ####
aux = nc_open("ncfiles/X190.191.231.100.152.8.30.8.nc")
sst = ncvar_get(aux, "sst")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
nc_close(aux)
# rotando el array
sst = sst[,ncol(sst):1,]

#### mascara ####
aux = nc_open("ncfiles/X190.191.246.159.142.12.48.11.nc")
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
nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO") 
auxT = array(data = NA, dim = c(56,76,29,4,8)) 
auxPP = auxT 
for(i in 1:8){
  auxT[,,,,i] = mean_sd(nombres[i])[[5]]
  auxPP[,,,,i] = mean_sd(nombres[i])[[6]]
}

vars = array(NA, dim = c(dim(auxT),2)); vars[,,,,,1] = auxT; vars[,,,,,2] = auxPP

t.ens = apply(vars[,,,,,1], c(1,2,3,4), mean, na.rm = T)
pp.ens = apply(vars[,,,,,2], c(1,2,3,4), mean, na.rm = T)

ens = array(NA, dim = c(dim(t.ens),2))
ens[,,,,1] = t.ens; ens[,,,,2] = pp.ens


# Temp CPC
ruta = "/pikachu/datos/osman/nmme/monthly"

tref = nc_open(paste(ruta,"tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
names(tref$var)
temp = ncvar_get(tref, "tref")

lat = ncvar_get(tref, "Y")
lon = ncvar_get(tref, "X")
nc_close(tref)
temp = temp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:371] 

lon2 = lon[which(lon==275):which(lon==330)]
lat2 = lat[which(lat==-60):which(lat==15)]

temp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 29, 12)) 

for(j in 1:12){
  for (i in 0:28){
    temp_estaciones[,,1+i,j] = temp[ , , j+12*i]
  }
}

t.obs = array(NA, dim = c(length(lon2), length(lat2), 29, 4))
i=1
while(i<=4){
  t.obs[,,,i] = apply(temp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}


# PP cmap 
require(fields)
aux = nc_open("/home/luciano.andrian/tesis/ncfiles/X190.191.242.210.56.5.48.49.nc")

lon4 = ncvar_get(aux, "lon")
lat4 = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[,,27:374] #363
nc_close(aux)


pp3_int = array(NA, dim = c(58, 78, 348)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:348){
  
  mod = list(x = lon4, y = lat4, z = aux2[,,i])
  
  grid = list(x=seq(min(lon4), max(lon4), by = 1), y = seq(min(lat4), max(lat2)+1, by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp3_int[,,i] = pp_aux$z  
}


pp3_estaciones = array(NA, dim = c(56, 76, 29, 12))

for(j in 1:12){
  for (i in 0:28){
    pp3_estaciones[,,1+i,j] = pp3_int[1:56 , 1:76, j+12*i]
  }
}


pp.obs = array(NA, dim = c(56, 76, 29, 4))
i=1
while(i<=4){
  pp.obs[,,,i] = apply(pp3_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)*30 # esta en mm/day
  i = i + 1
}

obs = array(NA, dim = c(dim(t.obs),2))
obs[,,,,1] = t.obs; obs[,,,,2] = pp.obs

save(vars, file = "vars.RData")
save(sst.ci, file = "sst.ci.RData")
save(obs, file="obs.RData")
save(ens, file = "ens.RData")
#############################################################################333333
## bias

anios = seq(from = 1982, to = 2010, by = 1)
variable = c("temp", "pp")
modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO")
V.mean = array(data = NA, dim = c(length(lon2), length(lat2), 4, 8, 2))

ruta = "/home/luciano.andrian/tesis/ncfiles/"
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
    
    print(paste("fin j = ", j))
  }
}

save(V.mean, file="V.mean.RData")
#############################################################################333333
