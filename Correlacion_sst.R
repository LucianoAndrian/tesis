# Correlacion SST
library(ncdf4)
source("funciones.R")

aux = nc_open("X140.172.38.222.142.12.42.20.nc")
sst = ncvar_get(aux, "sst")
nc_close(aux)
# rotando el array
sst = sst[,ncol(sst):1,]


aux = nc_open("X190.191.246.159.142.12.48.11.nc")
w.mask = ncvar_get(aux, "mask")
nc_close(aux)
# rotando..
w.mask = w.mask[,ncol(w.mask):1] 


sst.ci = array(data = NA, dim = c(360, 180, 29, 4))
ci = c(2, 5, 8, 11)
for(i in 1:4){
  for(j in 0:28)
    sst.ci[,,j + 1, i] = sst[,,ci[i]+12*j]*w.mask
}


# Modelos
aux = list()
nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2") # ACA ESTAN TODOS. INCLUYENDO CM2P1
for(i in 1:8){
  aux[[i]] = mean_sd(nombres[i])
}

aux2 = array(data = NA, dim = c(56,76,29,4,8))
aux3 = array(data = NA, dim = c(56,76,29,4,8))
for(i in 1:8){
  aux2[,,,,i] = aux[[i]][[5]]
  aux3[,,,,i] = aux[[i]][[6]]
}

# ensambles
t.ens = apply(aux2, c(1,2,3,4), mean, na.rm = T)
pp.ens = apply(aux3, c(1,2,3,4), mean, na.rm = T)
####
# seleccion des cajas
####
# prueba correlacion
# correlacion, usando funcion corr. (la de labo, modificada para simualacion de clima...)
# la funcion ya testea usando p.value. devuelve campo de correlacion y mascara de significancia

t.prom = apply(pp.ens, c(3,4), mean, na.rm = T)

prueba = corr(mod = t.prom[,4], obs = sst.ci[,,,4], lon = 360, lat = 180, cf = 0.95)
# prueba mapa_topo3
aux = prueba[,,1]
aux = array(aux, dim = c(dim(aux), 1))
aux2 = array(prueba[,,2],c(dim(prueba[,,1]),1))
mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", escala = seq(-1,1, by = 0.2)
           , titulo = "probando corr", label.escala = "", x.label = "Longitud", y.label = "Latitud", mapa = "mundo"
           , r = 1, width = 35, salida =  "/salidas/", nombre.fig = "prueba", na.fill = 0
           , sig = T, variable2 = aux2, color.v2 = "black", alpha.v2 =0.2, fill.mapa = T)


























