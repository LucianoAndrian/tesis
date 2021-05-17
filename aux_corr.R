##### corr sst ######

# Correlacion SST
library(ncdf4)

library(gridExtra)

#---------------------------------------------------------------#
g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}
#---------------------------------------------------------------#

source("funciones.R")
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

#### SST ####
aux = nc_open("ncfiles/X140.172.38.222.142.12.42.20.nc")
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
aux2 = ncvar_get(aux, "precip")[,,27:386] #363
nc_close(aux)


pp3_int = array(NA, dim = c(58, 78, 360)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:360){
  
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



####### CORR. entre una region del contienente y toda la SST ####

lats = list()
lats[[1]] =  seq(which(lat2 == -10), which(lat2 == 5), by = 1); lats[[2]] = seq(which(lat2 == -30), which(lat2 == -10), by = 1)
lats[[3]] = seq(which(lat2 == -20), which(lat2 == -5), by = 1); lats[[4]] = seq(which(lat2 == -20), which(lat2 == -10), by = 1)
lats[[5]] = seq(which(lat2 == -5), which(lat2 == 0), by = 1)

lons = list()
lons[[1]] =  seq(which(lon2 == 295), which(lon2 == 310), by = 1); lons[[2]] = seq(which(lon2 == 305), which(lon2 == 320), by = 1)
lons[[3]] = seq(which(lon2 == 310), which(lon2 == 320), by = 1); lons[[4]] = seq(which(lon2 == 310), which(lon2 == 320), by = 1)
lons[[5]] = seq(which(lon2 == 285), which(lon2 == 295), by = 1)

lat = rev(lat)
lats.area = list()
lats.area[[1]] =  seq(which(lat == -10.5), which(lat == 5.5), by = 1); lats.area[[2]] = seq(which(lat == -30.5), which(lat == -10.5), by = 1)
lats.area[[3]] = seq(which(lat == -20.5), which(lat == -5.5), by = 1); lats.area[[4]] = seq(which(lat == -20.5), which(lat == -10.5), by = 1)
lats.area[[5]] = seq(which(lat == -5.5), which(lat == 0.5), by = 1)

lons.area = list()
lons.area[[1]] =  seq(which(lon == 295.5), which(lon == 310.5), by = 1); lons.area[[2]] = seq(which(lon == 305.5), which(lon == 320.5), by = 1)
lons.area[[3]] = seq(which(lon == 300.5), which(lon == 320.5), by = 1); lons.area[[4]] = seq(which(lon == 310.5), which(lon == 320.5), by = 1)
lons.area[[5]] = seq(which(lon == 285.5), which(lon == 295.5), by = 1)
#### graficos ####

# temp
region = c("Area-CanCM4i-T", "Area-CFSv2-T", "Area-GEM-NEMO-PP", "Area-CM2p1-PP", "Area-CanCM4i-PP")
region.fig = c("CM4i-T", "CFSv2-T", "G-N-PP", "CM2p1-PP", "CM4i-PP")
estaciones = c("MAM", "JJA", "SON", "DJF")

nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO") 


seasons = list()
seasons[[1]] = c(1,3)
seasons[[2]] = c(NA, NA, 2,1,3) # **

wo.mod = list()
wo.mod[[1]] = c(7,6)
wo.mod[[2]] = c(NA, NA, 8,2,7) # **

# ** cada uno depende del area:
area.mod = list()
area.mod[[1]] = c(1,2)
area.mod[[2]] = c(3,4,5)


var.titulo = c("Temperatura", "Precipitación")
var.nombre = c("T", "PP")

for(v in 1:2){
  
  for(j in area.mod[[v]]){
    
    area = w.mask
    area[lons.area[[j]], lats.area[[j]]] = 2
    
    s = seasons[[v]][j]
    
    
    #### MME ###
    aux.prom = apply(ens[lons[[j]], lats[[j]],,,v], c(3,4), mean, na.rm = T)
    
    aux.corr = corr(mod = aux.prom[,s], obs = sst.ci[,,,s], lon = 360, lat = 180, cf = 0.95)
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    
    mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90)
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1
               , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .1)
               , titulo = paste("Correlación ", var.titulo[v] ," en MME ", region[j], " con SST - ", estaciones[s], sep = "")
               , label.escala = "", mapa = "mundo"
               , r = 1, contour.fill = T, na.fill = 0, fill.mapa = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red"
               , colorbar.pos = "bottom", cb.h.w = 40, cb.h.h = 1
               , nombre.fig = paste("corr_", region.fig[j],"_", var.nombre[v], "_", estaciones[s], sep = "")
               , salida = "/salidas/F.Finales/", save = T
               , cb.size = 20, letter.size = 14, width = 25)
    
    
    
    #### MODELOS ###
    
    m = wo.mod[[v]][j]
    
    aux.prom = apply(vars[lons[[j]], lats[[j]],,,m,v], c(3,4), mean, na.rm = T)
    
    aux.corr = corr(mod = aux.prom[,s], obs = sst.ci[,,,s], lon = 360, lat = 180, cf = 0.95)
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    
    mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90)
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1
               , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .1)
               , titulo = paste("Correlación ", var.titulo[v]," ", region[j], " con SST - ", estaciones[s], sep = "")
               , label.escala = "", mapa = "mundo"
               , r = 1, contour.fill = T, na.fill = 0, fill.mapa = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red"
               , colorbar.pos = "bottom", cb.h.w = 40, cb.h.h = 1
               , nombre.fig = paste("corr_MOD_", region.fig[j],"_", var.nombre[v], "_", estaciones[s], sep = "")
               , salida = "/salidas/F.Finales/", save = T
               , cb.size = 20, letter.size = 14, width = 25)
    
    
    #### OBSERVADO ##
    
    aux.prom = apply(obs[lons[[j]], lats[[j]],,,v], c(3,4), mean, na.rm = T)
    
    aux.corr = corr(mod = aux.prom[,s], obs = sst.ci[,,,s], lon = 360, lat = 180, cf = 0.95)
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
    
    mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90)
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1
               , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .1)
               , titulo = paste("Correlación", var.titulo[v], " OBS", region[j], " con SST - ", estaciones[s], sep = "")
               , label.escala = "", mapa = "mundo"
               , r = 1, contour.fill = T, na.fill = 0, fill.mapa = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red"
               , colorbar.pos = "bottom", cb.h.w = 40, cb.h.h = 1
               , nombre.fig = paste("corr_OBS_", region.fig[j],"_", estaciones[s], sep = "")
               , salida = "/salidas/F.Finales/", save = T
               , cb.size = 20, letter.size = 14, width = 25)
  
    
  }  
}





# corr inversa.
# Solo para casos 1 2 y 3

mask2 = as.matrix(read.table("mascara.txt"))
mask2 = array(mask2, dim = c(56,76))


lats.area.inv = lons.area.inv = list()

lats.area.inv[[1]] =  seq(which(lat == 10.5), which(lat == 20.5), by = 1); lats.area.inv[[2]] =  seq(which(lat == -10.5), which(lat == 10.5), by = 1)
lats.area.inv[[3]] =  seq(which(lat == 10.5), which(lat == 30.5), by = 1)

lons.area.inv[[1]] =  seq(which(lon == 300.5), which(lon == 330.5), by = 1); lons.area.inv[[2]] =  seq(which(lon == 180.5), which(lon == 240.5), by = 1)
lons.area.inv[[3]] =  seq(which(lon == 300.5), which(lon == 330.5), by = 1)


region = c("AT-CanCM4i-T", "PC-CFSv2-T", "AT-GEM-NEMO-PP")
region.fig = c("CM4i-T", "CFSv2-T", "G-N-PP")


# seasons = list()
# seasons[[1]] = c(1,3)
# seasons[[2]] = c(NA, NA, 2,1,3) # **
# 
# wo.mod = list()
# wo.mod[[1]] = c(7,6)
# wo.mod[[2]] = c(NA, NA, 8,2,7) # **
# 
# # ** cada uno depende del area:
# area.mod = list()
# area.mod[[1]] = c(1,2)
# area.mod[[2]] = c(3,4,5)


lats.n34 = seq(which(lat == -4.5), which(lat == 5.5))
lons.n34 = seq(which(lon == 190.5), which(lon == 240.5))


for(v in 1:2){
  #corta el for cuando falla, por la cantidad de lons.inv
  for(j in area.mod[[v]]){
    
    s = seasons[[v]][j]
    
    
    #### MME ###
    
    aux.prom = apply(sst.ci[lons.area.inv[[j]], lats.area.inv[[j]],,], c(3,4), mean, na.rm = T)
    
    aux.cor = corr(mod = aux.prom[,s], obs = ens[,,,s,v], lon = 56, lat = 76, cf = .95)
    aux = array(aux.cor[,,1]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    aux2 = array(aux.cor[,,2]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    

    
    g1 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
               , sig = T, variable.sig = aux2,  color.vsig = "black"
               , alpha.vsig = 0.4, altura.topo = 1500, type.sig = "point", size.point = 0.2
               , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .2)
               , titulo = paste("Corr. ", var.nombre[v], " MME", " - ", region[j], " - ", estaciones[s], sep = "")
               , label.escala = "", mapa = "SA"
               , r = 1, contour.fill = T, na.fill = -10000
               , cb.v.w = 1, cb.v.h = 25, save = F
               , cb.size = 20, letter.size = 14, width = 25, mostrar = T)
    
    ### MOD ### 
    
    m = wo.mod[[v]][j]
    
    aux.cor = corr(mod = aux.prom[,s], obs = vars[,,,s,m,v], lon = 56, lat = 76, cf = .95)
    aux = array(aux.cor[,,1]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    aux2 = array(aux.cor[,,2]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    
    
    g2 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , sig = T, variable.sig = aux2,  color.vsig = "black"
                    , alpha.vsig = 0.4, altura.topo = 1500, type.sig = "point", size.point = 0.2
                    , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .2)
                    , titulo = paste("Corr. ", var.nombre[v], " - ", region[j], " - ", estaciones[s], sep = "")
                    , label.escala = "", mapa = "SA"
                    , r = 1, contour.fill = T, na.fill = -10000
                    , cb.v.w = 1, cb.v.h = 30, save = F
                    , cb.size = 20, letter.size = 14, width = 25, mostrar = T)
    
    
    ### OBS ###

    aux.cor = corr(mod = aux.prom[,s], obs = obs[,,,s,v], lon = 56, lat = 76, cf = .95)
    aux = array(aux.cor[,,1]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    aux2 = array(aux.cor[,,2]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    
    
    g3 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , sig = T, variable.sig = aux2,  color.vsig = "black"
                    , alpha.vsig = 0.4, altura.topo = 1500, type.sig = "point", size.point = 0.2
                    , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .2)
                    , titulo = paste("Correlación ", var.nombre[v], " OBS", " - ", region[j], " - ", estaciones[s], sep = "")
                    , label.escala = "", mapa = "SA"
                    , r = 1, contour.fill = T, na.fill = -10000
                    , cb.v.w = 1, cb.v.h = 25, save = F
                    , cb.size = 20, letter.size = 14, width = 25, mostrar = T)
    
    
    
    
    
    colorbar = g_legend(g3)
    gp1 = g1 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp2 = g2 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp3 = g3 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    
    
    lay <- rbind(c(1,1))
    
    gpls <- lapply(list(gp1,gp2,gp3), ggplotGrob)
    
    
    p1 = grid.arrange(gpls[[1]],            
                      layout_matrix = lay
                      , bottom = textGrob("EMM", x = .5 
                                          ,rot = 0, gp=gpar(fontsize=16,font=8))) 
    
    p2 = grid.arrange(gpls[[2]],
                      layout_matrix = lay
                      , bottom = textGrob("MODELO", x = .5 
                                          ,rot = 0, gp=gpar(fontsize=16,font=8))) 
    
    p3 =  grid.arrange(gpls[[3]],     
                       layout_matrix = lay
                       , bottom = textGrob("OBSERVADO", x = .5 
                                           ,rot = 0, gp=gpar(fontsize=16,font=8))) 
    
    lay <- rbind(c(1,1,1,2,2,2,3,3,3,4),c(1,1,1,2,2,2,3,3,3,4))
    
    nombre_fig = paste(getwd(),"/salidas/F.Finales/","corr_inv-", region.fig[j], ".jpg", sep = "")
    
    ggsave(nombre_fig, plot = grid.arrange(p1,p2,p3, layout_matrix = lay, colorbar) ,width = 40, height = 15 ,units = "cm")


  }
}  



