
source("funciones.R")

library(ggplot2)
library(gridExtra)
library(ncdf4)

#---------------------------------------------------------------#
g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}
######---------------------- MEDIAS Y DESVIOS ----------------------######

# OBSERVADO

# Temp
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

temp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12)) 

for(j in 1:12){
  for (i in 0:29){
    temp_estaciones[,,1+i,j] = temp[ , , j+12*i]
  }
}

# Estaciones
estaciones_p_a_t = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
i=1
while(i<=4){
  estaciones_p_a_t[,,,i] = apply(temp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}

estaciones_prom_t = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom_t[,,i] = apply(estaciones_p_a_t[,,,i], c(1,2), mean)
}



titulos = list()
titulos[[1]] = "                  MAM                "
titulos[[2]] = "                  JJA                "
titulos[[3]] = "                  SON                "
titulos[[4]] = "                  DJF                "

T_mean_obs = list()
for(season in 1:4){
  
  T_mean_obs[[season]] = mapa_topo3(variable = estaciones_prom_t, colorbar = "Spectral", revert = T, escala = seq(0, 35, by = 2.5)
                 , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), resta = 273, niveles = 11
                 , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                 , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                 , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                 , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                 , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
}


mask = estaciones_prom_t[,,1]  
mask[which(!is.na(mask))]=1

# sd
standar_d_t = array(NA, dim = c(length(lon2), length(lat2), 4))
for( i in 1:4 ){
  standar_d_t[,,i] = apply(estaciones_p_a_t[,,,i], c(1,2), sd)
}


T_SD_obs = list()
for(season in 1:4){
  
  T_SD_obs[[season]] = mapa_topo3(variable = standar_d_t, colorbar = "YlOrRd", revert = F,escala = seq(0, 1.5, by = 0.1)
                                    , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), niveles = 9
                                    , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                                    , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                    , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                                    , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                                    , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
}


# Precip

aux = nc_open("/home/luciano.andrian/tesis/X190.191.242.210.56.5.48.49.nc")

lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[,,27:386]
nc_close(aux)

lon4 = lon
lat4 = lat

pp3_int = array(NA, dim = c(56, 76, 360)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:360){
  
  mod = list(x = lon4, y = lat4, z = aux2[,,i])
  
  grid = list(x=seq(min(lon4), max(lon4)-2, by = 1), y = seq(min(lat4), max(lat2)-1, by = 1))
  
  pp_aux = fields::interp.surface.grid(obj = mod, grid.list = grid)
  
  pp3_int[,,i] = pp_aux$z 
}


pp3_estaciones = array(NA, dim = c(56, 76, 30, 12))

for(j in 1:12){
  for (i in 0:29){
    pp3_estaciones[,,1+i,j] = pp3_int[1:56 , 1:76, j+12*i]
  }
}


estaciones_p_a_pp3 = array(NA, dim = c(56, 76, 30, 4))
i=1
while(i<=4){
  estaciones_p_a_pp3[,,,i] = apply(pp3_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)*30 # esta en mm/day
  i = i + 1
}

estaciones_prom_pp3 = array(NA, dim = c(56, 76, 4))

for( i in 1:4){
  estaciones_prom_pp3[,,i] = apply(estaciones_p_a_pp3[,,,i], c(1,2), mean)*mask
}


PP_mean_obs = list()
for(season in 1:4){
  
PP_mean_obs[[season]] = mapa_topo3(variable = estaciones_prom_pp3, lon = lon2, lat = lat2, resta = 0, colorbar = "PuBuGn"
             , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), niveles = 9,escala = seq(0, 400, by = 50)
             , label.escala = "ºC", mapa = "SA", width = 20, height = 20
             , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
             , cajas = F, estacion = season, mostrar = T, save = F,  cb.v.w = 1
             , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
             , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
}




## sd
standar_d_pp3 = array(NA, dim = c(56, 76, 4))
for( i in 1:4 ){
  standar_d_pp3[,,i] = apply(estaciones_p_a_pp3[,,,i], c(1,2), sd)*mask
}


PP_SD_obs = list()
for(season in 1:4){
  
  PP_SD_obs[[season]] = mapa_topo3(variable = standar_d_pp3, lon = lon2, lat = lat2, resta = 0, colorbar = "PuBuGn"
                                     , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), niveles = 9,escala = seq(0, 70, by = 5)
                                     , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                                     , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                     , cajas = F, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                                     , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                                     , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
}


mean_OBS = list(); mean_OBS[[1]] = T_mean_obs; mean_OBS[[2]] = PP_mean_obs
SD_OBS = list(); SD_OBS[[1]] = T_SD_obs; SD_OBS[[2]] = PP_SD_obs

## modelos

ruta =  "/home/luciano.andrian/tesis/ncfiles/"
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
anios = seq(from = 1982, to = 2010, by = 1)
variable = c("temp", "pp")
modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO")
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
escala = list(); escala[[1]] = seq(0, 1.5, by = 0.1); escala[[2]] = seq(0, 70, by = 5)  
variable = c("Temperatura", "Precipitación")
lab.escala = c("ºC", "mm")



sd_mod = list()
mean_mod = list()
for(v in 1:2){
  
  for(season in 1:4){
    
    color = c("YlOrRd", "PuBuGn")
    escala = list(); escala[[1]] = seq(0, 1.5, by = 0.1); escala[[2]] = seq(0, 70, by = 5)  
    variable = c("Temperatura", "Precipitación")
    lab.escala = c("ºC", "mm")
    
    aux = v.sd1[,,,v]; aux2 = array(mask,c(dim(mask),4))

    sd_mod[[season]] = mapa_topo3(variable = aux, lon = lon2, lat = lat2, resta = 0, colorbar = color[v]
                                  , titulo =  paste(letters[season+4],".",titulos[[season]], sep = ""), niveles = 9, escala = escala[[v]]
                                  , label.escala = lab.escala[v], mapa = "SA", width = 20, height = 20
                                  , na.fill = 0, r = 4, estaciones = T, altura.topo = 1500
                                  , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1 #?¿?¿
                                  , cajas = F, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                                  , cb.v.h = 26, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                                  , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
    
    
    color = c("Spectral", "PuBuGn")
    escala = list(); escala[[1]] = seq(0, 35, by = 2.5); escala[[2]] = seq(0, 400, by = 50)  
    variable = c("Temperatura", "Precipitación")
    lab.escala = c("ºC", "mm")
    nombre.fig = c("t", "pp")
    revert = c(T,F )
    resta = c(273,0)
    niveles = c(11,9)
    
    aux = v.mean[,,,v]; aux2 = array(mask,c(dim(mask),4))
  
    mean_mod[[season]] = mapa_topo3(variable = aux, lon = lon2, lat = lat2, resta = resta[v], colorbar = color[v]
                                    , titulo =  paste(letters[season + 4], ".",titulos[[season]], sep = ""), niveles = niveles[v], escala = escala[[v]], revert = revert[v]
                                    , label.escala = lab.escala[v], mapa = "SA", width = 20, height = 20
                                    , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                    , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1 #?¿?¿
                                    , cajas = F, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                                    , cb.v.h = 26, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                                    , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
  }
    # GRID.ARRANGE. incluyendo lo obs
    
   
    colorbar1 <- g_legend(mean_mod[[1]])
    colorbar2 <- g_legend(sd_mod[[1]])
    
    # MEAN
    # panel 1 - obs
    gp1 = mean_OBS[[v]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp2 = mean_OBS[[v]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
    gp3 = mean_OBS[[v]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp4 = mean_OBS[[v]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    
    #panel 2 - emm
    gp5 = mean_mod[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp6 = mean_mod[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp7 = mean_mod[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp8 = mean_mod[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    
    # SD
    gp9 = SD_OBS[[v]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp10 = SD_OBS[[v]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
    gp11 = SD_OBS[[v]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp12 = SD_OBS[[v]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    
    #panel 2 - emm
    gp13 = sd_mod[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp14 = sd_mod[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp15 = sd_mod[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp16 = sd_mod[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    
    
    gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                        gp11, gp12, gp13, gp14, gp15, gp16), ggplotGrob )
    
    lay <- rbind(c(1,1,2,2,3,3,4,4),c(1,1,2,2,3,3,4,4))
    
    # de esta forma tan elegante no afecta los margenes  
    titulo_obs = c("                          CPC                       "
                   , "                          CMAP                       ") 
    
    
    p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],             
                      layout_matrix = lay
                      , left = textGrob(titulo_obs[v]
                                        ,rot = 90, gp=gpar(fontsize=16,font=8))
                      , top = textGrob(" ", x = 0 
                                       , gp=gpar(fontsize=16,font=8))) 
    
    p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                      layout_matrix = lay
                      , left = textGrob("                          EMM                       "
                                        ,rot = 90, gp=gpar(fontsize=16,font=8))
                      , top = textGrob(" ", x = 0 
                                       , gp=gpar(fontsize=16,font=8))) 
    
    
    
    p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                      layout_matrix = lay
                      , left = textGrob(titulo_obs[v]
                                        ,rot = 90, gp=gpar(fontsize=16,font=8))
                      , top = textGrob(" ", x = 0 
                                       , gp=gpar(fontsize=16,font=8))) 
    
    p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                      layout_matrix = lay
                      ,  left = textGrob("                          EMM                      "
                                         ,rot = 90, gp=gpar(fontsize=16,font=8))
                      , top = textGrob(" ", x = 0 
                                       , gp=gpar(fontsize=16,font=8))) 
    
    
    
    
    lay <- rbind(c(1,1,1,1,1,1,1,1,5),c(1,1,1,1,1,1,1,1,5),
                 c(2,2,2,2,2,2,2,2,5),c(2,2,2,2,2,2,2,2,5))
    if(v ==1){
      nombre = "T_"
    } else {
      nombre = "PP_"
    }
    
    
    nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, ".mean", ".jpg", sep = "")
    
    ggsave(nombre_fig,plot =grid.arrange(p1, p2, ncol = 2, layout_matrix = lay, colorbar1) ,width = 30, height = 17 ,units = "cm")
    
    
    nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, ".SD", ".jpg", sep = "")
    
    ggsave(nombre_fig,plot =grid.arrange(p3, p4, ncol = 2, layout_matrix = lay, colorbar2) ,width = 30, height = 17 ,units = "cm")
    
    
      
      
      

}


###### ------------------------- ANOVA ------------------------######
#-------------------------------------------------------------------#
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}

source("funciones.R")
letras = c(as.character("\u03b1"), as.character("\u03B2"), as.character("\u194"), as.character("\u03B5"))
titulos = list()
titulos[[1]] = "              SSα              "
titulos[[2]] = "              SSβ              "
titulos[[3]] = "              SSε              "
titulos[[4]] = "              SSƔ              "
#-------------------------------------------------------------------#

sin_m = function(m, season, v, titulo_num){
  #v = 1 o 3
  #v.sig = 2 o 4 
  
  if(is.na(m)){
   return(NA)
  } else {
    
  
  
  if(v == 1){
    v.sig = 2
  } else if(v == 3){
    v.sig = 4
  } else {
    print("ESTA MAAAAL")
    break
  }
  
  aux = list()
  aux[[1]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[6]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[6-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo =  paste(letters[1+titulo_num],".",titulos[[1]], sep = "") #que trucazo
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = F, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = F, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  aux[[2]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[7]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[7-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo = paste(letters[2+titulo_num],".",titulos[[2]], sep = "")
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = F, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = F, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux[[4]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[9]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[9-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo =  paste(letters[4+titulo_num],".",titulos[[3]], sep = "")
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = F, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = F, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux[[3]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[8-5]], colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                        , titulo = paste(letters[3+titulo_num],".",titulos[[4]], sep = "")
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = F, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F, cb.v.w = 0.7, cb.v.h = 13, cb.size = 7
                        , lats.size = 7,letter.size = 12, margen.zero = F, cajas = F, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  return(aux)
  }
}


# EMM
ss_temp = anova_fun(variable = "temp", ensemble_total = "si") 
sig_temp_emm = test_cos(ss_temp, ensemble_total = "si", nomodel_selec = "no", no_model = "no")

ss_pp = anova_fun(variable = "pp", ensemble_total = "si")
sig_pp_emm = test_cos(ss_pp, ensemble_total = "si", nomodel_selec = "no", no_model = "no")

EMM = list()
EMM[[1]] = ss_temp; EMM[[2]] = sig_temp_emm
EMM[[3]] = ss_pp; EMM[[4]] = sig_pp_emm


load("ss_T.RData")
sig_temp = list()
for(i in 1:8){
  sig_temp[[i]] = test_cos(ss_T[[i]], ensemble_total = "no", nomodel_selec = "no", no_model = i )
}

load("ss_PP.RData")
sig_pp = list()
for(i in 1:8){
  sig_pp[[i]] = test_cos(ss_PP[[i]], ensemble_total = "no", nomodel_selec = "no", no_model = i )
}

EMM_wo = list()
EMM_wo[[1]] = ss_T; EMM_wo[[2]] = sig_temp
EMM_wo[[3]] = ss_PP; EMM_wo[[4]] = sig_pp

colorbars = list()
colorbars[[1]] = "YlOrRd"; colorbars[[3]] = "PuBuGn"

colorbars_gamma = list()
colorbars_gamma[[1]] = "RdPu"; colorbars_gamma[[3]] = "BuPu"

seasons = c("MAM", "JJA", "SON", "DJF")

wo_mod = array(NA, dim = c(3,4,2))

wo_mod[,1,1] = c(6,7,NA)
wo_mod[,2,1] = c(2,NA,NA)
wo_mod[,3,1] = c(6,2,NA)
wo_mod[,4,1] = c(6,7,NA)

wo_mod[,1,2] = c(6,NA,NA)
wo_mod[,2,2] = c(6,2,NA)
wo_mod[,3,2] = c(6,2,8)
wo_mod[,4,2] = c(6,2,NA)


nombres2 = c("CCSM4", "CM2p1", "FLOR-A06", "FLOR-B01", "GEOS5", "CFSv2", "CM4i", "GEM-NEMO") 

for(v in c(1,3)){
  for(season in 1:4){
    
    
    # panel 1. EMM completo
    if(v == 1){
      v.sig = 2
    } else if(v == 3){
      v.sig = 4
    } else {
      print("ESTA MAAAAL")
      break
    }
   # "SSα"  "SSβ"   "SSƔ" "SSε"
    signal = mapa_topo3(variable = EMM[[v]][[6]]*mask_arr, variable.sig = EMM[[v.sig]][[6-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo = paste(letters[1],".",titulos[[1]], sep = "")
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F,  cb.v.w = 1
                        , cb.v.h = 17, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    bias = mapa_topo3(variable =  EMM[[v]][[7]]*mask_arr, variable.sig =  EMM[[v.sig]][[7-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                      , titulo = paste(letters[2],".",titulos[[2]], sep = "")
                      , label.escala = "", mapa = "SA", width = 20, height = 20
                      , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                      , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F
                      ,  cb.v.w = 1, cb.v.h = 30, cb.size = 14, lats.size = 7, letter.size = 12, color.vcont = "black"
                      , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    noise = mapa_topo3(variable =  EMM[[v]][[9]]*mask_arr, variable.sig =  EMM[[v.sig]][[9-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                       , titulo =  paste(letters[4],".",titulos[[3]], sep = "")
                       , label.escala = "", mapa = "SA", width = 20, height = 20
                       , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                       , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F
                       ,  cb.v.w = 1, cb.v.h = 30, cb.size = 14, lats.size = 7, letter.size = 12, color.vcont = "black"
                       , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    structural = mapa_topo3(variable = EMM[[v]][[8]]*mask_arr, variable.sig =  EMM[[v.sig]][[8-5]], colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                            , titulo = paste(letters[3],".",titulos[[4]], sep = "")
                            , label.escala = "", mapa = "SA", width = 20, height = 20
                            , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                            , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F, cb.v.w = 1
                            , cb.v.h = 17, cb.size = 10, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                            , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
    
    
    
    if(v == 1){
      v3 = 1
    } else {
      v3 = 2
    }
    
    
    no_mod1 = sin_m(wo_mod[1,season,v3], season,v, titulo_num = 4)
    
    no_mod2 = sin_m(wo_mod[2,season,v3], season,v, titulo_num = 8)
    
    no_mod3 = sin_m(wo_mod[3,season,v3], season, v, titulo_num = 12)
    
    

colorbar1 <- g_legend(signal)
colorbar2 <- g_legend(structural)
# panel 1 - emm
gp1 = signal + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp2 = bias + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp3 = structural + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp4 = noise + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))

#panel 2 - no_mod1
gp5 = no_mod1[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp6 = no_mod1[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp7 = no_mod1[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp8 = no_mod1[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))

if(!is.na(no_mod2[1])){
  
  #panel 3 - no_mod2
  gp9 = no_mod2[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp10 = no_mod2[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp11 = no_mod2[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp12 = no_mod2[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  if(!is.na(no_mod3[1])){
    
    
    #panel 4 - no_mod3
    gp13 = no_mod3[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp14 = no_mod3[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp15 = no_mod3[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    gp16 = no_mod3[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
    
    gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                        gp11, gp12, gp13, gp14, gp15, gp16), ggplotGrob )
    
    
    
    
  } else {
    
    gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                        gp11, gp12), ggplotGrob )
    
  }
  
  
  
  
} else {
  
  gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8), ggplotGrob )
  
}





lay <- rbind(c(1,1,2,2,3,3,4,4),c(1,1,2,2,3,3,4,4))

p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],             
                  layout_matrix = lay
                  , left = textGrob("                          EMM Completo                       " # de esta forma tan elegante no afecta los margenes
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8)))  


p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                  layout_matrix = lay
                  , left = textGrob(paste("                          EMM sin ", nombres2[wo_mod[1,season,v3]], "                       ")
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 

if(!is.na(no_mod2[1])){
  
  p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                    layout_matrix = lay
                    , left = textGrob(paste("                          EMM sin ", nombres2[wo_mod[2,season,v3]], "                       ")
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8)))

  
  
  
  if(!is.na(no_mod3[1])){
    
    p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                      layout_matrix = lay
                      , left = textGrob(paste("                          EMM sin ", nombres2[wo_mod[3,season,v3]], "                       ")
                                        ,rot = 90, gp=gpar(fontsize=16,font=8))
                      , top = textGrob(" ", x = 0 
                                       , gp=gpar(fontsize=16,font=8))) 
    
    
    lay <- rbind(c(1,1,1,1,1,1,1,1,5),c(1,1,1,1,1,1,1,1,5),
                 c(2,2,2,2,2,2,2,2,5),c(2,2,2,2,2,2,2,2,5), 
                 c(3,3,3,3,3,3,3,3,6),c(3,3,3,3,3,3,3,3,6), 
                 c(4,4,4,4,4,4,4,4,6),c(4,4,4,4,4,4,4,4,6))
    
    
    
  } else {lay <- rbind(c(1,1,1,1,1,1,1,1,4),c(1,1,1,1,1,1,1,1,4),
                       c(1,1,1,1,1,1,1,1,4),c(1,1,1,1,1,1,1,1,4),
                       c(2,2,2,2,2,2,2,2,6),c(2,2,2,2,2,2,2,2,4),
                       c(2,2,2,2,2,2,2,2,6),c(2,2,2,2,2,2,2,2,5), 
                       c(3,3,3,3,3,3,3,3,5),c(3,3,3,3,3,3,3,3,5),
                       c(3,3,3,3,3,3,3,3,5),c(3,3,3,3,3,3,3,3,5))
  }
  
  
  
} else {
  
  lay <- rbind(c(1,1,1,1,1,1,1,1,3),c(1,1,1,1,1,1,1,1,3),
               c(2,2,2,2,2,2,2,2,4),c(2,2,2,2,2,2,2,2,4))
               
}

if(v ==1){
  nombre = "T_"
} else {
  nombre = "PP_"
}


nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, seasons[season], ".anova", ".jpg", sep = "")
if(!is.na(no_mod2[1])){
  if(!is.na(no_mod3[2])){
    ggsave(nombre_fig,plot =grid.arrange(p1, p2,p3,p4, ncol = 2, layout_matrix = lay, colorbar1, colorbar2) ,width = 30, height = 35 ,units = "cm")
  } else {
    ggsave(nombre_fig,plot =grid.arrange(p1, p2, p3, ncol = 2, layout_matrix = lay, colorbar1, colorbar2) ,width = 30, height = 26.25 ,units = "cm")
  }
} else {
  ggsave(nombre_fig,plot =grid.arrange(p1, p2, ncol = 2, layout_matrix = lay, colorbar1, colorbar2) ,width = 30, height = 17.5 ,units = "cm")
}


  }
  
}






#######------------------- Predictibilidad --------------------------########
# mismos modelos (+ GEM-NEMO en una estacion para T y PP)
titulos = list()
titulos[[1]] = "                    MAM                  "
titulos[[2]] = "                    JJA                  "
titulos[[3]] = "                    SON                  "
titulos[[4]] = "                    DJF                  "
sin_m_pred = function(m, v, pred, titulo_num){
  #v = 1 o 3
  #v.sig = 2 o 4 
  
  if(v == 1){
    v.sig = 2
  } else if(v == 3){
    v.sig = 4
  } else {
    print("ESTA MAAAAL")
    break
  }
  
  aux = list()
  aux[[1]] = mapa_topo3(variable = pred[[m]][[v]]*mask_arr, variable.sig = pred[[m]][[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                        , titulo =  paste(letters[1+titulo_num],".",titulos[[1]], sep = "")
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , nombre.fig = "pred_temp", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 1
                        , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = 1, mostrar = T, save = F,  cb.v.w = 1
                        , cb.v.h = 35, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  aux[[2]] = mapa_topo3(variable = pred[[m]][[v]]*mask_arr, variable.sig = pred[[m]][[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                        , titulo =  paste(letters[2+titulo_num],".",titulos[[2]], sep = "")
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , nombre.fig = "pred_temp", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                        , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = 2, mostrar = T, save = F
                        , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  aux[[3]] = mapa_topo3(variable = pred[[m]][[v]]*mask_arr, variable.sig = pred[[m]][[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                        , titulo =  paste(letters[3 + titulo_num],".",titulos[[3]], sep = "")
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , nombre.fig = "pred_temp", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                        , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = 3, mostrar = T, save = F
                        , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  aux[[4]] = mapa_topo3(variable = pred[[m]][[v]]*mask_arr, variable.sig = pred[[m]][[v.sig]], colorbar = colorbars[[v]], revert = F, escala =escala_pred[[v]]
                        , titulo = paste(letters[4 + titulo_num],".",titulos[[4]], sep = "")
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , nombre.fig = "pred_temp", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                        , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = 4, mostrar = T, save = F
                        , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  return(aux)
}


# EMM
pred = pp_test(ss_temp = ss_temp, ss_pp = ss_pp, ensemble_total = "si")

# EMM sin mod
pred_wo =pp_test(ss_temp = ss_T, ss_pp = ss_PP, ensemble_total = "no")

escala_pred = list()
escala_pred[[1]] = seq(0,1, by = .1); escala_pred[[3]] = seq(0, .4, by = .05)

for(v in c(1,3)){
  if(v == 1){
    v.sig = 2
  } else if(v == 3){
    v.sig = 4
  } else {
    print("ESTA MAAAAL")
    break
  }
  
  
  MAM = mapa_topo3(variable = pred[[v]]*mask_arr, variable.sig = pred[[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                   , titulo =  paste(letters[1],".",titulos[[1]], sep = "")
                   , label.escala = "", mapa = "SA", width = 20, height = 20
                   , nombre.fig = "pred_temp", na.fill = -1000
                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 1
                   , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = 1, mostrar = T, save = F,  cb.v.w = 1
                   , cb.v.h = 26, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                   , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  JJA = mapa_topo3(variable = pred[[v]]*mask_arr, variable.sig = pred[[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                   , titulo =  paste(letters[2],".",titulos[[2]], sep = "")
                   , label.escala = "", mapa = "SA", width = 20, height = 20
                   , nombre.fig = "pred_temp", na.fill = -1000
                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                   , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = 2, mostrar = T, save = F
                   , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                   , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  SON = mapa_topo3(variable = pred[[v]]*mask_arr, variable.sig = pred[[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                   , titulo =  paste(letters[3],".",titulos[[3]], sep = "")
                   , label.escala = "", mapa = "SA", width = 20, height = 20
                   , nombre.fig = "pred_temp", na.fill = -1000
                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                   , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = 3, mostrar = T, save = F
                   , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                   , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  DJF = mapa_topo3(variable = pred[[v]]*mask_arr, variable.sig = pred[[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                   , titulo = paste(letters[4],".",titulos[[4]], sep = "")
                   , label.escala = "", mapa = "SA", width = 20, height = 20
                   , nombre.fig = "pred_temp", na.fill = -1000
                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                   , cajas = F, lon = lon2, lat = lat2, type.sig = "point", estacion = 4, mostrar = T, save = F
                   , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                   , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  no_mod1 = sin_m_pred(6, v, pred_wo, titulo_num = 4)
  
  no_mod2 = sin_m_pred(2, v, pred_wo, titulo_num = 8)
  
  no_mod3 = sin_m_pred(7, v, pred_wo, titulo_num = 12)
  
  
  
  colorbar1 <- g_legend(MAM)
  
  # panel 1 - emm
  gp1 = MAM + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp2 = JJA + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
  gp3 = SON + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp4 = DJF + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  #panel 2 - no_mod1
  gp5 = no_mod1[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp6 = no_mod1[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp7 = no_mod1[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp8 = no_mod1[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  #panel 3 - no_mod2
  gp9 = no_mod2[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp10 = no_mod2[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp11 = no_mod2[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp12 = no_mod2[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  
  #panel 4 - no_mod3
  gp13 = no_mod3[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp14 = no_mod3[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp15 = no_mod3[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp16 = no_mod3[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  
  gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                      gp11, gp12, gp13, gp14, gp15, gp16), ggplotGrob )
  
  lay <- rbind(c(1,1,2,2,3,3,4,4),c(1,1,2,2,3,3,4,4))
  
  p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],             
                    layout_matrix = lay
                    , left = textGrob("                          EMM Completo                       " # de esta forma tan elegante no afecta los margenes
                                     ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8)))  
  
  
  p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                    layout_matrix = lay
                    , left = textGrob("                          EMM sin CFSv2                      "
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8)))  
                    
  
  p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                    layout_matrix = lay
                    , left = textGrob("                          EMM sin CM2p1                      "
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8)))  
  
  p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                    layout_matrix = lay
                    ,  left = textGrob("                          EMM sin CM4i                      "
                                        ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8)))  
  
  
  
  lay <- rbind(c(1,1,1,1,1,1,1,1,3),c(1,1,1,1,1,1,1,1,3),
               c(2,2,2,2,2,2,2,2,3),c(2,2,2,2,2,2,2,2,3))
  
  if(v ==1){
    nombre = "T_"
  } else {
    nombre = "PP_"
  }
  
  
  nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, ".PRED", ".jpg", sep = "")
  
  ggsave(nombre_fig,plot =grid.arrange(p1, p2, ncol = 2, layout_matrix = lay, colorbar1) ,width = 30, height = 17 ,units = "cm")

}



##### MAPA cajas #######
# MAPA cajas y nombres. VER! que cajas usar sobre brasil.
load("topo_sa.RData")
topo2 = topo_sa
rm(topo_sa)

topo3 = topo2
topo3[which(topo2$h<1500)]=NA

mask2= expand.grid(lon = lon2, lat = lat2)
mask2[,3] = array(mask, dim = length(lon2)*length(lat2))

map <- map_data("world2", regions = c("Brazil", "French Guiana", "Suriname", "Colombia", "Venezuela","Argentina", "Chile", "Uruguay",
                                      "Bolivia", "Ecuador", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua",
                                      "Martinique", "falkland islands", "Honduras", "El Salvador", "Guatemala", "Belice"), colour = "black")

breaks.lon = seq(270, 335, by = 10); limits.lon = c(min(breaks.lon), max(breaks.lon))
breaks.lat = seq(-60, 20, by = 10); limits.lat = c(min(breaks.lat), max(breaks.lat))

lats = list()
lats[[1]] = seq(which(lat2 == -29), which(lat2 == -17), by = 1); lats[[2]] = seq(which(lat2 == -39), which(lat2 == -25), by = 1)
lats[[3]] = seq(which(lat2 == -15), which(lat2 == 2), by = 1); lats[[4]] = seq(which(lat2 == -55), which(lat2 == -37), by = 1)
lats[[5]] = seq(which(lat2 == -13), which(lat2 == 2), by = 1)
#

lons = list()
lons[[1]] = seq(which(lon2 == 303), which(lon2 == 315), by = 1); lons[[2]] = seq(which(lon2 == 296), which(lon2 == 306), by = 1)
lons[[3]] = seq(which(lon2 == 311), which(lon2 == 325), by = 1); lons[[4]] = seq(which(lon2 == 287), which(lon2 == 294), by = 1)
lons[[5]] = seq(which(lon2 == 291), which(lon2 == 304), by = 1)



area = array(1, dim = c(56,76))

for(i in 1:6){
  if(i == 1){
    area[lons[[i]], lats[[i]]] = 2
  } else if(i<5){
    area[lons[[i]], lats[[i]]] = 2.01
  } else if(i==5){
    area[lons[[i]], lats[[i]]] = 2.02
  }
}


data2 = expand.grid(lon = lon2, lat = lat2)
data2[,3] = array(area, dim = length(lon2)*length(lat2))
colnames(data2)<-c("lon", "lat", "cont")


g = ggplot(topo2, aes(lon, lat)) + theme_minimal() +
  xlab(NULL) + ylab(NULL) + 
  theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey", size = 0.3), panel.grid.minor = element_blank()) +
  geom_contour_fill(aes(z = h),alpha = 1, na.fill = -10000 , breaks = seq(0,4500, by = 250), show.legend = F) +
  scale_fill_gradient2(low = "steelblue", high = "goldenrod2", mid = "olivedrab")+
  geom_polygon(data = map, aes(x = long ,y = lat, group = group),fill = NA, color = "black") +
  geom_tile(data = topo3, aes(x = lon, y = lat, fill = h ), na.rm = T, alpha = 1, color = "black", show.legend = F) +
  ggtitle(paste("cajas")) +
  geom_text(x = 300, y = -32, label = "S-SESA", aes(angle = 0), size = 5) +
  geom_text(x = 309, y = -22, label = "N-SESA", aes(angle = 0), size = 5) +
  geom_text(x = 290, y = -45, label = "Patagonia", aes(angle = 90), size = 5) +
  geom_text(x = 317, y = -5, label = "NeB", aes(angle = 0), size = 5) +
  geom_text(x = 297, y = -5, label = "Am", aes(angle = 0), size = 5) +
  stat_contour(data = data2, aes(x = lon, y = lat, z = cont), color = "black", size = .3, breaks = c(2, 2.01, 2.02)) +
  scale_x_longitude(breaks = breaks.lon, name = NULL, limits = c(270, 335))+
  scale_y_latitude(breaks = breaks.lat, name = NULL, limits = c(-60, 20))+
  theme(axis.text.y   = element_text(size = 15), axis.text.x   = element_text(size = 15), axis.title.y  = element_text(size = 15),
        axis.title.x  = element_text(size = 15), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
        panel.ontop = TRUE, 
        plot.title = element_text(hjust = 0.5, size = 15)) + geom_hline(yintercept = 0, color = "black") 

ggsave(paste("/home/luciano.andrian/tesis/salidas/F.Finales/", "fig", ".jpg", sep = ""), plot = g, width = 20, height = 20, units = "cm")


##### --------------------------- ACC ----------------------- ######
#save(resultados, file = "ACC.RData")
load("ACC.RData") # "resutlados"

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}


resultados[[2]] = resultados[[2]][,,,3]
resultados[[5]] = resultados[[5]][,,,,3]
colorbars = list()
colorbars[[1]] = "YlOrRd"; colorbars[[2]] = "PuBuGn"

colorbars_gamma = list()
colorbars_gamma[[1]] = "RdPu"; colorbars_gamma[[3]] = "BuPu"

seasons = c("MAM", "JJA", "SON", "DJF")


titulo_acc1_2 = c("                          MAM                        ",
                  "                          JJA                        ",
                  "                          SON                        ",
                  "                          DJF                        ")
titulo_season = c("MAM", "JJA", "SON", "DJF")
l = c("a)               ","b)              ","c)              ","d)             ")

nombres2 = c("CCSM4", "CM2p1", "FLOR-A06", "FLOR-B01", "GEOS5", "CFSv2", "CanCM4i", "GEM-NEMO") 
ACC_EMM = list(); ACC_wo_mod = list(); ACC_teo = list()


for(v in 1:2){
  for(acc in 1:3){
    for(season in 1:4){
      
      if(acc == 1){
        titulo_num = 4
        if(v == 1){
          v1 = 6
        } else if(v == 2){
          v1 = 7
        }
        ### ACC teorico
        ACC_teo[[season]] =  mapa_topo3(variable = resultados[[v1]]*mask_arr, variable.sig = resultados[[v1]]*mask_arr, v.sig = resultados[[3]]
                                        , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                                        , titulo =   paste(letters[season+titulo_num],".",titulo_acc1_2[season], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                                        , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5
                                        , r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                                        , lon = lon2, lat = lat2, type.sig = "point2", estacion = season
                                        , mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 35, cb.size = 14
                                        , lats.size = 7, letter.size = 12, cajas = T
                                        , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
        
      } else if(acc == 2){
        titulo_num = 0
       ACC_EMM[[season]] =  mapa_topo3(variable = resultados[[v]]*mask_arr, variable.sig = resultados[[v]]*mask_arr, v.sig = resultados[[3]]
                                            , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                                            , titulo =  paste(letters[season+titulo_num],".",titulo_acc1_2[season], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                                            , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5
                                            , r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                                            , lon = lon2, lat = lat2, type.sig = "point2", estacion = season
                                            , mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 35, cb.size = 14
                                            , lats.size = 7, letter.size = 12, cajas = T
                                            , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
       
        
      } else if( acc == 3){
        titulo_num = 8
        if(v == 1){
          v2 = 4
          
          if(season == 1){
            m = 4
          } else if(season == 2){
            m = 8
          } else if(season == 3){
            m = 7
          } else if(season == 4){
            m = 8
          }
          
        } else if(v == 2){
          v2 = 5
          
          if(season == 1){
            m = 3
          } else if(season == 2){
            m = 2
          } else if(season == 3){
            m = 2
          } else if(season == 4){
            m = 5
          }
        }
        
        
        
        
        ACC_wo_mod[[season]] = mapa_topo3(variable = resultados[[v2]][,,,m]*mask_arr, variable.sig = resultados[[v2]][,,,m]*mask_arr, v.sig = resultados[[3]]
                                   , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                                   , titulo =  paste(letters[season+titulo_num],".", "      ", titulo_season[season], " - EMM sin ", nombres2[m],"             ", sep = ""), label.escala = ""
                                   , mapa = "SA", width = 20, height = 20, na.fill = -1000
                                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4
                                   , estaciones = T, altura.topo = 1500, size.point = 0.2
                                   , lon = lon2, lat = lat2, type.sig = "point2",estacion = season
                                   , mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                                   , lats.size = 7, letter.size = 12, cajas = T
                                   , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
   
          
      
        
      
      }
    }
  }
  
  
  colorbar1 <- g_legend(ACC_EMM[[1]])

  # panel 1 
  gp1 = ACC_teo[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp2 = ACC_teo[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp3 = ACC_teo[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp4 = ACC_teo[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
 
  # panel 2 
  gp5 = ACC_EMM[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp6 = ACC_EMM[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp7 = ACC_EMM[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp8 = ACC_EMM[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  # panel 3
  gp9 = ACC_wo_mod[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp10 = ACC_wo_mod[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp11 = ACC_wo_mod[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp12 = ACC_wo_mod[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))


  gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                      gp11, gp12), ggplotGrob)
  
  lay <- rbind(c(1,1,2,2,3,3,4,4),c(1,1,2,2,3,3,4,4))
  
  p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],          
                    layout_matrix = lay
                    , left = textGrob("ACC Teorico", y = .5 
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8)))  
  
  
  p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                    layout_matrix = lay
                    , left = textGrob("ACC Observado", y =.5
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8)))  
  
  
  
  p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                    layout_matrix = lay
                    , left = textGrob("ACC", y = 0.5
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8)))  
  
  
  lay <- rbind(c(1,1,1,1,1,1,1,1,4),c(1,1,1,1,1,1,1,1,4),
               c(2,2,2,2,2,2,2,2,4),c(2,2,2,2,2,2,2,2,4), 
               c(3,3,3,3,3,3,3,3,4),c(3,3,3,3,3,3,3,3,4))
  
  if(v ==1){
    nombre = "T_"
  } else {
    nombre = "PP_"
  }
  
  
  nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, ".ACC", ".jpg", sep = "")
  
  ggsave(nombre_fig,plot =grid.arrange(p2, p1, p3, ncol = 2, layout_matrix = lay, colorbar1) ,width = 35, height = 35 ,units = "cm")

}

##### ------------------- ACC obs vs teorico ----------------####
#
load("ACC.RData")


ruta = getwd()

titulo_acc1_2 = c("         MAM          ",
                  "         JJA          ",
                  "         SON          ",
                  "         DJF          ")
titulo_num = c(4,8,12,16,20,0)

lats = list()
lats[[1]] = seq(which(lat2 == -29), which(lat2 == -17), by = 1); lats[[2]] = seq(which(lat2 == -39), which(lat2 == -25), by = 1)
lats[[3]] = seq(which(lat2 == -15), which(lat2 == 2), by = 1); lats[[4]] = seq(which(lat2 == -55), which(lat2 == -37), by = 1)
lats[[5]] = seq(which(lat2 == -13), which(lat2 == 2), by = 1); lats[[6]] = lat2
 
lons = list()
lons[[1]] = seq(which(lon2 == 303), which(lon2 == 315), by = 1); lons[[2]] = seq(which(lon2 == 296), which(lon2 == 306), by = 1)
lons[[3]] = seq(which(lon2 == 311), which(lon2 == 325), by = 1); lons[[4]] = seq(which(lon2 == 287), which(lon2 == 294), by = 1)
lons[[5]] = seq(which(lon2 == 291), which(lon2 == 304), by = 1); lons[[6]] = lon2

figs = list()

for(c in 1:6){
  
  figs[[c]] = list()
  area = array(NA, dim = c(56,76))

  for(i in 1:4){
    
    if(c != 6){
      area[lons[[c]], lats[[c]]] = 1
      size.point = 1; stroke.size = 0.8
    } else {
      area = 1
      size.point = 0.3; stroke.size = 0.6
    }
    
    
    data = matrix(data = NA, nrow = length(lon2)*length(lat2), ncol = 4)
    data2 = matrix(data = NA, nrow = length(lon2)*length(lat2), ncol = 4)
    
    data[,1] = array(data = resultados[[1]][,,i]*mask*area, dim = length(lon2)*length(lat2))
    data[,2] = array(data = resultados[[6]][,,i]*mask*area, dim = length(lon2)*length(lat2))
    data[,3] = 1
    
    data2[,1] = array(data = resultados[[2]][,,i,3]*mask*area, dim = length(lon2)*length(lat2))
    data2[,2] = array(data = resultados[[7]][,,i]*mask*area, dim = length(lon2)*length(lat2))
    data2[,3] = 2
    
    
    data3 = as.data.frame(rbind(data, data2))
    
    colnames(data3) = c("Y", "X", "var")
    
    data3$var = as.factor(data3$var)
    
    dataline = matrix(data = NA, nrow = 2, ncol = 2)
    dataline[,1] = c(-0.1,1)
    dataline[,2] = c(-0.1,1)
    dataline = as.data.frame(dataline)
    colnames(dataline) = c("XX", "YY")
    
    
    g  = ggplot(data = data3, mapping = aes(x = X, y = Y)) + theme_minimal() +
      geom_point(data = data3,aes(colour = var, shape = as.factor(var)),show.legend = T, size = size.point, stroke = stroke.size) +
      scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), name = "ACC Teorico") +
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), name = "ACC Observado") +
      scale_color_manual(values = c("tomato3","steelblue3"), breaks = c(1,2), name = "", labels = c("Temp", "Precip")) +
      geom_hline(yintercept = 0.31, linetype = 2)+
      geom_abline() +
      scale_shape_discrete(guide = F) +
      theme(axis.text.y   = element_text(size = 8), axis.text.x   = element_text(size = 8), axis.title.y  = element_text(size = 8),
            axis.title.x  = element_text(size = 8),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
            panel.ontop = F,
            plot.title = element_text(hjust = 0.5), legend.position = "bottom") 
    
      g = g + ggtitle(paste(letters[i+titulo_num[c]],".",titulo_acc1_2[i], sep = "")) #titulo_acc1_2[i])


   
    
    figs[[c]][[i]] = g
    
  }
}



colorbar1 <- g_legend(figs[[1]][[1]])

# panel 1 


gp1 = figs[[1]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp2 = figs[[1]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp3 = figs[[1]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp4 = figs[[1]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))

# panel 2 
gp5 = figs[[2]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp6 = figs[[2]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp7 = figs[[2]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp8 = figs[[2]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))

# panel 3
gp9 = figs[[3]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp10 = figs[[3]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp11 = figs[[3]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp12 = figs[[3]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))

# panel 3
gp13 = figs[[4]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp14 = figs[[4]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp15 = figs[[4]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp16 = figs[[4]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))

# panel 3
gp17 = figs[[5]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp18 = figs[[5]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp19 = figs[[5]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp20 = figs[[5]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))

gp21 = figs[[6]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp22 = figs[[6]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp23 = figs[[6]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))
gp24 = figs[[6]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,0,.2), "lines"))

gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                    gp11, gp12, gp13, gp14, gp15, gp16, gp17,gp18,
                    gp19, gp20, gp21, gp22, gp23, gp24), ggplotGrob)

lay <- rbind(c(1,1,2,2,3,3,4,4),c(1,1,2,2,3,3,4,4))

p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],          
                  layout_matrix = lay
                  , left = textGrob("N-SESA", y = .5 
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0
                                   , gp=gpar(fontsize=16,font=8))) 


p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                  layout_matrix = lay
                  , left = textGrob("S-SESA", y =.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 



p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                  layout_matrix = lay
                  , left = textGrob("NeB", y = 0.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 

p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                  layout_matrix = lay
                  , left = textGrob("Patagonia", y = 0.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 

p5 = grid.arrange(gpls[[17]], gpls[[18]], gpls[[19]], gpls[[20]],
                  layout_matrix = lay
                  , left = textGrob("Am", y = 0.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 

p6 = grid.arrange(gpls[[21]], gpls[[22]], gpls[[23]], gpls[[24]],
                  layout_matrix = lay
                  , left = textGrob("Sudamérica", y = 0.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 



lay <- rbind(c(1,1,1,1,1,1,1,1),c(1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,2,2,2),c(2,2,2,2,2,2,2,2), 
             c(3,3,3,3,3,3,3,3),c(3,3,3,3,3,3,3,3),
             c(4,4,4,4,4,4,4,4),c(4,4,4,4,4,4,4,4),
             c(5,5,5,5,5,5,5,5),c(5,5,5,5,5,5,5,5),
             c(6,6,6,6,6,6,6,6),c(6,6,6,6,6,6,6,6))


nombre_fig = paste(getwd(),"/salidas/F.Finales/", "ACC_vs", ".jpg", sep = "")

ggsave(nombre_fig,plot =grid.arrange(p6, p1, p2, p3, p4, p5, ncol = 2, layout_matrix = lay) ,width = 20, height = 35 ,units = "cm")

##### ACC cajas y modelos #####
# source("aux.desemp.R")
save(resultados, file = "ACC.RData")
load("ACC.RData")

t.ACC = resultados[[1]]; pp.ACC = resultados[[2]]
t.ACC_mod = resultados[[8]]; pp.ACC_mod = resultados[[9]]
rc = resultados[[3]]
cajas_lat = list()
cajas_lat[[1]] = seq(which(lat2 == -29), which(lat2 == -17), by = 1); cajas_lat[[2]] = seq(which(lat2 == -39), which(lat2 == -25), by = 1)
cajas_lat[[3]] = seq(which(lat2 == -15), which(lat2 == 2), by = 1); cajas_lat[[4]] = seq(which(lat2 == -55), which(lat2 == -37), by = 1)
cajas_lat[[5]] = seq(which(lat2 == -13), which(lat2 == 2), by = 1)
#

cajas_lon= list()
cajas_lon[[1]] = seq(which(lon2 == 303), which(lon2 == 315), by = 1); cajas_lon[[2]] = seq(which(lon2 == 296), which(lon2 == 306), by = 1)
cajas_lon[[3]] = seq(which(lon2 == 311), which(lon2 == 325), by = 1); cajas_lon[[4]] = seq(which(lon2 == 287), which(lon2 == 294), by = 1)
cajas_lon[[5]] = seq(which(lon2 == 291), which(lon2 == 304), by = 1)

t.ACC_box = list()
pp.ACC_box = list()
t.ACC_ens_box = list()
pp.ACC_ens_box = list()
for(i in 1:5){
  
  t.ACC_ens_box[[i]] = apply(t.ACC[cajas_lon[[i]], cajas_lat[[i]],], c(3), mean, na.rm = T)
  pp.ACC_ens_box[[i]] = apply(pp.ACC[cajas_lon[[i]], cajas_lat[[i]],,], c(3, 4), mean, na.rm = T)
  
  t.ACC_box[[i]] = apply(t.ACC_mod[cajas_lon[[i]], cajas_lat[[i]],,], c(3, 4), mean, na.rm = T)
  
  pp.ACC_box[[i]] = apply(pp.ACC_mod[cajas_lon[[i]], cajas_lat[[i]],,,], c(3, 4, 5), mean, na.rm = T)
  
}
cajas = c("N-SESA", "S-SESA", "NeB", "Patagonia", "Am")
cajas_num = seq( 1, 5)


t.data = fig10(prom_cajas = t.ACC_box, prom_ensamble = t.ACC_ens_box, variable = "temp")


pp.data = fig10(prom_cajas = pp.ACC_box, prom_ensamble = pp.ACC_ens_box, variable = "pp")


# Grafico
g = ggplot() + theme_minimal()+
  geom_text(data = t.data[[1]], aes(x = cajas_mam, y = MAM, label = value), color = "goldenrod2", size = 4) + 
  geom_point(data = t.data[[2]], aes(x = cajas_mam, y = MAM), color = "goldenrod4", shape = "*" , size = 11) +
  geom_text(data = t.data[[1]], aes(x = cajas_jja, y = JJA, label = value), color = "royalblue", size = 4) + 
  geom_point(data = t.data[[2]], aes(x = cajas_jja, y = JJA), color = "royalblue4", shape = "*" , size = 11) +
  geom_text(data = t.data[[1]], aes(x = cajas_son, y = SON, label = value), color = "springgreen2", size = 4) + 
  geom_point(data = t.data[[2]], aes(x = cajas_son, y = SON), color = "springgreen4", shape = "*" , size = 11) +
  geom_text(data = t.data[[1]], aes(x = cajas_djf, y = DJF, label = value), color = "red1", size = 4) + 
  geom_point(data = t.data[[2]], aes(x = cajas_djf, y = DJF), color = "red4", shape = "*" , size = 11) +
  geom_hline(yintercept = rc, color = "grey", size = 1) +
  geom_hline(yintercept = 0, color = "black")+
  ggtitle(paste("ACC - Temperatura"))+
  scale_y_continuous(limits = c(-0.2, 0.8), breaks = seq(-0.2,0.8, by = 0.2)) + 
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0,0.8, by = 0.2)) + 
  scale_x_continuous(labels=c("1" = "N-SESA", "2" = "S-SESA", "3" = "NeB", "4" = "Patagonia", "5" = "Am"),breaks = seq(1, 5, by = 1))+
  xlab(label = "COLA-CCSM4(1), GFDL-CM2p1(2), GFDL-FLOR-A06(3), GFDL-FLOR-B01(4), NASA-GEOS5(5), NCEP-CFSv2(6) CMC-CanCM4i(7), CMC-GEM-NEMO(8)" )+
  theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black", face = "bold"), axis.title.y  = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5)) 
ggsave(paste("/home/luciano.andrian/tesis/salidas/F.Finales/", "ACC_T.jpg",sep =""), plot = g, width = 30, height = 15  , units = "cm")


    


g = ggplot() + theme_minimal()+
  geom_text(data = pp.data[[1]], aes(x = cajas_mam, y = MAM, label = value), color = "forestgreen", size = 4) + 
  geom_point(data = pp.data[[2]], aes(x = cajas_mam, y = MAM), color = "darkgreen", shape = "*" , size = 11) +
  geom_text(data = pp.data[[1]], aes(x = cajas_jja, y = JJA, label = value), color = "tan2", size = 4) + 
  geom_point(data = pp.data[[2]], aes(x = cajas_jja, y = JJA), color = "tan4", shape = "*" , size = 11) +
  geom_text(data = pp.data[[1]], aes(x = cajas_son, y = SON, label = value), color = "deepskyblue2", size = 4) + 
  geom_point(data = pp.data[[2]], aes(x = cajas_son, y = SON), color = "deepskyblue4", shape = "*" , size = 11) +
  geom_text(data = pp.data[[1]], aes(x = cajas_djf, y = DJF, label = value), color = "midnightblue", size = 4) + 
  geom_point(data = pp.data[[2]], aes(x = cajas_djf, y = DJF), color = "navyblue", shape = "*" , size = 11) +
  geom_hline(yintercept = rc, color = "grey", size = 1) +
  geom_hline(yintercept = 0, color = "black")+
  ggtitle(paste("ACC - Precipitación"))+
  scale_y_continuous(limits = c(-0.2, 0.8), breaks = seq(-0.2,0.8, by = 0.2)) + 
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0,0.8, by = 0.2)) + 
  scale_x_continuous(labels=c("1" = "N-SESA", "2" = "S-SESA", "3" = "NeB", "4" = "Patagonia", "5" = "Am"),breaks = seq(1, 5, by = 1))+
  xlab(label = "COLA-CCSM4(1), GFDL-CM2p1(2), GFDL-FLOR-A06(3), GFDL-FLOR-B01(4), NASA-GEOS5(5), NCEP-CFSv2(6) CMC-CanCM4i(7), CMC-GEM-NEMO(8)" )+
  theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black", face = "bold"), axis.title.y  = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5)) 
ggsave(paste("/home/luciano.andrian/tesis/salidas/F.Finales/", "ACC_PP.jpg",sep =""), plot = g, width = 30, height = 15  , units = "cm")


######



#### indices de verificacion #####

source("funciones.R")

library(ggplot2)
library(gridExtra)
library(ncdf4)

#---------------------------------------------------------------#
g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}
#source("aux.desemp2.R")
#save(resultados, file = "desemp.RData")

load("desemp.RData") # "resutlados"

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}


resultados2 = list()

resultados2[[1]] = resultados[[1]]
resultados2[[3]] = resultados[[3]]



resultados2[[2]] = resultados[[2]][,,,3]
resultados2[[4]] = resultados[[4]][,,,3]


colorbars = list()
colorbars[[1]] = c("RdYlBu",1,"YlOrRd",1, "PuOr")
colorbars[[2]] = c(1,"BrBG",1, "PuBuGn",1, "BrBG")


## RMSE normalizado con SD = 1 - RMSE/sd


source("rmse_corregido.R")

resultados2[[5]] = NRMSE[[1]]
resultados2[[6]] = NRMSE[[2]]



escala = list(); escala[[1]] = escala[[2]] = escala[[3]] = list()
escala[[1]][[1]] = seq(-5, 5, by = 1); escala[[1]][[3]] = seq(0, 5, by = 1); escala[[1]][[5]] = seq(-.6, .6, by = .15)

escala[[2]][[2]] = seq(-100, 100, by = 20); escala[[2]][[4]] = seq(0, 100, by = 20); escala[[2]][[6]] = seq(-.6, .6, by = .15)  

revert = list()
revert[[1]] = c(T,1,F,1,T); revert[[2]] = c(1,F,1,F,1, F)

seasons = c("MAM", "JJA", "SON", "DJF")


titulo_acc1_2 = c("                          MAM                        ",
                  "                          JJA                        ",
                  "                          SON                        ",
                  "                          DJF                        ")




desemp = list()
desemp[[1]] = desemp[[2]] = desemp[[3]] = desemp[[4]] = desemp[[5]] = desemp[[6]] = list()


var2 = list()
var2[[1]] = c(1,3,5); var2[[2]] = c(2,4,6)

for(v in 1:2){
  for(v2 in var2[[v]]){
    if(v2 == 1){
      titulo_num = 0
    } else if(v2 == 3){
      titulo_num = 4
    } else {
      titulo_num = 8
    }
      for(season in 1:4){

        desemp[[v2]][[season]] = mapa_topo3(variable = resultados2[[v2]]*mask_arr
                                           , colorbar = colorbars[[v]][v2], revert = revert[[v]][v2], escala = c(escala[[v]][[v2]])
                                           , titulo =  paste(letters[season+titulo_num],".", titulo_acc1_2[season], sep = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                                           , na.fill = -1000
                                           , r = 4, estaciones = T, altura.topo = 1500
                                           , lon = lon2, lat = lat2, estacion = season
                                           , mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 19, cb.size = 15
                                           , lats.size = 7, letter.size = 12, cajas = F
                                           , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
        
        
        
        
      
    }
  }
  
  if(v == 1){
    desemp[[2]] = desemp[[3]]; desemp[[3]] = desemp[[5]] 
  } else {
    desemp[[1]] = desemp[[2]]; desemp[[2]] = desemp[[4]]; desemp[[3]] = desemp[[6]] 
  }
  
  colorbar1 = g_legend(desemp[[1]][[1]]); colorbar2 = g_legend(desemp[[2]][[2]]); colorbar3 = g_legend(desemp[[3]][[1]])
  
  # panel 1 
  gp1 = desemp[[1]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp2 = desemp[[1]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp3 = desemp[[1]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp4 = desemp[[1]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  # panel 2 
  gp5 = desemp[[2]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp6 = desemp[[2]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp7 = desemp[[2]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp8 = desemp[[2]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  # panel 3
  gp9 = desemp[[3]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp10 = desemp[[3]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp11 = desemp[[3]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  gp12 = desemp[[3]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
  
  
  gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                      gp11, gp12), ggplotGrob)
  
  lay <- rbind(c(1,1,2,2,3,3,4,4),c(1,1,2,2,3,3,4,4))
  
  p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],          
                    layout_matrix = lay
                    , left = textGrob("Bias", y = .5 
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8))) 
  
  
  p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                    layout_matrix = lay
                    , left = textGrob("MAE", y =.5
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8))) 
  
  
  
  p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                    layout_matrix = lay
                    , left = textGrob("NRMSE", y = 0.5
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob(" ", x = 0 
                                     , gp=gpar(fontsize=16,font=8))) 
  
  
  lay <- rbind(c(1,1,1,1,1,1,1,1,4),c(1,1,1,1,1,1,1,1,4),
               c(2,2,2,2,2,2,2,2,5),c(2,2,2,2,2,2,2,2,5), 
               c(3,3,3,3,3,3,3,3,6),c(3,3,3,3,3,3,3,3,6))
  
  if(v ==1){
    nombre = "T_"
  } else {
    nombre = "PP_"
  }
  
  
  nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, "desemp", ".jpg", sep = "")
  
  ggsave(nombre_fig,plot =grid.arrange(p1, p2, p3, ncol = 2, layout_matrix = lay, colorbar1, colorbar2, colorbar3) ,width = 35, height = 35 ,units = "cm")
  
}
  
 
##### corr cm2p1 sst ######

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
nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO") 
aux3 = array(data = NA, dim = c(56,76,29,4,8))
for(i in 1:8){
  aux3[,,,,i] = mean_sd(nombres[i])[[6]]
}

#---- ensambles ----#
pp.ens = apply(aux3, c(1,2,3,4), mean, na.rm = T)

# "SESA_3"
#lats.area[[11]] = seq(which(lat == -37.5), which(lat == -20.5), by = 1)
#lons[[11]] = seq(which(lon2 == 298), which(lon2 == 313), by = 1)

# N-SESA y NeB
lats = lons = list()
lats[[1]] = seq(which(lat2 == -29), which(lat2 == -17), by = 1); lats[[2]] = seq(which(lat2 == -15), which(lat2 == 2), by = 1)
lons[[1]] = seq(which(lon2 == 303), which(lon2 == 315), by = 1); lons[[2]] = seq(which(lon2 == 311), which(lon2 == 325), by = 1)

# para la caja
lat = rev(lat)
lats.area = list() # un poco distintas debido al grillado del mapa. 1x1 pero 1.5 2.5 3.5
lats.area[[1]] =  seq(which(lat == -28.5), which(lat == -16.5), by = 1); lats.area[[2]] = seq(which(lat == -15.5), which(lat == 2.5), by = 1)

lons.area = list()
lons.area[[1]] =  seq(which(lon == 302.5), which(lon == 315.5), by = 1); lons.area[[2]] = seq(which(lon == 310.5), which(lon == 325.5), by = 1)

# titulos
region = c("N-SESA", "NeB")
region.fig = c("N-SESA", "NeB")
estaciones = c("MAM", "JJA", "SON", "DJF")

#### EMM ####
# pp 
mapa_EMM = list()
mapa_EMM[[1]] = mapa_EMM[[2]] = 1
for(i in 1:2){
  area = w.mask
  area[lons.area[[i]], lats.area[[i]]] = 2
  
  aux.prom = apply(pp.ens[lons[[i]], lats[[i]],,], c(3,4), mean, na.rm = T)
  
  j = 3
    
    aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
    
    # prueba mapa_topo3
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
   mapa_EMM[[i]] =  mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
               , titulo = paste("Correlación PP ", region[i], " vs SST - ", estaciones[j], sep = "")
               , label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
               , r = 1, width = 30, height = 15, salida =  "/salidas/corr/", save = F, mostrar = T
               , nombre.fig = paste("pp.corr_", region.fig[i],"_", estaciones[j], sep = ""), na.fill = 0
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, fill.mapa = T, lats.size = 7, letter.size = 12, margen.zero = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
    
  
}

#### Corr por modelos ####
mapa_cm2p1 = list()
mapa_cm2p1[[1]] = mapa_cm2p1[[2]] = 1
for(i in 1:2){ 
  m = 2 
    area = w.mask
    area[lons.area[[i]], lats.area[[i]]] = 2
    aux.prom = apply(aux3[lons[[i]], lats[[i]],,,m], c(3,4), mean, na.rm = T)
    
    j = 3
      
      aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
      
      aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
      aux.sig = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
      auxa = array(area, dim = c(dim(area), 1))
      
      mapa_cm2p1[[i]] = mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
                 , titulo = paste("Correlación PP ", region[i], " vs SST - ", estaciones[j], sep = "")
                 , label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
                 , r = 1, width = 30, height = 15, salida =  "/salidas/corr/modelos/", save = F, mostrar = T
                 , nombre.fig = paste("pp.corr_",region.fig[i], nombres[m],"_", estaciones[j], sep = ""), na.fill = 0
                 , sig = T, variable.sig = aux.sig, color.vsig = "white", alpha.vsig = 1, fill.mapa = T, lats.size = 7, letter.size = 12, margen.zero = T
                 , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
    
  
}


#--- SST con PP CMAP y CPC ---#
# cmap
# cambio de periodo para los dos, un año menos.

#### SST ####

aux = nc_open("X140.172.38.222.142.12.42.20.nc")
sst = ncvar_get(aux, "sst")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
nc_close(aux)
# rotando el array
sst = sst[,ncol(sst):1,1:336]

#### mascara ####
aux = nc_open("X190.191.246.159.142.12.48.11.nc")
w.mask = ncvar_get(aux, "mask")
nc_close(aux)
# rotando..
w.mask = w.mask[,ncol(w.mask):1] 

#### meses de ci ####
sst.ci = array(data = NA, dim = c(360, 180, 28, 4))
ci = c(2, 5, 8, 11)
for(i in 1:4){
  for(j in 0:27)
    sst.ci[,,j + 1, i] = sst[,,ci[i]+12*j]*w.mask
}


#### PP cmap ####
require(fields)
aux = nc_open("/home/luciano.andrian/tesis/X190.191.242.210.56.5.48.49.nc")

lon4 = ncvar_get(aux, "lon")
lat4 = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[,,27:363]
nc_close(aux)


pp3_int = array(NA, dim = c(58, 78, 336)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:336){
  
  mod = list(x = lon4, y = lat4, z = aux2[,,i])
  
  grid = list(x=seq(min(lon4), max(lon4), by = 1), y = seq(min(lat4), max(lat2)+1, by = 1))
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
  pp3_int[,,i] = pp_aux$z  # algo esta mal con esta
}


pp3_estaciones = array(NA, dim = c(58, 78, 28, 12))

for(j in 1:12){
  for (i in 0:27){
    pp3_estaciones[,,1+i,j] = pp3_int[1:58 , 1:78, j+12*i]
  }
}


estaciones_p_a_pp3 = array(NA, dim = c(58, 78, 28, 4))
i=1
while(i<=4){
  estaciones_p_a_pp3[,,,i] = apply(pp3_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)*30 # esta en mm/day
  i = i + 1
}

pp.cmap = estaciones_p_a_pp3


#### Graficos por zonas ###
# CMAP
mapa_cmap = list()
mapa_cmap[[1]] = mapa_cmap[[2]] = 1

for(i in 1:2){
  area = w.mask
  area[lons.area[[i]], lats.area[[i]]] = 2
  
  aux.prom = apply(pp.cmap[lons[[i]], lats[[i]],,], c(3,4), mean, na.rm = T)
  
  j = 3
    
    aux.corr = corr(mod = aux.prom[,j], obs = sst.ci[,,,j], lon = 360, lat = 180, cf = 0.95)
    
    # prueba mapa_topo3
    
    aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
    aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
    auxa = array(area, dim = c(dim(area), 1))
    
   mapa_cmap[[i]] =  mapa_topo3(variable = aux, lon = seq(1,360), lat = seq(-90,90), colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
               , titulo = paste("Correlación PP ", region[i], " vs SST - ", estaciones[j], sep = "")
               , label.escala = "", x.label = NULL, y.label = NULL, mapa = "mundo"
               , r = 1, width = 30, height = 15, salida =  "/salidas/corr/obs/", save = F, mostrar = T
               , nombre.fig = paste("pp.corr.cmap_", region.fig[i],"_", estaciones[j], sep = ""), na.fill = 0
               , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1, fill.mapa = T, lats.size = 7, letter.size = 12, margen.zero = T
               , variable.cont = auxa, contour = T, nivel.vcont = 2, color.vcont = "red", colorbar.pos = "bottom")
    
  
}


#niño3.4 con pp

aux = nc_open("X140.172.38.222.142.12.42.20.nc")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
nc_close(aux)

mask2 = as.matrix(read.table("mascara.txt"))
mask2 = array(mask2, dim = c(56,76,28,4))

pp.cmap = pp.cmap[2:57,2:77,,]*mask2


lats.n34 = seq(which(lat == -4.5), which(lat == 5.5))
lons.n34 = seq(which(lon == 190.5), which(lon == 240.5))

area = w.mask
area[lons.n34, lats.n34] = 2

aux.prom = apply(sst.ci[lons.n34, lats.n34,,], c(3,4), mean, na.rm = T)

j = 3
aux.corr = corr(mod = aux.prom[,j], obs = pp.cmap[,,,j], lon = 56, lat = 76, cf = 0.95)
aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
auxa = array(area, dim = c(dim(area), 1))

mapa_sa_cmap = mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
                          , titulo = paste("Correlación PP CMAP vs SST Niño3.4 - " , estaciones[j], sep = "")
                          , label.escala = "", x.label = NULL, y.label = NULL, mapa = "SA"
                          , r = 1, width = 20, height = 20, salida =  "/salidas/corr/n34/", save = F, mostrar = T
                          , nombre.fig = paste("pp.cmap.corr.SA_", estaciones[j], sep = ""), na.fill = -1000, lats.size = 7, letter.size = 12, margen.zero = T
                          , sig = T, variable.sig = aux2, color.vsig = "black", alpha.vsig = 0.4, altura.topo = 1500, type.sig = "point", size.point = 0.2)

  

##### Con pp del ensamble ####
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


area = w.mask
area[lons.n34, lats.n34] = 2

aux.prom = apply(sst.ci[lons.n34, lats.n34,,], c(3,4), mean, na.rm = T)

mask = array(as.matrix(read.table("mascara.txt")), dim = c(dim(pp.ens)))

pp.ens = pp.ens*mask

j = 3

aux.corr = corr(mod = aux.prom[,j], obs = pp.ens[,,,j], lon = 56, lat = 76, cf = 0.95)

# prueba mapa_topo3

aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
aux2 = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
auxa = array(area, dim = c(dim(area), 1))

mapa_sa_emm  =  mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
                           , titulo = paste("Correlación PP EMM vs SST Niño3.4 -  ", estaciones[j], sep = "")
                           , label.escala = "", x.label = NULL, y.label = NULL, mapa = "SA"
                           , r = 1, width = 20, height = 20, salida =  "/salidas/corr/n34/", save = F, mostrar = T
                           , nombre.fig = paste("pp.corr.ens.SA_",estaciones[j], sep = ""), na.fill = -1000, lats.size = 7, letter.size = 12, margen.zero = T
                           , sig = T, variable.sig = aux2, color.vsig = "black", alpha.vsig = 0.4, altura.topo = 1500, type.sig = "point", size.point = 0.2 )



### contra pp de cada modelo (por ahora con gfdl-cm2p1)


area = w.mask
area[lons.area[[i]], lats.area[[i]]] = 2
aux.prom = apply(aux3[lons[[i]], lats[[i]],,,m], c(3,4), mean, na.rm = T)
#### Corr por modelos ####


aux.prom = apply(sst.ci[lons.n34, lats.n34,,], c(3,4), mean, na.rm = T)

mask = array(as.matrix(read.table("mascara.txt")), dim = c(dim(aux3)))

pp = aux3*mask


m = 2
j = 3

aux.corr = corr(mod = aux.prom[,j], obs = pp[,,,j,m], lon = 56, lat = 76, cf = 0.95)

aux = array(aux.corr[,,1], dim = c(dim(aux.corr[,,1]), 1))
aux.sig = array(aux.corr[,,2],c(dim(aux.corr[,,1]),1))
auxa = array(area, dim = c(dim(area), 1))

mapa_sa_cm2p1  =     mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "RdBu", revert = T, escala = seq(-1,1, by = 0.1)
                                , titulo = paste("Correlación PP CM2p1 vs SST Niño3.4 - " , estaciones[j], sep = "")
                                , label.escala = "", x.label = NULL, y.label = NULL, mapa = "SA", save = F, mostrar = T
                                , r = 1, width = 20, height = 20, salida =  "/salidas/corr/n34/", altura.topo = 1500
                                , nombre.fig = paste("corr.N34_", nombres[m],"_", estaciones[j], sep = ""), na.fill = -1000
                                , sig = T, variable.sig = aux.sig, color.vsig = "black", alpha.vsig = .4, lats.size = 7, letter.size = 12, margen.zero = T
                                , variable.cont = auxa, contour = F, nivel.vcont = 2, color.vcont = "red"
                                , type.sig = "point", size.point = .2, cb.v.w = 1, cb.v.h = 40, cb.size = 15)

  
  
colorbar1 = g_legend(mapa_sa_cm2p1)


gp1 = mapa_EMM[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp2 = mapa_cm2p1[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp3 = mapa_cmap[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))


gp4 = mapa_sa_emm + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp5 = mapa_sa_cm2p1 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp6 = mapa_sa_cmap + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))

gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6), ggplotGrob)

lay <- rbind(c(1,1,1,2,2),c(1,1,1,2,2))

p1 = grid.arrange(gpls[[1]], gpls[[4]],           
                  layout_matrix = lay
                  , left = textGrob("EMM", y = .5 
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 


p2 = grid.arrange(gpls[[2]], gpls[[5]],
                  layout_matrix = lay
                  , left = textGrob("CM2p1", y =.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 



p3 = grid.arrange(gpls[[3]], gpls[[6]],
                  layout_matrix = lay
                  , left = textGrob("CMAP", y = 0.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob(" ", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 


lay <- rbind(c(1,1,1,1,1,1,1,1,4),c(1,1,1,1,1,1,1,1,4),
             c(2,2,2,2,2,2,2,2,4),c(2,2,2,2,2,2,2,2,4), 
             c(3,3,3,3,3,3,3,3,4),c(3,3,3,3,3,3,3,3,4))



nombre_fig = paste(getwd(),"/salidas/F.Finales/", "corr_cm2p1.jpg", sep = "")

ggsave(nombre_fig,plot =grid.arrange(p1, p2, p3,ncol = 2, layout_matrix = lay, colorbar1) ,width = 35, height = 35 ,units = "cm")





#### climatologia SSb ####

source("funciones.R")

library(ggplot2)
library(gridExtra)
library(ncdf4)

#---------------------------------------------------------------#
g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}
######---------------------- MEDIAS Y DESVIOS ----------------------######

# OBSERVADO

# Temp
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

temp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12)) 

for(j in 1:12){
  for (i in 0:29){
    temp_estaciones[,,1+i,j] = temp[ , , j+12*i]
  }
}

# Estaciones
estaciones_p_a_t = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
i=1
while(i<=4){
  estaciones_p_a_t[,,,i] = apply(temp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
  i = i + 1
}

estaciones_prom_t = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom_t[,,i] = apply(estaciones_p_a_t[,,,i], c(1,2), mean)
}




T_mean_obs = list()
for(season in 1:4){
  
  T_mean_obs[[season]] = mapa_topo3(variable = estaciones_prom_t, colorbar = "Spectral", revert = T, escala = seq(0, 35, by = 2.5)
                                    , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), resta = 273, niveles = 11
                                    , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                                    , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                    , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                                    , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                                    , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
}


mask = estaciones_prom_t[,,1]  
mask[which(!is.na(mask))]=1

# sd
standar_d_t = array(NA, dim = c(length(lon2), length(lat2), 4))
for( i in 1:4 ){
  standar_d_t[,,i] = apply(estaciones_p_a_t[,,,i], c(1,2), sd)
}


T_SD_obs = list()
for(season in 1:4){
  
  T_SD_obs[[season]] = mapa_topo3(variable = standar_d_t, colorbar = "YlOrRd", revert = F,escala = seq(0, 1.5, by = 0.1)
                                  , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), niveles = 9
                                  , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                                  , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                  , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                                  , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                                  , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  

}

# Precip

aux = nc_open("/home/luciano.andrian/tesis/X190.191.242.210.56.5.48.49.nc")

lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[,,27:386]
nc_close(aux)

lon4 = lon
lat4 = lat

pp3_int = array(NA, dim = c(56, 76, 360)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:360){
  
  mod = list(x = lon4, y = lat4, z = aux2[,,i])
  
  grid = list(x=seq(min(lon4), max(lon4)-2, by = 1), y = seq(min(lat4), max(lat2)-1, by = 1))
  
  pp_aux = fields::interp.surface.grid(obj = mod, grid.list = grid)
  
  pp3_int[,,i] = pp_aux$z 
}


pp3_estaciones = array(NA, dim = c(56, 76, 30, 12))

for(j in 1:12){
  for (i in 0:29){
    pp3_estaciones[,,1+i,j] = pp3_int[1:56 , 1:76, j+12*i]
  }
}


estaciones_p_a_pp3 = array(NA, dim = c(56, 76, 30, 4))
i=1
while(i<=4){
  estaciones_p_a_pp3[,,,i] = apply(pp3_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)*30 # esta en mm/day
  i = i + 1
}

estaciones_prom_pp3 = array(NA, dim = c(56, 76, 4))

for( i in 1:4){
  estaciones_prom_pp3[,,i] = apply(estaciones_p_a_pp3[,,,i], c(1,2), mean)*mask
}

PP_mean_obs = list()
for(season in 1:4){
  
  PP_mean_obs[[season]] = mapa_topo3(variable = estaciones_prom_pp3, lon = lon2, lat = lat2, resta = 0, colorbar = "PuBuGn"
                                     , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), niveles = 9,escala = seq(0, 400, by = 50)
                                     , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                                     , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                     , cajas = F, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                                     , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                                     , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
}




## sd
standar_d_pp3 = array(NA, dim = c(56, 76, 4))
for( i in 1:4 ){
  standar_d_pp3[,,i] = apply(estaciones_p_a_pp3[,,,i], c(1,2), sd)*mask
}


PP_SD_obs = list()
for(season in 1:4){
  
  PP_SD_obs[[season]] = mapa_topo3(variable = standar_d_pp3, lon = lon2, lat = lat2, resta = 0, colorbar = "PuBuGn"
                                   , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), niveles = 9,escala = seq(0, 70, by = 5)
                                   , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                                   , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                   , cajas = F, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                                   , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                                   , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
}



mean_OBS = list(); mean_OBS[[1]] = T_mean_obs; mean_OBS[[2]] = PP_mean_obs
SD_OBS = list(); SD_OBS[[1]] = T_SD_obs; SD_OBS[[2]] = PP_SD_obs
## modelos

ruta =  "/home/luciano.andrian/tesis/ncfiles/"
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
anios = seq(from = 1982, to = 2010, by = 1)
variable = c("temp", "pp")
modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO")
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

#climatologia del EMM sin CFSv2 (6) T MAM djf
no_mod = c(1,2,3,4,5,7,8)
v.mean_no_CFSv2 = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.mean_no_CFSv2[,,,1] = apply(V.mean[,,,no_mod,1], c(1,2,3), mean, na.rm = T)
v.mean_no_CFSv2[,,,2] = apply(V.mean[,,,no_mod,2], c(1,2,3), mean, na.rm = T)

v.sd1_no_CFSv2 = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.sd1_no_CFSv2[,,,1] = apply(V.sd1[,,,no_mod,1], c(1,2,3), mean, na.rm = T)
v.sd1_no_CFSv2[,,,2] = apply(V.sd1[,,,no_mod,2], c(1,2,3), mean, na.rm = T)



#climatologia del EMM sin CM4i (7) T MAM
no_mod = c(1,2,3,4,5,6,8)
v.mean_no_CM4i = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.mean_no_CM4i[,,,1] = apply(V.mean[,,,no_mod,1], c(1,2,3), mean, na.rm = T)
v.mean_no_CM4i[,,,2] = apply(V.mean[,,,no_mod,2], c(1,2,3), mean, na.rm = T)

v.sd1_no_CM4i = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.sd1_no_CM4i[,,,1] = apply(V.sd1[,,,no_mod,1], c(1,2,3), mean, na.rm = T)
v.sd1_no_CM4i[,,,2] = apply(V.sd1[,,,no_mod,2], c(1,2,3), mean, na.rm = T)


#climatologia del EMM sin NEMO (8) PP SON
no_mod = c(1,2,3,4,5,6,7)
v.mean_no_NEMO = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.mean_no_NEMO[,,,1] = apply(V.mean[,,,no_mod,1], c(1,2,3), mean, na.rm = T)
v.mean_no_NEMO[,,,2] = apply(V.mean[,,,no_mod,2], c(1,2,3), mean, na.rm = T)

v.sd1_no_NEMO = array(data = NA, dim = c(length(lon2), length(lat2), 4, 2))
v.sd1_no_NEMO[,,,1] = apply(V.sd1[,,,no_mod,1], c(1,2,3), mean, na.rm = T)
v.sd1_no_NEMO[,,,2] = apply(V.sd1[,,,no_mod,2], c(1,2,3), mean, na.rm = T)



#CFSv2 MAM y DJF T
no_CFSv2 = list()
aux = aux2 =list()
resta_CFS_EMM = V.mean[,,,6,1] - v.mean_no_CFSv2[,,,1]
resta_CFS_OBS = V.mean[,,,6,1] - estaciones_prom_t
#resta_CFS_OBS[which(resta_CFS_OBS<(-3))] = -3

for(season in 1:4){
  
  aux[[season]] = mapa_topo3(variable = resta_CFS_EMM, colorbar = "RdBu", escala = seq(-5, 5, by = 1), revert = T
                             , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), resta = 0, niveles = 11
                             , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                             , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                             , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                             , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                             , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux2[[season]] = mapa_topo3(variable =resta_CFS_OBS, colorbar = "RdBu", escala = seq(-5, 5, by = 1), revert = T
                              , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), resta = 0, niveles = 11
                              , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                              , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                              , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                              , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                              , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  
  
}

no_CFSv2[[1]] = aux; no_CFSv2[[2]] = aux2

#CM4i (7) MAM T
no_CM4i = list()
resta_CM4i_EMM = V.mean[,,,7,1] - v.mean_no_CM4i[,,,1]
resta_CM4i_OBS = V.mean[,,,7,1] - estaciones_prom_t
resta_CM4i_OBS[which(resta_CM4i_OBS<(-5))] = -5
resta_CM4i_OBS[which(resta_CM4i_OBS>5)] = 5
for(season in 1:4){
  
  aux[[season]] = mapa_topo3(variable = resta_CM4i_EMM, colorbar = "RdBu", escala = seq(-5, 5, by = 1), revert = T
                             , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), resta = 0, niveles = 11
                             , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                             , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                             , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                             , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                             , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux2[[season]] = mapa_topo3(variable =resta_CM4i_OBS, colorbar = "RdBu", escala = seq(-5, 5, by = 1), revert = T
                              , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), resta = 0, niveles = 11
                              , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                              , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                              , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                              , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                              , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  
  
}

no_CM4i[[1]] = aux; no_CM4i[[2]] = aux2


#NEMO (8) PP SON
no_NEMO = list()
resta_NEMO_EMM = V.mean[,,,8,1] - v.mean_no_NEMO[,,,1]
resta_NEMO_OBS = V.mean[,,,8,1] - estaciones_prom_t

for(season in 1:4){
  
  aux[[season]] = mapa_topo3(variable = resta_NEMO_EMM, colorbar = "BrBG", escala = seq(-5, 5, by = 1), revert = F
                             , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), resta = 0, niveles = 11
                             , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                             , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                             , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                             , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                             , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux2[[season]] = mapa_topo3(variable =resta_NEMO_OBS, colorbar = "BrBG", escala = seq(-5, 5, by = 1), revert = F
                              , titulo =  paste(letters[season],".",titulos[[season]], sep = ""), resta = 0, niveles = 11
                              , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                              , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                              , cajas = F, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
                              , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                              , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  
  
}

no_NEMO[[1]] = aux; no_NEMO[[2]] = aux2



























