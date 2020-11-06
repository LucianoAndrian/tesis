
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


mapa_topo3(variable = estaciones_prom_t, lon = lon2, lat = lat2, resta = 273, colorbar = "Spectral", niveles = 11
           , mapa = "SA", na.fill = -10000, r = 4, estaciones = T, altura.topo = 1500, width = 20, height = 20, label.escala = "ºC"
           , escala = seq(0, 35, by = 2.5), revert = T, titulo = "Temperatura - CPC", nombre.fig = "temp_cpc", salida = "/salidas/observado/") 

titulos = list()
titulos[[1]] = "a)              MAM              "
titulos[[2]] = "b)              JJA              "
titulos[[3]] = "c)              SON              "
titulos[[4]] = "d)              DJF              "

T_mean_obs = list()
for(season in 1:4){
  
  T_mean_obs[[season]] = mapa_topo3(variable = estaciones_prom_t, colorbar = "Spectral", revert = T, escala = seq(0, 35, by = 2.5)
                 , titulo =  titulos[[season]], resta = 273, niveles = 11
                 , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                 , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                 , cajas = T, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
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
                                    , titulo =  titulos[[season]], niveles = 9
                                    , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                                    , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                    , cajas = T, lon = lon2, lat = lat2, estacion = season, mostrar = T, save = F,  cb.v.w = 1
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
  
  pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
  
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
             , titulo =  titulos[[season]], niveles = 9,escala = seq(0, 400, by = 50)
             , label.escala = "ºC", mapa = "SA", width = 20, height = 20
             , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
             , cajas = T, estacion = season, mostrar = T, save = F,  cb.v.w = 1
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
                                     , titulo =  titulos[[season]], niveles = 9,escala = seq(0, 70, by = 5)
                                     , label.escala = "ºC", mapa = "SA", width = 20, height = 20
                                     , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                     , cajas = T, estacion = season, mostrar = T, save = F,  cb.v.w = 1
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
                                  , titulo =  titulos[[season]], niveles = 9, escala = escala[[v]]
                                  , label.escala = lab.escala[v], mapa = "SA", width = 20, height = 20
                                  , na.fill = 0, r = 4, estaciones = T, altura.topo = 1500
                                  , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1 #?¿?¿
                                  , cajas = T, estacion = season, mostrar = T, save = F,  cb.v.w = 1
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
                                    , titulo =  titulos[[season]], niveles = niveles[v], escala = escala[[v]], revert = revert[v]
                                    , label.escala = lab.escala[v], mapa = "SA", width = 20, height = 20
                                    , na.fill = -1000, r = 4, estaciones = T, altura.topo = 1500
                                    , sig = T, variable.sig = aux2, color.vsig = "white", alpha.vsig = 1 #?¿?¿
                                    , cajas = T, estacion = season, mostrar = T, save = F,  cb.v.w = 1
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
                      , top = textGrob("1.                                                                                                                                                        " 
                                       , gp=gpar(fontsize=16,font=8))) 
    
    
    p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                      layout_matrix = lay
                      , left = textGrob("                          EMM                       "
                                        ,rot = 90, gp=gpar(fontsize=16,font=8))
                      , top = textGrob("2.                                                                                                                                                       " 
                                       , gp=gpar(fontsize=16,font=8)))
    
    
    
    p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                      layout_matrix = lay
                      , left = textGrob(titulo_obs[v]
                                        ,rot = 90, gp=gpar(fontsize=16,font=8))
                      , top = textGrob("1.                                                                                                                                                       " 
                                       , gp=gpar(fontsize=16,font=8))) 
    
    p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                      layout_matrix = lay
                      ,  left = textGrob("                          EMM                      "
                                         ,rot = 90, gp=gpar(fontsize=16,font=8))
                      , top = textGrob("2.                                                                                                                                                        " 
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


letras = c(as.character("\u03b1"), as.character("\u03B2"), as.character("\u194"), as.character("\u03B5"))

#-------------------------------------------------------------------#

sin_m = function(m, season, v){
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
  aux[[1]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[6]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[6-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo =  "a)              SSα              " #que trucazo
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = T, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  aux[[2]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[7]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[7-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo =  "b)              SSβ              "
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = T, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux[[4]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[9]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[9-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo =  "d)              SSε              "
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = T, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux[[3]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[8-5]], colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                        , titulo = "c)              SSƔ              "
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F, cb.v.w = 0.7, cb.v.h = 13, cb.size = 7
                        , lats.size = 7,letter.size = 12, margen.zero = T, cajas = T, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  return(aux)
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
                        , titulo =  "a)              SSα              "
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F,  cb.v.w = 1
                        , cb.v.h = 32, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    bias = mapa_topo3(variable =  EMM[[v]][[7]]*mask_arr, variable.sig =  EMM[[v.sig]][[7-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                      , titulo = "b)              SSβ              "
                      , label.escala = "", mapa = "SA", width = 20, height = 20
                      , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                      , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F
                      ,  cb.v.w = 1, cb.v.h = 30, cb.size = 14, lats.size = 7, letter.size = 12, color.vcont = "black"
                      , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    noise = mapa_topo3(variable =  EMM[[v]][[9]]*mask_arr, variable.sig =  EMM[[v.sig]][[9-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                       , titulo =  "d)              SSε              "
                       , label.escala = "", mapa = "SA", width = 20, height = 20
                       , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                       , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F
                       ,  cb.v.w = 1, cb.v.h = 30, cb.size = 14, lats.size = 7, letter.size = 12, color.vcont = "black"
                       , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    structural = mapa_topo3(variable = EMM[[v]][[8]]*mask_arr, variable.sig =  EMM[[v.sig]][[8-5]], colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                            , titulo = "c)              SSƔ              "
                            , label.escala = "", mapa = "SA", width = 20, height = 20
                            , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                            , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F, cb.v.w = 1
                            , cb.v.h = 32, cb.size = 10, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                            , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
    
    
    
    
    # Panel 2 EMM sin CFSv2
    # nombres2 = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "ECCC-GEM-NEMO") 
    no_mod1 = sin_m(6, season,v)
    
    no_mod2 = sin_m(2, season,v)
    
    no_mod3 = sin_m(7, season, v)
    
    

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
                  , top = textGrob("1.                                                                                                                                                        " 
                                   , gp=gpar(fontsize=16,font=8))) 


p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                  layout_matrix = lay
                  , left = textGrob("                          EMM sin CFSv2                      "
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("2.                                                                                                                                                       " 
                                   , gp=gpar(fontsize=16,font=8))) 


p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                  layout_matrix = lay
                  , left = textGrob("                          EMM sin CM2p1                      "
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("3.                                                                                                                                                       " 
                                   , gp=gpar(fontsize=16,font=8))) 

p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                  layout_matrix = lay
                  ,  left = textGrob("                          EMM sin CM4i                      "
                                     ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("4.                                                                                                                                                        " 
                                   , gp=gpar(fontsize=16,font=8))) 



lay <- rbind(c(1,1,1,1,1,1,1,1,5),c(1,1,1,1,1,1,1,1,5),
             c(2,2,2,2,2,2,2,2,5),c(2,2,2,2,2,2,2,2,5), 
             c(3,3,3,3,3,3,3,3,6),c(3,3,3,3,3,3,3,3,6), 
             c(4,4,4,4,4,4,4,4,6),c(4,4,4,4,4,4,4,4,6))

if(v ==1){
  nombre = "T_"
} else {
  nombre = "PP_"
}


nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, seasons[season], ".anova", ".jpg", sep = "")

ggsave(nombre_fig,plot =grid.arrange(p1, p2, p3, p4, ncol = 2, layout_matrix = lay, colorbar1, colorbar2) ,width = 30, height = 35 ,units = "cm")

  }
}






#######------------------- Predictibilidad --------------------------########
# mismos modelos (+ GEM-NEMO en una estacion para T y PP)
sin_m_pred = function(m, v, pred){
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
                        , titulo =  "a)                  MAM                  "
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , nombre.fig = "pred_temp", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 1
                        , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = 1, mostrar = T, save = F,  cb.v.w = 1
                        , cb.v.h = 35, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  aux[[2]] = mapa_topo3(variable = pred[[m]][[v]]*mask_arr, variable.sig = pred[[m]][[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                        , titulo =  "b)                  JJA                  "
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , nombre.fig = "pred_temp", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                        , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = 2, mostrar = T, save = F
                        , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  aux[[3]] = mapa_topo3(variable = pred[[m]][[v]]*mask_arr, variable.sig = pred[[m]][[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                        , titulo =  "c)                  SON                  "
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , nombre.fig = "pred_temp", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                        , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = 3, mostrar = T, save = F
                        , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  aux[[4]] = mapa_topo3(variable = pred[[m]][[v]]*mask_arr, variable.sig = pred[[m]][[v.sig]], colorbar = colorbars[[v]], revert = F, escala =escala_pred[[v]]
                        , titulo =  "d)                  DJF                  "
                        , label.escala = "", mapa = "SA", width = 20, height = 20
                        , nombre.fig = "pred_temp", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                        , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = 4, mostrar = T, save = F
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
                   , titulo =  "a)                  MAM                  "
                   , label.escala = "", mapa = "SA", width = 20, height = 20
                   , nombre.fig = "pred_temp", na.fill = -1000
                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 1
                   , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = 1, mostrar = T, save = F,  cb.v.w = 1
                   , cb.v.h = 35, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                   , nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  JJA = mapa_topo3(variable = pred[[v]]*mask_arr, variable.sig = pred[[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                   , titulo =  "b)                  JJA                  "
                   , label.escala = "", mapa = "SA", width = 20, height = 20
                   , nombre.fig = "pred_temp", na.fill = -1000
                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                   , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = 2, mostrar = T, save = F
                   , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                   , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  SON = mapa_topo3(variable = pred[[v]]*mask_arr, variable.sig = pred[[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                   , titulo =  "c)                  SON                  "
                   , label.escala = "", mapa = "SA", width = 20, height = 20
                   , nombre.fig = "pred_temp", na.fill = -1000
                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                   , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = 3, mostrar = T, save = F
                   , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                   , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  DJF = mapa_topo3(variable = pred[[v]]*mask_arr, variable.sig = pred[[v.sig]], colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                   , titulo =  "d)                  DJF                  "
                   , label.escala = "", mapa = "SA", width = 20, height = 20
                   , nombre.fig = "pred_temp", na.fill = -1000
                   , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500,  size.point = 0.2
                   , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = 4, mostrar = T, save = F
                   , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                   , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
  
  no_mod1 = sin_m_pred(6, v, pred_wo)
  
  no_mod2 = sin_m_pred(2, v, pred_wo)
  
  no_mod3 = sin_m_pred(7, v, pred_wo)
  
  
  
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
                    , top = textGrob("1.                                                                                                                                                        " 
                               , gp=gpar(fontsize=16,font=8))) 
  
  
  p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                    layout_matrix = lay
                    , left = textGrob("                          EMM sin CFSv2                      "
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob("2.                                                                                                                                                       " 
                                     , gp=gpar(fontsize=16,font=8))) 
                    
  
  p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                    layout_matrix = lay
                    , left = textGrob("                          EMM sin CM2p1                      "
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob("3.                                                                                                                                                       " 
                                     , gp=gpar(fontsize=16,font=8))) 
  
  p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                    layout_matrix = lay
                    ,  left = textGrob("                          EMM sin CM4i                      "
                                        ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob("4.                                                                                                                                                        " 
                                     , gp=gpar(fontsize=16,font=8))) 
  
  
  
  lay <- rbind(c(1,1,1,1,1,1,1,1,5),c(1,1,1,1,1,1,1,1,5),
               c(2,2,2,2,2,2,2,2,5),c(2,2,2,2,2,2,2,2,5), 
               c(3,3,3,3,3,3,3,3,5),c(3,3,3,3,3,3,3,3,5), 
               c(4,4,4,4,4,4,4,4,5),c(4,4,4,4,4,4,4,4,5))
  
  if(v ==1){
    nombre = "T_"
  } else {
    nombre = "PP_"
  }
  
  
  nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, ".PRED", ".jpg", sep = "")
  
  ggsave(nombre_fig,plot =grid.arrange(p1, p2, p3, p4, ncol = 2, layout_matrix = lay, colorbar1) ,width = 30, height = 35 ,units = "cm")

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


titulo_acc1_2 = c("a)                        MAM                        ",
                  "b)                        JJA                        ",
                  "c)                        SON                        ",
                  "d)                        DJF                        ")
l = c("a)               ","b)              ","c)              ","d)             ")

nombres2 = c("CCSM4", "CM2p1", "FLOR-A06", "FLOR-B01", "GEOS5", "CFSv2", "CanCM4i", "GEM-NEMO") 
ACC_EMM = list(); ACC_wo_mod = list(); ACC_teo = list()


for(v in 1:2){
  for(acc in 1:3){
    for(season in 1:4){
      
      if(acc == 1){
        
        if(v == 1){
          v1 = 6
        } else if(v == 2){
          v1 = 7
        }
        ### ACC teorico
        ACC_teo[[season]] =  mapa_topo3(variable = resultados[[v1]]*mask_arr, variable.sig = resultados[[v1]]*mask_arr, v.sig = resultados[[3]]
                                        , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                                        , titulo =  titulo_acc1_2[season], label.escala = "", mapa = "SA", width = 20, height = 20
                                        , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5
                                        , r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                                        , lon = lon2, lat = lat2, type.sig = "point2", estacion = season
                                        , mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 35, cb.size = 14
                                        , lats.size = 7, letter.size = 12, cajas = T
                                        , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
        
      } else if(acc == 2){
        
       ACC_EMM[[season]] =  mapa_topo3(variable = resultados[[v]]*mask_arr, variable.sig = resultados[[v]]*mask_arr, v.sig = resultados[[3]]
                                            , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                                            , titulo =  titulo_acc1_2[season], label.escala = "", mapa = "SA", width = 20, height = 20
                                            , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5
                                            , r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                                            , lon = lon2, lat = lat2, type.sig = "point2", estacion = season
                                            , mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 35, cb.size = 14
                                            , lats.size = 7, letter.size = 12, cajas = T
                                            , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
       
        
      } else if( acc == 3){
        
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
                                   , titulo =  paste(l[season], "EMM sin ", nombres2[m],"              ", sep = ""), label.escala = ""
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
                    , top = textGrob("1.", x = 0
                                     , gp=gpar(fontsize=16,font=8))) 
  
  
  p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                    layout_matrix = lay
                    , left = textGrob("ACC Observado", y =.5
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob("2.", x = 0 
                                     , gp=gpar(fontsize=16,font=8))) 
  
  
  
  p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                    layout_matrix = lay
                    , left = textGrob("ACC", y = 0.5
                                      ,rot = 90, gp=gpar(fontsize=16,font=8))
                    , top = textGrob("3.", x = 0 
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

titulo_acc1_2 = c("a)                            MAM                            ",
                  "b)                            JJA                            ",
                  "c)                            SON                            ",
                  "d)                            DJF                            ")

titulo_acc2 = c("a)                                                              ",
                 "b)                                                              ",
                 "c)                                                              ",
                 "d)                                                              ")
 

lats = list()
lats[[1]] = seq(which(lat2 == -29), which(lat2 == -17), by = 1); lats[[2]] = seq(which(lat2 == -39), which(lat2 == -25), by = 1)
lats[[3]] = seq(which(lat2 == -15), which(lat2 == 2), by = 1); lats[[4]] = seq(which(lat2 == -55), which(lat2 == -37), by = 1)
lats[[5]] = seq(which(lat2 == -13), which(lat2 == 2), by = 1)

lons = list()
lons[[1]] = seq(which(lon2 == 303), which(lon2 == 315), by = 1); lons[[2]] = seq(which(lon2 == 296), which(lon2 == 306), by = 1)
lons[[3]] = seq(which(lon2 == 311), which(lon2 == 325), by = 1); lons[[4]] = seq(which(lon2 == 287), which(lon2 == 294), by = 1)
lons[[5]] = seq(which(lon2 == 291), which(lon2 == 304), by = 1)

figs = list()

for(c in 1:5){
  
  figs[[c]] = list()
  area = array(NA, dim = c(56,76))
  
  for(i in 1:4){
    
    area[lons[[c]], lats[[c]]] = 1
    
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
    
    
    g  = ggplot(data = data3, mapping = aes(x = X, y = Y)) + theme_minimal() +
      geom_point(data = data3,aes(colour = var, shape = as.factor(var)),show.legend = T, size = 1, stroke = 1) +
      scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1), name = "ACC Teorico") +
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1), name = "ACC Observado") +
      scale_color_manual(values = c("tomato3","steelblue3"), breaks = c(1,2), name = "", labels = c("Temp", "Precip")) +
      geom_hline(yintercept = 0.31)+
      scale_shape_discrete(guide = F) +
      theme(axis.text.y   = element_text(size = 10), axis.text.x   = element_text(size = 10), axis.title.y  = element_text(size = 10),
            axis.title.x  = element_text(size = 10),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
            panel.ontop = F,
            plot.title = element_text(hjust = 0.5), legend.position = "bottom") 
    
    if(c == 1){
      g = g + ggtitle(titulo_acc1_2[i])
    } else {
      g = g + ggtitle(titulo_acc2[i])
    }
    
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

gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                    gp11, gp12, gp13, gp14, gp15, gp16, gp17,gp18,
                    gp19, gp20), ggplotGrob)

lay <- rbind(c(1,1,2,2,3,3,4,4),c(1,1,2,2,3,3,4,4))

p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],          
                  layout_matrix = lay
                  , left = textGrob("N-SESA", y = .5 
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("1.", x = 0
                                   , gp=gpar(fontsize=16,font=8))) 


p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                  layout_matrix = lay
                  , left = textGrob("S-SESA", y =.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("2.", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 



p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                  layout_matrix = lay
                  , left = textGrob("NeB", y = 0.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("3.", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 

p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                  layout_matrix = lay
                  , left = textGrob("Patagonia", y = 0.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("4.", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 

p5 = grid.arrange(gpls[[17]], gpls[[18]], gpls[[19]], gpls[[20]],
                  layout_matrix = lay
                  , left = textGrob("Am", y = 0.5
                                    ,rot = 90, gp=gpar(fontsize=16,font=8))
                  , top = textGrob("5.", x = 0 
                                   , gp=gpar(fontsize=16,font=8))) 



lay <- rbind(c(1,1,1,1,1,1,1,1),c(1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,2,2,2),c(2,2,2,2,2,2,2,2), 
             c(3,3,3,3,3,3,3,3),c(3,3,3,3,3,3,3,3),
             c(4,4,4,4,4,4,4,4),c(4,4,4,4,4,4,4,4),
             c(5,5,5,5,5,5,5,5),c(5,5,5,5,5,5,5,5))


nombre_fig = paste(getwd(),"/salidas/F.Finales/", "ACC_vs", ".jpg", sep = "")

ggsave(nombre_fig,plot =grid.arrange(p1, p2, p3, p4, p5, ncol = 2, layout_matrix = lay) ,width = 40, height = 35 ,units = "cm")




