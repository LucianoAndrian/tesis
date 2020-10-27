
source("funciones.R")

library(ggplot2)
library(gridExtra)

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

g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}

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
                        , titulo =  paste("SS", letras[6-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                        , salida = "/salidas/ensemble/anova/sin_mod/sin_mod1/", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = T, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  
  aux[[2]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[7]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[7-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo =  paste("SS", letras[7-5], by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                        , salida = "/salidas/ensemble/anova/sin_mod/sin_mod1/", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = T, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux[[4]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[9]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[9-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo =  paste("SS", letras[9-5], by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                        , salida = "/salidas/ensemble/anova/sin_mod/sin_mod1/", na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                        , lats.size = 7, letter.size = 12, cajas = T, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  aux[[3]] = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr, variable.sig = EMM_wo[[v.sig]][[m]][[8-5]], colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                        , titulo = paste("SS", letras[8-5], by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                        , lon = lon2, lat = lat2, type.sig = "point",estacion = season, mostrar = T, save = F, cb.v.w = 0.7, cb.v.h = 13, cb.size = 7
                        , lats.size = 7,letter.size = 12, margen.zero = T, cajas = T, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
  
  return(aux)
}


#---- ANOVA ----#
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
    
    signal = mapa_topo3(variable = EMM[[v]][[6]]*mask_arr, variable.sig = EMM[[v.sig]][[6-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo =  paste("SS", letras[6-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                        , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                        , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F,  cb.v.w = 1
                        , cb.v.h = 35, cb.size = 10, lats.size = 7, letter.size = 12, margen.zero = T, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    bias = mapa_topo3(variable =  EMM[[v]][[7]]*mask_arr, variable.sig =  EMM[[v.sig]][[7-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                      , titulo =  paste("SS", letras[7-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                      , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                      , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F
                      ,  cb.v.w = 1, cb.v.h = 30, cb.size = 14, lats.size = 7, letter.size = 12, color.vcont = "black"
                      , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    noise = mapa_topo3(variable =  EMM[[v]][[9]]*mask_arr, variable.sig =  EMM[[v.sig]][[9-5]], colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                       , titulo =  paste("SS", letras[9-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                       , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                       , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F
                       ,  cb.v.w = 1, cb.v.h = 30, cb.size = 14, lats.size = 7, letter.size = 12, color.vcont = "black"
                       , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    structural = mapa_topo3(variable = EMM[[v]][[8]]*mask_arr, variable.sig =  EMM[[v.sig]][[8-5]], colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                            , titulo = paste("SS", letras[8-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                            , na.fill = -1000, sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.2
                            , cajas = T, lon = lon2, lat = lat2, type.sig = "point", estacion = season, mostrar = T, save = F
                            , cb.v.w = 0.7, cb.v.h = 14, cb.size = 7, lats.size = 7,letter.size = 12, margen.zero = T, color.vcont = "black"
                            , nivel.vcont = c(2,2.01, 2.02, 2.03, 2.02))
    
    
    
    
    # Panel 2 EMM sin CFSv2
    # nombres2 = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "ECCC-GEM-NEMO") 
    no_mod1 = sin_m(6, season,v)
    
    no_mod2 = sin_m(2, season,v)
    
    no_mod3 = sin_m(7, season, v)
    
    

colorbar1 <- g_legend(signal)

# panel 1 - emm
gp1 <- signal + theme(legend.position = "none", plot.margin = unit(c(0,.5,.2,1), "lines"))
gp2 <- bias + theme(legend.position = "none", plot.margin = unit(c(0,1,.2,.5), "lines")) 
gp3 = structural + theme(legend.position = "right", plot.margin = unit(c(0,.5,1,1), "lines"))
gp4 = noise + theme(legend.position = "none", plot.margin = unit(c(0,1,1,.5), "lines"))

#panel 2 - no_mod1
gp5 = no_mod1[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.5,.2,1), "lines"))
gp6 = no_mod1[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,1,.2,.5), "lines"))
gp7 = no_mod1[[3]] + theme(legend.position = "right", plot.margin = unit(c(0,.5,1,1), "lines"))
gp8 = no_mod1[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,1,1,.5), "lines"))

#panel 3 - no_mod2
gp9 = no_mod2[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.5,.2,1), "lines"))
gp10 = no_mod2[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,1,.2,.5), "lines"))
gp11 = no_mod2[[3]] + theme(legend.position = "right", plot.margin = unit(c(0,.5,1,1), "lines"))
gp12 = no_mod2[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,1,1,.5), "lines"))


#panel 4 - no_mod3
gp13 = no_mod3[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.5,.2,1), "lines"))
gp14 = no_mod3[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,1,.2,.5), "lines"))
gp15 = no_mod3[[3]] + theme(legend.position = "right", plot.margin = unit(c(0,.5,1,1), "lines"))
gp16 = no_mod3[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,1,1,.5), "lines"))


gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                    gp11, gp12, gp13, gp14, gp15, gp16), ggplotGrob )

lay <- rbind(c(1,1,2,2),c(1,1,2,2),
             c(3,3,4,4),c(3,3,4,4))

p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],
                  layout_matrix = lay, top = textGrob("EMM", gp=gpar(fontsize=20,font=8))) 

p2 = grid.arrange(gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                  layout_matrix = lay, top = textGrob("EMM - sin CFSv2", gp=gpar(fontsize=20,font=8))) 

p3 = grid.arrange(gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                  layout_matrix = lay, top = textGrob("EMM - sin CM2p1", gp=gpar(fontsize=20,font=8))) 

p4 = grid.arrange(gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                  layout_matrix = lay, top = textGrob("EMM - CM4i", gp=gpar(fontsize=20,font=8))) 


lay <- rbind(c(1,1,1,1,2,2,2,2,5),c(1,1,1,1,2,2,2,2,5),
             c(3,3,3,3,4,4,4,4,5),c(3,3,3,3,4,4,4,4,5))

if(v ==1){
  nombre = "T_"
} else {
  nombre = "PP_"
}


nombre_fig = paste(getwd(),"/salidas/F.Finales/", nombre, seasons[season], ".anova", ".jpg", sep = "")

ggsave(nombre_fig,plot =grid.arrange(p1, p2, p3, p4, ncol = 2, layout_matrix = lay, colorbar1) ,width = 30, height = 35 ,units = "cm")

  }
}






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
lats[[5]] =  seq(which(lat2 == -15), which(lat2 == 5), by = 1); lats[[6]] =  seq(which(lat2 == -15), which(lat2 == 5), by = 1)

lons = list()
lons[[1]] = seq(which(lon2 == 303), which(lon2 == 315), by = 1); lons[[2]] = seq(which(lon2 == 296), which(lon2 == 306), by = 1)
lons[[3]] = seq(which(lon2 == 311), which(lon2 == 325), by = 1); lons[[4]] = seq(which(lon2 == 287), which(lon2 == 294), by = 1)
lons[[5]] = seq(which(lon2 == 299), which(lon2 == 311), by = 1); lons[[6]] = seq(which(lon2 == 285), which(lon2 == 298), by = 1)

area = array(1, dim = c(56,76))

for(i in 1:6){
  if(i == 1){
    area[lons[[i]], lats[[i]]] = 2
  } else if(i<5){
    area[lons[[i]], lats[[i]]] = 2.01
  } else if(i==5){
    area[lons[[i]], lats[[i]]] = 2.02
  } else if(i == 6)
    area[lons[[i]], lats[[i]]] = 2.03
  
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
  geom_text(x = 305, y = -5, label = "CentroB", aes(angle = 0), size = 5) +
  geom_text(x = 292, y = -5, label = "OA", aes(angle = 0), size = 5) +
  stat_contour(data = data2, aes(x = lon, y = lat, z = cont), color = "black", size = .3, breaks = c(2,2.01,2.02,2.03)) +
  scale_x_longitude(breaks = breaks.lon, name = NULL, limits = c(270,335))+
  scale_y_latitude(breaks = breaks.lat, name = NULL, limits = c(-60,20))+
  theme(axis.text.y   = element_text(size = 15), axis.text.x   = element_text(size = 15), axis.title.y  = element_text(size = 15),
        axis.title.x  = element_text(size = 15), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
        panel.ontop = TRUE, 
        plot.title = element_text(hjust = 0.5, size = 15)) + geom_hline(yintercept = 0, color = "black") 

ggsave(paste("/home/luciano.andrian/tesis/salidas/F.Finales/", "fig", ".jpg", sep = ""), plot = g, width = 20, height = 20, units = "cm")


