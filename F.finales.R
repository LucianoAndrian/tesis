
source("funciones.R")

##### MAM #####
#---- ANOVA ----#
# EMM
ss_temp = anova_fun(variable = "temp", ensemble_total = "si") 
sig_temp = test_cos(ss_temp, ensemble_total = "si", nomodel_selec = "no", no_model = "no")

# signal
signal = mapa_topo3(variable = ss_temp[[6]]*mask_arr, variable.sig = sig_temp[[6-5]], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
           , titulo =  paste("SS", letras[6-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = paste("temp_ss", letras[6-5], sep = ""), na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
           , lon = lon2, lat = lat2, type.sig = "point", estacion = 1, mostrar = T, save = F,  cb.v.w = 2, cb.v.h = 40, cb.size = 14, lats.size = 8, letter.size = 15)



bias = mapa_topo3(variable = ss_temp[[7]]*mask_arr, variable.sig = sig_temp[[7-5]], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
                    , titulo =  paste("SS", letras[7-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                    , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = paste("temp_ss", letras[6-5], sep = ""), na.fill = -1000
                    , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                    , lon = lon2, lat = lat2, type.sig = "point", estacion = 1, mostrar = T, save = F, lats.size = 8, letter.size = 15)

noise = mapa_topo3(variable = ss_temp[[9]]*mask_arr, variable.sig = sig_temp[[9-5]], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
                    , titulo =  paste("SS", letras[9-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
                    , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = paste("temp_ss", letras[6-5], sep = ""), na.fill = -1000
                    , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
                    , lon = lon2, lat = lat2, type.sig = "point", estacion = 1, mostrar = T, save = F, lats.size = 8, letter.size = 15)

# structural. poner otra escala!!!!
structural = mapa_topo3(variable = ss_temp[[8]]*mask_arr, variable.sig = sig_temp[[8-5]], colorbar = "Reds", revert = F, escala = seq(0, 0.1, by = 0.01)
           , titulo = paste("SS", letras[8-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/F.Finales/", nombre.fig = paste("esc_temp_ss", letras[8-5], sep = ""), na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.8
           , lon = lon2, lat = lat2, type.sig = "point", estacion = 1, mostrar = T, save = F, cb.v.w = 0.8, cb.v.h = 14, cb.size = 8, lats.size = 8,letter.size = 15)





library(plyr)
library(foreach)
library(ggplot2)
library(gridExtra)


#update: added 7th part to the layer matrix for the legend

lay <- rbind(c(1,1,2,2,5,5,6,6,17),c(1,1,2,2,5,5,6,6,17),
             c(3,3,4,4,7,7,8,8,17), c(3,3,4,4,7,7,8,8,17),
             c(9,9,10,10,13,13,14,14,17), c(9,9,10,10,13,13,14,14,17),
             c(11,11,12,12,15,15,16,16,17), c(11,11,12,12,15,15,16,16,17))

# grab legend from one plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

aleg <- g_legend(signal)
gp1 <- signal+ theme(legend.position = "none")
gp2 <- structural+ theme(legend.position = "right")
gp3 = bias + theme(legend.position = "none")
gp4 = noise + theme(legend.position = "none")

gp5 = gp1; gp6 = gp3; gp7 = gp2
gp8 = gp4

gp9 = gp1; gp10 = gp3; gp11 = gp2
gp12 = gp4

gp13 = gp1; gp14 = gp3; gp15 = gp2
gp16 = gp4

  



gpls <- lapply(list(gp1,gp2,gp3, gp4, gp5, gp6, gp7, gp8, gp9, gp10,
                    gp11, gp12, gp13, gp14, gp15, gp16), ggplotGrob )

ggsave("probando.jpg",plot = grid.arrange(gpls[[1]], gpls[[3]], gpls[[2]], gpls[[4]],
                                          gpls[[5]], gpls[[6]], gpls[[7]], gpls[[8]],
                                          gpls[[9]], gpls[[10]], gpls[[11]], gpls[[12]],
                                          gpls[[13]], gpls[[14]], gpls[[15]], gpls[[16]],
                                          layout_matrix = lay, aleg, top = "funca?"),width = 35, height = 35 ,units = "cm")


