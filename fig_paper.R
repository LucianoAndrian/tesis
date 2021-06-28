# figuras final paper.
source("funciones.R")


library(ggplot2)
library(gridExtra)
library(ncdf4)
library(ggpubr)

#------------------------------------------------------------------------------#
g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}

#--------------------------- ANOVA --------------------------------------------#
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}


letras = c(as.character("\u03b1"), as.character("\u03B2"), as.character("\u194")
           , as.character("\u03B5"))

titulos = list()
titulos[[1]] = "α"
titulos[[2]] = "β"
titulos[[3]] = "ε"
titulos[[4]] = "Ɣ"



#------------------------------------------------------------------------------#
# ANOVA

# MME 
ss_temp = anova_fun(variable = "temp", ensemble_total = "si") 
sig_temp_emm = test_cos(ss_temp, ensemble_total = "si", nomodel_selec = "no"
                        , no_model = "no")

ss_pp = anova_fun(variable = "pp", ensemble_total = "si")
sig_pp_emm = test_cos(ss_pp, ensemble_total = "si", nomodel_selec = "no"
                      , no_model = "no")

EMM = list()
EMM[[1]] = ss_temp; EMM[[2]] = sig_temp_emm
EMM[[3]] = ss_pp; EMM[[4]] = sig_pp_emm


# MME without models
# ss_T = anova_fun(variable = "temp", ensemble_total = "no")    
# ss_PP = anova_fun(variable = "pp", ensemble_total = "no")


load("ss_T.RData")
sig_temp = list()
for(i in 1:8){
  sig_temp[[i]] = test_cos(ss_T[[i]], ensemble_total = "no"
                           , nomodel_selec = "no", no_model = i )
}

load("ss_PP.RData")
sig_pp = list()
for(i in 1:8){
  sig_pp[[i]] = test_cos(ss_PP[[i]], ensemble_total = "no"
                         , nomodel_selec = "no", no_model = i )
}


EMM_wo = list()
EMM_wo[[1]] = ss_T; EMM_wo[[2]] = sig_temp
EMM_wo[[3]] = ss_PP; EMM_wo[[4]] = sig_pp

#figures ANOVA

colorbars = list()
colorbars[[1]] = "YlOrRd"; colorbars[[3]] = "PuBuGn"

colorbars_gamma = list()
colorbars_gamma[[1]] = "RdPu"; colorbars_gamma[[3]] = "BuPu"

seasons = c("MAM", "JJA", "SON", "DJF")


# MME

signal = bias = noise = structural = list()

for(v in c(1,3)){

    if(v == 1){
      v.sig = 2
    } else if(v == 3){
      v.sig = 4
    } else {
      print("ta mal!")
      break
    }
    
  for(season in 1:4){
    
    signal[[season]] = mapa_topo3(variable = EMM[[v]][[6]]*mask_arr, variable.sig = EMM[[v.sig]][[6-5]]
                        , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                        , titulo = titulos[[1]]
                        , label.escala = NULL, mapa = "SA"
                        , width = 20, height = 20, title.size = 13, na.fill = -1000
                        , sig = T, color.vsig = "black", alpha.vsig = 1
                        , r = 4, estaciones = T, altura.topo = 2000, size.point = .01
                        , cajas = F, lon = lon2, lat = lat2
                        , type.sig = "point", estacion = season
                        , mostrar = T, save = F,  cb.v.w = 0.5
                        , cb.v.h = 9.5, cb.size = 6, lats.size = 4
                        , letter.size = 9, margen.zero = F, color.vcont = "black"
                        , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    bias[[season]] = mapa_topo3(variable =  EMM[[v]][[7]]*mask_arr, variable.sig =  EMM[[v.sig]][[7-5]]
                      , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                      , titulo = titulos[[2]]
                      , label.escala = NULL, mapa = "SA"
                      , width = 20, height = 20,title.size = 13, na.fill = -1000
                      , sig = T, color.vsig = "black", alpha.vsig = 1
                      , r = 4, estaciones = T, altura.topo = 2000, size.point = .01
                      , cajas = F, lon = lon2, lat = lat2
                      , type.sig = "point", estacion = season
                      , mostrar = T, save = F
                      ,  cb.v.w = 0.5, cb.v.h = 30, cb.size = 14
                      , lats.size = 4, letter.size = 8, color.vcont = "black"
                      , nivel.vcont = c(2,2.01, 2.02, 2.03),margen.zero = F)
    
    
    noise[[season]] = mapa_topo3(variable =  EMM[[v]][[9]]*mask_arr, variable.sig =  EMM[[v.sig]][[9-5]]
                       , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
                       , titulo =  titulos[[3]]
                       , label.escala = NULL, mapa = "SA"
                       , width = 20, height = 20,title.size = 13, na.fill = -1000
                       , sig = T, color.vsig = "black", alpha.vsig = 1
                       , r = 4, estaciones = T, altura.topo = 2000, size.point = .01
                       , cajas = F, lon = lon2, lat = lat2
                       , type.sig = "point", estacion = season
                       , mostrar = T, save = F
                       ,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
                       , lats.size = 4, letter.size = 8, color.vcont = "black"
                       , nivel.vcont = c(2,2.01, 2.02, 2.03),margen.zero = F)
    
    structural[[season]] = mapa_topo3(variable = EMM[[v]][[8]]*mask_arr, variable.sig =  EMM[[v.sig]][[8-5]]
                            , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                            , titulo = titulos[[4]]
                            , label.escala = NULL, mapa = "SA"
                            , width = 20, height = 20,title.size = 13, na.fill = -1000
                            , sig = T, color.vsig = "black", alpha.vsig = 0.1
                            , r = 4, estaciones = T, altura.topo = 2000, size.point = 0.0001
                            , cajas = F, lon = lon2, lat = lat2
                            , type.sig = "point", estacion = season
                            , mostrar = T, save = F
                            ,  cb.v.w = 0.5, cb.v.h = 9.5, cb.size = 6
                            , lats.size = 4, letter.size = 8, color.vcont = "black"
                            , nivel.vcont = c(2,2.01, 2.02, 2.03), margen.zero = F)
    
  }
  
  # GRID
  colorbar1 <- g_legend(signal[[1]])
  colorbar2 <- g_legend(structural[[1]])
  
  gp1 = signal[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) #tag = "a.")
  gp2 = bias[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) #tag = "b.")
  gp3 = structural[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) #tag = "c.")
  gp4 = noise[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) #tag = "d.")
  
  gp5 = signal[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "") #tag = "e.")
  gp6 = bias[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "f.")
  gp7 = structural[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "g.")
  gp8 = noise[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "h.")
  
  gp9 = signal[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "i.")
  gp10 = bias[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "j.")
  gp11 = structural[[3]] +  theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "k.")
  gp12 = noise[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "l.")
  
  gp13 = signal[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "m.")
  gp14 = bias[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "n.")
  gp15 = structural[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag = "o.")
  gp16 = noise[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")#tag =  "p.")
    
  
  p1 = ggarrange(gp1, gp2, gp3, gp4, gp5, gp6, gp7, gp8,
                 ncol = 4, nrow = 2
                 ,labels = paste(letters, ".", sep = "")
                 , font.label = list(size = 6, face = "plain"), vjust = 1)
  
  p2 = ggarrange(gp9, gp10, gp11, gp12, gp13, gp14, gp15, gp16,
                 ncol = 4, nrow = 2
                 ,labels = paste(letters[9:16], ".", sep = "")
                 , font.label = list(size = 6, face = "plain"), vjust = 1)
  
   p1 = ggarrange(p1, colorbar1, widths = c(15,1))
   p2 = ggarrange(p2, colorbar2, widths = c(15,1))
   

   nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                      ifelse(v == 1, yes = "t-", no = "pp-")
                      , "anova", ".eps", sep = "")
   
   ggsave(nombre_fig,plot =  ggarrange(p1, p2, ncol = 1, nrow = 2, align = "v")
          ,dpi = 300, device = cairo_ps, height = 5, units = "in", width = 7)
   
}


# MME without models
# temp. CanCM4i, MAM, Bias
v = 1; m = 7; season = 1
bias1 = mapa_topo3(variable = EMM_wo[[v]][[m]][[7]]*mask_arr
           , variable.sig = EMM_wo[[v.sig]][[m]][[7-5]]
           , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = 0.1)
           , titulo = paste(titulos[[2]], "without CanCM4i", sep = " ")
           , label.escala = "", mapa = "SA"
           , na.fill = -1000, title.size = 13
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4
           , estaciones = T, altura.topo = 2000, size.point = 1
           , lon = lon2, lat = lat2, type.sig = "point"
           , estacion = season, mostrar = T, save = F
           ,  cb.v.w = 1, cb.v.h = 30, cb.size = 14
           , lats.size = 7, letter.size = 12, cajas = F
           , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))

   
# temp, CFSv2, SON, gamma  
v = 1; m = 6; season = 3
gamma1 = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr
           , variable.sig = EMM_wo[[v.sig]][[m]][[8-5]]
           , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
           , titulo = paste(titulos[[4]], "without CFSv2", sep = " ")
           , label.escala = "", mapa = "SA", width = 20, height = 20
           , na.fill = -1000,title.size = 12
           , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4
           , estaciones = T, altura.topo = 1500, size.point = 0.2
           , lon = lon2, lat = lat2, type.sig = "point"
           ,estacion = season, mostrar = T, save = F, cb.v.w = 0.7, cb.v.h = 13, cb.size = 7
          , lats.size = 7,letter.size = 12, margen.zero = F
          , cajas = F, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))

# pp, GEM-NEMO, JJA, gamma  
v = 3; m = 8; season = 2
gamma2 = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr
                    , variable.sig = EMM_wo[[v.sig]][[m]][[8-5]]
                    , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                    , titulo = paste(titulos[[4]], "without GEM-NEMO", sep = " ")
                    , label.escala = "", mapa = "SA", width = 20, height = 20
                    , na.fill = -1000,title.size = 12
                    , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4
                    , estaciones = T, altura.topo = 1500, size.point = 0.2
                    , lon = lon2, lat = lat2, type.sig = "point"
                    ,estacion = season, mostrar = T, save = F, cb.v.w = 0.7, cb.v.h = 13, cb.size = 7
                    , lats.size = 7,letter.size = 12, margen.zero = F
                    , cajas = F, color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
 