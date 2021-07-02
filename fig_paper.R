# figuras final paper.
source("funciones.R")
#source("aux_figpaper.R")

library(ggplot2)
library(gridExtra)
library(ncdf4)
library(ggpubr)
#------------------------------------------------------------------------------#
# tamaño final de la figura no tiene un gran impacto en el peso final
# ploteando solo topografia arriba de 2500 reduce bastante el peso
# bajar la cantidad de invervalos de colores reduce moderadamente el peso
# cario_ps --> cairo_pdf reduce un 92% el peso... (overleaf tiene problemas con
# algunas figuras exportadas asi, texmaker ok)

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

# letras = c(as.character("\u03b1"), as.character("\u03B2"), as.character("\u194")
#            , as.character("\u03B5"))

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


# Figs. ANOVA

colorbars = list()
colorbars[[1]] = "YlOrRd"; colorbars[[3]] = "PuBuGn"

colorbars_gamma = list()
colorbars_gamma[[1]] = "RdPu"; colorbars_gamma[[3]] = "BuPu"

seasons = c("MAM", "JJA", "SON", "DJF")

letter.size = 14
colorbar.length = 18
colorbar.size = 10
lats.size = 4 #por ahora borradas
height.fig = 10
width.fig = 10
rc = sig_temp_emm[[9]] # valor critico de gamma, igual para las dos

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
                        , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = .1)
                        , titulo = titulos[[1]]
                        , label.escala = NULL, mapa = "SA"
                        , width = 20, height = 20, title.size = 13, na.fill = -1000
                        , color.vsig = "black", alpha.vsig = 1
                        , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                        , cajas = F, lon = lon2, lat = lat2
                        , type.sig = "point", estacion = season
                        , mostrar = T, save = F,  cb.v.w = 0.5
                        , cb.v.h = colorbar.length, cb.size = colorbar.size
                        , lats.size = lats.size
                        , letter.size = letter.size, margen.zero = F)
    
    bias[[season]] = mapa_topo3(variable =  EMM[[v]][[7]]*mask_arr, variable.sig =  EMM[[v.sig]][[7-5]]
                      , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = .1)
                      , titulo = titulos[[2]]
                      , label.escala = NULL, mapa = "SA"
                      , width = 20, height = 20,title.size = 13, na.fill = -1000
                      , color.vsig = "black", alpha.vsig = 1
                      , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                      , cajas = F, lon = lon2, lat = lat2
                      , type.sig = "point", estacion = season
                      , mostrar = T, save = F
                      , cb.v.h = colorbar.length, cb.size = colorbar.size
                      , lats.size = lats.size
                      , letter.size = letter.size, margen.zero = F)
    
    noise[[season]] = mapa_topo3(variable =  EMM[[v]][[9]]*mask_arr, variable.sig =  EMM[[v.sig]][[9-5]]
                       , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = .1)
                       , titulo =  titulos[[3]]
                       , label.escala = NULL, mapa = "SA"
                       , width = 20, height = 20,title.size = 13, na.fill = -1000
                       , color.vsig = "black", alpha.vsig = 1
                       , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                       , cajas = F, lon = lon2, lat = lat2
                       , type.sig = "point", estacion = season
                       , mostrar = T, save = F
                       , cb.v.h = colorbar.length, cb.size = colorbar.size
                       , lats.size = lats.size
                       , letter.size = letter.size, margen.zero = F)
    
    
    # el valor de rc, para gamma en el grafico no esta determinado por un valor
    # critico de SSgamma, sino es del test previo al analizar la cantidad de var
    # explicada por gamma. Por lo tanto:
    aux=EMM[[v]][[8]][,,season]*EMM[[v.sig]][[8-5]][,,season]
  
    
    structural[[season]] = mapa_topo3(variable = EMM[[v]][[8]]*mask_arr, variable.sig =  EMM[[v.sig]][[8-5]]
                            , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                            , titulo = titulos[[4]], variable.cont = EMM[[v]][[8]]*mask_arr
                            , label.escala = NULL, mapa = "SA", contour = F
                            , width = 20, height = 20,title.size = 13, na.fill = -1000
                            , sig = T, color.vsig = "black", alpha.vsig = 0.3
                            , r = 4, estaciones = T, altura.topo = 2500, size.point = 0.0001
                            , cajas = F, lon = lon2, lat = lat2
                            , type.sig = "point", estacion = season
                            , mostrar = T, save = F, cb.v.w = 0.5
                            , cb.v.h = colorbar.length, cb.size = colorbar.size
                            , lats.size = lats.size
                            , letter.size = letter.size, margen.zero = F, color.vcont = "black"
                            , nivel.vcont = min(aux, na.rm = T))
    
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
                 , font.label = list(size = 12, face = "plain"), vjust = 1)
  
  p2 = ggarrange(gp9, gp10, gp11, gp12, gp13, gp14, gp15, gp16,
                 ncol = 4, nrow = 2
                 ,labels = paste(letters[9:16], ".", sep = "")
                 , font.label = list(size = 12, face = "plain"), vjust = 1)
  
   p1 = ggarrange(p1, colorbar1, widths = c(15,1))
   p2 = ggarrange(p2, colorbar2, widths = c(15,1))
   

   nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                      ifelse(v == 1, yes = "t-", no = "pp-")
                      , "anova", ".eps", sep = "")
   
   p1 = ggarrange(p1, p2, ncol = 1, nrow = 2, align = "v") +
     theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))


     ggsave(nombre_fig,plot =  p1
            ,dpi = 300, device = cairo_pdf, height = height.fig, width = width.fig
            , units = "in")
     
}


# MME without models
colorbar.length = 16
letter.size = 10
colorbar.size = 6
lats.size = 4 #por ahora borradas

# temp. CanCM4i, MAM, Bias
v = 1; m = 7; season = 1; v.sig = 2
bias1 = mapa_topo3(variable = EMM_wo[[v]][[m]][[7]]*mask_arr
           , variable.sig = EMM_wo[[v.sig]][[m]][[7-5]]
           , colorbar = colorbars[[v]], revert = F, escala = seq(0, 1, by = .1)
           , titulo = paste(titulos[[2]], "without CanCM4i", sep = " ")
           , label.escala = "", mapa = "SA"
           , na.fill = -1000, title.size = 13
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4
           , estaciones = T, altura.topo = 2500, size.point = 1
           , lon = lon2, lat = lat2, type.sig = "point"
           , estacion = season, mostrar = T, save = F
           ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
           , lats.size = 6, letter.size = letter.size, cajas = F
           , color.vcont = "black", nivel.vcont = rc)

   
# temp, CFSv2, SON, gamma  
v = 1; m = 6; season = 3; v.sig = 2
aux=EMM_wo[[v]][[m]][[8]][,,season]*EMM_wo[[v.sig]][[m]][[8-5]][,,season]
rc = min(aux, na.rm = T)
gamma1 = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr
           , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
           , titulo = paste(titulos[[4]], "without CFSv2", sep = " ")
           , label.escala = "", mapa = "SA", width = 20, height = 20
           , na.fill = -1000, variable.cont = EMM_wo[[v]][[m]][[8]]*mask_arr
           , contour = F, sig = T, variable.sig = EMM_wo[[v.sig]][[m]][[8-5]]
           , color.vsig = "black", alpha.vsig = 0.4, r = 4
           , estaciones = T, altura.topo = 2500, size.point = .01
           , lon = lon2, lat = lat2, type.sig = "point"
           ,estacion = season, mostrar = T, save = F
           ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
           , lats.size = 6, letter.size = letter.size, cajas = F
           , color.vcont = "black", nivel.vcont = rc)

# pp, GEM-NEMO, JJA, gamma  
v = 3; m = 8; season = 2; v.sig = 4
aux=EMM_wo[[v]][[m]][[8]][,,season]*EMM_wo[[v.sig]][[m]][[8-5]][,,season]
rc = min(aux, na.rm = T)
gamma2 = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr
                    , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                    , titulo = paste(titulos[[4]], "without CFSv2", sep = " ")
                    , label.escala = "", mapa = "SA", width = 20, height = 20
                    , na.fill = -1000, variable.cont = EMM_wo[[v]][[m]][[8]]*mask_arr
                    , contour = F, sig = T, variable.sig = EMM_wo[[v.sig]][[m]][[8-5]]
                    , color.vsig = "black", alpha.vsig = 0.4, r = 4
                    , estaciones = T, altura.topo = 2500, size.point = .01
                    , lon = lon2, lat = lat2, type.sig = "point"
                    ,estacion = season, mostrar = T, save = F
                    ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                    , lats.size = 6, letter.size = letter.size, cajas = F
                    , color.vcont = "black", nivel.vcont = rc)
 


gammas = list()
gammas[[1]] = gamma1; gammas[[2]] = gamma2


# corr SST
##### corr sst ######
##################3333333333333333333333333
load("obs.RData")
load("ens.RData")
load("sst.ci.RData")
load("vars.RData")
####### CORR. entre una region del contienente y toda la SST ####

lats = list()
lats[[2]] = seq(which(lat2 == -30), which(lat2 == -10), by = 1)
lats[[3]] = seq(which(lat2 == -20), which(lat2 == -5), by = 1)

lons = list()
lons[[2]] = seq(which(lon2 == 305), which(lon2 == 320), by = 1)
lons[[3]] = seq(which(lon2 == 300), which(lon2 == 315), by = 1)

aux = nc_open("ncfiles/X140.172.38.222.142.12.42.20.nc")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
nc_close(aux)

lat = rev(lat)
lats.area = list()
lats.area[[2]] = seq(which(lat == -30.5), which(lat == -10.5), by = 1)
lats.area[[3]] = seq(which(lat == -20.5), which(lat == -5.5), by = 1)

lons.area = list()
lons.area[[2]] = seq(which(lon == 305.5), which(lon == 320.5), by = 1)
lons.area[[3]] = seq(which(lon == 300.5), which(lon == 315.5), by = 1)

#### graficos ####
# temp
region = c("Area-CanCM4i-T", "Area-CFSv2-T", "Area-GEM-NEMO-PP")
region.fig = c("CM4i-T", "CFSv2-T", "G-N-PP")
estaciones = c("MAM", "JJA", "SON", "DJF")

nombres = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO") 


seasons = c(3,2)

wo.mod = c(6,8)

area.mod = c(2,3)

var.nombre = c("T", "PP")

mask2 = as.matrix(read.table("mascara.txt"))
mask2 = array(mask2, dim = c(56,76))

lats.area.inv = lons.area.inv = list()

lats.area.inv[[2]] =  seq(which(lat == -4.5), which(lat == 5.5))
lats.area.inv[[3]] =  seq(which(lat == 10.5), which(lat == 20.5), by = 1)

lons.area.inv[[2]] =  seq(which(lon == 190.5), which(lon == 240.5))
lons.area.inv[[3]] =  seq(which(lon == 300.5), which(lon == 325.5), by = 1)

region = c("CFSv2", "GEM-NEMO")
region.fig = c("CFSv2-T", "G-N-PP")

letter.size = 10
colorbar.length = 18
colorbar.size = 10
# letter.size = 8
# colorbar.length = 15
# colorbar.size = 4
lats.size = 4 #por ahora borradas
height.fig = 4
width.fig = 10

for(v in 1:2){
  
  j = area.mod[v]
    
    s = seasons[v]
    
    
    #### MME ###
    
    aux.prom = apply(sst.ci[lons.area.inv[[j]], lats.area.inv[[j]],,], c(3,4), mean, na.rm = T)
    
    aux.cor = corr(mod = aux.prom[,s], obs = ens[,,,s,v], lon = 56, lat = 76, cf = .90)
    aux = array(aux.cor[,,1]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    aux2 = array(aux.cor[,,2]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    
    
    rc = min(abs(aux*aux2), na.rm = T)
    
    g1 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , variable.cont = aux, contour = F, nivel.vcont = c(-rc, rc), color.vcont = "black"
                    , sig = T, variable.sig = aux2,  color.vsig = "black"
                    , alpha.vsig = 0.3, altura.topo = 2500, type.sig = "point", size.point = .01
                    , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .2)
                    , titulo = "MME"
                    , label.escala = "", mapa = "SA", estaciones = T
                    , r = 1, contour.fill = T, na.fill = -10000, title.size = 8
                    , cb.h.w = colorbar.length, cb.h.h = 0.5, save = F, margen.zero = F
                    , cb.size = colorbar.size, letter.size = letter.size
                    , width = 25, mostrar = T, colorbar.pos = "bottom")
    
    ### MOD ### 
    
    m = wo.mod[v]
    
    aux.cor = corr(mod = aux.prom[,s], obs = vars[,,,s,m,v], lon = 56, lat = 76, cf = .90)
    aux = array(aux.cor[,,1]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    aux2 = array(aux.cor[,,2]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    rc = min(abs(aux*aux2), na.rm = T)
    
    g2 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , variable.cont = aux, contour = F, nivel.vcont = c(-rc, rc), color.vcont = "black"
                    , sig = T, variable.sig = aux2,  color.vsig = "black"
                    , alpha.vsig = 0.3, altura.topo = 2500, type.sig = "point", size.point = .01
                    , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .2)
                    , titulo = region[v]
                    , label.escala = "", mapa = "SA", estaciones = T
                    , r = 1, contour.fill = T, na.fill = -10000, title.size = 8
                    , cb.h.w = colorbar.length, cb.h.h = 0.5, save = F, margen.zero = F
                    , cb.size = colorbar.size, letter.size = letter.size
                    , width = 25, mostrar = T, colorbar.pos = "bottom")
    
    
    
    ### OBS ###
    
    aux.cor = corr(mod = aux.prom[,s], obs = obs[,,,s,v], lon = 56, lat = 76, cf = .90)
    aux = array(aux.cor[,,1]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    aux2 = array(aux.cor[,,2]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    rc = min(abs(aux*aux2), na.rm = T)
    
    g3 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , variable.cont = aux, contour = F, nivel.vcont = c(-rc, rc), color.vcont = "black"
                    , sig = T, variable.sig = aux2,  color.vsig = "black"
                    , alpha.vsig = 0.3, altura.topo = 2500, type.sig = "point", size.point = .01
                    , colorbar = "RdBu", revert = T, escala = seq(-1,1,by = .2)
                    , titulo = "Observed"
                    , label.escala = "", mapa = "SA", estaciones = T
                    , r = 1, contour.fill = T, na.fill = -10000, title.size = 8
                    , cb.h.w = colorbar.length, cb.h.h = 0.5, save = F, margen.zero = F
                    , cb.size = colorbar.size, letter.size = letter.size
                    , width = 25, mostrar = T, colorbar.pos = "bottom")
    
    
# GRID
colorbar1 <- g_legend(g1)
colorbar_gammaT = g_legend(gammas[[v]])

gp1 = g1 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp2 = g2 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp3 = g3 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 


gamma = gammas[[v]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))

p0 = ggarrange(gamma, colorbar_gammaT, widths =  c(7,1)
               , labels = paste(letters[1], ".", sep = "")
               , font.label = list(size = 10, face = "plain"), vjust = 1)


p1 = ggarrange(gp1, gp2, gp3,
               ncol = 3, nrow = 1
               ,labels = paste(letters[2:4], ".", sep = "")
               , font.label = list(size = 10, face = "plain"), vjust = 1)

p1 = ggarrange(p1, colorbar1, ncol = 1, nrow = 2, heights = c(10,1))


p1 = ggarrange(p0, p1, widths = c(1,2)) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))


nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                   ifelse(v == 1, yes = "t-", no = "pp-")
                   , "gamma", ".eps", sep = "")



  ggsave(nombre_fig,plot =  p1
         ,dpi = 300, device = cairo_pdf, height = height.fig, width = width.fig
         , units = "in")
  

}


#bias 
load("V.mean.RData")

seasons = list()
seasons[[1]] = c(1)

mods = list()
mods[[1]] = c(7)

mean.obs = apply(obs, c(1,2,4,5), mean)

escalas = list()
escalas[[1]] = seq(-5,5)
colorbars = list(); colorbars[[1]] = "RdBu"
revert = c(T, F)

var.titulo = c("T", "PP")

letter.size = 10
colorbar.length = 18
colorbar.size = 10
# letter.size = 8
# colorbar.length = 15
# colorbar.size = 4
lats.size = 4 #por ahora borradas
height.fig = 4
width.fig = 10

for(v in 1){
  
  j = 1
  for(m in 7){
    
    s = 1
    
    no_mod = c(1,2,3,4,5,6,7,8)
    no_mod[m] = NA
    
    if(m == 2){
      escalas[[1]] = seq(-8,8,by = 2)
      escalas[[2]] = seq(-150,150, by = 25)
    } else {
      escalas[[1]] = seq(-5,5)
      escalas[[2]] = seq(-100, 100, by = 20)
    }
    
    #ensamble sin el modelo m
    aux = apply(V.mean[,,s,no_mod,v], c(1,2), mean, na.rm = T)
    
    dif_emm = V.mean[,,s,m,v] - aux
    dif_obs = V.mean[,,s,m,v] - mean.obs[,,s,v]
    dif_emm.obs = aux - mean.obs[,,s,v]
    
    
    #emm
    aux = array(dif_emm, dim = c(56,76,1))
    g1 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , altura.topo = 2500
                    , colorbar = colorbars[[v]], revert = revert[v], escala = escalas[[v]]
                    , titulo = "MME minus Obs"
                    , label.escala = "ºC", mapa = "SA",estaciones = T
                    , r = 1, contour.fill = T, na.fill = -10000, margen.zero = F
                    , cb.h.w = colorbar.length, cb.h.h = 0.5, save = F
                    , cb.size = colorbar.size, letter.size = letter.size
                    , width = 25, mostrar = T, colorbar.pos = "bottom")
    #mod
    aux = array(dif_obs, dim = c(56,76,1))
    g2 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , altura.topo = 2500
                    , colorbar = colorbars[[v]], revert = revert[v], escala = escalas[[v]]
                    , titulo = "CanCM4i minus Obs."
                    , label.escala = "", mapa = "SA",estaciones = T
                    , r = 1, contour.fill = T, na.fill = -10000
                    , cb.h.w = colorbar.length, cb.h.h = 0.5, save = F
                    , cb.size = colorbar.size, letter.size = letter.size
                    , width = 25, mostrar = T, colorbar.pos = "bottom")
    
    #emm.obs
    aux = array(dif_emm.obs, dim = c(56,76,1))
    g3 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , altura.topo = 2500
                    , colorbar = colorbars[[v]], revert = revert[v], escala = escalas[[v]]
                    , titulo = "MME minus Obs."
                    , label.escala = "", mapa = "SA",estaciones = T
                    , r = 1, contour.fill = T, na.fill = -10000
                    , cb.h.w = colorbar.length, cb.h.h = 0.5, save = F, margen.zero = F
                    , cb.size = colorbar.size, letter.size = letter.size
                    , width = 25, mostrar = T, colorbar.pos = "bottom")

colorbar1 = g_legend(g1)
colorbar2 = g_legend(bias1)

gp1 = g1 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp2 = g2 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp3 = g3 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 


bias = bias1 + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))

p0 = ggarrange(bias, colorbar2, widths =  c(8,1)
               , labels = paste(letters[1], ".", sep = "")
               , font.label = list(size = 6, face = "plain"), vjust = 1)


p1 = ggarrange(gp1, gp2, gp3,
               ncol = 3, nrow = 1
               ,labels = paste(letters[2:4], ".", sep = "")
               , font.label = list(size = 10, face = "plain"), vjust = 1)

p1 = ggarrange(p1, colorbar1, ncol = 1, nrow = 2, heights = c(10,1))

 
p1 = ggarrange(p0, p1, widths = c(1,2)) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))


nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                   ifelse(v == 1, yes = "t-", no = "pp-")
                   , "bias", ".eps", sep = "")

  ggsave(nombre_fig,plot =  p1
         ,dpi = 300, device = cairo_pdf, height = height.fig, width = width.fig
         , units = "in")

    
  }
}


# Potential Predictability

# MME
pred = pp_test(ss_temp = ss_temp, ss_pp = ss_pp, ensemble_total = "si")

# MME wo mod
pred_wo =pp_test(ss_temp = ss_T, ss_pp = ss_PP, ensemble_total = "no")

escala_pred = list()
escala_pred[[1]] = seq(0,1, by = .1); escala_pred[[3]] = seq(0, .6, by = .1)

colorbars = list()
colorbars[[1]] = "YlOrRd"; colorbars[[3]] = "PuBuGn"

seasons = c("MAM", "JJA", "SON", "DJF")

letter.size = 10
colorbar.length = 18
colorbar.size = 10

# letter.size = 8
# colorbar.length = 14
# colorbar.size = 6
lats.size = 4 #por ahora borradas
height.fig = 5
width.fig = 10


# MME

pred_MME = pred_MME_wo = list()

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
    
    pred_MME[[season]] = mapa_topo3(variable = pred[[v]]*mask_arr, variable.sig = pred[[v.sig]]
                                  , colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                                  , titulo = seasons[[season]]
                                  , label.escala = NULL, mapa = "SA"
                                  , width = 20, height = 20, title.size = 13, na.fill = -1000
                                  , sig = T, color.vsig = "black", alpha.vsig = 1
                                  , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                                  , cajas = F, lon = lon2, lat = lat2
                                  , type.sig = "point", estacion = season
                                  , mostrar = T, save = F, margen.zero = F
                                  ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                                  , lats.size = 6, letter.size = letter.size
                                  , color.vcont = "black", nivel.vcont = rc)
    # sin cfsv2
    m = 6
    pred_MME_wo[[season]] = mapa_topo3(variable = pred_wo[[m]][[v]]*mask_arr, variable.sig = pred_wo[[m]][[v.sig]]
                                       , colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                                       , titulo = ""
                                       , label.escala = NULL, mapa = "SA"
                                       , width = 20, height = 20, title.size = 13, na.fill = -1000
                                       , sig = T, color.vsig = "black", alpha.vsig = 1
                                       , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                                       , cajas = F, lon = lon2, lat = lat2
                                       , type.sig = "point", estacion = season
                                       , mostrar = T, save = F, margen.zero = F
                                       ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                                       , lats.size = 6, letter.size = letter.size
                                       , color.vcont = "black", nivel.vcont = rc)
    
  }
    
colorbar1 = g_legend(pred_MME[[1]])

gp1 = pred_MME[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp2 = pred_MME[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp3 = pred_MME[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp4 = pred_MME[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))

gp5 = pred_MME_wo[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp6 = pred_MME_wo[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp7 = pred_MME_wo[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp8 = pred_MME_wo[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))


p1 = ggarrange(gp1, gp2, gp3, gp4, gp5, gp6, gp7, gp8,
               ncol = 4, nrow = 2
               ,labels = paste(letters, ".", sep = "")
               , font.label = list(size = 10, face = "plain"), vjust = 1)

p1 = ggarrange(p1, colorbar1, ncol = 2, widths = c(15,1)) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))


nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                   ifelse(v == 1, yes = "t-", no = "pp-")
                   , "pred", ".eps", sep = "")


  ggsave(nombre_fig,plot =  p1
         ,dpi = 300, device = cairo_pdf, height = height.fig, width = width.fig
         , units = "in")
  


}  

# RMSE; RMSE normalizado con SD = 1 - RMSE/sd

#source("rmse_corregido.R")

#save(NRMSE, file = "NRMSE.RData")

load("NRMSE.RData") # "resutlados"

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}


colorbars_nrmse = c("PuOr", "BrBG")
revert = c(T, F)

seasons = c("MAM", "JJA", "SON", "DJF")

letter.size = 10
colorbar.length = 9
colorbar.size = 10

# letter.size = 8
# colorbar.length = 14
# colorbar.size = 6
lats.size = 4 #por ahora borradas
height.fig = 5
width.fig = 10

aux = list()


for(v in 1:2){
  aux[[v]] = list()
  for(s in 1:4){
    
    aux[[v]][[s]] = mapa_topo3(variable = NRMSE[[v]]*mask_arr
               , colorbar = colorbars_nrmse[v], revert = revert[v]
               , escala = seq(-.6, .6, by = .15)
               , titulo = seasons[s]
               , label.escala = NULL, mapa = "SA"
               , width = 20, height = 20, title.size = 13, na.fill = -1000
               , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
               , cajas = F, lon = lon2, lat = lat2
               , type.sig = "point", estacion = s
               , mostrar = T, save = F, margen.zero = F
               ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
               , lats.size = 6, letter.size = letter.size
               , color.vcont = "black", nivel.vcont = rc)
    
  }
}

colorbar1 = g_legend(aux[[1]][[1]])
colorbar2 = g_legend(aux[[2]][[1]])


gp1 = aux[[1]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp2 = aux[[1]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp3 = aux[[1]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp4 = aux[[1]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))

gp5 = aux[[2]][[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")
gp6 = aux[[2]][[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")
gp7 = aux[[2]][[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")
gp8 = aux[[2]][[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")


p1 = ggarrange(gp1, gp2, gp3, gp4,
               ncol = 4, nrow = 1
               ,labels = paste(letters, ".", sep = "")
               , font.label = list(size = 10, face = "plain"), vjust = 1)

p1 = ggarrange(p1, colorbar1, ncol = 2, widths = c(14,1)) +
  theme(plot.margin = margin(0.2,0.2,0,0.2, "cm"))

p2 = ggarrange(gp5, gp6, gp7, gp8,
               ncol = 4, nrow = 1
               ,labels = paste(letters[5:8], ".", sep = "")
               , font.label = list(size = 10, face = "plain"), vjust = 1)

p2 = ggarrange(p2, colorbar2, ncol = 2, widths = c(14,1)) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))


nombre_fig = "/home/luciano.andrian/paper2021/NRMSE.eps" 

pf = ggarrange(p1, p2, ncol = 1, nrow = 2)             

ggsave(nombre_fig,plot =  pf
       ,dpi = 300, device = cairo_pdf, height = height.fig, width = width.fig
       , units = "in")

# ACC
# source("aux.desemp.R")
# save(resultados, file = "ACC.RData")
load("ACC.RData") # "resutlados"


resultados[[2]] = resultados[[2]][,,,3]
resultados[[5]] = resultados[[5]][,,,,3]
colorbars = list()
colorbars[[1]] = "YlOrRd"; colorbars[[2]] = "PuBuGn"


ACC_MME = list(); ACC_teo = list()

rc = qt(p = 0.95,df = 29-1)/sqrt((29-1)+qt(p = 0.95,df = 29-1))


letter.size = 10
colorbar.length = 18
colorbar.size = 10

# letter.size = 8
# colorbar.length = 14
# colorbar.size = 6
lats.size = 4 #por ahora borradas
height.fig = 5
width.fig = 10


for(v in 1:2){
  v1 = ifelse(v == 1, yes = 6, no = 7)
  for(s in 1:4){
    
    ACC_teo[[s]] = mapa_topo3(variable = resultados[[v1]]*mask_arr, variable.sig = resultados[[v1]]*mask_arr
                              , colorbar = colorbars[[v]], revert = F, nivel.vcont = c(2,2.01, 2.02, 2.03)
                              , escala = seq(0, 1, by = .1), contour = F
                              , titulo = seasons[s], v.sig = rc
                              , label.escala = NULL, mapa = "SA"
                              , width = 20, height = 20, title.size = 13, na.fill = -1000
                              , sig = T, color.vsig = "black", alpha.vsig = 0.3
                              , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                              , cajas = F, lon = lon2, lat = lat2
                              , type.sig = "point2", estacion = s
                              , mostrar = T, save = F, margen.zero = F
                              ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                              , lats.size = 6, letter.size = letter.size
                              , color.vcont = "black")
    
    ACC_MME[[s]] = mapa_topo3(variable = resultados[[v]]*mask_arr, variable.sig = resultados[[v]]*mask_arr
                              , colorbar = colorbars[[v]], revert = F, variable.cont = resultados[[v]]*mask_arr
                              , escala = seq(0, 1, by = .1), contour = F
                              , titulo = seasons[s], v.sig = rc
                              , label.escala = NULL, mapa = "SA"
                              , width = 20, height = 20, title.size = 13, na.fill = -1000
                              , sig = T, color.vsig = "black", alpha.vsig = 0.3
                              , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                              , cajas = T, lon = lon2, lat = lat2
                              , type.sig = "point2", estacion = s
                              , mostrar = T, save = F, margen.zero = F
                              ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                              , lats.size = 6, letter.size = letter.size
                              , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
    
   
  }



colorbar1 = g_legend(ACC_MME[[1]])

gp1 = ACC_MME[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))
gp2 = ACC_MME[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp3 = ACC_MME[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) 
gp4 = ACC_MME[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))

gp5 = ACC_teo[[1]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")
gp6 = ACC_teo[[2]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")
gp7 = ACC_teo[[3]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines")) + ggtitle(label = "")
gp8 = ACC_teo[[4]] + theme(legend.position = "none", plot.margin = unit(c(0,.2,.2,.2), "lines"))+ ggtitle(label = "")


p1 = ggarrange(gp1, gp2, gp3, gp4, gp5, gp6, gp7, gp8,
               ncol = 4, nrow = 2
               ,labels = paste(letters, ".", sep = "")
               , font.label = list(size = 10, face = "plain"), vjust = 1)

p1 = ggarrange(p1, colorbar1, ncol = 2, widths = c(15,1)) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))


nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                   ifelse(v == 1, yes = "t-", no = "pp-")
                   , "ACC", ".eps", sep = "")


  ggsave(nombre_fig,plot =  p1
         ,dpi = 300, device = cairo_pdf, height = height.fig, width = width.fig
         , units = "in")

}



##### ACC cajas y modelos #####


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
  pp.ACC_ens_box[[i]] = apply(pp.ACC[cajas_lon[[i]], cajas_lat[[i]],], c(3), mean, na.rm = T)
  
  t.ACC_box[[i]] = apply(t.ACC_mod[cajas_lon[[i]], cajas_lat[[i]],,], c(3, 4), mean, na.rm = T)
  
  pp.ACC_box[[i]] = apply(pp.ACC_mod[cajas_lon[[i]], cajas_lat[[i]],,,3], c(3, 4), mean, na.rm = T)
  
}

cajas = c("N-SESA", "S-SESA", "NeB", "Patagonia", "Am")
cajas_num = seq( 1, 5)


t.data = fig10(prom_cajas = t.ACC_box, prom_ensamble = t.ACC_ens_box, variable = "temp")


pp.data = fig10(prom_cajas = pp.ACC_box, prom_ensamble = pp.ACC_ens_box, variable = "temp")

gs = list()
# Grafico
g1 = ggplot() + theme_minimal()+
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5), size = 1, color = "gray", alpha = 0.3)+
  geom_hline(yintercept = rc, color = "gray4", size = 0.1) +
  geom_hline(yintercept = 0, color = "black",size = 0.2) +
  geom_text(data = t.data[[1]], aes(x = cajas_mam, y = MAM, label = value), color = "orangered2", size = 2, alpha = 0.55) + 
  geom_point(data = t.data[[2]], aes(x = cajas_mam, y = MAM), color = "orangered2", shape = "-" , size = 7) +
  geom_text(data = t.data[[1]], aes(x = cajas_jja, y = JJA, label = value), color = "royalblue4", size = 2, alpha = 0.55) + 
  geom_point(data = t.data[[2]], aes(x = cajas_jja, y = JJA), color = "royalblue4", shape = "-" , size = 7) +
  geom_text(data = t.data[[1]], aes(x = cajas_son, y = SON, label = value), color = "seagreen", size = 2, alpha = 0.65) + 
  geom_point(data = t.data[[2]], aes(x = cajas_son, y = SON), color = "seagreen", shape = "-" , size = 7) +
  geom_text(data = t.data[[1]], aes(x = cajas_djf, y = DJF, label = value), color = "palevioletred3", size = 2, alpha = 0.65) + 
  geom_point(data = t.data[[2]], aes(x = cajas_djf, y = DJF), color = "palevioletred3", shape = "-" , size = 7) +
  ggtitle(paste("ACC - Temperature")) + ylab(NULL) +
  scale_y_continuous(limits = c(-0.2, 0.8), breaks = seq(-0.2,0.8, by = .2)) + 
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0,0.8, by = .1)) + 
  theme(axis.text.y   = element_text(size = 6, color = "black"), axis.text.x   = element_blank(),
        axis.title.y  = element_text(size = 6),
        panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.1),
        panel.ontop = F,
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8)) 


g2 = ggplot() + theme_minimal()+
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5), size = 1, color = "gray", alpha = 0.3)+
  geom_hline(yintercept = rc, color = "gray4", size = 0.1) +
  geom_hline(yintercept = 0, color = "black",size = 0.2)+
  geom_text(data = pp.data[[1]], aes(x = cajas_mam, y = MAM, label = value), color = "orangered2", size = 2, alpha = 0.55) + 
  geom_point(data = pp.data[[2]], aes(x = cajas_mam, y = MAM), color = "orangered2", shape = "-" , size = 7) +
  geom_text(data = pp.data[[1]], aes(x = cajas_jja, y = JJA, label = value), color = "royalblue4", size = 2, alpha = 0.55) + 
  geom_point(data = pp.data[[2]], aes(x = cajas_jja, y = JJA), color = "royalblue4", shape = "-" , size = 7) +
  geom_text(data = pp.data[[1]], aes(x = cajas_son, y = SON, label = value), color = "seagreen", size = 2, alpha = 0.65) + 
  geom_point(data = pp.data[[2]], aes(x = cajas_son, y = SON), color = "seagreen", shape = "-" , size = 7) +
  geom_text(data = pp.data[[1]], aes(x = cajas_djf, y = DJF, label = value), color = "palevioletred3", size = 2, alpha = 0.65) + 
  geom_point(data = pp.data[[2]], aes(x = cajas_djf, y = DJF), color = "palevioletred3", shape = "-" , size = 7) +
  ggtitle(paste("ACC - Precipitation"))+ylab(NULL) +
  scale_y_continuous(limits = c(-0.2, 0.8), breaks = seq(-0.2,0.8, by = .2)) + 
  scale_x_continuous(labels=c("1" = "N-SESA", "2" = "S-SESA", "3" = "NeB", "4" = "Patagonia", "5" = "Am"),breaks = seq(1, 5, by = 1))+
  xlab(label = NULL)+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0,0.8, by = .1)) + 
  theme(axis.text.y   = element_text(size = 6, color = "black"),  axis.text.x   = element_text(size = 8, color = "black", face = "bold"), 
        axis.title.y  = element_text(size = 6),
        panel.grid.minor = element_blank(), axis.title.x = element_text(size = 8),  
        panel.border = element_rect(colour = "black", fill = NA, size = 0.1),
        panel.ontop = F,
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8)) 
  

gs = list(g1, g2)

p1 = ggarrange(plotlist = gs, labels = paste(letters, ".", sep = ""), nrow = 2, align = "v"
               , font.label = list(size = 7, face = "plain"), vjust = 1) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm")) 


nombre_fig = "/home/luciano.andrian/paper2021/ACC_box.eps"

ggsave(nombre_fig,plot =  p1
       ,dpi = 300, device = cairo_pdf, height = 6, width = 6
       , units = "in")

 ################################################################################

