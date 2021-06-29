# figuras final paper.
source("funciones.R")
source("aux_figpaper.R")

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

letter.size = 8
colorbar.length = 9.5
colorbar.size = 6
lats.size = 4 #por ahora borradas
height.fig = 6
width.fig = 7

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
                        , cb.v.h = colorbar.length, cb.size = colorbar.size
                        , lats.size = lats.size
                        , letter.size = letter.size, margen.zero = F, color.vcont = "black"
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
                      , cb.v.h = colorbar.length, cb.size = colorbar.size
                      , lats.size = lats.size
                      , letter.size = letter.size, margen.zero = F, color.vcont = "black"
                      , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
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
                       , cb.v.h = colorbar.length, cb.size = colorbar.size
                       , lats.size = lats.size
                       , letter.size = letter.size, margen.zero = F, color.vcont = "black"
                       , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    structural[[season]] = mapa_topo3(variable = EMM[[v]][[8]]*mask_arr, variable.sig =  EMM[[v.sig]][[8-5]]
                            , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                            , titulo = titulos[[4]]
                            , label.escala = NULL, mapa = "SA"
                            , width = 20, height = 20,title.size = 13, na.fill = -1000
                            , sig = T, color.vsig = "black", alpha.vsig = 0.1
                            , r = 4, estaciones = T, altura.topo = 2000, size.point = 0.0001
                            , cajas = F, lon = lon2, lat = lat2
                            , type.sig = "point", estacion = season
                            , mostrar = T, save = F, cb.v.w = 0.5
                            , cb.v.h = colorbar.length, cb.size = colorbar.size
                            , lats.size = lats.size
                            , letter.size = letter.size, margen.zero = F, color.vcont = "black"
                            , nivel.vcont = c(2,2.01, 2.02, 2.03))
    
    
    
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
   
   p1 = ggarrange(p1, p2, ncol = 1, nrow = 2, align = "v") +
     theme(plot.margin = margin(0.3,0.2,0.3,0.2, "cm"))
   
   ggsave(nombre_fig,plot =  p1
          ,dpi = 300, device = cairo_ps, height = height.fig, width = width.fig
          , units = "in")
   
}




# MME without models

letter.size = 8
colorbar.length = 9.5
colorbar.size = 6
lats.size = 4 #por ahora borradas

# temp. CanCM4i, MAM, Bias
v = 1; m = 7; season = 1; v.sig = 2
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
           ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
           , lats.size = 6, letter.size = letter.size, cajas = F
           , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))

   
# temp, CFSv2, SON, gamma  
v = 1; m = 6; season = 3; v.sig = 2
gamma1 = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr
           , variable.sig = EMM_wo[[v.sig]][[m]][[8-5]]
           , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
           , titulo = paste(titulos[[4]], "without CFSv2", sep = " ")
           , label.escala = "", mapa = "SA", width = 20, height = 20
           , na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4
           , estaciones = T, altura.topo = 2000, size.point = .01
           , lon = lon2, lat = lat2, type.sig = "point"
           ,estacion = season, mostrar = T, save = F
           ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
           , lats.size = 6, letter.size = letter.size, cajas = F
           , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))

# pp, GEM-NEMO, JJA, gamma  
v = 3; m = 8; season = 2; v.sig = 4
gamma2 = mapa_topo3(variable = EMM_wo[[v]][[m]][[8]]*mask_arr
                    , variable.sig = EMM_wo[[v.sig]][[m]][[8-5]]
                    , colorbar = colorbars_gamma[[v]], revert = F, escala = seq(0, 0.1, by = 0.01)
                    , titulo = paste(titulos[[4]], "without GEM-NEMO", sep = " ")
                    , label.escala = "", mapa = "SA", width = 20, height = 20
                    , na.fill = -1000,title.size = 12
                    , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4
                    , estaciones = T, altura.topo = 2000, size.point = .01
                    , lon = lon2, lat = lat2, type.sig = "point"
                    ,estacion = season, mostrar = T, save = F
                    ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                    , lats.size = 6, letter.size = letter.size, cajas = F
                    , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
 


gammas = list()
gammas[[1]] = gamma1; gammas[[2]] = gamma2


# corr SST
##### corr sst ######
##################3333333333333333333333333
load("obs.RData")
load("ens.RData")

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

letter.size = 8
colorbar.length = 14
colorbar.size = 6
lats.size = 4 #por ahora borradas
height.fig = 3
width.fig = 7

for(v in 1:2){
  
  j = area.mod[v]
    
    s = seasons[v]
    
    
    #### MME ###
    
    aux.prom = apply(sst.ci[lons.area.inv[[j]], lats.area.inv[[j]],,], c(3,4), mean, na.rm = T)
    
    aux.cor = corr(mod = aux.prom[,s], obs = ens[,,,s,v], lon = 56, lat = 76, cf = .90)
    aux = array(aux.cor[,,1]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    aux2 = array(aux.cor[,,2]*mask2, dim = c(dim(aux.cor[,,1]), 1))
    
    
    
    g1 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , sig = T, variable.sig = aux2,  color.vsig = "black"
                    , alpha.vsig = 0.4, altura.topo = 2000, type.sig = "point", size.point = .01
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
    
    
    g2 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , sig = T, variable.sig = aux2,  color.vsig = "black"
                    , alpha.vsig = 0.4, altura.topo = 2000, type.sig = "point", size.point = .01
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
    
    
    g3 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , sig = T, variable.sig = aux2,  color.vsig = "black"
                    , alpha.vsig = 0.4, altura.topo = 2000, type.sig = "point", size.point = .01
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

p0 = ggarrange(gamma, colorbar_gammaT, widths =  c(8,1)
               , labels = paste(letters[1], ".", sep = "")
               , font.label = list(size = 6, face = "plain"), vjust = 1)


p1 = ggarrange(gp1, gp2, gp3,
               ncol = 3, nrow = 1
               ,labels = paste(letters[2:4], ".", sep = "")
               , font.label = list(size = 6, face = "plain"), vjust = 1)

p1 = ggarrange(p1, colorbar1, ncol = 1, nrow = 2, heights = c(10,1))


p1 = ggarrange(p0, p1, widths = c(1,2)) +
  theme(plot.margin = margin(0.3,0.2,0.3,0.2, "cm"))


nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                   ifelse(v == 1, yes = "t-", no = "pp-")
                   , "gamma", ".eps", sep = "")

ggsave(nombre_fig,plot =  p1
       ,dpi = 300, device = cairo_ps, height = height.fig, width = width.fig
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
                    , altura.topo = 2000
                    , colorbar = colorbars[[v]], revert = revert[v], escala = escalas[[v]]
                    , titulo = "MME wo. CanCM4i minus Obs"
                    , label.escala = "ºC", mapa = "SA",estaciones = T
                    , r = 1, contour.fill = T, na.fill = -10000, margen.zero = F
                    , cb.h.w = colorbar.length, cb.h.h = 0.5, save = F
                    , cb.size = colorbar.size, letter.size = letter.size
                    , width = 25, mostrar = T, colorbar.pos = "bottom")
    #mod
    aux = array(dif_obs, dim = c(56,76,1))
    g2 = mapa_topo3(variable = aux, lon = lon2, lat = lat2
                    , altura.topo = 2000
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
                    , altura.topo = 2000
                    , colorbar = colorbars[[v]], revert = revert[v], escala = escalas[[v]]
                    , titulo = paste(var.nombre[v]," - Climatología EMM - OBS", " - ", estaciones[s], sep = "")
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
               , font.label = list(size = 6, face = "plain"), vjust = 1)

p1 = ggarrange(p1, colorbar1, ncol = 1, nrow = 2, heights = c(10,1))

 
p1 = ggarrange(p0, p1, widths = c(1,2)) +
  theme(plot.margin = margin(0.3,0.2,0.3,0.2, "cm"))


nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                   ifelse(v == 1, yes = "t-", no = "pp-")
                   , "bias", ".eps", sep = "")

ggsave(nombre_fig,plot =  p1
       ,dpi = 300, device = cairo_ps, height = height.fig, width = width.fig
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

letter.size = 8
colorbar.length = 14
colorbar.size = 6
lats.size = 4 #por ahora borradas
height.fig = 4
width.fig = 7


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
                                  , r = 4, estaciones = T, altura.topo = 2000, size.point = .01
                                  , cajas = F, lon = lon2, lat = lat2
                                  , type.sig = "point", estacion = season
                                  , mostrar = T, save = F, margen.zero = F
                                  ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                                  , lats.size = 6, letter.size = letter.size
                                  , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
    # sin cfsv2
    m = 6
    pred_MME_wo[[season]] = mapa_topo3(variable = pred_wo[[m]][[v]]*mask_arr, variable.sig = pred_wo[[m]][[v.sig]]
                                       , colorbar = colorbars[[v]], revert = F, escala = escala_pred[[v]]
                                       , titulo = ""
                                       , label.escala = NULL, mapa = "SA"
                                       , width = 20, height = 20, title.size = 13, na.fill = -1000
                                       , sig = T, color.vsig = "black", alpha.vsig = 1
                                       , r = 4, estaciones = T, altura.topo = 2000, size.point = .01
                                       , cajas = F, lon = lon2, lat = lat2
                                       , type.sig = "point", estacion = season
                                       , mostrar = T, save = F, margen.zero = F
                                       ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                                       , lats.size = 6, letter.size = letter.size
                                       , color.vcont = "black", nivel.vcont = c(2,2.01, 2.02, 2.03))
    
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
               , font.label = list(size = 6, face = "plain"), vjust = 1)

p1 = ggarrange(p1, colorbar1, ncol = 2, widths = c(15,1)) +
  theme(plot.margin = margin(0.3,0.2,0.3,0.2, "cm"))


nombre_fig = paste("/home/luciano.andrian/paper2021/", 
                   ifelse(v == 1, yes = "t-", no = "pp-")
                   , "pred", ".eps", sep = "")

ggsave(nombre_fig,plot =  p1
       ,dpi = 300, device = cairo_ps, height = height.fig, width = width.fig
       , units = "in")

}  
