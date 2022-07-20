source("funciones.R")
source('funciones_MC.R')
library(ncdf4)
library(parallel)
library(ggplot2)
library(ggpubr)
################################################################################
dir = "/pikachu/datos/luciano.andrian/aux_nmme_quantiles/"
lon2 = read.table("lon2.txt")[,1] # las usan algunas funciones
lat2 = read.table("lat2.txt")[,1]

variable = c('Temp', 'Prec')
variable2 = c('temp', 'pp')

# Pred del MME si hace falta (es un poco lento) ################################
# ss_temp = anova_fun(variable = "temp", ensemble_total = "si")
# # sig_temp_emm = test_cos(ss_temp, ensemble_total = "si", nomodel_selec = "no"
# #                          , no_model = "no")
# 
# ss_pp = anova_fun(variable = "pp", ensemble_total = "si")
# # sig_pp_emm = test_cos(ss_pp, ensemble_total = "si", nomodel_selec = "no"
# #                       , no_model = "no")
# 
# pred = pp_test(ss_temp = ss_temp, ss_pp = ss_pp, ensemble_total = "si")
# #----------------------------------------------------------------------------#
# 
# PPt = list()
# PPt[[1]] = pred[[1]]
# PPt[[2]] = pred[[3]]
# 
# save(PPt, file=paste(dir,'PPt.RData', sep='')
#------------------------------------------------------------------------------#

################################################################################
# Apertura de los archivos .nc de MonteCarlo_ANOVA.R de PPt y diferencia con 
# la PPt del MME original
#------------------------------------------------------------------------------#
# variable temp (1), prec (2)
for (v in 1:2){
  
  files = Sys.glob(paste(dir, variable[v], '/PPt_', variable2[v], '*', sep =''))
  
  dif_MME = function(file){ # Funcion para operar con mclapply
    
    lon2 = read.table("lon2.txt")[,1]
    lat2 = read.table("lat2.txt")[,1]
    load('/pikachu/datos/luciano.andrian/aux_nmme_quantiles/PPt.RData')
    
    nc = nc_open(file)
    nc = ncvar_get(nc, 'SSterms')[,,,1] # la variable quedo del mismo nombre que los SS*
    aux = nc - PPt[[v]]
    
    #Nombre del archivo .nc a guardar -----------------------------------------#
    namefile = strsplit(file, split = "/")[[1]][length(strsplit(file, split = "/")[[1]])]
    # nc_create aveces enloquece por alguna razon.
    namefile = strsplit(namefile, split ='.n')[[1]][1] 
    
    WriteNC_diff(tosave=aux, paste_name=paste('diff_', variable[v],
                                              '/', 'diff_', namefile, sep=''))
  }
  # Podria usar mas cores, no ocupa ram y es rapida.
  mclapply(files,dif_MME, mc.cores = 15, mc.preschedule = F)
  
} 

# diferencia MME_wo_CFSv2 (6) con el MME completo ##############################
load("ss_PP.RData")
load("ss_T.RData")
load('/pikachu/datos/luciano.andrian/aux_nmme_quantiles/PPt.RData')
pred_wo = pp_test(ss_temp = ss_T, ss_pp = ss_PP, ensemble_total = "no")

# Temp ------------------------------------------------------------------------#
dif_temp_wo_cfsv2 = pred_wo[[6]][[1]] - PPt[[1]]
WriteNC_diff(tosave=dif_temp_wo_cfsv2, 
             paste_name=paste('diff_Temp/', 'diff_temp_wo_cfsv2',  sep=''))

# Prec ------------------------------------------------------------------------#
dif_prec_wo_cfsv2 = pred_wo[[6]][[3]] - PPt[[2]]
WriteNC_diff(tosave=dif_prec_wo_cfsv2, 
             paste_name=paste('diff_Prec/', 'diff_prec_wo_cfsv2',  sep=''))


################################################################################
rm(list =ls())
################################################################################
# Correr PaperNMME_AUX_MC.py
# Calcula los percentiles usando dask.
################################################################################

################################################################################
# Comparar los quintiles de la distribucion random_MME - MME_original
# con el valor de MME_wo_cfsv2 - MME_original
library(ncdf4)
source('funciones.R')
dir = "/pikachu/datos/luciano.andrian/aux_nmme_quantiles/"
variable = c('Temp', 'Prec')
variable2 = c('temp', 'pp')
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
#------------------------------------------------------------------------------#
pred_MME = pred_MME_wo = list()
load("ss_PP.RData")
load("ss_T.RData")
load('/pikachu/datos/luciano.andrian/aux_nmme_quantiles/PPt.RData')
pred_wo = pp_test(ss_temp = ss_T, ss_pp = ss_PP, ensemble_total = "no")
pred_wo_cfs = list()
pred_wo_cfs[[1]] = pred_wo[[6]][[1]] #temp
pred_wo_cfs[[2]] = pred_wo[[6]][[3]]#prec

percentiles = c(.05, .1, .2, .3, .4, .5, .6, .7, .8, .9, .95)

#para plotear igual que en el paper -------------------------------------------#
colorbars = list()
colorbars[[1]] = "YlOrRd"; colorbars[[2]] = "PuBuGn"
seasons = c("MAM", "JJA", "SON", "DJF")
letter.size = 10
colorbar.length = 18
colorbar.size = 10
# letter.size = 8
# colorbar.length = 14
# colorbar.size = 6
lats.size = 4 #por ahora borradas
height.fig = 4.5
width.fig = 6.85

#------------------------------------------------------------------------------#
g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}
library(gridExtra)
#------------------------------------------------------------------------------#
# Ploteo de reguiones donde se superan determinados percentiles
# sigue la forma de fig_paper2021.R

for(v in c(1,2)){
  
  # abriendo quantiles
  qt_v = nc_open(paste(dir, 'diff_qt_PPt_', variable2[v], '.nc', sep=''))
  qt_v = ncvar_get(qt_v, 'dif')
  # > dim(qt_v)
  # [1] 56 76  4  11 --> , seasons, quantiles [..., .6, .7, .8, .9, .95]
  
  dif_or = nc_open(paste(dir, 'diff_',variable[v], 
                         '/diff_', tolower(variable[v]), '_wo_cfsv2.nc', sep=''))
  dif_or = ncvar_get(dif_or, 'dif')
  # > dim(dif_or)
  # [1] 56 76  4
  
  for (i in 7:11){ # desde los persentiles 0.6-> 0.95
    # Mascara para regiones donde se superan los percentiles
    aux = dif_or
    aux[which(dif_or<qt_v[,,,i])] = NA
    aux[which(!is.na(aux))] = 1
    aux[is.nan(aux)] = NA

    # Ploteo por seasons
    for(season in 1:4){
      
      pred_MME[[season]] = mapa_topo3(variable = PPt[[v]], variable.sig = NULL
                                      , colorbar = colorbars[[v]], revert = F, escala = seq(0,1,0.1)
                                      , titulo = seasons[[season]]
                                      , label.escala = NULL, mapa = "SA"
                                      , width = 20, height = 20, title.size = 13, na.fill = -1000
                                      , sig = F, color.vsig = "black", alpha.vsig = 1
                                      , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                                      , cajas = F, lon = lon2, lat = lat2
                                      , type.sig = "point", estacion = season
                                      , mostrar = T, save = F, margen.zero = F
                                      ,  cb.v.w = 0.5, cb.v.h = colorbar.length, cb.size = colorbar.size
                                      , lats.size = 6, letter.size = letter.size
                                      , color.vcont = "black", nivel.vcont = rc)
      # sin cfsv2
      pred_MME_wo[[season]] = mapa_topo3(variable = pred_wo_cfs[[v]], variable.sig = aux
                                         , colorbar = colorbars[[v]], revert = F, escala = seq(0,1,0.1)
                                         , titulo = paste("percentil", percentiles[i]) 
                                         , label.escala = NULL, mapa = "SA"
                                         , width = 20, height = 20, title.size = 13, na.fill = -1000
                                         , sig = T, color.vsig = "black", alpha.vsig = 1
                                         , r = 4, estaciones = T, altura.topo = 2500, size.point = .01
                                         , cajas = F, lon = lon2, lat = lat2
                                         , type.sig = "point2", estacion = season
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
    
    
    nombre_fig = paste("/home/luciano.andrian/tesis/aux_pics_2022/", 
                       ifelse(v == 1, yes = "t-", no = "pp-")
                       , "pred_qt", percentiles[i], ".png", sep = "")
    
    
    ggsave(nombre_fig,plot =  p1
           ,dpi = 300, device = png, height = height.fig, width = width.fig
           , units = "in")

  }  
}
################################################################################