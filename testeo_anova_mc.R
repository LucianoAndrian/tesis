source("funciones.R")
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
# MME ##########################################################################
# ss_temp = anova_fun(variable = "temp", ensemble_total = "si") 
# # sig_temp_emm = test_cos(ss_temp, ensemble_total = "si", nomodel_selec = "no"
# #                         , no_model = "no")
# 
# ss_pp = anova_fun(variable = "pp", ensemble_total = "si")
# # sig_pp_emm = test_cos(ss_pp, ensemble_total = "si", nomodel_selec = "no"
#                       , no_model = "no")
# MME wo. Model ################################################################

load('ss_T.RData')
load('ss_PP.RData')
pred_wo =pp_test(ss_temp = ss_T, ss_pp = ss_PP, ensemble_total = "no")
################################################################################
GetVar = function(file_name){
  library(ncdf4)
  dir = "/pikachu/datos/luciano.andrian/aux_nmme_quantiles/"
  aux = nc_open(paste(dir, file_name, ".nc", sep = ""))
  v = ncvar_get(aux, 'SSterms')
  return(v)
}
################################################################################
# Por encima del percentil 95 -------------------------------------------------#
variable = c('pp', 'temp')
for (v in variable){
  if (v == 'pp'){
    v2 = 3
  } else {
    v2 = 1
  }
  
  print(v)
  v_ppt = GetVar(paste('qt_PPt_', v, sep=''))
  
  for (m in c(1:8)){
    for (s in c(1:4)){
      if (length(pred_wo[[m]][[v2]][,,s][which(pred_wo[[m]][[v2]][,,s]>v_ppt[,,s,1,2])]) > 0){
        print('')
        print(paste('Modelo', m))
        print(paste('Seasons', s))
      }
    }
  }
  
}
# Nunca, para ninguna variable #

#------------------------------------------------------------------------------#
# Por encima del percentil 90 -------------------------------------------------#

variable = c('pp', 'temp')
for (v in variable){
  if (v == 'pp'){
    v2 = 3
  } else {
    v2 = 1
  }
  
  print(v)
  v_ppt = GetVar(paste('qt_90PPt_', v, sep=''))
  
  for (m in c(1:8)){
    for (s in c(1:4)){
      if (length(pred_wo[[m]][[v2]][,,s][which(pred_wo[[m]][[v2]][,,s]>v_ppt[,,s,1,2])]) > 0){
        print('')
        print(paste('Modelo', m))
        print(paste('Seasons', s))
      }
    }
  }
  
}
# Nunca, para ninguna variable #

################################################################################
################################################################################
# En algun caso se presentan MENOS 
# valores significativos en el percentil inferior? 

variable = c('pp', 'temp')
for (v in variable){
  if (v == 'pp'){
    v2 = 3
  } else {
    v2 = 1
  }
  
  print('#######################################################')
  print(v)
  v_ppt = GetVar(paste('qt_PPt_', v, sep=''))
  
  for (m in c(1:8)){
    for (s in c(1:4)){
      if (length(pred_wo[[m]][[v2]][,,s][which(pred_wo[[m]][[v2]][,,s]<v_ppt[,,s,1,1])]) > 0){
        print('')
        print(paste('Modelo', m))
        print(paste('Seasons', s))
        print(length(pred_wo[[m]][[v2]][,,s][which(pred_wo[[m]][[v2]][,,s]<v_ppt[,,s,1,1])]))
      }
    }
  }
}


