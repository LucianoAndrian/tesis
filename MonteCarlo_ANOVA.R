# Monte Carlo - ANOVA
################################################################################
library(ncdf4)
source('funciones_MC.R')

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
################################################################################
# Seleccion de modelos al azar en ensambles de tamaño M.
# cada uno de los m modelos con su K_m miembros de ensamble
M = c(1:8)
k = c(10, 10, 12, 12, 4, 24, 10, 12)
################################################################################
RANOVA = function(i, variable = 'pp'){
  random_ensemble = sample(M, replace=T)
  random_k = k[random_ensemble]
  
  ss = anova_fun_MC(variable, random_ensemble, random_k)
  paste_name=paste('SS_', variable, i, sep="")
  WriteNC(tosave = ss, paste_name = paste_name)
  
  #sig = test_cos_MC(SS = ss, random_ensemble, random_k)
  
  ppot = pp_test_MC(ss_v = ss, random_ensemble, random_k)
  paste_name=paste('PPt_', variable, i, sep="")
  WriteNC(tosave = ppot, paste_name)
  
  rm(ss, ppot)
}

require(parallel)

j=1
while(j<10000){
  x = seq(j,(j+9))
  j = j + 10
  
  h = format(Sys.time(), "%H")
  d = format(Sys.time(), "%d") 
  if (d < 18){ # solo valido para julio 2022 XD
    cores = 10
  } else {
    if (as.integer(h) >= 19 | as.integer(h) <= 8){
      cores = 10
    } else {
      cores = 5
    }
  }
  mclapply(x,RANOVA, mc.cores = cores, mc.preschedule = F)
}
################################################################################