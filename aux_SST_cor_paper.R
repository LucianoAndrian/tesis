# SSTs
library(ncdf4)
dir = '/pikachu/datos/luciano.andrian/aux_nmme_quantiles/sst/'
################################################################################
modelos = c("COLA-CCSM4", "CM2p1",
             "FLOR-A06", "FLOR-B01", "NASA-GEOSS2S",
            "NCEP-CFSv2", "CanCM4i", "GEM-NEMO") 

################################################################################
# Apertura de los .nc
#
#Todos los archivos comienzan en Dec 1981 y terminan en Dec 2010
# menos el CFSv2 (6) que termine en Nov 2010
#
#Los modelos canadienses (7 y 8) estan en Kelvin con la tierra como cero
#------------------------------------------------------------------------------#
# lead 0 para el mes previo a la estacion pronosticada
# ej. M para JJA
# se deben seleccionar los meses Feb (2), May (5), Aug (8) y Nov (11)
################################################################################

month_ci = c(2, 5, 8, 11)

# un serie para cada region. 
# una serie para cada seasons, para cada modelo
sst_ci_N34 = array(data = NA, dim = c(29, 4, 8))
sst_ci_TNA = array(data = NA, dim = c(29, 4, 8))

for (m in modelos){
  if (m == modelos[6]){ #CFSv2
    last_month = 348
  } else {
    last_month = 349
  }
  
  if (m == modelos[7] | m == modelos[8]){
    rest = 273
  } else {
    rest = 0
  }
  
  nc = nc_open(paste(dir, m, '_sst.nc', sep=''))
  sst = ncvar_get(nc, 'sst')[,,,2:last_month]
  sst[sst == 0] = NA #Solo ocurre para los mod. candienses
  sst = sst - rest
  
  # Lon lat de N34 y TNA ----------------------------------------------------#
  lat = ncvar_get(nc, 'Y')
  lon = ncvar_get(nc, 'X')
  
  lats_n34 =  seq(which(lat == -5), which(lat == 5), by = 1)
  lons_n34 =  seq(which(lon == -120), which(lon == -170), by = 1)
  
  lats_tna =  seq(which(lat == 10), which(lat == 20), by = 1)
  lons_tna =  seq(which(lon == -35), which(lon == -60), by = 1)
  
  # Seleccion y  Promedio de N34 y TNA ----------------------------------------#
  
  mean_n34 = apply(sst[lons_n34, lats_n34,,], c(4), mean, na.rm = T)

  mean_tna = apply(sst[lons_tna, lats_tna,,], c(4), mean, na.rm = T)
  
  # distribucio ---------------------------------------------------------------#
  m2 = which(modelos==m)
  for (s in 1:4){ #seasons
    for (y in 0:28){ #años
      sst_ci_N34[y + 1, s,m2] = mean_n34[month_ci[s]+12*y]
      sst_ci_TNA[y + 1, s,m2] = mean_tna[month_ci[s]+12*y]
    }
  }
}

save(sst_ci_N34, file = paste(dir, 'N34_sst','.RData', sep = ''))
save(sst_ci_TNA, file = paste(dir, 'TNA_sst','.RData', sep = ''))

################################################################################
# MME sin el modelo CFSv2 (6)
# La serie del modelo CFSv2, se puede obtener de lo guardaro previamente
# como sst_ci_N34[,,6]

# Eliminacion del modelo CFSv2 y promedio -------------------------------------#
sst_ci_N34_wo_cfsv2 = sst_ci_N34
sst_ci_N34_wo_cfsv2[,,6] = NA
sst_ci_N34_wo_cfsv2 = apply(sst_ci_N34_wo_cfsv2, c(1,2), mean, na.rm = T)

sst_ci_TNA_wo_cfsv2 = sst_ci_TNA
sst_ci_TNA_wo_cfsv2[,,6] = NA
sst_ci_TNA_wo_cfsv2 = apply(sst_ci_TNA_wo_cfsv2, c(1,2), mean, na.rm = T)
#------------------------------------------------------------------------------#

save(sst_ci_N34_wo_cfsv2, 
     file = paste(dir, 'sst_ci_N34_wo_cfsv2','.RData', sep = '')) 
save(sst_ci_TNA_wo_cfsv2, 
     file = paste(dir, 'sst_ci_TNA_wo_cfsv2','.RData', sep = ''))
################################################################################