"""
Calcular quintiles de los outputs de ~tesis/MonteCarlo_ANOVA.R
"""
#----------------------------------------------------------------------------------------------------------------------#
import xarray as xr
#----------------------------------------------------------------------------------------------------------------------#
dir = '/pikachu/datos/luciano.andrian/aux_nmme_quantiles/'
subdir = ['Temp', 'Prec']
terms = ['SS', 'PPt']
variables = ['temp', 'pp']
#----------------------------------------------------------------------------------------------------------------------#
for i in [0,1]:
    for t in terms:
        path = dir + subdir[i] + '/' + t + '*'
        print(path)
        aux = xr.open_mfdataset(path, combine='nested', concat_dim="time",
                                coords="different", compat="broadcast_equals",
                                parallel=True)

        aux = aux.chunk({'time': -1})
        qt = aux.quantile([.1, .90], dim='time', interpolation='linear')

        qt.to_netcdf(dir + 'qt_90' + t + '_' + variables[i] + '.nc', compute=True)
        del aux, qt
#----------------------------------------------------------------------------------------------------------------------#