.onLoad <- function(libname, pkgname) {
    #source('tools/make_cc.R')
    #stanfiles = list.files('src/stan_files/', pattern = '.stan')
    #setwd('src/')
    #for(f in stanfiles) {
    #    make_cc(paste0('stan_files/', f))
    #}
    #setwd('../')
    modules <- paste0("stan_fit4", names(stanmodels), "_mod")
    for (m in modules) loadModule(m, what = TRUE)
}
