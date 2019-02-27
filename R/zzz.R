.onLoad <- function(libname, pkgname) {

      packageStartupMessage('
mediascores: News-sharing Ideology from Social Media Link Data.
Version 0.0.1.9000 (Development Version)
copyright (c) 2019, Gregory Eady, New York University
                    Fridolin Linder, New York University
For citation information, type citation("mediascores").\n')

    # To add new .stan files, first restart R then uncomment below and
    # run devtools::load_all() TWO times.
    
    # source('tools/make_cc.R')
    # stanfiles = list.files('src/stan_files/', pattern = '.stan')
    # setwd('src/')
    # for(f in stanfiles) {
    #    make_cc(paste0('stan_files/', f))
    # }
    # setwd('../')

    modules <- paste0("stan_fit4", names(stanmodels), "_mod")
    for (m in modules) loadModule(m, what = TRUE)
}
