## code to prepare `example_params_unconfined` dataset goes here

params_default_unconfined <- function(area_km2=50) {
  par_default <- tibble::tibble(Qf=20, # total demand of French # 4
                                Qs=20, # total demand of Swiss
                                p0f=1, # alternative source cost per unit of water; de los Cobos, 2018
                                p0s=1, # alternative source cost per unit of water; de los Cobos, 2018
                                B=0.02, # cost per unit of water per meter depth; de los Cobos, 2018
                                PHIff=2, # m/MCM
                                PHIss=2, # m/MCM
                                PHIsf=1.5, # m/MCM
                                PHIfs=1.5, # m/MCM
                                dBs= 50, # m
                                dBf= 50, # m
                                h0s=48, # m
                                h0f=48, # m
                                rmN=8, #
                                rmT=8, #
                                rsN=(1/area_km2)*0.5, #m/MCM
                                rsT=(1/area_km2)*0.5, #m/MCM
                                rfN=(1/area_km2)*0.5, #m/MCM
                                rfT=(1/area_km2)*0.5, #m/MCM)
                                crs=0.1,
                                gs=0.5,
                                gf=0.5,
                                es=0,
                                ef=0)
  return(par_default)
}

example_params_unconfined <- params_default_unconfined()

usethis::use_data(example_params_unconfined)
