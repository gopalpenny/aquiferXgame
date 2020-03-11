## code to prepare `example_params_confined` dataset goes here

params_default_confined <- function(area_km2=50) {
  par_default <- tibble::tibble(Qf=20, # total demand of French # 4
                                Qs=20, # total demand of Swiss
                                p0f=1, # alternative source cost per unit of water; de los Cobos, 2018
                                p0s=1, # alternative source cost per unit of water; de los Cobos, 2018
                                B=0.02, # cost per unit of water per meter depth; de los Cobos, 2018
                                Dff=2, # m/MCM
                                Dss=2, # m/MCM
                                Dsf=1.5, # m/MCM
                                Dfs=1.5, # m/MCM
                                d0s=2, # m
                                d0f=2, # m
                                rmN=8, #
                                rmT=8, #
                                DrsN=(1/area_km2)*0.5, #m/MCM
                                DrsT=(1/area_km2)*0.5, #m/MCM
                                DrfN=(1/area_km2)*0.5, #m/MCM
                                DrfT=(1/area_km2)*0.5, #m/MCM)
                                crs=0.1,
                                gs=0.5,
                                gf=0.5,
                                es=0,
                                ef=0)
  return(par_default)
}

example_params_confined <- params_default_confined()

usethis::use_data(example_params_confined,overwrite = TRUE)
