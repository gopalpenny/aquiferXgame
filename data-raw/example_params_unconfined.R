## code to prepare `example_params_unconfined` dataset goes here

aquifer <- anem::aquifer_unconfined_example
wells <- anem::wells_example %>% dplyr::mutate(country=as.character(country)) %>%
  dplyr::bind_rows(tibble::tibble(x=500,y=500,diam=1,R=1000,country="C",weights=1,Q=0.25)) %>%
  anem::define_wells() %>%
  anem::generate_image_wells(aquifer) %>%
  dplyr::mutate(country=factor(country,levels=c("A","B","C"),labels=c("s","f","r")))
anem::get_drawdown_relationships(wells,aquifer,country,weights) %>%
  dplyr::mutate(var=gsub("fr","rf",gsub("sr","rs",gsub("_","",var))))

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
                                PHIsrN=(1/area_km2)*0.5, #m/MCM
                                PHIsrT=(1/area_km2)*0.5, #m/MCM
                                PHIfrN=(1/area_km2)*0.5, #m/MCM
                                PHIfrT=(1/area_km2)*0.5, #m/MCM
                                crs=0.1,
                                gs=0.5,
                                gf=0.5,
                                es=0,
                                ef=0)
  return(par_default)
}

example_params_unconfined <- params_default_unconfined()

usethis::use_data(example_params_unconfined,overwrite = TRUE)
