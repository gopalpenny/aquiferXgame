## code to prepare `example_params_unconfined` dataset goes here
library(magrittr)
bounds_df <- data.frame(bound_type=c("PB","PB","PB","PB"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer <- anem::define_aquifer("unconfined",Ksat=1e-3,bounds=bounds_df)
wells <- anem::wells_example %>% dplyr::mutate(country=as.character(country),R=5000) %>%
  dplyr::bind_rows(tibble::tibble(x=500,y=500,diam=1,R=1000,country="C",weights=1,Q=0.25)) %>%
  anem::define_wells() %>%
  anem::generate_image_wells(aquifer) %>%
  dplyr::mutate(country=factor(country,levels=c("A","B","C"),labels=c("s","f","r")))
drawdown <- anem::get_drawdown_relationships(wells,aquifer,country,weights) %>%
  dplyr::mutate(var=gsub("_","",var),
                pot=pot * 1e6 / (365 * 24 * 3600) )

params_default_unconfined <- function(drawdown) {
  drawdown_wide <- drawdown %>% dplyr::select(var,pot) %>% dplyr::filter(!grepl("PHIr",var)) %>%
    dplyr::select(var,pot) %>% tidyr::spread(var,pot) %>%
    dplyr::rename(PHIsrT=PHIsr,PHIfrT=PHIfr) %>%
    dplyr::mutate(PHIsrN=PHIsrT,PHIfrN=PHIfrT)
  par_default <- tibble::tibble(Qf=20, # total demand of French # 4
                                Qs=20, # total demand of Swiss
                                p0f=1, # alternative source cost per unit of water; de los Cobos, 2018
                                p0s=1, # alternative source cost per unit of water; de los Cobos, 2018
                                B=0.1, # cost per unit of water per meter depth; de los Cobos, 2018
                                dBs= 50, # m
                                dBf= 50, # m
                                h0s=48, # m
                                h0f=48, # m
                                rmN=1, #
                                rmT=1, #
                                crs=0.1,
                                gs=0.5,
                                gf=0.5,
                                es=0,
                                ef=0) %>%
    dplyr::bind_cols(drawdown_wide)
  return(par_default)
}

example_params_unconfined <- params_default_unconfined(drawdown)

usethis::use_data(example_params_unconfined,overwrite = TRUE)
