library(magrittr)

test_that('check_params returns warning if missing parameters',{
  expect_warning(check_params(example_params_confined %>% dplyr::select(-rmN)),"missing rmN in params")
  expect_warning(check_params(example_params_confined %>% dplyr::select(-rmN)),"missing rmN in params")
  expect_warning(check_params(example_params_confined %>% dplyr::select(-rmN)),"missing rmN in params")
})

params_confined <- example_params_confined
test_that('check_params returns warning if missing CONFINED parameters',{
  expect_warning(check_params(params_confined %>% dplyr::select(-Dff,-d0s)),"missing Dff, d0s in params")
  expect_warning(check_params(params_confined %>% dplyr::select(-d0s)),"missing d0s in params")
  expect_warning(check_params(params_confined %>% dplyr::select(-Dff)),"missing Dff in params")
})

params_unconfined <- example_params_unconfined
test_that('check_params returns correct aquifer type',{
  expect_equal(check_params(params_confined),"confined")
  expect_equal(check_params(params_unconfined),"unconfined")
})

params_multiple_un <- example_params_unconfined %>% dplyr::select(-gs) %>% tidyr::crossing(gs=c(0.2,0.8))
params_multiple_con <- example_params_confined %>% dplyr::select(-gs) %>% tidyr::crossing(gs=c(0.2,0.8))
test_that('check_params returns correct aquifer type for data.frame with multiple rows',{
  expect_equal(check_params(params_multiple_un),"unconfined")
  expect_equal(check_params(params_multiple_con),"confined")
})

test_that('check_params returns warning l is not in (0,1]',{
  expect_warning(check_params(params_unconfined %>% dplyr::mutate(l=1)),"Column l contains values equal to 1")
  expect_warning(check_params(params_unconfined %>% dplyr::mutate(l=-0.1)),"Column l contains values not in the range")
  expect_warning(check_params(params_unconfined %>% dplyr::mutate(l=1.1)),"Column l contains values not in the range")
})

test_that('check_params returns warning if missing UNCONFINED parameters',{
  expect_warning(check_params(params_unconfined %>% dplyr::select(-PHIff,-dBs)),"missing PHIff, dBs in params")
  expect_warning(check_params(params_unconfined %>% dplyr::select(-dBs)),"missing dBs in params")
  expect_warning(check_params(params_unconfined %>% dplyr::select(-PHIff)),"missing PHIff in params")
  expect_warning(check_params(params_unconfined %>% dplyr::select(-l)),"missing l in params")
})

params_both <- params_unconfined %>% dplyr::bind_cols(params_confined %>% dplyr::select(dplyr::matches("D[sfij][sfij]")))
test_that('check_params stops if UNCONFINED and CONFINED drawdown relationships supplied',{
  expect_error(check_params(params_both),"params should contain either Dix or PHIix, not both")
})

params_neither <- params_confined %>% dplyr::select(-dplyr::matches("D[sfij][sfij]"))
test_that('check_params stops if UNCONFINED and CONFINED drawdown relationships supplied',{
  expect_error(check_params(params_neither),"Missing drawdown parameters \\(Dxx or PHIxx\\) in params.")
})


params_negative <- example_params_unconfined %>% dplyr::bind_rows(example_params_unconfined %>% dplyr::mutate(Qf=-10))
test_that('check_params gives warning if a param column has negative values',{
  expect_warning(check_params(params_negative),"param column\\(s\\), Qf, contain negative values.")
  expect_warning(check_params(params_negative %>% dplyr::mutate(PHIsf=c(-1,2))),"param column\\(s\\), Qf, PHIsf, contain negative values.")
})

params_highconnectivity <- example_params_confined %>% dplyr::mutate(Dsf=Dss)
test_that('check_params gives warning if a param column has negative values',{
  expect_error(check_params(params_highconnectivity),"Dij must be less than Dii")
  expect_warning(check_params(params_highconnectivity %>% dplyr::mutate(Dsf=Dss*0.999)),"Dij must be less than Dii")
})


params_unconfined_normal <- example_params_unconfined %>% dplyr::mutate(c2=c("text"),c3=c(-10))
params_confined_normal <- example_params_confined %>% dplyr::mutate(c2=c("text"),c3=c(-10))
test_that('check_params does NOT give warning if another column has negative values',{
  expect_warning(check_params(params_confined_normal),NA)
  expect_warning(check_params(params_unconfined_normal),NA)
})




# check_game_dynamics no longer used
# game_dynamics_table1 <- tibble::tibble(ds_max=17.7047,
#                  df_max=18.9515,
#                  ds_threshold=10,
#                  df_threshold=10,
#                  dynamics="possibly interesting")
# game_dynamics_table2 <- tibble::tibble(ds_max=2.5456,
#                  df_max=2.6165,
#                  ds_threshold=10,
#                  df_threshold=10,
#                  dynamics="no interesting")
# # check_game_dynamics(example_params_unconfined %>% dplyr::mutate(Qs=1,Qf=1),text_results = FALSE) %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)) %>% ggp::print_data_frame_for_entry()
# # cat(check_game_dynamics(example_params_unconfined))
# test_that('check_game_dynamics works for table output',{
#   expect_equal(check_game_dynamics(example_params_unconfined,text_results = FALSE) %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)),
#                game_dynamics_table1)
#   expect_equal(check_game_dynamics(example_params_unconfined %>% dplyr::mutate(Qs=1,Qf=1),text_results = FALSE) %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)),
#                game_dynamics_table2)
# })
# test_that('check_game_dynamics works for text output',{
#   expect_equal(grepl("possibly interesting dynamics",check_game_dynamics(example_params_unconfined)),TRUE)
#   expect_equal(grepl("no interesting dynamics",check_game_dynamics(example_params_unconfined %>% dplyr::mutate(Qs=1,Qf=1))),TRUE)
# })
