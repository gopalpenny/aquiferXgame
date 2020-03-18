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


test_that('check_params returns warning if missing UNCONFINED parameters',{
  expect_warning(check_params(params_unconfined %>% dplyr::select(-PHIff,-dBs)),"missing PHIff, dBs in params")
  expect_warning(check_params(params_unconfined %>% dplyr::select(-dBs)),"missing dBs in params")
  expect_warning(check_params(params_unconfined %>% dplyr::select(-PHIff)),"missing PHIff in params")
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
test_that('check_params gives warning if a columns has negative values',{
  expect_warning(check_params(params_negative),"param column\\(s\\), Qf, contain negative values.")
  expect_warning(check_params(params_negative %>% dplyr::mutate(PHIsf=c(-1,2))),"param column\\(s\\), Qf, PHIsf, contain negative values.")
})


game_dynamics_table1 <- tibble::tibble(ds_max=17.7047,
                 df_max=18.9515,
                 ds_threshold=10,
                 df_threshold=10,
                 dynamics="possibly interesting")
game_dynamics_table2 <- tibble::tibble(ds_max=2.5456,
                 df_max=2.6165,
                 ds_threshold=10,
                 df_threshold=10,
                 dynamics="no interesting")
# check_game_dynamics(example_params_unconfined %>% dplyr::mutate(Qs=1,Qf=1),text_results = FALSE) %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)) %>% ggp::print_data_frame_for_entry()
# cat(check_game_dynamics(example_params_unconfined))
test_that('check_game_dynamics works for table output',{
  expect_equal(check_game_dynamics(example_params_unconfined,text_results = FALSE) %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)),
               game_dynamics_table1)
  expect_equal(check_game_dynamics(example_params_unconfined %>% dplyr::mutate(Qs=1,Qf=1),text_results = FALSE) %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)),
               game_dynamics_table2)
})
test_that('check_game_dynamics works for text output',{
  expect_equal(grepl("possibly interesting dynamics",check_game_dynamics(example_params_unconfined)),TRUE)
  expect_equal(grepl("no interesting dynamics",check_game_dynamics(example_params_unconfined %>% dplyr::mutate(Qs=1,Qf=1))),TRUE)
})
