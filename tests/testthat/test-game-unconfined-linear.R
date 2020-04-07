# test-game-unconfined-linear

example_params_unconfined_linear <- example_params_unconfined %>% dplyr::mutate(l=1)

df <- tibble::tibble(treaty="Y",
                     zRange=0.0097,
                     zMinSwiss=-0.0239,
                     zMaxFrench=-0.0143,
                     qshat=6.0392,
                     qfhat=5.1458,
                     qsstar=6.9265,
                     qfstar=6.1737,
                     qsdouble=7.0388,
                     qfdouble=6.2586)
results1 <- suppressWarnings(evaluate_treaty(example_params_unconfined_linear))
test_that("evaluate_treaty returns correct zRange, q values for unconfined aquifer",{
  expect_equal(results1 %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)),
               df)
})


results_a <- suppressWarnings(evaluate_treaty_cases(example_params_unconfined_linear,"a"))
results_q <- suppressWarnings(evaluate_treaty_cases(example_params_unconfined_linear,"q"))
results_u <- suppressWarnings(evaluate_treaty_cases(example_params_unconfined_linear,"u"))
results_d <- suppressWarnings(evaluate_treaty_cases(example_params_unconfined_linear,"d"))
# paste(evaluate_treaty_cases(example_params_unconfined,"a") %>% names(),collapse="\',\'")
test_that("evaluate_treaty_cases UNCONFINED returns all input parameters for options a, q, u, d input",{
  expect_equal(results_a %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','Qf','Qs','p0f','p0s','B','dBs','dBf','h0s','h0f','rmN','rmT',
                 'crs','gs','gf','es','ef','l','PHIff','PHIfrT','PHIfs','PHIsf','PHIsrT','PHIss','PHIsrN','PHIfrN'))
  expect_equal(results_q %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','qshat','qsstar','qsdouble','qfhat','qfstar','qfdouble'))
  expect_equal(results_u %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','Us_hat','Us_star','Us_double','Us_hat_double','Uf_hat','Uf_star','Uf_double','Uf_hat_double'))
  expect_equal(results_d %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','ds_hat','ds_star','ds_double','ds_hat_double','df_hat','df_star','df_double','df_hat_double'))
})
