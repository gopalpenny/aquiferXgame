# # test-game-unconfined-nonlinear

# output_0 <- tibble::tibble(treaty="Y", zRange=0.0097, zMinSwiss=-0.0239, zMaxFrench=-0.0143, qshat=6.0392, qsstar=6.9265, qsdouble=7.0388, qfhat=5.1458, qfstar=6.1737, qfdouble=6.2586, Us_hat=-17.594, Us_star=-17.6944, Us_double=-17.5286, Us_hat_double=-17.7469, Uf_hat=-17.8731, Uf_star=-17.9174, Uf_double=-17.7897, Uf_hat_double=-17.9901, ds_hat=5.8505, ds_star=6.527, ds_double=6.3468, ds_hat_double=6.3468, df_hat=5.8667, df_star=6.6266, df_double=6.4684, df_hat_double=6.4684)
output_1 <- tibble::tibble(treaty="Y", zRange=0.0097, zMinSwiss=-0.0239, zMaxFrench=-0.0143, qshat=6.0392, qsstar=6.9265, qsdouble=7.0388, qfhat=5.1458, qfstar=6.1737, qfdouble=6.2586, Us_hat=-17.594, Us_star=-17.6944, Us_double=-17.5286, Us_hat_double=-17.7469, Uf_hat=-17.8731, Uf_star=-17.9174, Uf_double=-17.7897, Uf_hat_double=-17.9901, ds_hat=5.8505, ds_star=6.527, ds_double=6.3468, ds_hat_double=6.3468, df_hat=5.8667, df_star=6.6266, df_double=6.4684, df_hat_double=6.4684)
output_2 <- tibble::tibble(treaty="Y", zRange=0.0087, zMinSwiss=-0.022, zMaxFrench=-0.0133, qshat=5.5893, qsstar=6.3833, qsdouble=6.4862, qfhat=4.7578, qfstar=5.6811, qfdouble=5.7583, Us_hat=-17.7563, Us_star=-17.8479, Us_double=-17.6968, Us_hat_double=-17.8955, Uf_hat=-18.0133, Uf_star=-18.0532, Uf_double=-17.9372, Uf_hat_double=-18.1196, ds_hat=5.5414, ds_star=6.1428, ds_double=5.9833, ds_hat_double=5.9833, df_hat=5.5573, df_star=6.2337, df_double=6.0941, df_hat_double=6.0941)
output_3 <- tibble::tibble(treaty="Y", zRange=0.0085, zMinSwiss=-0.0216, zMaxFrench=-0.013, qshat=5.4925, qsstar=6.2677, qsdouble=6.3686, qfhat=4.6743, qfstar=5.5765, qfdouble=5.6521, Us_hat=-17.7925, Us_star=-17.8822, Us_double=-17.7342, Us_hat_double=-17.9288, Uf_hat=-18.0446, Uf_star=-18.0835, Uf_double=-17.97, Uf_hat_double=-18.1485, ds_hat=5.4752, ds_star=6.0615, ds_double=5.9062, ds_hat_double=5.9062, df_hat=5.491, df_star=6.1507, df_double=6.0147, df_hat_double=6.0147)

test_that("evaluate_treaty_cases \"qud\"works for unconfined nonlinear cost solution",{
  expect_equal(evaluate_treaty_cases(example_params_unconfined %>% dplyr::mutate(l=0.999999),return_criteria = "qud") %>%
                 dplyr::mutate_if(is.numeric,function(x) round(x,4)),output_1)
  expect_equal(evaluate_treaty_cases(example_params_unconfined %>% dplyr::mutate(l=0.2),return_criteria = "qud") %>%
                 dplyr::mutate_if(is.numeric,function(x) round(x,4)),output_2)
  expect_equal(evaluate_treaty_cases(example_params_unconfined %>% dplyr::mutate(l=0),return_criteria = "qud") %>%
                 dplyr::mutate_if(is.numeric,function(x) round(x,4)),output_3)
})

params_2rows <- example_params_unconfined %>%
  rbind(example_params_unconfined %>% dplyr::mutate(gs=0.7,PHIsf=1))
results_p <- evaluate_treaty_cases(params_2rows,"p")
results_a <- evaluate_treaty_cases(example_params_unconfined,"a")
results_q <- evaluate_treaty_cases(example_params_unconfined,"q")
results_u <- evaluate_treaty_cases(example_params_unconfined,"u")
results_d <- evaluate_treaty_cases(example_params_unconfined,"d")
test_that("evaluate_treaty_cases UNCONFINED nonlinear returns all input parameters for options p, a, q, u, d input",{
  expect_equal(results_p %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','gs','PHIsf'))
  expect_equal(nrow(results_p),2)
  expect_equal(results_a %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','Qf','Qs','p0f','p0s','B','dBs','dBf','h0s','h0f','rmN','rmT',
                 'crs','c0rs','gs','gf','es','ef','l','PHIff','PHIfrT','PHIfs','PHIsf','PHIsrT','PHIss','PHIsrN','PHIfrN'))
  expect_equal(results_q %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','qshat','qsstar','qsdouble','qfhat','qfstar','qfdouble'))
  expect_equal(results_u %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','Us_hat','Us_star','Us_double','Us_hat_double','Uf_hat','Uf_star','Uf_double','Uf_hat_double'))
  expect_equal(results_d %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','ds_hat','ds_star','ds_double','ds_hat_double','df_hat','df_star','df_double','df_hat_double'))
})
