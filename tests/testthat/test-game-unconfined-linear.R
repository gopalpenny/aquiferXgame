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
                 'crs','c0rs','gs','gf','es','ef','l','PHIff','PHIfrT','PHIfs','PHIsf','PHIsrT','PHIss','PHIsrN','PHIfrN'))
  expect_equal(results_q %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','qshat','qsstar','qsdouble','qfhat','qfstar','qfdouble'))
  expect_equal(results_u %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','Us_hat','Us_star','Us_double','Us_hat_double','Us_double_double','Uf_hat','Uf_star','Uf_double','Uf_hat_double','Uf_double_double'))
  expect_equal(results_d %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','ds_hat','ds_star','ds_double','ds_hat_double','ds_double_double','df_hat','df_star','df_double','df_hat_double','df_double_double'))
})



### TEST SPECIFIC CASES

# uconfined aquifers:
# first estimate of first best produces negative qs, maximum qf=5. second estimate must have qs>0
df_input <- tibble::tibble(B=0.91, dBf=75.4, dBs=75.4, h0f=45.3, h0s=42.7, p0s=100, PHIff=110, PHIfrN=97.5, PHIfrT=97.5, PHIfs=91.4, PHIsf=91.4, PHIsrN=121, PHIsrT=121, PHIss=110, year=1989, row=50, Qs=15, Qf=5, gs=1, gf=1, p0f=5e+05, crs=1e+05, rmN=8.2, rmT=8.2, es=0, ef=0, l=1)
df_output <- tibble::tibble(treaty="N", zRange=0, zMinSwiss=0, zMaxFrench=0, qshat=15, qsstar=15, qsdouble=15, qfhat=5, qfstar=5, qfdouble=5, ds_hat=48.7825, ds_star=48.7825, ds_double=48.7825, ds_hat_double=48.7825, ds_double_double=48.7825, df_hat=44.8944, df_star=44.8944, df_double=44.8944, df_hat_double=44.8944, df_double_double=44.8944)
df_results <- suppressWarnings(evaluate_treaty_cases(df_input,'qd'))
# df_results %>% mutate_if(is.numeric,function(x) round(x,4)) %>% ggp::print_data_frame_for_entry(single_line = TRUE)
test_that("evaluate_treaty_cases first estimate of first best produces negative qs, maximum qf=5. second estimate must have qs>0",{
  expect_equal(df_results %>% tidyr::gather(var,val,-treaty) %>% dplyr::mutate(val=round(val,4)),df_output %>% tidyr::gather(var,val,-treaty))
})


# Unconfined aquifer fully depleted
df_input_depleted <- data.frame(B=0.91, c0rs = 1e5, dBf=75.4, dBs=75.4, h0f=45.3, h0s=42.7, p0s=100, PHIff=110, PHIfrN=97.5, PHIfrT=97.5, PHIfs=91.4, PHIsf=91.4, PHIsrN=121, PHIsrT=121, PHIss=110, year=1956, row=17, Qs=17, Qf=0, gs=0.6, gf=0.6, p0f=5e+05, crs=5e+05, rmN=0, rmT=0, es=0, ef=0, l=1)
df_output_depleted <- tibble::tibble(treaty="D", zRange=NaN, zMinSwiss=NaN, zMaxFrench=0, AD_fb=TRUE, AD_nash=TRUE, AD_cheat=FALSE, qshat=17, qsstar=17, qsdouble=15.3, qfhat=0, qfstar=0, qfdouble=0, ds_hat=NaN, ds_star=NaN, ds_double=63.5556, ds_hat_double=63.5556, df_hat=53.0776, df_star=53.0776, df_double=53.0776, df_hat_double=53.0776)
test_that('evaluate_treaty_cases returns \'D\' for depleted aquifer',{
  expect_warning(evaluate_treaty_cases(df_input_depleted,'qd'),paste("(Column l contains values equal to 1)|",
                                                                     "(NaNs produced)|",
                                                                     "(The aquifer was fully depleted for at least one player in some parameter sets in the First Best, Nash scenario\\(s\\))"))
  # expect_equal(evaluate_treaty_cases(df_input_depleted,'qd') %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)),
  #              df_output_depleted)
})


# Unconfined aquifer, q_double is not less than q_hat
# in row 1, qshat is 17, qsdouble should be 17
# in row 2, qfhat is 5, qfdouble should be 5
df_input_qdouble <- data.frame(B=c(0.91, 0.91), dBf=c(75.4, 75.4), dBs=c(75.4, 75.4), h0f=c(45.3, 45.3), h0s=c(42.7, 42.7), p0s=c(1e+05, 1e+05), year=c(1956, 1976), row=c(17, 37), Qs=c(17, 20), Qf=c(0, 5), gs=c(0.6, 0.925), gf=c(0.6, 0.925), p0f=c(1000, 200), crs=c(50000, 10000), rmN=c(0, 8.2), rmT=c(0, 8.2), es=c(0, 0), ef=c(0, 0), drawdown_years=c(5, 50),
                               PHIff=c(34.9166483561281, 109.80662752661), PHIfrT=c(18.6388805781934, 97.4085453273349), PHIfs=c(14.362429420851, 91.3701219010559), PHIsf=c(14.3624294208507, 91.3701219010546), PHIsrT=c(29.6525338473577, 120.883872795857), PHIss=c(25.9900272148918, 110.030353031565), PHIsrN=c(59.3050676947154, 241.767745591714), PHIfrN=c(9.3194402890967, 48.7042726636675), l = c(1,1))
df_output_qdouble <- tibble::tibble(treaty=c("N", "N"), zRange=c(0, -328.9488), zMinSwiss=c(0, 388.6396), zMaxFrench=c(0, 59.6909), qshat=c(17, 20), qsstar=c(17, 20), qsdouble=c(17, 20), qfhat=c(0, 5), qfstar=c(0, 5), qfdouble=c(0, 5), ds_hat=c(38.232, 62.8668), ds_star=c(38.232, 41.513), ds_double=c(38.232, 62.8668), ds_hat_double=c(38.232, 62.8668), ds_double_double=c(38.232, 62.8668), df_hat=c(32.8803, 53.6192), df_star=c(32.8803, 66.738), df_double=c(32.8803, 53.6192), df_hat_double=c(32.8803, 53.6192), df_double_double=c(32.8803, 53.6192))
df_qdouble_results <- suppressWarnings(evaluate_treaty_cases(df_input_qdouble,'qd'))
# df_qdouble_results %>% mutate_if(is.numeric,function(x) round(x,4)) %>% ggp::print_data_frame_for_entry(single_line = TRUE)
test_that('evaluate_treaty_cases returns qx_double equal to qi_hat and qi_star in cases where qj is 0',{
  expect_equal(df_qdouble_results %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)),
               df_output_qdouble)
})
