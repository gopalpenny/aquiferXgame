# test-game-confined-linear


## evaluate_treaty_cases
df_cases <- tibble::tibble(treaty="Y", zRange=0.0443, zMinSwiss=-0.0222, zMaxFrench=0.0222, qshat=6.8686, qsstar=8.7418, qsdouble=9.0376, qfhat=6.8686, qfstar=8.7418, qfdouble=9.0376, Us_hat=-17.4976, Us_star=-17.7432, Us_double=-17.2388, Us_hat_double=-17.9445, Us_double_double=-17.8269, Uf_hat=-16.6976, Uf_star=-16.9432, Uf_double=-16.4388, Uf_hat_double=-17.1445, Uf_double_double=-17.0269, ds_hat=25.96, ds_star=32.5164, ds_double=30.298, ds_hat_double=29.2135, ds_double_double=33.5516, df_hat=25.96, df_star=32.5164, df_double=30.298, df_hat_double=29.2135, df_double_double=33.5516)
# evaluate_treaty_cases(example_params_confined,"qud") %>% dplyr::mutate_if(is.numeric,function(x) round(x,digits=4)) %>% ggp::print_data_frame_for_entry(single_line = T)
test_that("evaluate_treaty_cases returns correct zRange, q, U, and d values",{
  expect_equal(evaluate_treaty_cases(example_params_confined,"qud") %>% dplyr::mutate_if(is.numeric,function(x) round(x,digits=4)),
               df_cases)
})

# paste(evaluate_treaty_cases(example_params_confined,"d") %>% names(),collapse="\',\'")
test_that("evaluate_treaty_cases CONFINED returns all input parameters for options a, q, u, d input",{
  expect_equal(evaluate_treaty_cases(example_params_confined,"a") %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','Qf','Qs','p0f','p0s','B','Dff','Dss','Dsf','Dfs',
                 'd0s','d0f','rmN','rmT','DsrN','DsrT','DfrN','DfrT','crs','c0rs','gs','gf','es','ef'))
  expect_equal(evaluate_treaty_cases(example_params_confined,"q") %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','qshat','qsstar','qsdouble','qfhat','qfstar','qfdouble'))
  expect_equal(evaluate_treaty_cases(example_params_confined,"u") %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','Us_hat','Us_star','Us_double','Us_hat_double','Us_double_double','Uf_hat','Uf_star','Uf_double','Uf_hat_double','Uf_double_double'))
  expect_equal(evaluate_treaty_cases(example_params_confined,"d") %>% names(),
               c('treaty','zRange','zMinSwiss','zMaxFrench','ds_hat','ds_star','ds_double','ds_hat_double','ds_double_double','df_hat','df_star','df_double','df_hat_double','df_double_double'))
})


params_c0rs <- rbind(
  example_params_confined,
  example_params_confined %>% dplyr::mutate(c0rs=c0rs+1000))
params_c0rs_out <- evaluate_treaty_cases(params_c0rs,"qup")
params_c0rs_check <- tibble::tibble(treaty=c("Y", "Y"), zRange=c(0.044324885034313, 0.044324885034313), zMinSwiss=c(-0.0221624425171565, -0.0221624425171565), zMaxFrench=c(0.0221624425171565, 0.0221624425171565), qshat=c(6.86857142857143, 6.86857142857143), qsstar=c(8.74181818181818, 8.74181818181818), qsdouble=c(9.0375939849624, 9.0375939849624), qfhat=c(6.86857142857143, 6.86857142857143), qfstar=c(8.74181818181818, 8.74181818181818), qfdouble=c(9.0375939849624, 9.0375939849624), Us_hat=c(-17.4975908571429, -1017.49759085714), Us_star=c(-17.7432245950413, -1017.74322459504), Us_double=c(-17.2388346203856, -1017.23883462039), Us_hat_double=c(-17.9445334479055, -1017.94453344791), Us_double_double=c(-17.8269169766522, -1017.82691697665), Uf_hat=c(-16.6975908571429, -16.6975908571429), Uf_star=c(-16.9432245950413, -16.9432245950413), Uf_double=c(-16.4388346203855, -16.4388346203855), Uf_hat_double=c(-17.1445334479055, -17.1445334479055), Uf_double_double=c(-17.0269169766522, -17.0269169766522), c0rs=c(0, 1000))
test_that("confined game accounts for c0rs",{
  expect_equal(params_c0rs_out,params_c0rs_check)
})
