# test_game_evaluate.R

## evaluate_treaty
test_that("evaluate_agreement returns tibble",{
  expect_equal(class(evaluate_treaty(example_params_confined)),c("tbl_df","tbl","data.frame"))
})

df <- tibble::tibble(treaty="Y",
                 zRange=0.044324885034313,
                 zMinSwiss=-0.0221624425171565,
                 zMaxFrench=0.0221624425171565,
                 qshat=6.86857142857143,
                 qfhat=6.86857142857143,
                 qsstar=8.74181818181818,
                 qfstar=8.74181818181818,
                 qsdouble=9.0375939849624,
                 qfdouble=9.0375939849624) %>%
  dplyr::mutate_if(is.numeric,function(x) round(x,digits=4))
test_that("evaluate_treaty returns correct zRange, q values for confined aquifer",{
  expect_equal(evaluate_treaty(example_params_confined) %>% dplyr::mutate_if(is.numeric,function(x) round(x,digits=4)),
               df)
})


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

df_grid <- expand.grid(x=seq(-5,5,length.out=20),y=seq(-5,5,length.out=20))
df_grid$z <- sqrt(df_grid$x^2+df_grid$y^2)
cl_results <- tibble::tibble(x=c(-1.8421, -1.9824, -1.9824, -1.8421, -1.8375, -1.4976),
                             y=c(-0.7742, -0.2632, 0.2632, 0.7742, 0.7895, 1.3158),
                             level=c(2, 2, 2, 2, 2, 2),
                             line=c(1, 1, 1, 1, 1, 1),
                             i=c(1, 2, 3, 4, 5, 6),
                             level_factor=c("2", "2", "2", "2", "2", "2"))
cl_output <- head(get_contours(df_grid,levels=seq(2,10,by=2))) %>%
  dplyr::mutate_if(is.numeric,function(x) round(x,4)) %>%
  dplyr::mutate(level_factor=as.character(level_factor)) %>% tibble::as_tibble()
test_that("get_contours works for simple example",{
  expect_equal(cl_output,cl_results)
})



####
params <- example_params_confined
params$gs <- NULL
params <- tidyr::crossing(params,gs=seq(0,1,by=0.05))
treaty_df <- evaluate_treaty_cases(params,'quda')
# results <- gather_outcomes(treaty_df)
test_that('gather_outcomes working',{
  expect_equal(dim(gather_outcomes(treaty_df)),c(546,34))
  expect_equal(dim(gather_outcomes(treaty_df,expectation=TRUE)),c(630,34))
})



