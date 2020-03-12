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

df <- tibble::tibble(treaty="N",
                     zRange=0,
                     zMinSwiss=0,
                     zMaxFrench=0,
                     qshat=20,
                     qfhat=20,
                     qsstar=20,
                     qfstar=20,
                     qsdouble=20,
                     qfdouble=20)
test_that("evaluate_treaty returns correct zRange, q values for unconfined aquifer",{
  expect_equal(evaluate_treaty(example_params_unconfined),
               df)
})
evaluate_treaty(example_params_unconfined) %>% ggp::print_data_frame_for_entry()


## evaluate_treaty_cases
df_cases <- tibble::tibble(treaty="Y", zRange=0.0443,zMinSwiss=-0.0222,zMaxFrench=0.0222,
                           qshat=6.8686, qfhat=6.8686, qsstar=8.7418, qfstar=8.7418, qsdouble=9.0376, qfdouble=9.0376,
                           Us_hat=-17.4976, Uf_hat=-16.6976,Us_star=-17.7432,Uf_star=-16.9432,Us_double=-17.2388, Uf_double=-16.4388,Us_hat_double=-17.9445,Uf_hat_double=-17.1445,
                           ds_hat=25.96,df_hat=25.96,ds_star=32.5164,df_star=32.5164,ds_double=30.298, df_double=30.298,ds_hat_double=30.298,df_hat_double=30.298) %>%
  dplyr::mutate_if(is.numeric,function(x) round(x,digits=4))
test_that("evaluate_treaty_cases returns correct zRange, q, U, and d values",{
  expect_equal(evaluate_treaty_cases(example_params_confined,"qud") %>% dplyr::mutate_if(is.numeric,function(x) round(x,digits=4)),
               df_cases)
})
