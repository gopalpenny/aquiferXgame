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

df_grid <- expand.grid(x=seq(-5,5,length.out=20),y=seq(-5,5,length.out=20))
df_grid$z <- sqrt(df_grid$x^2+df_grid$y^2)
cl_results <- tibble::tibble(x=c(-1.8421, -1.9824, -1.9824, -1.8421, -1.8375, -1.4976),
                             y=c(-0.7742, -0.2632, 0.2632, 0.7742, 0.7895, 1.3158),
                             level=c(2, 2, 2, 2, 2, 2),
                             line=c(1, 1, 1, 1, 1, 1),
                             i=c(1, 2, 3, 4, 5, 6),
                             level_factor=c("2", "2", "2", "2", "2", "2"))
cl_output <- tibble::tibble(head(get_contours(df_grid,levels=seq(2,10,by=2))))
cl_output[,c("x","y")] <- round(cl_output[,c("x","y")],4)
cl_output$level_factor <- as.character(cl_output$level_factor)
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



