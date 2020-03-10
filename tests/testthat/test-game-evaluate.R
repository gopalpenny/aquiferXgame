# test_game_evaluate.R

test_that("evaluate_agreement returns tibble",{
  expect_equal(class(evaluate_treaty(default_params)),c("tbl_df","tbl","data.frame"))
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
test_that("evaluate_agreement returns correct zRange, q values",{
  expect_equal(evaluate_treaty(default_params) %>% dplyr::mutate_if(is.numeric,function(x) round(x,digits=4)),
               df)
})
