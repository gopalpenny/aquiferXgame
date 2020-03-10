# test_game_evaluate.R

test_that("evaluate_agreement returns tibble",{
  expect_equal(class(evaluate_treaty(default_params)),c("tbl_df","tbl","data.frame"))
})
