# test-game-evaluate-specifics

# uconfined aquifers:
# first estimate of first best produces negative qs, maximum qf=5. second estimate must have qs>0
df_input <- tibble::tibble(B=0.91, dBf=75.4, dBs=75.4, h0f=45.3, h0s=42.7, p0s=100, PHIff=110, PHIfrN=97.5, PHIfrT=97.5, PHIfs=91.4, PHIsf=91.4, PHIsrN=121, PHIsrT=121, PHIss=110, year=1989, row=50, Qs=15, Qf=5, gs=1, gf=1, p0f=5e+05, crs=1e+05, rmN=8.2, rmT=8.2, es=0, ef=0)
df_output <- tibble::tibble(treaty="N", zRange=0, zMinSwiss=0, zMaxFrench=0, qshat=15, qsstar=15, qsdouble=15, qfhat=5, qfstar=5, qfdouble=5, ds_hat=48.7825, ds_star=48.7825, ds_double=48.7825, ds_hat_double=48.7825, df_hat=44.8944, df_star=44.8944, df_double=44.8944, df_hat_double=44.8944)
test_that("evaluate_treaty_cases first estimate of first best produces negative qs, maximum qf=5. second estimate must have qs>0",{
  expect_equal(evaluate_treaty_cases(df_input,'qd') %>% tidyr::gather(var,val,-treaty) %>% dplyr::mutate(val=round(val,4)),df_output %>% tidyr::gather(var,val,-treaty))
})


# Unconfined quifer fully depleted
df_input_depleted <- data.frame(B=0.91, dBf=75.4, dBs=75.4, h0f=45.3, h0s=42.7, p0s=100, PHIff=110, PHIfrN=97.5, PHIfrT=97.5, PHIfs=91.4, PHIsf=91.4, PHIsrN=121, PHIsrT=121, PHIss=110, year=1956, row=17, Qs=17, Qf=0, gs=0.6, gf=0.6, p0f=5e+05, crs=5e+05, rmN=0, rmT=0, es=0, ef=0)
df_output_depleted <- tibble::tibble(treaty="D", zRange=NaN, zMinSwiss=NaN, zMaxFrench=0, AD_fb=TRUE, AD_nash=TRUE, AD_cheat=FALSE, qshat=17, qsstar=17, qsdouble=15.3, qfhat=0, qfstar=0, qfdouble=0, ds_hat=NaN, ds_star=NaN, ds_double=63.5556, ds_hat_double=63.5556, df_hat=53.0776, df_star=53.0776, df_double=53.0776, df_hat_double=53.0776)
test_that('evaluate_treaty_cases returns \'D\' for depleted aquifer',{
  expect_equal(evaluate_treaty_cases(df_input_depleted,'qd') %>% dplyr::mutate_if(is.numeric,function(x) round(x,4)),
               df_output_depleted)
})
