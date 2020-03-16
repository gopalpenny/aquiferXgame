perform <- function(n=1) {
  evaluate_treaty_cases(tidyr::crossing(example_params_confined,num=1:n),"qud")
}
