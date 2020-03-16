# https://adv-r.hadley.nz/perf-measure.html
# Measuring performance through profiling
library(profvis)
source("performance/perform_function.R")
profvis(perform(5))
