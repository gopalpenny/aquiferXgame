# game_helpers.R


#' Apply interval contraints to numeric value
apply_constraints <- function(x,interval) {
  if (x<interval[1]) {
    x <- interval[1]
  } else if (x>interval[2]) {
    x <- interval[2]
  }
  return(x)
}


#' Check if value is within an inclusive range
#' @examples
#' in_range(c(-3,-1,0,5,10,14),c(0,10))
in_range <- function(x,range) {
  x >= range[1] & x <= range[2]
}
