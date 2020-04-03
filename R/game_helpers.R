# game_helpers.R


#' Apply interval contraints to numeric value
#' @keywords internal
apply_constraints <- function(x,interval) {
  if (x<interval[1]) {
    x <- interval[1]
  } else if (x>interval[2]) {
    x <- interval[2]
  }
  return(x)
}


#' Check if value is within an inclusive range
#' @keywords internal
#' @examples
#' \dontrun{
#' in_range(c(-3,-1,0,5,10,14),c(0,10))
#' }
in_range <- function(x,range) {
  x >= range[1] & x <= range[2]
}


#' Convert Mathematica to R
#' @param path Path to file to convert
#' @param ... Arguments to change, listed in vector format as \code{c(x1,x2)}
#' @details
#' For each argument in \code{...}, replaces x1 with x2 using gsub. For example, \code{c(" ","*")} changes
#'   " " to "*". Special characters must be escaped
#' @return
#' Returns the corrected equations using
#' @keywords internal
#' @examples
#' \dontrun{
#' eqns <- MM2R(path="mathematica/unconfined_eqns.txt", g1=c(" ","*"), g1=c("Sqrt","sqrt"))
#' eqns <- MM2R(path="mathematica/unconfined_nl_eqns.txt",
#'              g1=c(" ","*"),
#'              g2=c("Sqrt","sqrt"),
#'              g3=c("Log","log"),
#'              g4=c("\\[","\\("),
#'              g4=c("\\]","\\)"))
#' }
MM2R <- function(path,...) {
  fil <- file(path)
  # cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = fil,
  #     sep = "\n")
  file_lines <- readLines(fil, n = -1)
  close(fil) # tidy up
  params <- list(...)
  for (i in 1:length(params)) {
    file_lines <- gsub(params[[i]][1],params[[i]][2],file_lines)
  }
  for (i in 1:length(file_lines)) {
    cat(file_lines[i],"\n")
  }
  return(file_lines)
}
