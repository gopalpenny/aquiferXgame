% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/game_functions_confined.R
\name{evaluate_treaty_confined}
\alias{evaluate_treaty_confined}
\title{Evaluate the treaty in a confined aquifer}
\usage{
evaluate_treaty_confined(params)
}
\arguments{
\item{params}{Parameter list (or data.frame with 1 row) containing
necessary parameters to evaluate the agreement.}
}
\value{
Returns a 1-row tibble containing pumping, utility ranges needed for the treaty,
and whether or not there is a treaty (i.e., if zRange > 0)
}
\description{
Evaluate whether or not the treaty will be made.
}
\details{
Evaluate the treaty given social, economic, and geophysical parameters.
}
\examples{
\dontrun{
evaluate_treaty_confined(example_params_confined)
}
}
\keyword{internal}
