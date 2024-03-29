% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cruncher.R
\encoding{UTF-8}
\name{update_workspace}
\alias{update_workspace}
\title{Update a workspace}
\usage{
update_workspace(
  workspace = NULL,
  policy = "parameters",
  cruncher_bin_directory = getOption("cruncher_bin_directory"),
  log_file = NULL
)
}
\arguments{
\item{workspace}{Path to the workspace. By default a dialog box opens to choose the workspace.}

\item{policy}{Refreshing policy of the processing. By default \code{policy = "parameters"} (re-estimation of the coefficients of the reg-ARIMA model, see details).}

\item{cruncher_bin_directory}{Path to the "bin" directory of the 'JWSACruncher'. By default
the value of the option \code{"cruncher_bin_directory"} is used.}

\item{log_file}{Name of the log file of 'JWSACruncher'. By default the log isn't exported.}
}
\value{
Path to the workspace.
}
\description{
Function to update a workspace without exporting the results.
}
\seealso{
\code{\link[=cruncher]{cruncher()}}, \code{\link[=cruncher_and_param]{cruncher_and_param()}}.
}
