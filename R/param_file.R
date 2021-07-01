#' Create paramater file for the 'JWSACruncher'
#'
#' To run the 'JWSACruncher' needs a parameter file and \code{create_param_file} allows to create it.
#'
#' @param dir_file_param Path to the directory that will contains the parameter file "parameters.param".
#' @param bundle Maximum size for a group of series (in output). By default \code{bundle = 10000}.
#' @param csv_layout Layout of the CSV files (series only). By default \code{csv_layout = "list"}. Other options: \code{csv_layout = "vtable"} (vertical table) or \code{csv_layout = "htable"} (horizontal table).
#' @param csv_separator the field separator string used in the CSV file. By default \code{csv_separator = ";"}.
#' @param ndecs Number of decimals used in the output. By default \code{ndec = 6}.
#' @param policy refreshing policy of the processing. By default \code{policy = "parameters"} (re-estimation of the coefficients of the reg-ARIMA model, see details).
#' @param output Full path of the output folder. By default (\code{output = NULL}) a folder is create in the path to the workspace (\[workspace\]/Output).
#' @param matrix_item character containing the items of the matrix output (see the 'JDemetra+' manual for more information). By default, the items defined in the option \code{getOption("default_matrix_item")} are used (option initialized by the default output of the 'JWSACruncher' 2.2.2).
#' @param tsmatrix_series character containing the names of the times series to export (see the 'JDemetra+' manual for more information).  By default, the items defined in the option \code{getOption("default_tsmatrix_series")} are used (option initialized by the default output of the 'JWSACruncher' 2.2.2).
#' @param paths_path The paths used for relative addresses (see the "Demetra Paths" of the graphical interface of 'JDemetra+').
#' 
#' @details When the 'JWSACruncher' is launched, the data is refreshed with a specific policy that is defined by the paramater \code{policy}. The available options are:
#' \itemize{
#' \item \code{policy = "current"}: all the estimations are fixed;
#' \item \code{policy = "fixedparameters"} or \code{policy = "fixed"}: re-estimation of the coefficients of the regression variables (but not the ARIMA coefficients);
#' \item \code{policy = "parameters"} (the default): \code{policy = "fixedparameters"} + re-estimation of ARIMA coefficients;
#' \item \code{policy = "lastoutliers"}: \code{policy = "parameters"} + re-identification of last outliers (on the last year);
#' \item \code{policy = "outliers"}: \code{policy = "lastoutliers"} + re-identification of all outliers;
#' \item \code{policy = "stochastic"}: \code{policy = "outliers"} + re-identification of ARIMA orders;
#' \item \code{policy = "complete"} or \code{policy = "concurrent"}: the model is completely re-identified and re-estimated.
#' }
#' 
#' @return Path to the paramater file.
#' @seealso \code{\link{cruncher_and_param}}.
#' @encoding UTF-8
#' @export
create_param_file <- function(dir_file_param, bundle = 10000, csv_layout = "list", csv_separator = ";",
                              ndecs = 6, policy = "parameters", output = NULL,
                              matrix_item = getOption("default_matrix_item"),
                              tsmatrix_series = getOption("default_tsmatrix_series"),
                              paths_path = NULL){
    if (missing(dir_file_param))
        stop("The parameter dir_file_param is missing")
    
    first_line <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
    param_line <- paste("<wsaConfig bundle=", bundle, " csvlayout=", csv_layout, " csvseparator=",
                        csv_separator, " ndecs=",ndecs,">", sep = "\"")
    policy_line <- paste0("    <policy>", policy, "</policy>")

    output_line <- matrix_lines <- tsmatrix_lines <- path_lines <- NULL

    if (!is.null(output)) {
        output <- normalizePath(output)
        output_line <- paste0("    <output>", gsub("/", "\\", output, fixed = TRUE), "</output>")
    }

    if (!is.null(matrix_item)) {
        matrix_lines <- c("    <matrix>",
                          paste0("        <item>", matrix_item, "</item>"),
                          "    </matrix>")
    }

    if (!is.null(tsmatrix_series)) {
        tsmatrix_lines <- c("    <tsmatrix>",
                            paste0("        <series>", tsmatrix_series, "</series>"),
                            "    </tsmatrix>")
    }

    if (!is.null(paths_path)) {
        path_lines <- c("    <paths>",
                        paste0("        <path>", gsub("/", "\\", paths_path, fixed = TRUE), "</path>"),
                        "    </paths>")
    }

    file_param <- c(first_line, param_line, policy_line, output_line,
                    matrix_lines, tsmatrix_lines, path_lines,
                    "</wsaConfig>"
    )
    writeLines(file_param, con = paste0(dir_file_param,"/parameters.param"))
    return(invisible(paste0(dir_file_param,"/parameters.param")))
}
