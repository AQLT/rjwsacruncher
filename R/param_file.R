#' Create parameter file for the 'JWSACruncher'
#'
#' To run the 'JWSACruncher' needs a parameter file and \code{create_param_file} allows to create it.
#'
#' @param dir_file_param Path to the directory that will contains the parameter file \code{"parameters.param"} (if `file_param` not supplied).
#' @param file_param Path to the parameters file. 
#' By default the file is named `parameters.param` and it is created at the `fir_file_param` directory.
#' @param bundle Maximum size for a group of series (in output). By default \code{bundle = 10000}.
#' @param csv_layout Layout of the CSV files (series only). By default \code{csv_layout = "list"}. Other options: \code{csv_layout = "vtable"} (vertical table) or \code{csv_layout = "htable"} (horizontal table).
#' @param csv_separator The field separator string used in the CSV file. By default \code{csv_separator = ";"}.
#' @param ndecs Number of decimals used in the output. By default \code{ndec = 6}.
#' @param full_series_name Boolean indicating if the fully qualified name of the series will be used (the default \code{full_series_name = TRUE}) or if only the name of the series  should be displayed.
#' Only used when \code{v3 = TRUE}.
#' @param short_column_headers Boolean indicating if the full column names should be printed (the default `short_column_headers = TRUE`) instead of always outputting shortened ones.
#' Only used when \code{v3 = TRUE} (since v.3.4.0).
#' @param rslt_name_level Only used when \code{v3 = TRUE}.
#' @param policy Refreshing policy of the processing. By default \code{policy = "parameters"} (re-estimation of the coefficients of the reg-ARIMA model, see details).
#' @param refreshall Boolean indicating if the data is refreshed (by default `refreshall = TRUE`).
#' @param output Full path of the output folder. By default (\code{output = NULL}) a folder is create in the path to the workspace (\[workspace\]/Output).
#' @param matrix_item Character containing the items of the matrix output (see the 'JDemetra+' manual for more information). By default, the items defined in the option \code{getOption("default_matrix_item")} are used (option initialized by the default output of the 'JWSACruncher' 2.2.2).
#' @param tsmatrix_series Character containing the names of the times series to export (see the 'JDemetra+' manual for more information).  By default, the items defined in the option \code{getOption("default_tsmatrix_series")} are used (option initialized by the default output of the 'JWSACruncher' 2.2.2).
#' @param paths_path The paths used for relative addresses (see the "Demetra Paths" of the graphical interface of 'JDemetra+').
#' @param v3 Boolean indicating if the parameter file should be compatible with a version 3.0.0 and higher of 'JWSACRuncher' (\code{v3 = TRUE}) or a lower version (\code{v3 = FALSE}). By default 
#' the value of the option \code{"is_cruncher_v3"} is used (equals to \code{FALSE} by default).
#' @details When the 'JWSACruncher' is launched, the data is refreshed with a specific policy that is defined by the parameter \code{policy}. The available options are:
#' \itemize{
#' \item \code{policy = "current"}: all the estimations are fixed and AO added for new data (since v.2.2.3), short name `policy = "n"`;
#' \item \code{policy = "fixed"}: all the estimations are fixed (since v.2.2.3), short name `policy = "f"`;
#' \item \code{policy = "fixedparameters"}: re-estimation of the coefficients of the regression variables (but not the ARIMA coefficients), short name `policy = "fp"`;
#' \item \code{policy = "fixedarparameters"}: re-estimation of the coefficients of the regression variables and of the MA coefficients of the ARIMA model (but not the AR coefficients), short name `policy = "farp"` (since v.3.4.0);
#' \item \code{policy = "parameters"} (the default): \code{policy = "fixedparameters"} + re-estimation of ARIMA coefficients, short name `policy = "p"`;
#' \item \code{policy = "lastoutliers"}: \code{policy = "parameters"} + re-identification of last outliers (on the last year), short name `policy = "l"`;
#' \item \code{policy = "outliers"}: \code{policy = "lastoutliers"} + re-identification of all outliers, short name `policy = "o"`;
#' \item \code{policy = "stochastic"}: \code{policy = "outliers"} + re-identification of ARIMA orders, short name `policy = "s"`;
#' \item \code{policy = "complete"} or \code{policy = "concurrent"}: the model is completely re-identified and re-estimated, short name `policy = "c"`.
#' }
#' 
#' @return Path to the parameter file.
#' @seealso [read_param_file()], [list2param_file()], [default_param_file] [cruncher_and_param()].
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' dir = tempdir()
#' # Here a file parameters.param is created in the directory dir
#' # with default parameters of the different options
#' create_param_file(dir)
#' # to only export the raw and the seasonally adjusted series
#'  create_param_file(dir, 
#'                    tsmatrix_series = c("y", "sa"))
#' }
#' 
#' @export
create_param_file <- function(
    dir_file_param, bundle = 10000, csv_layout = "list", csv_separator = ";",
    ndecs = 6, full_series_name = TRUE, short_column_headers = TRUE, rslt_name_level = 2, policy = "parameters", refreshall = TRUE, 
    output = NULL,
    matrix_item = getOption("default_matrix_item"),
    tsmatrix_series = getOption("default_tsmatrix_series"),
    paths_path = NULL,
    v3 = getOption("is_cruncher_v3"),
    file_param = file.path(dir_file_param, "parameters.param")
    ){
  if (missing(dir_file_param) & missing(file_param))
    stop("The parameter dir_file_param is missing")
  
  first_line <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  param_line <- paste("<wsaConfig bundle=", bundle, " csvlayout=", csv_layout, " csvseparator=",
                      csv_separator, " ndecs=",ndecs, sep = "\"")
  if (v3){
    param_line <- paste(param_line,
                        " fullseriesname=", ifelse(full_series_name, "true", "false"),
                        " shortcolumnheaders=", ifelse(short_column_headers, "true", "false"),
                        " rsltnamelevel=", rslt_name_level,
                        " format=", 
                        "JD3", sep = "\"")
  }
  param_line <- paste(param_line,">", sep = "\"")
  policy_line <- paste0("    <policy>", policy, "</policy>")
  refresh_line <- paste0("    <refreshall>", ifelse(refreshall, 'true', 'false'), "</refreshall>")
  
  output_line <- matrix_lines <- tsmatrix_lines <- path_lines <- NULL
  
  if (!is.null(output)) {
    output <- full_path(output)
    output_line <- paste0("    <output>", output, "</output>")
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
    paths_path <- full_path(paths_path)
    path_lines <- c("    <paths>",
                    paste0("        <path>", paths_path, "</path>"),
                    "    </paths>")
  }
  
  export_file <- c(first_line, param_line, policy_line, refresh_line, output_line,
                  matrix_lines, tsmatrix_lines, path_lines,
                  "</wsaConfig>"
  )
  writeLines(export_file, con = file_param)
  return(invisible(file_param))
}

#' Read parameter file of the 'JWSACruncher'
#' 
#' @param file Path to the parameter file.
#' 
#' @seealso [create_param_file()], [list2param_file()], [default_param_file()] [cruncher_and_param()].
#' @examples
#' dir = tempdir()
#' list_param <- default_param_file(v3 = FALSE)
#' list2param_file(dir, list_param)
#' list_param_2 <- read_param_file(file.path(dir, "parameters.param"))
#' all.equal(list_param, list_param_2)
#' @export
read_param_file <- function(file){
  f <- readLines(file)
  config <- strsplit(f[2], " ")[[1]]
  config <- gsub('(")|(<)|(>)', "", config)
  config <- grep("=", config, value = TRUE)
  list_config <- gsub("=.*", "", config)
  config <- lapply(list_config, function(param) {
    res <- grep(param, config, value = TRUE)
    if(length(res) == 0) {
      NULL
    } else {
      gsub("^.*=", "", res)
    }
  })
  list_config <- gsub("csv", "csv_", list_config)
  list_config <- gsub("fullseriesname", "full_series_name", list_config)
  list_config <- gsub("shortcolumnheaders", "short_column_headers", list_config)
  list_config <- gsub("rsltnamelevel", "rslt_name_level", list_config)
  names(config) <- list_config
  if (!is.null(config$full_series_name)) {
    config$full_series_name <- config$full_series_name == 'true'
  }
  if (!is.null(config$short_column_headers)) {
    config$short_column_headers <- config$short_column_headers == 'true'
  }
  policy <- grep("policy", f, value = TRUE)
  if (length(policy) > 0) {
    policy <- gsub("(.*<policy>)|(</.*)", "", policy)
  } else {
    policy <- NULL
  }
  
  refreshall <- grep("refreshall", f, value = TRUE)
  if (length(refreshall) > 0) {
    refreshall <- gsub("(.*<refreshall>)|(</.*)", "", refreshall)
    refreshall <- refreshall == 'true'
  } else {
    refreshall <- NULL
  }
  
  output <- grep("output", f, value = TRUE)
  if (length(output) > 0) {
    output <- gsub("(.*<output>)|(</.*)", "", output)
  } else {
    output <- NULL
  }
  
  id_mat <- grep("</*matrix>", f)
  if (length(id_mat) == 2 && diff(id_mat) > 1) {
    matrix_item <- f[seq.int(id_mat[1] + 1, id_mat[2] - 1, by = 1)]
    matrix_item <- gsub("(.*<item>)|(</.*)", "", matrix_item)
  } else {
    matrix_item <- NULL
  }
  
  id_tsmat <- grep("</*tsmatrix>", f)
  if (length(id_tsmat) == 2 && diff(id_tsmat) > 1) {
    tsmatrix_series <- f[seq.int(id_tsmat[1] + 1, id_tsmat[2] - 1, by = 1)]
    tsmatrix_series <- gsub("(.*<series>)|(</.*)", "", tsmatrix_series)
  } else {
    tsmatrix_series <- NULL
  }
  
  
  id_paths_paths <- grep("</*paths>", f)
  if (length(id_paths_paths) == 2 && diff(id_paths_paths) > 1) {
    paths_path <- f[seq.int(id_paths_paths[1] + 1, id_paths_paths[2] - 1, by = 1)]
    paths_path <- gsub("(.*<path>)|(</.*)", "", paths_path)
  } else {
    paths_path <- NULL
  }
  
  list(config = config, policy = policy, refreshall = refreshall, output = output,
       matrix_item = matrix_item,
       tsmatrix_series = tsmatrix_series,
       paths_path = paths_path)
}



#' Create parameter file from list
#' 
#' @inheritParams create_param_file
#' @param x A list, for example create by [read_param_file()] or [default_param_file()].
#' @seealso [create_param_file()], [read_param_file()], [default_param_file()] [cruncher_and_param()].
#' @examples
#' \dontrun{
#' dir = tempdir()
#' # Here a file parameters.param is created in the directory dir
#' # with default parameters of 'JWSACruncher' v2
#' list_param <- default_param_file(v3 = FALSE)
#' list2param_file(dir, list_param)
#' # to only export the raw and the seasonally adjusted series
#' list_param$tsmatrix_series <- c("y", "sa")
#' list2param_file(dir, list_param)
#' }
#' @export
list2param_file <- function(dir_file_param, x,
                            file_param = file.path(dir_file_param, "parameters.param")){
  config <- x$config
  v3 <- !is.null(config$format)
  config$format <- x$config <- NULL
  if (!missing(file_param)) {
    params <- c(list(file_param = file_param, v3 = v3),
                config, x)
  } else {
    params <- c(list(dir_file_param = dir_file_param, v3 = v3),
                config, x)
  }
  
  do.call(create_param_file, params)
}

#' Get default parameters of the 'JWSACruncher'
#' 
#' @param v3 Boolean indicating if the parameters are the from a version 3.0.0 and higher of 'JWSACRuncher' (\code{v3 = TRUE}) or a lower version (\code{v3 = FALSE}). By default 
#' the value of the option \code{"is_cruncher_v3"} is used (equals to \code{FALSE} by default).
#' @param cruncher_bin_directory Path to the directory that contains the 'JWSACruncher' binary.
#' If defined, the parameter `v3` is ignored and the 'JWSACruncher' is run without  parameter to generate the default parameters file.
#' @seealso [create_param_file()], [read_param_file()], [list2param_file()], [cruncher_and_param()].
#' @export
default_param_file <- function(v3 = getOption("is_cruncher_v3"), cruncher_bin_directory = NULL){
  v3_param <- 
    list(
      config = list(bundle = "10000", csv_layout = "list", csv_separator = ";", 
                    ndecs = "6", full_series_name = TRUE, 
                    short_column_headers = TRUE,
                    rslt_name_level = "2", 
                    format = "JD3"),
      policy = "parameters", refreshall = TRUE, 
      output = NULL,
      matrix_item = c("period", "span.start", "span.end", 
                      "span.n", "span.missing", "log", "adjust", "likelihood.ll", 
                      "likelihood.adjustedll", "likelihood.ssqerr", "likelihood.aic", 
                      "likelihood.bic", "likelihood.aicc", "likelihood.bicc", "likelihood.bic2", 
                      "likelihood.hannanquinn", "likelihood.nparams", "likelihood.nobs", 
                      "likelihood.neffectiveobs", "likelihood.df", "arima.p", "arima.d", 
                      "arima.q", "arima.bp", "arima.bd", "arima.bq", "arima.theta(*)", 
                      "arima.phi(*)", "arima.btheta(*)", "arima.bphi(*)", "regression.espan.start", 
                      "regression.espan.end", "regression.espan.n", "regression.espan.missing", 
                      "regression.mean", "regression.nlp", "regression.ntd", "regression.leaster", 
                      "regression.nmh", "regression.nout", "regression.nao", "regression.nls", 
                      "regression.ntc", "regression.nso", "regression.nusers", 
                      "regression.mu", "regression.lp", "regression.td(*)", "regression.td-derived", 
                      "regression.td-ftest", "regression.easter", "regression.outlier(*)", 
                      "regression.user(*)", "residuals.ser", "residuals.type", 
                      "residuals.mean", "residuals.skewness", "residuals.kurtosis", 
                      "residuals.doornikhansen", "residuals.lb", "residuals.bp", 
                      "residuals.lb2", "residuals.bp2", "residuals.seaslb", "residuals.seasbp", 
                      "residuals.nruns", "residuals.lruns", "residuals.nudruns", 
                      "residuals.ludruns", "mode", "seasonal", "diagnostics.seas-lin-combined", 
                      "diagnostics.seas-lin-evolutive", "diagnostics.seas-lin-stable", 
                      "diagnostics.seas-si-combined", "diagnostics.seas-si-combined3", 
                      "diagnostics.seas-si-evolutive", "diagnostics.seas-si-stable", 
                      "diagnostics.seas-res-combined", "diagnostics.seas-res-combined3", 
                      "diagnostics.seas-res-evolutive", "diagnostics.seas-res-stable", 
                      "diagnostics.seas-sa-combined", "diagnostics.seas-sa-combined3", 
                      "diagnostics.seas-sa-evolutive", "diagnostics.seas-sa-stable", 
                      "diagnostics.seas-i-combined", "diagnostics.seas-i-combined3", 
                      "diagnostics.seas-i-evolutive", "diagnostics.seas-i-stable", 
                      "diagnostics.seas-lin-qs", "diagnostics.seas-lin-f", "diagnostics.seas-lin-friedman", 
                      "diagnostics.seas-lin-kw", "diagnostics.seas-lin-periodogram", 
                      "diagnostics.seas-lin-spectralpeaks", "diagnostics.seas-res-qs", 
                      "diagnostics.seas-res-f", "diagnostics.seas-res-friedman", 
                      "diagnostics.seas-res-kw", "diagnostics.seas-res-periodogram", 
                      "diagnostics.seas-res-spectralpeaks", "diagnostics.seas-sa-qs", 
                      "diagnostics.seas-sa-f", "diagnostics.seas-sa-friedman", 
                      "diagnostics.seas-sa-kw", "diagnostics.seas-sa-periodogram", 
                      "diagnostics.seas-sa-spectralpeaks", "diagnostics.seas-i-qs", 
                      "diagnostics.seas-i-f", "diagnostics.seas-i-friedman", "diagnostics.seas-i-kw", 
                      "diagnostics.seas-i-periodogram", "diagnostics.seas-i-spectralpeaks", 
                      "diagnostics.seas-sa-ac1", "diagnostics.td-res-all", "diagnostics.td-res-last", 
                      "diagnostics.td-sa-all", "diagnostics.td-sa-last", "diagnostics.td-i-all", 
                      "diagnostics.td-i-last", "decomposition.icratio", "decomposition.trend-filter", 
                      "decomposition.seasonal-filters", "decomposition.d9-global-msr", 
                      "m-statistics.m1", "m-statistics.m2", "m-statistics.m3", 
                      "m-statistics.m4", "m-statistics.m5", "m-statistics.m6", 
                      "m-statistics.m7", "m-statistics.m8", "m-statistics.m9", 
                      "m-statistics.m10", "m-statistics.m11", "m-statistics.q", 
                      "m-statistics.q-m2"), 
      tsmatrix_series = c("residuals.tsres", 
                          "y", "y_f(?)", "y_ef(?)", "y_b(?)", "y_eb(?)", "yc", "det", 
                          "det_f(?)", "det_b(?)", "cal", "cal_f(?)", "cal_b(?)", "ycal", 
                          "tde", "tde_f(?)", "tde_b(?)", "ee", "ee_f(?)", "ee_b(?)", 
                          "omhe", "omhe_f(?)", "omhe_b(?)", "mhe", "mhe_f(?)", "mhe_b(?)", 
                          "out", "out_f(?)", "out_b(?)", "reg", "reg_f(?)", "reg_b(?)", 
                          "l", "out_t", "out_s", "out_i", "reg_t", "reg_s", "reg_i", 
                          "reg_sa", "reg_u", "reg_y", "det_t", "det_s", "det_i", "out_t_f(?)", 
                          "out_s_f(?)", "out_i_f(?)", "reg_t_f(?)", "reg_s_f(?)", "reg_i_f(?)", 
                          "reg_sa_f(?)", "reg_u_f(?)", "reg_y_f(?)", "det_t_f(?)", 
                          "det_s_f(?)", "det_i_f(?)", "out_t_b(?)", "out_s_b(?)", "out_i_b(?)", 
                          "reg_t_b(?)", "reg_s_b(?)", "reg_i_b(?)", "reg_sa_b(?)", 
                          "reg_u_b(?)", "reg_y_b(?)", "det_t_b(?)", "det_s_b(?)", "det_i_b(?)", 
                          "sa", "t", "s", "i", "sa_f", "t_f", "s_f", "i_f", "decomposition.y_lin", 
                          "decomposition.sa_lin", "decomposition.t_lin", "decomposition.s_lin", 
                          "decomposition.i_lin", "decomposition.sa_lin_e", "decomposition.t_lin_e", 
                          "decomposition.s_lin_e", "decomposition.i_lin_e", "decomposition.y_lin_f", 
                          "decomposition.sa_lin_f", "decomposition.t_lin_f", "decomposition.s_lin_f", 
                          "decomposition.i_lin_f", "decomposition.y_lin_ef", "decomposition.sa_lin_ef", 
                          "decomposition.t_lin_ef", "decomposition.s_lin_ef", "decomposition.i_lin_ef", 
                          "decomposition.y_lin_b", "decomposition.sa_lin_b", "decomposition.t_lin_b", 
                          "decomposition.s_lin_b", "decomposition.i_lin_b", "decomposition.y_lin_eb", 
                          "decomposition.sa_lin_eb", "decomposition.t_lin_eb", "decomposition.s_lin_eb", 
                          "decomposition.i_lin_eb", "decomposition.y_cmp", "decomposition.y_cmp_f", 
                          "decomposition.y_cmp_b", "decomposition.sa_cmp", "decomposition.t_cmp", 
                          "decomposition.s_cmp", "decomposition.i_cmp", "preadjustment.a1", 
                          "preadjustment.a1a", "preadjustment.a1b", "preadjustment.a6", 
                          "preadjustment.a7", "preadjustment.a8", "preadjustment.a8t", 
                          "preadjustment.a8i", "preadjustment.a8s", "preadjustment.a9", 
                          "preadjustment.a9u", "preadjustment.a9sa", "preadjustment.a9ser", 
                          "decomposition.b1", "decomposition.b2", "decomposition.b3", 
                          "decomposition.b4", "decomposition.b5", "decomposition.b6", 
                          "decomposition.b7", "decomposition.b8", "decomposition.b9", 
                          "decomposition.b10", "decomposition.b11", "decomposition.b13", 
                          "decomposition.b17", "decomposition.b20", "decomposition.c1", 
                          "decomposition.c2", "decomposition.c4", "decomposition.c5", 
                          "decomposition.c6", "decomposition.c7", "decomposition.c9", 
                          "decomposition.c10", "decomposition.c11", "decomposition.c13", 
                          "decomposition.c17", "decomposition.c20", "decomposition.d1", 
                          "decomposition.d2", "decomposition.d4", "decomposition.d5", 
                          "decomposition.d6", "decomposition.d7", "decomposition.d8", 
                          "decomposition.d9", "decomposition.d10", "decomposition.d10a", 
                          "decomposition.d11", "decomposition.d11a", "decomposition.d12", 
                          "decomposition.d12a", "decomposition.d13", "finals.d11", 
                          "finals.d12", "finals.d13", "finals.d16", "finals.d18", "finals.d11a", 
                          "finals.d12a", "finals.d16a", "finals.d18a", "finals.e1", 
                          "finals.e2", "finals.e3", "finals.e11"), 
      paths_path = NULL)
  v2_param <- 
    list(
      config = list(bundle = "10000", csv_layout = "list", csv_separator = ";", 
                    ndecs = "6"), policy = "parameters", refreshall = TRUE, 
      output = NULL, 
      matrix_item = c("period", "span.start", "span.end", 
                      "span.n", "span.missing", "espan.start", "espan.end", "espan.n", 
                      "log", "adjust", "regression.lp", "regression.ntd", "regression.nmh", 
                      "regression.td-derived", "regression.td-ftest", "regression.easter", 
                      "regression.nout", "regression.noutao", "regression.noutls", 
                      "regression.nouttc", "regression.noutso", "regression.td(*)", 
                      "regression.out(*)", "regression.user(*)", "likelihood.neffectiveobs", 
                      "likelihood.np", "likelihood.logvalue", "likelihood.adjustedlogvalue", 
                      "likelihood.ssqerr", "likelihood.aic", "likelihood.aicc", 
                      "likelihood.bic", "likelihood.bicc", "residuals.ser", "residuals.ser-ml", 
                      "residuals.mean", "residuals.skewness", "residuals.kurtosis", 
                      "residuals.dh", "residuals.lb", "residuals.lb2", "residuals.seaslb", 
                      "residuals.bp", "residuals.bp2", "residuals.seasbp", "residuals.nudruns", 
                      "residuals.ludruns", "residuals.nruns", "residuals.lruns", 
                      "arima", "arima.mean", "arima.p", "arima.d", "arima.q", "arima.bp", 
                      "arima.bd", "arima.bq", "arima.phi(*)", "arima.bphi(*)", 
                      "arima.th(*)", "arima.bth(*)", "decomposition.seasonality", 
                      "decomposition.trendfilter", "decomposition.seasfilter", 
                      "m-statistics.m1", "m-statistics.m2", "m-statistics.m3", 
                      "m-statistics.m4", "m-statistics.m5", "m-statistics.m6", 
                      "m-statistics.m7", "m-statistics.m8", "m-statistics.m9", 
                      "m-statistics.m10", "m-statistics.m11", "m-statistics.q", 
                      "m-statistics.q-m2", "method", "variancedecomposition.cycle", 
                      "variancedecomposition.seasonality", "variancedecomposition.irregular", 
                      "variancedecomposition.tdh", "variancedecomposition.others", 
                      "variancedecomposition.total", "diagnostics.logstat", "diagnostics.levelstat", 
                      "diagnostics.fcast-insample-mean", "diagnostics.fcast-outsample-mean", 
                      "diagnostics.fcast-outsample-variance", "diagnostics.seas-lin-f", 
                      "diagnostics.seas-lin-qs", "diagnostics.seas-lin-kw", "diagnostics.seas-lin-friedman", 
                      "diagnostics.seas-lin-periodogram", "diagnostics.seas-lin-spectralpeaks", 
                      "diagnostics.seas-si-combined", "diagnostics.seas-si-evolutive", 
                      "diagnostics.seas-si-stable", "diagnostics.seas-res-f", "diagnostics.seas-res-qs", 
                      "diagnostics.seas-res-kw", "diagnostics.seas-res-friedman", 
                      "diagnostics.seas-res-periodogram", "diagnostics.seas-res-spectralpeaks", 
                      "diagnostics.seas-res-combined", "diagnostics.seas-res-combined3", 
                      "diagnostics.seas-res-evolutive", "diagnostics.seas-res-stable", 
                      "diagnostics.seas-i-f", "diagnostics.seas-i-qs", "diagnostics.seas-i-kw", 
                      "diagnostics.seas-i-periodogram", "diagnostics.seas-i-spectralpeaks", 
                      "diagnostics.seas-i-combined", "diagnostics.seas-i-combined3", 
                      "diagnostics.seas-i-evolutive", "diagnostics.seas-i-stable", 
                      "diagnostics.seas-sa-f", "diagnostics.seas-sa-qs", "diagnostics.seas-sa-kw", 
                      "diagnostics.seas-sa-friedman", "diagnostics.seas-sa-periodogram", 
                      "diagnostics.seas-sa-spectralpeaks", "diagnostics.seas-sa-combined", 
                      "diagnostics.seas-sa-combined3", "diagnostics.seas-sa-evolutive", 
                      "diagnostics.seas-sa-stable", "diagnostics.seas-sa-ac1", 
                      "diagnostics.td-sa-all", "diagnostics.td-sa-last", "diagnostics.td-i-all", 
                      "diagnostics.td-i-last", "diagnostics.td-res-all", "diagnostics.td-res-last", 
                      "diagnostics.ic-ratio-henderson", "diagnostics.ic-ratio", 
                      "diagnostics.msr-global", "diagnostics.msr(*)", "decomposition.parameters_cutoff", 
                      "decomposition.model_changed", "decomposition.tvar-estimator", 
                      "decomposition.tvar-estimate", "decomposition.tvar-pvalue", 
                      "decomposition.savar-estimator", "decomposition.savar-estimate", 
                      "decomposition.savar-pvalue", "decomposition.svar-estimator", 
                      "decomposition.svar-estimate", "decomposition.svar-pvalue", 
                      "decomposition.ivar-estimator", "decomposition.ivar-estimate", 
                      "decomposition.ivar-pvalue", "decomposition.tscorr-estimator", 
                      "decomposition.tscorr-estimate", "decomposition.tscorr-pvalue", 
                      "decomposition.ticorr-estimator", "decomposition.ticorr-estimate", 
                      "decomposition.ticorr-pvalue", "decomposition.sicorr-estimator", 
                      "decomposition.sicorr-estimate", "decomposition.sicorr-pvalue", 
                      "decomposition.ar_root(*)", "decomposition.ma_root(*)", "diagnostics.basic checks.definition:2", 
                      "diagnostics.basic checks.annual totals:2", "diagnostics.visual spectral analysis.spectral seas peaks", 
                      "diagnostics.visual spectral analysis.spectral td peaks", 
                      "diagnostics.regarima residuals.normality:2", "diagnostics.regarima residuals.independence:2", 
                      "diagnostics.regarima residuals.spectral td peaks:2", "diagnostics.regarima residuals.spectral seas peaks:2", 
                      "diagnostics.outliers.number of outliers:2", "diagnostics.out-of-sample.mean:2", 
                      "diagnostics.out-of-sample.mse:2", "diagnostics.seats.seas variance:2", 
                      "diagnostics.seats.irregular variance:2", "diagnostics.seats.seas/irr cross-correlation:2", 
                      "diagnostics.m-statistics.q:2", "diagnostics.m-statistics.q-m2:2", 
                      "diagnostics.residual trading days tests.f-test on sa (td):2", 
                      "diagnostics.residual trading days tests.f-test on i (td):2", 
                      "diagnostics.residual seasonality tests.qs test on sa:2", 
                      "diagnostics.residual seasonality tests.qs test on i:2", 
                      "diagnostics.residual seasonality tests.f-test on sa (seasonal dummies):2", 
                      "diagnostics.residual seasonality tests.f-test on i (seasonal dummies):2", 
                      "diagnostics.combined seasonality test.combined seasonality test on sa:2", 
                      "diagnostics.combined seasonality test.combined seasonality test on sa (last 3 years):2", 
                      "diagnostics.combined seasonality test.combined seasonality test on irregular:2", 
                      "diagnostics.quality"), 
      tsmatrix_series = c("y", "y_f", "y_ef", 
                          "yc", "yc_f", "yc_ef", "l", "y_lin", "y_lin_f", "ycal", "ycal_f", 
                          "det", "det_f", "l_f", "l_b", "cal", "cal_f", "tde", "tde_f", 
                          "mhe", "mhe_f", "ee", "ee_f", "omhe", "omhe_f", "out", "out_f", 
                          "out_i", "out_i_f", "out_t", "out_t_f", "out_s", "out_s_f", 
                          "reg", "reg_f", "reg_t", "reg_t_f", "reg_s", "reg_s_f", "reg_i", 
                          "reg_i_f", "reg_sa", "reg_sa_f", "reg_y", "reg_y_f", "reg_u", 
                          "reg_u_f", "fullresiduals", "fcasts(?)", "bcasts(?)", "lin_fcasts(?)", 
                          "lin_bcasts(?)", "efcasts(?)", "decomposition.y_cmp", "decomposition.y_cmp_f", 
                          "decomposition.t_cmp", "decomposition.t_cmp_f", "decomposition.sa_cmp", 
                          "decomposition.s_cmp", "decomposition.s_cmp_f", "decomposition.i_cmp", 
                          "decomposition.a-tables.a1", "decomposition.a-tables.a1a", 
                          "decomposition.a-tables.a1b", "decomposition.a-tables.a6", 
                          "decomposition.a-tables.a7", "decomposition.a-tables.a8", 
                          "decomposition.a-tables.a8t", "decomposition.a-tables.a8s", 
                          "decomposition.a-tables.a8i", "decomposition.a-tables.a9", 
                          "decomposition.a-tables.a9sa", "decomposition.a-tables.a9u", 
                          "decomposition.a-tables.a9ser", "decomposition.b-tables.b1", 
                          "decomposition.b-tables.b2", "decomposition.b-tables.b3", 
                          "decomposition.b-tables.b4", "decomposition.b-tables.b5", 
                          "decomposition.b-tables.b6", "decomposition.b-tables.b7", 
                          "decomposition.b-tables.b8", "decomposition.b-tables.b9", 
                          "decomposition.b-tables.b10", "decomposition.b-tables.b11", 
                          "decomposition.b-tables.b12", "decomposition.b-tables.b13", 
                          "decomposition.b-tables.b14", "decomposition.b-tables.b15", 
                          "decomposition.b-tables.b16", "decomposition.b-tables.b17", 
                          "decomposition.b-tables.b18", "decomposition.b-tables.b19", 
                          "decomposition.b-tables.b20", "decomposition.c-tables.c1", 
                          "decomposition.c-tables.c2", "decomposition.c-tables.c3", 
                          "decomposition.c-tables.c4", "decomposition.c-tables.c5", 
                          "decomposition.c-tables.c6", "decomposition.c-tables.c7", 
                          "decomposition.c-tables.c8", "decomposition.c-tables.c9", 
                          "decomposition.c-tables.c10", "decomposition.c-tables.c11", 
                          "decomposition.c-tables.c12", "decomposition.c-tables.c13", 
                          "decomposition.c-tables.c14", "decomposition.c-tables.c15", 
                          "decomposition.c-tables.c16", "decomposition.c-tables.c17", 
                          "decomposition.c-tables.c18", "decomposition.c-tables.c19", 
                          "decomposition.c-tables.c20", "decomposition.d-tables.d1", 
                          "decomposition.d-tables.d2", "decomposition.d-tables.d3", 
                          "decomposition.d-tables.d4", "decomposition.d-tables.d5", 
                          "decomposition.d-tables.d6", "decomposition.d-tables.d7", 
                          "decomposition.d-tables.d8", "decomposition.d-tables.d9", 
                          "decomposition.d-tables.d10", "decomposition.d-tables.d10a", 
                          "decomposition.d-tables.d10b", "decomposition.d-tables.d11", 
                          "decomposition.d-tables.d11a", "decomposition.d-tables.d12", 
                          "decomposition.d-tables.d12a", "decomposition.d-tables.d13", 
                          "decomposition.d-tables.d14", "decomposition.d-tables.d15", 
                          "decomposition.d-tables.d16", "decomposition.d-tables.d16a", 
                          "decomposition.d-tables.d16b", "decomposition.d-tables.d18", 
                          "decomposition.d-tables.d19", "decomposition.d-tables.d20", 
                          "decomposition.e-tables.e1", "decomposition.e-tables.e2", 
                          "decomposition.e-tables.e3", "decomposition.e-tables.e11", 
                          "t", "t_f", "sa", "sa_f", "s", "s_f", "i", "i_f", "benchmarking.original", 
                          "benchmarking.target", "benchmarking.result", "decomposition.y_lin", 
                          "decomposition.y_lin_f", "decomposition.y_lin_ef", "decomposition.t_lin", 
                          "decomposition.t_lin_f", "decomposition.t_lin_e", "decomposition.t_lin_ef", 
                          "decomposition.sa_lin", "decomposition.sa_lin_f", "decomposition.sa_lin_e", 
                          "decomposition.sa_lin_ef", "decomposition.s_lin", "decomposition.s_lin_f", 
                          "decomposition.s_lin_e", "decomposition.s_lin_ef", "decomposition.i_lin", 
                          "decomposition.i_lin_f", "decomposition.i_lin_e", "decomposition.i_lin_ef", 
                          "decomposition.sa_cmp_f", "decomposition.i_cmp_f", "decomposition.i_cmp_e", 
                          "decomposition.t_cmp_e", "decomposition.s_cmp_e", "decomposition.sa_cmp_e", 
                          "decomposition.i_cmp_ef", "decomposition.t_cmp_ef", "decomposition.s_cmp_ef", 
                          "decomposition.sa_cmp_ef", "decomposition.si_cmp"), 
      paths_path = NULL)
  if(! is.null(cruncher_bin_directory)) {
    if (!file.exists(file.path(cruncher_bin_directory,"jwsacruncher")))
      stop (sprintf("JWSACruncher not found in %s.\n Check the installation", paste0(cruncher_bin_directory,"/jwsacruncher"))) 
    param_file <- file.path(cruncher_bin_directory, "wsacruncher.params")
    exists_param_file <- file.exists(param_file)
    if (!exists_param_file) {
      wd <- getwd()
      setwd(cruncher_bin_directory)
      on.exit(setwd(wd))
      os <- Sys.info()[['sysname']]
      if (os == "Windows") {
        log <- system(
          "jwsacruncher", intern = TRUE)
      } else {
        # Mac OS and linux
        log <- system(
          "./jwsacruncher", 
          intern = TRUE)
      }
    }
    res <- read_param_file(param_file)
    if (!exists_param_file)
      file.remove(param_file)
  } else if (v3) {
    res <- v3_param
  } else {
    res <- v2_param
  }
  return(res)
}
