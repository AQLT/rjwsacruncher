#' Check Cruncher configuration
#' This function checks if the Cruncher configuration is valid by verifying the presence of the `jwsacruncher` executable in the specified `cruncher_bin_directory`. 
#' 
#' @inheritParams cruncher
#' @param print_message Logical. 
#' If `TRUE`, a message will be printed indicating whether the configuration is valid or not.
#' @export
check_cruncher_configuration <- function(
    cruncher_bin_directory = getOption("cruncher_bin_directory"),
    print_message = TRUE
) {
  if (is.null(cruncher_bin_directory)) {
    if (print_message)
      message("cruncher_bin_directory is not set. Please set it using options(cruncher_bin_directory = 'path/to/cruncher/bin').")
    return(FALSE)
  }
  if (file.exists(paste0(cruncher_bin_directory,"/jwsacruncher"))) {
    if (print_message)
      message("Cruncher configuration is valid.")
    return(TRUE)
  } else {
    if  (file.exists(paste0(cruncher_bin_directory,"/bin","/jwsacruncher"))) {
      if (print_message)
        message(
          paste0(
            "Cruncher configuration is not valid: ",
            "you should specify the path to the 'bin' directory not the folder of the 'jwsacruncher'.\n",
            "However, the 'cruncher()' functions will still work."
          )
        )
      return(FALSE)
    } else {
      if (print_message)
        message(
          "Cruncher configuration is not valid: 'jwsacruncher' executable not found in the specified directory. Please check the installation and the path provided."
        )
      return(FALSE)
    }
  }
}
