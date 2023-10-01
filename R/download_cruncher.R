#' Dowload the 'JWSACruncher'
#'
#' Function to download the ZIP file of the 'JWSACruncher'
#'
#' @param directory Directory where to save the 'JWSACruncher'. In Windows, a dialog box opens 
#' by default to select the directory.
#' @param cruncher_version Character of the version of the 'JWSACruncher' to download ("X.Y.Z" format). By default the last version is downloaded.
#' @param v3 Boolean indicating, when parameter \code{cruncher_version} is missing, if the last version of the 'JWSACruncher' should be a 3.x.y version or a 2.x.y. By default 
#' the value of the option \code{"is_cruncher_v3"} is used (equals to \code{FALSE} by default).
#' @details The 'JWSACruncher' is downloaded from <https://github.com/jdemetra/jwsacruncher/releases> for versions lower than 3.0.0 and from <https://github.com/jdemetra/jdplus-main/releases> for the other versions.
#'  To use it, it has to be unzip.
#' @encoding UTF-8
#' @examples \dontrun{
#' # On Windows opens a dialog box to choose the directory where to
#' # download the last version of the 'JWSACruncher'
#' download_cruncher()
#' 
#' dir <- tempdir()
#' # To download the last release:
#' download_cruncher(dir)
#' 
#' # To download the release of the version 2.2.2:
#' download_cruncher(dir, "2.2.2")
#' }
#' @seealso [configure_jwsacruncher()].
#' @export
download_cruncher <- function(directory, cruncher_version, v3 = getOption("is_cruncher_v3")){
  if (missing(directory)) {
    if (Sys.info()[['sysname']] == "Windows") {
      directory <- utils::choose.dir(caption = "Choose to directory where to download the 'JWSACruncher'")
      
      if (is.na(directory)) {
        stop("You must select or specify the directory where to export the 'JWSACruncher'")
      }
    }else{
      stop("You must specify the directory where to export the 'JWSACruncher'")
    }
  }
  
  if (missing(cruncher_version)) { 
    if (v3) {
      url_release <- "https://api.github.com/repos/jdemetra/jdplus-main/releases/latest"
    } else {
      url_release <- "https://api.github.com/repos/jdemetra/jwsacruncher/releases/latest"
    }
  }else{
    v3 <- cruncher_version >= "3.0.0"
    if (v3) {
      url_release <- sprintf("https://api.github.com/repos/jdemetra/jdplus-main/releases/tags/v%s",
                             cruncher_version)
    } else {
      url_release <- sprintf("https://api.github.com/repos/jdemetra/jwsacruncher/releases/tags/v%s",
                             cruncher_version)
    }
  }
 
  tryCatch(release_url <- readLines(url_release,
                                    warn = FALSE),
           error = function(e){
             stop("Error downloading the cruncher. Check the URL:", url)
           })
  release_url <- strsplit(release_url, ",")[[1]]
  release_url <- grep("browser_download_url", release_url, value = TRUE)
  release_url <- gsub("^.*browser_download_url\":\"", "", release_url)
  release_url <- gsub("\".*", "", release_url)
  release_url <- grep("\\.zip$", release_url, value = TRUE)
  release_url <- grep("jwsacruncher", release_url, value = TRUE)
  if (v3) {
    release_url <- grep("windows", release_url, value = TRUE,
                        invert = Sys.info()[['sysname']] != "Windows")
  }
  zip_name <- gsub(".*/", "", release_url)
  utils::download.file(release_url, file.path(directory, zip_name))
  return(invisible(TRUE))
}

# Autre possibilité
# if (missing(cruncher_version)) {
#   url_gh <- "GET /repos/jdemetra/jwsacruncher/releases/latest"
# }else{
#   url_gh <- sprintf("GET /repos/jdemetra/jwsacruncher/releases/tags/v%s",
#                     cruncher_version)
# }
# list_url <- sapply(gh(url_gh)[["assets"]],
#                    "[[","browser_download_url")
# release_url <- grep("\\.zip$", list_url, value = TRUE)
# zip_name <- gsub(".*/", "", release_url)

#' Configure the 'JWSACruncher' with a portable version of 'Java'
#'
#' Function configure the 'JWSACruncher' with a portable version of 'Java'.
#'
#' @param jwsacruncher_path Path to the file \code{jwsacruncher.bat} of the 'JWSACruncher' (see details).
#' @param java_path Path to the file \code{java.exe} of the portable version of 'Java' (see details).
#' @details Since the version 2.2.0, the 'JWSACruncher' needs 'Java' 8 or higher to run. 
#' For versions 3.0.0 and higher, 'JWSACruncher' needs 'Java' 17 or higher.
#' In 'Windows' versions 3.0.0 and higher of 'JWSACruncher' includes a portable version of 'Java'.
#' For lower version of 'JWSACruncher', if you cannot install 'Java' (for example for security reasons) you can install a portable version of 'Java' 
#' (that does not require administrator rights) and configure the 'JWSACruncher' to use this portable version. 
#' To do it you have to:
#' \enumerate{
#' \item Unzip the downloaded file of the 'JWSACruncher';   
#' \item Open, with a Text Editor, the file \code{jwsacruncher.bat} that is in the sub-folder \code{/bin/} of the 'JWSACruncher';   
#' \item Edit the line 71 that contains \code{if "\%JAVACMD\%"=="" set JAVACMD=java} and replace \code{java} 
#' by the path to the file \code{java.exe} of the portable version. For example, if the portable 
#' version of 'Java' is installed under \code{D:/Software/Java}, the path to \code{java.exe} should be at 
#' \code{D:/Software/Java/bin/java.exe} and the new line 71 would be 
#' \code{if "\%JAVACMD\%"=="" set JAVACMD="D:\\Software\\Java\\bin\\java.exe"}.
#' }
#' The function \code{configure_jwsacruncher()} does the steps 2 and 3.
#' 
#' @seealso [download_cruncher()].
#' @encoding UTF-8
#' @export
configure_jwsacruncher <- function(jwsacruncher_path, java_path){

  if (missing(jwsacruncher_path) || is.null(jwsacruncher_path)) {
    if (Sys.info()[['sysname']] == "Windows") {
      jwsacruncher_path <- utils::choose.files(caption = "Select the file 'jwsacruncher.bat'",
                                       filters = c("jwsacruncher.bat","jwsacruncher.bat"),
                                       multi = FALSE)
      if (length(jwsacruncher_path) == 0)
        stop("You have to select a 'jwsacruncher.bat' file")
    }else{
      jwsacruncher_path <- base::file.choose()
    }
  }
  if (missing(java_path) || is.null(java_path)) {
    if (Sys.info()[['sysname']] == "Windows") {
      java_path <- utils::choose.files(caption = "Select the file 'java.exe'",
                                               filters = c("java.exe","java.exe"),
                                               multi = FALSE)
      if (length(java_path) == 0)
        stop("You have to select a 'java.exe' file")
    }else{
      java_path <- base::file.choose()
    }
  }
  jwsacruncher_path <- full_path(jwsacruncher_path)
  jwsacruncher <- readLines(jwsacruncher_path)
  line_to_change <- grep('if "%JAVACMD%"=="" set JAVACMD=', jwsacruncher)
  if (length(line_to_change) == 0)
    stop(paste0("No line to change: '",
                'if "%JAVACMD%"=="" set JAVACMD=',"' not found in the file ",
                jwsacruncher_path))
  if (length(line_to_change) != 1)
    stop(paste0('\'if "%JAVACMD%"=="" set JAVACMD=\' found more than once in the file',
                jwsacruncher_path,
                "\nDownload again the 'JWSACruncher'"))
  
  java_path <- gsub("/","\\\\",
                    full_path(java_path))
  jwsacruncher[line_to_change] <- paste0('if "%JAVACMD%"=="" set JAVACMD=',
                                         java_path)
  writeLines(jwsacruncher, jwsacruncher_path)

  return(invisible(TRUE))
}
