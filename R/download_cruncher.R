#' Dowload the 'JWSACruncher'
#'
#' Function to download the .zip file of the 'JWSACruncher'
#'
#' @param directory directory where to save the 'JWSACruncher'. In Windows, a dialog box opens 
#' by default to select the directory.
#' @param cruncher_version character of the version of the 'JWSACruncher' to download ("X.Y.Z" format). By default the last version is downloaded.
#' @details The 'JWSACruncher' is downloaded from <https://github.com/jdemetra/jwsacruncher/releases>
#' @encoding UTF-8
#' @examples \dontrun{
#' # In Windows opens a dialog box to choose the directory where to download the last
#' # version of the 'JWSACruncher'
#' download_cruncher()
#' 
#' dir <- tempdir()
#' # To download the last release:
#' download_cruncher(dir)
#' 
#' # To download the release of the version 2.2.2:
#' download_cruncher(dir, "2.2.2")
#' }
#' @export
download_cruncher <- function(directory, cruncher_version){
  if(missing(directory)){
    if(Sys.info()[['sysname']] == "Windows"){
      directory <- utils::choose.dir(caption = "Choose to directory where to download the 'JWSACruncher'")
      
      if(is.na(directory)){
        stop("You must select or specify the directory where to export the 'JWSACruncher'")
      }
    }else{
      stop("You must specify the directory where to export the 'JWSACruncher'")
    }
  }
  
  if(missing(cruncher_version)){
    url_release <- "https://api.github.com/repos/jdemetra/jwsacruncher/releases/latest"
  }else{
    url_release <- sprintf("https://api.github.com/repos/jdemetra/jwsacruncher/releases/tags/v%s",
                           cruncher_version)
  }
 
  tryCatch(release_url <- readLines(url_release,
                                    warn = FALSE),
           error = function(e){
             stop("Error downloading the cruncher. Check the URL:", url)
           })
  release_url <- gsub("^.*browser_download_url\":\"", "", release_url)
  release_url <- gsub("\".*", "", release_url)
  zip_name <- gsub(".*/", "", release_url)
  utils::download.file(release_url, file.path(directory, zip_name))
  return(invisible(TRUE))
}


configure_cruncher <- function(jwsacruncher_path, java_path){
  if(missing(jwsacruncher_path)){
    if(Sys.info()[['sysname']] == "Windows"){
      jwsacruncher_path <- utils::choose.dir(caption = "Choose to directory to the 'JWSACruncher'")
      
      if(is.na(jwsacruncher_path)){
        stop("You must select or specify the path to the 'JWSACruncher'")
      }
    }else{
      stop("You must specify the path to the 'JWSACruncher'")
    }
  }
  if(missing(java_path)){
    if(Sys.info()[['sysname']] == "Windows"){
      java_path <- utils::choose.dir(caption = "Choose to directory to a portable version of Java")
      
      if(is.na(java_path)){
        stop("You must select or specify the path to a portable version of Java")
      }
    }else{
      stop("You must specify the path to a portable version of Java")
    }
  }
  
  return(invisible(TRUE))
}



