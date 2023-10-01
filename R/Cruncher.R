#' Run the 'JWSACruncher'
#'
#' Function to run the 'JWSACruncher' on a workspace from a parameter file.
#'
#' @param workspace Path to the workspace. By default a dialog box opens to choose the workspace.
#' @param cruncher_bin_directory Path to the "bin" directory of the 'JWSACruncher'. By default 
#' the value of the option \code{"cruncher_bin_directory"} is used.
#' @param param_file_path Path to the parameter file of the 'JWSACruncher'. By default a .params file is search in the save directory of the workspace.
#' @param log_file Name of the log file of 'JWSACruncher'. By default the log isn't exported.
#' @encoding UTF-8
#' @return The path to the workspace.
#' @seealso Around the 'JWSACruncher': [cruncher_and_param()], [update_workspace()].
#' To create the parameter file: [create_param_file()] and [list2param_file()].
#' @examples
#' \dontrun{
#' dir = tempdir()
#' # First create a parameter file
#' # here a file parameters.param is created in the directory dir
#' # with default parameters of 'JWSACruncher' v2
#' list2param_file(dir, default_param_file(v3 = FALSE))
#' # If the option "cruncher_bin_directory" is correctly defined:
#' cruncher("workspace.xml",
#'           param_file_path = file.path(dir, "parameters.param"))
#' }
#' 
#' @export
cruncher <- function(workspace,
                     cruncher_bin_directory = getOption("cruncher_bin_directory"),
                     param_file_path, log_file = NULL){
  if (is.null(cruncher_bin_directory))
    stop("You must specify the path to the cruncher")
  
  if (missing(workspace) || is.null(workspace)) {
    if (Sys.info()[['sysname']] == "Windows") {
      workspace <- utils::choose.files(caption = "Select a workspace",
                                       filters = c("XML file","*.xml"),
                                       multi = FALSE)
    }else{
      workspace <- base::file.choose()
    }
  }
  
  if (length(workspace) == 0)
    stop("You have to select a workspace")
  
  # The complete path to the workspace is needed
  workspace <- full_path(workspace)
  # Remove if necessary the .xml in the path to the workspace
  workspace <- sub("\\.xml$","",workspace)
  
  if (missing(param_file_path) || is.null(param_file_path)) {
    param_file_path <- list.files(path = workspace,
                                  recursive = TRUE,
                                  pattern = "\\.params$",
                                  full.names = TRUE)
    if (length(param_file_path) != 0)
      stop("None or at least 2 .param files are found")
  }
  param_file_path <- full_path(param_file_path)
  
  workspace <- paste0(workspace,".xml")
  
  if (!file.exists(param_file_path))
    stop (sprintf("The parameter file %s doesn't exists", param_file_path))
  if (dir.exists(param_file_path))
    stop ("The parameter file must be a file and not a directory")
  
  if (!file.exists(workspace))
    stop (sprintf("The workspace %s doesn't exists", workspace)) 
  
  if (!file.exists(paste0(cruncher_bin_directory,"/jwsacruncher")))
    stop (sprintf("JWSACruncher not found in %s.\n Check the installation", paste0(cruncher_bin_directory,"/jwsacruncher"))) 
  
  if (!missing(log_file) && !is.null(log_file))
    log_file <- full_path(log_file)
  
  wd <- getwd()
  setwd(cruncher_bin_directory)
  on.exit(setwd(wd))
  
  os <- Sys.info()[['sysname']]
  if (os == "Windows") {
    log <- system(paste0(
      "jwsacruncher \""
      , workspace
      ,"\" -x \""
      , param_file_path,"\""
    ), intern = TRUE)
  } else {
    # Mac OS and linux
    log <- system(paste0(
      "./jwsacruncher \""
      , workspace
      ,"\" -x \""
      , param_file_path,"\""
    ), intern = TRUE)
  }
  
  if (!missing(log_file) && !is.null(log_file))
    writeLines(text = log, con = log_file)
  
  return(invisible(workspace))
}

#' Run quickly the 'JWSACruncher'
#'
#' Function to run the 'JWSACruncher' on a workspace while creating the parameter file.
#'
#' @inheritParams cruncher
#' @inheritParams create_param_file
#' @param rename_multi_documents Boolean indicating whether to rename the folders 
#' containing the outputs according to the names of the multi-documents of the workspace. 
#' By default \code{rename_multi_documents = FALSE}: the names of the XML files of the multi-documents are used.
#' @param delete_existing_file Only used if \code{rename_multi_documents = TRUE}. Boolean indicating whether to 
#' delete existing folders when renaming them. By default (\code{delete_existing_file = FALSE}) they are not deleted.
#' @param ... Other parameters of [create_param_file()].
#' @seealso [cruncher()], [update_workspace()], [create_param_file()], [multiprocessing_names()].
#' @encoding UTF-8
#' @return Path to the workspace.
#' @export
cruncher_and_param <- function(workspace = NULL,
                               output = NULL,
                               rename_multi_documents = FALSE,
                               delete_existing_file = FALSE,
                               log_file = NULL,
                               cruncher_bin_directory = getOption("cruncher_bin_directory"),
                               ...){
  
  dossier_temp <- tempdir()
  fichier_param <- create_param_file(dossier_temp, output = output, ...)
  workspace <- cruncher(workspace = workspace, cruncher_bin_directory = cruncher_bin_directory,
                        param_file_path = fichier_param, log_file = log_file)
  
  if (rename_multi_documents) {
    if (is.null(output))
      output <- paste0(sub("\\.xml","",workspace),"\\Output")
    
    noms_multi_documents <- multiprocessing_names(workspace)
    if (nrow(noms_multi_documents) == 0)
      stop("No multi-document in the workspace")
    noms_multi_documents$name <- file.path(output,noms_multi_documents$name)
    noms_multi_documents$file <- file.path(output, noms_multi_documents$file)
    noms_multi_documents <- noms_multi_documents[noms_multi_documents$name != noms_multi_documents$file,]
    
    if (any(file.exists(noms_multi_documents$name))) {
      
      if (delete_existing_file) {
        unlink(noms_multi_documents$name[file.exists(noms_multi_documents$name)],
               recursive = TRUE)
        file.rename(from = noms_multi_documents$file, to = noms_multi_documents$name)
      }else{
        warning("Some folders already exist: none are renamed")
      }
      
    }else{
      file.rename(from = noms_multi_documents$file, to = noms_multi_documents$name)
    }
    
  }
  
  return(invisible(workspace))
}

#' Get the names of the multiprocessings of a workspace
#'
#' Function to get the name of the multiprocessings that appears on 'JDemetra+' and the name of the corresponding XML file.
#'
#' @inheritParams cruncher
#' @encoding UTF-8
#' @return A \code{data.frame} containing the name of the multiprocessings that appears on 'JDemetra+' (column \code{name}) and 
#' the name of the associated XML files (column \code{file}).
#' @seealso [cruncher_and_param()].
#' @export
multiprocessing_names <- function(workspace){
  if (missing(workspace) || is.null(workspace)) {
    if (Sys.info()[['sysname']] == "Windows") {
      workspace <- utils::choose.files(caption = "Select a workspace",
                                       filters = c("Fichier XML","*.xml"),
                                       multi = FALSE)
    }else{
      workspace <- base::file.choose()
    }
  }
  
  if (length(workspace) == 0)
    stop("You have to select a workspace")
  
  workspace <- full_path(workspace)
  workspace <- paste0(sub("\\.xml$","",workspace),".xml")
  
  if (!file.exists(workspace))
    stop("Le workspace n'existe pas")
  
  xml_workspace <- suppressWarnings(XML::xmlParse(workspace, error = function(...){}))
  noms_objets <- XML::xmlToDataFrame(nodes = XML::getNodeSet(xml_workspace,
                                                             "//ns2:demetraGenericWorkspace/ns2:items/ns2:item"))
  noms_multi_documents <- noms_objets[grep("multi-documents",noms_objets$family),]
  noms_multi_documents <- noms_multi_documents[,c("name","file")]
  
  
  return(noms_multi_documents)
}

#' Update a workspace
#'
#' Function to update a workspace without exporting the results.
#'
#' @inheritParams cruncher
#' @inheritParams create_param_file
#' @encoding UTF-8
#' @return Path to the workspace.
#' @seealso [cruncher()], [cruncher_and_param()].
#' @export
update_workspace <- function(workspace = NULL,
                             policy = "parameters",
                             cruncher_bin_directory = getOption("cruncher_bin_directory"),
                             log_file = NULL){
  
  dossier_temp <- tempdir()
  fichier_param <- create_param_file(dossier_temp, output = dossier_temp, policy = policy,
                                     matrix_item = NULL, tsmatrix_series = NULL)
  workspace <- cruncher(workspace = workspace, cruncher_bin_directory = cruncher_bin_directory,
                        param_file_path = fichier_param,
                        log_file = log_file)
  
  
  return(invisible(workspace))
}


full_path <- function(path) {
  base::file.path(
    base::normalizePath(dirname(path), mustWork = TRUE, winslash = "/"),
    base::basename(path),
    fsep = "/")
}
