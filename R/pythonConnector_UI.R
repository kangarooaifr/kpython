
# -------------------------------------------------------------
#' Which Python
#'
#' @param id the namespace used to call the module server side.
#'
#' @return a text output with path to the active python.
#' @export which_python_UI
#' @import shinycssloaders
#'
#' @examples which_python_UI("python")


which_python_UI <- function(id){

  # namespace
  ns <- NS(id)

  # build output
  withSpinner(verbatimTextOutput(ns('which_python')))

}


# -------------------------------------------------------------
#' Python Version
#'
#' @param id the namespace used to call the module server side.
#'
#' @return a text output containing active python version.
#' @export python_version_UI
#'
#' @examples python_version_UI("python")


python_version_UI <- function(id){

  # namespace
  ns <- NS(id)

  # build output
  withSpinner(verbatimTextOutput(ns('python_version')))

}


# -------------------------------------------------------------
#' Python Version
#'
#' @param id the namespace used to call the module server side.
#'
#' @return a DT table output containing system info.
#' @export sysinfo_UI
#' @importFrom DT dataTableOutput
#'
#' @examples sysinfo_UI("python")


sysinfo_UI <- function(id){

  # namespace
  ns <- NS(id)

  # build output
  withSpinner(DT::dataTableOutput(ns("sysinfo")))

}

