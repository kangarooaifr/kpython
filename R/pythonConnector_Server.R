
# ------------------------------------------------------------------------------
#' Title Python Connector Server
#'
#' @param id
#' @param script_path
#' @param file
#'
#' @description Python connector implementation through Reticulate package.
#' configure Python virtualenv and paths in .Rprofile file.
#'
#' @return
#' @export pythonConnector_Server
#' @import shiny reticulate
#'
#' @examples


# ------------------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------------------

pythonConnector_Server <- function(id, script_path, script_file = NULL, dependencies = NULL) {
  moduleServer(id, function(input, output, session) {

    # -- Get namespace
    ns <- session$ns

    # log
    cat("[PYTHON] Starting Python Connector Server... \n")

    # check path
    if(!dir.exists(script_path)) stop('Python script path does not exist! \n')

    # -- Python packages needed for the app:
    PYTHON_DEPENDENCIES <- NULL
    if(!is.null(dependencies)){

      #PYTHON_DEPENDENCIES <- c('pip', 'numpy')
      cat("[PYTHON] Reading python_requirements file \n")
      PYTHON_DEPENDENCIES <- readLines(con <- file(file.path(dependencies, "python_requirements.txt"), encoding = "UTF-8"))
      close(con)

    }

    # -- Get environment variables
    virtualenv_dir <- Sys.getenv('VIRTUALENV_NAME')
    python_path <- Sys.getenv('PYTHON_PATH')


    # ----------------------------------------------------------------------------
    # Python environments setup
    # ----------------------------------------------------------------------------

    # check user
    if (Sys.info()[['user']] %in% c('shiny', 'rstudio-connect')){

      # -- Running on Remote Server
      # Create virtual env and install dependencies
      cat("[PYTHON] Create virtual environment :", virtualenv_dir, "\n")
      reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
      cat("[PYTHON] Install dependencies :", PYTHON_DEPENDENCIES, "\n")
      reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)
      reticulate::use_virtualenv(virtualenv_dir, required = T)
      cat("[PYTHON] Environment ready!", PYTHON_DEPENDENCIES, "\n")

    } else {

      # -- Running locally
      cat("Running on local machine [user] =", Sys.info()[['user']], "\n")

      # Setup anaconda env
      use_condaenv(condaenv = virtualenv_dir, conda = "auto", required = FALSE)

    }


    # ----------------------------------------------------------------------------
    # Source external Python code
    # ----------------------------------------------------------------------------

    # log
    cat("Python function import into: ")
    str(globalenv())

    # source script
    if(!is.null(script_file)){

      source_python(script_file, envir = globalenv())

    } else {

      cat("   Loading python script from: ", script_path, "\n")
      for (nm in list.files(script_path, full.names = TRUE, recursive = TRUE, include.dirs = FALSE))
      {
        # Evaluate and convert Python script
        source_python(nm, envir = globalenv())

      }

    }


    # ----------------------------------------------------------------------------
    # Build outputs with python info
    # ----------------------------------------------------------------------------

    # Info about the system running the code
    output$sysinfo <- DT::renderDataTable({
      s = Sys.info()
      df = data.frame(Info_Field = names(s),
                      Current_System_Setting = as.character(s))
      return(datatable(df, rownames = F, selection = 'none',
                       style = 'bootstrap', filter = 'none', options = list(dom = 't')))
    })

    # System path to python
    output$which_python <- renderText({
      paste0('Python path: ', Sys.which('python'))
    })

    # Python version
    output$python_version <- renderText({
      rr = reticulate::py_discover_config(use_environment = 'python35_env')
      paste0('Python version: ', rr$version)
    })


    # --------------------------------------------------------------------------
    # *** END SERVER ***
    # --------------------------------------------------------------------------
  })
}


