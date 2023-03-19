
# ------------------------------------------------------------------------------
#' Title Python Connector Server
#'
#' @param id server module id
#' @param script_path path to the python script files
#' @param script_file if provided, only this list of scripts will be loaded
#' @param dependencies path where to find the python_requirements.txt file containing the package dependencies for installation
#' @param skip_init logical, if TRUE python initialization will be skiped
#'
#' @description Python connector implementation through Reticulate package.
#' configure Python virtualenv and paths in .Rprofile file.
#'
#' @export pythonConnector_Server
#' @import shiny reticulate
#'
#' @examples


# ------------------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------------------

pythonConnector_Server <- function(id, script_path, script_file = NULL, dependencies = NULL, skip_init = FALSE) {
  moduleServer(id, function(input, output, session) {

    # -- Get namespace
    ns <- session$ns
    cat("[PYTHON] Starting Python Connector Server... \n")

    # -- check Python script path
    if(!dir.exists(script_path)) stop('Python script path does not exist! \n')

    # -- Read Python packages needed for the app:
    if(!is.null(dependencies)){

      target_file <- file.path(dependencies, "python_requirements.txt")
      cat("[PYTHON] Reading python_requirements file:", target_file, "\n")
      PYTHON_DEPENDENCIES <- readLines(con <- file(target_file, encoding = "UTF-8"))
      close(con)

      cat("[PYTHON] Requirements:", PYTHON_DEPENDENCIES, "\n")

    } else PYTHON_DEPENDENCIES <- NULL


    # -- check skip argument
    if(skip_init){

      cat("   [WARNING] skip_init == TRUE, no python env init will be done! \n")
      cat("   [PYTHON] Which python =", Sys.which('python'), "\n")

      # Install python package
      if(!is.null(PYTHON_DEPENDENCIES)){
        cat("[PYTHON] Install dependencies :", PYTHON_DEPENDENCIES, "\n")
        reticulate::py_install(packages = PYTHON_DEPENDENCIES, envname = "r-reticulate", method = "auto")}

    } else {

      # -- check if already initialized
      if(py_available()){

        cat("   [WARNING] Python is already initialized! \n")
        cat("   [PYTHON] Which python =", Sys.which('python'), "\n")

      } else {

        # -- Get environment variables
        virtualenv_dir <- Sys.getenv('VIRTUALENV_NAME')
        cat("[PYTHON] VIRTUALENV_NAME:", virtualenv_dir, "\n")

        python_path <- Sys.getenv('PYTHON_PATH')
        cat("[PYTHON] PYTHON_PATH:", python_path, "\n")


        # ----------------------------------------------------------------------------
        # Python environments setup
        # ----------------------------------------------------------------------------

        # check user
        if (Sys.info()[['user']] %in% c('shiny', 'rstudio-connect')){

          # -- Running on Remote Server
          cat("[PYTHON] Running on Remote Server [user] =", Sys.info()[['user']], "\n")

          # Create virtual env and install dependencies
          cat("[PYTHON] Create virtual environment :", virtualenv_dir, "\n")
          reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)

          # Install python package
          if(!is.null(PYTHON_DEPENDENCIES)){
            cat("[PYTHON] Install dependencies :", PYTHON_DEPENDENCIES, "\n")
            reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)}

          # Set use env
          reticulate::use_virtualenv(virtualenv_dir, required = T)
          cat("[PYTHON] Environment ready!", virtualenv_dir, "\n")

        } else {

          # -- Running locally
          cat("[PYTHON] Running on local machine [user] =", Sys.info()[['user']], "\n")
          cat("[PYTHON] Working with condaenv =", virtualenv_dir, "\n")

          # Setup anaconda env
          reticulate::use_condaenv(condaenv = virtualenv_dir, conda = "auto", required = FALSE)

        }
      }
    }


    # ----------------------------------------------------------------------------
    # Source external Python code
    # ----------------------------------------------------------------------------

    # log
    cat("[PYTHON] Function import into: ")
    utils::str(globalenv())

    # source script
    if(!is.null(script_file)){

      reticulate::source_python(script_file, envir = globalenv())

    } else {

      cat("[PYTHON] Loading python script from: ", script_path, "\n")
      for (nm in list.files(script_path, full.names = TRUE, recursive = TRUE, include.dirs = FALSE))
      {
        # Evaluate and convert Python script
        reticulate::source_python(nm, envir = globalenv())

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
      return(DT::datatable(df, rownames = F, selection = 'none',
                           style = 'bootstrap', filter = 'none', options = list(dom = 't')))
    })

    # System path to python
    output$which_python <- renderText({
      paste0('Python path: ', Sys.which('python'))
    })

    # Python version
    output$python_version <- renderText({
      rr = reticulate::py_discover_config()
      paste0('Python version: ', rr$version)
    })


    # --------------------------------------------------------------------------
    # *** END SERVER ***
    # --------------------------------------------------------------------------
  })
}


