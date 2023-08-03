#' Start the Survey Solutions Questionnaire Manual Application
#'
#' @description A wrappter function to start the application. Please make sure you have read the
#' documentation on how to use the app.
#'
#' @details
#' This application is part of the large set of tools, to facilitate survey implementation with
#' [Survey Solutions](https://docs.mysurvey.solutions/). Enumeration Areas, sampled for example
#' with the JDC-Survey Solutions Spatial Sampling application, can be used to enumerate all buildings
#' visible within the boundaries on a google map. The enumerate buildings can then be used for the
#' second stage sampling frame and to draw the survey units within the cluster from it.
#'
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinymaterial
#' @import DT
#' @importFrom jsonlite fromJSON
#' @importFrom data.table as.IDate data.table as.data.table as.ITime copy setnames setorderv tstrsplit
#' @importFrom shinyjs show
#' @import officer
#'
#' @importFrom readr write_csv col_character cols_only locale read_csv
#' @importFrom stats setNames
#' @importFrom shinyjs useShinyjs disable enable toggleState disabled click
#' @importFrom stringr str_remove_all str_replace_all str_starts str_split
#' @importFrom SurveySolutionsAPI suso_clear_keys suso_getQuestDetails suso_PwCheck suso_set_key
#' @importFrom tools file_ext R_user_dir
#' @importFrom utils unzip
#' @importFrom tippy tippy_this
#' @importFrom waiter spin_fading_circles
#'
#' @inherit shiny::runApp

#' @export
runQuestManualApp <- function(launch.browser = TRUE) {
  shiny::addResourcePath("www", system.file("www", package = "susoquestionnairemanual"))
  shiny::addResourcePath("rmdfiles", system.file("rmdfiles", package = "susoquestionnairemanual"))


  withr::local_options(
    # Temporary change of environment options
    list(
      shiny.maxRequestSize=5000*1024^2,
      spinner.color.background="#0d47a1"
    ))

  appObj<-shiny::shinyApp(ui = main_ui, server = main_server)
  shiny::runApp(appObj, launch.browser = launch.browser)
}
