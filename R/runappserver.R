#' Start the Survey Solutions Questionnaire Manual Application on Shiny Server
#'
#' @description A wrappter function to start the application on the SERVER. Please make sure you have read the
#' documentation.
#'
#' @details
#' This function only works on shiny server to do so, create a script in the app directory and
#' run this function. An example myListingApp.R file could be:
#'
#' ```
#' susoquestionnairemanual::runQuestManualApp()
#'
#' ```
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

#' @export

runQuestManualAppServer<-function(){
  shiny::addResourcePath("www", system.file("www", package = "susoquestionnairemanual"))
  shiny::addResourcePath("rmdfiles", system.file("rmdfiles", package = "susoquestionnairemanual"))

  shiny::shinyApp(ui = susoquestionnairemanual:::main_ui, server = susoquestionnairemanual:::main_server)
}
