#' Shiny UI module for upload of text (csv, tab) file
#'
#'
#'
#' @param id Namespace identifier
#' @param label File input label
#' @param accept Mime type (i.e. must be one of)
#' @return file input
#'
#'
#'
#' @export
zipFileInput <- function(id, label, accept) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("frame_file"), label, multiple = F,
              accept = accept, width = "100%")
  )
}

#' Shiny server module for upload of text (csv, tab) file
#'
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param loc local
#'
#'
#' @return returns datatable with upload data by using data.table's fread function
#'
#'
#' @export

zipFile <- function(input, output, session, loc = "ro") {
  userFile <- reactive({
    # If no file is selected, don't do anything
    shiny::validate(need(input$frame_file, message = FALSE))
    input$frame_file
  })

  frameFile<-reactive({
    fp<-userFile()
    fp<-fp$datapath
    tab<-readr::read_csv(file = fp, locale = locale(loc))
    tab<-data.table(tab)
    return(tab)
  })

  return(frameFile)

}
