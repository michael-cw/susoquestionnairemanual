#` Shiny module to generate word doc for download
#'
#'
#' Generates WORD reports, for the following LIST of input elements
#'    - doc_titel = character(1)
#'    - sec_title = character(number of sections)
#'    - sec_para = list(para1 =
#'                      para2 = ....
#'                      number of para)
#'    - sec_table = list(para1 = ...)
#'    - sec_figure = list(para1 = ...)
#'
#'
#' @keywords internal
#' @noRd


# UI
dwl_reportUI<-function(id, btnlabel = "Generate Report") {
  styleActButtonActivate<-c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")

  styleActButtonActivate<-c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")
  styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")

  invisibleButton<-c("color: #FFFFFF; background-color: #FFFFFF;
                  border-color: #FFFFFF; margin:0% 0% 0% 0%;height:2px;")
  ns <- NS(id)
  #############################
  ## output format is dynamic
  ##    - leafletouput for small
  ##    - mapdeck for large
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      #add_busy_bar(color = "#68cbf8", timeout = 60000, height = "10px"),
      column(10,
             actionButton(ns("generateReportInt"),
                          btnlabel,
                          icon("search"), width = "100%",
                          style=styleActButtonActivate)
      ),
      column(1)
    ),
    fluidRow(
      ## Button is INVISIBLE, activated by shinyjs::click
      downloadButton(ns("dwl_report"), "Not visible", style=invisibleButton)
    )
  )
  ####################FIN UI####################################################
}
# SERVER
dwl_reportSRV<-function(id, content = NULL, fn = "QuestionnaireManual", wordstyles = "inst/rmdfiles/FINAL_report_for_download.docx") {
  moduleServer(
    id,
    function(input, output, session) {
      ## Initate Report Creation
      observeEvent(input$generateReportInt, {

        rep_cont<-content()
        req(rep_cont)

        #####################
        ## 1. Get Content
        ### 2.1 Generate File
        shiny::withProgress(message = 'Preparing Word Document', value = 0,{
          doc.full<-read_docx(wordstyles) %>%
            set_doc_properties(title = "Survey Solutions Questionnaire Manual",
                               creator = "Questionnaire Manual Application v1.0.0",
                               created = Sys.time())




          ## 2.2 DOC TITLE & DATE
          doc.full <- doc.full  %>%
            body_add(rep_cont$doc_title) %>%
            body_add_break()
          ## 2.3. Add Section Para and Tables
          ##    i. loop over section
          ##        ii. loop over para
          incProgress(0.2)
          for (sec_para in names(rep_cont$sec_para)) {
            #incProgress(0.2)
            p<-rep_cont$sec_para[[sec_para]]
            t<-rep_cont$sec_table[[sec_para]]
            i<-rep_cont$sec_graph[[sec_para]]
            sectitle<-rep_cont$sec_title[[sec_para]]
            doc.full <- doc.full  %>%
              body_add_par(sectitle, style = "heading 2") %>%
              body_add_par(NULL, style = "Normal")
            for (para in names(p)) {
              pp<-p[[para]]
              tt<-t[[para]]
              ii<-i[[para]]
              ## add para
              if (!is.null(pp)) {
                doc.full <- doc.full  %>%
                  body_add(pp, style = "Normal")
              }
              ## add table
              if (!is.null(tt)) {
                doc.full<-doc.full %>%
                  body_add_table(tt,
                                 style = "Grid Table 6 Colorful Accent 2",
                                 header = T) %>%
                  body_add_par(NULL, style = "Normal") %>%
                  body_add_par(NULL, style = "Normal")%>%
                  body_add_break()

              }
              ## add graph
              if (!is.null(ii)){
                doc.full<-doc.full %>%
                  body_add_gg(ii, style = "Figure") %>%
                  body_add_par(NULL, style = "Normal") %>%
                  body_add_par(NULL, style = "Normal") %>%
                  body_add_break()
              }

            }
            ## page break after each section
            doc.full<-doc.full %>%
              body_add_break()
          }
          ## 2.4. Tempfile
          ##############################
          ## 2. Switch working directory for report
          wdOld<-getwd()
          setwd(tempdir())
          on.exit(setwd(wdOld))
          doc.full %>%
            print(target = "report_for_download_v1.docx")
        })
        ## 2.5. Click DWL button
        shinyjs::click("dwl_report")
      })
      ## Download Report
      output$dwl_report <- downloadHandler(
        filename = function() {
          paste(fn, "-", stringr::str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".docx", sep="")
        },
        content = function(file) {
          wdOld<-getwd()
          setwd(tempdir())
          on.exit(setwd(wdOld))
          file.copy("report_for_download_v1.docx", file)
        }, contentType = NULL)

      ####################FIN SERVER####################################################
    }
  )
}
