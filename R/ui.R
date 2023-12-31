#` Shiny UI
#'
#'
#'
#' @keywords internal
#' @noRd


main_ui<-function(request){
  styleActButtonActivate<-c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")

  styleActButtonActivate<-c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")
  styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")

  material_page(nav_bar_color="blue darken-4",
                background_color = "white",
                title = "Survey Solutions Questionnaire Manual",
                tags$br(),
                shinyjs::useShinyjs(),
                waiter::use_waiter(),
                # shiny alert conditional on version
                if (utils::packageVersion("shinyalert") < 3) shinyalert::useShinyalert(),
                startupModalUI("startupModal"),
                ##  STYLES
                tags$style(
                  ".btn-file {
                    color: #FFFFFF;
                    background-color: #0d47a1;
                    width: 120%;
                    border-color: #0d47a1;
                    margin:0% 0% 0% -10%;
                  }
                  .btn-file:hover {
                    background: #0d47a1;
                  }
                  #slide-out.sidenav {
                    width: 400px;
                  }
                  .shiny-notification {
                     width: 250px !important;
                     position:fixed;
                     top: 65px;
                     right: 10px;
                     color: #FFFFFF;
                     background-color: #0d47a1;
                  }

                  .shiny-notification-content-text {
                    overflow-wrap: break-word !important;
                  }

                  /*change color and opacity for warning */
                  .shiny-notification-warning {
                         background-color:#ff0f0f;
                         color: #FFFFFF;
                         opacity: 0.7;
                  }

                  .tippy-tooltip.suso-theme {
                    background-color: #0d47a1;
                    font-weight: bold;
                    color: #fff;
                  }
                  .tippy-tooltip.material-theme {
                    background-color: #0d47a1;
                    font-weight: bold;
                    color: #fff;
                  }
                  .tippy-tooltip.material-red-theme {
                    color: #fff;
                    background-color: #a10d47;
                    font-weight: bold;
                    border-color: #2e6da4;
                  }
                  #topText .card {
                    height: 50vh;
                  }
                  textarea {
                    width: 45vw;
                    height: 20vh;
                    margin: 0;
                    padding: 0;
                    column-fill: balance;
                  }"
                ),
                ##  TOOL TIPS
                tippy_this("framediv",
                           "Upload the translation file from the Survey Solutions Designer.
                           The file must be in CSV format, not EXCEL!",
                           placement = "top-end",
                           arrow = "true",
                           offset = 20,
                           size = "small",
                           showOnInit = "true",
                           theme = "material"),
                tippy_this("loadQuestionnaire",
                           "Load the selected questionnaire for the manual!",
                           placement = "top-end",
                           arrow = "true",
                           offset = 20,
                           size = "small",
                           showOnInit = "true",
                           theme = "material"),
                tippy_this("outputFormat",
                           "Select the desired format before uploading",
                           placement = "top-end",
                           arrow = "true",
                           offset = 20,
                           size = "small",
                           showOnInit = "true",
                           theme = "material"),
                material_row(material_column(width=10),

                             ###############################################################################
                             ## ADMIN SETTINGS #############################################################
                             material_column(width=1,
                                             shiny::div(id = "serversettingsdiv",
                                                        material_modal(
                                                          modal_id = "serverSettings",
                                                          button_text = "Server Settings",
                                                          button_color = "blue darken-1",
                                                          button_icon = "perm_data_setting",
                                                          title = "Admin",
                                                          material_row(
                                                            material_column(
                                                              width = 6,
                                                              textInput("suso.server", "Provide SuSo Server",
                                                                        placeholder = "Survey Solutions Server"),br(),br()),

                                                            material_column(
                                                              width = 6,
                                                              textInput("suso.user", "Provide SuSo API user",
                                                                        placeholder = "API User"),br(),br())
                                                          ),
                                                          material_row(
                                                            material_column(
                                                              width = 6,
                                                              passwordInput("suso.pass", "Provide SuSo password",
                                                                            placeholder = "API Password"),br(),br()),
                                                            material_column(
                                                              width = 6,
                                                              textInput("suso.workspace", "Provide SuSo workspace",
                                                                            value = "primary"),br(),br(),
                                                              )
                                                          ),
                                                          material_row(
                                                            material_column(
                                                              width = 6,
                                                              material_button("suso.save",
                                                                              "Save Admin Settings",
                                                                              color = "blue darken-4",
                                                                              icon = "save")),

                                                            material_column(
                                                              width = 6)
                                                          )
                                                        ))),
                             material_column(width = 1,
                                             shinyjs::disabled(
                                               shiny::div(id = "serversettingserasediv",
                                                          actionButton("suso.erase",
                                                                       "Erase Settings",
                                                                       style = "background-color:#B71C1C;",
                                                                       icon = icon("save"))
                                               )))),
                ###############################################################################
                material_side_nav(
                  fixed = FALSE,
                  image_source = "www/suso_wb.png",
                  br(),br(),
                  material_side_nav_tabs(
                    side_nav_tabs = c(
                      "Edit Content" = "edit",
                      "Review Content" = "review"
                    ),
                    icons = c("insert_chart", "explore")
                  ),

                  material_row(
                    # material_column (
                    #   width = 12,
                    #   material_radio_button(input_id = "loadQuest",
                    #                         label = "How is the questionnaire provided?",
                    #                         choices = c("Import from Server"=1,
                    #                                     "Upload zip file"=2),
                    #                         color = "#0d47a1")
                    # )
                  ),
                  ######################################################################
                  conditionalPanel("true", #input.loadQuest==1
                                   material_row(
                                     material_column (
                                       width = 12,
                                       material_dropdown(input_id = "susoQuestionnaire",
                                                         label = "Select Questionnaire",
                                                         choices = c("NONE LOADED"), multiple = F,
                                                         selected = "NONE LOADED",
                                                         color = "#0d47a1")
                                     )
                                   ),
                                   material_row(
                                     material_column(width = 1),
                                     material_column(
                                       width = 10,
                                       shiny::actionButton("loadQuestionnaire",
                                                           "Load Questionnaire",
                                                           icon = icon("download"),
                                                           style="color: #FFFFFF; background-color: #0d47a1; width: 120%;
                                                              border-color: #0d47a1; margin:0% 0% 0% -10%;"
                                       )),
                                     material_column(
                                       width = 1)
                                   )
                  ),
                  conditionalPanel("true", #input.loadQuest==2
                                   material_row(
                                     material_column (
                                       width = 2),
                                     material_column (
                                       width = 8,
                                       div(id = "framediv",
                                           zipFileInput(id = "translation",label = "Upload Translation (.csv!)",
                                                        accept = (c("text/csv", ".csv")))
                                       )
                                     ),
                                     material_column (
                                       width = 2)
                                   )
                  ),
                  material_row(
                    material_column(width = 1),
                    material_column(
                      width = 10,
                      radioButtons("outputFormat",
                                   "Output Format",
                                   choices = c("Word", "HTML", "PPT"),
                                   inline = TRUE)
                    ),
                    material_column(
                      width = 1)
                  ),
                  div(id = "manualdiv",
                  conditionalPanel("input.outputFormat=='HTML'",
                                   material_row(
                                     material_column(width = 1),
                                     material_column(
                                       width = 10,
                                       downloadButton("manual", "Generate HTML Manual",
                                                      style="color: #FFFFFF; background-color: #0d47a1; width: 120%;
                                                              border-color: #0d47a1; margin:0% 0% 0% -10%;"),
                                       tippy_this("manualdiv",
                                                  "Generates and downloads the HTML manual",
                                                  placement = "top-end",
                                                  arrow = "true",
                                                  offset = 20,
                                                  size = "small",
                                                  showOnInit = "true",
                                                  theme = "material")
                                     ),
                                     material_column(
                                       width = 1)
                                   )
                  )),
                  conditionalPanel("input.outputFormat=='Word'",
                                   material_row(
                                     material_column(width = 1),
                                     material_column(
                                       width = 10,
                                       div(id = "manual_doc",
                                           dwl_reportUI("wordManual", " Generate DOC Manual")
                                       ),
                                       tippy_this("manual_doc",
                                                  "Generates and downloads the MS WORD manual",
                                                  placement = "top-end",
                                                  arrow = "true",
                                                  offset = 20,
                                                  size = "small",
                                                  showOnInit = "true",
                                                  theme = "material")
                                     ),
                                     material_column(
                                       width = 1)
                                   )
                  ),
                  div(id = "manualpptdiv",
                  conditionalPanel("input.outputFormat=='PPT'",
                                   material_row(
                                     material_column(width = 1),
                                     material_column(
                                       width = 10,
                                       dwl_reportUI("pptManual", " Generate PPT Manual"),
                                       tippy_this("manualpptdiv",
                                                  "Generates and downloads the MS Powerpoint manual",
                                                  placement = "top-end",
                                                  arrow = "true",
                                                  offset = 20,
                                                  size = "small",
                                                  showOnInit = "true",
                                                  theme = "material")
                                     ),
                                     material_column(
                                       width = 1)
                                   )
                  )),
                  material_row(
                    material_column(width = 1),
                    material_column(
                      width = 10,
                      actionButton("loadProg", "Load Manual",
                                   style="width: 100%; color: #fff; background-color: #0d47a1; border-color: #2e6da4",
                                   icon = icon("file-upload")),
                      tippy_this("loadProg",
                                 "If you have stored your work from previous sessions, you will see it in the table bellow. Just select the
                                 desired questionnaire, and click this button. Make sure, you have loaded the original questionnaire first.",
                                 placement = "top-end",
                                 arrow = "true",
                                 offset = 20,
                                 size = "small",
                                 showOnInit = "true",
                                 theme = "material")

                    ),
                    material_column(
                      width = 1)
                  ),
                  material_row(
                    material_column(width = 1),
                    material_column(
                      width = 10,
                      DT::dataTableOutput("tpkDirTable"),
                    ),
                    material_column(
                      width = 1)
                  ),
                  material_row(
                    material_column(width = 1),
                    material_column(
                      width = 10,
                      actionButton("saveProg", "Save Manual",
                                   style="width: 100%; color: #fff; background-color: #a10d47; border-color: #2e6da4",
                                   icon = icon("save")),
                      tippy_this("saveProg",
                                 "To save your work progress, click here.",
                                 placement = "top-end",
                                 arrow = "true",
                                 offset = 20,
                                 size = "small",
                                 showOnInit = "true",
                                 theme = "material-red")
                    ),
                    material_column(
                      width = 1)
                  )
                ),
                material_side_nav_tab_content(
                  side_nav_tab_id = "edit",
                  useShinyjs(),
                  material_row(
                    material_column(
                      width = 12,
                      material_card(
                        depth = 4,
                        material_row(
                          material_column(
                            width = 6,
                            div(id = "topText",
                                material_card(
                                  depth = 4,
                                  htmlOutput("Title", inline = T),
                                  br(),br(),
                                  htmlOutput("Section", inline = T),
                                  br(),
                                  htmlOutput("Item", inline = T)
                                )
                            )
                          ),
                          material_column(
                            width = 6,
                            div(id = "topText",
                                material_card(
                                  depth = 4,
                                  htmlOutput("displayTitle", inline = T),
                                  br(),
                                  DT::dataTableOutput("Validations"),
                                  br(),br(),br()
                                )
                            )
                          )
                        ),
                        br(),br(),
                        material_row(
                          material_column(width = 6,
                                          textAreaInput("qtext", "Please Provide the Instructions for this Question",
                                                        placeholder = "Instructions", resize = "both"),
                                          actionButton("PREV", "Previous Item",
                                                       style="color: #fff; background-color: #0d47a1; border-color: #2e6da4",
                                                       icon = icon("arrow-left"))
                          ),
                          material_column(width = 6,
                                          textAreaInput("qExamp", "Please Provide an Example for this Question",
                                                        placeholder = "Example", resize = "both"),
                                          actionButton("NEXT", "Next Item",
                                                       style="color: #fff; background-color: #0d47a1; border-color: #2e6da4",
                                                       icon = icon("arrow-right"))
                          )
                        )
                      )
                    )
                  )
                ),
                material_side_nav_tab_content(
                  side_nav_tab_id = "review",
                  material_row(
                    #material_column(2),
                    #material_column(
                    # width = 10,
                    div(id="review", style = "margin: auto; width: 95%;",
                        tags$h3("Review"),
                        DT::dataTableOutput("REVIEW", width = "95vw")
                    )
                  )
                )

                ################################## FIN #################################
  )

}




















