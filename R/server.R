#` Shiny server
#'
#'
#'
#' @keywords internal
#' @noRd



###########
main_server<-function(input, output, session) {

  ## START UP MODAL FOR USER
  startupkeyusr <- startupModalSRV("startupModal",
                                   useronly = T,
                                   welcometitle = "Welcome to the Survey Solutions Questionnaire Manual Application!",
                                   welcomemessage = HTML("Please make sure you have read the relevant
                                              <a href='https://michael-cw.github.io/susoquestionnairemanual/index.html' target='_blank'>documentation</a>.
                                              To continue please provide your username, so your personal working directory can be created.")
                                   )
  # file path
  fp<-reactiveVal(NULL)
  # filepath for admin
  fpadmDIR<-reactiveVal(NULL)
  # for manuals
  fpDIR<-reactiveVal(NULL)
  observeEvent(startupkeyusr$user(), {
    usr <- req(startupkeyusr$user())
    # user appdir
    appdir <- file.path(tools::R_user_dir("susoquestionnairemanual", which = "data"))
    appdir <- file.path(appdir, paste0(usr))

    if (!dir.exists(appdir)) {
      dir.create(appdir, recursive = TRUE, showWarnings = FALSE)
    }
    fp(appdir)

    # GADM file dir
    appdir_sub <- file.path(appdir, "admin")
    if (!dir.exists(appdir_sub)) {
      dir.create(appdir_sub, recursive = TRUE, showWarnings = FALSE)
    }
    fpadmDIR(appdir_sub)

    # Paradata file dir
    appdir_sub <- file.path(appdir, "saved_manuals")
    if (!dir.exists(appdir_sub)) {
      dir.create(appdir_sub, recursive = TRUE, showWarnings = FALSE)
    }
    fpDIR(appdir_sub)

    notmessage <- HTML(
      sprintf(
        "Your files for this session will be stored in you personal user directory under, <b>%s/%s</b>.",
        appdir, usr
      ) %>%
        stringr::str_remove_all("\\n") %>%
        stringr::str_squish()
    )

    showNotification(
      ui = notmessage,
      #duration = NULL,
      id = "userinfostart",
      type = "message",
      session = session,
      closeButton = T
    )
  })
  ######################################################################################
  ##                SERVER ADMIN SETTINGS
  ######################################################################################
  ADMIN<-reactiveValues()
  # filepath for admin
  fpadm<-reactiveVal(NULL)
  # # filepath creation
  observe({
    # create admin file
    appdir<-req(fpadmDIR())
    admfile<-file.path(appdir, "admin_settings.rds")
    fpadm(admfile)
  })
  # Check if server settings are stored & work
  fields<-reactiveVal(c("suso.server", "suso.user", "suso.pass", "suso.workspace"))
  observe({
    admfile<-req(fpadm())
    if(file.exists(admfile)){
      tmp.admin<-readRDS(paste0(admfile))
    }  else {
      tmp.admin<-character(0)
    }

    if (length(tmp.admin)==length(fields())){
      admin.vars<-tmp.admin
      suso_set_key(admin.vars[["suso.server"]], admin.vars[["suso.user"]], admin.vars[["suso.pass"]])
      # check credentials and set to TBD if wrong
      waiter::waiter_show(
        color = "rgba(13, 71, 161, 0.7)",
        html = tagList(
          spin_fading_circles(),
          "Checking Credentials ..."
        )
      )
      credcheck<-suso_PwCheck(workspace = admin.vars[["suso.workspace"]])$status_code[1]
      waiter::waiter_hide()
      if (credcheck==200) {
        ADMIN$settings<-admin.vars
        shinyjs::disable("serversettingsdiv")
        shinyjs::enable("serversettingserasediv")
      } else {
        admin.vars<-rep("TBD", length(admin.vars))
        names(admin.vars)<-fields()
        shiny::showNotification("Wrong Credentials")
        ADMIN$settings<-NULL

        req(FALSE)
      }

    } else {
      admin.vars<-c(rep("TBD", length(fields())))
      names(admin.vars)<-fields()
      shiny::showNotification("Some credentials are missing. Please reset!")
      ADMIN$settings<-NULL
      req(FALSE)
    }


  }, priority = 0)

  ##  ii) write to file in data/admin/admin_settings.rds

  observeEvent(input$suso.save, {
    #admin.vars.new<-ADMIN$settings
    admfile<-req(fpadm())
    admin.vars <- sapply(fields(), function(x) input[[x]])
    names(admin.vars)<-fields()

    if(file.exists(admfile)){
      try(
        {file.remove(admfile)},
        silent = T
      )
    }
    suso_clear_keys()
    suso_set_key(admin.vars[["suso.server"]], admin.vars[["suso.user"]], admin.vars[["suso.pass"]])
    ##  2. Check settings again
    waiter::waiter_show(
      color = "rgba(13, 71, 161, 0.7)",
      html = tagList(
        spin_fading_circles(),
        "Checking Credentials ..."
      )
    )
    credcheck<-suso_PwCheck(workspace = admin.vars[["suso.workspace"]])$status_code[1]
    waiter::waiter_hide()

    if (credcheck==200) {
      saveRDS(admin.vars, admfile)
      shinyjs::disable("serversettingsdiv")
      shinyjs::enable("serversettingserasediv")
      ADMIN$settings<-admin.vars

    } else {
      admin.vars<-rep("TBD", length(admin.vars))
      names(admin.vars)<-fields()
      shiny::showNotification("Wrong Credentials")
      ADMIN$settings<-NULL
      req(FALSE)
    }
    ##  2. Hand Over Settings

  }, priority = -2, ignoreInit = T)

  ##  iv) erase settings
  observeEvent(input$suso.erase, {
    admfile<-req(fpadm())
    try(
      {file.remove(admfile)},
      silent = T
    )
    shinyjs::enable("serversettingsdiv")
    shinyjs::disable("serversettingserasediv")
    ADMIN$settings<-NULL

    # update selection
    update_material_dropdown(session = session,
                             input_id = "susoQuestionnaire",
                             choices = c("NONE LOADED"),
                             value = "NONE LOADED")
  }, ignoreInit = T)

  ##  3. Load the questionnaires for selection
  questionnaires<-reactiveValues()
  observe({
    settings<-req(ADMIN$settings)
    if (SurveySolutionsAPI::suso_PwCheck(
      settings[["suso.server"]], settings[["suso.user"]], settings[["suso.pass"]], settings[["suso.workspace"]]
    )$status_code[1]!=200) {

      settings<-rep("TBD", length(fields()))
      names(settings)<-fields()
      req(FALSE)
    }
    ##  if settings check is ok, questionnaires are loaded.
    if (sum(grepl(x=settings,pattern = "TBD"))==0){
      apicall<-SurveySolutionsAPI::suso_getQuestDetails(workspace = settings[["suso.workspace"]])
      # if no data, stop and show message
      if(length(apicall)==1 | is.null(apicall)) {
        showNotification("No questionnaire loaded on the provided server/workspace. Please import questionnaires first!", type = "error")
        req(FALSE)
      } else {
        tab<-data.table(apicall,
                        key = c("Title", "Version"))
      }



      tab[,c("date", "time"):=tstrsplit(LastEntryDate, "T", fixed=TRUE)][]
      tab[,time:=as.ITime(time)]
      tab[,date:=as.IDate(date)]
      tab[,QuestionnaireIdVersion:=sprintf("%s_%d", QuestionnaireId, Version)]
      setorderv(tab, c("date", "time"), -1)
      dropDown<-sprintf("(ver. %d) %s", tab$Version, tab$Title)

      dropDown<-setNames(object = tab$QuestionnaireIdVersion, dropDown)

      update_material_dropdown(session = session,
                               input_id = "susoQuestionnaire",
                               choices = dropDown,
                               value = dropDown[1])
      questionnaires$tab<-tab}
  })


  ##  4. Load the individual Questionnaire
  ##  4.1 From SERVER
  observeEvent(input$loadQuestionnaire, {
    if(input$susoQuestionnaire!="NONE LOADED") {
      waiter::waiter_show(
        color = "rgba(13, 71, 161, 0.7)",
        html = tagList(
          spin_fading_circles(),
          "Loading Questionnaire ..."
        )
      )

      qid<-str_split(input$susoQuestionnaire, "_", simplify = T)[1,1]
      v<-str_split(input$susoQuestionnaire, "_", simplify = T)[1,2]
      qestStruct<-SurveySolutionsAPI::suso_getQuestDetails(workspace = input$suso.workspace,
                                                           quid = qid,
                                                           version = v,
                                                           operation.type = "structure")

      questionnaires$qestStruct<-qestStruct$q
      questionnaires$validations<-qestStruct$val
      questionnaires$Title<-questionnaires$tab[QuestionnaireId==qid & Version==v, Title]
      rv$qCOUNTER<-1

      waiter::waiter_hide()
    }
  }, ignoreInit = T)

  ##  4.2 From FILE
  observeEvent(input$susoUpLoadQuestionnaire, {
    uploadFile<-input$susoUpLoadQuestionnaire
    # a) load file
    shiny::validate(need(uploadFile, message = F))
    file.list<-unzip(uploadFile$datapath, list=T)
    thefiles<-tempdir()
    fp<-file.path(thefiles, file.list$Name[1])
    req(file_ext(fp)=="json")
    unzip(uploadFile$datapath, files=file.list$Name, exdir = thefiles)

    # b) process file function, input is path (fp)
    test_json<-fromJSON(fp[1], flatten = T)
    qestStruct<-test_json
    questionnaires$qestStruct<-qestStruct
    questionnaires$Title<-qestStruct$Title$Title
    rv$qCOUNTER<-1

  }, ignoreInit = T)



  ##  4.1. Hand over Questionnaire & Change Variable Names
  observe({
    allQuestions<-questionnaires$qestStruct
    shiny::validate(need(allQuestions, message = F))
    allQuestions<-allQuestions[!is.na(QuestionText)]
    ## Drop AUDIO
    sel<-stringr::str_starts(allQuestions$VariableName, "aud_", negate = T)
    allQuestions<-allQuestions[sel]
    setorderv(allQuestions, c("L0", "L1"))
    #allQuestions$QuestionText<-ifelse(is.na(allQuestions$QuestionText), allQuestions$Text, allQuestions$QuestionText)
    setnames(allQuestions, "L0", "Section")
    allQuestions[, PublicKey:=stringr::str_remove_all(PublicKey, '-')]
    questionnaires$allQuestions<-allQuestions
  })

  ##  4.4. SAVE/Load Existing Manual
  # 1. Settings
  # fpDIR<-reactiveVal(NULL)
  # observe({
  #   appdir<-file.path(tools::R_user_dir("susoquestionnairemanual", which = "data"), "saved_manuals")
  #   if(!dir.exists(appdir)){
  #     dir.create(appdir, recursive = TRUE, showWarnings = FALSE)
  #   }
  #
  #   fpDIR(appdir)
  # })
  smTabDir<-list(dom="t", pagelength=500, scrollY="250px", scrollcollapse=TRUE, paging=FALSE)
  # 2. Save File
  observeEvent(input$saveProg, {
    req(fpDIR())
    tab<-questionnaires$FINAL
    shiny::validate(need(tab, message = F))
    fn<-paste0(questionnaires$Title, ".csv")
    # only key and text is saved, original questionnaire must be loaded first
    tab<-tab[,.(PublicKey, Instruction, Example)]
    fn<-file.path(fpDIR(), fn)
    loadMan<-readr::write_csv(tab, file = fn)

    # read dir after save
    lf<-list.files(fpDIR(), pattern = ".csv$")
    if(length(lf)>0) flDIR(lf)

  }, ignoreInit = T)
  # 3. Read Directory

  # 4. DT Directory
  flDIR<-reactiveVal(NULL)
  observe({
    req(fpDIR())
    lf<-list.files(fpDIR(), pattern = ".csv$")

    if(length(lf)>0) flDIR(lf)

  })
  loadSelected<-reactiveVal(NULL)
  output$tpkDirTable<-DT::renderDataTable({
    shiny::req(fpDIR())
    shiny::validate(need(!is.null(flDIR()), message = "No Manuals Available!"))
    tab<-data.table(Files=flDIR())
    loadSelected(tab)
    DT::datatable(tab, smTabDir ,selection = "single",  rownames = F,
                  style = "bootstrap")

  })
  #. 5. Load selected MANUAL
  observeEvent(input$loadProg, {
    ls<-req(loadSelected())
    shiny::validate(need(input$tpkDirTable_rows_selected, message = F))

    ls<-ls[input$tpkDirTable_rows_selected, "Files"]
    questionnaires$Title<-strsplit(ls$Files, ".csv")[[1]][1]
    fn<-file.path(fpDIR(), ls)
    loadMan<-readr::read_csv(file = fn, locale = locale("ro"),
                             col_types = cols_only(
                               PublicKey = col_character(),
                               Instruction = col_character(),
                               Example = col_character()
                             )
    )
    loadMan<-data.table(loadMan)
    loadMan[, PublicKey:=stringr::str_remove_all(PublicKey, '-')]
    loadMan[, Instruction:=stringr::str_remove_all(Instruction, '"')]
    questionnaires$allQuestionsLoaded<-copy(loadMan)
    rv$qCOUNTER<-1
  }, ignoreInit = T)
  #####################################################################################
  ##      TRANSLATION
  #####################################################################################
  translationinput<-callModule(zipFile, "translation")


  ######################################################################################
  ##                INPUTS
  ######################################################################################
  ##::validate() Questionnaire Title
  output$Title<-renderUI({
    Title<-questionnaires$Title
    shiny::validate(need(Title, message = "Load Questionnaire First!"))
    HTML(paste0("<h4><font color = '#0d47a1'>", Title, "</font></h4>"))
  })

  ##  2. Questionnaire Item
  ##  2.1. SET counter
  rv <- reactiveValues(qCOUNTER=1)

  observe({
    toggleState(id = "PREV", condition = rv$qCOUNTER > 1)
  })

  navPage <- function(direction) {
    rv$qCOUNTER <- rv$qCOUNTER + direction
  }

  observeEvent(translationinput(), {
    ## Add translation
    transl<-translationinput()
    shiny::validate(need(transl, message = F))
    CHECKtransl<-copy(transl)

    if(!is.null(transl)) {
      ## Adjust translation
      allQuestions<-questionnaires$allQuestions
      shiny::validate(need(allQuestions, message = F))
      setnames(transl, "Entity Id", "PublicKey")
      transl[, PublicKey:=stringr::str_remove_all(PublicKey, '-')]
      setnames(transl, "Translation", "QuestionTextTrans")
      setnames(transl, "Variable", "VariableName")
      transl[, QuestionTextTrans:=stringr::str_remove_all(QuestionTextTrans, '"')]
      transl[,QuestionTextTrans:=str_replace_all(QuestionTextTrans, "[^[:print:]]", "")]
      ## keep only Title, Instruction, ValidationMessage
      translQuest<-transl[Type %in% c("Title")]
      translQuest<-translQuest[,.(PublicKey, QuestionTextTrans, VariableName)]
      ## Adjust main file
      #toSelect<-names(allQuestions)
      #toSelect<-toSelect[!(toSelect%in%c("QuestionText"))]
      #allQuestions<-allQuestions[,toSelect, with=F]
      allQuestions<-merge(allQuestions, translQuest, by = c("PublicKey", "VariableName"), all.x = T)
      allQuestions[,QuestionText:=ifelse(is.na(QuestionTextTrans), QuestionText, QuestionTextTrans)]
      questionnaires$allQuestions<-allQuestions
    }
  }, ignoreInit = F)
  observeEvent(questionnaires$allQuestionsLoaded, {
    ## Add existing manual
    if(!is.null(questionnaires$allQuestionsLoaded)) {
      allQuestions<-questionnaires$allQuestions
      shiny::validate(need(allQuestions, message = F))
      toSelect<-names(allQuestions)
      toSelect<-toSelect[!(toSelect%in%c("Example", "Instruction"))]
      allQuestions<-allQuestions[,toSelect, with=F]
      loadedQuestion<-questionnaires$allQuestionsLoaded
      allQuestions<-merge(allQuestions, loadedQuestion, by = "PublicKey")
      questionnaires$allQuestions<-allQuestions
    }
  }, ignoreInit = F)

  ##  2.2. NEXT
  observeEvent(input$NEXT, {
    allQuestions<-questionnaires$allQuestions
    shiny::validate(need(allQuestions, message = F))

    setorderv(allQuestions, c("Section", "L1"))
    CHECKallQuestions<-allQuestions
    ## Pass on the item to render text
    item<-allQuestions[rv$qCOUNTER]
    item[,qCOUNTER:=rv$qCOUNTER]
    questionnaires$ITEM_SEL<-item
    ############################
    ##  Store the text
    if (rv$qCOUNTER>0 & rv$qCOUNTER<=nrow(allQuestions)) {
      ## for regular items line 1+
      itemIndex<-rv$qCOUNTER-1
      updateQtext<-allQuestions[itemIndex, c("Instruction", "Example"):=list(input$qtext, input$qExamp)]
      questionnaires$FINAL<-updateQtext
      ########################
      ## reset TEXTBOX
      updateTextAreaInput (
        session = session,
        inputId = "qtext",
        value = updateQtext[itemIndex+1, Instruction]
      )
      updateTextAreaInput (
        session = session,
        inputId = "qExamp",
        value = updateQtext[itemIndex+1, Example]
      )
    } else if (rv$qCOUNTER>nrow(allQuestions)) {
      ## for the last item
      itemIndex<-rv$qCOUNTER-1
      updateQtext<-allQuestions[itemIndex, c("Instruction", "Example"):=list(input$qtext, input$qExamp)]
      questionnaires$FINAL<-updateQtext

      ########################
      ## reset TEXTBOX
      updateTextAreaInput (
        session = session,
        inputId = "qtext",
        value = "YOU HAVE PROCESSED ALL QUESTIONS, PLEASE GENERATE THE MANUAL"
      )
      updateTextAreaInput (
        session = session,
        inputId = "qExamp",
        value = "YOU HAVE PROCESSED ALL QUESTIONS, PLEASE GENERATE THE MANUAL"
      )

    }
    navPage(1)
    ## disable NEXT button
    toggleState(id = "NEXT", condition = rv$qCOUNTER<=nrow(allQuestions)+1)
  }, ignoreInit = T)

  ##  2.2. PREV
  observeEvent(input$PREV, {
    allQuestions<-questionnaires$FINAL
    shiny::validate(need(allQuestions, message = F))
    navPage(-1)
    ## Pass on the item to render text
    item<-allQuestions[rv$qCOUNTER-1]
    item[,qCOUNTER:=rv$qCOUNTER-1]
    questionnaires$ITEM_SEL<-item
    ############################
    ##  Store the text
    if (rv$qCOUNTER>0 & rv$qCOUNTER<=nrow(allQuestions)+1) {
      ## for regular items line 1+
      itemIndex<-rv$qCOUNTER-1

      ########################
      ## reset TEXTBOX
      updateTextAreaInput (
        session = session,
        inputId = "qtext",
        value = allQuestions[itemIndex, Instruction]
      )
      ########################
      ## reset TEXTBOX
      updateTextAreaInput (
        session = session,
        inputId = "qExamp",
        value = allQuestions[itemIndex, Example]
      )
    }

    toggleState(id = "NEXT", condition = rv$qCOUNTER<=nrow(allQuestions)+1)
  }, ignoreInit = T)

  ##  3. ITEM
  ##  3.1. Section title
  output$Section<-renderUI({
    item<-questionnaires$ITEM_SEL
    shiny::validate(need(item$Section, message = F))

    if (is.na(item$Section)) {
      section<-HTML(paste0("<br>"))
    } else {
      section<-HTML(paste0("<b>Section: ",item$Section, "</b>"))
    }
    return(section)
  })

  ##  3.2. Question
  output$Item<-renderUI({
    item<-questionnaires$ITEM_SEL
    shiny::validate(need(item, message = "Click NEXT to load first question!"))
    HTML(paste0("<font color='red'>",item$qCOUNTER,". </font>", item$QuestionText ))
  })
  ###############################################
  ## 3.3 Validations
  output$Validations<-DT::renderDataTable({
    check_item<-questionnaires$ITEM_SEL
    shiny::validate(need(check_item$VariableName, message = F))
    allValidations<-questionnaires$validations
    if(!is.null(allValidations)) {
      val<-allValidations[VariableName==check_item$VariableName]
      req(nrow(val)>0)
      val<-val[,.(Expression, Message)]
      val<-datatable(val, options = list(pageLength = 1, dom="tp")) %>%
        # formatStyle('Section', columnWidth = '5%') %>%
        # formatStyle('type', columnWidth = '5%') %>%
        # formatStyle('VariableName', columnWidth = '5%') %>%
        formatStyle('Expression',
                    color = 'red',
                    backgroundColor = 'orange',
                    fontWeight = 'bold') %>%
        formatStyle('Message',
                    color = 'purple')
      return(val)
    }
  })
  ## valtitle
  output$displayTitle<-renderUI({
    Title<-"Validations"
    shiny::validate(need(Title, message = "Load Questionnaire First!"))
    HTML(paste0("<h4><font color = '#0d47a1'>", Title, "</font></h4>"))
  })

  ####################################################
  ##  REVIEW
  output$REVIEW<-DT::renderDataTable({
    tab<-questionnaires$FINAL
    shiny::validate(need(tab, message = "You have not provided any comments yet!"))
    tab<-tab[,.(Section, type, VariableName, QuestionText, Instruction, Example)]
    datatable(tab, options = list(pageLength = 10)) %>%
      # formatStyle('Section', columnWidth = '5%') %>%
      # formatStyle('type', columnWidth = '5%') %>%
      # formatStyle('VariableName', columnWidth = '5%') %>%
      formatStyle('QuestionText',
                  color = 'red',
                  backgroundColor = 'orange',
                  fontWeight = 'bold') %>%
      formatStyle('Instruction',
                  color = 'purple',
                  fontWeight = 'bold')

  }, server = T)

  proxy = dataTableProxy('REVIEW')
  observeEvent(input$NEXT, {
    tab<-questionnaires$FINAL
    shiny::validate(need(tab, message = "You have not provided any comments yet!"))
    tab<-tab[,.(Section, type, VariableName, QuestionText, Instruction, Example)]
    replaceData(proxy, tab)
  })



  # HTML generation & download, uses file report.rmd, plot_child.rmd and plot_child_chil.rmd
  # from directory rmdfiles
  output$manual <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){
      paste0("Questionnaire_Manual_",Sys.Date(),
             ifelse(input$outputFormat=="HTML",".html",".docx")
      )
    },
    content = function(file) {
      tab<-questionnaires$FINAL
      # Check if content is available
      if(is.null(tab)) {
        shiny::showNotification("Please provide comments! If you have just loaded the comments and the questionnaire,
                                make sure you have clicked NEXT at least once", type = "error")
        req(FALSE)
      }

      waiter::waiter_show(
        color = "rgba(13, 71, 161, 0.7)",
        html = tagList(
          spin_fading_circles(),
          "Generating HTML document ..."
        )
      )
      ##  1. Load the template
      fprmd<-system.file("rmdfiles", package = "susoquestionnairemanual")
      ##  1.1 HTML
      if (input$outputFormat=="HTML"){
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(file.path(fprmd, "report.Rmd"), tempReport, overwrite = TRUE)
        tempChild <-file.path(tempdir(), "plot_child.Rmd")
        file.copy(file.path(fprmd, "plot_child.Rmd"), tempChild, overwrite = TRUE)
        tempChildChild <-file.path(tempdir(), "plot_child_child.Rmd")
        file.copy(file.path(fprmd, "plot_child_child.Rmd"), tempChildChild, overwrite = TRUE)
        tempCSS <-file.path(tempdir(), "style_qManual.css")
        file.copy(file.path(fprmd, "style_qManual.css"), tempCSS, overwrite = TRUE)
      }
      ##  2. Load the data
      tab[,Instruction:=str_replace_all(Instruction, "[^[:print:]]", "")]
      tab[,Example:=str_replace_all(Example, "[^[:print:]]", "")]
      title<-questionnaires$Title
      validations<-questionnaires$validations
      # CHECK<<-list(tab, title, validations)

      shiny::validate(need(tab, message = "You have not provided any comments yet!"))
      tab<-tab[,.(Section, type, VariableName, QuestionText, Instruction, Example, Featured)]
      # remove line breaks in text-->leave other html tags
      tab[, QuestionText:=stringr::str_remove_all(string = QuestionText, pattern = "(<br>)|(\\n)")]

      # Set up parameters to pass to Rmd document
      params <- list(questionnaireDT = tab, qTitle=title, qValidations = validations)

      # setwd(tempdir())
      withr::with_dir(tempdir(),
                      ## 3. KNIT the documdent
                      rmarkdown::render("report.Rmd", output_file = file, quiet = T,
                                        params = params,
                                        envir = new.env(parent = globalenv())
                      )
      )
      waiter::waiter_hide()
    }
  )
  # generate reactive to acivate dwl
  generate_manual<-reactive({
    list(
      input$`wordManual-generateReportInt`,
      input$`pptManual-generateReportInt`

    )
  })

  # creates content for WORD and PPT
  full_content<-eventReactive(generate_manual(), {
    tab<-questionnaires$FINAL
    # check if data loaded stop w message if not
    if(is.null(tab)) {
      shiny::showNotification("Please provide comments! If you have just loaded the comments and the questionnaire,
                                make sure you have clicked NEXT at least once", type = "error")
      req(FALSE)
    }
    if(input$outputFormat=="Word") {
      ##  2. Load the data
      req(tab)
      qTitle<-questionnaires$Title
      tab[,Instruction:=str_replace_all(Instruction, "[^[:print:]]", "")]
      tab[,Example:=str_replace_all(Example, "[^[:print:]]", "")]

      validations<-questionnaires$validations

      shiny::validate(need(tab, message = "You have not provided any comments yet!"))
      #tab<-tab[,.(Section, type, VariableName, QuestionText, Instruction, Example, Featured)]


      ##############
      ## create the content
      # withProgress(message = 'Report generation in progress',
      #              value = 0,
      #              {
      ######################################################
      ###############################
      ## 2. Transformations
      fpwww<-system.file("www", package = "susoquestionnairemanual")

      full_content<-list()
      ## styles title
      fp_title<-fp_text(color = "#a10d47", font.size = 20, bold = T)
      fp_title_sty<-fp_par(text.align = "center", padding.bottom = 1,
                           border.bottom = fp_border(color = "black"))
      fp_stitle<-fp_text(color = "#a10d47", font.size = 14, bold = F)
      fp_stitle_sty<-fp_par(text.align = "center", padding.bottom = 0)
      ##
      full_content$doc_title<-block_list(
        fpar(ftext(qTitle, prop = fp_title),
             run_linebreak(), run_linebreak(),
             fp_p = fp_par(text.align = "center")),
        fpar(ftext("Questionnaire Manual Application v1.0.0", prop = fp_stitle), fp_p = fp_stitle_sty),
        fpar(ftext(as.character(Sys.Date()), prop = fp_stitle),
             run_linebreak(), run_linebreak(),
             run_linebreak(), run_linebreak(),
             fp_p = fp_stitle_sty),
        fpar(
          external_img(src = file.path(fpwww, "suso_wb.png"), height = 1.06*2, width = 1.39*2),
          fp_p = fp_par(text.align = "center", padding.top = 5)
        )
      )
      ###########################
      ## Sections
      sections<-unique(tab$Section)
      full_content$sec_title<-list()
      for(i in 1:length(sections)){
        full_content$sec_title[[paste0("sec",i)]]<-sprintf("Section %d",sections[i])

      }
      # Remove HTML tags in question text:
      tab[, QuestionText:=stringr::str_remove_all(string = QuestionText, pattern = "<.*?>")]

      ############################
      ## Questions (with fpar)
      ## 1. Styles
      fp_qte<-fp_text(color = "#a10d47", font.size = 14, bold = T)
      fp_qte_sty<-fp_par(text.align = "center", padding.bottom = 2, keep_with_next = T)
      fp_hea<-fp_text(color = "#0d47a1", font.size = 14, bold = T)
      fp_hea_sty<-fp_par(text.align = "center", padding.bottom = 1, keep_with_next = T)
      fp_qtt<-fp_text(color = "#0d47a1", font.size = 12, bold = F)
      fp_qtt_sty<-fp_par(text.align = "justify", padding.bottom = 2)
      full_content$sec_para<-list()
      for(i in 1:length(sections)){
        paras_sub<-subset(tab, Section==sections[i])
        paras<-nrow(paras_sub)
        full_content$sec_para[[paste0("sec",i)]]<-list()
        ## loop over questions in sections
        for(k in 1:(paras)){
          qte<-ftext(paras_sub[k, QuestionText], fp_qte)
          qin<-ftext(paras_sub[k, Instruction], fp_qtt)
          qhea2<-ftext("Example", fp_hea)
          qex<-ftext(paras_sub[k, Example], fp_qtt)

          full_content$sec_para[[paste0("sec",i)]][[paste0("para",k)]]<-
            block_list(
              ## QUESTION TEXT
              fpar(qte, fp_p = fp_qte_sty,
                   run_linebreak()),
              ## INSTRUCTIONS
              fpar(qin, fp_p = fp_qtt_sty,
                   run_linebreak(), run_linebreak()),
              ## HEADER EXAMPLE
              fpar(qhea2, fp_p = fp_hea_sty,
                   run_linebreak()),
              ## EXAMPLE
              fpar(qex, fp_p = fp_qtt_sty,
                   run_linebreak(),
                   run_linebreak(),
                   run_linebreak())
            );
        }
      }
      ######################################################
      #})

    } else if(input$outputFormat=="PPT") {
      ##  2. Load the data
      req(tab)

      # Title
      qTitle<-questionnaires$Title
      tab[,Instruction:=str_replace_all(Instruction, "[^[:print:]]", "")]
      tab[,Example:=str_replace_all(Example, "[^[:print:]]", "")]

      validations<-questionnaires$validations

      shiny::validate(need(tab, message = "You have not provided any comments yet!"))


      ##############
      ## create the content
      # withProgress(message = 'Report generation in progress',
      #              value = 0,
      #              {
      ######################################################
      ###############################
      ## 2. Transformations
      fpwww<-system.file("www", package = "susoquestionnairemanual")

      full_content<-list()
      ## styles title
      fp_title<-fp_text(color = "#002244", font.size = 30, bold = T)
      fp_title_sty<-fp_par(text.align = "left", padding.bottom = 1,
                           border.bottom = fp_border(color = "black"))
      fp_stitle<-fp_text(color = "#98252B", font.size = 18, bold = F)
      fp_stitle_sty<-fp_par(text.align = "left", padding.bottom = 0)
      ##
      full_content$doc_title<-list(
        fpar(ftext(qTitle, prop = fp_title),
             fp_p = fp_par(text.align = "left")),
        fpar(ftext("Questionnaire Manual Application v1.0.0", prop = fp_stitle), fp_p = fp_stitle_sty),
        fpar(ftext(as.character(Sys.Date()), prop = fp_stitle),
             fp_p = fp_stitle_sty)
        # fpar(
        #   external_img(src = file.path(fpwww, "suso_wb.png"), height = 1.06*2, width = 1.39*2),
        #   fp_p = fp_par(text.align = "center", padding.top = 5)
        # )
      )

      # Remove HTML tags in question text:
      tab[, QuestionText:=stringr::str_remove_all(string = QuestionText, pattern = "<.*?>")]

      ############################
      ## Questions (with fpar)
      ## 1. Styles
      fp_qte<-fp_text(color = "#98252B", font.size = 22, bold = T)
      fp_qte_sty<-fp_par(text.align = "center", padding.bottom = 2, keep_with_next = T)
      fp_hea<-fp_text(color = "#002244", font.size = 16, bold = T)
      fp_hea_sty<-fp_par(text.align = "center", padding.bottom = 1, keep_with_next = T)
      fp_qtt<-fp_text(color = "#002244", font.size = 13, bold = F)
      fp_qtt_sty<-fp_par(text.align = "justify", padding.bottom = 2)
      fp_ftr<-fp_text(color = "#00224480", font.size = 10, bold = F)
      fp_ftr_sty<-fp_par(text.align = "center", padding.bottom = 2)
      fp_fdt_sty<-fp_par(text.align = "right", padding.bottom = 2)
      fp_fslnum_sty<-fp_par(text.align = "right", padding.bottom = 2)

      # get date for footer
      TDATE<-as.character(lubridate::date(Sys.time()))

      ## Sections
      sections<-unique(tab$Section)
      full_content$sec_title<-list()
      for(i in 1:length(sections)){
        full_content$sec_title[[paste0("sec",i)]]<-list(
          title = sprintf("Section %d",sections[i]),
          ftr = fpar(ftext("Survey Solutions Questionnaire Manual", fp_ftr), fp_p = fp_ftr_sty),
          fdt = fpar(ftext(TDATE, fp_ftr), fp_p = fp_fdt_sty)
        )
      }
      # create list for loop
      full_content$sec_para<-list()

      # get slide number
      sldNum<-2
      for(i in 1:length(sections)){
        paras_sub<-subset(tab, Section==sections[i])
        paras<-nrow(paras_sub)
        full_content$sec_para[[paste0("sec",i)]]<-list()
        sldNum<-sldNum+1
        ## loop over questions in sections
        for(k in 1:(paras)){
          qte<-ftext(paras_sub[k, QuestionText], fp_qte)
          qhea1<-ftext("Instructions", fp_hea)
          qin<-ftext(paras_sub[k, Instruction], fp_qtt)
          qhea2<-ftext("Example", fp_hea)
          qex<-ftext(paras_sub[k, Example], fp_qtt)
          qftr<-ftext("Survey Solutions Questionnaire Manual", fp_ftr)
          qdt<-ftext(TDATE, fp_ftr)
          sldNumForm<-ftext(sldNum, fp_ftr)

          full_content$sec_para[[paste0("sec",i)]][[paste0("para",k)]]<-
            list(
              ## QUESTION TEXT
              qte=fpar(qte, fp_p = fp_qte_sty),
              ## HEADER Instructions
              qhea1=fpar(qhea1, fp_p = fp_hea_sty),
              ## INSTRUCTIONS
              quin=fpar(qin, fp_p = fp_qtt_sty),
              ## HEADER EXAMPLE
              qhea2=fpar(qhea2, fp_p = fp_hea_sty),
              ## EXAMPLE
              qex=fpar(qex, fp_p = fp_qtt_sty),
              ## Footer
              qftr=fpar(qftr, fp_p = fp_ftr_sty),
              ## date
              qdt=fpar(qdt, fp_p = fp_fdt_sty),
              ## slide number
              sldNum=fpar(sldNumForm, fp_p = fp_fslnum_sty)
            );
          sldNum<-sldNum+1
        }
      }
      ######################################################
      #})

    }
    return(full_content)
  })
  # generate word doc -> styles are currently under rmdfiles, however in the future upload may be an option.
  dwl_reportSRV("wordManual",
                wordstyles = file.path(system.file("rmdfiles", package = "susoquestionnairemanual"), "FINAL_report_for_download.docx"),
                content = full_content,
                type = "word")
  # generate pptxtyles are currently under rmdfiles, however in the future upload may be an option.
  dwl_reportSRV("pptManual",
                pptxstyles = file.path(system.file("rmdfiles", package = "susoquestionnairemanual"), "wb_dg_suso_modern.pptx"),
                content = full_content,
                type = "pptx")
  ################################################################################################
  ###############################F     i     N####################################################

}
