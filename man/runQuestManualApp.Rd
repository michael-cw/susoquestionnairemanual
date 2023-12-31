% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runapp.R
\name{runQuestManualApp}
\alias{runQuestManualApp}
\title{Start the Survey Solutions Questionnaire Manual Application}
\usage{
runQuestManualApp(launch.browser = TRUE)
}
\arguments{
\item{launch.browser}{If true, the system's default web browser will be
launched automatically after the app is started. Defaults to true in
interactive sessions only. The value of this parameter can also be a
function to call with the application's URL.}
}
\description{
A wrappter function to start the application. Please make sure you have read the
documentation on how to use the app.
}
\details{
This application is part of the large set of tools, to facilitate survey implementation with
\href{https://docs.mysurvey.solutions/}{Survey Solutions}. Enumeration Areas, sampled for example
with the JDC-Survey Solutions Spatial Sampling application, can be used to enumerate all buildings
visible within the boundaries on a google map. The enumerate buildings can then be used for the
second stage sampling frame and to draw the survey units within the cluster from it.
}
\examples{
\dontrun{
# Start app in the current working directory
runApp()

# Start app in a subdirectory called myapp
runApp("myapp")
}

## Only run this example in interactive R sessions
if (interactive()) {
  options(device.ask.default = FALSE)

  # Apps can be run without a server.r and ui.r file
  runApp(list(
    ui = bootstrapPage(
      numericInput('n', 'Number of obs', 100),
      plotOutput('plot')
    ),
    server = function(input, output) {
      output$plot <- renderPlot({ hist(runif(input$n)) })
    }
  ))


  # Running a Shiny app object
  app <- shinyApp(
    ui = bootstrapPage(
      numericInput('n', 'Number of obs', 100),
      plotOutput('plot')
    ),
    server = function(input, output) {
      output$plot <- renderPlot({ hist(runif(input$n)) })
    }
  )
  runApp(app)
}
}
