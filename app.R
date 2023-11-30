library(shiny)
library(tidyverse)
library(shinyFeedback)
library(rclipboard)
options(shiny.maxRequestSize = 30*1024^2)

# 001~010
short <- gsub("\\\\", "/", r"(\\oneabbott.com\dept\ADC\Technical_OPS\Clinical_Affairs\CDM_23238\)")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  titlePanel("InHouse 23238 Reconciliation Report"),

  useShinyFeedback(),
  a(span("Please email Alvin Lin if you run into any issues",style = "color:black"),href = "mailto:alvin.lin@abbott.com"),
  fluidRow(column(width = 12,
                  textInput("study",h6("Please enter InHouse study"),value = "",width = "400px"),
                  verbatimTextOutput("text"),
                  rclipboardSetup(),
                  uiOutput("clip"),
                  br())),
  fluidRow(column(width = 3,
                  fileInput("du1",h6("Please Upload DU1"),accept = ".sas7bdat",width = "400px")),
           column(width = 3,
                  fileInput("du2",h6("Please Upload DU2"),accept = ".sas7bdat",width = "400px")),
           column(width = 3,
                  fileInput("de",h6("Please Upload DE"),accept = ".sas7bdat",width = "400px")),
           column(width = 3,
                  fileInput("sa",h6("Please Upload Sensor Sheet"),accept = ".xlsx",width = "400px")),
                  actionButton("reset","Refresh")),
  br(),
  fluidRow(column(width = 12, downloadButton("download","Download Report",class = "btn-block", style = "width:100%;")))
)

server <- function(input, output, session) {

  ## Dynamic User interface
  observeEvent(input$reset,{
    session$reload()
  })

  text <- reactive({
        req(input$study)

        exists <- input$study %in% c(str_c("00",as.character(seq(1,9))),str_c("0",as.character(seq(10,15))))
        feedbackWarning("study",!exists,"Unkown Study")
        req(exists,cancelOutput = FALSE)

        cat(str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-23238","\\","OpenClinicaExtract","\\","Current","\\",input$study))
    })

  output$text <- renderPrint({text()})



  ## Copy button

  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy Path",
      clipText = str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-23238","\\","OpenClinicaExtract","\\","Current","\\",input$study),
      icon = icon("clipboard")
    )
  })

  if (interactive()){
    observeEvent(input$clipbtn, clipr::write_clip(str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-23238","\\","OpenClinicaExtract","\\","Current","\\",input$study)))
  }

  ## Download Report
  output$download <- downloadHandler(
    filename =
      function(){
        str_c("ADC-US-RES-23238"," ",input$study," Reconciliation Report ",Sys.Date(),".html")
      },
    content = function(file){

      params <- list(Study = input$study,
                     data1 = input$du1$datapath,
                     data2 = input$du2$datapath,
                     data3 = input$de$datapath,
                     data4 = input$sa$datapath,
                     data5 = str_c(short,input$study)
      )
      id <- showNotification(
        "Rendering Report...",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      ## Run Rmarkdown
      rmarkdown::render("ADC-US-RES-23238-Recon.Rmd",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )

}

shinyApp(ui, server)