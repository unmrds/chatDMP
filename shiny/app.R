library(shiny)
library(openai)
library(stringi)
library(uuid)
library(shinyalert)
library(tidyr)

# read local configuration options
source("config.R")
# read reference lists for use in controls
source("reference.R")


# Define Shiny App components
shinyApp(
  ui = fluidPage(
    titlePanel("Build / Upload a DMP for Review"),
    tags$hr(),
    sidebarLayout(
      sidebarPanel(
        h2("Project Information"),
        textInput("projectTitle", "Project Title", value="Project Title"),
        textInput("projectPI", "Project Principal Investigator", value="PI Name"),
        textInput("projectDM", "Project Data Manager", value="Data Manager Name"),
        textAreaInput("projectDesc", 
                      "Project Description", 
                      value="Short Project Description - e.g. An examiniation of the relationship between ground water level and vegetation greenness"),
        selectInput("sponsor",
                    "Sponsor Agency",
                    sponsors,
                    selected="Institute of Museum and Library Services"),
        dateRangeInput("projectDates",
                       "Project Dates",
                       start = Sys.Date(),
                       end = Sys.Date()+365),
        
        h2("Additional Information"), 
        sliderInput("pages",
                    "Length of DMP (pages)",
                    min=1,
                    max=5,
                    value=2),
        actionButton("submit", "Update DMP Information"),
        
        
        tags$hr(),
        h2("Information About the Data"),
        p("You can enter information about more than one type of data/code."),
        selectInput("dataFormat_active",
                    "Data Format During the Project",
                    dataFormats,
                    selected="Structured Data - CSV"),
        selectInput("dataVolume_active",
                    "Data Volumne During the Project",
                    dataVolumes,
                    selected="1-50 GB"),
        selectInput("dataFormat_sharing",
                    "Data Format For Shared Data",
                    dataFormats,
                    selected="Structured Data - CSV"),
        selectInput("dataVolume_sharing",
                    "Data Volumne That Will Be Shared",
                    dataVolumes,
                    selected="1-50 GB"),
        selectInput("programmingLanguage",
                    "Programming Language for Shared Code",
                    languages,
                    selected="None"),
        selectInput("targetRepository",
                    "Target Repository",
                    repositories,
                    selected="Dryad"),
        selectInput("documentationStandard",
                    "Documentation Standard for Shared Data/Code",
                    documentationStandards,
                    selected="Readme file(s)"),
        selectInput("license",
                    "License for Shared Data/Code",
                    licenses,
                    selected="CC0"),
        actionButton("addData", "Add Data"),
        actionButton("clearData", "Clear Data"),
        
        
        tags$hr(),
        h2("Additional Model Parameters"),
        p("Options for changing the ChatGPT prompt parameters."),
        selectInput("model",
                    "ChatGPT Model to Use",
                    models,
                    selected="gpt-3.5-turbo"),
        sliderInput("temp",
                    "Temperature - ranging from more deterministic -> more random",
                    min=0,
                    max=2,
                    value=1),
        sliderInput("pp",
                    "Presence Penalty - low to high liklihood of new topics in response",
                    min=-2,
                    max=2,
                    value=0),
        sliderInput("fp",
                    "Frequency Penalty - high to low likelyhood of repetition",
                    min=-2,
                    max=2,
                    value=0),
        
        
      ),
      mainPanel(
        h2("Currently active Data Management Plan"),
        tableOutput(outputId = "dmp_params"),
        h2("DMP Data Elements"),
        tableOutput(outputId = "dmp_data")
      )
    )
  ),
  
  server = function(input, output, session) {
    # https://stackoverflow.com/questions/64359528/creating-a-data-frame-from-inputs-in-shiny
    rv <- reactiveValues(
      dmp_df = data.frame(
        ProjectTitle = character(),
        ProjectPI = character(),
        ProjectDM = character(),
        ProjectDesc = character(),
        Sponsor = character(),
        ProjectDates_start = character(),
        ProjectDates_end = character(),
        Pages = character()
      ) 
    )
    dv <- reactiveValues(
      data_df = data.frame(
        DataFormatActive = character(),
        DataVolumeActive = character(),
        DataFormatSharing = character(),
        DataVolumeSharing = character(),
        ProgrammingLanguage = character(),
        TargetRepository = character(),
        DocumentationStandard = character(),
        License = character()
      ) 
    )
    mv <- reactiveValues(
      modelValues_df = data.frame(
        Model = character(),
        Temp = numeric(),
        PP = numeric(),
        FP = numeric(),
        Completion = list()
      )
    )
  
    observeEvent(input$submit, {
      rv$dmp_df <- rbind(data.frame(ProjectTitle = input$projectTitle,
                                    ProjectPI = input$projectPI,
                                    ProjectDM = input$projectDM,
                                    ProjectDesc = input$projectDesc,
                                    Sponsor = input$sponsor,
                                    ProjectDates_start = as.character(input$projectDates[1]),
                                    ProjectDates_end = as.character(input$projectDates[2]),
                                    Pages = as.character(input$pages)))
    })
    observeEvent(input$addData, {
      dv$data_df <- rbind(dv$data_df,data.frame(DataFormatActive = input$dataFormat_active,
                                                DataVolumeActive = input$dataVolume_active,
                                                DataFormatSharing = input$dataFormat_sharing,
                                                DataVolumeSharing = input$dataVolume_sharing,
                                                ProgrammingLanguage = input$programmingLanguage,
                                                TargetRepository = input$targetRepository,
                                                DocumentationStandard = input$documentationStandard,
                                                License = input$license))
    })
    observeEvent(input$clearData, {
      dv$data_df <- rbind(data.frame(DataFormatActive = character(),
                                                DataVolumeActive = character(),
                                                DataFormatSharing = character(),
                                                DataVolumeSharing = character(),
                                                ProgrammingLanguage = character(),
                                                TargetRepository = character(),
                                                DocumentationStandard = character(),
                                                License = character()))
    })
    output$dmp_params <- renderTable({
      new_df <- rv$dmp_df %>% 
        pivot_longer(cols = everything(),
                     names_to="Parameter",
                     values_to="Value")
      new_df
    })
    output$dmp_data <- renderTable({
      new_df <- dv$data_df
      new_df
    })
  }
  
)