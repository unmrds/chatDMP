library(shiny)
library(shinyjs)
library(openai)
library(stringi)
library(uuid)
library(shinyalert)
library(tidyr)
library(jsonlite)
library(dplyr)
library(lubridate)

# read local configuration options
source("config.R")
# read reference lists for use in controls
source("reference.R")
# read utility functions 
source("functions.R")


# Define Shiny App components
shinyApp(
  ui = fluidPage(
    useShinyjs(),
    titlePanel("Build a DMP and an Associated Machine Actionable JSON Version"),
    actionButton("fillSampleData", "Fill with sample content", class = "btn-lg"),
    tags$hr(),
    sidebarLayout(
      sidebarPanel(
        h2("Provide DMP and Submission Content and Update Centent for Submission"),
        h3("Data Management Plan Information"),
        textInput("dmpTitle", "Data Management Plan (DMP) Title", value=""),
        textAreaInput("dmpDescription", "DMP Description", value=""),
        textInput("dmpContact_name", "DMP Contact Name", value=""),
        textInput("dmpContact_orcid", "DMP Contact ORCID", value=""),
        textInput("dmpContact_mbox", "DMP Contact Email Address", value=""),
        sliderInput("pages",
                    "Length of DMP (pages)",
                    min=1,
                    max=5,
                    value=2),
        
        
        h3("Project Information"),
        selectInput("sponsor",
                    "Sponsor Agency",
                    sponsors,
                    selected="Generic"),
        textInput("projectTitle", "Project Title", value=""),
        textInput("projectPI", "Project Principal Investigator (PI)", value=""),
        textInput("projectPI_orcid", "PI ORCID", value=""),
        textInput("projectPI_mbox", "PI Email Address", value=""),
        textInput("projectDM", "Project Data Manager", value=""),
        textInput("projectDM_orcid", "Data Manager ORCID", value=""),
        textInput("projectDM_mbox", "Data Manager Email Address", value=""),
        textAreaInput("projectDesc", 
                      "Project Description", 
                      value=""),
        dateRangeInput("projectDates",
                       "Project Dates",
                       start = Sys.Date(),
                       end = Sys.Date()+365),
        
        
        h3("Information About the Data"),
        p("You can enter information about one or more types of data/code that will be produced/shared by the project."),
        textAreaInput("dataDesc", 
                      "Data Description", 
                      value=""),
        selectInput("dataFormat_active",
                    "Data Format During the Project",
                    dataFormats,
                    selected=""),
        selectInput("dataVolume_active",
                    "Data Volumne During the Project",
                    dataVolumes,
                    selected=""),
        selectInput("dataFormat_sharing",
                    "Data Format For Shared Data",
                    dataFormats,
                    selected=""),
        selectInput("dataVolume_sharing",
                    "Data Volumne That Will Be Shared",
                    dataVolumes,
                    selected=""),
        selectInput("programmingLanguage",
                    "Programming Language for Shared Code",
                    languages,
                    selected=""),
        selectInput("targetRepository",
                    "Target Repository",
                    repositories,
                    selected=""),
        selectInput("documentationStandard",
                    "Documentation Standard for Shared Data/Code",
                    documentationStandards,
                    selected=""),
        selectInput("license",
                    "License for Shared Data/Code",
                    licenses,
                    selected=""),
        radioButtons("data_pii",
                      "Do these data contain Personally Identifiable Information",
                     list("Yes","No"),
                     selected = "No"),
        radioButtons("data_cui",
                     "Do these data contain other Controlled Unclassified Information (CUI)",
                     list("Yes","No"),
                     selected = "No"),
        dateInput("dataAvailableDate",
                  "Date when the data will be made available for sharing",
                  value = Sys.Date()
                  ),
        actionButton("addData", "Add Data"),
        actionButton("clearDataItem", "Clear Last Data Item"),
        
        h3("DMP Generation Model Parameters"),
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
#        h2("2. Review Defined Submission Content Prior to Submitting for Processing"),
#        h3("Currently active Data Management Plan"),
#        tableOutput(outputId = "dmp_params"),
#        h3("DMP Data Elements"),
#        tableOutput(outputId = "dmp_data"),
#        tags$hr(),
        actionButton("buildAndSubmitCompletion", "Build and Submit the DMP Generation Request to ChatGPT", class = "btn-lg"),
        h3("ChatGPT Response"),
        verbatimTextOutput(outputId = "response"),
        h3("Metadata"),
        verbatimTextOutput(outputId = "rawData")
      )
    )
  ),
  
  server = function(input, output, session) {
    # https://stackoverflow.com/questions/64359528/creating-a-data-frame-from-inputs-in-shiny
    dmp <- reactiveVal("")
    dmp_uuid <- reactiveVal(UUIDgenerate())
    session_timestamp <- reactiveVal(gsub(":","",gsub(" ","_",ymd_hms(now(tzone="UTC")))))
    gptResponse <- reactiveVal("No DMP has been generated yet")
    change_index <- reactiveVal(0)
    
    data <- reactiveValues(
      data_list = data.frame(
        description = "",
        DataFormatActive = "",
        DataVolumeActive = "",
        DataFormatSharing = "",
        DataVolumeSharing = "",
        ProgrammingLanguage = "",
        TargetRepository = "",
        metadata = "",
        License = "",
        DataPII = "",
        DataCUI = "",
        DataAvailableDate = ""
      ) 
    )
    
    # Update dataframes with updated content from the form
    observeEvent(input$addData, {
      data$data_list <- rbind(data$data_list,data.frame(
        description = input$dataDesc,                                        
        DataFormatActive = input$dataFormat_active,
        DataVolumeActive = input$dataVolume_active,
        DataFormatSharing = input$dataFormat_sharing,
        DataVolumeSharing = input$dataVolume_sharing,
        ProgrammingLanguage = input$programmingLanguage,
        TargetRepository = input$targetRepository,
        metadata = input$documentationStandard,
        License = input$license,
        DataPII = input$data_pii,
        DataCUI = input$data_cui,
        DataAvailableDate = as.character(input$dataAvailableDate)))
    })
    observeEvent(input$clearDataItem, {
      data$data_list <- data$data_list %>% filter(row_number() <= n()-1)
    })
    
   
    observeEvent(input$fillSampleData, {
      updateTextInput(session, "dmpTitle", value="Generic Data Management Plan")
      updateTextInput(session, "dmpDescription", value="This DMP is meant as a demonstration of the AI generation process based on complete demo data") 
      updateTextInput(session, "dmpContact_name", value="DCfirst DClast")
      updateTextInput(session, "dmpContact_orcid", value="0000-0000-0000-0000")
      updateTextInput(session, "dmpContact_mbox", value="dc@email.address.com")
      
      updateTextInput(session, "projectTitle", value="This is the title of our fascinating simulated project")
      updateTextInput(session, "projectPI", value="PIFirst PILast")
      updateTextInput(session, "projectPI_orcid", value="0000-0000-0000-0000")
      updateTextInput(session, "projectPI_mbox", value="pi@email.address.com")
      updateTextInput(session, "projectDM", value="DMFirst DMLast")
      updateTextInput(session, "projectDM_orcid", value="0000-0000-0000-0000")
      updateTextInput(session, "projectDM_mbox", value="dm@email.address.com")
      updateTextInput(session, "projectDesc", value="This project will collect a wide range of data, analyze it, and produce significant outcomes.")
      
      data$data_list <- rbind(data$data_list,data.frame(
        description = "The first dataset in the collection of data",                                        
        DataFormatActive = "Spreadsheet - Excel",
        DataVolumeActive = "1-100 MB",
        DataFormatSharing = "Structured Data - CSV",
        DataVolumeSharing = "1-100 MB",
        ProgrammingLanguage = "Python",
        TargetRepository = "Dryad",
        metadata = "Readme file(s)",
        License = "CC0",
        DataPII = "No",
        DataCUI = "Yes",
        DataAvailableDate = as.character(Sys.Date())))
      
      data$data_list <- rbind(data$data_list,data.frame(
        description = "The second dataset in the collection of data",                                        
        DataFormatActive = "Statistical - MatLab",
        DataVolumeActive = "1-50 GB",
        DataFormatSharing = "Structured Data - CSV",
        DataVolumeSharing = "1-50 GB",
        ProgrammingLanguage = "MatLab",
        TargetRepository = "Dryad",
        metadata = "Readme file(s)",
        License = "CC0",
        DataPII = "Yes",
        DataCUI = "No",
        DataAvailableDate = as.character(Sys.Date())))
      
      data$data_list <- rbind(data$data_list,data.frame(
        description = "The third dataset in the collection of data",                                        
        DataFormatActive = "Geospatial - Shapefile",
        DataVolumeActive = "1-50 GB",
        DataFormatSharing = "Geospatial - GML",
        DataVolumeSharing = "1-50 GB",
        ProgrammingLanguage = "",
        TargetRepository = "ICPSR",
        metadata = "ISO 19115",
        License = "CC-BY-SA-ND-NC 4.0",
        DataPII = "No",
        DataCUI = "No",
        DataAvailableDate = as.character(Sys.Date())))
      
      print(data$data_list)
      
    })
    
    observeEvent(input$buildAndSubmitCompletion, {
      # update chat messages content for the ChatGPT request
      chatCompletion$model <- unbox(input$model)
      chatCompletion$temperature <- unbox(input$temp)
      chatCompletion$presence_penalty <- unbox(input$pp)
      chatCompletion$frequency_penalty <- unbox(input$fp)
      
      # rebuild messages list ##################################################
      chatCompletion$messages[[1]] <- list(
        "role" = unbox("system"),
        "content" = unbox("You are an experienced researcher")
      )
      # sponsor
      if (unbox(input$sponsor) != "") {
        chatCompletion$messages[[length(chatCompletion$messages) + 1]] <- list(
          "role" = unbox("user"),
          "content" = unbox(paste("Please write a ", unbox(as.character(input$pages)), " page ", unbox(input$sponsor), " ", unbox(planNames[[match(unbox(input$sponsor), sponsors)]]), sep = ""))
        )
      }
      # DMP title
      if (unbox(input$dmpTitle) != "") {
        chatCompletion$messages[[length(chatCompletion$messages) + 1]] <- list(
          "role" = unbox("user"),
          "content" = unbox(paste("The title of the data management plan is '", unbox(input$dmpTitle), "'", sep = ""))
        )
      }
      ## title
      if (unbox(input$projectTitle) != "") {
        chatCompletion$messages[[length(chatCompletion$messages) + 1]] <- list(
          "role" = unbox("user"),
          "content" = unbox(paste("The title of the project is '", unbox(input$projectTitle), "'", sep = ""))
        )
      }
      ## description
      if (unbox(input$projectDesc) != "") {
        chatCompletion$messages[[length(chatCompletion$messages) + 1]] <- list(
          "role" = unbox("user"),
          "content" = unbox(input$projectDesc)
        )
      }
      ## Data (process each row of the dataframe of individual dataset attributes)
      for (row in 1:nrow(data$data_list)) {
        if (data$data_list[row, "description"] != "") {
          chatCompletion$messages[[length(chatCompletion$messages) + 1]] <- list(
            "role" = unbox("user"),
            "content" = paste(unbox(data$data_list[row, "description"]),". ",
                              "The collected data will include ", unbox(data$data_list[row, "DataVolumeActive"]), " of ", unbox(data$data_list[row, "DataFormatActive"]), " data. ",
                              unbox(data$data_list[row, "DataVolumeSharing"]), " of ", unbox(data$data_list[row, "DataFormatSharing"]), " data will be shared through the ", 
                              unbox(data$data_list[row, "TargetRepository"]), " repository after ", unbox(data$data_list[row, "DataAvailableDate"]), ". ",
                              "The shared data will be shared using the ", unbox(data$data_list[row, "License"]), " license ",
                              "and documented using the ", unbox(data$data_list[row, "metadata"]), " metadata standard. ", 
                              ifelse(unbox(data$data_list[row, "DataPII"]) == "Yes", "This dataset contains Personally Identifiable Information. ", "This dataset does not contain Personally Identifiable Information. "),
                              ifelse(unbox(data$data_list[row, "DataCUI"]) == "Yes", "This dataset contains other Controlled Unclassified Information. ", "This dataset does not contain other Controlled Unclassified Information. "))
          )
        }
      }
      print(chatCompletion$messages)
      
      
      # submit request #########################################################
      print(paste("submitting request:", ymd_hms(now(tzone="UTC"))))
      gptResponse(paste("Submitting request: ", ymd_hms(now(tzone="UTC")), "\n", sep=""))
      #print(chatCompletion)
      response <- create_chat_completion(
        model = chatCompletion$model,
        temperature = as.numeric(chatCompletion$temperature),
        presence_penalty = as.numeric(chatCompletion$presence_penalty),
        frequency_penalty = as.numeric(chatCompletion$frequency_penalty),
        openai_api_key = .api_key,
        messages = chatCompletion$messages
      )
      print
      gptResponse(paste(gptResponse(), "Response received: ", ymd_hms(now(tzone="UTC")),"\n\n", sep=""))
      gptResponse(paste(gptResponse(), "\n\n", response$choices$message.content, sep=""))
      print(paste("Request Completed:", ymd_hms(now(tzone="UTC"))))
      dmp(response$choices$message.content)
      #gptResponse(response)
      gptResponse <- response$choices$message.content
    })
    
    output$response <- renderText({
      Response <- gptResponse()
      Response <- paste(Response, "\n\nNote: The first draft of this DMP was generated by ChatGPT version: ", unbox(input$model), " and then reviewed and revised for commpleteness and accuracy.", sep="")
      Response
    })
    
    output$rawData <- renderText({
      ChangeIndex = change_index()
      
      # DMP metadata elements
      DMPTitle = unbox(input$dmpTitle)
      DMPDescription = unbox(input$dmpDescription)
      DMPContactName = unbox(input$dmpContact_name)
      DMPContactORCID = unbox(input$dmpContact_orcid)
      DMPContactMbox = unbox(input$dmpContact_mbox)
      DMPLength = unbox(as.character(input$pages))
      DMPText = unbox(dmp())
      
      # Project metadata elements
      ProjectTitle = unbox(input$projectTitle)
      ProjectDesc = unbox(input$projectDesc)
      ProjectDates_start = unbox(as.character(input$projectDates[1]))
      ProjectDates_end = unbox(as.character(input$projectDates[2]))
      ProjectSponsor = unbox(input$sponsor)
      ProjectPI = unbox(input$projectPI)
      ProjectPI_orcid = unbox(input$projectPI_orcid)
      ProjectPI_mbox = unbox(input$projectPI_mbox)
      ProjectDM = unbox(input$projectDM)
      ProjectDM_orcid = unbox(input$projectDM_orcid)
      ProjectDM_mbox = unbox(input$projectDM_mbox)
      
      DataList = data$data_list
      
      # update chat messages content for the ChatGPT request
      chatCompletion$model <- unbox(input$model)
      chatCompletion$temperature <- unbox(input$temp)
      chatCompletion$presence_penalty <- unbox(input$pp)
      chatCompletion$frequency_penalty <- unbox(input$fp)
      # rebuild messages list
      chatCompletion$messages[[1]] <- list(
        "role" = unbox("system"),
        "content" = unbox("You are an experienced researcher")
      )
      # sponsor
      if (unbox(input$sponsor) != "") {
        chatCompletion$messages[[length(chatCompletion$messages) + 1]] <- list(
          "role" = unbox("user"),
          "content" = unbox(paste("Please write a ", unbox(as.character(input$pages)), " page ", unbox(input$sponsor), " ", unbox(planNames[[match(unbox(input$sponsor), sponsors)]]), sep = ""))
        )
      }
      # title
      if (unbox(input$projectTitle) != "") {
        chatCompletion$messages[[length(chatCompletion$messages) + 1]] <- list(
          "role" = unbox("user"),
          "content" = unbox(paste("The title of the project is '", unbox(input$projectTitle), "'", sep = ""))
        )
      }
      # description
      if (unbox(input$projectDesc) != "") {
        chatCompletion$messages[[length(chatCompletion$messages) + 1]] <- list(
          "role" = unbox("user"),
          "content" = unbox(input$projectDesc)
        )
      }
      
      
      # Project metadata elements
      temp_data_list <- data$data_list
      output_list <- list(
        "dmp" = list(
          title = DMPTitle,
          description = DMPDescription,
          created = unbox(ymd_hms(now(tzone="UTC"))),
          modified = unbox(ymd_hms(now(tzone="UTC"))),
          contact = list(
            contact_id = list(
              identifier = DMPContactORCID,
              type = unbox("orcid")
            ),
            mbox = DMPContactMbox,
            name = DMPContactName
          ),
          contributor = list(
            list(
              name = ProjectPI,
              mbox = ProjectPI_mbox,
              role = "Principle Investigator",
              contributor_id = list(
                identifier = ProjectPI_orcid,
                type = unbox("orcid")
              )
            ),
            list(
              name = ProjectDM,
              mbox = ProjectDM_mbox,
              role = "Data Manager",
              contributor_id = list(
                identifier = ProjectDM_orcid,
                type = unbox("orcid")
              )
            )
          ),
          dmp_id = list(
            identifier = unbox(dmp_uuid()),
            type = unbox("other")
          ),
          project = list(
            list(
              title = ProjectTitle,
              description = ProjectDesc,
              start = ProjectDates_start,
              end = ProjectDates_end
            )
          ),
          dataset = DataList
        ),
        "dmp_text" = DMPText,
        "generation_notes" = unbox(
          paste("DMP generated by ChatGPT version '", unbox(input$model), "', with a Temp setting of '", unbox(input$temp), "', with a Presence Penalty of '", unbox(input$pp), "', and a Frequency Penalty of '", unbox(input$fp), "'", sep = "")
        ),
        "other_elements" = list(
          sponsor = ProjectSponsor
        ),
        "ChatGPT_chat_completion" = chatCompletion
      )
      #output_list <- list("project"=unbox(temp_dmp_list), "data"=temp_data_list, "model"=unbox(temp_model_list), "plan" = dmp())
      outfile_name <- paste("output/",session_timestamp(),"_",dmp_uuid(),".json", sep="")
      output_text <- prettify(toJSON(output_list), indent = 4)
      write(output_text, outfile_name)
      output_text
    })
  }
  
)