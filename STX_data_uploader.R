library(shiny)
library(tidyverse)
library(openxlsx2)
library(sharepointR)
library(httr)
library(readr)

source('www/src/STX_functions.R')

from_chars <- "асоуікеАСОУІКЕ"
to_chars <- "acoyikeACOYIKE"


ui <- fluidPage(
  titlePanel("Upload your datasets here"),
  tags$style(HTML(
    "
        #table-container {
          overflow: visible !important;
        }"
  )),
  HTML(
    '<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>'
  ),
  includeCSS("www/style.css"),
  HTML(
    '<a style="padding-left:10px;" class="app-title" href= "https://www.reach-initiative.org/" target="_blank"><img src="reach.jpg" height = "50"></a><span class="app-description" style="font-size: 16px; color: #FFFFFF"><strong>Database_test</strong></span>'
  ),
  sidebarLayout(
    sidebarPanel(
      # Block 1: File Upload and Column Selection
      fileInput("datafile", "Please select your data file"),
      
      selectInput("selector0", "Select the sheet from your datafile that holds the main dataframe", choices = NULL),
      
      selectizeInput("selected_sheets", "Select sheets of your dataframe that hold the data", choices = NULL, multiple = TRUE),
      
      
      # Block 2: Additional Selectors (Enable only after data upload)
      selectInput("selector1", "Select the Project ID", choices = NULL),
      selectInput("selector2", "Select the round", choices = NULL),
      selectInput("selector3", "Select the type of the survey", 
                  choices = c('Household', 'Individual','Consumer','Retailer','Key_informant')),
      
      
      selectizeInput("selected_columns", "Select the columns in your data for which it is still representative", choices = NULL, multiple = TRUE),
      
      
      br(),
      
      # Block 3: File Upload for Block 2
      
      selectInput("selector4", "Does this project have a Kobo tool?", choices = c('Yes','No')),

      conditionalPanel(condition = "input.selector4 =='Yes'",
                       fileInput("datafile2", "Upload the tool for this round of the research cycle here")
      ),
      br(),
      actionButton("custom_action", "Upload your data into the DB")
    ),
    
    
    mainPanel(
      # Display data, plots, or other output here
    )
  )
)

server <- function(input, output, session) {
  
  choices_pr <- reactive({
    if (is.null(input$datafile))
      return(NULL)
    
    
    database_research_cycle <-
      od$load_rds("Documents/Questions_db/Research_cycle_tracker.rds")
    
    
    choices <- database_research_cycle[['Research_cycle_ID']]
    return(choices)
  })
  
  
  # Observe the uploaded data file
  observeEvent(input$datafile, {
    req(input$datafile)
    
    sheets <- readxl::excel_sheets(path = input$datafile$datapath)
    
    updateSelectInput(session, "selector0", choices = sheets, selected = NULL)
    
    updateSelectizeInput(session, "selected_sheets", choices = sheets,selected = NULL)
    
  })

  observeEvent(input$selector0, {
    req(input$datafile)
    req(input$selector0)

    data <- readxl::read_excel(input$datafile$datapath, sheet = input$selector0, col_types = 'text')

    column_names <- data%>%
      select_if(~any(grepl("^UA\\d+", .))) %>%
      names()
    
    column_names <- c(column_names,'None')

    # Propose column names for selection
    updateSelectizeInput(session, "selected_columns", choices = column_names,selected = NULL)
    
  })

  # Update additional selectors when data is uploaded
  observe({
    req(input$datafile)
    updateSelectInput(session, "selector1", choices = choices_pr())
    updateSelectInput(session, "selector2", choices = 1:30)
  })

  observeEvent(input$custom_action, {
    req(input$datafile)
    if(input$selector4 == 'Yes'){
      req(input$datafile2)
    }

    project_name <- input$selector1
    project_round <- input$selector2
    project_type <- input$selector3
    
    inFile <- input$datafile$datapath

    if (is.null(inFile))
      # Only read data if button is clicked
      return(NULL)
    sheet_names <-  input$selected_sheets
    # create your workbook, it'll be one of the inputs in the sp_post_file
    
    file_names <- c('main', paste0('loop',1:length(sheet_names)))
    
    for (i in 1:length(sheet_names)){
      data <- readxl::read_excel(inFile, sheet = sheet_names[i], col_types = 'text')
      names(data) <- chartr(from_chars, to_chars, names(data))


      
    od$save_rds(data, paste0('Documents/Questions_db/',project_name,'_round_',project_round,'_',project_type,'_data_',file_names[i],'.csv'))
      
    }
    
    if(input$selector4 == 'Yes'){
      
    infile2 <- input$datafile2$datapath
    if (is.null(infile2))
      # Only read data if button is clicked
      return(NULL)
    sheet_names <-  c('survey', 'choices')

    for (sheet in sheet_names){
      data <- readxl::read_excel(infile2, sheet = sheet, col_types = 'text')
      if (sheet == 'survey'){
      relevant_columns <- names(data)[
        tolower(names(data)) %in% c('sector','type','name','relevant','calculation','required',
                                    'repeat_count','appearance','parameters','choice_filter','default'
                                    )]
      data <- data %>% select(all_of(relevant_columns), starts_with(c('label','hint','constraint')))
      }else{
        data$list_name <- chartr(from_chars, to_chars, data$list_name)
      }
      data$name <- chartr(from_chars, to_chars, data$name)
      
      eval(parse(text = paste0(
        project_name,'_round_',project_round,'_',project_type,'_tool_',sheet,' <<- data'
      )))
      
    od$save_rds(data, paste0('Documents/Questions_db/',project_name,'_round_',project_round,'_',project_type,'_tool_',sheet,'.rds'))
    
    }}
    # add a thing to append the thing

    
    representativeness_full <-
      od$load_rds("Documents/Data/Representativeness_dictionary.rds")
    
    representativeness <- data.frame(
      Project_ID = project_name,
      Round_ID = project_round,
      Representative_columns = input$selected_columns
    )
    
    representativeness <- rbind(representativeness_full,representativeness) %>% 
      distinct()
    

    od$save_rds(data, 'Data/Representativeness_dictionary.rds')

    showModal(
      modalDialog(
        title = "Tables Saved",
        "The tables have been saved into the database.",
        footer = NULL,
        easyClose = TRUE
      )
    )
    rm(list = ls()[sapply(ls(), \(x) inherits(get(x), 
                                              what = c("data.frame")))])
    
    Sys.sleep(2)
    session$reload()
  })

}

shinyApp(ui, server)
