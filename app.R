# Install packages
library(shiny)
library(plyr)
library(dplyr)
library(shinythemes)
library(shinycssloaders)
library(formattable)
library(ggplot2)
library(scales)

prettyNames <- function(DFin) {
  names(DFin) <- gsub(" ","",names(DFin),fixed=TRUE)
  names(DFin) <- gsub(".","",names(DFin),fixed=TRUE)
  names(DFin) <- gsub("-","",names(DFin),fixed=TRUE)
  names(DFin) <- tolower(names(DFin))
}

# Set up maximum file size
options(shiny.maxRequestSize = 500*1024^2)

# 1. Uploading and parsing the file:
ui_upload <- sidebarLayout(
  sidebarPanel(
    fileInput("file0", "P1", buttonLabel = "Upload..."),
    fileInput("file1", "SFTP", buttonLabel = "Upload..."),
    numericInput("skip1", "Rows to skip in SFTP", 0, min = 0),
    numericInput("rows", "Rows to preview", 5, min = 1),
    radioButtons("sep1", "SFTP Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ",")),
  mainPanel(
    h3("P1 Raw data"),
    tableOutput("preview0"),
    h3("SFTP Raw data"),
    tableOutput("preview1")
  )
)

# 2. Selecting wanted columns:
ui_select0 <- sidebarLayout(
  sidebarPanel(
    selectInput("select0a", "Select dimensions for P1",  c('Column names'), multiple = TRUE),
    selectInput("select0b", "Select metrics",  c('Dmp Impressions'), multiple = FALSE),
    div(style = "text-align:left; font-size:13px; font-style: italic","Notes: Select the metrics that you want to summarize in the final table."),
    actionButton("update0", "Update", class = "btn-primary", style="color: #fff; background-color: #05cc7a; border-color: #05cc7a")
  ),
  mainPanel(
    h3("P1 Selected data"),
    tableOutput("preview00")
  )
)

ui_select1 <- sidebarLayout(
  sidebarPanel(
    selectInput("select1", "Select columns for SFTP",  c('Column names'), multiple = TRUE),
    actionButton("update1", "Update", class = "btn-primary", style="color: #fff; background-color: #05cc7a; border-color: #05cc7a")
  ),
  mainPanel(
    h3("SFTP Selected data"),
    tableOutput("preview11")
  )
)

# 3. Cleaning the file:
ui_QA <- sidebarLayout(
  sidebarPanel(
    numericInput("rows2", "Rows to preview", 10, min = 1),
    actionButton("update", "Preview your QA results", class = "btn-primary", style="color: #fff; background-color: #05cc7a; border-color: #05cc7a"),
  ),
  mainPanel(
    h3("Summary"),
    withSpinner(plotOutput("plot"),color = '#05cc7a', proxy.height = 100),
    h3("Preview your QA table"),
    withSpinner(tableOutput("preview2"), color = '#05cc7a', proxy.height = 100)
  )
)

# 4. Downloading the file:
ui_download <- fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block"))
)

#Assembling into the FluidPage():
ui <- fluidPage(
  theme = shinytheme('flatly'),
  img(src = "neustar-logo.png", height = "70px"),
  titlePanel("Automation Spend/Taxonomy QA"),
  ui_upload,
  ui_select0,
  ui_select1,
  ui_QA,
  ui_download
)

# Server:
server <- function(input, output, session) {
  
  # Upload ---------------------------------------------------------
  df0 <- reactive({
    ext <- tools::file_ext(input$file0$datapath)
    validate(need((ext == "csv"), "Please upload a csv file"))
    data = read.csv(input$file0$datapath, check.names = FALSE)
    validate(need(ncol(data) == 30, 'Please upload a discrepancy report from P1'))
    data = read.csv(input$file0$datapath, check.names = FALSE)
  })
  
  output$preview0 <- renderTable(
    if(!is.na(input$rows)){
      head(df0(), input$rows)}
    else{
      head(df0(), 0)
    })
  
  df0a <- reactive({df0() %>% dplyr::select(1:15, 29:30)})
  df0b <- reactive({df0() %>% dplyr:: select(16:17)})
  
  df1 <- reactive({
    if (!is.na(input$skip1)) {
      ext <- tools::file_ext(input$file1$datapath)
      validate(need((ext == "csv"), "Please upload a csv file"))
      data = read.csv(input$file1$datapath, skip = input$skip1, check.names = FALSE, sep = input$sep1)}
    else {
      ext <- tools::file_ext(input$file1$datapath)
      validate(need((ext == "csv"), "Please upload a csv file"))
      data = read.csv(input$file1$datapath, skip = 0, check.names = FALSE)}
  })
  
  output$preview1 <- renderTable(if (!is.na(input$rows)){
    head(df1(), input$rows)}
    else{
      head(df1(), 0)
    })
  
  # Select ---------------------------------------------------------
  filtereddata0a <- eventReactive({
    input$update0
    df0a()
  },  {
    req(df0a())
    if(is.null(input$select0a) || input$select0a == "")
      df0a() else 
        df0a()[, colnames(df0a()) %in% c(input$select0a)]
    
  })
  observeEvent(df0a(), {
    updateSelectInput(session, "select0a", choices = colnames(df0a()))
  })
  
  filtereddata0b <- eventReactive({
    input$update0
    df0b()
  },  {
    req(df0b())
    if(is.null(input$select0b) || input$select0b == "")
      df0b() else 
        df0b()[, colnames(df0b()) %in% c(input$select0b)]
    
  })
  
  observeEvent(df0b(), {
    updateSelectInput(session, "select0b", choices = colnames(df0b()))
  })
  output$text <- renderText({
    "Notes: Select the metrics you want to summarize in the final table."
  })
  
  output$preview00 <- renderTable(if (!is.na(input$rows)){
    head(cbind(filtereddata0a(), Metrics = filtereddata0b()), input$rows)}
    else{
      head(cbind(filtereddata0a(), Metrics = filtereddata0b()), 0)
    })
  
  
  filtereddata1 <- eventReactive({
    input$update1
    df1()
  },  {
    req(df1())
    if(is.null(input$select1) || input$select1 == "")
      df1() 
    else 
      df1()[, input$select1, drop = FALSE]
  })
  observeEvent(df1(), {
    updateSelectInput(session, "select1", choices = colnames(df1()))
  })
  output$preview11 <- renderTable(if (!is.na(input$rows)){
    head(filtereddata1(), input$rows)}
    else{
      head(filtereddata1(), 0)
    })
  
  # QA ----------------------------------------------------------
  tidied <- eventReactive({
    input$update},{
      df0 <- cbind(filtereddata0a(), Metrics = filtereddata0b())
      df1 <- filtereddata1()
      names(df0) <- prettyNames(df0)
      df0[is.na(df0)] <- 0 # Main purpose is to converts NULL/NA so it does not affect roll up dimensions
      df0 <- as.data.frame(lapply(df0, as.character)) 
      names(df1) <- prettyNames(df1)
      df1[is.na(df1)] <- 0 # Main purpose is to converts NULL/NA so it does not affect roll up dimensions
      df1 <- as.data.frame(lapply(df1, as.character)) 
      
      # Create dummy summary tables ####
      join_summary <- data.frame(matrix(ncol = 9, nrow = 0))
      summary_col_names <- c("mapped","SumEvents", "distinct_key_count", "event_percent","distinct_key_percent",  "p1_join_key", "ext_join_key", "integration", "type")
      colnames(join_summary) <- summary_col_names
      
      # Start of multiple combo loops ####
      # h loop gets filter the row(s) of the file to pick the locations
      for (i in 1:ncol(df1)) {
        distinct_taxonomy_by_keys <- df1 %>%
          group_by_at(i) %>% 
          summarise_all(n_distinct)
        join_key_map_df <- distinct_taxonomy_by_keys[,1]
        join_key_map_df$mapped <- "Y"
        for (j in 1:ncol(df0)) {
          joined_table <- merge(df0, join_key_map_df, by.x = colnames(df0[j]), by.y = colnames(join_key_map_df[1]), all.x = TRUE, all.y = FALSE)
          join_key_full_table <- joined_table %>%
            group_by_at(vars(one_of(colnames(joined_table[1]),colnames(joined_table[ncol(joined_table)])))) %>%
            summarise(SumEvents = sum(as.numeric(`metrics`)),
                      distinct_key_count = n_distinct(colnames(joined_table[1])))
          mapped_summary <- join_key_full_table %>%
            group_by(mapped) %>% 
            summarise(SumEvents = sum(as.numeric(SumEvents)),
                      distinct_key_count = sum(as.numeric(distinct_key_count))) %>% 
            #mutate(event_percent = paste(round((SumEvents/sum(SumEvents)*100),0),"%", sep = ""),
                   #distinct_key_percent = paste(round((distinct_key_count/sum(distinct_key_count)*100),0),"%", sep = "")) %>%
            #mutate_if(is.numeric, format, 1)
            mutate(event_percent = (SumEvents/sum(SumEvents)),
                    distinct_key_percent = distinct_key_count/sum(distinct_key_count))
          mapped_summary$p1_join_key <- colnames(joined_table[1])
          mapped_summary$ext_join_key <- colnames(join_key_map_df[1])
          join_summary <- rbind.fill(join_summary,mapped_summary)
          #join_summary <- join_summary %>% arrange(desc(mapped))
        }
        
      }  
      join_summary
    })
    
    
  output$plot <- renderPlot({
    data <- tidied() %>% 
      filter(mapped != '') 
    validate(need((nrow(data) > 0), "No matched field exists. There will be no charts shown."))
    if (nrow(data) > 0 ) {
      ggplot(data = data, aes(x = reorder(p1_join_key, event_percent), y = event_percent)) +
        geom_bar(stat = "identity") + 
        scale_y_continuous(labels = scales::percent) +
        scale_x_discrete(labels = paste(data$p1_join_key, '\n', data$ext_join_key)) +
        geom_text(label = percent(data$event_percent))+
        coord_flip()+
        ylab("Event percentage") +
        xlab("Join key") + 
        theme(
          axis.text.x = element_text(color="black", size=14, angle=45),
          axis.text.y = element_text(color="black", size=14),
          axis.title.x = element_text(color="#05cc7a", size=14, face="bold"),
          axis.title.y = element_text(color="#05cc7a", size=14, face="bold")
        )}
  })
  
  output$preview2 <- renderTable({
    if (!is.na(input$rows2)){
      head(tidied(), input$rows2)}
    else{
      head(tidied(), 0)
    }})
  
  # Download -------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {"qaresults.xlsx"},
    content = function(file) {writexl::write_xlsx(tidied(), path = file)}
  )}
shinyApp(ui = ui, server = server)

