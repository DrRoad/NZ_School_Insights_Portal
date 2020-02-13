library(dplyr)
library(rjson)
library(jsonlite)
library(RCurl)
library(shiny)
library(highcharter)
library(shinydashboard)
library(leaflet)
#library(googleway)
library(leaflet.extras)
#library(mapview)
library(DT)
#library(leafem)
#library(data.table)


#key <- "AIzaSyBAh7LNeVKW9tjwaiCS7TSW3RRAcgHrOpw"
#set_key(key = key)


school_data_url <- "https://catalogue.data.govt.nz/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%22bdfe0e4c-1554-4701-a8fe-ba1c8e0cc2ce%22"

school_data <- fromJSON(school_data_url)

school_data_df <- school_data$result$records
school_data_df <- school_data_df[,-c(37)]

school_data_df$Decile <- as.numeric(school_data_df$Decile)
school_data_df$Isolation_Index <- as.numeric(school_data_df$Isolation_Index)

school_data_df$Longitude <- as.numeric(school_data_df$Longitude)
school_data_df$Latitude <- as.numeric(school_data_df$Latitude)

school_data_df$School_Id <- as.numeric(school_data_df$School_Id)


# Define UI for
ui <- dashboardPage(
  dashboardHeader(title = 'NZ School Insights Portal'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('School Summary', tabName = 'summary', icon = icon('th')),
      menuItem('School Search', tabName = 'search', icon = icon('school')),
      menuItem('School Compare', tabName = 'comparison', icon = icon('th'))
    )
  ),
  dashboardBody(
    tabItems(
      #define summary tab
      tabItem(tabName = 'summary',
              h2('School Summary'),
              selectInput("region",
                                   "Region:",
                                   c("All",
                                     unique(as.character(school_data_df$Education_Region)))),
              
              fluidRow(
                
                infoBoxOutput('infobox1', width = 3),
                infoBoxOutput('infobox2', width = 3),
                infoBoxOutput('infobox3', width = 3),
                infoBoxOutput('infobox4', width = 3)
                
              ),
              
              fluidRow(
                
                column(6,highchartOutput("graph1")),
                
                column(6,highchartOutput("graph2"))
                
              ),
              
              
              fluidRow(
                
                column(6,highchartOutput("graph3")),
                
                column(6,highchartOutput("graph4"))
                
              ),
              
              fluidRow(
                
                column(6,highchartOutput("graph5")),
                
                column(6,highchartOutput("graph6"))
                
              )
            
            ),
      #define search tab
      tabItem(tabName = 'search',
              
              h2('School Search Criteria'),
              
              fluidRow(
                column(4,
                selectInput("search_region",
                                   "Regional Council:",
                                   c("All",unique(as.character(school_data_df$Regional_Council))))
                ),
                
                column(4,
                       
                       selectInput("search_territorial_authority",
                                   "Territorial Authority:",
                                   c("All",unique(as.character(school_data_df$Territorial_Authority))))
                       ),
                column(4,
                       selectInput("search_subburb",
                                   "Subburb/Area:",
                                   c("All",unique(as.character(school_data_df$Area_Unit)))))
              ),
              
              
              fluidRow(
                column(4,
                       selectInput("search_school_type",
                                   "School Type:",
                                   c("All",unique(as.character(school_data_df$Org_Type))))
                ),
                
                column(4,
                       
                       selectInput("search_school_authority",
                                   "School Authority:",
                                   c("All",unique(as.character(school_data_df$Authority))))
                ),
                column(4,
                       selectInput("search_school_gender",
                                   "School Gender:",
                                   c("All",unique(as.character(school_data_df$CoEd_Status)))))
              ),
              
              fluidRow(
                
                column(6,sliderInput("decile_range", width = '80%', label = h3("Decile Number Range:"), min = 1, 
                                                 max = 10, value = c(1, 10))),
                
                column(6,sliderInput("isolation_range", width = '80%', label = h3("Isolation Index Range:"), min = min(school_data_df$Isolation_Index,na.rm = TRUE), 
                                            max = max(school_data_df$Isolation_Index,na.rm = TRUE), 
                                            value = c(min(school_data_df$Isolation_Index,na.rm = TRUE), max(school_data_df$Isolation_Index,na.rm = TRUE)))   
                       
                       
                       
                )
                
                
                ),
              
              h2('School Search Results (Click and Select a Row to View Detailed Information about the School)'),
              
              DT::dataTableOutput("search_results_table")
              
              
     #         fluidRow(
              
    #          column(6,
              
    #          DT::dataTableOutput("search_results_table"),
    #          ),
              
    #          column(6,
             
     #         leafletOutput('school_map',height = 500)
     #         )
    #          )
              
              #mapviewOutput("school_map", width = "100%", height = 400)
              
              ),
      #define compare tab
      tabItem(tabName = 'comparison',
              h2('Compare schools'),
              selectizeInput("selected_schools", "Select at least two schools to compare:", 
                             choices = unique(as.character(school_data_df$Org_Name)), multiple = TRUE, width = "100%"),
              actionButton("compare_button", "Compare Selected Schools"),
              
              conditionalPanel(
                condition = ("input.compare_button > 0"),
              #  DT::dataTableOutput("selected_schools_table"),
              
              
              fluidRow(
                
                column(6,
                highchartOutput("school_compare_chart1")
                ),
                column(6,
                highchartOutput("school_compare_chart2")
                )
              ),
              
              highchartOutput("school_compare_chart3")
              
              
              )
              
              
              )
    )
    
  )
)

server <- function(input, output,session) {
  
  
  school_search_results <- reactive({
    
    if(input$search_region == "All") {
      
      school_search_results <- school_data_df 
      
    } else {
      
      if(input$search_territorial_authority == "All") {
        
        school_search_results <- school_data_df %>% filter (Regional_Council == input$search_region)
        
      }
        
        else {
          
          
          if(input$search_subburb == "All"){
            
            school_search_results <- school_data_df %>% filter (Regional_Council == input$search_region) %>%
              filter (Territorial_Authority == input$search_territorial_authority)
            
          } else {
            
            school_search_results <- school_data_df %>% filter (Regional_Council == input$search_region) %>%
              filter (Territorial_Authority == input$search_territorial_authority) %>%
              filter (Area_Unit == input$search_subburb) 
              
            
          }
          
        }
      
    }
    
  
    if(input$search_school_type != "All") {
      
      school_search_results <- school_search_results %>% filter (Org_Type == input$search_school_type)
      
    }
    
    
    if(input$search_school_authority != "All") {
      
      school_search_results <- school_search_results %>% filter (Authority == input$search_school_authority)
      
    }
    
    if(input$search_school_gender != "All") {
      
      school_search_results <- school_search_results %>% filter (CoEd_Status == input$search_school_gender)
      
    }
    
    
    school_search_results <- school_search_results %>% filter (Decile >= input$decile_range[1] & Decile <= input$decile_range[2])
    
    
    school_search_results <- school_search_results %>% filter (Isolation_Index >= input$isolation_range[1] & Decile <= input$isolation_range[2])
    
    
    return(school_search_results)
    
    
    
  })
  
  
  
  
  observeEvent (input$search_region, {
    
    school_data_df <- school_data_df %>% filter (Regional_Council == input$search_region)
    
    updateSelectInput(session, "search_territorial_authority","Territorial Authority:",
                      c("All",unique(as.character(school_data_df$Territorial_Authority))))
    
    
    updateSelectInput(session, "search_school_type","School Type:",
                      c("All",unique(as.character(school_data_df$Org_Type))))
    
    updateSelectInput(session, "search_school_authority",
                      "School Authority:",
                      c("All",unique(as.character(school_data_df$Authority))))
    
    updateSelectInput(session, "search_school_gender",
                      "School Gender:",
                      c("All",unique(as.character(school_data_df$CoEd_Status))))
    
      
     
    
   
   
    
    
    
  })
  
  
  
  
  
 
  observeEvent (input$search_territorial_authority, {
    
    school_data_df <- school_data_df %>% filter (Regional_Council == input$search_region) %>% 
                                          filter (Territorial_Authority == input$search_territorial_authority)
    
    updateSelectInput(session, "search_subburb","Area/Subburb:",
                      c("All",unique(as.character(school_data_df$Area_Unit))))
    
    
    updateSelectInput(session, "search_school_type","School Type:",
                      c("All",unique(as.character(school_data_df$Org_Type))))
    
    updateSelectInput(session, "search_school_authority",
                      "School Authority:",
                      c("All",unique(as.character(school_data_df$Authority))))
    
    updateSelectInput(session, "search_school_gender",
                      "School Gender:",
                      c("All",unique(as.character(school_data_df$CoEd_Status))))
    
    
    
    
  })
  
  
  observeEvent (input$search_subburb, {
    
    school_data_df <- school_data_df %>% filter (Regional_Council == input$search_region) %>% 
      filter (Territorial_Authority == input$search_territorial_authority) %>% 
      filter (Area_Unit == input$search_subburb)
    
    
    updateSelectInput(session, "search_school_type","School Type:",
                      c("All",unique(as.character(school_data_df$Org_Type))))
    
    updateSelectInput(session, "search_school_authority",
                      "School Authority:",
                      c("All",unique(as.character(school_data_df$Authority))))
    
    updateSelectInput(session, "search_school_gender",
                      "School Gender:",
                      c("All",unique(as.character(school_data_df$CoEd_Status))))
    
    
    
  })
  
  
  
  
  output$infobox1 <- renderInfoBox({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    total_number_of_schools <- nrow(school_data_df)
    
    infoBox(
      "Total Number of Schools",
      total_number_of_schools,
      icon = icon("school")
    )
    
    
  })
  
  output$infobox2 <- renderInfoBox({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    total_number_of_students <- sum(as.numeric(school_data_df$Total))
    
    
    infoBox(
      "Total Number of Enrollments",
      total_number_of_students,
      color = "green",
      icon = icon("grin")
    )
    
    
  })
  
  
  
  
  
  output$infobox3 <- renderInfoBox({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    average_decile_number <- round(mean(as.numeric(school_data_df$Decile),na.rm = TRUE),digits = 2)
    
    infoBox(
      "Average Decile Number",
      average_decile_number,
      color = "purple",
      icon = icon("user-graduate")
    )
    
    
  })
  
  output$infobox4 <- renderInfoBox({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    average_isolation_index <- round(mean(as.numeric(school_data_df$Isolation_Index),na.rm = TRUE),digits = 2)
    
    infoBox(
      "Average Isolation Index",
      average_isolation_index,
      color = "orange",
      icon = icon("object-ungroup")
    )
    
    
  })
  
  
  output$graph1 <- renderHighchart({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    graph_data <- school_data_df %>% group_by(Org_Type) %>% summarise(school_number = n()) %>% arrange(desc(school_number))
    
    
    
    highchart() %>% 
      # Data
      hc_add_series(graph_data, "column", hcaes(x = Org_Type, y = school_number,name = "school_number")) %>%
     
      # Optiosn for each type of series
      hc_plotOptions(
        series = list(
          showInLegend = FALSE
        ),
        column = list(
          colorByPoint = TRUE
        )) %>%
      # Axis
      hc_yAxis(
        title = list(text = "Number of Schools"),
        type = "logarithmic"
      ) %>% 
      hc_xAxis(categories = graph_data$Org_Type ) %>%
      # Titles and credits
      hc_title(
        text = "Distributions of Schools"
      ) %>%
      hc_subtitle(text = "by Organisation Type") %>% 
      hc_credits(
        enabled = TRUE, text = "Source: EducationCounts",
        href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
        style = list(fontSize = "12px")
      )
    
    
    
    
  })
  
  
  output$graph2 <- renderHighchart({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    graph_data <- school_data_df %>% group_by(Org_Type) %>% summarise(student_number = sum(as.numeric(Total))) %>% arrange(desc(student_number))
    
    
    
    highchart() %>% 
      # Data
      hc_add_series(graph_data, "column", hcaes(x = Org_Type, y = student_number,name = "student_number")) %>%
      
      # Optiosn for each type of series
      hc_plotOptions(
        series = list(
          showInLegend = FALSE
        ),
        column = list(
          colorByPoint = TRUE
        )) %>%
      # Axis
      hc_yAxis(
        title = list(text = "Number of Schools"),
        type = "logarithmic"
      ) %>% 
      hc_xAxis(categories = graph_data$Org_Type ) %>%
      # Titles and credits
      hc_title(
        text = "Distributions of Student Enrollments"
      ) %>%
      hc_subtitle(text = "by Organisation Type") %>% 
      hc_credits(
        enabled = TRUE, text = "Source: EducationCounts",
        href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
        style = list(fontSize = "12px")
      )
    
    
    
    
  })
  
  
  
  output$graph3 <- renderHighchart({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    graph_data <- school_data_df %>% group_by(Urban_Area) %>% summarise(school_number = n()) %>% arrange(desc(school_number))
    
    
    
    highchart() %>% 
      # Data
      hc_add_series(graph_data, "column", hcaes(x = Urban_Area, y = school_number,name = "school_number")) %>%
      
      # Optiosn for each type of series
      hc_plotOptions(
        series = list(
          showInLegend = FALSE
        ),
        column = list(
          colorByPoint = TRUE
        )) %>%
      # Axis
      hc_yAxis(
        title = list(text = "Number of Schools"),
        type = "logarithmic"
      ) %>% 
      hc_xAxis(categories = graph_data$Urban_Area) %>%
      # Titles and credits
      hc_title(
        text = "Distributions of Schools"
      ) %>%
      hc_subtitle(text = "by Urban Area") %>% 
      hc_credits(
        enabled = TRUE, text = "Source: EducationCounts",
        href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
        style = list(fontSize = "12px")
      )
    
    
    
    
  })
  
  
  output$graph4 <- renderHighchart({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    graph_data <- school_data_df %>% group_by(Urban_Area) %>% summarise(student_number = sum(as.numeric(Total))) %>% arrange(desc(student_number))
    
    
    
    highchart() %>% 
      # Data
      hc_add_series(graph_data, "column", hcaes(x = Urban_Area, y = student_number,name = "student_number")) %>%
      
      # Optiosn for each type of series
      hc_plotOptions(
        series = list(
          showInLegend = FALSE
        ),
        column = list(
          colorByPoint = TRUE
        )) %>%
      # Axis
      hc_yAxis(
        title = list(text = "Number of Schools"),
        type = "logarithmic"
      ) %>% 
      hc_xAxis(categories = graph_data$Urban_Area ) %>%
      # Titles and credits
      hc_title(
        text = "Distributions of Student Enrollments"
      ) %>%
      hc_subtitle(text = "by Urban Area") %>% 
      hc_credits(
        enabled = TRUE, text = "Source: EducationCounts",
        href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
        style = list(fontSize = "12px")
      )
    
    
    
    
  })
  
  
  output$graph5 <- renderHighchart({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    graph_data <- school_data_df %>% group_by(CoEd_Status) %>% summarise(school_number = n()) %>% arrange(desc(school_number))
    
    
    
    highchart() %>% 
      # Data
      hc_add_series(graph_data, "column", hcaes(x = CoEd_Status, y = school_number,name = "school_number")) %>%
      
      # Optiosn for each type of series
      hc_plotOptions(
        series = list(
          showInLegend = FALSE
        ),
        column = list(
          colorByPoint = TRUE
        )) %>%
      # Axis
      hc_yAxis(
        title = list(text = "Number of Schools"),
        type = "logarithmic"
      ) %>% 
      hc_xAxis(categories = graph_data$CoEd_Status) %>%
      # Titles and credits
      hc_title(
        text = "Distributions of Schools"
      ) %>%
      hc_subtitle(text = "by CoEd Status") %>% 
      hc_credits(
        enabled = TRUE, text = "Source: EducationCounts",
        href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
        style = list(fontSize = "12px")
      )
    
    
    
    
  })
  
  
  output$graph6 <- renderHighchart({
    
    if (input$region=="All"){
      
      school_data_df <- school_data_df
      
    }
    else{
      
      school_data_df <- school_data_df %>% filter (Education_Region == input$region)
      
      
    }
    
    graph_data <- school_data_df %>% group_by(CoEd_Status) %>% summarise(student_number = sum(as.numeric(Total))) %>% arrange(desc(student_number))
    
    
    
    highchart() %>% 
      # Data
      hc_add_series(graph_data, "column", hcaes(x = CoEd_Status, y = student_number,name = "student_number")) %>%
      
      # Optiosn for each type of series
      hc_plotOptions(
        series = list(
          showInLegend = FALSE
        ),
        column = list(
          colorByPoint = TRUE
        )) %>%
      # Axis
      hc_yAxis(
        title = list(text = "Number of Schools"),
        type = "logarithmic"
      ) %>% 
      hc_xAxis(categories = graph_data$CoEd_Status) %>%
      # Titles and credits
      hc_title(
        text = "Distributions of Student Enrollments"
      ) %>%
     
      hc_subtitle(text = "by CoEd Status") %>% 
      hc_credits(
        enabled = TRUE, text = "Source: EducationCounts",
        href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
        style = list(fontSize = "12px")
      )
    
    
    
    
  })
  
  
  output$search_results_table <- renderDT({
    
    table_data <- school_search_results()
    
    table_data <- table_data[,c(44,22,3,18,11,38,39,34,31,4,5,10,30,42,24)]
    
    DT::datatable(table_data, selection = 'single', rownames= FALSE,options = list(scrollX = TRUE,stateSave = TRUE,
                                       pageLength = 5, 
                                       searching = TRUE,
                                       autoWidth = TRUE))
    
    
  })
  
  
  
  
  observeEvent(input$search_results_table_rows_selected, {
    
    table_data <- school_search_results()
    
    modal_data <-  table_data[input$search_results_table_rows_selected,]
    
    showModal(modalDialog(
      title = "Selected School Information:",
      size = "l",
      easyClose = TRUE,
      fluidRow(
        
        #infoBoxOutput('infobox11', width = 3),
        infoBoxOutput('infobox21', width = 4),
        infoBoxOutput('infobox31', width = 4),
        infoBoxOutput('infobox41', width = 4)
        
      ),
      
      hr(),
      br(),
      
      leafletOutput('school_map',height = 500),
      
      hr(),
      br(),
      
      highchartOutput("school_plot1")
      
    ))
    
    
    
    
    output$infobox21 <- renderInfoBox({
      
      
      infoBox(
        "No of Enrollments",
        modal_data$Total,
        color = "green",
        icon = icon("grin")
      )
      
      
      
      
    })
    
    
    
    
    
    output$infobox31 <- renderInfoBox({
      
      
      infoBox(
        "Decile Number",
        modal_data$Decile,
        color = "purple",
        icon = icon("user-graduate")
      )
      
      
    })
    
    output$infobox41 <- renderInfoBox({
      
      
      infoBox(
        "Isolation Index",
        modal_data$Isolation_Index,
        color = "orange",
        icon = icon("object-ungroup")
      )
      
      
    })
    
    
    output$school_map <- renderLeaflet({
      
      data <- modal_data
      
      bb1 <- min(data$Latitude,na.rm = TRUE)
      bb2 <- min(data$Longitude,na.rm = TRUE)
      bb3 <- max(data$Latitude,na.rm = TRUE)
      bb4 <- max(data$Longitude,na.rm = TRUE)
      
      
      popup = paste0("<b>School Name: </b>",   data$Org_Name, "<br>",
                     "<b>School Type: </b>",data$Org_Type, "<br>",
                     "<b>Authority: </b>",data$Authority, "<br>",
                     "<b>School Address: </b>",data$Add2_Line1, "<br>",
                     "<b>Telephone: </b>",data$Telephone, "<br>",
                     "<b>Email: </b>",data$Email, "<br>",
                  #   "<b>Website: </b>",data$URL, "<br>",
                     "<b>Website:</b>",paste0("<a href=",data$URL,">",data$URL,"</a>"),"<br>",
                     "<b>Contact Name: </b>", data$Contact1_Name)
      
      leaflet(data = data) %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>%
        addFullscreenControl(pseudoFullscreen = TRUE) %>%
        addSearchOSM() %>%
        addScaleBar() %>%
        addMeasure(primaryLengthUnit = "meters",
                   secondaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters",
                   secondaryAreaUnit = "hectares") %>%
        addMiniMap() %>%
        
        addMarkers(~Longitude, ~Latitude, popup = popup) %>%
        
        fitBounds(lat1 = bb1-0.002, lng1 = bb2-0.002, lat2 = bb3+0.002, lng2 = bb4+0.002) %>% 
        
        # Layers control
        addLayersControl(
          baseGroups = c("Esri WorldImagery", "OSM (default)", "Toner", "Toner Lite"),
          #overlayGroups = c("Work Activity","Closure Type", "OD Load","Network Boundary"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      
    })
    
    
    output$school_plot1 <- renderHighchart({ 
      
      data <- modal_data
      
      
      
      data <- data[,c(17,2,7,40,41,6,9)]
      
      data <- t(data)
      
      data <- as.numeric(data)
      
      
      hc <- highchart() %>%
        hc_title(text = "Student Distribution") %>% 
     #   hc_subtitle(text = modal_data$Org_Name) %>% 
        hc_chart(type = "column") %>% 
        hc_xAxis(categories = c("European","Maori","Pacific","Asian","MELAA","Other","International")) %>% 
        hc_add_series(name = "Ethnic Group", data = data) %>% 
      
        hc_plotOptions(
        series = list(
          showInLegend = TRUE
        ),
        column = list(
          colorByPoint = TRUE
        )) 
      
      hc
      
      
      
      })
    
  })
  
  
  observeEvent(input$selected_schools, {
    
    
    
    
    
    
    output$school_compare_chart1 <- renderHighchart({
      
      graph_data <- school_data_df[school_data_df$Org_Name %in% input$selected_schools,]
      
      highchart() %>% 
        # Data
        hc_add_series(graph_data, "column", hcaes(x = Org_Name, y = Decile,name = "Decile")) %>%
        
        # Optiosn for each type of series
        hc_plotOptions(
          series = list(
            showInLegend = FALSE
          ),
          column = list(
            colorByPoint = TRUE
          )) %>%
        # Axis
        hc_yAxis(
          title = list(text = "Decile Number")
        ) %>% 
        hc_xAxis(categories = graph_data$Org_Name ) %>%
        # Titles and credits
        hc_title(
          text = "Compare Schools"
        ) %>%
        hc_subtitle(text = "by Decile Number") %>% 
        hc_credits(
          enabled = TRUE, text = "Source: EducationCounts",
          href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
          style = list(fontSize = "12px")
        )
      
      
      
      
    })
    
    
    output$school_compare_chart2 <- renderHighchart({
      
      graph_data <- school_data_df[school_data_df$Org_Name %in% input$selected_schools,]
      
       highchart() %>% 
        # Data
        hc_add_series(graph_data, "column", hcaes(x = Org_Name, y = Isolation_Index,name = "Isolation_Index")) %>%
        
        # Optiosn for each type of series
        hc_plotOptions(
          series = list(
            showInLegend = FALSE
          ),
          column = list(
            colorByPoint = TRUE
          )) %>%
        # Axis
        hc_yAxis(
          title = list(text = "Isolation Index")
        ) %>% 
        hc_xAxis(categories = graph_data$Org_Name) %>%
        # Titles and credits
        hc_title(
          text = "Compare Schools"
        ) %>%
        hc_subtitle(text = "by Isolation Index") %>% 
        hc_credits(
          enabled = TRUE, text = "Source: EducationCounts",
          href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
          style = list(fontSize = "12px")
        )
      
      
    })
    
    
    
    output$school_compare_chart3 <- renderHighchart({
      
      data <- school_data_df[school_data_df$Org_Name %in% input$selected_schools,]
      
      data$European <- as.numeric(data$European)
      data[,2] <- as.numeric(data[,2])
      data$Pacific <- as.numeric(data$Pacific)
      data$Asian <- as.numeric(data$Asian)
      data$MELAA <- as.numeric(data$MELAA)
      data$Other <- as.numeric(data$Other)
      data$International <- as.numeric(data$International)
      
      
      data <- data[,c(22,17,2,7,40,41,6,9)]
      
     
      
      
      
      hc <- highchart() %>% 
        hc_chart(type = "column") %>%
        hc_plotOptions(column = list(stacking = "normal"))%>% 
        hc_xAxis(categories = data$Org_Name) %>% 
        hc_add_series(name = "European", data = data$European) %>% 
        hc_add_series(name = "Maori", data = data$Maori) %>% 
        hc_add_series(name = "Pacific", data = data$Pacific) %>% 
        hc_add_series(name = "Asian", data = data$Asian) %>% 
        hc_add_series(name = "MELAA", data = data$MELAA) %>% 
        hc_add_series(name = "Other", data = data$Other) %>% 
        hc_add_series(name = "International", data = data$International) %>%
        # Axis
        hc_yAxis(
          title = list(text = "No of Students")
        ) %>% 
        
        # Titles and credits
        hc_title(
          text = "Compare Schools"
        ) %>%
        hc_subtitle(text = "by Student Distribution") %>% 
        hc_credits(
          enabled = TRUE, text = "Source: EducationCounts",
          href = "https://www.educationcounts.govt.nz/data-services/directories/list-of-nz-schools",
          style = list(fontSize = "12px")
        )
        
      hc
      
    
      
    })
    
    
    output$selected_schools_table <- renderDT({
      
     # table_data <- school_data_df %>% filter(Org_Name==input$selected_schools)
      
      table_data <- school_data_df[school_data_df$Org_Name %in% input$selected_schools,]
      
    
      
      
      
      #table_data <- table_data[,c(44,22,3,18,11,38,39,34,31,4)]
      
      DT::datatable(table_data, selection = 'single', rownames= FALSE,options = list(scrollX = TRUE,stateSave = TRUE,
                                                                                     pageLength = 5, 
                                                                                     searching = TRUE,
                                                                                     autoWidth = TRUE,
                                                                                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ))
      
    })
    
    
  })
  
  
 
  
  
  

}
# Run the application 
shinyApp(ui = ui, server = server)

               