#Shiny ICCS databases

library(shiny)
library(ggplot2)
library(dplyr)

dir <- "C:/Users/pamel/OneDrive - KU Leuven/Master in Statistics/Master Thesis/DataAnalysis/"
load(file = paste0(dir,"Codebook.RData"))
Countries <- read_xlsx(paste0(dir,"Metadata.xlsx"), sheet = "CNT")
Files <- read_xlsx(paste0(dir,"Metadata.xlsx"), sheet = "Files")
Countries$year4 <- ifelse(Countries$year == "99", paste0("19",Countries$year), paste0("20",Countries$year))


#ui.R
ui <- fluidPage(


  titlePanel("Shiny summary ICCS questionnaires"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "year",
                  label = "Choose a cycle:",
                  choices = c(sort(unique(Countries$year4))),
                  selected = "2016"),
      uiOutput("datasetSelection"),
      uiOutput("countrySelection")),
    
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("func"),
      tableOutput("view")
    )
  )
)

#server.R
server <- function(input, output){
  p1 <- reactive({
      
    if (input$year == "2016") {
      p1 <- VarsToUse %>% 
        filter(!is.na(VariableC3)) 
    } else  if (input$year == "2009") {
      p1 <- VarsToUse %>% 
        filter(!is.na(VariableC2)) 
    } else  if (input$year == "1999") {
      p1 <- VarsToUse %>% 
        filter(!is.na(VariableC1)) 
    } else {
      p1 <- VarsToUse %>% 
        filter(!is.na(VariableC3)) 
    }
  })
  output$datasetSelection <- renderUI({
  selectInput(inputId = "dataset",
              label = "Choose a questionnaire:",
              choices = c(sort(unique(p1()$Dataset))),
              selected = c(sort(unique(p1()$Dataset))))
  }) 
  
  output$countrySelection <- renderUI({
    checkboxGroupInput(inputId = "cnt", 
                       label = "Country",
                       choices = eval(parse(text = as.character(Countries[Countries$year4 == input$year,"Country"]))),
                       selected = eval(parse(text = as.character(Countries[Countries$year4 == input$year,"Country"]))))
  })
  
  output$func <-  renderPlot({
  
    
    p <- ggplot(p1(), aes(x = Domain, color = Dataset, fill = Dataset)) +
      geom_bar() +
      facet_grid(. ~ Dataset) +
      labs(title = "Description of questionnaire composition", x = "Domain", y = "Number of columns") +
      theme(axis.text.x = element_text(angle = 90), legend.position = "none") 
    p
  })
  
  output$view <- renderTable({
 
    mu <- p1()  %>% filter(Dataset == input$dataset) %>%  group_by(Dataset, Domain) %>% summarise(Nvar = n())
    mu
  })
}



shinyApp(ui = ui, server = server)