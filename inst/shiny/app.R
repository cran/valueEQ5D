library(shiny)
library(valueEQ5D)

##Section 1 ____________________________________________________
#load your data or create a data table as follows:
scoreData = read.table(
  text = " Score	Country	Method
  EQ-5D-3L	Argentina	VAS
  EQ-5D-3L	Belgium	VAS
  EQ-5D-3L	Denmark 	VAS
  EQ-5D-3L	Europe	VAS
  EQ-5D-3L	Finland	VAS
  EQ-5D-3L	Germany	VAS
  EQ-5D-3L	Malaysia	VAS
  EQ-5D-3L	NewZealand	VAS
  EQ-5D-3L	Slovenia	VAS
  EQ-5D-3L	Spain	VAS
  EQ-5D-3L	UK	VAS
  EQ-5D-3L	Argentina	TTO
  EQ-5D-3L	Australia	TTO
  EQ-5D-3L	Brazil	TTO
  EQ-5D-3L	Canada	TTO
  EQ-5D-3L	Chile	TTO
  EQ-5D-3L	China	TTO
  EQ-5D-3L	Denmark 	TTO
  EQ-5D-3L	France	TTO
  EQ-5D-3L	Germany	TTO
  EQ-5D-3L	Italy	TTO
  EQ-5D-3L	Japan	TTO
  EQ-5D-3L	Korea	TTO
  EQ-5D-3L	Malaysia	TTO
  EQ-5D-3L	Netherlands	TTO
  EQ-5D-3L	Poland	TTO
  EQ-5D-3L	Portugal	TTO
  EQ-5D-3L	Singapore	TTO
  EQ-5D-3L	Spain	TTO
  EQ-5D-3L	SriLanka	TTO
  EQ-5D-3L	Sweden	TTO
  EQ-5D-3L	Taiwan	TTO
  EQ-5D-3L	Thailand	TTO
  EQ-5D-3L	Trinidad_and_Tobago	TTO
  EQ-5D-3L	UK	TTO
  EQ-5D-3L	USA	TTO
  EQ-5D-3L	Zimbabwe	TTO
  EQ-5D-5L	Canada	VT
  EQ-5D-5L	China	VT
  EQ-5D-5L	England 	VT
  EQ-5D-5L	Ethopia 	VT
  EQ-5D-5L	France 	VT
  EQ-5D-5L	Germany	VT
  EQ-5D-5L	HongKong	VT
  EQ-5D-5L	Indonesia	VT
  EQ-5D-5L	Ireland	VT
  EQ-5D-5L	Japan	VT
  EQ-5D-5L	Korea	VT
  EQ-5D-5L	Malaysia	VT
  EQ-5D-5L	Netherlands	VT
  EQ-5D-5L	Poland 	VT
  EQ-5D-5L	Portugal 	VT
  EQ-5D-5L	Spain	VT
  EQ-5D-5L	Taiwan	VT
  EQ-5D-5L	Thailand	VT
  EQ-5D-5L	Uruguay	VT 
  EQ-5D-5L	USA	VT
  EQ-5D-5L	Vietnam	VT
  EQ-5D-5L	Denmark	CW
  EQ-5D-5L	France	CW
  EQ-5D-5L	Germany	CW
  EQ-5D-5L	Japan	CW
  EQ-5D-5L	Netherlands	CW
  EQ-5D-5L	Spain	CW
  EQ-5D-5L	Thailand	CW
  EQ-5D-5L	UK	CW
  EQ-5D-5L	USA	CW
  EQ-5D-5L	Zimbabwe	CW",
  header = TRUE, stringsAsFactors = FALSE)

## Only run examples in interactive R sessions
if (interactive()) {

  ui <- fluidPage(
    # App title ----
    titlePanel("EQ-5D scoring"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        checkboxInput("header", "Header", TRUE),
        htmlOutput("score_selector"),#add selectinput boxs
        htmlOutput("country_selector"),# from objects created in server
        htmlOutput("method_selector"),# from objects created in server

        textInput("col1", "Column name of EQ5D response to question 1 (Mobility) :", "MO"),
        textInput("col2", "Column name of EQ5D response to question 2 (Self care) :", "SC"),
        textInput("col3", "Column name of EQ5D response to question 3 (Usual activity):", "UA"),
        textInput("col4", "Column name of EQ5D response to question 4 (Pain) :", "PD"),
        textInput("col5", "Column name of EQ5D response to question 5 (Anxiety/Depression):", "AD"),
        
        checkboxInput("gendercriteria", "Gender Crtieria Inclusion"),
        conditionalPanel(
          condition = "input.gendercriteria == true",
          radioButtons("gender", label = h3("Choose the gender"),
                       choices = list("NA"="NA","Male" = "Male", "Female" = "Female"))
        ),
        
        checkboxInput("agecriteria", "Age Crtieria Inclusion"),
        conditionalPanel(
          condition = "input.agecriteria == true",
          # Input: Specification of range within
          sliderInput("agerange", "Age range:",
                      min = 0, max = 120,
                      value = c(0,120))
        ),
        # Button
        downloadButton("downloadData", "Download modified data")
  
      ),
      mainPanel(
         tabsetPanel(type = "tabs",
                    tabPanel("Data",tableOutput("contents")),
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("Summary", verbatimTextOutput("summary")),
                    tabPanel("Table", tableOutput("table")),
                    tabPanel("Frequency Table", tableOutput("freqtable")),
                    tabPanel("Modified data with calculated scores", tableOutput("mod.data"))
        )
      )
    )
  )

  server <- function(input, output) {
    
    output$score_selector = renderUI({ #creates State select box object called in ui
      selectInput(inputId = "Score", #name of input
                  label = "Score:", #label displayed in ui
                  choices = as.character(unique(scoreData$Score)),
                  # calls unique values from the State column in the previously created table
                  selected = "UK") #default choice (not required)
    })
    output$country_selector = renderUI({#creates County select box object called in ui
      data_available1 = scoreData[scoreData$Score == input$Score,"Country"]  

      #creates a reactive list of available counties based on the State selection made
      selectInput(inputId = "Country", #name of input
                  label = "Country:", #label displayed in ui
                  choices = unique(data_available1),#calls list of available countries
                  selected = unique(data_available1)[1])
    })
    output$method_selector = renderUI({#creates County select box object called in ui
      data_available1 = scoreData[scoreData$Score == input$Score,]  
      data_available = data_available1[data_available1$Country == input$Country,"Method"]  
      #creates a reactive list of available counties based on the State selection made
      
      selectInput(inputId = "Method", #name of input
                  label = "Method:", #label displayed in ui
                  choices = unique(data_available), #calls list of available counties
                  selected = unique(data_available)[2])
    })
    output$method_selector = renderUI({#creates County select box object called in ui
      data_available1 = scoreData[scoreData$Score == input$Score,]  
      data_available = data_available1[data_available1$Country == input$Country,"Method"]  
      #creates a reactive list of available counties based on the State selection made
      
      selectInput(inputId = "Method", #name of input
                  label = "Method:", #label displayed in ui
                  choices = unique(data_available), #calls list of available counties
                  selected = unique(data_available)[2])
    })
    datasetInput <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      eq5d.data<-read.csv(inFile$datapath, header = input$header)
    })

    output$contents <- renderTable({
      dataset <- datasetInput()
    })
    output$summary <- renderText({
      paste0('You have selected : Score as ', input$Score, ' Country as ', input$Country, ', age range as ',input$agerange [1], ' - ',input$agerange [2], ', and gender as ', input$gender)
    })
    
    doAnalysis<-reactive({
      dataset <- datasetInput()
      
      if(input$Score=="EQ-5D-5L"){
        if(input$Method=="CW"){
          result<-eq5dmap5Lto3L(dataset,input$col1,input$col2,input$col3, input$col4,input$col5,input$Country,input$Method,input$gender,c(input$agerange[1],input$agerange[2]))
        }else{
          result<-valueEQ5D5L(dataset,input$col1,input$col2,input$col3, input$col4,input$col5,input$Country,input$gender,c(input$agerange[1],input$agerange[2]))
        }
      }else{
        result<-valueEQ5D3L(dataset,input$col1,input$col2,input$col3, input$col4,input$col5,input$Country,input$Method,input$gender,c(input$agerange[1],input$agerange[2]))
        
      }
      return(result)
    })
    output$table <- renderTable({
      ans<-doAnalysis()
      ans$stats
    })

    output$freqtable <- renderTable({
       ans<-doAnalysis()
       ans$frequencyTable
    })

    output$plot <- renderPlot({
      ans<-doAnalysis()
      ans$histogram
    })
    output$mod.data <- renderTable({
      ans<-doAnalysis()
      ans$modifiedData
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$Score, "_",input$Country,"_",input$Method,".csv", sep = "")
      },
      content = function(file) {
        ans<-doAnalysis()
        write.csv(ans$modifiedData, file, row.names = FALSE)
      }
    )
    
  }
  shinyApp(ui, server)
}
