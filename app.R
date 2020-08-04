#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#To execute: 
#library(shiny)
#runApp('D:/Diego/Personal Projects/WebScCov-app/WebScCov')

#Check for needed packages
#and install the ones missing:

packages = c('shiny','rvest','TTR','plotly')
package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            #library(x, character.only = TRUE)
        }
    }
)


library(shiny)
library(rvest)
library(TTR)
library(plotly)


source("Web Scraping.R")

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("CoVid New Hospitalizations"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            helpText("Check Covid Information by State (Only states with
                     \"good\" data quality are included)"),
            selectInput('state',label='Select a State',
                        choices = list("Arkansas",
                                       "Colorado",
                                       "Florida",
                                       "Georgia",
                                       "Hawaii",
                                       "Idaho",
                                       "Indiana",
                                       "Kansas",
                                       "Kentucky",
                                       "Maine",
                                       "Maryland",
                                       "Massachusetts",
                                       "Minnesota",
                                       "Montana",
                                       "New Hampshire",
                                       "New Mexico",
                                       "New York",
                                       "North Dakota",
                                       "Ohio",
                                       "Oklahoma",
                                       "Oregon",
                                       "South Carolina",
                                       "South Dakota",
                                       "Tennessee",
                                       "Utah",
                                       "Virginia",
                                       "Wisconsin",
                                       "Wyoming"
                                       ), 
                        selected = "Florida"),
            h5(tags$ul(
                tags$li("You need to have R 4.0.2 and the shiny library installed."),
                tags$li("Other packages needed will be installed if not found."),
                tags$li("Information used is pulled from:",br(),
                        "https://covidtracking.com/data")))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("selected_state"), align="center"),
           plotlyOutput("lineplot")
           #this tableoutput is for debugging (to see what is the reactive
           #function bringing)
           tableOutput("test1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dataInput <- reactive({
        WebScraping(tolower(sub(" ","-",input$state)))
        })

    output$test1 <- renderTable({
        dataInput()})
    
    output$selected_state <- renderText({
                            paste("Hospitalizations in ",input$state)})
    
    output$lineplot <- renderPlotly({
        #plot(dataInput()$Date, dataInput()$daily, type = "l",
        #     main=paste('Hospitalizations in',input$state), 
        #     ylab='New Hospitalizations', xlab='Month',bty="l")
        #lines(dataInput()$Date,dataInput()$ma, col='red')
        #legend("topleft", 
        #       legend=c('New Hospitalizations','Moving Average (7)'),
        #       col=c('black','red'), lwd=2)
        
        plot <- plot_ly(dataInput(), x = ~Date, y = ~daily, type = 'scatter', 
                        mode = 'lines', name = 'Hospitalizations') %>% 
            add_trace(y = ~ma, name = 'Moving Average (7)')%>% 
            layout(colorway=c('dark blue','dark red'),yaxis=list(showline=TRUE))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
