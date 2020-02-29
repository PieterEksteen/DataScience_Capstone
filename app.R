library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(fst)
library(stringr)
library(tictoc)
library(shinydashboard)
library(wordcloud2)

source('mod_word_prediction.R')

# Define UI ----
ui <- fluidPage(
    div(id="back",tags$b("Developed by Pieter Eksteen")),
    tags$head(tags$script(src = "enter_button.js")), 
    tags$style(type="text/css", "#back {position:absolute;bottom:0;color:#3063A5;
                   max-width: 100%; width: 100%;}"),
    h1(id="big-heading", strong("Next Word Prediction - Coursera Capstone Project"), align = "center"),
    tags$style(HTML("#big-heading{color: #3063A5;}")),
    setBackgroundImage(src = 'wordcloud_background.png', shinydashboard = FALSE),
    mainPanel(
        br(),
        br(),
        br(),
        br(),
        fluidRow(column(12, align="center", offset = 3,
                        textInput("string", label=h5(strong('Input text below and hit "Enter" or click the Predict button:')),value = "", width = "80%"),
                        tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"),
                        actionBttn('predict',
                                   'Predict',
                                   style = 'unite',
                                   color = 'primary'),
                        br(),
                        br(),
                        br(),
                        h5(strong('Predicted next word:')),
                        h1(textOutput('next_word')),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        plotlyOutput('probability_plot')
                        
        )
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    
    cat("\n")
    cat("Loading Prediction Table . . .")
    cat("\n")
    
    tic()
    word_prediction_lookup_table <- read.fst('word_prediction_lookup_table_final.fst')
    toc()
    
    cat("\n")
    cat("     Done")
    cat("\n")
    
    next_word_prediction <- eventReactive(input$predict, {
        
        cat("\n")
        cat("Calling Predict Next Word Function . . .")
        cat("\n")
        
        prediction <- predict_word(input$string, word_prediction_lookup_table)
        
    })
    
    output$next_word <- reactive({
        
        cat("\n")
        cat("Creating Next Word Output . . .")
        cat("\n")
        
        paste0('"', 
               next_word_prediction() %>%
                   head(1) %>%
                   .$next_word,
               '"')
    })
    
    generate_plot <- eventReactive(input$predict, {
        
        cat("\n")
        cat("Creating Plot . . .")
        cat("\n")
        
        g <- ggplot(next_word_prediction(), aes(x=reorder(next_word, -prob),
                                                y=prob)) + 
            geom_bar(stat='identity', fill="#3063A5")  +
            labs(title = "10 Most Probable Next Words",
                 x = 'Next Word',
                 y = 'Probability (%)') +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent", color = NA))
        
        
        plotly::ggplotly(g)
        
    })
    
    output$probability_plot <- renderPlotly({
        
        cat("\n")
        cat("Creating Plot Output . . .")
        cat("\n")
        
        generate_plot()
        
    })
    
}


# Run the app ----
shinyApp(ui = ui, server = server)