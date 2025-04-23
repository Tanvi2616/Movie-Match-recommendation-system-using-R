library(shiny)
library(shinythemes)
library(ggplot2)

# Load the pre-processed data
new_df <- readRDS("movies.rds")
similarity_matrix <- readRDS("similarity_matrix.rds")

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("MovieMatch:Recommendation System"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Find Similar Movies"),
      selectizeInput(
        "movie_title",
        "Search for a movie:",
        choices = NULL,
        options = list(
          placeholder = 'Type to search...',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      actionButton("recommend", "Get Recommendations", class = "btn-primary"),
      br(), br(),
      h5("How it works:"),
      p("1. Type or select a movie from the dropdown"),
      p("2. Click the button to see recommendations"),
      p("3. Results are based on content similarity")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      width = 5,
      h3(textOutput("rec_title")),
      hr(),
      uiOutput("recommendations"),
      br(),
      plotOutput("similarity_plot")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Update selectize input with movie titles
  updateSelectizeInput(
    session,
    "movie_title",
    choices = sort(new_df$title),
    server = TRUE
  )
  
  # Reactive function to get recommendations
  recommendations <- eventReactive(input$recommend, {
    req(input$movie_title)
    
    movie_index <- which(new_df$title == input$movie_title)
    
    if (length(movie_index) == 0) {
      return(NULL)
    }
    
    similarities <- similarity_matrix[movie_index, ]
    top_indices <- order(similarities, decreasing = TRUE)[2:6] # Exclude self
    
    data.frame(
      Movie = new_df$title[top_indices],
      Similarity = round(similarities[top_indices], 3)
    )
  })
  
  # Output the recommendation title
  output$rec_title <- renderText({
    if (is.null(recommendations())) {
      "No movie selected or movie not found"
    } else {
      paste("Movies similar to:", input$movie_title)
    }
  })
  
  # Output the recommendations
  output$recommendations <- renderUI({
    recs <- recommendations()
    
    if (is.null(recs)) {
      return(p("Please select a movie from the dropdown and click the button."))
    }
    
    tagList(
      lapply(1:nrow(recs), function(i) {
        wellPanel(
          h4(paste0(i, ". ", recs$Movie[i])),
          p(paste("Similarity score:", recs$Similarity[i]))
        )
      })
    )
  })
  
  # Output the similarity plot
  output$similarity_plot <- renderPlot({
    recs <- recommendations()
    
    if (is.null(recs)) return(NULL)
    
    ggplot(recs, aes(x = reorder(Movie, Similarity), y = Similarity)) +
      geom_col(fill = "#3498db") +
      coord_flip() +
      labs(x = "", y = "Similarity Score", 
           title = "Recommendation Similarity Scores") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the application
shinyApp(ui = ui, server = server)