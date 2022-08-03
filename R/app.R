
#' app
#' @keywords internal
#' @noRd


library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)



# Define UI for application that draws a histogram
ui <- fluidPage(


    # Application title
  titlePanel("U.S. Presidential Speech Ngram Viewer"),

  tabsetPanel(
    tabPanel("Ngram Viewer",
    mainPanel(selectizeInput('terms',label = NULL, choices = NULL, multiple = TRUE),
              width = 12),


    fixedRow(
      column(2,
        checkboxInput("byparty","Trends by party", value = TRUE)
        ),
      # column(2,checkboxGroupInput("multipanel","Multi-panel display",
      #                     choices = c("Separate by ngram","Separate by party")
      #        )),
      column(2,
        checkboxInput("facet_ngram","Separate by ngram", value = FALSE)
        ),
      column(2,
        checkboxInput("facet_party","Separate by party", value = FALSE)
        ),
      column(2,
             sliderInput("years",NULL,min = 1952, max = 2020, value = c(1952,2020), step = 4, round = FALSE, sep = "")),
      column(2,NULL),
      column(2,NULL)

    ),

       mainPanel(
           plotOutput("plot"),
           width = 12
        )
  ),
  tabPanel("About",
           mainPanel(
             textOutput("about"),
             width = 12
           )

  )
  )

)


server <- function(input, output, session) {

  #source("functions.R")

  choices <-
    ngrams_overall %>%
    ungroup() %>%
    distinct(word, ngram) %>%
    mutate(order = case_when(
      ngram == "unigram" ~ 1,
      ngram == "bigram" ~ 2,
      ngram == "trigram" ~3
    )) %>%
    arrange(order) %>%
    dplyr::select(word)


      updateSelectizeInput(session, "terms", label = NULL, choices = choices$word, server = TRUE, selected = c("economy","the people"))
      output$plot <- renderPlot({
      words <- input$terms

      speechesr:::plot_ngrams(terms = words, byparty = input$byparty, facet_ngram = input$facet_ngram, facet_party = input$facet_party, years = input$years)


    })
      output$about <- renderText({
        "The U.S. Presidential Speech Ngram Viewer displays frequencies of ngrams (unigrams, bigrams, and trigrams are possible) from U.S. Presidential Campaign speeches given by major party candidates between 1952 and 2020. Start typing an ngram in the search box, and possible selections will appear. The data have been limited to ngrams that are not made up entirely of stopwords (i.e., common words such as 'the', 'in', 'but', etc.).

        The frequencies display a given ngram's usage as a percent of all corresponding ngrams in a given year, either overall or by a given candidate. For example, an overall frequency of .05% in the year 2016 for the unigram 'country' indicates that .05% of all unigrams used in 2016 campaign speeches were 'country'. As another example: if the frequency of the bigram 'the people' is .05% for the Democrat in 2008 (i.e., Barack Obama), that would indicate that .05% of all bigrams used by Barack Obama in 2008 were 'the people'.

        There are several options for viewing frequencies of ngrams. The 'Trends by party' option displays ngram trends disaggregated by party (unchecked displays overall frequencies in each election). If multiple ngrams are entered, the 'Separate by ngram' option will display each ngram in a separate panel. Likewise, if 'Trends by party' is selected, the 'Separate by party' will display the results of each party in a separate panel."

      })


}

# Run the application
shinyApp(ui = ui, server = server)


