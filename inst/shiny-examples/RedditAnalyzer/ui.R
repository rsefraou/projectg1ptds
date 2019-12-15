# Load all the required libraries for the package, display them in an alphabetical order


# Define UI for our application, we decided to use a navbar at the top
shiny::shinyUI(shiny::navbarPage(
  inverse = TRUE,
  "Reddit Analyser",

# First Page - Intro, where we present the project
  shiny::tabPanel(
    "Welcome!",
    # the theme we chose for our application
    shiny::includeCSS("style.css"),
    #  includeCSS("box.css"),
    #  includeScript("main.js"),
    shiny::fluidPage(
      h1("Welcome to our Reddit analysing tool!"),
      br(),
      br(),
      p(
        "During the last semester of our master, we had the pleasure to attend the ",
        a("Programming Tools in Data Science", href = "https://ptds.netlify.com/"),
        "course. This shiny app was made in this context and we hope you will appreciate it!"
      ),
      br(),
      br(),
      div(img(src = "reddit.png", height = "20%", width = "20%"), style="text-align: center;"),
      br(),
      br(),
      br(),
      div(p(strong(
          "Built by Rita Sefraoui Tahiri, Leonard Philippossian, Alexandre Schroeter, Simon Fornerod and David Pitteloud using RStudio."
        ),
        style = "text-align: right;",
      )),

    )
  ),

  # Second Page  - Downloads page where user may download db
  shiny::navbarMenu(
    "Downloads",
    #first subsection of the section downloads, to target reddit content
    shiny::tabPanel("Reddit content",
             shiny::fluidPage(
               h3("Here is where you may download data directly from the reddit API"),
               br(),
               shiny::sidebarLayout(
                 position = "right",
                 shiny::sidebarPanel(
                   style = "background: rgba(255, 255, 255, 0)",
                   shiny::wellPanel(
                     style = "background: rgba(192, 57, 43, 0.5)",
                     # different parameters that define the scrapping
                     shiny::textInput("search", "What are you interested in?", value = "Greta"),
                     shiny::textInput("subreddit", "Select an existing subreddit", value = "news"),
                     shiny::selectInput(
                       "sortby",
                       "How do you want to sort your data by?",
                       choices = c(
                         "Comments" = "comments",
                         "New" = "new",
                         "Relevance" = "relevance",
                         "Top" = "top",
                         "Front page" = "front_page"
                       ),
                       selected = "Comments"
                     ),

                     shiny::selectInput(
                       "timeframe",
                       "Select a time frame",
                       choices = c(
                         "Day" = "day",
                         "Week" = "week",
                         "Month" = "month",
                         "Year" = "year",
                         "All" = "all"
                       ),
                       selected = "week"
                     ),

                     shiny::actionButton(class="btn", "load1", "Download"),
                     shiny::actionButton(class="btn","display1", "Show table")

                   )
                 ),
                 shiny::mainPanel(shiny::uiOutput("id1"),

                           withSpinner(DT::dataTableOutput("download1",width = "auto"), type = 4, color="#c0392b")
                 ))

             )),
    # second subsection of the section downloads, to gather user information
    shiny::tabPanel("Stalking?",
              shiny::fluidPage(
               br(),
               br(),
               h3("Welcome to the second downloading option!!"),
               br(),
               shiny::sidebarLayout(
                 position = "right",
                 shiny::sidebarPanel(
                   style = "background: rgba(255, 255, 255, 0)",
                   shiny::wellPanel(
                     style = "background: rgba(192, 57, 43, 0.5)",
                                # Just enter the username to scrap its info
                     shiny::textInput("user", "Enter a username"),

                     shiny::actionButton(class="btn","load2", "Stalk!"),
                     shiny::actionButton(class="btn","display2", "Show table")

                              )),
                 shiny::mainPanel(shiny::uiOutput("id2"),

                      shinycssloaders::withSpinner(DT::dataTableOutput("download2",width = "auto"), type = 4, color="#c0392b")
                 ))
             ))
  ),




  # Third Page  - Main section where the content of reddit will be analysed
shiny::tabPanel("Inside Reddit",
            shiny::fluidPage(
             h3("Here are different possible analysis on the content"),
             p(
               "Each possible section provides a different analytical perspective of Reddit Content!"
             ),
             shiny::sidebarLayout(
               position = "right",
               shiny::sidebarPanel(
                 style = "background: rgba(255, 255, 255, 0)",


                 shiny::wellPanel(
                   style = "background: rgba(192, 57, 43, 0.5)",
                   shiny::fileInput("file", NULL, accept = ".csv"),
                   # Different options to choose what analysis should be displayed
                   shiny::radioButtons(
                     "col",
                     "Switch Plot",
                     choices =  c("Where is it trending?", "Who is the most popular?", "Gallery of pictures","Wordcloud","Sentiment analysis"),
                     selected = "Where is it trending?"

                   ),
                   # Supplementary output shown only if the mentionned parameters is selected
                   shiny::conditionalPanel("input.col == 'Sentiment analysis'",
                          shinycssloaders::withSpinner(DT::dataTableOutput("counttable"), type = 4,color="#c0392b"))


               )),


              shiny::mainPanel(
                # Wordcloud output
                shiny::conditionalPanel(
                 "input.col == 'Wordcloud'",
                 shinycssloaders::withSpinner(
                     wordcloud2::wordcloud2Output("wordcloud"), type = 4, color="#c0392b")
                 )

                 ,

                 # Sentiment analysis
                 shiny::conditionalPanel("input.col == 'Sentiment analysis'", shinycssloaders::withSpinner(
                   plotly::plotlyOutput("redditsentiment"),
                   type = 4,
                   color = "#c0392b"


                 )),
                 # A map that shows where the subject is most related to
                 shiny::conditionalPanel(
                   condition = "input.col == 'Where is it trending?'",
                   shinycssloaders::withSpinner(leaflet::leafletOutput("mymap"), type = 4, color = "#c0392b")
                 ),
                 # An analysis of comments, and calculation of who get the most vote
                shiny::conditionalPanel(condition = "input.col == 'Who is the most popular?'", shinycssloaders::withSpinner(
                  plotly::plotlyOutput("histcomment"), type = 4, color = "#c0392b"
                 )),
                 # Display of pictures that were posted in the subreddit
                 shiny::conditionalPanel(
                   condition = "input.col == 'Gallery of pictures'",
                   shinycssloaders::withSpinner(imageOutput("img"), type = 4, color = "#c0392b")
                 )


               )
             )
           )),

  # Fourth page - Analysis on specified users
  shiny::tabPanel("Track users!",
           shiny::fluidPage(
             shiny::sidebarLayout(
               position = "right",
               shiny::sidebarPanel(
                 style = "background: rgba(255, 255, 255, 0)",
                 shiny::wellPanel(
                   style = "background: rgba(192, 57, 43, 0.5)",
                   shiny::fileInput("file2", NULL, accept = ".csv"),
                   #define the choice of output
                   shiny::radioButtons(
                     "col1",
                     "Switch Plot",
                     choices = c("Where are you?", "In a good mood?", "What is he saying?"),
                     selected = "Where are you?"
                   )
                 )
               ),
               shiny::mainPanel(
                 # First plot that guess where a user potentially lives
                 shiny::conditionalPanel(condition = "input.col1 == 'Where are you?'", shinycssloaders::withSpinner(
                   shiny::plotOutput("plotcycle"), type = 4, color = "#c0392b"
                 )),
                 # Second output whihc is a plot about sentiment analysis of comment
                 shiny::conditionalPanel(condition = "input.col1 == 'In a good mood?'", shinycssloaders::withSpinner(
                   plotly::plotlyOutput("usersentiment"),
                   type = 4,
                   color = "#c0392b"
                 )),
                 shiny::conditionalPanel( condition =
                                     "input.col1 == 'What is he saying?'",
                                     shinycssloaders::withSpinner(
                                     wordcloud2::wordcloud2Output("wordcloud_user"), type = 4, color="#c0392b")),
               )


             )
           ))
))
