

# Define UI for our application, we decided to use a navbar at the top
shinyUI(navbarPage(
  inverse = TRUE,
  "Reddit Analyser",

  # First Page - Intro, where we present the project
  tabPanel(
    "Welcome!",
    # the theme we chose for our application
    includeCSS("style.css"),
    #  includeCSS("box.css"),
    #  includeScript("main.js"),
    fluidPage(
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
  navbarMenu(
    "Downloads",
    #first subsection of the section downloads, to target reddit content
    tabPanel("Reddit content",
             fluidPage(
               h3("Here is where you may download data directly from the reddit API"),
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   style = "background: rgba(255, 255, 255, 0)",
                   wellPanel(
                     style = "background: rgba(192, 57, 43, 0.5)",
                     # different parameters that define the scrapping
                     textInput("search", "What are you interested in?", value = "Greta"),
                     textInput("subreddit", "Select an existing subreddit", value = "news"),
                     selectInput(
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

                     selectInput(
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

                     actionButton(class="btn", "load1", "Download"),
                     actionButton(class="btn","display1", "Show table")

                   )
                 ),
                 mainPanel(shiny::uiOutput("id1"),

                           withSpinner(DT::dataTableOutput("download1",width = "auto"), type = 4, color="#c0392b")
                 ))

             )),
    # second subsection of the section downloads, to gather user information
    tabPanel("Stalking?",
             fluidPage(
               br(),
               br(),
               h3("Welcome to the second downloading option!!"),
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   style = "background: rgba(255, 255, 255, 0)",
                   wellPanel(
                     style = "background: rgba(192, 57, 43, 0.5)",
                     # Just enter the username to scrap its info
                     textInput("user", "Enter a username"),

                     actionButton(class="btn","load2", "Stalk!"),
                     actionButton(class="btn","display2", "Show table")

                   )),
                 mainPanel(shiny::uiOutput("id2"),

                           withSpinner(DT::dataTableOutput("download2",width = "auto"), type = 4, color="#c0392b")
                 ))
             ))
  ),




  # Third Page  - Main section where the content of reddit will be analysed
  tabPanel("Inside Reddit",
           fluidPage(
             h3("Here are different possible analysis on the content"),
             p(
               "Each possible section provides a different analytical perspective of Reddit Content!"
             ),
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 style = "background: rgba(255, 255, 255, 0)",


                 wellPanel(
                   style = "background: rgba(192, 57, 43, 0.5)",
                   fileInput("file", NULL, accept = ".csv"),
                   # Different options to choose what analysis should be displayed
                   radioButtons(
                     "col",
                     "Switch Plot",
                     choices =  c("Where is it trending?", "Gallery of pictures","Wordcloud","Sentiment analysis"),
                     selected = "Where is it trending?"

                   ),
                   # Supplementary output shown only if the mentionned parameters is selected
                   conditionalPanel("input.col == 'Wordcloud'",
                                    withSpinner(DT::dataTableOutput("table"), type = 4,color="#c0392b"))


                 )),


               mainPanel(
                 # Wordcloud output
                 conditionalPanel(
                   "input.col == 'Wordcloud'",
                   withSpinner(
                     wordcloud2Output("wordcloud"), type = 4, color="#c0392b")
                 )

                 ,

                 # Sentiment analysis
                 conditionalPanel("input.col == 'Sentiment analysis'", withSpinner(
                   plotlyOutput("redditsentiment"),
                   type = 4,
                   color = "#c0392b"


                 )),
                 # A map that shows where the subject is most related to
                 conditionalPanel(
                   condition = "input.col == 'Where is it trending?'",
                   withSpinner(leafletOutput("mymap"), type = 4, color = "#c0392b")
                 ),

                 # Display of pictures that were posted in the subreddit
                 conditionalPanel(
                   condition = "input.col == 'Gallery of pictures'",
                   withSpinner(imageOutput("img"), type = 4, color = "#c0392b")
                 )


               )
             )
           )),

  # Fourth page - Analysis on specified users
  tabPanel("Track users!",
           fluidPage(
             h3("Here are different possible analysis on the user"),
                     p(
                       "Each possible section provides a different analytical perspective on a Reddit user!"
                     ),
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 style = "background: rgba(255, 255, 255, 0)",
                 wellPanel(
                   style = "background: rgba(192, 57, 43, 0.5)",
                   fileInput("file2", NULL, accept = ".csv"),
                   #define the choice of output
                   radioButtons(
                     "col1",
                     "Switch Plot",
                     choices = c("Where are you?", "In a good mood?", "What is he saying?"),
                     selected = "Where are you?"
                   )
                 )
               ),
               mainPanel(
                 # First plot that guess where a user potentially lives
                 conditionalPanel(condition = "input.col1 == 'Where are you?'", withSpinner(
                   plotOutput("plotcycle"), type = 4, color = "#c0392b"
                 )),
                 # Second output whihc is a plot about sentiment analysis of comment
                 conditionalPanel(condition = "input.col1 == 'In a good mood?'", withSpinner(
                   plotlyOutput("usersentiment"),
                   type = 4,
                   color = "#c0392b"
                 )),
                 conditionalPanel( condition =
                                     "input.col1 == 'What is he saying?'",
                                   withSpinner(
                                     wordcloud2Output("wordcloud_user"), type = 4, color="#c0392b")),
               )


             )
           ))
))
