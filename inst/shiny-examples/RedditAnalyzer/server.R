

# Increase the size of possible file input in this shiny app

options(shiny.maxRequestSize = 50 * 1024 ^ 2)

# Define server logic so that the application work
shinyServer(function(input, output) {



# Download page server

# First function to scrape data from reddit

  df1 <- shiny::eventReactive(input$load1, {
    projectg1ptds::scrapping_function(input$search,
                       input$subreddit,
                       input$sortby,
                       input$timeframe)
  })

# Second function to create a csv from the content gathered above
  shiny::observeEvent(input$load1, (utils::write.csv(
    df1(),
    file = paste0(
      input$search,
      "-",
      input$subreddit,
      "-",
      input$sortby,
      "-",
      input$timeframe,
      ".csv"
    )
  )))

# Scrap the data relative to the user
  df2 <- shiny::eventReactive(input$load2, {
    projectg1ptds::get_user_comments(input$user)
  })

# Create a csv file for the user data
  shiny::observeEvent(input$load2, (utils::write.csv(df2(), file = paste0(
    input$user, ".csv"
  ))))

# Text to explain how difficult downloading can be
  output$id1 <- shiny::renderUI(
    p(
      "As you may know, downloading is not always an easy task! It may take some time and be quite irritating.
To avoid such a situation, here is a bit of reading! While we are scrapping our data from the social media Reddit, why don't we talk about it?
Reddit was founded in 2005 in the United States. At first, it was not popular. People that were using Reddit were mostly interested in technology and programming.
It was a good place to ask question, when problems occured while coding, a similar situation as in our project.
For a few years Reddit has became more and more popular in Europe. If your download is not finished yet, here is some more",
      a("reading", href = "https://en.wikipedia.org/wiki/Reddit")
         ,style = "text-align: left;")
  )

# Print a part of the dataframe to show user what has be downloaded
  printdf1<- shiny::eventReactive(input$display1, {
    df1()
  })


  output$download1 <- DT::renderDataTable({
    DT::datatable(printdf1(), options = list(lengthMenu = c(10, 20, 50), autoWidth=TRUE,
                                       scrollX = TRUE, scrollY=TRUE, pageLength = 10),
                  rownames = FALSE,  class="compact")})

# Text to explain how cool stalking is

  output$id2 <- shiny::renderUI(
    p(
      "As we are done with the formal aspect, welcome to the fun part!
    We are sure that most of us like to check what how our friends are interacting on social media!
    With the file you are probably downloading now, you will learn a lot about what your virtual or in real life friends are doing."
    )
  )


# Function to show what data are collected from an user

  printdf2<- shiny::eventReactive(input$display2, {
    df2()
  })

  output$download2 <- DT::renderDataTable({
    DT::datatable(printdf2(),options = list(lengthMenu = c(10, 20, 50),autoWidth=TRUE,
                                       scrollX = TRUE,scrollY=TRUE, pageLength = 10),
                  rownames = FALSE,  class="compact")})


# Content analysis

# Upload of a file for the analysis

  df3 <- shiny::reactive({
    shiny::req(input$file)
    ext <- tools::file_ext(input$file$filename)

    df<- tibble::as_tibble(vroom::vroom(input$file$datapath, delim = ",")[, -1])

    return(df)
  })

# Wordcloud
# Word counts is a fonction counting word occurence


  word_counts <- shiny::reactive({

    word <- c("ll", "ve", "does", "http", "subreddit","guy", "people", "doesn","didn","guys")
    lexicon <- c("mine", "mine", "mine", "mine","mine", "mine", "mine", "mine", "mine", "mine")
    mystopwords <- data.frame(word, lexicon)
    stop_words <- rbind(stop_words, mystopwords)

   shiny::req(input)

    df3() %>%
      tibble::as.tibble() %>%
      tidytext::unnest_tokens(word, comment) %>%
      dplyr::filter(is.na(as.numeric(word)))%>%
      dplyr::anti_join(stop_words, by = "word") %>%
      dplyr::count(word, sort = TRUE) %>%
      dplyr::filter(word!="")

  })


# Word search table created with the help of the function above

  output$table <- DT::renderDataTable({
    shiny::req(df3)
    DT::datatable(word_counts(), options = list(lengthMenu = c(10, 20, 50), pageLength = 10),
                  rownames = FALSE, colnames = c("Word", "Count"), class = 'compact',
                  caption = 'Common English words (stop words) are excluded')
  })

# Creation of the interactive wordcloud

  output$wordcloud <- wordcloud2::renderWordcloud2({
   wordcloud2::wordcloud2(word_counts(), size = 1.6,
              shape ="circle",
              fontFamily = "Courier",
              color = rep_len(c("red", "orange", "black", "grey"),
                             nrow(word_counts()))
             )
  })


# Sentiment analysis on reddit content, more information about the function on the package

  output$redditsentiment <- plotly::renderPlotly({
    projectg1ptds::plot_sentiments_reddit(df3())
  })



# Map of where the subject chosen is more trending or at leat mentionned

  output$mymap <- leaflet::renderLeaflet({
    tmap::tmap_leaflet(projectg1ptds::map_reddit(df3()))
  })


# Display of different pictures shown in a subreddit


  output$img <- shiny::renderImage({

    shiny::req(input)
    inkart <- df3()

    # Creation of an error message if there is no picture in the subreddit

    inkart <- inkart[unlist(lapply(1:nrow(inkart), function(x) grepl("*.jpg$", inkart[x,16]))),]

    if(dim(inkart)[1] == 0){
      stop("No image link in this dataframe")
    }


    intake <- stringr::str_match(inkart$link, ".*jpg") %>% unique()
    intake <- intake[-1, ]
    intake <- tibble::as.tibble(intake)

    images_list <- purrr::map(intake$value, magick::image_read)
    img <- magick::image_join(images_list)

    img<-magick::image_montage(image_scale(img, "x200"))

    imagei <<- magick::image_convert((img), "jpeg")

    tmpfile<-imagei %>%
      magick::image_write(tempfile(fileext='jpg'), format = 'jpg')
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })


# Input of the database for the user analysis section

  df4 <- shiny::reactive({
    shiny::req(input$file2)
    ext <- tools::file_ext(input$file2$filename)

    df <- tibble::as.tibble(vroom::vroom(input$file2$datapath, delim = ",")[, -1])

    return(df)
  })


# Function created to guess when a user is sleeping and therefore where he is approximately
  output$plotcycle <- renderPlot({
    df <-df4() %>% na.omit()

    df$date <- as.POSIXct(df$date, format="%Y-%m-%d %H:%M", tz="GMT")
    projectg1ptds::plot_cycle(df)
  })


# Plot of user sentiment and this time not reddit content
  output$usersentiment <- plotly::renderPlotly({
    projectg1ptds::sentiments_per_hour(df4())
  })

  # Create a wordcloud for the user

  output$wordcloud_user <- wordcloud2::renderWordcloud2({

    df <-df4() %>% na.omit()

    df$comment <- projectg1ptds::cleaning_text_function(pull(df[,6]))

    word_counts <- df %>%
      tibble::as_tibble() %>%
      tidytext::unnest_tokens(word, comment) %>%
      dplyr::filter(is.na(as.numeric(word))) %>%
      dplyr::anti_join(stop_words, by = "word") %>%
      dplyr::count(word, sort = TRUE)

    wordcloud2::wordcloud2(word_counts, size=1.6,
               shape="circle",
               fontFamily = "Courier",
               color= rep_len(c("red","orange","black","grey"),
                              nrow(word_counts))
    )
  })

})
