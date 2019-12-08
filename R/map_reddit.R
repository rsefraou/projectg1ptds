#' @title Get a map with countries that appear in comments
#'
#' @param word a word to research reddit for
#' @return  map
#' @export
map_reddit<-function(word){
  #to create a function first we need to form a database
  Adresses<-projectg1ptds::reddit_urls_mod(search_terms = word, subreddit = NA
                                           , sort_by = "new", time_frame= "week")

  #scrap the content of the discussion link
  stopwords_vec <- c(stopwords::stopwords("en"), "don", "isn", "gt", "i")
  stopwords_a <- c("don","t","i","gt")

  contenu <- projectg1ptds::reddit_content(Adresses[,5])

  contenu[,5] <- tibble::as_tibble(sapply(contenu[,5],
                                          projectg1ptds::cleaning_text_function,
                                          stopwords =stopwords::stopwords("en") ))


  #upload of a data base with all the country name

  data("World")
  b <- as.tibble(World)
  spData <- b %>% mutate(name = tolower(name))
  countries <- spData[,2] %>%mutate(word=name)



  #clean the data set to have a better overview of words
  contenu_cleaned <- contenu %>%
    as.tibble() %>%
    unnest_tokens(word, comment) %>%
    filter(is.na(as.numeric(word)))

  #create two loops in order to represent USA and UK in the map
  usa<-c("united states" , "us", "usa", "america","u.s","u.s.a")
  for(i in c(1:length(contenu_cleaned$word))){
    if (contenu_cleaned$word[i] %in% usa){
      contenu_cleaned$word[i]<-"united states"
    }
  }
  uk<-c("uk" , "gb", "england", "England","Wales","NIR","northern ireland", "england", "wales","scotland","scottland")
  for(i in c(1:length(contenu_cleaned$word))){
    if (contenu_cleaned$word[i] %in% uk){
      contenu_cleaned$word[i]<-"united kingdom"
    }
  }

  # create a token with the count of how many times a country was mentionned

  contenu_tokens <- contenu_cleaned %>%
    inner_join(countries, by = "word") %>%
    plyr::count()

  contenu_tokens_2 <- contenu_tokens[,19:20]


  clean_country <- contenu_tokens_2%>%dplyr:: group_by(name) %>% dplyr::summarise("frequency"=sum(freq))

  #deal with capital letter

  firstup <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  clean_country$name<- firstup(clean_country$name)
  for (i in c(1: length(clean_country$name))){
    if (clean_country$name[i]=="United states"){
      clean_country$name[i]<-"United States"
    }
  }

  for (i in c(1: length(clean_country$name))){
    if (clean_country$name[i]=="United kingdom"){
      clean_country$name[i]<-"United Kingdom"
    }
  }

  # add the variable to the sp World data frame

  name_order <- data.frame(name =  as.character(World$name))
  clean_country$name<-clean_country$name%>%as.factor()
  final_joint <- left_join(name_order, clean_country, by = "name" )

  World$frequency <- final_joint$frequency

  #creation of the static map
  tm_shape(World)+
    tm_fill(col="frequency")+
    tm_borders()+
    tm_style("classic")+
    tmap_mode("view")
}
