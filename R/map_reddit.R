#' @title Get a map with countries that appear in comments
#'
#' @param word a word to research reddit for
#' @return  map
#' @export
map_reddit<-function(df){

  #upload of a data base with all the country name

  data("World")
  b <- as.tibble(World)
  spData <- b %>% mutate(name = tolower(name))
  countries <- spData[,2] %>% mutate(word=name)



  #clean the data set to have a better overview of words
  contenu_cleaned <- df %>%
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
  # i guess the problem in shiny may come from this line
  if(dim(contenu_tokens)[1] == 0){
    stop("There is no mention of any countries in this dataset")
  }

  contenu_tokens_2 <- contenu_tokens[, 19:20]


  clean_country <- contenu_tokens_2 %>%
    dplyr::group_by(name) %>%
    dplyr::summarise("frequency" = sum(freq))

  #deal with capital letter

  firstup <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  clean_country$name <- firstup(clean_country$name)
  for (i in c(1: length(clean_country$name))){
    if (clean_country$name[i] == "United states"){
      clean_country$name[i] <- "United States"
    }
  }

  for (i in c(1: length(clean_country$name))){
    if (clean_country$name[i] == "United kingdom"){
      clean_country$name[i]<-"United Kingdom"
    }
  }

  # add the variable to the sp World data frame

  name_order <- data.frame(name =  as.character(World$name))
  clean_country$name <- clean_country$name%>%as.factor()
  final_joint <- left_join(name_order, clean_country, by = "name" )

  World$frequency <- final_joint$frequency

  #creation of the static map
  map <- tm_shape(World) +
    tm_fill(col="frequency") +
    tmap_mode('view') +
    tm_borders() +
    tm_basemap(server = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")

  return(map)
}
