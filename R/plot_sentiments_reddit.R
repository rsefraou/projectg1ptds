#' @title Plot sentiments from a researched word
#'
#' @param word used to specify the word to research reddit for
#' @param stopwords  used to specify a list of words that will be used as stopwords
#' @return sentiments plot
#' @importFrom magrittr "%>%"
#' @export
plot_sentiments_reddit <- function( word, stopwords) {

  
  data<-projectg1ptds::reddit_urls_mod(search_terms = "word", 
                                       subreddit =NA, 
                                       sort_by = "new",
                                       time_frame= "day")
  
  stopwords_vec <- c(stopwords::stopwords("en"), "don", "isn", "gt", "i", word)
  
  data.1 <- projectg1ptds::reddit_content(data[1:10,5], wait_time = 2)
  
  data.1["comment"] <- tibble::as_tibble(sapply(data.1["comment"],
                                                projectg1ptds::cleaning_text_function,
                                                stopwords= c(stopwords::stopwords("en"), 
                                                             word, 
                                                             stopwords_vec )))
  
  contenu_wordcloud <- data.1 %>%
    tibble::as_tibble() %>%
    tidytext::unnest_tokens(word, comment) %>%
    dplyr::filter(is.na(as.numeric(word)))
  
  
  ##SentimentAnalysis:
  
  contenu_sentiments <- contenu_wordcloud %>%
    dplyr::inner_join(tidytext::get_sentiments("nrc"), by = "word") %>%
    dplyr::group_by(sentiment) %>%
    dplyr::count()
  
  ggplot2::ggplot(contenu_sentiments, ggplot2::aes(x = sentiment,y=n, fill = sentiment)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(x = "", y = "Number of words", fill = "Sentiment")
  
}

