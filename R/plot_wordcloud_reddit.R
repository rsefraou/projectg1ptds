#' @title Get wordcloud
#'
#' @param word  used to specify the word to research reddit for
#' @param stopwordsused to specify a list of words that will be used as stopwords
#' @return  with the wordcloud
#' @export
plot_wordcloud_reddit <- function(word, stopwords) {
  data <- projectg1ptds::reddit_urls_mod(
    search_terms = "word",
    subreddit = NA,
    sort_by = "new",
    time_frame = "day"
  )
    stopwords_vec <- c(
      stopwords::stopwords("en"),
      "don",
      "isn",
      "gt",
      "i",
      "re",
      "removed",
      "deleted",
      "m",
      "you re",
      "we ll",
      "ve",
      "hasn",
      "they re",
      "id",
      "tl dr",
      word
    )
  data.1 <-
    projectg1ptds::reddit_content(data[1:10, 5], wait_time = 2)

  data.1["comment"] <- tibble::as_tibble(sapply(
    data.1["comment"],
    projectg1ptds::cleaning_text_function,
    stopwords = c(stopwords::stopwords("en"), word, stopwords_vec)
  ))

  contenu_wordcloud <- data.1 %>%
    tibble::as_tibble() %>%
    tidytext::unnest_tokens(word, comment) %>%
    dplyr::filter(is.na(as.numeric(word)))

  contenu_wordcloud %>%
    dplyr::count(word) %>%
    with(wordcloud::wordcloud(
      word,
      n,
      max.words = 50,
      colors = RColorBrewer::brewer.pal(8, "Spectral")
    ))
}
