#' @title Get wordcloud
#'
#' @param word  used to specify the word to research reddit for
#' @param stopwordsused to specify a list of words that will be used as stopwords
#' @return  with the wordcloud
#' @export

plot_wordcloud_reddit <- function(df) {


  contenu_wordcloud <- df %>%
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
