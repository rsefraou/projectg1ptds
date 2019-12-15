#' @title Get sentiments per hour of the day
#'
#' @param b  a database extracted from reddit_content()
#' @return  plot
#' @export
sentiments_per_hour<-function(df){

  #import database of type get_user_comments()
  b1 <- df
  #formatting


  b1$date <- lubridate::ymd_hm(b1$date)
  b1 <- df %>% dplyr::mutate(
    year = lubridate::year(date),
    month = lubridate::month(date),
    day = lubridate::day(date),
    hour = lubridate::hour(date),
    minute = lubridate::minute(date)
  )
  b1$hour <- as.factor(b1$hour)
  b1$ID <- seq.int(nrow(b1))

  #Unnest tokens for sentiment analysis
  b3 <- b1 %>%
    tibble::as_tibble() %>%
    tidytext::unnest_tokens(word, comment) %>%
    dplyr::filter(is.na(as.numeric(word)))

  #Sentiment analysis
  drv_comment <- b3 %>%
    dplyr::group_by(ID) %>%
    dplyr::inner_join(tidytext::get_sentiments("afinn"), by = "word")

  #Make them either positive or negative for a more easy visualisation
  drv_comment2 <- drv_comment %>%
    mutate(sentiment = ifelse(value < 0, "negative", "positive"))

  #plot
  comment_sent <- ggplot(drv_comment2, aes(x = hour, fill = sentiment)) +
    geom_bar(aes()) +
    scale_fill_manual(values = c("#c0392b", "#006633")) +
    theme_classic() +
    ggtitle("Sentiments per hour")

  ggplotly( comment_sent )

}
