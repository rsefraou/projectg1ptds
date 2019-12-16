#' @title Comment_plot
#'
#' @param analyse the comment on reddit
#' @param adapted dataframe
#' @return plot and analysis of the comment
#' @export
comment_plot <- function(df) {

  by_user_score<- df %>%
    tibble::as_tibble() %>%
    dplyr::filter(!user == "[deleted]")%>%
    dplyr::group_by(user) %>%
    dplyr::summarize(total = sum(comment_score), na.rm=T) %>%
    dplyr::mutate(rank = percent_rank(total)) %>%
    dplyr::arrange(desc(total)) %>%
    dplyr::mutate(posneg = ifelse(total < 0, 1,0))

  by_user_score <- by_user_score[c(1:10, (nrow(by_user_score)-10):nrow(by_user_score)) , ]

  by_user_score$user <- factor(by_user_score$user, levels = by_user_score$user[order(-by_user_score$total)])
  by_user_score$result <- ifelse(by_user_score$total >0, "Positive", "Negative")
  by_user_score$result <- as.factor(by_user_score$result)

  #create a barplot to plot the total score by user

  hist_score <-
    ggplot(by_user_score, aes(
      x = user,
      y = total,
      fill = result
    )) +
    scale_fill_manual(values = c("Positive" ="#006633" , "Negative" = "#c0392b"))+
    geom_bar(stat = "identity") +
    xlab("Pseudo of reddit user") +
    ylab("Total number of comment scores") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.text.y = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = 8)
    )

  hist_score_2 <-ggplotly(hist_score)
  return(hist_score_2)
}

