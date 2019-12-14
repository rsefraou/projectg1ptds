#' @title Comment_plot
#'
#' @param analyse the comment on reddit
#' @param adapted dataframe
#' @return plot and analysis of the comment
#' @export
comment_plot <- function(df) {

  #compute the total score received by each user

  # by_user_comment<- df %>%
  #  tibble::as_tibble() %>%
  #  group_by(user) %>%
  #  count() %>%
  #  arrange(desc(n))
  #
  #     by_user_comment$user <- factor(by_user_comment$user, levels = by_user_comment$user[order(-by_user_comment$n)])

  by_user_score<- df %>%
    tibble::as_tibble() %>%
    group_by(user) %>%
    summarize(total = sum(comment_score)) %>%
    mutate(rank = percent_rank(total)) %>%
    filter(rank > 0.95 | rank < 0.01) %>%
    arrange(desc(total)) %>%
    mutate(posneg = ifelse(total < 0, 1,0))

  by_user_score$user <- factor(by_user_score$user, levels = by_user_score$user[order(-by_user_score$total)])

  #create a barplot to plot the total score by user

  # hist_comm <-
  #   ggplot(by_user_comment, aes(
  #     x = user,
  #     y = n,
  #     )) +
  #   geom_histogram(stat = "identity") +
  #   scale_y_continuous() +
  #   xlab("pseudo of reddit user") +
  #   ylab("Total number of comments") +
  #   theme_minimal() +
  #   theme(
  #     axis.text.x = element_text(angle = 90),
  #     legend.position = "none",
  #     axis.text = element_text(size = 8)
  #   )
  #

  hist_score <-
    ggplot(by_user_score, aes(
      x = user,
      y = total,
      fill = total > 0
    )) +
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
  return(hist_score)

}
