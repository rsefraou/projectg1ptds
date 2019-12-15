#' @title Scrap data from reddit to get a table
#'
#' @param search_terms  The word we look for
#' @param subreddit the subreddit we search in
#' @param sort_by the sort by new, top...
#' @param time_frame the time frame taken into account
#' @return the scraped data cleaned
#' @export
scrapping_function <- function(search_terms, subreddit, sort_by, time_frame) {
  #Extract links with given research
  exctracted_link <- reddit_urls_mod(search_terms, subreddit, sort_by, time_frame)

  #Extract comments from the links
  exctracted_data <- reddit_content(exctracted_link[, 5])

  #Clean them for text mining process
  exctracted_data[, 13] <-  cleaning_text_function(exctracted_data[, 13])

  return(exctracted_data)
}
