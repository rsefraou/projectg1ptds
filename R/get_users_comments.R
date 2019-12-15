#' @title Get comments from a user
#'
#' @param user  used to specify what the username
#' @param page_treshold  used to specify the maxiumum number of pages to scrap
#' @param wait_time  used to specify the waiting time between scrappings.
#' @return A dataframe with the scraping done
#' @export
get_user_comments <- function(user = "", page_threshold = 5, wait_time = 2) {
    if (is.na(user) | user == "") {
      stop("Please enter an user name")
    }

    cached_links = tibble(
      date = as.Date(character()),
      num_comments = numeric(),
      title = character(),
      subreddit = character(),
      URL = character(),
      comment = character(),
      score = numeric()
    )

    user_address = search_query = paste0("https://www.reddit.com/user/",
                                         user,
                                         "/comments/.json")
    next_page = ""
    page_counter = 0

    withProgress(message = 'Work in progress', value = 0, {
    while (is.null(next_page) == FALSE &
           page_counter < page_threshold) {
      search_JSON = tryCatch(
        RJSONIO::fromJSON(readLines(search_query,
                                    warn = FALSE)),
        error = function(e)
          NULL
      )
      if (is.null(search_JSON)) {
        cat(paste("Cannot connect to the website, skipping...\n"))
        break
      } else if (rlang::is_empty(search_JSON[[2]]$children)) {
        break
      } else {
        contents = search_JSON[[2]]$children
        search_permalink = paste0("http://www.reddit.com",
                                  sapply(seq(contents),
                                         function(x)
                                           contents[[x]]$data$permalink))
        search_num_comments = sapply(seq(contents), function(x)
          contents[[x]]$data$num_comments)
        search_title = sapply(seq(contents), function(x)
          contents[[x]]$data$link_title)
        search_score = sapply(seq(contents), function(x)
          contents[[x]]$data$score)
        search_subreddit = sapply(seq(contents), function(x)
          contents[[x]]$data$subreddit)
        search_comment = sapply(seq(contents), function(x)
          contents[[x]]$data$body)
        search_date = format(as.POSIXct(unlist(
          lapply(seq(contents),
                 function(x)
                   contents[[x]]$data$created_utc)
        ),
        origin = "1970-01-01 00:00"),
        "%Y-%m-%d %H:%M")

        temp_dat = tibble(
          date = search_date,
          num_comments = search_num_comments,
          title = search_title,
          subreddit = search_subreddit,
          URL = search_permalink,
          comment = search_comment,
          score = search_score
        )[c(seq(25 * (page_counter + 1))), ]

        cached_links = rbind(cached_links, temp_dat)

        next_page = search_JSON$data$after
        comm_filter = utils::tail(search_num_comments, 1)
        search_query = paste0(user_address,
                              "?count=",
                              (page_counter + 1) * 25,
                              "&after=",
                              next_page)

        page_counter = page_counter + 1
        incProgress(amount = 1/page_threshold)
        Sys.sleep(min(2, wait_time))
      }

    }
    })
    final_table = cached_links[!duplicated(cached_links),]
    if (dim(final_table)[1] == 0) {
      cat(paste("\nNo results retrieved, check your query"))
    }
    else {
      remove_row = which(final_table[, 1] == "")
      if (length(remove_row) > 0) {
        final_table = final_table[-remove_row,]
      }
      return(final_table)
    }
  }
