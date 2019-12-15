#' @title Scrapping urls
#'
#' @param search_terms A \code{char} (character) used to specify what the user is looking for
#' @param subreddit A \code{char} (character) used to specify what subreddit we want to scrape from. Default is NA
#' @param sort_by A \code{char} (character) used to specify if we scrape by new, by relevance etc. Relevance is the default
#' @param time_frame A \code{char} (character) used to specify if we look only at last week, last month, or all
#' @return A \code{dataframe} with the scraping done
#' @export
reddit_urls_mod<- function (search_terms = "", subreddit = "",
                            sort_by = "", time_frame= "")
{

  if (subreddit == ""){
    subreddit <- NA
  }

  if (search_terms == ""){
    search_terms <- NA
  }

  if (!grepl("^[0-9A-Za-z]*$", subreddit) & !is.na(subreddit) ) {
    stop("subreddit must be a sequence of letter and number without special characters and spaces")
  }

  regex_filter = ""
  cn_threshold = 0
  page_threshold = 15
  wait_time = 1

  cached_links = data.frame(date = as.Date(character()),
                            num_comments = numeric(),
                            title = character(),
                            subreddit = character(),
                            URL = character(),
                            link = character())

  if (sort_by != "front_page"){

    if (!grepl("^comments$|^new$|^relevance$|^top$|^front_page$", sort_by)) {
      stop("sort_by must be either 'new', 'comments', 'top', 'relevance' or 'front_page'")
    }

    if (!grepl("^hour$|^day$|^week$|^month$|^year$|^all$", time_frame)) {
      stop("time_frame must be either 'hour', 'day', 'week', 'month', 'year or 'all'")
    }


    sterms = ifelse(is.na(search_terms), NA, gsub("\\s", "+",search_terms))

    subreddit = ifelse(is.na(subreddit), "", paste0("r/", gsub("\\s+","+", subreddit), "/"))

    sterms = ifelse(is.na(sterms), "", paste0("q=", sterms, "&restrict_sr=on&"))
    sterms_prefix = ifelse(sterms == "", "new", "search")
    time_frame_in = ifelse(is.na(search_terms), "", paste0("t=",time_frame,"&"))

    search_address = search_query = paste0("https://www.reddit.com/",
                                           subreddit, sterms_prefix,
                                           ".json?",
                                           sterms,time_frame_in,
                                           "sort=",
                                           sort_by)

  } else {
    if (is.na(subreddit)) {
      stop("if you choose sort_by = front_page please enter a subreddit")
    }

    search_address = search_query = paste0("https://www.reddit.com/r/",
                                           subreddit,
                                           ".json?")
  }

  next_page = index = ""
  page_counter = 0
  comm_filter = 10000
  while (is.null(next_page) == FALSE & page_counter < page_threshold &
         comm_filter >= cn_threshold & length(index) > 0) {
    search_JSON = tryCatch(RJSONIO::fromJSON(readLines(search_query,
                                                       warn = FALSE)), error = function(e) NULL)
    if (is.null(search_JSON)) {
      stop(paste("Unable to connect to reddit website or invalid subreddit entered"))
    } else if (length(search_JSON$data$children)==0){
      stop(paste("This search term returned no results or invalid subreddit entered"))
    } else {
      contents = search_JSON[[2]]$children
      search_permalink = paste0("http://www.reddit.com",
                                sapply(seq(contents), function(x) contents[[x]]$data$permalink))
      search_num_comments = sapply(seq(contents), function(x) contents[[x]]$data$num_comments)
      search_title = sapply(seq(contents), function(x) contents[[x]]$data$title)
      search_score = sapply(seq(contents), function(x) contents[[x]]$data$score)
      search_subreddit = sapply(seq(contents), function(x) contents[[x]]$data$subreddit)
      search_link = sapply(seq(contents), function(x) contents[[x]]$data$url)
      index = which(search_num_comments >= cn_threshold &
                      grepl(regex_filter, search_title, ignore.case = T,
                            perl = T))
      if (length(index) > 0) {
        search_date = format(as.Date(as.POSIXct(unlist(lapply(seq(contents), function(x) contents[[x]]$data$created_utc)),
                                                origin = "1970-01-01")), "%d-%m-%y")


        temp_dat = data.frame(date = search_date,
                              num_comments = search_num_comments,
                              title = search_title,
                              subreddit = search_subreddit,
                              URL = search_permalink,
                              link = search_link,
                              stringsAsFactors = FALSE)[index,]

        cached_links = as.data.frame(rbind(cached_links,
                                           temp_dat))
        next_page = search_JSON$data$after
        comm_filter = utils::tail(search_num_comments,
                                  1)
        search_query = paste0(search_address, "&after=",
                              next_page)
        page_counter = page_counter + 1
      }
      Sys.sleep(min(2, wait_time))
    }
  }
  final_table = cached_links[!duplicated(cached_links), ]
  if (dim(final_table)[1] == 0) {
    cat(paste("\nNo results retrieved, should be invalid subreddit entered, down server or simply unsuccessful search query :("))
  }
  else {
    remove_row = which(final_table[, 1] == "")
    if (length(remove_row) > 0) {
      final_table = final_table[-remove_row, ]
    }
    return(final_table)
  }
}
