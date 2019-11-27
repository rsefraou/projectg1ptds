#' @title Scrapping urls
#'
#' @describeIn reddit_urls_mod returns a dataframe scrapped using a url. It differs from the existing function with the addition of the time frame that is limited to one week.
#' @param search_terms A \code{char} (character) used to specify what the user is looking for
#' @param regex_filter A \code{char} (character) used to specify what expression to filter
#' @param subreddit A \code{char} (character) used to specify what subreddit we want to scrape from. Default is NA
#' @param cn_threshold A \code{num} (numeric) used to specify the minimum number of commentaries in the discussion to scrap
#' @param page_treshold A \code{num} (numeric) used to specify the maxiumum number of pages to scrap
#' @param sort_by A \code{char} (character) used to specify if we scrape by new, by relevance etc. Relevance is the default
#' @param time_frame A \code{char} (character) used to specify if we look only at last week, last month, or all
#' @param wait_time A \code{num} (numeric) used to specify the waiting time between scrappings.
#' @return A \code{dataframe} with the scraping done
#' @import magrittr dplyr scales stringr ggplot2 stats remotes devtools
#' @export
#' @example
#' Adresses<-reddit_urls_mod(search_terms = "federer", regex_filter = "", subreddit = "tennis",
#' cn_threshold = 1, page_threshold = 25, sort_by = "new", time_frame= "all", wait_time = 4)
reddit_urls_mod<- function (search_terms = "", regex_filter = "", subreddit = NA,
cn_threshold = 0, page_threshold = 1, sort_by = "relevance", time_frame= "week",
wait_time = 2)
{
  install_cran("RedditExtractoR",force=T)
  library("RedditExtractoR")
  if (!grepl("^comments$|^new$|^relevance$|^top$", sort_by)) {
    stop("sort_by must be either 'new', 'comments', 'top' or 'relevance'")
  }
  if (!grepl("^hour$|^day$|^week$|^month$|^year$|^all$", time_frame)) {
    stop("time_frame must be either 'hour', 'day', 'week', 'month', 'year or 'all'")
  }

  sterms = ifelse(is.na(search_terms), NA, gsub("\\s", "+",search_terms))

  cached_links = data.frame(date = as.Date(character()),
                            num_comments = numeric(),
                            title = character(),
                            subreddit = character(),
                            URL = character(),
                            link = character())

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
  next_page = index = ""
  page_counter = 0
  comm_filter = 10000
  while (is.null(next_page) == FALSE & page_counter < page_threshold &
         comm_filter >= cn_threshold & length(index) > 0) {
    search_JSON = tryCatch(RJSONIO::fromJSON(readLines(search_query,
                                                       warn = FALSE)), error = function(e) NULL)
    if (is.null(search_JSON)) {
      cat(paste("Cannot connect to the website, skipping...\n"))
      next
    }
    else {
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
    cat(paste("\nNo results retrieved, check your query"))
  }
  else {
    remove_row = which(final_table[, 1] == "")
    if (length(remove_row) > 0) {
      final_table = final_table[-remove_row, ]
    }
    return(final_table)
  }
}


#' @title Retrieve content from given URL
#'
#' @describeIn reddit_content returns a dataframe scrapped using a url.
#' @param URL  used to specify what URL we want to extract the data from
#' @param wait_time  used to specify the time between scrappings
#' @return A \code{dataframe} with the scraping done
#' @import magrittr dplyr scales stringr ggplot2 stats remotes devtools
#' @export
reddit_content <- function (URL, wait_time = 2) {
 # install_cran("RedditExtractoR",force=T)
  library("RedditExtractoR")
  if (is.null(URL) | length(URL) == 0 | !is.character(URL)) {
    stop("invalid URL parameter")
  }
  GetAttribute = function(node, feature) {
    Attribute = node$data[[feature]]
    replies = node$data$replies
    reply.nodes = if (is.list(replies))
      replies$data$children
    else NULL
    return(list(Attribute, lapply(reply.nodes, function(x) {
      GetAttribute(x, feature)
    })))
  }
  get.structure = function(node, depth = 0) {
    if (is.null(node)) {
      return(list())
    }
    filter = is.null(node$data$author)
    replies = node$data$replies
    reply.nodes = if (is.list(replies))
      replies$data$children
    else NULL
    return(list(paste0(filter, " ", depth), lapply(1:length(reply.nodes),
                                                   function(x) get.structure(reply.nodes[[x]], paste0(depth,
                                                                                                      "_", x)))))
  }
  data_extract = data.frame(id = numeric(), structure = character(),
                            post_date = as.Date(character()), comm_date = as.Date(character()),
                            num_comments = numeric(), subreddit = character(), upvote_prop = numeric(),
                            post_score = numeric(), author = character(), user = character(),
                            comment_score = numeric(), controversiality = numeric(),
                            comment = character(), title = character(), post_text = character(),
                            link = character(), domain = character(), URL = character())
  pb = utils::txtProgressBar(min = 0, max = length(URL), style = 3)
  for (i in seq(URL)) {
    if (!grepl("^https?://(.*)", URL[i]))
      URL[i] = paste0("https://www.", gsub("^.*(reddit\\..*$)",
                                           "\\1", URL[i]))
    if (!grepl("\\?ref=search_posts$", URL[i]))
      URL[i] = paste0(gsub("/$", "", URL[i]), "/?ref=search_posts")
    X = paste0(gsub("\\?ref=search_posts$", "", URL[i]),
               ".json?limit=500")
    raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)),
                        error = function(e) NULL)
    if (is.null(raw_data)) {
      Sys.sleep(min(1, wait_time))
      raw_data = tryCatch(RJSONIO::fromJSON(readLines(X,
                                                      warn = FALSE)), error = function(e) NULL)
    }
    if (is.null(raw_data) == FALSE) {
      meta.node = raw_data[[1]]$data$children[[1]]$data
      main.node = raw_data[[2]]$data$children
      if (min(length(meta.node), length(main.node)) > 0) {
        structure = unlist(lapply(1:length(main.node),
                                  function(x) get.structure(main.node[[x]], x)))
        TEMP = data.frame(id = NA, structure = gsub("FALSE ",
                                                    "", structure[!grepl("TRUE", structure)]),
                          post_date = format(as.Date(as.POSIXct(meta.node$created_utc,
                                                                origin = "1970-01-01")), "%d-%m-%y"), comm_date = format(as.Date(as.POSIXct(unlist(lapply(main.node,
                                                                                                                                                          function(x) {
                                                                                                                                                            GetAttribute(x, "created_utc")
                                                                                                                                                          })), origin = "1970-01-01")), "%d-%m-%y"),
                          num_comments = meta.node$num_comments, subreddit = ifelse(is.null(meta.node$subreddit),
                                                                                    "UNKNOWN", meta.node$subreddit), upvote_prop = meta.node$upvote_ratio,
                          post_score = meta.node$score, author = meta.node$author,
                          user = unlist(lapply(main.node, function(x) {
                            GetAttribute(x, "author")
                          })), comment_score = unlist(lapply(main.node,
                                                             function(x) {
                                                               GetAttribute(x, "score")
                                                             })), controversiality = unlist(lapply(main.node,
                                                                                                   function(x) {
                                                                                                     GetAttribute(x, "controversiality")
                                                                                                   })), comment = unlist(lapply(main.node, function(x) {
                                                                                                     GetAttribute(x, "body")
                                                                                                   })), title = meta.node$title, post_text = meta.node$selftext,
                          link = meta.node$url, domain = meta.node$domain,
                          URL = URL[i], stringsAsFactors = FALSE)
        TEMP$id = 1:nrow(TEMP)
        if (dim(TEMP)[1] > 0 & dim(TEMP)[2] > 0)
          data_extract = rbind(TEMP, data_extract)
        else print(paste("missed", i, ":", URL[i]))
      }
    }
    utils::setTxtProgressBar(pb, i)
    Sys.sleep(min(2, wait_time))
  }
  data_extract[,13] <- cleaning_text_function(data_extract[,13], stopwords =   stopwords_vec)

  close(pb)
  return(data_extract)
}


#' @title Get comments from a user
#'
#' @describeIn get_user_comments returns a dataframe scrapped using username.
#' @param user  used to specify what the username
#' @param page_treshold  used to specify the maxiumum number of pages to scrap
#' @param wait_time  used to specify the waiting time between scrappings.
#' @return A dataframe with the scraping done
#' @import magrittr dplyr scales stringr ggplot2 stats remotes devtools
#' @export
get_user_comments <- function(user = "",page_threshold = 2,wait_time = 4){
  #install_cran("RedditExtractoR",force=T)
  library("RedditExtractoR")
  if (is.na(user) | user == "") {
    stop("Please enter an user name")
  }

  cached_links = tibble(date = as.Date(character()),
                        num_comments = numeric(),
                        title = character(),
                        subreddit = character(),
                        URL = character(),
                        comment = character(),
                        score = numeric())

  user_address = search_query = paste0("https://www.reddit.com/user/",
                                       user,
                                       "/comments/.json")
  next_page = ""
  page_counter = 0

  while (is.null(next_page) == FALSE & page_counter < page_threshold) {
    search_JSON = tryCatch(RJSONIO::fromJSON(readLines(search_query,
                                                       warn = FALSE)), error = function(e) NULL)
    if (is.null(search_JSON)) {
      cat(paste("Cannot connect to the website, skipping...\n"))
      break
    } else if (rlang::is_empty(search_JSON[[2]]$children)) {
      break
    } else {
      contents = search_JSON[[2]]$children
      search_permalink = paste0("http://www.reddit.com",sapply(seq(contents),
                                                               function(x) contents[[x]]$data$permalink))
      search_num_comments = sapply(seq(contents), function(x) contents[[x]]$data$num_comments)
      search_title = sapply(seq(contents), function(x) contents[[x]]$data$link_title)
      search_score = sapply(seq(contents), function(x) contents[[x]]$data$score)
      search_subreddit = sapply(seq(contents), function(x) contents[[x]]$data$subreddit)
      search_comment = sapply(seq(contents), function(x) contents[[x]]$data$body)
      search_date = format(as.POSIXct(unlist(lapply(seq(contents),
                                                    function(x) contents[[x]]$data$created_utc)),
                                      origin = "1970-01-01 00:00"), "%Y-%m-%d %H:%M")

      temp_dat = tibble(date = search_date,
                        num_comments = search_num_comments,
                        title = search_title,
                        subreddit = search_subreddit,
                        URL = search_permalink,
                        comment = search_comment,
                        score = search_score)[c(seq(25*(page_counter+1))),]

      cached_links = rbind(cached_links, temp_dat)

      next_page = search_JSON$data$after
      comm_filter = utils::tail(search_num_comments,1)
      search_query = paste0(user_address,"?count=",
                            (page_counter+1)*25,
                            "&after=",
                            next_page)

      page_counter = page_counter + 1

      Sys.sleep(min(2, wait_time))
    }
  }
  final_table = cached_links[!duplicated(cached_links), ]
  if (dim(final_table)[1] == 0) {
    cat(paste("\nNo results retrieved, check your query"))
  }
  else {
    remove_row = which(final_table[, 1] == "")
    if (length(remove_row) > 0) {
      final_table = final_table[-remove_row, ]
    }
    return(final_table)
  }
}

#' @title Clean data scrapped from reddit
#'
#' @describeIn cleaning_text_function returns a dataframe cleaned
#' @param x  we want to clean
#' @param stopwords  used to specify the stopwords we want to take off the dataframe
#' @return the scraped data cleaned
#' @import magrittr dplyr scales stringr ggplot2 stats remotes tidyverse stringi devtools
#' @export
cleaning_text_function <- function(x,stopwords){
 # install_cran("RedditExtractoR",force=T)
  library("RedditExtractoR")
  if(is.character(x)) {
    #condition to ingore characters columns with only internet link
    if (sum(str_sub(x, 1, 10) == "http://www",na.rm = TRUE) < length(x)/2) {
      #met les accents à la place du code html
      Encoding(x) <- 'latin1'
      #enleve les accents et met des lettres simples
      x <- stri_trans_general(x, 'Latin-ASCII')
      x <- unlist(lapply(x, function(x, stopwords) {
        #sépare tout les mots
        x <- unlist(strsplit(x, " "))
        #Enlève liens dans les commentaires
        x <- x[!grepl("\\S+www\\S+|\\S+https://\\S+", x)]
        #Enlèves codes ASCII et la ponctuation
        x <- gsub("\n|[[:punct:]]|[\x01-\x09\x11-\x12\x14-\x1F\x7F]", " ", x)
        #Enlève les lettres solitaires
        x <- gsub("(\\s{1}|^)[a-z]{1}(\\s{1}|$)","", x)
        #enlève les mots dans la liste stopwords
        x <-paste(x[!x %in% stopwords], collapse = " ")
        #mets tout en lower case
        x <- tolower(x)
        return(x)
      },
      stopwords = stopwords))
    } else {
      x
    }
  } else{
    x
  }
  return(x)
}

#' @title Plot sentiments from a researched word
#'
#' @describeIn plot_sentimentsReddit returns a sentiment analysis of a typed word .
#' @param word used to specify the word to research reddit for
#' @param stopwords  used to specify a list of words that will be used as stopwords
#' @return sentiments plot
#' @import magrittr dplyr ggplot2  wordcloud tidytext stopwords
#' @export
 plot_sentimentsReddit <- function( word, stopwords) {
# install_cran("RedditExtractoR",force=T)
library("RedditExtractoR")

  data<-projectg1ptds::reddit_urls_mod(search_terms = "word", regex_filter = "", subreddit =NA,
                                       cn_threshold = 1, page_threshold = 1, sort_by = "new", time_frame= "day",
                                       wait_time = 12)

  stopwords_vec <- c(stopwords::stopwords("en"), "don", "isn", "gt", "i", word)

  data.1 <- projectg1ptds::reddit_content(data[1:10,5], wait_time = 2)

  data.1["comment"] <- tibble::as_tibble(sapply(data.1["comment"],
                                                projectg1ptds::cleaning_text_function,
                                                stopwords= c(stopwords::stopwords("en"), word, stopwords_vec )))

  contenu_wordcloud <- data.1 %>%
    mutate(comment2 = comment) %>%
    tibble::as_tibble() %>%
    tidytext::unnest_tokens(word, comment) %>%
    filter(is.na(as.numeric(word)))


  ##SentimentAnalysis:

  contenu_sentiments <- contenu_wordcloud %>%
    dplyr::inner_join(tidytext::get_sentiments("nrc"), by = "word") %>%
    dplyr::group_by(sentiment) %>%
    count()

  ggplot(contenu_sentiments, aes(x = sentiment,y=n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")

 }



#' @title Get wordcloud
#'
#' @describeIn plot_wordcloudReddit returns a wordcloud of a typed word
#' @param word  used to specify the word to research reddit for
#' @param stopwordsused to specify a list of words that will be used as stopwords
#' @return  with the wordcloud
#' @import magrittr dplyr ggplot2 wordcloud tidytext stopwords RColorBrewer
#' @export
 plot_wordcloudReddit<-function(word, stopwords){

   library("RedditExtractoR")
     #word<-as.character(word)
     data<-projectg1ptds::reddit_urls_mod(search_terms ="word",
                                          regex_filter ="",
                                          subreddit =NA,
                                          cn_threshold =1,
                                          page_threshold =5,
                                          sort_by ="new",
                                          time_frame="day",
                                          wait_time =12)

     stopwords_vec <- c(stopwords::stopwords("en"), "don", "isn", "gt", "i", word)

     data.1 <- projectg1ptds::reddit_content(data[1:10,5],
                                             wait_time = 2)

     data.1["comment"] <- tibble::as_tibble(sapply(data.1["comment"],
                                                   projectg1ptds::cleaning_text_function,
                                                   stopwords= c(stopwords::stopwords("en"),
                                                                word,
                                                                stopwords_vec)))

     contenu_wordcloud <- data.1 %>%
       mutate(comment2 = comment) %>%
       tibble::as_tibble() %>%
       tidytext::unnest_tokens(word, comment) %>%
       filter(is.na(as.numeric(word)))

     contenu_wordcloud %>%
       count(word) %>%
       with(wordcloud(word, n, max.words = 50, colors=brewer.pal(8, "Spectral")))
 }


