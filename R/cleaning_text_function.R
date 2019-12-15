#' @title Clean data scrapped from reddit
#'
#' @param x  we want to clean
#' @param stopwords  used to specify the stopwords we want to take off the dataframe
#' @return the scraped data cleaned
#' @export
cleaning_text_function <- function(x,stopwords=stopwords_vec) {
  stopwords_vec <- c(stopwords::stopwords("en"), "don", "isn", "gt", "i", "re","removed","deleted","m","you re","we ll", "ve", "hasn","they re","id","tl dr", "didn", "wh","oh","tl","dr","shes","hes","aren","edit","ok","ll","wasn","shouldn","t","doesn","youre","going","still","much", "many","also")

  if (is.character(x)) {
    #Put accents instead of code html (only for french)
    Encoding(x) <- 'latin1'
    #take out accent
    x <- stri_trans_general(x, 'latin-ascii')
    x <- unlist(lapply(x, function(x, stopwords = stopwords_vec) {
      #separate words
      x <- unlist(strsplit(x, " "))
      #take out internet links
      x <- x[!grepl("\\S+www\\S+|\\S+https://\\S+|https://\\S+", x)]
      #take out codes ASCII and ponctuation
      x <-gsub("\n|[[:punct:]]|[\x01-\x09\x11-\x12\x14-\x1F\x7F]|gt"," ",x)
      #take out simple alone numbers
      x <-gsub("(^[0-9]{1}\\s|^[0-9]{1}$|\\s{1}[0-9]{1}$|\\s{1}[0-9]{1}\\s{1})"," ",x)
      #take out space in the beginning and end of stringg
      x <-gsub("(^[[:blank:]]+|[[:blank:]]+$)", "", x)
      #lowercase
      x <- tolower(x)
      #take out alone letters
      x <-gsub("(^[a-z]{1}\\s+|^[a-z]{1}$|\\s+[a-z]{1}$|\\s+[a-z]{1}\\s+)", "", x)
      #take out words in stopwords list
      x <-paste(x[!x %in% stopwords], collapse = " ")
      #rerun stopwords again to get ride of stopword in composed string
      x <- unlist(strsplit(x, " "))
      x <-gsub("(^[[:blank:]]+|[[:blank:]]+$)", "", x)
      x <-paste(x[!x %in% stopwords], collapse = " ")
      return(x)
    }))
  } else{
    stop("please enter a character vector")
  }
  return(x)
}
