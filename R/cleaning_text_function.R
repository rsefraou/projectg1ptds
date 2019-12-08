#' @title Clean data scrapped from reddit
#'
#' @param x  we want to clean
#' @param stopwords  used to specify the stopwords we want to take off the dataframe
#' @return the scraped data cleaned
#' @export
cleaning_text_function <- function(x,stopwords =stopwords_vec ){
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