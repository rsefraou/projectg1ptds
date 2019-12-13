#' @title Get a mosaique from image in the subreddit
#'
#' @param specify a subreddit
#' @return pane of images that show what are images within the subreddit
#' @export


montage <- function(df) {
  #Create new dataframe to not touch the original one
  inkart <- df
  #Select unique links to image.
  intake <- stringr::str_match(inkart$link, ".*jpg") %>% unique()

  intake <- intake[-1, ]
  intake <- tibble::as_tibble(intake)

  read_image_wrapper <- function(url) image_read(url)
  images_list <- purrr::map(intake$value, read_image_wrapper)
  img <- magick::image_join(images_list)


  magick::image_append(image_scale(img, "x200"))
}
