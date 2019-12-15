#' @title Plot cycle
#'
#' @param enter a df
#' @param guess user location
#' @return ggplot with results
#' @export

plot_cycle <- function(df) {


  df <- df %>%
    na.omit() %>%
    dplyr::mutate(hour = lubridate::hour(date)) %>%
    mutate(hour = hour + 1) %>%
    group_by(hour) %>%
    summarise(comments = n())


  df$hour <- as.integer(df$hour)

  df <- left_join(data.frame(hour = 1:24), df , by = "hour")

  df[is.na(df)] <- 0

  #Calculate the moving average of the last 6 hours for each hour
  moving_average <- NULL
  for (i in 0:4) {
    moving_average <- rbind(moving_average,
                            sum(rbind(df[(24 - 4 + i):24, ] , df[1:(1 + i), ])[, 2]) /  6)
  }

  for (i in 0:18) {
    moving_average <- rbind(moving_average, sum(df[(1 + i):(6 + i), 2]) / 6)
  }

  moving_average <- data.frame(moving_average)

  #find min moving average position
  potential_end_nigh_vect <- which(night <- (moving_average ==
                                               min(moving_average)))

  #chose which hour is the end of the night
  if (any(potential_end_nigh_vect == 1)) {
    end_night <-
      max(potential_end_nigh_vect[!potential_end_nigh_vect > 7])
  } else {
    end_night <- max(potential_end_nigh_vect)
  }

  #create start and end night point

  if (end_night < 7) {
    sleep_start <- 24 - (6 - end_night)
    sleep_end <- end_night
    xstart <- c(1, sleep_end, sleep_start)
    xend <- c(sleep_end, sleep_start, 24)
    time_of_day <- c("sleep", "awake", "sleep")
  } else if (end_night == 7) {
    sleep_end <- end_night
    xstart <- c(1, sleep_end)
    xend <- c(sleep_end, 24)
    time_of_day <- c("sleep", "awake")
  } else if (end_night == 24) {
    sleep_end <- end_night
    xstart <- c(1, end_night - 6)
    xend <- c(end_night - 6, 24)
    time_of_day <- c("awake", "sleep")
  } else {
    sleep_start <- end_night - 6
    sleep_end <- end_night
    xstart <- c(1, sleep_start, sleep_end)
    xend <- c(sleep_start, sleep_end, 24)
    time_of_day <- c("awake", "sleep", "awake")
  }


  #Guess in fonction of when he wake up where the user is living

  if (5 < end_night & end_night <= 11) {
    continent <- "This user live in Europe or Africa"
  } else if (11 < end_night & end_night <= 18) {
    continent <- "This user live in South or North America"
  }  else if (18 < end_night & end_night <= 24) {
    continent <- "This user live in East asian or in Oceania"
  } else {
    continent <- "This user live in central Asia or Middle East"
  }


  #create data frame to plot the night/day cycle of the user
  day_cylce <- data.frame(start = xstart,  end = xend, user_cycle = time_of_day)

  plot <-ggplot() +
    geom_rect(data = day_cylce, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = user_cycle), alpha = 0.4) +
    geom_line(data = df, aes(hour, comments)) +
    scale_fill_manual(values = alpha(c("#FEFE00", "#1B1C46"), 0.4)) +
    labs(title = paste("Average daily activity of the selected user"),
         subtitle = continent) +
    xlab("hour of the day UTC +1") +
    ylab("Average number of comments") +
    theme_classic()

  return(plot)
}
