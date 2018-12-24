parse_period <- function(x){
  if(is.numeric(x)) return(x)

  x <- lubridate::as.period(x)
  x <- c(attributes(x)[c("year", "month", "day", "hour", "minute")], second = x)

  periods <- c(365.25, 30.5, 1, 1/24, 1/1440, 1/86400)

  sum(as.numeric(x)*periods)
}
