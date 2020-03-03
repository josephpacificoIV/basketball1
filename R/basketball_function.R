#' SomePlayer function
#'
#' This function allows you to generate a data point of interest in the dataset for some player
#' @param year select a given year.
#' @keywords some
#' @export
#' @examples
#' SomePlayer()


SomePlayer <- function(year){
  Y = dplyr::group_by(d, "Year" == year)
  dplyr::sample_n(Y, 1, replace = TRUE)


}

#' OldestPlayer Function
#'
#' This function allows you to find the oldest player in the NBA data for that year.
#' @param year select a given year.
#' @keywords oldest
#' @export
#' @examples
#' OldestPlayer()



OldestPlayer <- function(year) {
  Yr = dplyr::filter(d, d$Year == year)
  newYr = select( Yr, Age, Player, Year)
  newYr = dplyr::arrange(newYr, desc(Age))
  head(newYr, 1)


}

#' CorrMatrix Function
#'
#' This function allows you create a correlation matrix for all numeric variables in the data for that year.
#' @param year select a given year.
#' @keywords corr
#' @export
#' @examples
#' CorrMatrix()



CorrMatrix <- function(year){
  Yr = dplyr::filter(d, d$Year == year)
  newYr = keep(Yr, is.numeric)
  newYr = na.omit(newYr)
  return(cor(newYr))


}




