
#' @title Loading speech data
#' @description Loads speech data and returns data as a tibble
#' @return Tibble
#' @export
#'
#' @examples load_data()
#'
#'
#'
load_data <- function(){

   stump_speeches <<- readr::read_rds("data/stump_speeches.RDS")

}
