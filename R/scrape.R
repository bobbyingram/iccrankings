#' Scrape the top 100 batting ratings by format for a given date.
#'
#' @param format One of 'Test', 'ODI' or 'T20'.
#' @param date The date to look up the rankings for.
#' @return A tibble with a row for each player.
#'
#' The fields are:
#'
  #' \describe{
  #' \item{Date}{The date to which these rankings and ratings apply.}
  #' \item{Format}{The type of cricket.}
  #' \item{Rank}{Ranking.}
  #' \item{Rating}{ICC player rating.}
  #' \item{Name}{The name of the player.}
  #' }
#'
#' @export
#' @md
scrape_batting <- function(format, date){
  scrape("batting", format, date)
}

#' Scrape the top 100 bowlers by format for a given date.
#'
#' @inheritParams scrape_batting
#' @return A tibble with a row for each player.
#'
#' The fields are:
#'
#' \describe{
#' \item{Date}{The date to which these rankings and ratings apply.}
#' \item{Format}{The type of cricket.}
#' \item{Rank}{Ranking.}
#' \item{Rating}{ICC player rating.}
#' \item{Name}{The name of the player.}
#' }
#'
#' @export
#' @md
scrape_bowling <- function(format, date){
  scrape("bowling", format, date)
}

scrape <- function(stat, format, date){
  # Scrape
  site <- "http://www.relianceiccrankings.com/datespecific/"
  stat <- stringr::str_to_lower(stat)
  format <- stringr::str_to_lower(format)
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::mday(date)

  url <- glue::glue("{site}/{format}/?stattype={stat}&day={day}&month={month}&year={year}")
  try(html <- xml2::read_html(url), silent = TRUE)
  node <- rvest::html_node(html, "table")
  if(is.na(node))
    return(data.frame())
  raw <- rvest::html_table(node, trim = TRUE, header = TRUE)

  rankings <- raw %>%
    magrittr::set_colnames(.[1, ]) %>%
    dplyr::slice(-1) %>%
    dplyr::rename(Rank = .data$ID,
                  Rating = .data$Rat.) %>%
    dplyr::mutate(Rank = as.integer(.data$Rank),
                  Rating = as.integer(.data$Rating),
                  Date = date,
                  Format = stringr::str_to_upper(format)) %>%
    dplyr::select(.data$Date,
                  .data$Format,
                  .data$Rank,
                  .data$Rating,
                  .data$Name)

  rankings
}
