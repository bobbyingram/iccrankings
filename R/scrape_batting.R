scrape_batting <- function(format, date){
  scrape("batting", format, date)
}

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
  raw <- rvest::html_table(node, trim = TRUE, header = TRUE)

  rankings <- raw %>%
    magrittr::set_colnames(.[1, ]) %>%
    dplyr::slice(-1) %>%
    dplyr::rename(Rank = ID,
                  Rating = Rat.) %>%
    dplyr::mutate(Rank = as.integer(Rank),
                  Rating = as.integer(Rating),
                  Date = date,
                  Format = stringr::str_to_upper(format)) %>%
    dplyr::select(Date, Format, Rank, Rating, Name)

  rankings
}
