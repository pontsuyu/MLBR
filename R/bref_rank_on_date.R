#' Scrape MLB Standings on a Given Date
#'
#' This function allows you to scrape the standings from MLB for any date you choose.
#' @param date a date object
#' @param division One or more of AL East, AL Central, AL West,
#' AL Overall, NL East, NL Central, NL West, and NL Overall
#' @param from a logical indicating whether you want standings up to and
#' including the date (FALSE, default) or rather standings for games played
#' after the date
#' @keywords MLB, standings
#' @importFrom lubridate day month year
#' @importFrom rvest html_text html_nodes
#' @importFrom xml2 read_html
#' 
#' @examples
#' \dontrun{
#' bref_rank_on_date("2015-08-04", "AL East")
#' }

bref_rank_on_date <- function(date, division, from = FALSE) {
  
  stopifnot(intersect(grepl("AL|NL", division), grepl("East|Central|West|Overall", division)))
  url <- paste0("http://www.baseball-reference.com/boxes",
                "?year=", sprintf("%04i", lubridate::year(date)), "&month=",
                sprintf("%02i", lubridate::month(date)), "&day=", sprintf("%02i", lubridate::day(date)))
  
  tables <- url %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("table")
  min <- length(tables)
  max <- length(tables) - 15
  tables <- tables %>% .[min:max] %>% html_table
  
  table_names <- c("NL Overall", "AL Overall", "NL West" , "NL Central", "NL East", "AL West", "AL Central", "AL East",
                   "NL Overall", "AL Overall", "NL West" , "NL Central", "NL East", "AL West", "AL Central", "AL East")
  table_names[1:8] <- paste0(table_names[1:8], "_after_", date)     # Customizing list names for "After this Date" case
  table_names[9:16] <- paste0(table_names[9:16], "_up to_", date)   # Customizing list names for "From this Date" case
  names(tables) <- table_names
  after <- tables[1:8]
  current <- tables[9:16]
  
  if (!from) {
    div_date <- paste0(division, "_up to_", date)
    x <- current[div_date]
  } else if (from) {
    div_date <- paste0(division, "_after_", date)
    x <- after[div_date]
  }
  return(x)
}

#' Scrape MLB Standings on a Given Period and Visualize the Games Behind (GB) on any division or league the league
#'
#' This function allows you to scrape the standings from MLB for a period you choose, and visualize the GB of teams along that period.
#' @param start_date a date object representing the first date of the period
#' @param end_date a date object representing the last date of the period
#' @param lg_div One or more of AL East, AL Central, AL West,
#' AL Overall, NL East, NL Central, NL West, and NL Overall
#' @keywords MLB, standings
#' @importFrom highcharter hchart hc_title hc_subtitle hc_credits hc_yAxis hc_xAxis hc_add_theme hcaes hc_theme_smpl
#' @importFrom pbapply pbsapply
#' @importFrom tidyr separate
#' 
#' @examples
#' \dontrun{
#' bref_gamebehind_plot("2017-04-02","2017-04-10", "AL East")
#' }

bref_gamebehind_plot <- function(start_date, end_date, lg_div) {
  
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "days")   # Crate a vector of dates for the period
  standings <- sapply(dates, bref_rank_on_date, division = lg_div)
  
  all <- do.call("rbind", standings)
  all$id <- rep(names(standings), sapply(standings, nrow))
  rownames(all) <- NULL
  names(all) <- c("Team", "W", "L", "WLpct", "GB", "RS", "RA", "pythWLpct", "id")
  all <- all %>%
    tidyr::separate(id, c("League", "From", "Date"), "_") %>% 
    as_tibble() %>% 
    mutate(GB = ifelse(GB == "--", 0, GB) %>% as.numeric(digits = 2),
           pythWLpct = ifelse(is.na(pythWLpct), 0, pythWLpct),
           Date = as.Date(Date)) %>% 
    select(League, Date, Team, W, L, WLpct, GB)
  
  first_end <- all %>%
    filter(Date == min(Date) | Date == max(Date)) %>%
    arrange(Date, GB)
  print(first_end)
  
  highcharter::hchart(all, "line", highcharter::hcaes(x = Date, y = GB, group = Team)) %>%
    highcharter::hc_title(text = paste(all$League[1], "Standings (GB - Games behind)")) %>%
    highcharter::hc_subtitle(text = paste("from", start_date, "to", end_date)) %>%
    highcharter::hc_credits(enabled = TRUE, # add credits
                            text = "Source: Baseball Reference. Using 'baseballr' R package") %>%
    highcharter::hc_yAxis(title = list(text = "GB"),
                          reversed = TRUE) %>%
    highcharter::hc_xAxis(title = list(text = "Date")) %>%
    highcharter::hc_add_theme(highcharter::hc_theme_smpl()) %>%
    highcharter::hc_tooltip(valueDecimals = 1) %>% # round the value to the decimals
    highcharter::hc_exporting(enabled = TRUE) # enable exporting option
}

