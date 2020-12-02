#' Scrape Pitcher Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a pitcher from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @param pit_bat pitcher or batter
#' 
#' @export
#' @examples
#' \dontrun{fg_gamelog(playerid = 104, year = 2006, "pitcher")}

fg_gamelog <- function(playerid, year, pit_bat) {
  switch(pit_bat,
         pitcher = fg_pitcher_gamelog(playerid, year),
         batter = fg_batter_gamelog(playerid, year)
  )
}
#' Scrape Pitcher Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a pitcher from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' 
#' @import dplyr
#' @import rvest
#' @export
#' @examples
#' \dontrun{fg_pitcher_gamelog(playerid = 104, year = 2006)}

fg_pitcher_gamelog <- function(playerid, year = 2017) {
  
  url <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=", 
                playerid, "&season=", year, "&position=P")
  
  payload <- xml2::read_html(url) %>%
    html_nodes("table") %>%
    .[length(.)] %>%
    html_table(fill = TRUE) %>%
    as.data.frame()
  
  payload <- payload %>%
    filter(!grepl("Date|Total", Date)) %>%
    rename(K_9 = K.9, BB_9 = BB.9, HR_9 = HR.9,
           LOB_perc = LOB., GB_perc = GB.,
           HR_FB = HR.FB)
  
  if (nrow(payload) > 1) {
    payload <- sapply(payload, function(x) gsub("\\%", "", x)) %>% 
      as.data.frame(stringsAsFactors=F)
  } else {
    payload <- lapply(payload, function(x) gsub("\\%", "", x)) %>%
      bind_rows()
  }
  
  payload$K_9 <- as.numeric(payload$K_9)
  payload$BB_9 <- as.numeric(payload$BB_9)
  payload$HR_9 <- as.numeric(payload$HR_9)
  payload$LOB_perc <- as.numeric(payload$LOB_perc)/100
  payload$GB_perc <- as.numeric(payload$GB_perc)/100
  payload$HR_FB <- as.numeric(payload$HR_FB)
  
  return(payload)
}

#' Scrape Batter Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a batter from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' 
#' @import dplyr
#' @import rvest
#' @export
#' @examples
#' \dontrun{fg_batter_gamelog(playerid = 6184, year = 2017)}

fg_batter_gamelog <- function(playerid, year) {
  
  url <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                playerid, "&season=", year, "&position=PB")
  
  payload <- xml2::read_html(url) %>%
    html_nodes("table") %>%
    .[length(.)] %>%
    html_table(fill = TRUE) %>%
    as.data.frame()
  
  payload <- payload %>%
    filter(!grepl("Date|Total", Date)) %>%
    rename(BB_perc = BB., K_perc = K., wRC_plus = wRC.)
  
  
  if (nrow(payload) > 1) {
    payload <- sapply(payload, function(x) gsub("\\%", "", x)) %>% 
      as.data.frame(stringsAsFactors=F)
  } else {
    payload <- lapply(payload, function(x) gsub("\\%", "", x)) %>%
      bind_rows()
  }
  
  payload$BB_perc <- as.numeric(payload$BB_perc)/100
  payload$K_perc <- as.numeric(payload$K_perc)/100
  
  return(payload)
}

