#' # Scrape MiLB game logs for batters from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MILB game logs for individual batters from FanGraphs.com.
#' @param playerid The batter's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' @param pit_bat pitcher or batter
#' 
#' @export
#' @examples
#' \dontrun{fg_milb_gamelog(playerid = 20126, 2018, "batter")}

fg_milb_gamelog <- function(playerid, year, pit_bat) {
  switch(pit_bat,
         batter = fg_milb_batter_gamelog(playerid, year),
         pitcher = fg_milb_pitcher_gamelog(playerid, year))
}

#' # Scrape MiLB game logs for batters from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MILB game logs for individual batters from FanGraphs.com.
#' @param playerid The batter's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' 
#' @import dplyr
#' @import rvest
#'
#' @examples
#' \dontrun{fg_milb_batter_gamelog(playerid = "sa917940", 2018)}

fg_milb_batter_gamelog <- function(playerid, year) {
  
  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                      playerid, "&season=", year, "&position=PB","&type=-1")
  
  # url for advanced game log table
  url_adv <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                    playerid, "&season=", year, "&position=PB","&type=-2")
  
  # standard table
  payload1 <- xml2::read_html(url_basic) %>%
    html_nodes("table") %>%
    .[length(.)] %>%
    html_table() %>%
    as.data.frame() %>%
    filter(!grepl("Date|Total", Date))
  
  # advanced table
  payload2 <- xml2::read_html(url_adv) %>%
    html_nodes("table") %>%
    .[length(.)] %>%
    html_table() %>%
    as.data.frame() %>%
    filter(!grepl("Date|Total", Date)) %>%
    rename(BB_perc = BB., K_perc = K.,
           wRC_plus = wRC., BB_per_K = BB.K)
  
  payload2 <- sapply(payload2, function(x) gsub("\\%", "", x)) %>% 
    as.data.frame(stringsAsFactors=F) %>% 
    mutate(BB_perc = as.numeric(BB_perc)/100,
           K_perc = as.numeric(K_perc)/100)
  
  # combine standard & advanced game log tables
  payload <- cbind(payload1, payload2 %>% select(-c("Date", "Team", "Opp", "AVG"))) %>% 
    # separate Team column into Team & MiLB level
    tidyr::separate(Team, into = c("Team","Level"), sep=" ")
  
  # extract player name
  player_name <- xml2::read_html(url_basic) %>%
    html_nodes("h1") %>%
    html_text()
  
  # add playerid to payload
  payload <- payload %>%
    mutate(name = player_name,
           playerid = playerid) %>%
    select(name, playerid, everything())
  
  return(payload)
}


#' # Scrape MiLB game logs for pitchers from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MILB game logs for individual batters from FanGraphs.com.
#' @param playerid The pitcher's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' 
#' @import dplyr
#' @import rvest
#'
#' @examples
#' \dontrun{milb_pitcher_game_logs_fg(playerid = "sa873980", 2018)}

fg_milb_pitcher_gamelog <- function(playerid, year) {
  
  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                      playerid, "&season=", year, "&position=P","&type=-1")
  
  # url for advanced game log table
  url_adv <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                    playerid, "&season=", year, "&position=P","&type=-2")
  
  # standard table
  payload1 <- xml2::read_html(url_basic) %>%
    html_nodes("table") %>%
    .[length(.)] %>%
    html_table() %>%
    as.data.frame() %>% 
    filter(!grepl("Date|Total", Date))
  
  # advanced table
  payload2 <- xml2::read_html(url_adv) %>%
    html_nodes("table") %>%
    .[length(.)] %>%
    html_table() %>%
    as.data.frame() %>% 
    filter(!grepl("Date|Total", Date)) %>%
    rename(BB_perc = BB., K_perc = K.,
           K_minus_BB = K.BB., LOB_perc = LOB.)
  
  payload2 <- sapply(payload2, function(x) gsub("\\%", "", x)) %>% 
    as.data.frame(stringsAsFactors=F) %>% 
    mutate(BB_perc = as.numeric(BB_perc)/100,
           K_perc = as.numeric(K_perc)/100,
           K_minus_BB = as.numeric(K_minus_BB)/100,
           LOB_perc = as.numeric(LOB_perc)/100)
  
  # combine standard & advanced game log tables
  payload <- cbind(payload1, payload2 %>% select(-c("Date", "Team", "Opp", "GS", "ERA"))) %>% 
    # separate Team column into Team & MiLB level
    tidyr::separate(Team, into = c("Team","Level"),sep=" ")
  
  # extract player name
  player_name <- xml2::read_html(url_basic) %>%
    html_nodes("h1") %>%
    html_text()
  
  # add playerid to payload
  payload <- payload %>%
    mutate(name = player_name,
           playerid = playerid) %>%
    select(name, playerid, everything())
  
  return(payload)
}
