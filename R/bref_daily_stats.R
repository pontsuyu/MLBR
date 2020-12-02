
#' Scrape Batter Performance Data Over a Custom Time Frame
#'
#' This function allows you to scrape basic batter statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param start_date First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param end_date Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param pit_bat
#' 
#' @export
#' @examples
#' \dontrun{daily_batter_bref("2015-05-10", "2015-06-20", "pitcher")}
bref_daily_batter <- function(start_date, end_date, pit_bat) {
  switch(
    pit_bat,
    pitcher = bref_daily_pitcher,
    batter = bref_daily_batter
  )
}

#' Scrape Batter Performance Data Over a Custom Time Frame
#'
#' This function allows you to scrape basic batter statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param start_date First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param end_date Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' 
#' @import dplyr
#' @importFrom rvest html_table html_nodes
#'
#' @examples
#' \dontrun{bref_daily_batter("2015-05-10", "2015-06-20")}

bref_daily_batter <- function(start_date, end_date) {
  
  payload <- xml2::read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=b&lastndays=7&dates=fromandto&fromandto=", start_date, ".", end_date, "&level=mlb&franch=&stat=&stat_value=0"))
  df <- payload %>%
    html_nodes(xpath = '//*[@id="daily"]') %>%
    html_table(fill = TRUE) %>% 
    as.data.frame() %>% 
    .[-c(1,3,5)]
  
  colnames(df)[1:4] <- c("Name", "Age", "Level", "Team")
  df[,c(2,5:26)] <- lapply(df[,c(2,5:26)],as.numeric)
  df$X1B <- with(df, H-(X2B+X3B+HR))
  df$season <- substr(start_date, 1, 4)
  df$uBB <- with(df, BB-IBB)
  df <- df[,c(28,1:9, 27, 10:15, 29, 16:26)]
  df$Team <- gsub(" $", "", df$Team, perl=T)
  df <- df %>% 
    filter(Name != "Name") %>% 
    arrange(desc(PA), desc(OPS))
  
  playerids <- payload %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    rename(slug = ".") %>%
    filter(grepl("redirect", slug)) %>%
    mutate(playerid = gsub("/redirect.fcgi\\?player=1&mlb_ID=", "", slug))
  
  df <- df %>%
    mutate(bbref_id = playerids$playerid) %>%
    select(bbref_id, everything())
  
  return(df)
}

#' Scrape Pitcher Performance Data Over a Custom Time Frame
#'
#' This function allows you to scrape basic pitcher statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param start_date First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param end_date Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @import dplyr
#' @importFrom rvest html_nodes html_table html_attr
#' 
#' @examples
#' \dontrun{bref_daily_pitcher("2015-05-10", "2015-06-20")}
bref_daily_pitcher <- function(start_date, end_date) {
  
  payload <- xml2::read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=p&lastndays=7&dates=fromandto&fromandto=", start_date, ".", end_date, "&level=mlb&franch=&stat=&stat_value=0"))
  df <- payload %>%
    html_nodes(xpath = '//*[@id="daily"]') %>%
    html_table(fill = TRUE) %>% 
    as.data.frame() %>% 
    .[-c(1,3,5)]
  
  colnames(df)[1:4] <- c("Name", "Age", "Level", "Team")
  df[,c(2,5:29, 36:39)] <- lapply(df[,c(2,5:29, 36:39)], as.numeric)
  df$X1B <- with(df, H-(X2B+X3B+HR))
  df$season <- substr(start_date, 1, 4)
  df$uBB <- with(df, BB-IBB)
  for(i in 30:35)
    df[,i] <- as.numeric(sub("%", "", df[, i]))/100
  df <- df[,c(41,1:13,42,14:19,40,20:39)]
  df$SO_perc <- with(df, round(SO/BF,3))
  df$uBB_perc <- with(df, round(uBB/BF,3))
  df$SO_uBB <- with(df, round(SO_perc - uBB_perc))
  df$Team <- gsub(" $", "", df$Team, perl=T)
  df <- df %>% 
    filter(Name != "Name") %>% 
    arrange(desc(IP), desc(WHIP))
  
  playerids <- payload %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    rename(slug = ".") %>%
    filter(grepl("redirect", slug)) %>%
    mutate(playerid = gsub("/redirect.fcgi\\?player=1&mlb_ID=", "", slug))
  
  df <- df %>%
    mutate(bbref_id = playerids$playerid) %>%
    select(bbref_id, everything())
  
  return(df)
}
