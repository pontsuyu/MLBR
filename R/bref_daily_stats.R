#' Scrape Batter/Pitcher Performance Data Over a Custom Time Frame
#'
#' This function allows you to scrape basic batter statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param start_date First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param end_date Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param pit_bat
#' 
#' @export
#' @examples
#' \dontrun{bref_daily_Performance("2015-05-10", "2015-06-20", "pitcher")}
bref_daily_Performance <- function(start_date, end_date, pit_bat) {
  switch(
    pit_bat,
    pitcher = bref_daily_pitcher(start_date, end_date),
    batter = bref_daily_batter(start_date, end_date)
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
    .[-c(1,3,5)] %>% 
    filter(Age!="Age")
  
  colnames(df)[1:4] <- c("Name", "Age", "Level", "Team")
  df[,c(2,5:26)] <- lapply(df[,c(2,5:26)], as.numeric)
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
    .[-c(1,3,5)] %>% 
    filter(Age!="Age")
  
  colnames(df)[1:4] <- c("Name", "Age", "Level", "Team")
  df[,c(2,5:29, 36:39)] <- lapply(df[,c(2,5:29, 36:39)], as.numeric)
  df$X1B <- with(df, H-(X2B+X3B+HR))
  df$season <- substr(start_date, 1, 4)
  df$uBB <- with(df, BB-IBB)
  for(i in 30:35)
    df[,i] <- as.numeric(sub("\\%", "", df[, i]))/100
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
    select(bbref_id, everything()) %>% 
    fg_fip_add()
  
  return(df)
}

#' Calculate FIP and related metrics for any set of data
#'
#' This function allows you to calculate FIP and related metrics for any given set of data, provided the right variables are in the data set. The function currently returns both FIP per inning pitched, wOBA against (based on batters faced), and wOBA against per instance of fair contact.
#'
#' @param df A data frame of statistics that includes, at a minimum, the following columns: IP (innings pitched), BF (batters faced), uBB (unintentional walks), HBP (Hit By Pitch), x1B (singles), x2B (doubles), x3B (triples), HR (home runs), AB (at-bats), SH (sacrafice hits), SO (strike outs), and season.
#' 
#' 

fg_fip_add <- function(df) {
  df$season <- as.character(df$season)
  guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>%
    html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE) %>% 
    as.data.frame() %>% 
    .[-(1:2), (1:14)]
  name <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  colnames(guts_table) <- name
  for(i in c(2:ncol(guts_table)))
    guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
  
  df_join <- df %>% 
    left_join(guts_table, by = "season") %>% 
    mutate(FIP = round(((((13*HR) + (3*(uBB + HBP)) - (2*SO))/IP) + cFIP), 2),
           wOBA_against = round((((wBB * uBB) + (wHBP * HBP) + (w1B * X1B) + (w2B * X2B) + (w3B * X3B) + (wHR * HR))/(BF)),3),
           wOBA_CON_against = round((((w1B * X1B) + (w2B * X2B) + 	(w3B * X3B) + (wHR * HR))/(AB - SO)),3)) %>% 
    arrange(desc(wOBA_against))
  x <- colnames(df_join) %in% name[-1]
  return(df_join[!x])
}
