#' Query Statcast and PITCHf/x Data for data from baseballsavant.mlb.com
#'
#' This function allows you to query Statcast and PITCHf/x data as provided on baseballsavant.mlb.com and have that data returned as a dataframe.
#' @param start_date Date of first game for which you want data. Format must be in YYYY-MM-DD format.
#' @param end_date Date of last game for which you want data. Format must be in YYYY-MM-DD format.
#' @param pit_bat Choose pitcher or batter
#' 
#' @import dplyr
#' @import data.table
#' @importFrom foreach foreach %do%
#' @export
#' @examples
#' \dontrun{
#' scrape_statcast(start_date = "2016-04-06", end_date = "2016-04-15", pit_bat = 'pitcher')
#' scrape_statcast(start_date = "2016-04-06", end_date = "2016-04-15", pit_bat = 'batter')
#' }

scrape_statcast <- function(start_date, end_date, pit_bat) {

  if(!pit_bat %in% c("pitcher", "batter"))
    stop("pit_bat must be 'pitcher' or 'batter'.")
  if(!is.character(start_date) | !is.character(end_date))
    stop("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
  if(as.Date(start_date)<="2015-03-01") # March 1, 2015 was the first date of Spring Training.
    stop("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
  if(as.Date(start_date)<="2008-03-25") # March 25, 2008 was the first date of Spring Training.
    stop("The data are limited to the 2008 MLB season and after.")
  if(as.Date(start_date)>as.Date(end_date))
    stop("The start date is later than the end date.")

  year <- substr(start_date, 1, 4)
  days <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  start_days <- as.character(days[1:length(days)%%7==1])
  end_days <- as.character(days[1:length(days)%%7==0])
  res <- list()
  res <- foreach(i = 1:min(length(start_days), length(end_days))) %do%
           {
             url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=", year, "%7C&hfSit=&player_type=", pit_bat, "&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", start_days[i], "&game_date_lt=", end_days[i], "&team=&position=&hfRO=&home_road=&&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
             fread(url, showProgress = F, stringsAsFactors = F, data.table = F) %>% 
               .[NROW(.):1,]
           }
  processed_payload <- lapply(res, process_statcast_payload)
  return(do.call("rbind", processed_payload) %>% 
    mutate(pitch_n = row_number()))
}

process_statcast_payload <- function(payload) {
  # Clean up formatting.
  payload <- payload %>% purrr::map_dfc(function(x) ifelse(x=="null", NA, x))
  payload$game_date <- as.Date(payload$game_date)
  # payload$des <- as.character(payload$des)
  payload$game_pk <- as.numeric(payload$game_pk)
  payload$on_1b <- as.numeric(payload$on_1b)
  payload$on_2b <- as.numeric(payload$on_2b)
  payload$on_3b <- as.numeric(payload$on_3b)
  payload$release_pos_x <- as.numeric(payload$release_pos_x)
  payload$hit_distance_sc <- as.numeric(payload$hit_distance_sc)
  payload$launch_speed <- as.numeric(payload$launch_speed)
  payload$launch_angle <- as.numeric(payload$launch_angle)
  payload$effective_speed <- as.numeric(payload$effective_speed)
  payload$release_speed <- as.numeric(payload$release_speed)
  payload$release_spin_rate <- as.numeric(payload$release_spin_rate)
  payload$release_extension <- as.numeric(payload$release_extension)
  # payload$pitch_name <- as.character(payload$pitch_name)
  payload$home_score <- as.numeric(payload$home_score)
  payload$away_score <- as.numeric(payload$away_score)
  payload$bat_score	<- as.numeric(payload$bat_score)
  payload$fld_score <- as.numeric(payload$fld_score)
  payload$post_away_score <- as.numeric(payload$post_away_score)
  payload$post_home_score	<- as.numeric(payload$post_home_score)
  payload$post_bat_score <- as.numeric(payload$post_bat_score)
  payload$post_fld_score <- as.numeric(payload$post_fld_score)
  payload$barrel <- with(payload, (launch_angle <= 50 &
                                   launch_speed >= 98 & 
                                   launch_speed * 1.5 - launch_angle >= 11 & 
                                   launch_speed + launch_angle >= 124) * 1)
  return(payload)
}
