#' Scrape statcast data
#'
#' @param start_date Date of first game you want. Format must be in YYYY-MM-DD format.
#' @param end_date Date of last game you want. Format must be in YYYY-MM-DD format.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom foreach foreach %do%
#' @export
#' @examples
#' \dontrun{
#' scrape_statcast(start_date = "2018-04-06", end_date = "2018-04-15")
#' scrape_statcast(start_date = "2018-04-06", end_date = "2018-04-15")
#' }
scrape_statcast <- function(start_date, end_date) {
  if (!is.character(start_date) | !is.character(end_date)) {
    stop("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
  }
  if (as.Date(start_date) <= "2015-03-01") {
    warning("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
  }
  if (as.Date(start_date) <= "2008-03-25") {
    stop("The data are limited to the 2008 MLB season and after.")
  }
  if (as.Date(start_date) > as.Date(end_date)) {
    stop("The start date is later than the end date.")
  }

  year <- substr(start_date, 1, 4)
  days <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  start_days <- as.character(days[(1:length(days)) %% 7 == 1])
  end_days <- as.character(days[(1:length(days)) %% 7 == 0])
  res <- list()
  n <- max(length(start_days), length(end_days))
  res <- foreach(i = 1:n) %do% {
    if (i == n) end_days[i] <- end_date
    # url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=", year, "%7C&hfSit=&player_type=", pit_bat, "&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", start_days[i], "&game_date_lt=", end_days[i], "&team=&position=&hfRO=&home_road=&&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=&min_abs=0&type=details&")
    # url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C&hfC&hfSea=", year, "%7C&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&player_type=", pit_bat, "&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&game_date_gt=", start_days[i], "&game_date_lt=", end_days[i], "&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details")
    url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C&hfC&hfSea=", year, "%7C&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&player_type=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&game_date_gt=", start_days[i], "&game_date_lt=", end_days[i], "&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details")
    data.table::fread(url, showProgress = F, stringsAsFactors = F, data.table = F)
  }
  
  res <- lapply(res, cleanup_format)
  res_data <- do.call("rbind", res) %>%
    select(
      # game
      game_year, game_date, game_type, game_pk, inning, inning_topbot,
      home_team, away_team, home_score, away_score, bat_score, fld_score,
      post_away_score, post_home_score, post_bat_score, post_fld_score,
      # pitch
      pitcher, pitch_number, pitch_name, p_throws, pitch_type, release_speed, release_pos_x, release_pos_y, release_pos_z,
      release_spin_rate, release_extension, vx0, vy0, vz0, ax, ay, az, zone,
      pfx_x, pfx_z, plate_x, plate_z,
      # batter
      batter, stand, sz_top, sz_bot, at_bat_number,
      # runner
      on_1b, on_2b, on_3b,
      # result
      balls, strikes, outs_when_up, type,
      hit_location, bb_type, events, description,
      des, launch_speed, launch_angle, launch_speed_angle,
      hc_x, hc_y, hit_distance_sc,
      # sv_id, effective_speed, if_fielding_alignment, of_fielding_alignment,
      fielder_2, fielder_3, fielder_4, fielder_5,
      fielder_6, fielder_7, fielder_8, fielder_9,
      estimated_ba_using_speedangle, estimated_woba_using_speedangle,
      woba_value, woba_denom, babip_value, iso_value
    ) %>%
    arrange(game_year, game_date, game_pk, inning, desc(inning_topbot), at_bat_number, pitch_number) %>% 
    as.data.frame()
  return(res_data)
}

cleanup_format <- function(payload) {
  payload <- payload %>% purrr::map_dfc(function(x) ifelse(x == "null", NA, x))
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
  payload$bat_score <- as.numeric(payload$bat_score)
  payload$fld_score <- as.numeric(payload$fld_score)
  payload$post_away_score <- as.numeric(payload$post_away_score)
  payload$post_home_score <- as.numeric(payload$post_home_score)
  payload$post_bat_score <- as.numeric(payload$post_bat_score)
  payload$post_fld_score <- as.numeric(payload$post_fld_score)
  return(payload)
}
