#' Scrape statcast data
#'
#' @param start_date First game date. Format must be in yyyy-mm-dd.
#' @param end_date Last game date. Format must be in yyyy-mm-dd.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom foreach foreach %do%
#' @export
#' @examples
#' \dontrun{
#' sc_pbp(start_date = "2018-04-06", end_date = "2018-04-15")
#' }
sc_pbp <- function(start_date, end_date) {
  # if (!is.character(start_date) | !is.character(end_date))
  #   stop("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
  if (as.Date(start_date) <= "2015-03-01")
    warning("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
  if (as.Date(start_date) <= "2008-03-25")
    stop("The data are limited to the 2008 MLB season and after.")
  if (as.Date(start_date) > as.Date(end_date))
    stop("The start date is later than the end date.")
  
  year <- lubridate::year(start_date)
  days <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  start_days <- as.character(days[(1:length(days)) %% 7 == 1])
  end_days <- as.character(days[(1:length(days)) %% 7 == 0])
  if(length(start_days)!=length(end_days))
    end_days <- c(end_days, as.character(days[length(days)]))
  res <- list()
  numeric_col <- c(
    "release_speed", "release_pos_x", "release_pos_z",
    "pfx_x", "pfx_z", "plate_x", "plate_z",
    "hc_x", "hc_y", "vx0", "vy0", "vz0",
    "ax", "ay", "az", "sz_top", "sz_bot",
    "hit_distance_sc", "launch_speed", "launch_angle",
    "effective_speed", "release_spin_rate", "release_extension",
    "release_pos_y", "estimated_ba_using_speedangle",
    "estimated_woba_using_speedangle",
    "woba_value", "woba_denom", "babip_value",
    "iso_value", "launch_speed_angle"
  )
  n <- length(start_days)
  res <- foreach(i = 1:n) %do% {
    url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C&hfC&hfSea=", year, "%7C&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&player_type=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&game_date_gt=", start_days[i], "&game_date_lt=", end_days[i], "&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details")
    suppressMessages(suppressWarnings(
      data.table::fread(url, data.table = FALSE, check.names = T, showProgress = F) %>% 
        select(-pitcher, -fielder_2) %>% 
        purrr::map_if(is.character, ~str_replace(.x, "^null$", NA_character_)) %>% 
        as_tibble() %>% 
        mutate_at(numeric_col, as.numeric)
    )) %>% 
      transmute(
        game_year,
        game_date,
        game_type,
        home_team,
        away_team,
        game_pk,
        home_score,
        away_score,
        bat_score,
        fld_score,
        post_away_score,
        post_home_score,
        post_bat_score,
        post_fld_score,
        inning,
        inning_topbot,
        strikes,
        balls,
        outs_when_up,
        on_1b,
        on_2b,
        on_3b,
        fielder_1=as.character(pitcher.1),
        fielder_2=as.character(fielder_2.1),
        fielder_3=as.character(fielder_3),
        fielder_4=as.character(fielder_4),
        fielder_5=as.character(fielder_5),
        fielder_6=as.character(fielder_6),
        fielder_7=as.character(fielder_7),
        fielder_8=as.character(fielder_8),
        fielder_9=as.character(fielder_9),
        p_throws,
        pitch_number,
        pitch_type,
        pitch_name,
        release_speed,
        release_pos_x,
        release_pos_z,
        spin_dir,
        pfx_x,
        pfx_z,
        plate_x,
        plate_z,
        vx0,
        vy0,
        vz0,
        ax,
        ay,
        az,
        launch_speed,
        launch_angle,
        effective_speed,
        release_spin_rate,
        release_extension,
        release_pos_y,
        launch_speed_angle,
        zone,
        type,
        batter,
        batter_name=player_name,
        sz_top,
        sz_bot,
        at_bat_number,
        stand,
        events,
        description,
        des,
        bb_type,
        hit_location,
        hc_x,
        hc_y,
        hit_distance_sc
      )
  }
  
  res_data <- do.call("rbind", res) %>% 
    arrange(game_year, game_date, game_pk, inning, desc(inning_topbot), at_bat_number, pitch_number) %>% 
    sc_process_payload()
  
  return(res_data)
}

sc_process_payload <- function(payload) {
  
  # Clean up formatting of Baseball Savant download
  payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
  payload$des <- as.character(payload$des)
  payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
  payload$hc_x <- as.character(payload$hc_x) %>% as.numeric()
  payload$hc_y <- as.character(payload$hc_y) %>% as.numeric()
  payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
  payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
  payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
  payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
  payload$release_pos_y <- as.character(payload$release_pos_y) %>% as.numeric()
  payload$release_pos_z <- as.character(payload$release_pos_z) %>% as.numeric()
  payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
  payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
  payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
  payload$pfx_x <- as.character(payload$pfx_x) %>% as.numeric()
  payload$pfx_z <- as.character(payload$pfx_z) %>% as.numeric()
  payload$plate_x <- as.character(payload$plate_x) %>% as.numeric()
  payload$plate_z <- as.character(payload$plate_z) %>% as.numeric()
  payload$vx0 <- as.character(payload$vx0) %>% as.numeric()
  payload$vy0 <- as.character(payload$vy0) %>% as.numeric()
  payload$vz0 <- as.character(payload$vz0) %>% as.numeric()
  payload$ax <- as.character(payload$ax) %>% as.numeric()
  payload$az <- as.character(payload$az) %>% as.numeric()
  payload$ay <- as.character(payload$ay) %>% as.numeric()
  payload$sz_bot <- as.character(payload$sz_bot) %>% as.numeric()
  payload$sz_top <- as.character(payload$sz_top) %>% as.numeric()
  payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
  payload$release_speed <- as.character(payload$release_speed) %>% as.numeric()
  payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
  payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
  payload$pitch_name <- as.character(payload$pitch_name)
  payload$home_score <- as.character(payload$home_score) %>% as.numeric()
  payload$away_score <- as.character(payload$away_score) %>% as.numeric()
  payload$bat_score	<- as.character(payload$bat_score) %>% as.numeric()
  payload$fld_score <- as.character(payload$fld_score) %>% as.numeric()
  payload$post_away_score <- as.character(payload$post_away_score) %>% as.numeric()
  payload$post_home_score	<- as.character(payload$post_home_score) %>% as.numeric()
  payload$post_bat_score <- as.character(payload$post_bat_score) %>% as.numeric()
  payload$post_fld_score <- as.character(payload$post_fld_score) %>% as.numeric()
  payload$zone <- as.character(payload$zone) %>% as.numeric()
  
  # Create a specific variable for barrels
  payload <- payload %>%
    mutate(barrel = ifelse(launch_speed_angle == 6, 1, 0))
  
  return(payload)
}
