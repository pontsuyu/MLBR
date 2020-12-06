#' Retrieve batting orders for a given MLB game via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param game_pk The unique game_pk identifier for the game
#' @param type Whether to just return the starting lineup ('starting') or all
#' batters that appeared ('all')
#' 
#' @import dplyr
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @return Returns a data frame that includes probable starting pitchers and
#' the home plate umpire for the game_pk provided
#' requested
#' 
#' @export
#'
#' @examples \dontrun{statsapi_batting_orders(566001)}

statsapi_batting_orders <- function(game_pk) {
  
  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk, "/feed/live")
  list <- jsonlite::fromJSON(api_call, flatten = TRUE)
  home_team <- tibble(homeTeam = list$gameData$teams$home$name,
                      homeTeamId = list$gameData$teams$home$id)
  away_team <- tibble(awayTeam = list$gameData$teams$away$name,
                      awayTeamId = list$gameData$teams$away$id)
  home_players <- tibble(playerid = names(list[["liveData"]][["boxscore"]][["teams"]][["home"]][["players"]]))
  away_players <- tibble(playerid = names(list[["liveData"]][["boxscore"]][["teams"]][["away"]][["players"]]))
  
  home_players <- home_players %>%
    group_split(playerid) %>%
    map(~players(list = list, team = "home", playerid = .x$playerid)) %>% 
    do.call("rbind", .) %>% 
    mutate(batting_order = as.character(batting_order),
           batting_position_num = as.character(batting_position_num),
           team = "home",
           teamName = home_team$homeTeam,
           teamID = home_team$homeTeamId) %>% 
    arrange(batting_order)
  
  away_players <- away_players %>%
    group_split(playerid) %>%
    map(~players(list = list, team = "away", playerid = .x$playerid)) %>% 
    do.call("rbind", .) %>% 
    mutate(batting_order = as.character(batting_order),
           batting_position_num = as.character(batting_position_num),
           team = "away",
           teamName = away_team$awayTeam,
           teamID = away_team$awayTeamId) %>% 
    arrange(batting_order)
  
  final_batting_order_table <- bind_rows(away_players, home_players) %>%
    select(-c(link, code, name, type)) %>%
    arrange(team, batting_order, batting_position_num) %>%
    filter(!is.na(batting_order))
  
  return(final_batting_order_table)
}

players <- function(list, team = "home", playerid) {
  person <- list[["liveData"]][["boxscore"]][["teams"]][[team]][["players"]][[playerid]][["person"]] %>%
    bind_rows()
  position <- list[["liveData"]][["boxscore"]][["teams"]][[team]][["players"]][[playerid]][["position"]] %>%
    bind_rows()
  batting_position <- list[["liveData"]][["boxscore"]][["teams"]][[team]][["players"]][[playerid]][["battingOrder"]]
  final_table <- bind_cols(person, position) %>% 
    mutate(batting_order = ifelse(is.null(batting_position), NA, substr(batting_position, 1, 1)),
           batting_position_num = ifelse(is.null(batting_position), NA, as.numeric(substr(batting_position, 2, 3))))
}

