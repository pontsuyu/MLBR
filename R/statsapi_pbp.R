#' Acquire pitch-by-pitch data for Major and Minor League games via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param game_pk The date for which you want to find game_pk values for MLB games
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes over 100 columns of data provided
#' by the MLB Stats API at a pitch level.
#'
#' Some data will vary depending on the
#' park and the league level, as most sensor data is not availble in
#' minor league parks via this API. Note that the column names have mostly
#' been left as-is and there are likely duplicate columns in terms of the
#' information they provide. I plan to clean the output up down the road, but
#' for now I am leaving the majority as-is.
#'
#' Both major and minor league pitch-by-pitch data can be pulled with this function.
#' @keywords MLB, sabermetrics
#' @import dplyr
#' @export
#'
#' @examples \dontrun{statsapi_pbp(575156)}

statsapi_pbp <- function(game_pk) {
  
  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk, "/feed/live")
  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)
  
  plays <- payload$liveData$plays$allPlays$playEvents %>% bind_rows()
  at_bats <- payload$liveData$plays$allPlays
  current <- payload$liveData$plays$currentPlay
  game_status <- payload$gameData$status$abstractGameState
  home_team <- payload$gameData$teams$home$name
  home_level <- payload$gameData$teams$home$sport
  home_league <- payload$gameData$teams$home$league
  away_team <- payload$gameData$teams$away$name
  away_level <- payload$gameData$teams$away$sport
  away_league <- payload$gameData$teams$away$league
  list_columns <- lapply(at_bats, class) %>%
    bind_rows(.id = "variable") %>%
    tidyr::gather(key, value) %>%
    filter(value == "list") %>%
    pull(key)
  
  at_bats <- at_bats %>%
    select(-one_of(list_columns))
  
  pbp <- plays %>%
    left_join(at_bats, by = c("endTime" = "playEndTime")) %>%
    tidyr::fill(atBatIndex:matchup.splits.menOnBase, .direction = "up") %>%
    mutate(game_pk = game_pk,
           game_date = substr(payload$gameData$datetime$dateTime, 1, 10)) %>%
    select(game_pk, game_date, everything()) %>% 
    mutate(matchup.batter.fullName =
             factor(matchup.batter.fullName),
           matchup.pitcher.fullName =
             factor(matchup.pitcher.fullName),
           atBatIndex = factor(atBatIndex)
           # batted.ball.result = case_when(!result.event %in% c(
           #   "Single", "Double", "Triple", "Home Run") ~ "Out/Other",
           #   TRUE ~ result.event),
           # batted.ball.result = factor(batted.ball.result,
           #                             levels = c("Single", "Double", "Triple", "Home Run", "Out/Other"))
    ) %>%
    mutate(home_team = home_team,
           home_level_id = home_level$id,
           home_level_name = home_level$name,
           home_parentOrg_id = payload$gameData$teams$home$parentOrgId,
           home_parentOrg_name = payload$gameData$teams$home$parentOrgName,
           home_league_id = home_league$id,
           home_league_name = home_league$name,
           away_team = away_team,
           away_level_id = away_level$id,
           away_level_name = away_level$name,
           away_parentOrg_id = payload$gameData$teams$away$parentOrgId,
           away_parentOrg_name = payload$gameData$teams$away$parentOrgName,
           away_league_id = away_league$id,
           away_league_name = away_league$name,
           batting_team = factor(ifelse(about.halfInning == "bottom",
                                        home_team,
                                        away_team)),
           fielding_team = factor(ifelse(about.halfInning == "bottom",
                                         away_team,
                                         home_team))) %>% 
    # arrange(desc(atBatIndex), desc(pitchNumber)) %>% 
    group_by(atBatIndex) %>%
    mutate(last.pitch.of.ab =
             ifelse(pitchNumber == max(pitchNumber), "true", "false"),
           last.pitch.of.ab = factor(last.pitch.of.ab)) %>%
    ungroup()
  
  pbp <- pbp %>%
    rename(count.balls.start = count.balls.x,
           count.strikes.start = count.strikes.x,
           count.outs.start = count.outs.x,
           count.balls.end = count.balls.y,
           count.strikes.end = count.strikes.y,
           count.outs.end = count.outs.y)
  
  return(pbp)
}
