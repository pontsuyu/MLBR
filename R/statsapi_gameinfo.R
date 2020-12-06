#' Find game_pk values for professional baseball games (major and minor leagues)
#' via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param year The year for which you want to find game_pk values for MLB games
#' @param level_ids A numeric vector with ids for each level where game_pks are
#' desired. See below for a reference of level ids.
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes game_pk values and additional
#' information for games scheduled or played
#' 
#' @section Level IDs:
#'
#' The following IDs can be passed to the level_ids argument:
#'
#' 1 = MLB
#' 11 = Triple-A
#' 12 = Doubl-A
#' 13 = Class A Advanced
#' 14 = Class A
#' 15 = Class A Short Season
#' 5442 = Rookie Advanced
#' 16 = Rookie
#' 17 = Winter League
#' @export
#'
#' @examples \dontrun{statsapi_gameinfo(2018:2019, 1))}

statsapi_gameinfo <- function(year, level_ids = c(1,11:17,5442)) {
  dates <- year %>% 
    map(~as.Date(paste0(.x,"-01-01")) %>% 
          seq.Date(by = "day", length.out = 365) %>% 
          as.character()) %>% 
    unlist()
  
  res <- dates %>% 
    map(~api_func(.x, level_ids)) %>% 
    bind_rows()
  
  return(res)
}

api_func <- function(date, level_ids){
  api_call <- paste0("http://statsapi.mlb.com/api/v1/schedule?sportId=", 
                     paste(level_ids, collapse = ','), "&date=", date)
  payload <- fromJSON(api_call, flatten = TRUE)
  if(length(payload$dates)!=0){
    gameinfo <- payload$dates$games %>%
      as.data.frame() %>%
      rename(game_pk = gamePk)
    return(gameinfo)
  }
}
