#' Get player list
#'
#' @export
#' @examples
#' \dontrun{get_playerlist()}
#' 
get_playerlist <- function(){
  # read.csv("http://crunchtimebaseball.com/master.csv", stringsAsFactors = F)
  data.table::fread("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv", stringsAsFactors = F, data.table = F)
}
