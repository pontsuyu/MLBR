#' Get game_ids in specified years
#'
#' @param start_year integer. Over 2008
#' @param end_year integer.
#' @import tidyverse
#' @import rvest
#' @import foreach
#'
#' @return game_id vector
#' @export
#'
get_gameids <- function(start_year = 2018L, end_year = 2018L){
  `%+%` <- function(x, y) paste0(x, y)
  if (!is.integer(start_year) || !is.integer(end_year))
    stop("'start/end_year' is NOT integer. example: 2018L")
  if (start_year < 2008 || end_year > lubridate::year(Sys.Date()))
    stop("'start/end_year' must be between 2008 and " %+% lubridate::year(Sys.Date()))
  date_seq <- seq(as.Date(start_year %+% "-02-01"),
                  as.Date(end_year %+% "-10-31"), by = "day")
  date <- data.frame(date = date_seq, stringsAsFactors = F) %>%
            separate(date, c("year", "month", "day"), sep = "-")
  candidate <- "http://gd2.mlb.com/components/game/mlb/year_" %+% 
                 date[["year"]] %+% "/month_" %+% date[["month"]] %+%
                 "/day_" %+% date[["day"]]
  
  res_list <- foreach(i=1:length(candidate)) %do% {
    suppressWarnings({
      tmp <- try(read_html(candidate[i]), silent = T)
      if (class(tmp)[1] != "try-error") {
        tmp %>%
          html_nodes("li") %>%
          html_text() %>%
          str_subset("^ gid_") %>%
          str_subset(".*mlb.*") %>%
          str_replace_all(" |/", "")
      }
    })
  }
  return(do.call("c", res_list))
}
