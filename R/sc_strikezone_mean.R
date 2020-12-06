#' Calculate strikezone boundaries
#'
#' @param df The data of scrape_statcast function.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @return Returns a list of boundaries for both right handed batters and left handed batters
#' @export
#'
sc_strikezone_mean <- function(df) {
  idx <- c("sz_top", "sz_bot", "stand")
  if (!all(idx %in% colnames(df)))
    stop(paste0("'df' must have these columns: ", paste(idx, collapse = ", ")))
  df$sz_top <- as.numeric(df$sz_top)
  df$sz_bot <- as.numeric(df$sz_bot)
  bounds <- df %>%
    filter(complete.cases(.[, c("sz_top", "sz_bot")])) %>%
    group_by(stand) %>%
    summarise(
      Top = mean(sz_top), # feet to cm
      Bottom = mean(sz_bot)
    ) %>%
    as.data.frame()
  righty <- as.numeric(bounds$stand == "R")
  lefty <- as.numeric(bounds$stand == "L")
  # bounds$Top <- righty*(2.6 + bounds$height*0.136) + lefty*(2 + bounds$height*0.229)
  # bounds$Bottom <- righty*(0.92 + bounds$height*0.136) + lefty*(0.35 + bounds$height*0.229)
  bounds$Left <- (righty + lefty) * -1
  bounds$Right <- righty + lefty
  return(bounds)
}
