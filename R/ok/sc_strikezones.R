#' Calculate strikezone boundaries
#'
#' @param data The data of scrape_statcast function.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @return Returns a list of boundaries for both right handed batters and left handed batters
#' @export
#'
sc_strikezones <- function(data) {
  idx <- c("sz_top", "sz_bot", "stand")
  if (!all(idx %in% colnames(data)))
    stop(paste0("'data' must have these columns: ", paste(idx, collapse = ", ")))
  data$sz_top <- as.numeric(data$sz_top)
  data$sz_bot <- as.numeric(data$sz_bot)
  bounds <- data %>%
    filter(complete.cases(.[, c("sz_top", "sz_bot")])) %>%
    group_by(stand) %>%
    summarise(
      Top = mean(sz_top) * 30.48, # feet tocm
      Bottom = mean(sz_bot) * 30.48
    ) %>%
    as.data.frame()
  righty <- as.numeric(bounds$stand == "R") * 30.48
  lefty <- as.numeric(bounds$stand == "L") * 30.48
  # bounds$Top <- righty*(2.6 + bounds$height*0.136) + lefty*(2 + bounds$height*0.229)
  # bounds$Bottom <- righty*(0.92 + bounds$height*0.136) + lefty*(0.35 + bounds$height*0.229)
  bounds$Left <- (righty + lefty) * -1
  bounds$Right <- righty + lefty
  return(bounds)
}
