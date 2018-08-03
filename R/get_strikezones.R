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
get_strikezones <- function(data) {
  idx <- c("sz_top", "sz_bot", "stand")
  if(!all(idx %in% colnames(data)))
      stop(paste0("'data' must have these columns: ", paste(idx, collapse = ", ")))
  for(i in c("sz_top", "sz_bot")) data[,i] <- as.numeric(data[,i])
  bounds <- data %>% 
    filter(complete.cases(.[,c("sz_top", "sz_bot")])) %>% 
    # mutate(sz_top = sz_top + sz_bot) %>%
    group_by(stand) %>% 
    summarise(Top = mean(sz_top) * 30.48, 
              Bottom = mean(sz_bot) * 30.48
              ) %>% 
    as.data.frame
  righty <- as.numeric(bounds$stand == "R") * 30.48
  lefty <- as.numeric(bounds$stand == "L") * 30.48
  # bounds$Top <- righty*(2.6 + bounds$height*0.136) + lefty*(2 + bounds$height*0.229)
  # bounds$Bottom <- righty*(0.92 + bounds$height*0.136) + lefty*(0.35 + bounds$height*0.229)
  bounds$Left <- (righty + lefty)*-1
  bounds$Right <- righty + lefty
  return(bounds)
}