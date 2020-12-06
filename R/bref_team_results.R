#' This function allows you to scrape schedule and results for a major league team from Baseball-Reference.com
#'
#' @param Team The abbreviation used by Baseball-Reference.com for the team whose results you want to scrape.
#' @param year Season for which you want to scrape the park factors.
#' @import dplyr
#' @export
#' @examples
#' bref_team_results("NYM", 2015)
#' bref_team_results("TBR", 2008)
bref_team_results <- function(Team, year) {
  url <- paste0(
    "https://www.baseball-reference.com/teams/", Team,
    "/", year, "-schedule-scores.shtml")
  data <- url %>%
    xml2::read_html() %>%
    rvest::html_nodes("table") %>%
    .[[length(.)]] %>%
    rvest::html_table()
  colnames(data) <- c(
    "Gm", "Date", "boxscore", "Tm", "Home_Away", "Opp", "Result", "R", "RA",
    "Inn", "Win_Lose", "Rank", "GB", "Win", "Loss", "Save",
    "Time", "Day_Night", "Attendance", "cLI", "Streak", "Orig_Scheduled"
  )
  data <- data %>%
    select(-boxscore, -cLI) %>%
    mutate(Year = year)
  data$Home_Away <- ifelse(grepl("@", data$Home_Away, fixed = TRUE), "A", "H")
  data$Attendance <- gsub(",", "", data$Attendance)
  data$Streak <- ifelse(grepl("-", data$Streak, fixed = TRUE),
                        nchar(data$Streak) * -1,
                        nchar(data$Streak) * 1
  )
  
  for (i in c("R", "Rank", "Attendance"))
    if (!is.numeric(data[, i])) data[, i] <- suppressWarnings(as.numeric(data[, i]))
  data <- data %>% filter(Gm !="Gm#")
  
  return(data)
}
