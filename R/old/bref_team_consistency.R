#' Calculate Team-level Consistency
#'
#' This function allows you to calculate team-level consistency in run scoring and run prevention over the course of an entire season.
#' @param year Season consistency should be run for.
#' @keywords MLB, sabermetrics
#' @importFrom reldist gini
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#'
#' @examples \dontrun{bref_team_consistency(2015)}

#load packages: currently requires XML to scrape, dplyr to tidy, reldist for Gini calculations. Will be switching to rvest for scraping

bref_team_consistency <- function(year) {

  url <- paste0("http://www.baseball-reference.com/leagues/MLB/",year,".shtml")
  teams <- xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    .[1] %>%
    html_table() %>%
    as.data.frame() %>% 
    dplyr::filter(!Tm %in% c("LgAvg", "Tm", "")) %>% 
    pull(Tm)
  results <- teams %>%
    map_df(~bref_team_results(.x, year))

  results <- results %>% 
    dplyr::select(Year, Date, Team = Tm, R, RA) %>% 
    mutate(R = as.numeric(R),
           RA = as.numeric(RA)) %>% 
    dplyr::filter(!is.na(R))
  RGini <- results %>%
    group_by(Team) %>%
    summarize(R = gini(R))
  RAGini <- results %>%
    group_by(Team) %>%
    summarize(RA = gini(RA))
  VOL <- left_join(RGini, RAGini, by = "Team")
  VOL$R <- round(VOL$R, 2)
  VOL$RA <- round(VOL$RA, 2)
  colnames(VOL)[1] <- "bref_t"
  VOL <- VOL %>%
    mutate(percrank = rank(R)/length(R))
  colnames(VOL)[4] <- "R_Ptile"
  VOL <- VOL %>%
    mutate(percrank = rank(RA)/length(RA))
  colnames(VOL)[5] <- "RA_Ptile"
  VOL$R_Ptile <- round(VOL$R_Ptile, 2)*100
  VOL$RA_Ptile <- round(VOL$RA_Ptile, 2)*100
  colnames(VOL) <- c("Team", "Con_R", "Con_RA", "Con_R_Ptile", "Con_RA_Ptile")

  return(VOL)
}

