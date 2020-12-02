#' Scrape FanGraphs.com Guts!
#'
#' @description This function allows you to scrape the historical, season-by-season wOBA and FIP constants and coefficients at FanGraphs.com.
#' @param year Season for which you want to scrape the park factors.
#' @keywords MLB, sabermetrics
#' @importFrom rvest html_node
#' @export
#' @examples
#' fg_guts("2018")

# scrape historical FanGraphs Guts! table
# wOBA and FIP coefficients and constants
fg_guts <- function(year) {
  woba_fip_cons <- read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>%
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
    html_table %>%
    setNames(c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
               "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
  
  park <- read_html(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", year)) %>%
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
    html_table %>%
    setNames(c("season", "home_team", "basic_5yr", "3yr", "1yr", "single", "double", "triple", "hr",
               "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP"))
  
  park_hand <- read_html(paste0("http://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season=", year)) %>%
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
    html_table %>%
    stats::setNames(c("season", "home_team", "single_as_LHH", "single_as_RHH",
                      "double_as_LHH", "double_as_RHH", "triple_as_LHH", "triple_as_RHH",
                      "hr_as_LHH", "hr_as_RHH"))
  
  return(list(woba_fip_cons = woba_fip_cons, park = park, park_hand = park_hand))
}
