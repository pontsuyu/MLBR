#' Calculate FIP and related metrics for any set of data
#'
#' This function allows you to calculate FIP and related metrics for any given set of data, provided the right variables are in the data set. The function currently returns both FIP per inning pitched, wOBA against (based on batters faced), and wOBA against per instance of fair contact.
#'
#' @param df A data frame of statistics that includes, at a minimum, the following columns: IP (innings pitched), BF (batters faced), uBB (unintentional walks), HBP (Hit By Pitch), x1B (singles), x2B (doubles), x3B (triples), HR (home runs), AB (at-bats), SH (sacrafice hits), SO (strike outs), and season.
#' 
#' @export

fg_fip_add <- function(df) {
  df$season <- as.character(df$season)
  guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>%
    html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE) %>% 
    as.data.frame() %>% 
    .[-(1:2), (1:14)]
  name <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  colnames(guts_table) <- name
  for(i in c(2:ncol(guts_table)))
    guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
  
  df_join <- df %>% 
    left_join(guts_table, by = "season") %>% 
    mutate(FIP = round(((((13*HR) + (3*(uBB + HBP)) - (2*SO))/IP) + cFIP), 2),
           wOBA_against <- round((((wBB * uBB) + (wHBP * HBP) + (w1B * X1B) + (w2B * X2B) + (w3B * X3B) + (wHR * HR))/(BF)),3),
           wOBA_CON_against <- round((((w1B * X1B) + (w2B * X2B) + 	(w3B * X3B) + (wHR * HR))/(AB - SO)),3)) %>% 
    arrange(desc(wOBA_against))
  x <- colnames(df_join) %in% name[-1]
  return(df_join[!x])
}
