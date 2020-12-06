#' Generate linear weight values for events using Baseball Savant data
#'
#' This function allows a user to generate linear weight values for events using Baseball Savant data. Output includes both linear weights above average and linear weights above outs for home runs, triples, doubles, singles, walks, hit by pitches, and outs.
#' @param df A data frame generated from Baseball Savant that has been run through
#' the \code{\link{run_expectancy_code}} function.
#' @param level Whether to calculate linear weights the plate appearance or pitch level. "PA" or "Pitch"
#' 
#' @import dplyr
#' @export
#' @examples
#' \dontrun{sc_linear_weights(df, level = "PA")}

sc_linear_weights <- function(df, level){
  
  switch(level,
         PA = {
           non_outs <- c("home_run", "triple", "double", "single",
                         "walk", "hit_by_pitch", "pickoff_2B", "caught_stealing_2b",
                         "caught_stealing_3b", "caught_stealing_home", "pickoff_1b",
                         "pickoff_3b", "pickoff_caught_stealing_2b",
                         "pickoff_caught_stealing_home", "catcher_interf",
                         "batter_interference")
           df <- df %>%
             filter(final_pitch_at_bat == 1)
           
           linear_wght_outs <- df %>%
             filter(!events %in% non_outs) %>%
             summarise(outs = round(mean(re24, na.rm = TRUE), 2))
           linear_above_outs <- df %>%
             filter(events %in% c("home_run", "triple", "double", "single", "walk", "hit_by_pitch")) %>%
             group_by(events) %>%
             summarise(linear_weights_above_average =
                         round(mean(re24,na.rm = TRUE), 2)) %>%
             add_row(events = "outs",
                     linear_weights_above_average = linear_wght_outs$outs) %>%
             arrange(desc(linear_weights_above_average)) %>%
             mutate(linear_weights_above_outs = linear_weights_above_average +
                      abs(linear_wght_outs$outs))
           return(linear_above_outs)
         },
         Pitch = {
           
           df <- df %>%
             mutate(events = ifelse(is.na(events), type, events),
                    events = ifelse(events == "B", "ball",
                                    ifelse(events == "S", "strikes", events)))
           
           non_outs <- c("ball", "strikes", "home_run", "triple", "double", "single",
                         "walk", "hit_by_pitch", "pickoff_2B", "caught_stealing_2b",
                         "caught_stealing_3b", "caught_stealing_home", "pickoff_1b",
                         "pickoff_3b", "pickoff_caught_stealing_2b",
                         "pickoff_caught_stealing_home", "catcher_interf",
                         "batter_interference")
           
           linear_wght_outs <- df %>%
             filter(!events %in% non_outs) %>%
             summarise(outs = round(mean(re24, na.rm = TRUE), 2))
           linear_above_outs <- df %>%
             filter(events %in% c("home_run", "triple", "double", "single", "walk", "hit_by_pitch", "ball", "strikes")) %>%
             group_by(events) %>%
             summarise(linear_weights_above_average =
                         round(mean(re24,na.rm = TRUE), 2)) %>%
             add_row(events = "outs",
                     linear_weights_above_average = linear_wght_outs$outs) %>%
             arrange(desc(linear_weights_above_average)) %>%
             mutate(linear_weights_above_outs = linear_weights_above_average +
                      abs(linear_wght_outs$outs))
           return(linear_above_outs)
         }
  )
}
