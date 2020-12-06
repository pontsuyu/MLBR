#' Generate run expectancy and related measures and variables from Baseball Savant data
#'
#' These functions allow a user to generate run expectancy and related measures and variables from Baseball Savant data.
#' Measures and variables will be added to the data frame and a run expectancy table will be assigned to the Global Environment.
#' @param df A data frame generated from Baseball Savant.
#' @param level Whether you want run expectancy calculated at the plate appearance or pitch level. Defaults to plate appearance.
#' 
#' @import dplyr
#' @importFrom stringr str_count
#' @export
#' @examples
#' \dontrun{sc_run_expectancy(df, level = "PA")}

sc_run_expectancy <- function(df, level) {
  # single_outs <- c("strikeout", "caught_stealing_2b", "pickoff_caught_stealing_2b",
  #                  "other_out", "caught_stealing_3b", "caught_stealing_home",
  #                  "field_out", "force_out", "pickoff_1b", "batter_interference",
  #                  "fielders_choice", "pickoff_2b", "pickoff_caught_stealing_3b",
  #                  "pickoff_caught_stealing_home")
  df <- df %>%
    arrange(game_pk, at_bat_number, pitch_number) %>%
    group_by(game_pk) %>%
    mutate(final_pitch_game =
             ifelse(pitch_number == max(pitch_number), 1, 0)) %>%
    ungroup() %>%
    group_by(game_pk, inning_topbot, at_bat_number) %>%
    mutate(final_pitch_at_bat =
             ifelse(pitch_number == max(pitch_number), 1, 0)) %>%
    ungroup()
  
  df <- df %>%
    arrange(game_pk, inning_topbot, at_bat_number, pitch_number) %>%
    mutate(runs_scored_on_pitch = stringr::str_count(des, "scores"),
           runs_scored_on_pitch = ifelse(events == "home_run", runs_scored_on_pitch + 1, runs_scored_on_pitch),
           bat_score_after = bat_score + runs_scored_on_pitch) %>%
    arrange(game_pk, at_bat_number, pitch_number) %>%
    mutate(final_pitch_inning = ifelse(final_pitch_at_bat == 1 & inning_topbot != lead(inning_topbot), 1, 0),
           final_pitch_inning = ifelse(is.na(final_pitch_inning), 1, final_pitch_inning))
  
  switch(level,
         PA = {
           df <- df %>%
             group_by(game_pk, inning, inning_topbot) %>%
             mutate(bat_score_start_inning = min(bat_score),
                    bat_score_end_inning = max(bat_score),
                    cum_runs_in_inning = cumsum(runs_scored_on_pitch),
                    runs_to_end_inning = bat_score_end_inning - bat_score) %>%
             ungroup() %>%
             mutate(base_out_state123 = paste0(outs_when_up, " ", 
                                               (!is.na(on_1b))*1,
                                               (!is.na(on_2b))*1,
                                               (!is.na(on_3b))*1))
           
           re_table <- run_expectancy_table(df, level = "PA") %>% 
             arrange(base_out_state123)
           
           df <- df %>% 
             left_join(re_table, by = "base_out_state123") %>% 
             filter(final_pitch_at_bat == 1) %>%
             arrange(game_pk, inning, inning_topbot) %>%
             group_by(game_pk, inning, inning_topbot) %>%
             mutate(next_base_out_state123 = lead(base_out_state123)) %>%
             ungroup() %>% 
             left_join(re_table, by = c(next_base_out_state123 = "base_out_state123")) %>%
             rename(next_avg_re = avg_re.y, avg_re = avg_re.x) %>%
             mutate(next_avg_re = ifelse(is.na(next_avg_re), 0, next_avg_re),
                    change_re = next_avg_re - avg_re,
                    re24 = change_re + runs_scored_on_pitch) %>%
             arrange(game_pk, inning, inning_topbot)
           return(list(data = df, RE = re_table))
           
         },
         Pitch = {
           df <- df %>%
             group_by(game_pk, inning, inning_topbot) %>%
             mutate(bat_score_start_inning = min(bat_score),
                    bat_score_end_inning = max(bat_score),
                    cum_runs_in_inning = cumsum(runs_scored_on_pitch),
                    runs_to_end_inning = bat_score_end_inning - bat_score) %>%
             ungroup() %>%
             mutate(count_base_out_state123 = paste0(balls, "-", strikes, " ",
                                                     outs_when_up, " ", 
                                                     (!is.na(on_1b))*1,
                                                     (!is.na(on_2b))*1,
                                                     (!is.na(on_3b))*1))
           
           re_table <- run_expectancy_table(df, level = "Pitch") %>% 
             arrange(count_base_out_state123)
           
           df <- df %>% 
             left_join(re_table, by = "count_base_out_state123") %>%
             arrange(game_pk, inning, inning_topbot) %>%
             group_by(game_pk, inning, inning_topbot) %>%
             mutate(next_count_base_out_state123 = lead(count_base_out_state123)) %>%
             ungroup() %>%
             left_join(re_table, by = c(next_count_base_out_state123 = "count_base_out_state123")) %>%
             rename(next_avg_re = avg_re.y, avg_re = avg_re.x) %>%
             mutate(next_avg_re = ifelse(is.na(next_avg_re), 0, next_avg_re),
                    change_re = next_avg_re - avg_re,
                    runs_scored_on_pitch = ifelse(is.na(runs_scored_on_pitch), 0, runs_scored_on_pitch),
                    re24 = change_re + runs_scored_on_pitch) %>%
             arrange(game_pk, inning, inning_topbot)
           return(list(data = df, RE = re_table))
         }
  )
  
}


#' Generate run expectancy tables from Baseball Savant data
#'
#' These functions allow a user to generate run expectancy tables from Baseball Savant data.
#' Tables are automatically assigned to the Global Environment.
#' @param df A data frame generated from Baseball Savant that has been formatted using the run_expectancy_code() function.
#' @param level Whether you want run expectancy calculated at the plate appearance or pitch level. "PA" or "Pitch"
#' 
#' @import dplyr
#' 
#' @examples
#' \dontrun{run_expectancy_table(df, level = "PA")}
run_expectancy_table <- function(df, level) {
  switch(level,
         PA = df %>%
           filter(final_pitch_at_bat == 1, inning < 9) %>%
           group_by(base_out_state123) %>%
           summarise(avg_re = mean(runs_to_end_inning, na.rm = TRUE)) %>%
           arrange(desc(avg_re)),
         Pitch = df %>%
           filter(inning < 9) %>%
           group_by(count_base_out_state123) %>%
           summarise(avg_re = mean(runs_to_end_inning, na.rm = TRUE)) %>%
           arrange(desc(avg_re))
  )
}
