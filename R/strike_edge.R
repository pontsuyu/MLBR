#' Edge Code
#'
#' This function allows you to classify individual pitches based on the various categories from the Edge% metric. The dataframe passed to the function must include the batter's handedness, the px and pz coordinates from the PITCHf/x system, and the batter's height.
#' @param df A dataframe that, at a minimum, includes the following columns: batter height (b_height), the batter's handedness (stand), vertical location of the pitch (pz), and then horizontal location of the pitch (pz)
#' @param height_var_name The name of the variable in the dataset that includes the batter's height. Defaults to b_height which assumes an height + inch format. If the variable name is "Height" it assumes the variable is already converted to inches (as is the case in some databases)
#' 
#' @import dplyr
#' @export
#' @examples \dontrun{strike_edge_add(df)}

strike_edge_add <- function(df, height_var_name = "b_height") {
  if (height_var_name == "b_height") {
    if (class(df$px) == "factor") df$px <- as.numeric(levels(df$px))[df$px]
    if (class(df$pz) == "factor") df$pz <- as.numeric(levels(df$pz))[df$pz]
    if (class(df$b_height) == "factor") df$b_height <- as.numeric(levels(df$b_height))[df$b_height]
    df <- df %>% 
      separate(b_height, c("f", "i"), sep = "-") %>% 
      mutate(b_height_inch = as.numeric(f) * 12 + as.numeric(i),
             called_pitch = ifelse(grepl("Called|Ball", des2, gnore.case = TRUE), 1, 0),
             called_strike = ifelse(grepl("Called", des2, gnore.case = TRUE), 1, 0),
             swing = ifelse(grepl("Swinging|Foul|In play", des2, gnore.case = TRUE), 1, 0),
             whiff = ifelse(grepl("Swinging", des2, gnore.case = TRUE), 1, 0))
  } else {
    if (class(df$px) == "factor") df$px <- as.numeric(levels(df$px))[df$px]
    if (class(df$pz) == "factor") df$pz <- as.numeric(levels(df$pz))[df$pz]
    df <- df %>% 
      rename(b_height_inch = Height) %>% 
      mutate(called_pitch = ifelse(grepl("Called|Ball", descriptioni, gnore.case = TRUE), 1, 0),
             called_strike = ifelse(grepl("Called", description, ignore.case = TRUE), 1, 0),
             swing = ifelse(grepl("Swinging|Foul|In play", description, ignore.case = TRUE), 1, 0),
             whiff = ifelse(grepl("Swinging", description, ignore.case = TRUE), 1, 0))
  }
  LHH <- df %>% 
    filter(stand == "L") %>% 
    mutate(location = case_when(
      !is.na(px) & !is.na(pz) & px > .21 & px < .81 & 
        pz > (.35 + b_height_inch/12 *.229) & 
        pz < (2.0 + b_height_inch/12 *.229) ~ "Inside Edge",
      !is.na(px) & !is.na(pz) & px > -1.20 & px < -0.9 &
        pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229) ~ "Outside Edge",
      !is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 &
        pz > (1.7 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229) ~ "Upper Edge",
      !is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & 
        pz > (.35 + b_height_inch/12 *.229) & pz < (.65 + b_height_inch/12 *.229) ~ "Lower Edge",
      !is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & 
        pz >= (.65 + b_height_inch/12 *.229) & pz <= (1.7 + b_height_inch/12 *.229) ~ "Heart", 
      is.na(px) | is.na(pz) ~ NA,
      TRUE ~ "Out of Zone"))
  
  RHH <- df %>% 
    filter(stand == "R") %>% 
    mutate(location = case_when(
      !is.na(px) & !is.na(pz) & px > -1.03 & px < -.43 & 
        pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136) ~ "Inside Edge",
      !is.na(px) & !is.na(pz) & px > .7 & px < 1.00 &
        pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136) ~ "Outside Edge",
      !is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 &
        pz > (2.3 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136) ~ "Upper Edge",
      !is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & 
        pz > (.92 + b_height_inch/12 *.136) & pz < (1.22 + b_height_inch/12 *.136) ~ "Lower Edge", 
      !is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & 
        pz >= (1.22 + b_height_inch/12 *.136) & pz <= (2.30 + b_height_inch/12 *.136) ~ "Heart",
      is.na(px) | is.na(pz) ~ NA,
      TRUE ~ "Out of Zone"))
  
  df_combined <- rbind(LHH, RHH) %>% 
    mutate(Upper_Edge = ifelse(location == "Upper Edge", 1, 0),
           Lower_Edge = ifelse(location == "Lower Edge", 1, 0),
           Inside_Edge = ifelse(location == "Inside Edge", 1, 0),
           Outside_Edge = ifelse(location == "Outside Edge", 1, 0),
           Heart = ifelse(location == "Heart", 1, 0),
           OutOfZone = ifelse(location == "Out of Zone", 1, 0))
  
  return(df_combined)
}

#' Edge Percentage Frequency
#'
#' This function allows you to calculate the percent of pitches thrown to different edges of the strike zone for a pitch by pitch data set that has been coded using the edge_code() function.
#' @param df A data frame of pitch by pitch data that has been coded using the edge_code() function.
#' @param group Character string indicating what column to group the frequency by. For example, "pitcher" or "batter". Defaults to NULL, which calculates the frequencies across the entire data set.
#' 
#' @import dplyr
#' @examples \dontrun{strike_edge_freq(df, group = "pitcher")}
strike_edge_freq <- function(df, group = NULL) {
  if (is.null(group)) {
    tmp <- df %>% 
      filter(!is.na(px), !is.na(pz))
  } else {
    tmp <- df %>% 
      filter(!is.na(px), !is.na(pz)) %>%
      group_by_at(group)
  } 
  tmp %>% 
    summarise(All_pitches = n(),
              All_calls = sum(called_pitch),
              Called_Strike = sum(called_strike), 
              Called_strike_rate = round(sum(called_strike)/sum(called_pitch),3), 
              Upper_Edge = sum(Upper_Edge)/All_pitches, 
              Lower_Edge = sum(Lower_Edge)/All_pitches, 
              Inside_Edge = sum(Inside_Edge)/All_pitches,
              Outside_Edge = sum(Outside_Edge)/All_pitches, 
              Heart = sum(Heart)/All_pitches, Out_of_Zone = sum(OutOfZone)/All_pitches) %>%
    mutate(Total_Edge = Upper_Edge + Lower_Edge + Inside_Edge + Outside_Edge)
}
