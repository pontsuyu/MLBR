#' Produce time sequenced pitch locations from statcast parameters
#'
#' @references http://baseball.physics.illinois.edu/KaganPitchfx.pdf
#'
#' @param df The data which have parameters used to determine the location of a pitch at a given time.
#' @param interval the amount of time between 'snapshots'
#' @importFrom magrittr %>%
#' @import dplyr
#' @return Return a three dimensional array.
#' @export
#'
sc_snapshots <- function(df, interval = 0.01) {
  idx <- c(
    "release_pos_x", "release_pos_y", "release_pos_z",
    "vx0", "vy0", "vz0", "ax", "ay", "az"
  )
  if (!all(idx %in% names(df))) {
    stop("'df' must have the following variables in your dataset: 
            'release_pos_x', 'release_pos_y', 'release_pos_z',
            'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  }
  df <- df %>% filter(complete.cases(.[, idx]))
  parameters <- df %>%
    select(all_of(idx)) %>%
    purrr::map_dfc(as.numeric) %>%
    as.data.frame()
  # Figure out how long it takes each pitch to reach home plate
  times <- with(parameters, (-vy0 - sqrt(vy0^2 - 2 * ay * (release_pos_y - 1.417))) / ay)
  # Number of 'snapshots' required for EVERY pitch to reach home plate
  nplots <- ceiling(max(times / interval)) + 1
  # Number of pitches (within each plot)
  npitches <- NROW(df)
  t.matrix <- matrix(rep(0:(nplots - 1) * interval, times = npitches), byrow = TRUE, nrow = npitches)
  # Restrict time if ball already crossed home plate
  t <- pmin(t.matrix, times)
  # t <- ifelse(t.matrix<times, t.matrix, NA)
  # Repeat the parameters for the total amount of plots needed
  snapshots <- array(rep(c(parameters, recursive = TRUE), nplots), dim = c(dim(parameters), nplots))
  # Horizontal location at time t
  x <- snapshots[, 1, ] + snapshots[, 4, ] * t + 0.5 * snapshots[, 7, ] * t^2
  # Distance from batter at time t
  y <- snapshots[, 2, ] + snapshots[, 5, ] * t + 0.5 * snapshots[, 8, ] * t^2
  # Height from ground at time t
  z <- snapshots[, 3, ] + snapshots[, 6, ] * t + 0.5 * snapshots[, 9, ] * t^2
  # convert feet to cm
  return(array(c(x, y, z), dim = c(npitches, nplots, 3)) * 30.48)
}
