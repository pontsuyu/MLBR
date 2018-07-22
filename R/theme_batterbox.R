#' Adds batter box for use in a ggplot plot.
#'
#' @return list of ggplot geoms to be added to a ggplot plot
#'
#' @examples
#' library(ggplot2)
#' shots_data <- data.frame(x = c(90, 85, 82, 78, 83),
#'                          y = c(43, 40, 52, 56, 44))
#' ggplot(shots_data, aes(x = x, y = y)) +
#'   theme_batterbox()
#'
#' @export
theme_batterbox <- function() {
  kizyun <- 18.44*3.28084 - 54.53
  homebase <- data.frame(x=c(-.71,.71,.71,0,-.71), 
                         y=c(kizyun,kizyun,kizyun-3,1.417,kizyun-3),
                         stringsAsFactors = F)
  
  markings <- list(
    geom_polygon(data = homebase, aes(x*30.48, y*30.48)),
    # batter box(right)
    geom_rect(xmin = -36.88, xmax = -36.88-100,
              ymin = -150, ymax = 400,
              fill="transparent", color="black"),
    # batter box(left)
    geom_rect(xmin = 36.88, xmax = 36.88+100,
              ymin = -150, ymax = 400,
              fill="transparent", color="black"),
    
    xlim(-200,200),
    ylim(-200,1800),
    xlab(NULL),
    ylab(NULL),
    theme_bw()
  )
  
  return(markings)
}
