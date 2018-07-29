#' Add batter box layer.
#'
#' @import ggplot2
#' @return ggplot theme
#'
#' @export
#' 
theme_batterbox <- function() {
  width <- 21.6
  homebase <- data.frame(x=c(-width, width, width, 0, -width), 
                         y=c(width*2, width*2, width, 0, width),
                         stringsAsFactors = F)
  
  markings <- list(
    geom_polygon(data = homebase, aes(x, y)),
    # batter box(right)
    geom_rect(xmin = -width-15-121, xmax = -width-15,
              ymin = width-91.5, ymax = width+91.5,
              fill="transparent", color="black"),
    # batter box(left)
    geom_rect(xmin = width+15, xmax = width+15+121,
              ymin = width-91.5, ymax = width+91.5,
              fill="transparent", color="black"),
    xlim(-200, 200),
    ylim(-50, 1800),
    xlab("Horizontal Distance(cm)"),
    ylab("Vertical Distance(cm)"),
    theme_bw()
  )
  return(markings)
}
