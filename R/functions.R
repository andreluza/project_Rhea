# binomial smooth 
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),...)
  # geom_smooth(method = "glm", method.args = list(family = "poisson"), ...) # glm option
}


# extract legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}