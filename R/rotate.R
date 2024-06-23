rotate <- function(x, angle, radians = T) {
  angle <- ifelse(radians, angle, angle * pi / 180)

  matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow = 2, ncol = 2, byrow = T) %*% x
}
