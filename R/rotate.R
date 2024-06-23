#' Rotate cartesian coordinates around 0
#'
#' @param x Matrix of coordinates to rotate. Should be either a numeric vector of length 2, or a matrix with two rows (for rotating multiple coordinates at once)
#' @param angle The angle to rotate by. Default is radians.
#' @param radians Logical indicating if the given angle is in radians. If FALSE, degrees are used.
#'
#' @return A numeric matrix of rotated coordinates. Contains two rows (x and y coordinates) and one column per input coordinate.
#' @export
#'
#' @examples
#' rotate(c(1, 2), pi)
#'
#' rotate(matrix(c(1, 2,
#'                 3, 4),
#'                 byrow = FALSE, nrow = 2),
#'        angle = 90, radians = FALSE)
rotate <- function(x, angle, radians = T) {
  angle <- ifelse(radians, angle, angle * pi / 180)

  matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow = 2, ncol = 2, byrow = T) %*% x
}
