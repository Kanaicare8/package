#' Interpolation polynomiale par la méthode de Newton
#'
#' @param x Un vecteur de valeurs x
#' @param y Un vecteur de valeurs y correspondant aux valeurs x
#' @param newx Un vecteur de nouvelles valeurs x pour lesquelles nous voulons interpoler les valeurs y
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(1, 4, 9)
#' newx <- c(2.5)'
#' newy <- newton_interpolation(x, y, newx)'
#' print(newy)
#'
#' @return Un vecteur de nouvelles valeurs y interpolées pour les nouvelles valeurs x
newton <- function(x, y) {
  n <- length(x)
  f <- matrix(0, n, n)
  f[,1] <- y
  for (j in 2:n) {
    for (i in j:n) {
      f[i,j] <- (f[i,j-1] - f[i-1,j-1]) / (x[i] - x[i-j+1])
    }
  }
  function(t) {
    sum <- f[n,n]
    for (i in (n-1):1) {
      sum <- sum * (t - x[i]) + f[i,i]
    }
    sum
  }
}
