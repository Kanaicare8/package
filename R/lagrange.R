#' Interpolation de Lagrange
#'
#' @param x Un vecteur des abscisses des points.
#' @param y Un vecteur des ordonnées des points.
#' @return Une fonction qui évalue le polynôme interpolant de Lagrange aux points donnés.
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(1, 4, 9)
#' f <- lagrange(x, y)
#' print(f(0.5))  # Évalue le polynôme interpolant à 0.5
#'
#' @details
#' La fonction `lagrange` prend en entrée deux vecteurs `x` et `y` représentant les points à interpoler,
#' et renvoie une fonction qui évalue le polynôme interpolant de Lagrange à un point donné.
#' Les vecteurs `x` et `y` doivent être de même longueur.
#'
#' @seealso
#' \code{\link[base]{diff}}, \code{\link[base]{seq}}
#'
#' @references
#' Burden, R. L., Faires, J. D., & Burden, A. M. (2016). Numerical Analysis. Cengage Learning.
lagrange <- function(x, y) {
  n <- length(x)
  return(function(t) {
    s <- 0
    # Calcul du polynôme interpolant de Lagrange
    for (i in 1:n) {
      p <- 1
      for (j in 1:n) {
        if (i != j) p <- p * (t - x[j]) / (x[i] - x[j])
      }
      s <- s + y[i] * p
    }
    return(s)
  })
}
