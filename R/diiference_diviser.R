#' Calcul de la différence divisée pour l'interpolation de Newton
#'
#' @param x Un vecteur de valeurs x
#' @param y Un vecteur de valeurs y correspondant aux valeurs x
#' @return La différence divisée des valeurs y
diff_div <- function(x, y) {
  # Obtenir le nombre de valeurs y
  n <- length(y)

  # Si n est égal à 1, retourner la valeur y
  if (n == 1) {
    return(y[1])
  } else {
    # Sinon, calculer la différence divisée récursivement
    return((diff_div(x[-1], y[-1]) - diff_div(x[-n], y[-n])) / (x[n] - x[1]))
  }
}
