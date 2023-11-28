#' Fonction d'interpolation d'Hermite
#'
#' @param x Un vecteur de points x.
#' @param y Un vecteur de points y.
#' @return Une fonction qui évalue l'interpolant d'Hermite aux points donnés.
#' @examples
#' # Génération de 40 points entre -2*pi et 2*pi
#' x <- runif(min=-1,max=1,n=40)
#' #x <- seq(-2*pi, 2*pi, length.out = 400)
#' y <- exp(x)
#' # Utilisation de la fonction d'interpolation d'Hermite
#' h <- hermite(x, y)
#' # Génération de points supplémentaires pour le tracé
#' #y_plot <- sapply(x_plot, h)
#' # Création d'un dataframe pour ggplot
#' df_curve <- data.frame(x = x, y = y)
#' df_points <- data.frame(x = x, y = y)
#' # Création du graphique
#' p <- ggplot() +
#'   geom_line(data = df_curve, aes(x = x, y = y, color = "Interpolation d'Hermite")) +  # Ajout de la courbe
#'     geom_point(data = df_points, aes(x = x, y = y, color = "Points de données")) +  # Ajout des points de données
#'     labs(color = "Légende") +  # Ajout de la légende
#'     theme_minimal()  # Thème minimal pour un rendu propre
#'  # Affichage du graphique
#'  print(p)




hermite <- function(x, y) {
  # Vérifie que les longueurs des vecteurs sont cohérentes
  stopifnot(length(x) == length(y))

  # Calcule les dérivées en utilisant la méthode des différences finies
  yp <- c(diff(y)/diff(x), 0)

  # Calcule le nombre de points
  n <- length(x)

  # Crée la fonction d'interpolation
  function(t) {
    # Initialise la somme
    sum <- 0

    # Boucle sur chaque point
    for (i in 1:n) {
      # Calcule le produit pour ce point
      prod <- 1
      for (j in 1:n) {
        if (i != j) {
          prod <- prod * ((t - x[j])^2 * (2*(x[i] - t)/(x[i] - x[j]) + 1))/((x[i] - x[j])^2)
        }
      }

      # Ajoute à la somme
      sum <- sum + y[i] * prod
    }

    # Retourne la somme
    return(sum)
  }
}
