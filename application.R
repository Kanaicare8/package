# Chargement de la bibliothèque ggplot2
library(ggplot2)

# Création d'un dataframe avec des valeurs manquantes
set.seed(123)  # Pour la reproductibilité
x <- seq(-2*pi, 2*pi, length.out = 50)
y <- sin(x)
y[sample(length(y), 25)] <- NA  # Remplace 10 valeurs aléatoires par NA

# Suppression des lignes avec des valeurs manquantes pour l'interpolation
df_interp <- data.frame(x = x[!is.na(y)], y = y[!is.na(y)])

# Utilisation des fonctions d'interpolation
h <- hermite(df_interp$x, df_interp$y)
n <- newton(df_interp$x, df_interp$y)
l <- lagrange(df_interp$x, df_interp$y)

# Remplacement des valeurs manquantes par des valeurs interpolées
y_hermite <- ifelse(is.na(y), sapply(x, h), y)
y_newton <- ifelse(is.na(y), sapply(x, n), y)
y_lagrange <- ifelse(is.na(y), sapply(x, l), y)

# Création d'un dataframe pour ggplot
df_points <- data.frame(x = x, y = y)
df_hermite <- data.frame(x = x[is.na(y)], y = y_hermite[is.na(y)], method = "Hermite")
df_newton <- data.frame(x = x[is.na(y)], y = y_newton[is.na(y)], method = "Newton")
df_lagrange <- data.frame(x = x[is.na(y)], y = y_lagrange[is.na(y)], method = "Lagrange")
df_plot <- rbind(df_hermite, df_newton, df_lagrange)

# Création du graphique
p <- ggplot() +
  geom_line(data = df_points, aes(x = x, y = y, color = "données initiales")) +  # Ajout des points de données
  geom_point(data=df_plot, aes(x = x, y = y, color = method))+
  labs(color = "Méthode") +  # Ajout de la légende
  theme_minimal()+  # Thème minimal pour un rendu propre
  facet_wrap(~method,scales = "free_y")

# Affichage du graphique
print(p)

# Affichage de la matrice
print(df_plot)
