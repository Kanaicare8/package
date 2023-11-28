library(ggplot2)
library(tidyverse)
x <- seq(0, 2*pi, length.out = 20)
y <- -x^3 + (3*x-1)/(2*x^4-x^3+1)+ x^2 +1
h <- hermite(x, y)
n <- newton(x, y)
l <- lagrange(x, y)

# Génération de points supplémentaires pour le tracé
y_plot_h <- sapply(x, h)
y_plot_n <- sapply(x, n)
y_plot_l <- sapply(x, l)

# Création d'un dataframe pour ggplot
df_curve_h <- data.frame(x = x, y = y_plot_h, method = "Hermite")
df_curve_n <- data.frame(x = x, y = y_plot_n, method = "Newton")
df_curve_l <- data.frame(x = x, y = y_plot_l, method = "Lagrange")
df_curve <- rbind(df_curve_h, df_curve_n, df_curve_l)
df_points <- data.frame(x = x, y = y)


p <- ggplot() +
  geom_line(data = df_curve, aes(x = x, y = y, color = method)) +  # Ajout des courbes
  geom_point(data = df_points, aes(x = x, y = y, color = "Points de données")) +  # Ajout des points de données
  scale_color_manual(values = c("red","blue","green","black"))+
  labs(color = "Légende") +  # Ajout de la légende
  theme_minimal()+  # Thème minimal pour un rendu propre
  facet_wrap(~method)

# Affichage du graphique
print(p)

