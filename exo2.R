# Création de la matrice
A <- rbind(c(-6, -3, 6, 1),
           c(-1, 2, 1, -6),
           c(3, 6, 3, -2),
           c(6, -3, 6, -1),
           c(2, -1, 2, 3),
           c(-3, 6, 3, 2),
           c(-2, -1, 2, -3),
           c(1, 2, 1, 6))



##################################################
# PARTIE 1
produits_scalaires <- crossprod(A)
mask <- upper.tri(produit_scalaire)
ortho <- all(produit_scalaire[mask] == 0)

if (ortho) {
  print("Toutes les colonnes sont orthogonales.")
} else {
  print("Toutes les colonnes ne sont pas orthogonales.")
}
##################################################



##################################################
# PARTIE 2
produits_scalaires
