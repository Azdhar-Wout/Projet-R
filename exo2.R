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
produits_scalaires <- function(A) {
  U <- apply(
    A, # base matrix
    2, # 1 for row, 2 for col
    function(my_column) my_column / sqrt(sum(my_column^2))
  )
  return(U)
}

# Construire la matrice U
U <- produits_scalaires(A)
print(U)
##################################################



##################################################
# PARTIE 3
U_t = t(U)
print(round(U_t %*% U, 2)) # -> matrice identitée
print(round(U %*% U_t, 2)) # -> matrice symétrique
##################################################



##################################################
# PARTIE 4
y <- c(
  floor(
    runif(8, min=-10, max=11)    # Matrice entre -10 et 10 pour ne pas avoir de résultats trop gros 
  )
)

p <- round(U %*% U_t %*% y, 2)
z = y - p

#! TODO : pourquoi p appartient ) Col(A) ?

result_produit_scal <- round(sum(z * p), 2)
if (result_produit_scal != 0) {
  print("z est orthogonal à p")
} else {
  print("z n'est pas orthogonal à p")
}
##################################################



##################################################
# PARTIE 5
scallaire_per_column <- function(z, U) {
  new_list <- list()
  
  for (i in 1:ncol(U)) {
    new_list[[i]] <- round(sum(z * U[, i]), 2)
  }
  
  return(new_list)
}

z_ortho <- scallaire_per_column(z, U)

for (i in 1:length(z_ortho)) {
  if(z_ortho[[i]] == 0) {
    print(paste("z est orthogonal à la colonne ", i))
  } else {
    print(paste("z n'est pas orthogonal à la colonne ", i))
  }
}
##################################################



##################################################
# PARTIE 6
#! TODO: Expliquez pourquoi z appartient à (Col(A)?)



##################################################



##################################################
# PARTIE 7
new_y = rep(1, 8)
projection_y_on_u <- U %*% U_t %*% new_y
##################################################



##################################################
# PARTIE 8
b = c(1,1,1,1,-1,-1,-1,-1)
projection_b_on_u <- U %*% U_t %*% b
z <- b - projection_b_on_u
distance_b_u <- round(sqrt(sum(z^2)),2)

