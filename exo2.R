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
mask <- upper.tri(produits_scalaires)
ortho <- all(produits_scalaires[mask] == 0)

if (ortho) {
  print("Toutes les colonnes sont orthogonales.")
} else {
  print("Toutes les colonnes ne sont pas orthogonales.")
}
##################################################



##################################################
# PARTIE 2
normalize_over_column <- function(A) {
  U <- apply(
    A, # base matrix
    2, # 1 for row, 2 for col
    function(my_column) my_column / sqrt(sum(my_column^2))
  )
  return(U)
}

# Construire la matrice U
U <- normalize_over_column(A)
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
print(p)
print(z)

# Les colonnes de U sont orthogonales (puisque celles de A le sont) et normées, donc elles forment une base orthonormale
# de l'espace vectoriel engendré par Col(U) (ou Col(A), vu que c'est le même espace, puisque U est une normalisation de A).
# On le retrouve avec : U^t·U = I_n , n = ncol(U)
# Appliquer U·U^t revient à faire une projection orthogonale dans l'espace engendré par Col(U).
# Ainsi, p = U·U^t·y est le projeté orthogonal de y dans l'espace vectoriel engendré par Col(U), et appartient forcément à cet espace.

# z·p = (y-p)·p
#     = y·p - p·p
#     = p·p - p·p    # par définition, puisque p est le projeté de y
#     = 0

result_produit_scal <- round(sum(z %*% t(p)), 2)
if (result_produit_scal != 0) {
  print("z est orthogonal à p")
} else {
  print("z n'est pas orthogonal à p")
}
##################################################



##################################################
# PARTIE 5
scalaire_per_column <- function(z, U) {
  new_list <- list()
  
  for (i in 1:ncol(U)) {
    new_list[[i]] <- round(sum(z * U[, i]), 2)
  }
  
  return(new_list)
}

z_ortho <- scalaire_per_column(z, U)

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
# On sait que z = y - p
# Puisque p est le projeté orthogonal de y dans l'espace vectoriel engendré par Col(U), on peut noter :
# y = p + w , avec w ∈ Col(A)^⊥
# On a donc w = z ∈ Col(A)^⊥ 



##################################################



##################################################
# PARTIE 7
# Ce point est le projeté orthogonal de y sur Col(U)
new_y = rep(1, 8)
projection_y_on_u <- U %*% U_t %*% new_y
##################################################



##################################################
# PARTIE 8
# Il s'agit de la distance entre le point et son son projeté orthogonal sur le plan.
b = c(1,1,1,1,-1,-1,-1,-1)
projection_b_on_u <- U %*% U_t %*% b
z <- b - projection_b_on_u
distance_b_u <- round(sqrt(sum(z^2)),2)

