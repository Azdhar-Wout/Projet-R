library(expm)



valeurs_propres_multiplic <- function(matrix) {
  val <- eigen(matrix)
  round_val <- round(val$values, 2)
  
  print("Valeurs propres et multiplicités de la matrice : ")
  print(table(round_val))
  
  return(val)
}



##################################################
# PARTIE 1
is_diagonalisable <- function(matrix) {
  nb_col <- ncol(matrix)
  nb_row <- nrow(matrix)
  
  if(nb_row != nb_col) {
    print("Cette matrice n'est pas diagonalisable.")
    return(FALSE)
  }
  
  val <- valeurs_propres_multiplic(A)
  val_propres_A <- round(val$values, 2)
  
  if(length(val_propres_A) != nb_col) {
    print("Cette matrice n'est pas diagonalisable.")
    return(FALSE)
  }
  
  print("Cette matrice est diagonalisable.")
  return(val)
}

# Création de la matrice
A <- rbind(c(   0,  1/2,  1/2), 
           c(-2/3,    1,  2/3),
           c(-1/3,  1/2,  5/6))

val <- is_diagonalisable(A)




# Somme des multiplicités des valeurs propres est égale à la 
# dimention de la matrice
# A est donc diagonalisable
##################################################



##################################################
# PARTIE 2
create_matrix <- function(p) {
  p <- -1
  
  while (p <= 0) {
    p_input <- readline(prompt="Enter a number: ")
    p <- floor(as.numeric(p_input))
  }
  
  new_matrix <- c(
    floor(
      runif(p * p, min=1, max=11)
      )
    )
  dim(new_matrix) <- c(p, p)
  
  return(new_matrix)
}

result <- create_matrix(floor(as.numeric(p)))
print(result)
valeurs_propres_multiplic(result)
##################################################



##################################################
# PARTIE 3
vect_propres_A <- eigen(A)
round(vect_propres_A$vectors, 2)
##################################################



##################################################
# PARTIE 4
P <- round(vect_propres_A$vectors, 2)
D <- diag(val_propres_A )
P_inv <- round(solve(P), 2)

B <- round(P %*% D %*% P_inv,2)

identical(round(A,2), B)
##################################################



##################################################
# PARTIE 5
powerize <- function() {
  n <- -1
  
  while (n <= 0) {
    n_input <- readline(prompt="Enter a positive int: ")
    n <- floor(as.numeric(n_input))
  }
  a0 <- 1
  b0 <- 2
  c0 <- 3
  v0 <- c(a0, b0, c0)
  
  vector <- round(P %*% (D %^% n) %*% P_inv %*% v0, 2)
  print(vector)
  
  return(vector)
}

n <- powerize()
# Pour n tendant vers l'infini, on observe an = bn = cn = 3.99
##################################################