##################################################
# PARTIE 1
generate_plot <- function(a, 
                          b, 
                          c, 
                          d, 
                          nb_points) {
  list_xi <- sort(
    round(
      runif(
        nb_points, 
        min=a, 
        max=b),
      2)
    )
  list_yi <- round(
    runif(nb_points, 
          min=c, 
          max=d), 
    2)
  
  plot(x=list_xi, y=list_yi, main="Nuage de points", pch=19)
  
  return(list(list_xi, list_yi))
}

create_delta <- function() {
  nb_points <- floor(as.numeric(readline(prompt="Enter a number of points: ")))
  a_input <- as.numeric(readline(prompt="Enter value 'a': "))
  b_input <- as.numeric(readline(prompt="Enter value 'b': "))
  c_input <- as.numeric(readline(prompt="Enter value 'c': "))
  d_input <- as.numeric(readline(prompt="Enter value 'd': "))
  
  a <- min(a_input, b_input)
  b <- max(a_input, b_input)
  c <- min(c_input, d_input)
  d <- max(c_input, d_input)
  
  list_of_xiyi <- generate_plot(a, b, c, d, nb_points)
  return(list_of_xiyi)
}

# Exemple
result <- generate_plot(a=0, 
                        b=5, 
                        c=0, 
                        d=5, 
                        nb_points=10)
##################################################



##################################################
# PARTIE 2
vandertruc_matrix <- function(x, n) {
  return(
    round(
      outer(x, 0:n, `^`)
      ,2)
    )
}

interpolation_polynomiale_vander <- function(list_xi, 
                                             list_yi, 
                                             degre) {
  V <- vandertruc_matrix(list_xi, degre)
  coefficients <- solve(V, list_yi)
  
  return(coefficients)
}

draw <- function(list_xi, 
                 list_yi,
                 coeffs) {
  degre <- length(coeffs) - 1
  xs <- seq(min(list_xi), max(list_xi), length.out=1000)
  ys <- sapply(xs, function(xi) sum(coeffs * xi^(0:degre)))
  
  plot(list_xi, list_yi, main=paste("Interpolation par vandermonde"), pch=19)
  lines(xs, ys, col="red")
}

do_p2 <- function(degre, 
                  a=-2, 
                  b=2, 
                  c=-2, 
                  d=2) {
  result <- generate_plot(a=a, b=b, c=c, d=d, nb_points=degre+1)
  list_xi <- result[[1]]
  list_yi <- result[[2]]
  coeffs <- interpolation_polynomiale_vander(list_xi, list_yi, degre)
  draw(list_xi, list_yi, coeffs)
}


do_p2(9)
do_p2(19)    # Impossible si les valeurs b et d sont trop grandes
do_p2(29)    # Ne marche pas souvent car trop de points trop proches ou les polynomes trop grands
##################################################



##################################################
# PARTIE 3
newton_coefficients <- function(list_xi, list_yi) {
  n <- length(list_xi)
  coeffs <- list_yi
  for (j in 2:n) {
    for (i in n:1) {
      if (i >= j) {
        coeffs[i] <- (coeffs[i] - coeffs[i-1]) / (list_xi[i] - list_xi[i - j+1])
      }
    }
  }
  return(round(coeffs,2))
}


interpolation_polynomiale_newton <- function(list_xi, 
                                             coeffs, 
                                             eval_points) {
  n <- length(coeffs)
  y_eval <- rep(coeffs[n], length(eval_points))
  for (k in (n-1):1) {
    y_eval <- y_eval * (eval_points - list_xi[k]) + coeffs[k]
  }
  return(round(y_eval,2))
}


draw_newton <- function(list_xi, 
                        list_yi, 
                        coeffs) {
  xs <- seq(min(list_xi), max(list_xi), length.out=1000)
  ys <- interpolation_polynomiale_newton(list_xi, coeffs, xs)
  
  plot(list_xi, list_yi, main="Interpolation par Newton", pch=19, xlab="x", ylab="y")
  lines(xs, ys, col="blue")
}


compare_interpolations <- function(degre, 
                                   a=-2, 
                                   b=2, 
                                   c=-2, 
                                   d=2) {
  result <- generate_plot(a=a, b=b, c=c, d=d, nb_points=degre+1)
  list_xi <- result[[1]]
  list_yi <- result[[2]]
  
  vandermonde_coeffs <- interpolation_polynomiale_vander(list_xi, list_yi, degre)
  draw(list_xi, list_yi, vandermonde_coeffs)
  
  newton_coeffs <- newton_coefficients(list_xi, list_yi)
  draw_newton(list_xi, list_yi, newton_coeffs)
}

compare_interpolations(9)
compare_interpolations(19)
# test du 29 sur Newton
result <- generate_plot(a=-2, b=2, c=-2, d=2, nb_points=29+1)
list_xi <- result[[1]]
list_yi <- result[[2]]
newton_coeffs <- newton_coefficients(list_xi, list_yi)
draw_newton(list_xi, list_yi, newton_coeffs)
# Newton bien meilleur, mais 29 toujours KO
##################################################



##################################################
# PARTIE 4




##################################################



##################################################
# PARTIE 5




