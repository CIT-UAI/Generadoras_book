

# Sí, aquí tienes un ejemplo básico de cómo calcular un vector propio de una matriz:
    
# Definir una matriz A
A <- matrix(c(1, 7, 5, 1/3,
              1/7,1, 1/3, 1/9,
              1/5, 3,1,1/7,
              3,9,7,1), nrow = 4, ncol = 4, byrow = T)

# Normalizacion de la Matriz
normalize_matrix <- function(matriz){
    return(matriz %*% diag(1/colSums(matriz)))
}


A_n <- normalize_matrix(A)

# Vector de Pesos (ponderacion de Criterios)
w <- rowMeans(A_n)


# Multiplicacion Matricial a por w
Axw <-  A %*% w

sum_Axw <-  sum(Axw)

eigenvalues = eigen(A)
lambda_max <- max(abs(eigenvalues$values))



# Función para calcular el vector propio utilizando el método de potencias
potencias <- function(A, x0, tol, maxiter) {
    n <- length(x0)
    x <- x0/norm(x0)
    k <- 0
    while (k < maxiter) {
        k <- k + 1
        y <- A %*% x
        lambda <- sum(y * x)
        x <- y/norm(y)
        if (abs(lambda - sum(y * x)) < tol) break
    }
    list(x = x, lambda = lambda)
}

# Ejemplo de uso
x0 <- c(1, 1)
resultados <- potencias(A, x0, 1e-6, 100)
w <- resultados$x

# Función para calcular el vector propio utilizando el método de potencias
potencias <- function(A, x0, tol, maxiter) {
    n <- length(x0)
    x <- x0/norm(matrix(x0, ncol = 1))
    k <- 0
    while (k < maxiter) {
        k <- k + 1
        y <- A %*% x
        lambda <- sum(y * x)
        x <- y/norm(y)
        if (abs(lambda - sum(y * x)) < tol) break
    }
    list(x = x, lambda = lambda)
}


# Ejemplo de uso
A <- matrix(c(2, 1, 1, 2), ncol = 2)
x0 <- rep(1, n)
resultados <- potencias(A= m, x0, 1e-6, 100)
w <- resultados$x


# Encontrar los valores y vectores propios de la matriz A
eigen_decomp <- eigen(A)


# Imprimir los valores propios
eigen_decomp$values

# Imprimir los vectores propios
eigen_decomp$vectors
