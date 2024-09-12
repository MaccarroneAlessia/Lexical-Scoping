## Funzione che crea una cache per una matrice e il suo inverso
## Uso: Passa il risultato della chiamata a createCacheMatrix a solveCachedMatrix

#' Funzione che imposta la matrice e il suo inverso in un ambiente
#' @param mat una matrice invertibile
#' esempi
#' cache = createCacheMatrix(matrix(rnorm(9), 3, 3))
#' cache$set(matrix(rnorm(16), 4, 4))
createCacheMatrix <- function(mat = matrix()) {
  # Controlla se l'input è una matrice
  if(!is.matrix(mat)) stop("L'input deve essere una matrice")
  inv_mat <- NULL
  set <- function(new_mat) {
    mat <<- new_mat
    inv_mat <<- NULL
  }
  get <- function() mat
  setInverse <- function(inverse) inv_mat <<- inverse
  getInverse <- function() inv_mat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#' Calcola e memorizza l'inverso di una matrice nella cache
#' @param cache il risultato di una precedente chiamata a createCacheMatrix
#' @param ... argomenti aggiuntivi da passare alla funzione solve
#' esempi
#' cache = createCacheMatrix(matrix(rnorm(9), 3, 3))
#' solveCachedMatrix(cache)
solveCachedMatrix <- function(cache, ...) {
  ## Restituisce la matrice inversa, se già calcolata
  inv_mat <- cache$getInverse()
  if(!is.null(inv_mat)) {
    message("Restituzione dell'inverso della matrice dalla cache")
    return(inv_mat)
  }
  data <- cache$get()
  inv_mat <- solve(data, ...)
  cache$setInverse(inv_mat)
  inv_mat
}
