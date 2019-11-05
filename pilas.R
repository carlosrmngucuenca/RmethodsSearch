library(R6)

Pila <- R6Class("Pila",
                 public = list(
                   add = function(x) {
                     private$pila <- c(private$pila, list(x))
                     invisible(self)
                   },
                   remove = function() {
                     if (private$length() == 0) return(TRUE)
                     # Can use private$queue for explicit access
                     head <-tail(private$pila,1)[[1]] 
                     id=private$length()
                     private$pila <- private$pila[-id]
                     
                   },
                   peek = function() {
                     if (private$length() == 0) return(TRUE)
                     # Can use private$queue for explicit access
                     head <- private$pila[[private$length()]]
                     
                     return(head)
                   },
                   vacia = function() {
                     if (private$length() == 0) {
                       #cat("tamano de pila : ",private$length(),"\n")
                       return(TRUE)
                     }else{
                       #cat("tamano de pila : ",private$length(),"\n")
                       return(FALSE)
                     }
                     
                     # Can use private$queue for explicit access
                     
                   },frente = function() {
                     
                     
                     if (private$length() == 0) {
                       return(TRUE)
                     }else{
                       cima<-head(private$pila,1)[[1]]
                       return(cima)
                     }
                     # Can use private$queue for explicit access
                     
                   }
                 ),
                 private = list(
                   pila = list(),
                   length = function() base::length(private$pila)
                 )
)