library(R6)

Queue <- R6Class("Queue",
                 public = list(
                   add = function(x) {
                     private$queue <- c(private$queue, list(x))
                     invisible(self)
                   },
                   remove = function() {
                     if (private$length() == 0) return(TRUE)
                     # Can use private$queue for explicit access
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                    
                   },
                   peek = function() {
                     if (private$length() == 0) return(TRUE)
                     # Can use private$queue for explicit access
                     head <- private$queue[[1]]
          
                     return(head)
                   },
                   vacia = function() {
                     if (private$length() == 0) {
                       #cat("tamano de cola : ",private$length(),"\n")
                       return(TRUE)
                     }else{
                       #cat("tamano de cola : ",private$length(),"\n")
                       return(FALSE)
                     }
                      
                     # Can use private$queue for explicit access
                    
                   },frente = function() {
                     
                    
                     if (private$length() == 0) {
                       return(TRUE)
                     }else{
                       cima<-tail(private$queue,1)[[1]]
                       return(cima)
                     }
                     # Can use private$queue for explicit access
                     
                   }
                 ),
                 private = list(
                   queue = list(),
                   length = function() base::length(private$queue)
                 )
)