#install.packages('R6')
library(R6)

Nodo <- R6Class("Nodo",
                  public = list(
                    heuristic="numeric",
                    dato = NULL,
                    visitado="logical",
                    agregado="logical",
                    lista = "list",
                    initialize = function(heuristic ="numeric",dato ="character", lista = "list") {
                      self$heuristic <- heuristic
                      self$dato <- dato
                      self$lista <-NULL
                      self$visitado<-FALSE
                      self$agregado<-FALSE
                    },
                    set_dato = function(dato) {
                      self$dato <- dato
                    },set_hijos = function(lista) {
                      nodoindice<-paste0("nodo",Nodo$dato)
                      self$lista<-lista
                    },set_visitado = function(visitado) {
                      self$visitado<-visitado
                    },set_added = function(aderido) {
                        self$agregado<-aderido
                      },
                    get_dato = function() {
                      return(self$dato)
                    },
                    get_lista = function() {
                      
                        return(self$lista)
                      
                     
                    },
                    get_visited = function() {
                      return(self$visitado)
                    },
                    get_added = function() {
                      return(self$agregado)
                    }
                   
                  )
)


