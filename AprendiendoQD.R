#install.packages('R6')
library(R6)

Nodo <- R6Class("Nodo",
                  public = list(
                    heuristic="numeric",
                    dato = NULL,
                    nivel = "numeric",
                    hasnivel = "logical",
                    visitado="logical",
                    agregado="logical",
                    lista = "list",
                    initialize = function(heuristic ="numeric",dato ="character", lista = "list") {
                      self$heuristic <- heuristic
                      self$dato <- dato
                      self$lista <-NULL
                      self$visitado<-FALSE
                      self$agregado<-FALSE
                      self$hasnivel<-FALSE
                    },set_heuristica = function(heuristica) {
                      self$heuristic <- heuristica
                    },
                    set_hasnivel = function(dato) {
                      self$hasnivel <- dato
                    },
                    set_nivel = function(dato) {
                      self$nivel <- dato
                    },
                    set_dato = function(dato) {
                      self$dato <- dato
                    },set_hijos = function(hijo_nodo) {
                      nodoindice<-paste0("nodo",Nodo$dato)
                      self$lista[[length(self$lista)+1]]<-hijo_nodo
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
                    },
                    get_heuristica = function() {
                      return(self$heuristic)
                    },
                    get_nivel = function() {
                      return(self$nivel)
                    },
                    get_hasnivel = function() {
                      return(self$hasnivel)
                    }
                   
                  )
)


