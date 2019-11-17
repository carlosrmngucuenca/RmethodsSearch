#install.packages('R6')
library(R6)

Grafo <- R6Class("Grafo",
                public = list(
                  matriz="matrix",
                  set_matriz_aristapeso = function(tam,lista=c()) {
                    cat("entra \n")
                    self$matriz <-matrix(0,tam,tam)
                    print(self$matriz)
                    cat("entra \n")
                    cat("entra ",lista)
                    rownames(self$matriz) <- lista
                    colnames(self$matriz) <- lista
                  },set_agregar_aristapeso = function(inicio,fin,peso) {
                    self$matriz[inicio,fin]<-peso
                  },
                  get_aristas_de_un_nodo_= function(inicio) {
                    return(self$matriz[inicio,])  
                  },boolean_existe_arista = function(inicio,fin) {
                   
                  },get_matrix = function() {
                    return(self$matriz)
                  }
                  
            )
)


