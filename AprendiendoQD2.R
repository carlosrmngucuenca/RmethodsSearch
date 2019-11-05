#install.packages('R6')
library(R6)

MetodosBusqueda <- R6Class("MetodosBusqueda",
                public = list(
                  BFS = function(Raiz,Meta){
                    print(str(Raiz))
                    cola<-list()
                    cola[[1]]<-Raiz
                    while (length(cola)!=0) {
                      if(Raiz$get_dato()==Meta$get_dato()){
                      }else{
                        
                        Raiz$set_visitado(TRUE)
                        cola[[1]]<-NULL
                        if (length(Raiz$get_lista())!=0) {
                          for (variable in Raiz$get_lista()) {
                            Nodohijo=variable
                            if (Nodohijo$get_visited() == FALSE && Nodohijo$get_added()==FALSE ){
                              cola[[names(Nodohijo)]]<-Nodohijo
                            }
                          }
                        }
                        
                        
                      }
                      
                    }
                    
                    
                    
                  }##fin bfs
                )#fin lista de parametros
)


