cola<-c()

BS<-function(Raiz, i) {
  print("Entra")
  
  
  print("Entra")
  print(Raiz)
  if (Raiz$get_hasnivel() == FALSE) {
    print("Entra")
    Raiz$set_hasnivel(TRUE)
    Raiz$set_nivel(i)
    cola <- c(cola, Raiz)
    return(Raiz)
  } else{
    print("Entra al else")
    frente<-cola[1]
    print(frente[[1]])
    if(frente[[1]]$get_nivel()==i){
      return()
    }
    
    #cola$add(Raiz)
    cat("Recorrido  : ->  ", frente[[1]]$get_dato(), "\n")
    #cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
    #cat("el valor de la cola al entrar es : ->  ",Raiz$get_dato(),"\n")
    for (obj.nodo in cola) {
      if (length(obj.nodo$get_lista()) != 0) {
        cat("Entra al for de la cola ","\n")
        #cat("el valor de el tam   es : ->  ",length(Raiz$get_lista()),"\n")
        for (variable in 1:length(obj.nodo$get_lista())) {
          #cat("el valor de el contador   es : ->  ",variable,"\n")
          Nodohijo = obj.nodo$get_lista()[[variable]]
          #cat("el valor de un hijo de un nodo   es : ->  ",Nodohijo$get_dato(),"\n")
          if (Nodohijo$get_visited() == FALSE &&
              Nodohijo$get_added() == FALSE) {
            Nodohijo$set_added(TRUE)
            Nodohijo$set_nivel(i)
            Nodohijo$set_hasnivel(TRUE)
            cola<-c(cola,Nodohijo)
            if (cola$vacia() != TRUE) {
              #cat("el valor  en la cola    es : ->  ",cola$frente()$get_dato(),"\n")
            }
            
            
          }
        }
      }
    }
    #cola<-cola[-1]
    return()
    #print("sale del while")
    #return(0)
    
    
  }#fin elseprincipal
  
}#fin BS
IDS<-function(Raiz, Meta, nivel) {
  
  for (i in 0:nivel) {
    #RaizRaiz
    BS(Raiz, i)
    #k()
  }
  
  
  
}

k<-function(){
  print("hols")
}
NodoR<-nodos[["A"]]
NodoM<-nodos[["C"]]
IDS(NodoR,NodoM,2)