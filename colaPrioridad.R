library(R6)


colapr <- R6Class("colapr",
                 public = list(
                   queue="data.frame",
                   initialize=function(){
                     
                     self$queue<-data.frame(Nodo=c("0"),Heuristic=c(121345),stringsAsFactors=FALSE)
                   },
                   add = function(x) {
                     
                     self$queue<-rbind(self$queue,list(Nodo=x$get_dato(),Heuristic=x$get_heuristica()))
                     print("Entra cola a adherirse")
                     print(self$queue)
                   },
                   Inicialisar = function(x) {
                     self$queue <-self$queue[-1,]
                     print(self$queue)
                     
                   },
                   remove = function() {
                     if (self$length() == 1) return(TRUE)
                     # Can use self$queue for explicit access
                     #head <- self$queue[1,]
                     self$queue <- self$queue[-1,]
                     print(self$queue)
                   },
                   peek = function() {
                     if (self$length() == 1) return(TRUE)
                     # Can use self$queue for explicit access
                     #str(self$queue)
                     self$queue <- self$queue[order(self$queue$Heuristic,decreasing = FALSE),]
                     #print(self$queue)
                     head<-self$queue[1,]
                     
                     return(head)
                   },
                    vaciar = function() {
                     #if (nrow(self$queue) == 0) return(TRUE)
                     # Can use self$queue for explicit access
                     #str(self$queue)
                      
                     #ordena <- self$queue[order(self$queue$Heuristic,decreasing = FALSE),]
                     #head<-ordena[1,]
                      #self$queue<-self$queue[-(1:(length(self$queue)-1)),]
                      #print(self$queue)
                      self$queue<-self$queue[self$queue$Heuristic >=121345,]
                      #self$queue<-NULL
                      
                      print(self$queue)
                     #return(head)
                    },
                   vaciaq = function() {
                     if (self$length() == 1) {
                       print("Entra")
                       cat("tamano de cola vacia: ",self$length(),"\n")
                       return(TRUE)
                     }else{
                       #print(self$queue)
                       cat("tamano de cola con mas de un elemento : ",self$length(),"\n")
                       return(FALSE)
                     }
                     
                   },
                   length = function() {
                     
                     return(nrow(self$queue))
                   }
                     
                   
                 )
)
