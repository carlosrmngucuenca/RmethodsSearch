vector<- c(1,2,3,4)
print(vector)


actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David","Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))

relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David","David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))

print(actors)
print(relations)

library("igraph")
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
print_all(g)

o <- make_empty_graph() +vertices(letters[1:10])
o <- o + edge("a", "b")
print(o)

library("igraph")
gr <- make_empty_graph()+vertices("hola","rojo",1,2,4)
gr <- gr + edges("hola","rojo")
print_all(gr)
plot(gr)

gf <- make_empty_graph()+vertices(c(1, 2, 3, 4, 5, 6, 1, 5))

plot(gf)

x <- seq(-pi,pi,0.1)
plot(x, sin(x))

library("igraph")
grafo <- make_empty_graph()
grafo <- grafo + vertices("bar2", "foobar2", color=1:2, shape="rectangle")

plot(grafo)

g <- make_empty_graph(n = 5) %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5)) %>%
  set_edge_attr("color", value = "red") %>%
  add_edges(c(5,1), color = "green")
E(g)[[]]
plot(g)

library("igraph")
M <- matrix(c(0,0,1,0,1,0,0,0,1,1,0,1,1,1,0,1,1,0,0,1,1,0,1,0,1,0,1,1,0,0,0,1,0,0,0,0), 6, dimnames= list(c('A','B','C','D','E','F'),c('A','B','C','D','E','F')))
M
cuadrada <- function (M) {
  if (dim(M)[1]==dim(M)[2]) {cuad=TRUE}
  else {cuad=FALSE}
  return(cuad)
}

names(which(M['B',]==1))
names(which(M['D',]==1))
grV <- colSums(M)
grV

m <- sum(M[upper.tri(M)==1])
m

dim(M)[1]

dim(M)[2]

y1<-matrix(scan(),ncol=3)