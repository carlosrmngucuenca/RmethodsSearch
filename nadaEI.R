df <- data.frame(Nodo=c("X"), Heuristica=c(0), stringsAsFactors = FALSE) 

df<-rbind(df,list(Nodo="A",Heuristica=4))
df<-rbind(df,list(Nodo="B",Heuristica=5))
df <- df[0,]
df<-rbind(df,list(Nodo="B",Heuristica=5))
str(df)
View(df)