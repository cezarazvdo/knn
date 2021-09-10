#Upload da base de dados
# testeServidor <- tabelaUnicaAtaqueServidor
# testeNotebook <- tabelaUnicaAtaqueNotebook
# testeRede <- tabelaUnicaAtaqueRedeFX

#Fase 1: pearson and plot
df <- testeNotebook
df[is.na(df)] <- 0 #transform NA in 0
vetPositivos <- numeric()
vetNegativos <- numeric()
vetMean <- numeric()
vet <- numeric()
nVariaveis <- 5

#seleção de melhores correlações
#Será feita uma média das correlações separando negativas e positivas
#Serão selecionadas as N melhores corelações
for(i in 1:(length(df)-1)){
  
  aux <- cor(df[, c(i)], df$stress)
  if(is.na(aux)) aux = 0
  if(aux < 0) aux = (aux * -1)
  vet[length(vet)+1] <- aux
}
print(vet)
org <- order(vet, decreasing = TRUE, na.last = TRUE)
print(org)
df <- df[, c(org[1:nVariaveis], length(df))]
print(cor(df, df$stress))


#plot de várias colunas
for(i in 1:(length(df)-1)){
  print(paste0("i ", j))
  for(j in (i+1):(length(df)-1)){
    if(i == j) next
    print(paste0("j ", j))
    setEPS()
    postscript(paste0("plots/plotsRede/rede-plot-column-",ls.str(df[i]),"X", ls.str(df[j]), ".eps"))
    plot(df[, c(i,j)], col = df$stress+3 , pch=19, xlab=ls.str(df[i]), ylab= ls.str(df[j]))
   # text(x = 28, y = 190)
    dev.off()
  }
}


#Fase 2: KNN
#Preparação df para o KNN
dfNormal = df
dfNormal[, -length(df)] = scale(dfNormal[, -length(df)])
train_index = sample(1:nrow(dfNormal), 0.6*nrow(dfNormal), replace = FALSE)
treino = data.frame()
treino = dfNormal[train_index,]
teste = data.frame()
teste = dfNormal[-train_index,]

#Execução do KNN
library("class")
Knn_Testes = list()
acuracia = numeric()
for(k in 1:20){
  Knn_Testes[[k]] = knn(treino[, -length(treino)], teste[, -length(treino)], treino$stress, k, prob=TRUE)
  acuracia[k] = sum(Knn_Testes[[k]]==teste$stress)/length(teste$stress)*100
}

setEPS()
postscript(paste0("plots/plotsNotebook/plot-acuracia-notebook.eps"))
plot(acuracia, type="b", col="blue", cex=1, pch=1,
     xlab="k", ylab="Acuracia",
     main="Acuracia para cada k")
abline(v=which(acuracia==max(acuracia)), col="red", lwd=1.5)
abline(h=max(acuracia), col="grey", lty=2)
abline(h=min(acuracia), col="grey", lty=2)
dev.off()

