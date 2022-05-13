
# CRIANDO ASPECTOS DE CLUSTER PARA CATEGORIZAR GRUPOS DE PAÍSES

# CRIANDO DBS COM DATAS SEPARADAS
db_cluster_19 <- db_all_19A20[!(db_all_19A20$year==2020),]
db_cluster_20 <- db_all_19A20[!(db_all_19A20$year==2019),]

# RETIRANDO AS VARIÁVEIS QUE NÃO SERÃO USADAS PARA CALCULO DAS DISTÃNCIAS
db_cluster_19 <- db_cluster_19[-(2:3)]
db_cluster_19 <- db_cluster_19[-(4)]

db_cluster_20 <- db_cluster_20[-(2:3)]
db_cluster_20 <- db_cluster_20[-(4)]

#CALCULANDO MATRIZ DE DISTANCIAS

dist_euclid_19 <- dist(db_cluster_19, method = "euclidean")
dist_euclid_20 <- dist(db_cluster_20, method = "euclidean")

#DEFININDO O CLUSTER A PARTIR DO METODO ESCOLHIDO
#metodos disponiveis "average", "single", "complete" e "ward.D"

# cluster para o ano de 2019
hc3_19 <- hclust(dist_euclid_19, method = "average" )
hc4_19 <- hclust(dist_euclid_19, method = "ward.D" )

# cluster para o ano de 2020 (apenas para efeito de visualização)
hc3_20 <- hclust(dist_euclid_20, method = "average" )
hc4_20 <- hclust(dist_euclid_20, method = "ward.D" )


#DESENHANDO O DENDOGRAMA
plot(hc3_19, cex = 0.6, hang = -1)
plot(hc3_20, cex = 0.6, hang = -1)


#BRINCANDO COM O DENDOGRAMA PARA 2 GRUPOS
rect.hclust(hc3, k = 5)

#COMPARANDO DENDOGRAMAS DE ANOS DIFERENTES DOS MESMOS PAÍSES
dend3 <- as.dendrogram(hc3_19)
dend4 <- as.dendrogram(hc3_20)
dend_list <- dendlist(dend3, dend4) 

#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend3, dend4, main = paste("Emaranhado =", round(entanglement(dend_list),2)))
 
#criando 2 grupos de alunos
grupo_paises_19 <- cutree(hc3_19, k = 4)
table(grupo_paises_19)

grupo_paises_20 <- cutree(hc3_20, k = 4)
table(grupo_paises_20)

#CRIANDO UM DATAFRAME DOS GRUPOS OBSERVADOS, JUNTANDO OS GRUPOS COM O DB ORIGINAL
df_cluster_19 <- data.frame(grupo_paises_19)
df_cluster_19_f <- cbind(db_cluster_19,df_cluster_19)

#FALTA JUNTAR OS OUTROS DADOS AOS DB'S CRIADOS ANTERIORMENTE (YEAR E OUTROS)
