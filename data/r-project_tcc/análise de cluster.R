
# CRIANDO ASPECTOS DE CLUSTER PARA CATEGORIZAR GRUPOS DE PAÍSES

# CRIANDO DBS COM DATAS SEPARADAS
db_cluster_19_pre <- db_all_19A20[!(db_all_19A20$year==2020),]
db_cluster_20_pre <- db_all_19A20[!(db_all_19A20$year==2019),]

# RETIRANDO AS VARIÁVEIS QUE NÃO SERÃO USADAS PARA CALCULO DAS DISTÃNCIAS
db_cluster_19 <- db_cluster_19_pre[-(2:3)]
db_cluster_19 <- db_cluster_19_pre[-(4)]

db_cluster_20 <- db_cluster_20_pre[-(2:3)]
db_cluster_20 <- db_cluster_20_pre[-(4)]

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
plot(hc4_19, cex = 0.6, hang = -1)
plot(hc4_20, cex = 0.6, hang = -1)


#BRINCANDO COM O DENDOGRAMA PARA 2 GRUPOS
rect.hclust(hc4, k = 3)

#COMPARANDO DENDOGRAMAS DE ANOS DIFERENTES DOS MESMOS PAÍSES
dend3 <- as.dendrogram(hc4_19)
dend4 <- as.dendrogram(hc4_20)
dend_list <- dendlist(dend3, dend4) 

#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend3, dend4, main = paste("Emaranhado =", round(entanglement(dend_list),2)))
 
#criando 2 grupos de alunos
grupo_paises_19 <- cutree(hc4_19, k = 4)
table(grupo_paises_19)

grupo_paises_20 <- cutree(hc4_20, k = 4)
table(grupo_paises_20)

#CRIANDO UM DATAFRAME DOS GRUPOS OBSERVADOS, JUNTANDO OS GRUPOS COM O DB ORIGINAL
df_cluster_19 <- data.frame(grupo_paises_19)
df_cluster_19_f <- cbind(db_cluster_19_pre,df_cluster_19)
df_cluster_19_f <- mutate(df_cluster_19_f,
                       grupo_paises_19 = replace(grupo_paises_19, grupo_paises_19==1, "1"),
                       grupo_paises_19 = replace(grupo_paises_19, grupo_paises_19==2, "2"),
                       grupo_paises_19 = replace(grupo_paises_19, grupo_paises_19==3, "3"),
                       grupo_paises_19 = replace(grupo_paises_19, grupo_paises_19==4, "4"))

df_cluster_20 <- data.frame(grupo_paises_20)
df_cluster_20_f <- cbind(db_cluster_20_pre,df_cluster_20)
df_cluster_20_f <- mutate(df_cluster_20_f,
                          grupo_paises_20 = replace(grupo_paises_20, grupo_paises_20==1, "1"),
                          grupo_paises_20 = replace(grupo_paises_20, grupo_paises_20==2, "2"),
                          grupo_paises_20 = replace(grupo_paises_20, grupo_paises_20==3, "3"),
                          grupo_paises_20 = replace(grupo_paises_20, grupo_paises_20==4, "4"))


