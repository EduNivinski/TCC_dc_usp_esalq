
# CRIANDO UM DB PARA CADA GRUPO DO CLUSTER PARA ENTENDER POSSÍVEIS EFEITOS DA COVID

# DF de 2019 dos clusters:
df_cluster_19_f

# DF de 2020 com os mesmos grupos criados em 2019, para enxergar a variação de
# movimento desses grupos com a pandemia:
df_cluster_21_f

# DF agregada, com todos os dados das DF anteriores
db_test <- bind_rows(df_cluster_19_f,df_cluster_21_f)

# CRIANDO UM DF PARA CADA GRUPO COM O BANCO DE DADOS DE 2019 E 2020.
db_test_grupo1 <- db_test[!(db_test$grupo_paises_19==2),]
db_test_grupo1 <- db_test_grupo1[!(db_test_grupo1$grupo_paises_19==3),]
db_test_grupo1 <- db_test_grupo1[!(db_test_grupo1$grupo_paises_19==4),]

db_test_grupo2 <- db_test[!(db_test$grupo_paises_19==1),]
db_test_grupo2 <- db_test_grupo2[!(db_test_grupo2$grupo_paises_19==3),]
db_test_grupo2 <- db_test_grupo2[!(db_test_grupo2$grupo_paises_19==4),]

db_test_grupo3 <- db_test[!(db_test$grupo_paises_19==1),]
db_test_grupo3 <- db_test_grupo3[!(db_test_grupo3$grupo_paises_19==2),]
db_test_grupo3 <- db_test_grupo3[!(db_test_grupo3$grupo_paises_19==4),]

db_test_grupo4 <- db_test[!(db_test$grupo_paises_19==1),]
db_test_grupo4 <- db_test_grupo4[!(db_test_grupo4$grupo_paises_19==3),]
db_test_grupo4 <- db_test_grupo4[!(db_test_grupo4$grupo_paises_19==2),]

# TESTE COMPARATIVO DOS NOVOS GRUPOS GERADOS

# COMPARAÇÃO GRUPO x - 2019 E 2020
ggplotly(
  db_test_grupo3 %>%
    ggplot(aes(x = healthy_exp, y = ladder_score, color = year)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "log gdp grupo 3",
         y = "ladder score grupo 3") +
    theme_bw()
)

summary(test_db)

# OLS DE CADA GRUPO
modelo_mult_1 <- lm(ladder_score ~ log_gdp + healthy_exp + efeito_covid, db_test_grupo4)
summary(modelo_mult_1)




