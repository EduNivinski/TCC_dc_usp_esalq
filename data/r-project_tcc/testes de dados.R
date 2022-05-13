# Testes estatísticos

test_db <- df_cluster_20_f
  # Testes estatísticos
  
  
  #Algoritmo para determinação dos erros-padrão das variâncias no componente de
  #efeitos aleatórios
  #ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
  #e HLM3, desde que estimados pelo pacote nlme
  
  stderr_nlme <- function(model){
    if(base::class(model) != "lme"){
      base::message("Use a lme object model from nlme package")
      stop()}
    resume <- base::summary(model)
    if(base::length(base::names(model$groups))==1){
      m.type <- "HLM2"
    } else if(base::length(base::names(model$groups))==2){
      m.type <- "HLM3"
    }
    if(m.type == "HLM2"){
      vcov_matrix <- model$apVar
      logs_sd_re <- base::attr(vcov_matrix,"Pars")
      if(base::length(logs_sd_re)==2){
        stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
        stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
        results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                    `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                    base::exp(logs_sd_re[[2]])^2),
                                    `Std Err.`=base::c(stderr_tau00,
                                                       stderr_sigma),
                                    z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                              base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                    `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                                 base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                       lower.tail=F)*2,3))
        return(results)
      }
      else{
        stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
        stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
        stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
        results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                    Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                         base::exp(logs_sd_re[[2]])^2,
                                                         base::exp(logs_sd_re[[4]])^2),
                                    Std_Err=base::c(stderr_tau00,
                                                    stderr_tau01,
                                                    stderr_sigma),
                                    z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                              base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                              base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                    `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                                 base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                                 base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                       lower.tail=F)*2,3))
        return(results)
      }
    }
    if(m.type == "HLM3"){
      vcov_matrix <- model$apVar
      logs_sd_re <-  base::attr(vcov_matrix,"Pars")
      if(base::length(logs_sd_re) == 3){
        stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
        stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
        stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
        results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                    Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                        base::exp(logs_sd_re)[[1]]^2,
                                                        base::exp(logs_sd_re)[[3]]^2),
                                    Std_Err=base::c(stderr_tau_u000,
                                                    stderr_tau_r000,
                                                    stderr_sigma),
                                    z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                              base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                              base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                    `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                                 base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                                 base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                       lower.tail=F)*2,3))
        return(results)
      } 
      else{
        stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
        stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
        stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
        stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
        stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
        results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                            "Var(v0jk)","Var(v1jk)",
                                                            "Var(e)"),
                                    `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                   base::exp(logs_sd_re)[[5]]^2,
                                                                   base::exp(logs_sd_re)[[1]]^2,
                                                                   base::exp(logs_sd_re)[[2]]^2,
                                                                   base::exp(logs_sd_re)[[7]]^2),
                                    `Std Err.`=base::c(stderr_tau_u000,
                                                       stderr_tau_u100,
                                                       stderr_tau_r000,
                                                       stderr_tau_r100,
                                                       stderr_sigma),
                                    z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                              base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                              base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                              base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                              base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                    `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                                 base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                                 base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                                 base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                                 base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                       lower.tail=F)*2,3))
        return(results)
      }
    }
  }


#Visualização da base de dados
test_db %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25) 
#Estatísticas descritivas
summary(test_db)

# Explorando a correlação entre as variáveis através da função chart.correlation
test_db %>%
  corr_plot(3:5,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 6,
            minsize = 6,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")




#Exploração visual do ledder_score (média do bem-estar em um e outro ano)
test_db %>%
  group_by(year) %>%
  mutate(ladder_score_medio = mean(ladder_score, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = ladder_score),color = "orange", alpha = 1, size = 3) +
  geom_line(aes(x = year, y = ladder_score_medio, 
                group = 1, color = "ladder score medio"), size = 2) +
  scale_colour_viridis_d() +
  labs(x = "year",
       y = "ladder score") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, size = 15))

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (ladder_score), com histograma
ggplotly(
  ggplot(test_db, aes(x = ladder_score)) +
    geom_density(aes(x = ladder_score), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)
#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (ladder_score) por year

ggplotly(
  ggplot(test_db, aes(x = ladder_score)) +
    geom_density(aes(color = grupo_paises_19, fill = grupo_paises_19), 
                 position = "identity", alpha = 0.3) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (ladder_score), com histograma e por escola separadamente
#(função facet_wrap)
test_db %>% 
  group_by(year) %>% 
  mutate(linhas = 1:n()) %>% 
  mutate(x = unlist(density(ladder_score, n = max(linhas))["x"]),
         y = unlist(density(ladder_score, n = max(linhas))["y"])) %>%
  ggplot() +
  geom_area(aes(x = x, y = y, group = year, fill = year), color = "black", alpha = 0.3) +
  geom_histogram(aes(x = ladder_score, y = ..density.., fill = year), 
                 color = "black", position = 'identity', alpha = 0.1) +
  facet_wrap(~ year) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw()


#Gráfico de desempenho x horas por escola (visualização do contexto)
#NOTE QUE A PERSPECTIVA MULTINÍVEL NATURALMENTE CONSIDERA O COMPORTAMENTO
#HETEROCEDÁSTICO NOS DADOS!
ggplotly(
  test_db %>%
    ggplot(aes(x = log_gdp, y = ladder_score, color = grupo_paises_20)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "log gdp",
         y = "ladder score") +
    theme_bw()
)

#Agora plotamos o mesmo gráfico, porém de forma tridimensional,
#considerando modelos distintos para as diferentes escolas. Plotamos
#apenas as 06 primeiras escolas em razão de uma limitação do algoritmo
scatter3d(ladder_score ~ log_gdp + efeito_covid,
          groups = factor(test_db$efeito_covid),
          data = test_db,
          fit = "linear",
          surface = T)



################################################################################
#                         ESTIMAÇÃO DO MODELO NULO HLM2                        #
################################################################################

# A PERGUNTA AQUI É, EXISTE DIFERENÇA NO LADDER SCORE BASEADO NOS DIFERENTES
# ANOS DE OBSERVAÇÃO? (NESSE CASO APARENTEMENTE NÃO)

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_hlm2 <- lme(fixed = ladder_score ~ 1, 
                        random = ~ 1 | year,
                        data = test_db,
                        method = "REML")

#Parâmetros do modelo
summary(modelo_nulo_hlm2)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm2)


################################################################################
#                    COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO                   #
################################################################################
#Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = ladder_score ~ 1, 
                      data = test_db)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#            ESTIMAÇÃO DO MODELO COM INTERCEPTOS ALEATÓRIOS HLM2               #
################################################################################



#Estimação do modelo com Interceptos Aleatórios
modelo_intercept_hlm2 <- lme(fixed = ladder_score ~ log_gdp,
                             random = ~ 1 | efeito_covid,
                             data = test_db,
                             method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_hlm2)

#Comparação entre os LLs dos modelos
# aparentemente não há muita diferença na relação entre felicidade e poder
# economico quando avaliamos os diferentes anos de observações
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

################################################################################
#      ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2       #
################################################################################

#Estimação do modelo com Interceptos e Inclinações Aleatórios
ctrl <- lmeControl(opt='optim')
modelo_intercept_inclin_hlm2 <- lme(fixed = ladder_score ~ log_gdp,
                                    random = ~ log_gdp + healthy_exp | efeito_covid,
                                  
                                    data = test_db,
                                    method = "REML")



#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

################################################################################
#                       ESTIMAÇÃO DO MODELO FINAL HLM2                         #
################################################################################

#Estimação do modelo final
modelo_final_hlm2 <- lme(fixed = ladder_score ~ log_gdp + healthy_exp + log_gdp:healthy_exp,
                         random = ~ log_gdp + healthy_exp | efeito_covid,
                         control=ctrl,
                         data = test_db,
                         method = "REML")

#Parâmetros do modelo
summary(modelo_final_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_final_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4,
         `HLM2 Modelo Final` = 5) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3",
                               "deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


#Melhor visualização dos interceptos e das inclinações aleatórios por efeito covid,
#para o modelo final HLM2

v_final <- data.frame(modelo_final_hlm2[["coefficients"]][["random"]][["efeito covid"]]) %>%
  
v_final$efeito_covid <- c(1:2)
v_final$efeito_covid <- as.factor(v_final$efeito_covid)

v_final %>% 
  select(efeito_covid, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)



#Gerando os fitted values do modelo HLM2 Final
test_db$hlm2_fitted <- predict(modelo_final_hlm2,
                                        test_db)

# Visualizando os fitted values do modelo
#Visualizando os fitted values por estudante e por escola
predict(modelo_final_hlm2, level = 0:1) %>% 
  mutate(efeito_covid = gsub("^.*?\\/","",efeito_covid),
         efeito_covid = as.factor(as.numeric(efeito_covid)),
         ladder_score = test_db$ladder_score,
         etjk = resid(modelo_final_hlm2)) %>% #função resid gera os termos etjk
  select(efeito_covid, ladder_score, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)




################################################################################
#                       COMPARAÇÃO COM UM MODELO OLS                           #
################################################################################

#Elaborando um modelo OLS para fins de comparação
modelo_ols <- lm(formula = ladder_score ~ log_gdp + healthy_exp,
                 data = test_db)

#Parâmetros
summary(modelo_ols)

#Comparando os LL dos modelos elaborados
data.frame(OLS = logLik(modelo_ols),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS` = 1,
         `HLM2 Modelo Final` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#LR Test
lrtest(modelo_ols, modelo_final_hlm2)

#Comparando a aderência dos fitted values dos modelos estimados
#Gerando os fitted values do modelo OLS
test_db$ols_fitted <- modelo_ols$fitted.values

#Plotagem
test_db %>%
  ggplot() +
  geom_smooth(aes(x = ladder_score, y = ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = ladder_score, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = ladder_score, y = ladder_score), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  geom_point(aes(x = ladder_score, y = ols_fitted,
                 color = "OLS")) +
  geom_point(aes(x = ladder_score, y = hlm2_fitted,
                 color = "HLM2 Final"))  +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1","darkorchid")) +
  labs(x = "ladder score", y = "Fitted Values") +
  theme_bw()

#Efetuando predições

predict(modelo_final_hlm2, level = 0:1,
        newdata = data.frame(efeito_covid = "0",
                             log_gdp = 12,
                             healthy_exp = 80))

# rodar uma OLS no ano com covid e rodar outra no ano sem covid para ver juntas
# a ideia é entender a diferença das curvas
