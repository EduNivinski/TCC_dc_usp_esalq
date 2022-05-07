
# Passo a passo do desenvolvimento das análises de dados

# link de auxílio: "https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf"

# instalação dos pacotes
install.packages("tidyverse")

library("tidyverse")
library(readxl)

# renomeando as variáveis de interesse
whr_all_data21 <- read_excel("DataPanelWHR2021C2.xls")
db_all <- whr_all_data21

db_all <- db_all %>% rename(country = 1,
                         year = 2,
                         ladder_score = 3,
                         log_gdp = 4,
                         social_support = 5,
                         healthy_exp = 6)

# para visualização dos dados (total de 165 países)
table(db_all$year)
unique(db_all$country)


#excluindo as linhas que possuem anos que não são de interesse (todas exceto 2019 e 2020 )
db_all <- db_all[!(db_all$year==2005),]
db_all <- db_all[!(db_all$year==2006),]
db_all <- db_all[!(db_all$year==2007),]
db_all <- db_all[!(db_all$year==2008),]
db_all <- db_all[!(db_all$year==2009),]
db_all <- db_all[!(db_all$year==2010),]
db_all <- db_all[!(db_all$year==2011),]
db_all <- db_all[!(db_all$year==2012),]
db_all <- db_all[!(db_all$year==2013),]
db_all <- db_all[!(db_all$year==2014),]
db_all <- db_all[!(db_all$year==2015),]
db_all <- db_all[!(db_all$year==2016),]
db_all <- db_all[!(db_all$year==2017),]
db_all <- db_all[!(db_all$year==2018),]
db_all

# excluindo linhas que possuem dados ausentes
db_all <- na.omit(db_all)
db_all

# excluindo paises que não estão em ambos os anos
table(db_all$country==1)

db_all <- subset(db_all,duplicated(country) | duplicated(country, fromLast=TRUE))
db_all

# quantidade de países restantes (76) 
unique(db_all$country)

# excluindo colunas desnecessárias
db_all <- db_all[-(5)]
db_all <- db_all[-(6:10)]

test_db <- db_all


# transformando variável year em categórica
test_db <- mutate(test_db,
                  year = replace(year, year==2019, "2019"),
                  year = replace(year, year==2020, "2020"))


# Testes estatísticos

################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


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

#Exploração visual do ledder_score (média do bem-estar em um e outro ano)
test_db %>%
  group_by(year) %>%
  mutate(ladder_score_medio = mean(ladder_score, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = ladder_score),color = "orange", alpha = 1, size = 4) +
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
        axis.text.x = element_text(angle = 90))

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
    geom_density(aes(color = year, fill = year), 
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


