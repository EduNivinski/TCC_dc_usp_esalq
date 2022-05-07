
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
test_db

# Testes estatísticos

# há correlação entre o bem-estar das pessoas e as variáveis de qualidade 
# econômica e de saúde de cada país?




