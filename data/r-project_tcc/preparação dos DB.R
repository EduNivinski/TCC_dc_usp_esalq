################################################################################
#                 PREPARAÇÃO DOS POSSÍVEIS DADOS NECESSÁRIOS                   #
################################################################################

# link de auxílio: 
# "https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf"


#CARREGAMENTO DOS DADOS:
whr_all_data21 <- read_excel("DataPanelWHR2021C2.xls")
db_all <- whr_all_data21


# RENOMEANDO VARIÁVEIS DE INTERESSE:
db_all <- db_all %>% rename(country = 1,
                         year = 2,
                         ladder_score = 3,
                         log_gdp = 4,
                         social_support = 5,
                         healthy_exp = 6)


# EXCLUSÃO DE VARIÁVEIS DESNECESSÁRIAS:
db_all <- db_all[-(5)]
db_all <- db_all[-(6:10)]


# EXCLUSÃO DE LINHAS COM DADOS AUSENTES:
db_all <- na.omit(db_all)


# INCLUSÃO DA VARIAVEL "efeito_covid", DISCRETA BIONÁRIA QUE INDICA AUSENCIA OU
# PRESENÇA DO EFEITO COVID
db_all$efeito_covid <- ifelse(db_all$year==2020, 1, 0)

head(db_all)


# VISUALIZAÇÃO GERAL (total de 165 países)
table(db_all$year)
unique(db_all$country)


########################################################################
# PREPARANDO BANCO DE DADOS COM OS ANOS DE 2019 ATÉ 2020
########################################################################

db_all_19A20 <- db_all

#excluindo as linhas que possuem anos que não são de interesse (todas exceto 2019 até 2020 )
for (i in 2005:2018){
  db_all_19A20 <- db_all_19A20[!(db_all_19A20$year==i),]
}

# transformando variável year em categórica
db_all_19A20 <- mutate(db_all_19A20,
                      year = replace(year, year==2019, "2019"),
                      year = replace(year, year==2020, "2020"))


# excluindo paises que não estão em ambos os anos (funciona para duplicatas)
db_all_19A20 <- subset(db_all_19A20,duplicated(country) | duplicated(country, fromLast=TRUE))


# quantidade de países restantes (86) 
#unique(db_all_19A20$country)


########################################################################
# PREPARANDO BANCO DE DADOS COM OS ANOS DE 2016 ATÉ 2020
########################################################################

db_all_16A20 <- db_all

#excluindo as linhas que possuem anos que não são de interesse (todas exceto 2016 até 2020 )
for (i in 2005:2015){
  db_all_16A20 <- db_all_16A20[!(db_all_16A20$year==i),]
}

# transformando variável year em categórica
db_all_16A20 <- mutate(db_all_16A20,
                      year = replace(year, year==2016, "2016"),
                      year = replace(year, year==2017, "2017"),
                      year = replace(year, year==2018, "2018"),
                      year = replace(year, year==2019, "2019"),
                      year = replace(year, year==2020, "2020"))


# excluindo paises que não estão em ambos os anos (funciona para duplicatas)

table(db_all_16A20$country)

# criando uma tabela paralela com uma coluna de contagem única para cada país 
test <-
  aggregate(x = db_all_16A20$year,          # Specify data column
            by = list(db_all_16A20$country),      # Specify group indicator
            FUN = function(x) length(unique(x))) #Desired function


# mudando nome da variavel criada em test com nome dos países para que possa
# ser feito um inner join entre as bases
test <- test %>% rename(country = 1,
                        count_uniq = 2)


# aplicando inner join na base de 2016 a 2020
db_all_16A20 <- merge(db_all_16A20, test)


# excluindo países que não estão nos 5 anos, ou seja, que não podem ser usados
# na base por não estarem em todos os anos
for (i in 1:4) {
  db_all_16A20 <- db_all_16A20[!(db_all_16A20$count_uniq==i),]
}
db_all_16A20


# quantidade de países restantes (81) 
#unique(db_all_16A20$country)



