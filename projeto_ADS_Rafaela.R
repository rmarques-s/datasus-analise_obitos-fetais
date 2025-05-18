library(tidyverse)
library(tidyr)
library(dplyr)

setwd("C:/Users/rafae/Downloads/Projeto ADS")
getwd()

# CRIANDO DATAFRAMES 

obitos_fetais_tipos_partos <- read.csv2("sim_compl_blocbr163051179_190_176_78.csv",
                                        stringsAsFactors = FALSE,
                                        fileEncoding = "latin1")
Encoding(obitos_fetais_tipos_partos$UF) <- "UTF-8"
colnames(obitos_fetais_tipos_partos) <- c(
  "UF Ocorrência",              
  "Percentual de ocorrência",     
  "Não preenchido",
  "Ignorado",
  "Parto normal",
  "Cesario",
  "Total"
)

obitos_fetais_tipos_partos$Total <- NULL
View(obitos_fetais_tipos_partos)

obitos_fetais_morte <- read.csv2("sim_compl_blocbr170040179_190_176_78.csv",
                                        stringsAsFactors = FALSE,
                                        fileEncoding = "latin1")
Encoding(obitos_fetais_morte$UF) <- "UTF-8"
obitos_fetais_morte$Total <- NULL
colnames(obitos_fetais_morte) <- c(
  "UF Ocorrência",              
  "Percentual de ocorrência",     
  "Não preenchido",
  "Ignorado",
  "Antes do parto",
  "Depois do parto"
)

View(obitos_fetais_morte)

obitos_fetais_duracao_gestacao <- read.csv2("sim_compl_blocbr170653179_190_176_78.csv",
                                 stringsAsFactors = FALSE,
                                 fileEncoding = "latin1")

obitos_fetais_duracao_gestacao$Total <- NULL
colnames(obitos_fetais_duracao_gestacao) <- c(
  "UF Ocorrência",              
  "Não preenchido",
  "Ignorado",
  "Menos 22",
  "22 a 27",
  "28 a 31",
  "32 a 36",
  "37 a 41",
  "42+"
)

View(obitos_fetais_duracao_gestacao)

obitos_fetais_tipo_gestacao <- read.csv2("sim_compl_blocbr172802179_190_176_78.csv",
                                            stringsAsFactors = FALSE,
                                            fileEncoding = "latin1")
obitos_fetais_tipo_gestacao$Total <- NULL
colnames(obitos_fetais_tipo_gestacao) <- c(
  "UF Ocorrência",              
  "Não preenchido",
  "Ignorado",
  "Única",
  "Dupla",
  "Tripla ou +"
)

View(obitos_fetais_tipo_gestacao)

obitos_fetais_n_filhos_mortos <- read.csv2("sim_compl_blocbr172900179_190_176_78 (1).csv",
                                         stringsAsFactors = FALSE,
                                         fileEncoding = "latin1")
obitos_fetais_n_filhos_mortos$Total <- NULL
colnames(obitos_fetais_n_filhos_mortos) <- c(
  "UF Ocorrência",
  "Ignorado",
  "Zero nº de filhos",
  "01",
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09",
  "10",
  "11",
  "14",
  "21",
  "24",
  "25"
)

View(obitos_fetais_n_filhos_mortos)
