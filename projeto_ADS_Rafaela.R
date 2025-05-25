library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/rafae/Downloads/Projeto ADS")
getwd()

# CRIANDO DATAFRAMES 

obitos_fetais_tipos_partos <- read.csv2("tipos_partos.csv",
                                        stringsAsFactors = FALSE,
                                        fileEncoding = "latin1",
                                        na = c("", "NA", "n/d", "NULL", "-"))
colnames(obitos_fetais_tipos_partos) <- c(
  "UF",
  "Não preenchido",
  "Ignorado",
  "Parto normal",
  "Cesario",
  "Total"
)

obitos_fetais_tipos_partos$`Não preenchido` <- NULL
obitos_fetais_tipos_partos$`Ignorado` <- NULL
obitos_fetais_tipos_partos$Total <- NULL
View(obitos_fetais_tipos_partos)


# 1. Remover os números no início da string
obitos_fetais_tipos_partos$UF <- gsub("^[0-9]+\\s+", "", obitos_fetais_tipos_partos$UF)

obitos_fetais_tipos_partos$UF <- sapply(obitos_fetais_tipos_partos$UF, function(uf) {
  switch(uf,
         "Rondï¿½nia" = "Rondônia",
         "Acre" = "Acre",
         "Amazonas" = "Amazonas",
         "Roraima" = "Roraima",
         "Parï¿½" = "Pará",
         "Amapï¿½" = "Amapá",
         "Tocantins" = "Tocantins",
         "Maranhï¿½o" = "Maranhão",
         "Piauï¿½" = "Piauí",
         "Cearï¿½" = "Ceará",
         "Paraï¿½ba" = "Paraíba",
         "Pernambuco" = "Pernambuco",
         "Alagoas" = "Alagoas",
         "Sergipe" = "Sergipe",
         "Bahia" = "Bahia",
         "Minas Gerais" = "Minas Gerais",
         "Espï¿½rito Santo" = "Espírito Santo",
         "Rio de Janeiro" = "Rio de Janeiro",
         "Sï¿½o Paulo" = "São Paulo",
         "Paranï¿½" = "Paraná",
         "Santa Catarina" = "Santa Catarina",
         "Rio Grande do Sul" = "Rio Grande do Sul",
         "Mato Grosso do Sul" = "Mato Grosso do Sul",
         "Mato Grosso" = "Mato Grosso",
         "Goiï¿½s" = "Goiás",
         "Distrito Federal" = "Distrito Federal",
         uf
  )
})

# Função para mapear UF para Região
get_regiao <- function(uf) {
  switch(uf,
         "Rondônia" = "Norte",
         "Acre" = "Norte",
         "Amazonas" = "Norte",
         "Roraima" = "Norte",
         "Pará" = "Norte",
         "Amapá" = "Norte",
         "Tocantins" = "Norte",
         
         "Maranhão" = "Nordeste",
         "Piauí" = "Nordeste",
         "Ceará" = "Nordeste",
         "Rio Grande do Norte" = "Nordeste",
         "Paraíba" = "Nordeste",
         "Pernambuco" = "Nordeste",
         "Alagoas" = "Nordeste",
         "Sergipe" = "Nordeste",
         "Bahia" = "Nordeste",
         
         "Mato Grosso" = "Centro-Oeste",
         "Mato Grosso do Sul" = "Centro-Oeste",
         "Goiás" = "Centro-Oeste",
         "Distrito Federal" = "Centro-Oeste",
         
         "São Paulo" = "Sudeste",
         "Rio de Janeiro" = "Sudeste",
         "Minas Gerais" = "Sudeste",
         "Espírito Santo" = "Sudeste",
         
         "Paraná" = "Sul",
         "Santa Catarina" = "Sul",
         "Rio Grande do Sul" = "Sul",
         
         NA # Caso não seja mapeado
  )
}

# Criar a nova coluna de Região
obitos_fetais_tipos_partos$Regiao <- sapply(obitos_fetais_tipos_partos$UF, get_regiao)




# Transformar os dados em formato longo (long format)
dados_longos <- pivot_longer(obitos_fetais_tipos_partos, 
                             cols = c("Parto normal", "Cesario"), 
                             names_to = "TipoParto", 
                             values_to = "Percentual")
View(obitos_fetais_tipos_partos)

# Gráfico de barras empilhadas

ggplot(dados_longos, aes(x = reorder(UF, -Percentual), y = Percentual, fill = TipoParto)) +
  geom_bar(stat = "identity") +
  labs(title = "Proporção de Óbitos Fetais por Tipo de Parto e Estado",
       x = "Estado",
       y = "Percentual (%)",
       fill = "Tipo de Parto") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

obitos_fetais_morte <- read.csv2("mortes.csv",
                                stringsAsFactors = FALSE,
                                 fileEncoding = "latin1",
                                 na = c("", "NA", "n/d", "NULL", "-"))
obitos_fetais_morte$Total <- NULL
colnames(obitos_fetais_morte) <- c(
  "UF Ocorrência",               
  "Não preenchido",
  "Ignorado",
  "Antes do parto",
  "Durante o parto",
  "Depois do parto"
)

# 1. Remover os números no início da string
obitos_fetais_morte$`UF Ocorrência` <- gsub("^[0-9]+\\s+", "", obitos_fetais_morte$`UF Ocorrência`)

obitos_fetais_morte$`UF Ocorrência` <- sapply(obitos_fetais_morte$`UF Ocorrência`, function(uf) {
  switch(uf,
         "Rondï¿½nia" = "Rondônia",
         "Acre" = "Acre",
         "Amazonas" = "Amazonas",
         "Roraima" = "Roraima",
         "Parï¿½" = "Pará",
         "Amapï¿½" = "Amapá",
         "Tocantins" = "Tocantins",
         "Maranhï¿½o" = "Maranhão",
         "Piauï¿½" = "Piauí",
         "Cearï¿½" = "Ceará",
         "Paraï¿½ba" = "Paraíba",
         "Pernambuco" = "Pernambuco",
         "Alagoas" = "Alagoas",
         "Sergipe" = "Sergipe",
         "Bahia" = "Bahia",
         "Minas Gerais" = "Minas Gerais",
         "Espï¿½rito Santo" = "Espírito Santo",
         "Rio de Janeiro" = "Rio de Janeiro",
         "Sï¿½o Paulo" = "São Paulo",
         "Paranï¿½" = "Paraná",
         "Santa Catarina" = "Santa Catarina",
         "Rio Grande do Sul" = "Rio Grande do Sul",
         "Mato Grosso do Sul" = "Mato Grosso do Sul",
         "Mato Grosso" = "Mato Grosso",
         "Goiï¿½s" = "Goiás",
         "Distrito Federal" = "Distrito Federal",
         uf
  )
})

View(obitos_fetais_morte)

obitos_fetais_duracao_gestacao <- read.csv2("duracao_gestacao.csv",
                                 stringsAsFactors = FALSE,
                                 fileEncoding = "latin1",
                                 na = c("", "NA", "n/d", "NULL", "-"))

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

# 1. Remover os números no início da string
obitos_fetais_duracao_gestacao$`UF Ocorrência` <- gsub("^[0-9]+\\s+", "", obitos_fetais_duracao_gestacao$`UF Ocorrência`)

obitos_fetais_duracao_gestacao$`UF Ocorrência` <- sapply(obitos_fetais_duracao_gestacao$`UF Ocorrência`, function(uf) {
  switch(uf,
         "Rondï¿½nia" = "Rondônia",
         "Acre" = "Acre",
         "Amazonas" = "Amazonas",
         "Roraima" = "Roraima",
         "Parï¿½" = "Pará",
         "Amapï¿½" = "Amapá",
         "Tocantins" = "Tocantins",
         "Maranhï¿½o" = "Maranhão",
         "Piauï¿½" = "Piauí",
         "Cearï¿½" = "Ceará",
         "Paraï¿½ba" = "Paraíba",
         "Pernambuco" = "Pernambuco",
         "Alagoas" = "Alagoas",
         "Sergipe" = "Sergipe",
         "Bahia" = "Bahia",
         "Minas Gerais" = "Minas Gerais",
         "Espï¿½rito Santo" = "Espírito Santo",
         "Rio de Janeiro" = "Rio de Janeiro",
         "Sï¿½o Paulo" = "São Paulo",
         "Paranï¿½" = "Paraná",
         "Santa Catarina" = "Santa Catarina",
         "Rio Grande do Sul" = "Rio Grande do Sul",
         "Mato Grosso do Sul" = "Mato Grosso do Sul",
         "Mato Grosso" = "Mato Grosso",
         "Goiï¿½s" = "Goiás",
         "Distrito Federal" = "Distrito Federal",
         uf
  )
})

View(obitos_fetais_duracao_gestacao)

obitos_fetais_tipo_gestacao <- read.csv2("tipo_gestacao.csv",
                                          stringsAsFactors = FALSE,
                                          fileEncoding = "latin1",
                                          na = c("", "NA", "n/d", "NULL", "-"))
obitos_fetais_tipo_gestacao$Total <- NULL
colnames(obitos_fetais_tipo_gestacao) <- c(
  "UF Ocorrência",              
  "Não preenchido",
  "Ignorado",
  "Única",
  "Dupla",
  "Tripla ou +"
)
# 1. Remover os números no início da string
obitos_fetais_tipo_gestacao$`UF Ocorrência` <- gsub("^[0-9]+\\s+", "", obitos_fetais_tipo_gestacao$`UF Ocorrência`)

obitos_fetais_tipo_gestacao$`UF Ocorrência` <- sapply(obitos_fetais_tipo_gestacao$`UF Ocorrência`, function(uf) {
  switch(uf,
         "Rondï¿½nia" = "Rondônia",
         "Acre" = "Acre",
         "Amazonas" = "Amazonas",
         "Roraima" = "Roraima",
         "Parï¿½" = "Pará",
         "Amapï¿½" = "Amapá",
         "Tocantins" = "Tocantins",
         "Maranhï¿½o" = "Maranhão",
         "Piauï¿½" = "Piauí",
         "Cearï¿½" = "Ceará",
         "Paraï¿½ba" = "Paraíba",
         "Pernambuco" = "Pernambuco",
         "Alagoas" = "Alagoas",
         "Sergipe" = "Sergipe",
         "Bahia" = "Bahia",
         "Minas Gerais" = "Minas Gerais",
         "Espï¿½rito Santo" = "Espírito Santo",
         "Rio de Janeiro" = "Rio de Janeiro",
         "Sï¿½o Paulo" = "São Paulo",
         "Paranï¿½" = "Paraná",
         "Santa Catarina" = "Santa Catarina",
         "Rio Grande do Sul" = "Rio Grande do Sul",
         "Mato Grosso do Sul" = "Mato Grosso do Sul",
         "Mato Grosso" = "Mato Grosso",
         "Goiï¿½s" = "Goiás",
         "Distrito Federal" = "Distrito Federal",
         uf
  )
})

View(obitos_fetais_tipo_gestacao)

obitos_fetais_n_filhos_mortos <- read.csv2("n_filhos_mortos.csv",
                                         stringsAsFactors = FALSE,
                                         fileEncoding = "latin1",
                                         na = c("", "NA", "n/d", "NULL", "-"))
obitos_fetais_n_filhos_mortos$Total <- NULL
# Excluir as colunas 10 a 25 (poucos dados)
obitos_fetais_n_filhos_mortos <- obitos_fetais_n_filhos_mortos[, -c(13:22)]

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
  "09"
)

# Remover os números no início da string
obitos_fetais_n_filhos_mortos$`UF Ocorrência` <- gsub("^[0-9]+\\s+", "", obitos_fetais_n_filhos_mortos$`UF Ocorrência`)

obitos_fetais_n_filhos_mortos$`UF Ocorrência` <- sapply(obitos_fetais_n_filhos_mortos$`UF Ocorrência`, function(uf) {
  switch(uf,
         "Rondï¿½nia" = "Rondônia",
         "Acre" = "Acre",
         "Amazonas" = "Amazonas",
         "Roraima" = "Roraima",
         "Parï¿½" = "Pará",
         "Amapï¿½" = "Amapá",
         "Tocantins" = "Tocantins",
         "Maranhï¿½o" = "Maranhão",
         "Piauï¿½" = "Piauí",
         "Cearï¿½" = "Ceará",
         "Paraï¿½ba" = "Paraíba",
         "Pernambuco" = "Pernambuco",
         "Alagoas" = "Alagoas",
         "Sergipe" = "Sergipe",
         "Bahia" = "Bahia",
         "Minas Gerais" = "Minas Gerais",
         "Espï¿½rito Santo" = "Espírito Santo",
         "Rio de Janeiro" = "Rio de Janeiro",
         "Sï¿½o Paulo" = "São Paulo",
         "Paranï¿½" = "Paraná",
         "Santa Catarina" = "Santa Catarina",
         "Rio Grande do Sul" = "Rio Grande do Sul",
         "Mato Grosso do Sul" = "Mato Grosso do Sul",
         "Mato Grosso" = "Mato Grosso",
         "Goiï¿½s" = "Goiás",
         "Distrito Federal" = "Distrito Federal",
         uf
  )
})

View(obitos_fetais_n_filhos_mortos)

