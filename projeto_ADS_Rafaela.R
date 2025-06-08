library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/rafae/Downloads/Projeto ADS")
getwd()

# CRIANDO DATAFRAMES 

obitos_fetais <- read.csv2("obitos_fetais.csv",
                            stringsAsFactors = FALSE,
                            fileEncoding = "latin1",
                            na = c("", "NA", "n/d", "NULL", "-"))

colnames(obitos_fetais) <- c(
  "UF",
  "Óbitos por ocorrência"
)

# Remover os números no início da string
obitos_fetais$UF <- gsub("^[0-9]+\\s+", "", obitos_fetais$UF)

obitos_fetais$UF <- sapply(obitos_fetais$UF, function(uf) {
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

View(obitos_fetais)

dados <- obitos_fetais[order(obitos_fetais$`Óbitos por ocorrência`, decreasing = TRUE), ]

# Gráfico de barras
ggplot(dados, aes(x = reorder(UF, `Óbitos por ocorrência`), y = `Óbitos por ocorrência`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Óbitos por Ocorrência por Estado",
       x = "Unidade Federativa (UF)",
       y = "Óbitos por Ocorrência") +
  theme_minimal()

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


# Remover os números no início da string
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


View(obitos_fetais_tipos_partos)

# Criar a nova coluna de Região
obitos_fetais_tipos_partos$Regiao <- sapply(obitos_fetais_tipos_partos$UF, get_regiao)

# Transformar os dados em formato longo (long format)
dados_longos <- pivot_longer(obitos_fetais_tipos_partos, 
                             cols = c("Parto normal", "Cesario"), 
                             names_to = "TipoParto", 
                             values_to = "Percentual")
View(obitos_fetais_tipos_partos)

# Gráfico de barras empilhadas

library(ggplot2)

ggplot(dados_longos, aes(x = Regiao, y = Percentual, fill = TipoParto)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribuição dos Tipos de Parto por Região",
    x = "Região",
    y = "Percentual (%)",
    fill = "Tipo de Parto"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.1),
    text = element_text(size = 10)
  )


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

obitos_fetais_morte$`Não preenchido` <- NULL
obitos_fetais_morte$`Ignorado` <- NULL
View(obitos_fetais_morte)

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


View(obitos_fetais_morte)

# Criar a nova coluna de Região
obitos_fetais_morte$Regiao <- sapply(obitos_fetais_morte$UF, get_regiao)
View(obitos_fetais_morte)

# COLUNAS NAO PREENCHIDO E IGNORADO FORAM EXCLUIDAS PARA TER DADOS MAIS CONSISTENTES.
dados_longos_morte <- pivot_longer(obitos_fetais_morte,
                                   cols = c("Antes do parto", "Durante o parto", "Depois do parto"),
                                   names_to = "Momento",
                                   values_to = "Percentual")


library(ggplot2)

ggplot(dados_longos_morte, aes(x = Regiao, y = Percentual, fill = Momento)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribuição dos Óbitos Fetais por Momento do Parto e Região",
    x = "Região",
    y = "Percentual (%)",
    fill = "Momento do Óbito"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.1),
    text = element_text(size = 10)
  )


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

obitos_fetais_duracao_gestacao$`Não preenchido` <- NULL
obitos_fetais_duracao_gestacao$`Ignorado` <- NULL
View(obitos_fetais_duracao_gestacao)

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
obitos_fetais_duracao_gestacao$Regiao <- sapply(obitos_fetais_duracao_gestacao$UF, get_regiao)
View(obitos_fetais_duracao_gestacao)


dados_longos_duracao <- pivot_longer(
  obitos_fetais_duracao_gestacao,
  cols = c("Menos 22", "22 a 27", "28 a 31", "32 a 36", "37 a 41", "42+"),
  names_to = "Duração da Gestação",
  values_to = "Percentual"
)

ggplot(dados_longos_duracao, aes(x = Regiao, y = Percentual, fill = `Duração da Gestação`)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribuição dos Óbitos Fetais por Duração da Gestação e Região",
    x = "Região",
    y = "Percentual (%)",
    fill = "Duração da Gestação"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.1),
    text = element_text(size = 10)
  )


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

obitos_fetais_tipo_gestacao$`Não preenchido` <- NULL
obitos_fetais_tipo_gestacao$`Ignorado` <- NULL


View(obitos_fetais_tipo_gestacao)

obitos_fetais_n_filhos_mortos <- read.csv2("n_filhos_mortos.csv",
                                         stringsAsFactors = FALSE,
                                         fileEncoding = "latin1",
                                         na = c("", "NA", "n/d", "NULL", "-"))
obitos_fetais_n_filhos_mortos$Total <- NULL
# Excluir as colunas 10 a 25 (poucos dados)
obitos_fetais_n_filhos_mortos <- obitos_fetais_n_filhos_mortos[, -c(13:22)]

colnames(obitos_fetais_n_filhos_mortos) <- c(
  "UF",
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
obitos_fetais_n_filhos_mortos$UF <- gsub("^[0-9]+\\s+", "", obitos_fetais_n_filhos_mortos$UF)

View(obitos_fetais_n_filhos_mortos)
obitos_fetais_n_filhos_mortos$UF <- sapply(obitos_fetais_n_filhos_mortos$UF, function(uf) {
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
obitos_fetais_n_filhos_mortos$Regiao <- sapply(obitos_fetais_n_filhos_mortos$UF, get_regiao)
View(obitos_fetais_n_filhos_mortos)

obitos_long <- obitos_fetais_n_filhos_mortos %>%
  pivot_longer(
    cols = `01`:`09`,              # Colunas de número de filhos
    names_to = "Numero_filhos",    # Nova coluna com os rótulos (01, 02, ...)
    values_to = "Percentual"       # Nova coluna com os valores
  )

# Garantir que a coluna seja um fator ordenado
obitos_long$Numero_filhos <- factor(obitos_long$Numero_filhos, levels = sprintf("%02d", 1:9))

# Gráfico: média por número de filhos, por região
ggplot(obitos_long, aes(x = Numero_filhos, y = Percentual, color = Regiao, group = Regiao)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    title = "Média de óbitos fetais por número de filhos e região",
    x = "Número de filhos",
    y = "Percentual (%)",
    color = "Região"
  ) +
  theme_minimal()



