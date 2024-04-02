####  Big Data Real-Time Analytics com Python e Spark  ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/2.Big-Data-Real-Time-Analytics-com-Python-e-Spark/6.Pre-Processamento_de_Dados")
getwd()



## Importando Pacotes
library(readxl)         # carregar arquivos
library(dplyr)          # manipula dados
library(tidyr)          # manipula dados (funcao pivot_longer)
library(ggplot2)        # gera gráficos
library(patchwork)      # unir gráficos
library(corrplot)       # mapa de Correlação

library(randomForest)




#############################             Estudo de Caso             #############################


####  Pré Processamento de Dados Para E-Commerce Analytics



### Objetivo:

# - Este Estudo de Caso é uma continuação do trabalho iniciado no capítulo anterior.

# - O objetivo do Pré Processamento dos Dados é colocar tudo que for texto na sua representação numérica correspondente.
#   E além disso, precisamos garantir que os dados estejam na mesma escala.

# - Portanto agora aplicaremos técnicas de pré-processamento de variáveis categóricas (Label Encoding e One-HotEncoding) e Feature Scaling de variáveis
#   numéricas (Normalização e Padronização).

# - O objetivo é pré-processar os dados paraumaetapa de modelagem preditiva. 



### Definição do Problema

# - Uma empresa internacional de comércio eletrônico (E-commerce) que vende produtos eletrônicos deseja descobrir informações importantes de seu banco de dados
#   de clientes.

# - Os produtos ficam armazenados em um armazém na sede da empresa. Após concluir a compra no web site da empresa, o cliente recebe o produto em casa, em qualquer 
#   parte do mundo. Os produtos são enviados de Navio, Avião ou Caminhão, dependendo da região de entrega.

# - Em cada compra o cliente pode receber um desconto dependendo do peso do produto comprado. Cada cliente pode fazer chamadas ao suporte da empresa no caso de 
#   dúvidas ou problemas e após receber o produto o cliente pode deixar uma avaliação sobre a experiência de compra. O único dado pessoal sobre o cliente que está
#   disponível é o gênero.

# - Nosso trabalho neste Estudo de Caso é explorar os dados, compreender como estão organizados, detectar eventuais problemas e analisar os dados por diferentes
#   perspectivas.

# Trabalharemos com dados fictícios que representam dados reais de uma empresa de E-Commerce. Os dados estão disponíveis na pasta "dados".




#### Carregando os Dados
df <- data.frame(read.csv2("dados/df_eng.csv", sep = ","))
head(df)

## Realizando Análise Inicial (Sumário Estatístico, Veriricação de Valores NA, '' e especiais)

analise_inicial <- function(dataframe_recebido) {  # para encotrar linhas com caracter especial, vá para o fim do script
  # Sumário
  cat("\n\n####  DIMENSÕES  ####\n\n")
  print(dim(dataframe_recebido))
  cat("\n\n\n####  INFO  ####\n\n")
  print(str(dataframe_recebido))
  cat("\n\n\n####  SUMÁRIO  ####\n\n")
  print(summary(dataframe_recebido))
  cat("\n\n\n####  VERIFICANDO QTD DE LINHAS DUPLICADAS  ####\n\n")
  print(sum(duplicated(dataframe_recebido)))
  cat("\n\n\n####  VERIFICANDO VALORES NA  ####\n\n")
  valores_na <- colSums(is.na(dataframe_recebido))
  if(any(valores_na > 0)) {
    cat("\n-> Colunas com valores NA:\n\n")
    print(valores_na[valores_na > 0])
  } else {
    cat("\n-> Não foram encontrados valores NA.\n")
  }
  cat("\n\n\n####  VERIFICANDO VALORES VAZIOS ''  ####\n\n")
  valores_vazios <- sapply(dataframe_recebido, function(x) sum(x == ""))
  if(any(valores_vazios > 0)) {
    cat("\n-> Colunas com valores vazios \"\":\n\n")
    print(valores_vazios[valores_vazios > 0])
  } else {
    cat("\n-> Não foram encontrados valores vazios \"\".\n")
  }
  cat("\n\n\n####  VERIFICANDO VALORES COM CARACTERES ESPECIAIS  ####\n\n")
  caracteres_especiais <- sapply(dataframe_recebido, function(x) {
    sum(sapply(x, function(y) {
      if(is.character(y) && length(y) == 1) {
        any(charToRaw(y) > 0x7E | charToRaw(y) < 0x20)
      } else {
        FALSE
      }
    }))
  })
  if(any(caracteres_especiais > 0)) {
    cat("\n-> Colunas com caracteres especiais:\n\n")
    print(caracteres_especiais[caracteres_especiais > 0])
  } else {
    cat("\n-> Não foram encontrados caracteres especiais.\n")
  }
}

analise_inicial(df)


## Modificando todas as variáveis do tipo chr para factor e removendo coluna de "índice" chamado X
df <- dplyr::mutate_if(df, is.character, as.factor)
df <- df %>% select(-X)
str(df)





############      Pré-Processamento de Variáveis Categóricas (Label Encoding e One-HotEncoding)     ############


#### Label Encoding

# - Label Encoding (codificação de rótulos) é uma técnica de codificação para lidar com variáveis categóricas.
#   Nesta técnica, a cada rótulo é atribuído um número inteiro exclusivo com base na ordem alfabética.

# - Isso pode ser feito de maneira manual (usando dicionários) ou com o algoritmo .
# - Veremos os dois métodos na sequência.



## Método 1 (manual)

# -> Variável categórica 'prioridade_produto' (foi interpretado como uma variável categórica ordinal)
summary(df$prioridade_produto)

# Criando um Dicionário para Mapeamento (seguindo ordem alfabética)
mapeamento = c('alta' = 0, 'baixa' = 1, 'media' = 2)

# Aplicando a codificação de rótulos manualmente usando recode
df$prioridade_produto <- recode(df$prioridade_produto, !!!mapeamento)
table(df$prioridade_produto)


# -> Variável categórica 'modo_envio' (foi interpretado como uma variável categórica ordinal)
summary(df$modo_envio)

# Criando um Dicionário para Mapeamento (não foi seguido ordem alfabética)
mapeamento = c('Navio' = 0, 'Aviao' = 1, 'Caminhao' = 2)

# Aplicando a codificação de rótulos manualmente usando recode
df$modo_envio <- recode(df$modo_envio, !!!mapeamento)
table(df$modo_envio)



## Método 2

# -> Variável categórica 'genero' (foi interpretado como uma variável categórica nominal)
summary(df$genero)

# Usando a função factor para converter a variável categórica em fatores (Label Encoding)
df <- df %>%
  mutate(genero = as.numeric(factor(genero)) - 1)

table(df$genero)

str(df)






#### One-Hot Encoding

# -> Variável categórica 'corredor_armazem' (foi interpretado como uma variável categórica nominal)









############      Pré-Processamento de Variáveis Numéricas (Fature Scaling)     ############


#### Fature Scaling


