#Carregamento dos pacotes
load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

packages <- c("ggplot2",'corrplot',"readxl")
load(packages)

## Leitura do banco de dados
dados <- read_xlsx('Dados.xlsx')

## seis primeiras observacoes
head(dados)

## seis ultimas observacoes
tail(dados)

## estrutura do banco
str(dados)

##tratamento dos dados

dados$Município = substr(dados$Município,8, 99)
dados = dados[,2:7]

##sumarização do banco de dados
summary(dados)

##boxplot
boxplot(dados[,-1])

##matriz de covariâncias
S <- cov(dados[,-1]); S

##matriz de correlações
R <- cor(X); R

##gráfico de correlações
corrplot(cor(dados[, -1]), order = "hclust") 

##padronização dos dados
dados.sc=scale(dados[-1])

##boxplot padronizado
boxplot(dados.sc)

## Analise de componentes principais
dados.pc <- prcomp(dados.sc)
summary(dados.pc)

## Screeplot
plot(dados.pc, type = "l")
abline(h=1, col='red')

## Plot dos coeficientes das duas primeiras componentes
plot(dados.pc$rotation,asp=1,pch=19)
text(dados.pc$rotation,rownames(dados.pc$rotation), pos=3)

## Escores das componentes principais
dados.pc$x

##biplot
ggplot2::autoplot(dados.pc, label = TRUE, loadings.label = TRUE)

#Comparação com IDHM Original
IDHM_O = read_xls("ipeadata_IDHM_1991_2000.xls")
IDHM_O <- IDHM_O[complete.cases(IDHM_O), ]
IDHM_O = IDHM_O[,5]

cor(IDHM_O, dados.pc$x) #correlação

#padronizar escores para melhor comparação
min_max <- fdadosmin_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

escores=as.data.frame(dados.pc$x)[1]
escores[1]
IDHM_alt = min_max(escores)

head(IDHM_alt); head(IDHM_O)



