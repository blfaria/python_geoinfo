library(ggplot2)
library(dplyr)
library(rasterVis)

getwd()
setwd("C:/Users/Bruno/Documents")

# essa funçao serve para extrair a media do dataframe, que sera usada para imprimir as barras com a media, sera melhorado depois
media.do.dataframe <- function(ava){
  
  assiduidade = mean(ava$X2...Assiduidade)
  pontualidade  = mean(ava$X1..Pontualidade)
  organizacao = mean(ava$X3..OrganizaÃ.Ã.o.e.disciplina)
  relacionamento = mean(ava$X4..Relacionamento)
  interesse  = mean(ava$X5..Interesse)
  metodologia  = mean(ava$X6..Metodologia)
  avaliacao  = mean(ava$X7..AvaliaÃ.Ã.o)
  desenvolvimento  = mean(ava$X8..Desenvolvimento.profissional)
  compromisso = mean(ava$X9..Compromisso.institucional)
  etica = mean(ava$X10..Ã.tica)
  
  #colm = colMeans(etica)
  #nomes = x$Nome.do.professor.
  
  x = c("assiduidade","pontualidade","organizacao","relacionamento","interesse","metodologia","desenvolvimento","compromisso","etica")
  y = c(assiduidade,pontualidade,organizacao,relacionamento,interesse,metodologia,desenvolvimento,compromisso,etica)
  
  df.media <- data.frame(x,y)
  
  return(df.media)
}



getwd()

ava = read.csv("Avaliação de Desempenho Docente.csv")
str(ava)



df.medio = media.do.dataframe(ava)
  
##subsetting
for (name in unique(ava$Nome.do.professor)){
  print(name)
  # separa na variavel atual apenas a informaçao que vai usar
  atual = subset(ava,ava$Nome.do.professor==name)

  
  #################
  #a parte  de baixo nao foi alterada
  #################
  assiduidade = mean(atual$X2...Assiduidade)
  pontualidade  = mean(atual$X1..Pontualidade)
  organizacao = mean(atual$X3..OrganizaÃ.Ã.o.e.disciplina)
  relacionamento = mean(atual$X4..Relacionamento)
  interesse  = mean(atual$X5..Interesse)
  metodologia  = mean(atual$X6..Metodologia)
  avaliacao  = mean(atual$X7..AvaliaÃ.Ã.o)
  desenvolvimento  = mean(atual$X8..Desenvolvimento.profissional)
  compromisso = mean(atual$X9..Compromisso.institucional)
  etica = mean(atual$X10..Ã.tica)
  
  #colm = colMeans(etica)
  #nomes = x$Nome.do.professor.
  
  x = c("assiduidade","pontualidade","organizacao","relacionamento","interesse","metodologia","desenvolvimento","compromisso","etica")
  y = c(assiduidade,pontualidade,organizacao,relacionamento,interesse,metodologia,desenvolvimento,compromisso,etica)

  df <- data.frame(x,y)
  #################
  #a parte de cima nao foi alterada
  #################
  
  
  p = ggplot(df, aes(x, y, fill = as.factor(x))) + geom_bar(stat = "identity") +
    geom_density(data=df.medio) +
    xlab("group") +ylab("Medias das notas") +
    labs(fill="Criterios") + ggtitle(name) +ylim(0,5) #ylim faz com que todos os graficos gerados mostrem ate o valor 5
  
  
  # sequencia que faz a gravaçao
  p
  ggsave(paste(name,"180.jpg"),width = 250, height = 160,units = "mm", dpi = 180)
}













########testes

g = seq_along(ava)

##subsetting
pau = ava %>% 
    filter(Nome.do.professor. == "PAULO EVARISTO CABRAL DE OLIVEIRA")

mean(pau$X2...Assiduidade)

#or
paulo <- subset(ava, Nome.do.professor. == "DANIEL GUIMARÃfES SILVA")





       
       
