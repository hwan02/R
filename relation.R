#===================================================
#연관분석

install.packages("arules")
install.packages("igraph")
install.packages("combinat")
install.packages("KoNLP")
library(arules)
library(igraph)
library(combinat)
library(KoNLP)

useSejongDic()
getwd()
setwd('C:\\Users\\HUFS\\Desktop')
rules <- readLines("search.txt")

tran <- Map(extractNoun, rules)
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x){Filter(function(y){nchar(y)<=4&&nchar(y)>1&&is.hangul(y)},x)})

names(tran) <- paste("Tr", 1:length(tran), sep="")
names(tran)

wordtran <- as(tran, "transactions")
wordtab <- crossTable(wordtran)

ares <- apriori(wordtran, parameter=list(supp=0.009, conf=0.009))
inspect(ares)

#===================================================
# 시각화 작업

rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ", USE.NAMES=F)
rulemat <- do.call("rbind", rules)

ruleg <- graph.edgelist(rulemat[-c(1:16),], directed=F)
plot.igraph(ruleg, vertex.label=V(ruleg)$name, vertex.label.cex=1,
            vertex.size=30, layout=layout.fruchterman.reingold.grid)
