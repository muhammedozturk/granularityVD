library("stringr")
############Folders
#folders <- list.dirs("C:/Users/User/Downloads/2015-10-27-apache-jena-v2-11-0/153849-v1.0.0/src/jena-iri/src/main/java/org/apache/jena/iri/impl/")
folders <- list.dirs("E:/makaleler/makaleler/vulnerability/paper4/LLMVulne/dataset")

j <- 1
while(j<=length(folders))
{
	folders[j] <- paste(folders[j],"/") 
folders[j] <- str_replace_all(folders[j], " ", "")
j <- j+1
}
####################
allFiles <- c()
i <- 1
while(i<=length(folders)){


dosyalar <- list.files(path=folders[i], pattern="\\.cs", all.files=FALSE,
    full.names=TRUE)
uzunluk <- length(dosyalar)

k <- 1
while(k <= length(dosyalar)){
allFiles <- append(allFiles, dosyalar[k])
k <- k+1
}

i <- i+1
}
################################
dosyalar <- allFiles
uzunluk <- length(dosyalar)
#################################
##########2. kısım#################
library("tm")
k <- 1
while(k<uzunluk)
{
yazi <- read.delim(dosyalar[k])
yazi <- unlist(yazi)
yazi <- as.character(yazi)
data<- removePunctuation(yazi)
yazi <- data
yazi <- str_replace_all(yazi, "[\r\n]" , "")
write.table(yazi,file=dosyalar[k],sep="\t")
k <- k+1
}
####################################
list3 <- list("")
library('rlist')
k <-1 
while(k<uzunluk){
file1 <- read.table(dosyalar[k], sep = '=', quote = '')
list3 <- list.append(list3,file1)
print("##################")
print(k)
print("##################")
yazi <- list3
k <- k+1
}
yazi <- unlist(yazi)
yazi <- as.character(yazi)
###Bu kod gereksiz satırları "yazi" dan siler
i <- 2
uzunluk <- length(yazi)
vektor <- c(1)
while(i < uzunluk)
{
	vektor <- append(vektor,i)
	i <- i+2
print("##################")
print(i)
print("##################")
}
yazi <- yazi[-vektor]
library(tm)
yazi <- removeNumbers(yazi)


##################################
library(word2vec)
set.seed(123456789)
model <- word2vec(x = yazi, type = "cbow", dim = 15, iter = 5)
embedding <- as.matrix(model)
rowNumber <- dim(embedding)
colnames(embedding) <- paste0("WordVec_", 1:ncol(embedding))
write.csv(embedding, file="E:/makaleler/makaleler/vulnerability/paper4/LLMVulne/dataset/Calculator-master/dosya.txt")
#####################################
###creating vulnerability keywods
keywords <- c("select", "ValidateRequest","password","random","deserialize","xmldocument","process","select","form")
allwords <- rownames(embedding)
allwords <- tolower(allwords)
labels <- rep(0, length(allwords))
i <- 1
j <- 1
while(i<=length(keywords))
{
	while(j <= length(allwords))
	{
		if(keywords[i]==allwords[j])
			labels[j] <- 1
		else
			labels[j] <- 0
		j <- j+1
	}
	j <- 1 
	i <- i+1
}
########################################


