
dirname <- "/Users/silviaolabarriaga/Google Drive/REDcap/Dicionarios/RAnalysis"
setwd(dirname)

# read the sorted file
filename= "all-fields-sorted.csv"
tmp <-read.csv(filename,header=TRUE,sep=";", stringsAsFactors=FALSE)

library(wordcloud) 

#png("figs/wordcloud-fieldnames.png")

n=70

set.seed(142)   
p <- wordcloud(tmp$field, tmp$count, 
               max.words = n, scale=c(5,1),
               rot.per=.4, use.r.layout = FALSE,
               random.color = TRUE, colors=brewer.pal(8,"Dark2"))  
p
#dev.off()














############
# text mining
# see https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

# install packages - done once
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)  

# working dir 
dirname <- "/Users/silviaolabarriaga/Google Drive/REDcap/Dicionarios/RAnalysis"
setwd(dirname)

cname <- paste(dirname,"/texts",sep="")
dir(cname)

# read files
# split words beforehand with 
# tr '_' ' ' < all-fields-words.txt > all-fields-words
library(tm)

docs <- Corpus(DirSource(cname))
summary(docs)

# finish preparation
docs <- tm_map(docs, PlainTextDocument)
inspect(docs)

# create document term matrix
dtm <- DocumentTermMatrix(docs)   
dtm

# organize terms by frequency
freq <- colSums(as.matrix(dtm))   
length(freq) 
ord <- order(freq)

#  Start by removing sparse terms:  
# This makes a matrix that is 10% empty space, maximum. 
dtms <- removeSparseTerms(dtm, 0.1)   
inspect(dtms) 

# checking word frquency
freq[head(ord)] 
head(table(freq),20)
    
# prepare words for plot
wf <- data.frame(word=names(freq), freq=freq) 
head(wf)
     
# plot top frequencies
library(ggplot2)   

n=1
p <- ggplot(subset(wf, freq>=n), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


# wordcloud
library(wordcloud) 

png("wordcloud-fieldnames.png")
n=1
set.seed(142)   
p <- wordcloud(names(freq), freq, scale=c(5, .1), min.freq=n,colors=brewer.pal(6, "Dark2"))  
p <- title("words ")
p
dev.off()


