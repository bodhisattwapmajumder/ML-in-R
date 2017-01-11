-----------------------------------------------------------------------------------------------------
# The code has been written for the winter project guided by Latenview Analytics as a part of       #
# data preparation by Group 4                                                                       #
# Co-author: Bodhisattwa, Somya and Faichali                                                        #
-----------------------------------------------------------------------------------------------------

library(hash)
library(stringr)
library(tm)
library(RWeka)


setwd("C:/Users/Bodhisattya/Desktop/PGDBA/Latentview Winter/codes")
data_ngram=read.csv("CAX_Startup_Data.csv")

# enter the variable number
# k =  
myCorpus <- Corpus(VectorSource(data_ngram[,k]))
myStopwords <- c(stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
uni <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
uniTdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = uni))
frq1 <- findFreqTerms(uniTdm, lowfreq=50)
#uniTdm <- inspect(uniTdm)
FreqMat1 <- data.frame(sapply(seq(nrow(uniTdm)), function(i) sum(uniTdm[i,])))
library(slam)
FreqMat1_names <- data.frame(ST = row.names(uniTdm), Freq = FreqMat1[, 1])
FreqMat1_names <- FreqMat1_names[order(FreqMat1_names$Freq, decreasing = T), ]
row.names(FreqMat1_names) <- NULL
#folderaddress="G:\\PGDBA\\DecemberBreakLatentView\\Part1\\"
write.table(FreqMat1_names, file = "XXXX.tsv" , append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")
