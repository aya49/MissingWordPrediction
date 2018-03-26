# aya43@sfu.ca; Last modified 201512

# Input: matrix of articles with random words deleted and collaboration information
# Output: Predicts what words were most likelly deleted from each article

root = "home/project_wd"
setwd(root)
dir.create(file.path(root, "result"), showWarnings = FALSE)

#install.packages("arules")
#install.packages("Matrix")
library(arules)
library(recommenderlab)
source("IG.R")

#Data directories
input_collab_dir = "data/Collaboration.csv"
input_words_dir = "data/DocumentWords.csv"
input_vocab_dir = "data/Vocabulary.csv"
result_final_dir = "result/final.txt"

#Data directories for intermediate results
processed_input_dir = "doc.txt"
doc_short_dir = "docShort.txt"
doc_matrix_dir = "docMatrix.csv"
doc_meta_dir = "docMeta3.txt"
result_kmeans_dir = "result/kmeans.txt"
result_matrix_dir = "result/matrix.txt"
result_ibcf_dir = "result/ibcf.txt"
result_fpm_dir = "result/fpmISC.RData"
result_fpmframe_dir = "result/fpmFrame.RData"
result_fpmIS_dir = "result/fpmIS.RData"
result_ubcf_dir = "result/ubcf.RData"

#Data preparation---------------------------------------------------

docMetaO <- as.data.frame(read.csv(input_collab_dir)) #every article is created in a group of 4, Collaborators.csv tells us who have worked together before
docMeta <- NULL

doc <- strsplit(scan(input_words_dir, what="", sep="\n"), split=",")
for(i in 1:length(doc)) {
  docMeta$docID[i] <- as.numeric(doc[[i]][1])
  docMeta$length[i] <- as.numeric(doc[[i]][2])
  n <- which(docMetaO$docID == docMeta$docID[i])
  docMeta$s1[i] <- docMetaO$s1[n]
  docMeta$s2[i] <- docMetaO$s2[n]
  doc[[i]] <- doc[[i]][-1]
  doc[[i]] <- doc[[i]][-1]
  doc[[i]] <- as.numeric(doc[[i]])
}
lapply(doc, write, processed_input_dir, append=TRUE, ncolumns=1000)

#Display all documents made by one person;
alonePeople <- c()
aloneDoc <- c()
for(i in 1:length(docMeta$docID)) {
  if (docMeta$s1[i] == docMeta$s2[i]) {
    alonePeople <- append(alonePeople, docMeta$s1[i])
    aloneDoc <- append(aloneDoc, docMeta$docID[i])
    docMeta$s2[i] <- NA
  }
}

#make list of people
peopleall <- c(docMeta$s1, docMeta$s2)
peopleall <- peopleall[-which(is.na(peopleall))]
people <- data.frame(matrix(0, ncol=3, nrow=length(unique(peopleall))))
colnames(people) <- c("sID", "freq", "alone")
people$sID <- unique(peopleall)
for(i in 1:length(people$sID)) {
  people$freq[i] <- length(which(peopleall==people$sID[i]))
  if (length(which(alonePeople==people$sID[i]))!=0) {
    people$alone[i] <- aloneDoc[which(alonePeople==people$sID[i])]
  }
}

#create list of words
wordO <- read.csv(input_vocab_dir, sep="\n")
words <- data.frame(matrix(0, ncol=9, nrow=length(wordO[[1]])))
colnames(words) <- c("order", "words", "count", "IG", "kmeans1", "kmeans2", "kmeans3", "kmeans4", "recno")
words$words <- wordO[[1]]
for (i in 1:length(doc)) {
  for (j in 1:length(doc[[i]])) {
    wordOrder <- which(words$words==doc[[i]][j])
    words$count[wordOrder] <- words$count[wordOrder]+1
  }
}
words <- words[ order(-words[,3]), ]
words$order <- c(1:length(words$words))

#remove words with freq<3 from words & doc-----------------------------------
words <- words[-which(words$count<8),]
words <- words[-which(words$count>250),]

delindex <- c()
for (i in 1:length(doc)) {
  for (j in 1:length(doc[[i]])) {
    if (length(which(words$words == doc[[i]][j]))==0) {
      delindex <- append(delindex, j)
    }
  }
  if (length(delindex)!=0) {
    doc[[i]] <- doc[[i]][-delindex]
    delindex <- c()
  }
}
lapply(doc, write, doc_short_dir, append=TRUE, ncolumns=1000)


#create binary matrix-------------------------------------------------------------------------

docMatrix <- matrix(0, nrow=length(doc), ncol=length(words$words))
colnames(docMatrix) <- c(words$words)
rownames(docMatrix) <- c(docMeta$docID)
for (i in 1:length(doc)) {
  for (j in 1:length(words$words)) {
    if (length(which(doc[[i]]==words$words[j]))!=0) {
      docMatrix[i,j] <- 1
    }
  }
}
write.csv(docMatrix, file=doc_matrix_dir, quote=FALSE)

docMatrixPeople <- matrix("", nrow=length(doc), ncol=length(people$sID))
colnames(docMatrixPeople) <- c(people$sID)
for (i in 1:length(doc)) {
  for (j in 1:length(people$sID)) {
    if (length(which(docMeta$s1[i]==people$sID[j] | docMeta$s2[i]==people$sID[j]))!=0) {
      docMatrixPeople[i,j] <- 1
    }
  }
}
docMatrixPeople <- docMatrixPeople[,-which(people$freq<4)] #delete people who made 1,2 articles


#Cluster: cluster together similar articles ---------------------------------------------------------------------------------------

library(cluster)
kmeans <- kmeans(docMatrix, 4, iter.max=100)
docMeta$kmeans <- kmeans$cluster
docKmeans <- matrix(0, nrow=length(doc), ncol=2)
docKmeans[,1] <- docMeta$docID
docKmeans[,2] <- docMeta$kmeans
write.csv(docKmeans[,c(1,2)], result_kmeans_dir, quote=FALSE, row.names=FALSE)

write.csv(docMeta, doc_meta_dir)
#write.csv(docMeta, "docMeta3IG.txt")

docMeta <- read.csv(doc_meta_dir)
#docMeta <- read.csv("docMeta3IG.txt")

jpeopleMiss <- list()
jpeopleRepeat <- list() #repeat of 512 and 860 are in cluster 1 20151128
clusterJIndex <- c()
for (i in 1:4) {
  clusterJIndex <- which(docMeta$kmeans==i)
  jpeople <- c(docMeta$s1[clusterJIndex], docMeta$s2[clusterJIndex])
  jpeople <- jpeople[-which(is.na(jpeople))]
  jpeopleRepeat[[i]] <- jpeople[which(duplicated(jpeople)==TRUE)]
  jpeopleMiss[[i]] <- people$sID[which(is.na(people$sID[match(people$sID,jpeople)]))]
}
people$sID[which(people$freq>4)]

#rank documents to centre
#docMatrix <- orderCluster(docMatrix,docMeta$kmeans,kmeans$centers)


#move repeat people
for (i in 1:length(jpeopleRepeat)) {
  if (length(jpeopleRepeat[[i]])!=0) {
    for (j in 1:length(jpeopleRepeat[[i]])) {
      #choose a repeated person
      clusterMiss <- c()
      clusterSize <- c()
      clusterDist <- c()
      for (k in 1:length(jpeopleMiss)) {
        if (jpeopleRepeat[[i]][j]%in%jpeopleMiss[[k]]) {
          clusterMiss <- append(clusterMiss, k)
          clusterSize <- append(clusterSize, length(which(docMeta$kmeans==k)))
          
        }
      }
      if (length(clusterMiss)!=0) {
        #assign to smaller cluster
        clusterNew <- clusterMiss[which.min(clusterDist)] #smaller cluster
        docOfRepeatI <- which(((docMeta$s1==jpeopleRepeat[[i]][j])|(docMeta$s2==jpeopleRepeat[[i]][j]))&(docMeta$kmeans==j))
        clusterDist <-c()
        for (k in 1:length(docOfRepeatI)) {
          dt <- docMatrix[docOfRepeatI[k],1:length(docMatrix[1,])]
          ct <- kmeans$centers[clusterNew,]
          clusterDist <- append(clusterDist, apply((dt[,1:7207] - ct)^2,1,sum))
        }
        docMeta$kmeans[docOfRepeatI[which.min(clusterDist)]] <- clusterNew #1=first person, can choose
        jpeopleMiss[[clusterNew]] <- jpeopleMiss[[clusterNew]][-which(jpeopleMiss[[clusterNew]]==jpeopleRepeat[[i]][j])]
        jpeopleRepeat[[i]] <- jpeopleRepeat[[i]][-j]
      }
    }
  }
}

aloneKmeans <- c()
for (i in 1:length(aloneDoc)) {
  aloneKmeans <- append(aloneKmeans, docMeta$kmeans[ which(docMeta$docID==aloneDoc[i]) ])
}

#Information Gain / Entropy----------------------------------------------

#Add cluster to binary matrix
docMatrixk <- cbind(docMeta$kmeans, docMatrix)
colnames(docMatrixk)[1] <- "kmeans"
docMatrixs1 <- cbind(docMeta$s1, docMatrix)
colnames(docMatrixs1)[1] <- "s1"
docMatrixs2 <- cbind(docMeta$s2, docMatrix)
colnames(docMatrixs2)[1] <- "s2"

words$IGs1 <- c(1:length(words$words))
words$IGs2 <- c(1:length(words$words))
for(i in 1:ncol(docMatrixk)) {
  words$IG[i-1] <- InformationGain(docMatrixk, 1, i)
  #words$IGs1[i-1] <- InformationGain(docMatrixs1, 1, i)
  #words$IGs2[i-1] <- InformationGain(docMatrixs2, 1, i)
}
for(i in 1:ncol(docMatrixs1)) {
  words$IGs1[i-1] <- InformationGain(docMatrixs1, 1, i)
  #words$IGs2[i-1] <- InformationGain(docMatrixs2, 1, i)
}

#count how many of each word in each category
wordCluster <- c()
for (i in 1:length(words$words)) {
  for (j in 1:length(doc)) {
    #list clusters word belongs to
    if (length(which(doc[[j]]==words$words[i]))!=0) {
      wordCluster <- append(wordCluster, docMeta$kmeans[j])
    }
  }
  words[i,5] <- length(which(wordCluster==1))
  words[i,6] <- length(which(wordCluster==2))
  words[i,7] <- length(which(wordCluster==3))
  words[i,8] <- length(which(wordCluster==4))
  wordCluster <- c()
}

words <- words[ order(-words$IG), ]
#starting from largest information gain word, check which cluster has most (>80%, <20%) of that word and recommend to that cluster


#remove words with count<20 and has low information gain (cut off at L shaped graph somewhere...)

delI <- which((words$count>25) & (words$count>=12) & (words$IG<.03))
delI <- append(delI, which((words$count<12) & (words$IG<.022)))
delI <- append(delI, which(words$IG<.0245))
words <- words[-delI,]

#Result Matrix----------------------------------------------------
resultMatrix <- matrix(0, nrow=length(doc), ncol=length(words$words))
colnames(resultMatrix) <- words$words
rownames(resultMatrix) <- docMeta$docID

resultIBCF <- read.csv(result_ibcf_dir, header=FALSE)
for (i in 1:length(resultIBCF[,1])) {
  for(j in 1:5) {
    colNoss <- which(colnames(resultMatrix)==resultIBCF[i,(j+1)])
    rowNoss <- which(rownames(resultMatrix)==resultIBCF[i,1])
    resultMatrix[rowNoss,colNoss] <- resultMatrix[rowNoss,colNoss]+(5-j+1)
  }
}

#write.csv(resultMatrix, file=, quote=FALSE)
resultMatrix <- read.csv(result_matrix_dir) #with tree
colnames(resultMatrix) <- words$words[1:length(resultMatrix[1,])]
rownames(resultMatrix) <- docMeta$docID

#People's preferred words (low accuracy)----------------------------------------------

doneDoc <- c()
for(i in 1:length(people$sID[which(people$freq>3)])) {
  istudentsdocNo <- which((docMeta$s1==people$sID[i]) | (docMeta$s2==people$sID[i])) #document index made by i'th person
  #freqOfWord <- data.frame(matrix(0, nrow=length(words$words), ncol=2))
  #freqOfWord[,1] <- words$words
  cat("Doc ", i, "\n", sep="")
  for(j in 1:length(words$words)) {
    wordinWhichDoc <- c()
    for (k in 1:length(istudentsdocNo)) {
      if (length(which(doc[[istudentsdocNo[k]]]==words$words[j]))!=0) { #if word is in student's doc, delete
        #freqOfWord[j,2] <- freqOfWord[j,2]+1
        wordinWhichDoc <- append(wordinWhichDoc, k)
      }
    }
    if ( length(wordinWhichDoc) > (length(istudentsdocNo)-length(wordinWhichDoc)) ) {
      wordnotinwhichDoc <- istudentsdocNo[-wordinWhichDoc]
      istudentsdocNo <- istudentsdocNo[-which((istudentsdocNo%in%doneDoc)==TRUE)]
      resultMatrix[istudentsdocNo,j] <- resultMatrix[istudentsdocNo,j]+1
      doneDoc <- append(doneDoc, istudentsdocNo)
    }
  }
}

cat("skipped, no words: ", "\n", sep="")

#FPM----------------------------------------------------------------

install.packages("arules")
library(arules)

#confidence=P(rhs|lhs); lift=P(rhs|lhs)/P(rhs)
docTrans <- as(t(docMatrix[1:600]), "transactions")
fpm <- apriori(docTrans, parameter= list(supp =.05, confidence = 0.8, minlen=2, target="rules"))
fpm <- sort(fpm, by="confidence")
fpm <- sort(fpm, by="lift")
fpmClosed <- apriori(docTrans, parameter= list(supp =0.05, confidence = 0.8, minlen=2, target="closed frequent itemsets"))
#quality(fpm)
#inspect(head(fpm,100))

#closed itemsets
fpmFrameC <- as(fpmClosed, "data.frame")
fpmISC <- list()
for (i in 1:length(fpmFrameC$items)) {
  stringg <- as.character(fpmFrameC[i,1])
  stringg <- gsub("\\{", "", stringg)
  stringg <- gsub("\\}", "", stringg)
  fpmISC[[i]] <- strsplit(stringg, split=",")
}

save(fpmISC, file=result_fpm_dir)
load(result_fpm_dir)

#association rules
fpmFrame <- as(fpm, "data.frame")
fpmIS <- list()
fpmFrame$recItem <- c(sample(c(0), length(fpmFrame$rules), replace=TRUE))
start <- Sys.time()
for (i in 1:length(fpmFrame$rules)) {
  stringg <- as.character(fpmFrame[i,1])
  stringg <- gsub("\\{", "", stringg)
  stringg <- gsub("\\}", "", stringg)
  stringg <- gsub(" => ", ",", stringg)
  stringg <- strsplit(stringg, split=",")
  fpmIS[[i]] <- as.numeric(stringg[[1]][1:(length(stringg[[1]])-1)])
  fpmFrame$recItem[i] <- (stringg[[1]][length(stringg[[1]])])
}
Sys.time()
fpmFrame$ISindex <- c(1:length(fpmFrame$recItem))
fpmFrame$rules <- NULL

save(fpmFrame, file=result_fpmframe_dir)
save(fpmIS, file=result_fpmIS_dir)
load(result_fpmframe_dir)
load(result_fpmIS_dir)

#keep only lift>3 or support>100
fpmFrame$ISindex <- c(1:length(fpmFrame$recItem))
fpmFrame1 <- fpmFrame[-which((fpmFrame$confidence==1) | (fpmFrame$lift<4)),]
over100I <- which(words$count>100)
over100IFrame <- c()
for (i in 1:length(over100I)) {
  over100IFrame <- append(over100IFrame, which( (fpmFrame$recItem==as.character(words$words[over100I[i]]))  & (fpmFrame$lift>1)))
}
fpmFrame2 <- fpmFrame[over100IFrame,]
fpmFrame <- rbind(fpmFrame1,fpmFrame2)

#closed
delI <- c()
for (i in length(fpmFrame[,1]):1) {
  if (!(i%in%delI)) {
    sameFPMsI <- which(all(fpmFrame[,1:4]%in%fpmFrame[i,1:4]))
    if (length(sameFPMsI)>0) {
      fpmISI <- fpmFrame$ISindex[sameFPMsI[1]]
      closedIS <- fpmIS[fpmISI]
      closedI <- fpmFrame$ISindex[fpmISI]
      for (j in 2:length(sameFPMsI)) {
        if (all(closedIS%in%fpmIS[fpmFrame$ISindex[sameFPMsI[j]]])) {
          closedIS <- fpmIS[[fpmFrame$ISindex[sameFPMsI[j]]]]
          delI <- append(delI, closedI)
          closedI <- fpmFrame$ISindex[sameFPMsI[j]]
        }
      }
    }
  }
  cat(i, "out of", length(fpmFrame[,1]), "\n")
}


#closed association rules, del non-closed
delI <- c()
for (i in 1:length(fpmFrame$recItem)) {
  stringg <- c(as.character(fpmIS[fpmFrame$ISindex[i]]), fpmFrame$recItem[i])
  delOr <- TRUE
  for (j in 1:length(fpmISC)) {
    if((all(fpmISC[[j]] %in% stringg))) {
      delOr <- FALSE
      if (!delOr) break
    }
  }
  if (DelOr) {
    delI <- append(delI, i)
  }
}
fpmFrame[-unique(delI),]




# load("fpmFrameCDone.RData")

countOfRecItems <- c()
for (i in 1:length(unique(fpmFrame$recItem))) {
  countOfRecItems <- append(countOfRecItems, words$count[which(words$words==as.numeric(unique(fpmFrame$recItem)[i]))])
}

fpmFrame3 <- fpmFrame
fpmFrame <- fpmFrame1

install.packages("fastmatch")
library(fastmatch)

word <- list()
for (i in 1:length(words$words)) {
  word[[i]] <- c(1)
}
for (i in 1:length(words$words)) {
  for(j in 1:length(doc)) {
    if (words$words[i]%in%doc[[j]]) {
      if (word[[i]]==c(1)) {
        word[[i]] <- docMeta$docID[j]
      } else {
        word[[i]] <- append(word[[i]], docMeta$docID[j])
      }
    }
  }
}

#5 min, 20 min for all
start <- Sys.time()
for(i in 1:length(word)) {
  cat("Doc ", i, sep="")
  Sys.time()
  for (j in 1:length(fpmIS)) {
    if (length(which(word[[i]]==as.numeric(fpmFrame$recItem[j])))==0) {
      itemSetMatch <- c()
      for (k in 1:length(fpmIS[[j]])) {
        itemSetMatch <- append(itemSetMatch, which(word[[i]]==fpmIS[[j]][k]))
      }
      if (length(itemSetMatch)==length(fpmIS[[j]])) {
        colNo <- which(colnames(resultMatrix)==fpmFrame$recItem[j])
        resultMatrix[i,colNo] <- resultMatrix[i,colNo]+1
      }
    }
  }
  cat(", done\n", sep="")
}
Sys.time()


#FPG Recommend--------------------------------------------------------

doc1 <- list()
kmeansindex <- which(docMeta$kmeans==1)
for (i in 1:length(kmeansindex)) {
  doc1[[i]] <- doc[[kmeansindex[i]]]
}
lapply(doc1, write, "doc1.txt", append=TRUE, ncolumns=1000)

doc2 <- list()
kmeansindex <- which(docMeta$kmeans==2)
for (i in 1:length(kmeansindex)) {
  doc2[[i]] <- doc[[kmeansindex[i]]]
}
lapply(doc2, write, "doc2.txt", append=TRUE, ncolumns=1000)

doc3 <- list()
kmeansindex <- which(docMeta$kmeans==3)
for (i in 1:length(kmeansindex)) {
  doc3[[i]] <- doc[[kmeansindex[i]]]
}
lapply(doc3, write, "doc3.txt", append=TRUE, ncolumns=1000)

doc4 <- list()
kmeansindex <- which(docMeta$kmeans==4)
for (i in 1:length(kmeansindex)) {
  doc4[[i]] <- doc[[kmeansindex[i]]]
}
lapply(doc4, write, "doc4.txt", append=TRUE, ncolumns=1000)

docMeta1 <- docMeta[which(docMeta$kmeans==1),]
docMeta2 <- docMeta[which(docMeta$kmeans==2),]
docMeta3 <- docMeta[which(docMeta$kmeans==3),]
docMeta4 <- docMeta[which(docMeta$kmeans==4),]





# docMeta$recno <- 0 #***
# docFPG <- scan("docFPG.txt", what="", sep="\n") #***
# docFPG <- strsplit(docFPG, split="," )
# docFPGfreq <- as.data.frame(0)
# for (i in 1:length(docFPG)) {
#   docFPGfreq[i,1] <- i #number the itemsets
#   docFPGfreq[i,2] <- as.numeric( docFPG[[i]][length(docFPG[[i]])] ) #frequency of itemset
#   docFPG[[i]] <- as.numeric(docFPG[[i]][1:(length(docFPG[[i]])-1)])
# }
# docFPGfreq <- docFPGfreq[order(-docFPGfreq[,2]), ] #order itemsets by frequency
# docFPGitems <- as.list(0) #order itemsets by frequency
# for(i in 1:length(docFPG)) {
#   docFPGitems[[i]] <- docFPG[[docFPGfreq[i,1]]]
# }
# result <- as.data.frame(matrix(0, nrow=length(doc), ncol=30)) #***
# for (i in 1:length(doc)) { #***
#   result1[i,1] <- docMeta$docID[i] #***2
#   docneeds <- c()
#   for (j in 1:length(docFPGitems)) {
#     #1 itemset, leave only missing values
#     for (k in 1:length(docFPGitems[[j]])) { # if item of itemset isn't already in doc, keep it as rec
#       if (length(which(doc[[i]] == docFPGitems[[j]][k])) == 0) { #*** 
#         docneeds <- append(docneeds, docFPGitems[[j]])
#       }
#     }
#     #if doc has some items from this itemset, then put rest of itemset as recommendation
#     if (length(docneeds) < length(docFPGitems[[j]])) {
#       for (k in 1:length(docneeds)) {
#         if (length(which(result[i,] == docneeds[k])) == 0) { #*** add recommendation to result if not already there
#           docMeta$recno[i] <- docMeta$recno[i]+1 #***2 increment recommendation#
#           result[i,docMeta$recno[i]+1] <- docneeds[k] #***2
#         }
#       }
#     }
#   }
# }

#Association Recommender----------------------------------------------------------------------------

recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")

rMatrixAns <- as(ansMatrix, "binaryRatingMatrix")

docMatrix <- cbind(docMatrix,docMeta$kmeans)
docMatrix <- docMatrix[,-length(docMatrix[1,])]
docMeta$No <- c(1:length(docMeta$docID))
rMatrix <- as(docMatrix, "binaryRatingMatrix")
for (i in 1:nrow(rMatrix)) {
  cat("doc: ", sep="")
  #Use modified Recommender function!!
  rRec <- Recommender(rMatrix[-i,], method="UBCF", param=list(nn=18)) #UBCF=30.8% nn=18! (cosin=29.2, matching=23.8) AR=12% IBCF=25.8%
  iTop5 <- predict(rRec, rMatrix[i,], n=10)
  iTop5 <- as(iTop5, "list")
  iTop5 <- iTop5[[1]]
  for (j in 1:length(iTop5)) {
    colNos <- which(colnames(resultMatrix)==iTop5[j])
    resultMatrix[i,colNos] <- resultMatrix[i,colNos]+((10-j+1))
  }
  cat(i, "\n", sep="")
}
save(resultMatrix, file=result_ubcf_dir)
load(result_ubcf_dir)

#seperate clusters
for (cluster in 1:4) {
  rMatrix <- as(docMatrix[which(docMeta$kmeans==cluster),], "binaryRatingMatrix") #***do for all clusters
  docMetaR <- docMeta[which(docMeta$kmeans==cluster),]
  for (i in 1:nrow(rMatrix)) {
    cat("doc: ", sep="")
    rRec <- Recommender(rMatrix[-i,], method="AR", param=list(support=.05, confidence=.7, maxlen=2)) #UBCF=30.8% nn=18! AR=12% IBCF=25.8%
    iTop5 <- predict(rRec, rMatrix[i,], n=5)
    iTop5 <- as(iTop5, "list")
    iTop5 <- iTop5[[1]]
    for (j in 1:5) {
      colNos <- which((colnames(resultMatrix)%in%iTop5[j])==TRUE)
      resultMatrix[docMeta$No[i],colNos] <- resultMatrix[docMeta$No[i],colNos]+(5-j+1)
    }
    cat(i, "\n", sep="")
  }
}

#seperate persons
for (persons in 1:length(people$sID)) {
  rMatrix <- as(docMatrix[which((docMeta$s1==persons)|(docMeta$s2==persons)),], "binaryRatingMatrix") #***do for all clusters
  docMetaR <- docMeta[which((docMeta$s1==persons)|(docMeta$s2==persons)),]
  for (i in 1:nrow(rMatrix)) {
    cat("doc: ", sep="")
    rRec <- Recommender(rMatrix[-i,], method="UBCF", param=list(nn=18)) #UBCF=30.8% nn=18! AR=12%
    iTop5 <- predict(rRec, rMatrix[i,], n=5)
    iTop5 <- as(iTop5, "list")
    iTop5 <- iTop5[[1]]
    for (j in 1:5) {
      colNos <- which((colnames(resultMatrix)%in%iTop5[j])==TRUE)
      resultMatrix[docMeta$No[i],colNos] <- resultMatrix[docMeta$No[i],colNos]+(5-j+1)
    }
    cat(i, "\n", sep="")
  }
}



#Decision Tree (low accuracy...)--------------------------------------------------
install.packages("C50")
library(C50)

docMatrixkt0 <- as.data.frame(cbind(docMatrix, docMeta$kmeans)) #add kmeans
docMatrixkt0 <- as.data.frame(sapply(docMatrixkt0, as.factor))
allwords <- words$words
start <- Sys.time()

for (i in 1:length(doc)) {
  cat("doc ", i, ": ", sep="")
  docMatrixktTEST <- docMatrixkt0[i,]
  docMatrixktTRAIN <- docMatrixkt0[-i,]
  clusteri <- docMeta$kmeans[i]
  for (j in 1:4) {
    if(clusteri==j) {
      docMatrixktTRAIN <- docMatrixktTRAIN[which(docMatrixktTRAIN[,ncol(docMatrixktTRAIN)]==j),]
    }
  }
  #for every word missing in i-th doc, create a tree
  delI <- c()
  for (j in 1:length(doc[[i]])) {
    delI <- append(delI, which(allwords==doc[[i]][j]))
  }
  if (length(delI)!=0) {
    treeWordI <- c(1:length(allwords))
    treeWordI <- treeWordI[-delI] #indexes of words not in doc
    for (j in 1:length(treeWordI)) {
      tree <- C5.0(docMatrixktTRAIN[,-treeWordI[j]], docMatrixktTRAIN[,treeWordI[j]])
      itreeWordIj <- predict(tree, docMatrixktTEST[-treeWordI[j]], type="class")
      itreeWordIj <- as.numeric(levels(itreeWordIj))[itreeWordIj]
      if ((docMatrixkt0[i,treeWordI[j]]==0) & (itreeWordIj==1)) {
        resultMatrix[i,treeWordI[j]] <- resultMatrix[i,treeWordI[j]]+1
      }
      cat(j, ", ", sep="")
    }
    cat("done: ", Sys.time(), "\n", sep="")
  } else {
    cat("skipped, no words: ", "\n", sep="")
    sys.time()
  }
}

#result random-------------------------------- 

candidateWords <- c()
for (i in 1:length(doc)) {
  for (j in 1:length(doc[[i]])) {
    #candidate words are words occuring 40+ times in cluster, starting with highest IG
    candidates <- words$words[which(words[,(as.numeric(docMeta$kmeans[i])+4)]>43)]
    if (length(which(candidates==doc[[i]][j]))!=0) {
      candidateWords <- candidates[-which(candidates==doc[[i]][j])]
    }
  }
  resultMatrix[i,sample(candidateWords, 5)] <- resultMatrix[i,sample(candidateWords, 5)] +1
  candidateWords <- c()
}

#resultFinal -----------------------------------------

#from all words
resultFinal <- as.data.frame(matrix(0, nrow=length(doc), ncol=6))
resultFinal[,1] <- docMeta$docID
for (i in 1:length(doc)) {
  recWords <- words$words[1:50]
  delI <- c()
  for (j in 1:length(doc[[i]])) {
    delTemp <- which(recWords==doc[[i]][j])
    if(length(delTemp)!=0) {
      delI <- append(delI, delTemp)
    }
  }
  if (length(delI)!=0) {
    recWords <- ans[-delI]
  }
  resultFinal[i,2:6] <- unique(recWords)[1:5]
}


#from resultMatrix
resultFinal <- as.data.frame(matrix(0, nrow=length(doc), ncol=6))
resultFinal[,1] <- docMeta$docID
for (i in 1:length(doc)) {
  recWords <- as.data.frame(which((resultMatrix[i,]>0)))
  colnames(recWords) <- "colI"
  if (length(recWords$colI)>0) {
    recWords$words <- as.numeric(rownames(recWords))
    rownames(recWords) <- c(1:length(recWords$colI))
    recWords$points <- resultMatrix[i,recWords$colI]
    recWords <- recWords[order(recWords$points, decreasing = TRUE),]
    #recWords <- colnames(resultMatrix)[ which(resultMatrix[i,] == sample(resultMatrix[i,which(resultMatrix[i,]>0)], 5)) ]
    #for (j in 1:min(length(recWords$colI),5)) {
    #  resultFinal[i,j+1] <- recWords$words[j]
    #}
    resultFinal[i,2:(min(length(recWords$colI),5)+1)] <- recWords$words[1:min(length(recWords$colI),5)]
  }
  #if recWords<5
  if (length(recWords$colI)<5) {
    otherRecWords <- words$words[which((words$words!=recWords$words)&(words$words!=doc[[i]]))]
    resultFinal[i,(length(recWords$colI)+2):6] <- otherRecWords[1:(5-length(recWords$colI))]
  }
  if (length(recWords$colI)==0) {
    otherRecWords <- words$words[which(words$words!=doc[[i]])]
    resultFinal[i,2:6] <- otherRecWords[1:5]
  }
}

write.csv(resultFinal, result_final_dir, row.names=FALSE, quote=FALSE)
