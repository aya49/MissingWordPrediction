#Information gain functions: can also be imported, written out for clarity------------------------------------------------------

entropyKmeans <- function (tble, classNo) {
	classCol <- tble[,classNo]
	classTypes <- unique(classCol)
	entropyD <- 0
	for (i in 1:length(classTypes)) {
		pCiD <- length(which(classCol==classTypes[i]))/length(classCol)
		entropyD <- entropyD-(pCiD*(log2(pCiD)))
	}
	return(entropyD)
}
InformationGain <- function( tble, classNo, colNo ) {
	colVector <- tble[,colNo]
	colTypes <- unique(colVector)
	entropyBefore <- entropyKmeans(tble, classNo)
	entropyAfter <- 0
	for(i in 1:length(colTypes)) {
		entropyAfter <- entropyAfter + ((length(which(colVector==colTypes[i]))/length(colVector)) * entropyKmeans(tble[which(colVector==colTypes[i]),], classNo))
	}
	informationGain <- entropyBefore - entropyAfter
	return (informationGain)
}


orderCluster <- function(data,kmeansNo,centers){
	#Extract cluster and center
	kNo <- length(centers[,1])
	for (i in 1:kNo) {
		dt <- data[kmeansNo==i,]
		ct <- centers[i,]
		
		#Calculate distances
		dt <- cbind(dist = apply((dt[,1:ncol(dt)] - ct)^2,1,sum), dt) 
		#Sort
		return(dt[order(dt[,1]),])
	}
}

