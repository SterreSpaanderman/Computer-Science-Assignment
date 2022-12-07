install.packages("glue")
install.packages("vctrs")
install.packages("stringr")
install.packages("dplyr")
install.packages("numbers")
install.packages("stringdist")
install.packages("lsa")
install.packages("factoextra")
install.packages("cluster")
install.packages("fpc")
install.packages("psych")

library(readxl)
library(glue)
library(vctrs)
library(stringr)
library(dplyr)
library(numbers)
library(stringdist)
library(lsa)
library(factoextra)
library(cluster)
library(fpc)
library(psych)


#inserting data
data <- read_excel("C:/Users/sterr/Documents/Master/Blok 2/Computer Science/Data Paper Assignment.xlsx")
cleaneddata <- as.data.frame(data)

rep_str = c('inches'='inch', 'Inch'='inch', '"'='inch', '-inch'= 'inch', ' inch'= 'inch')
rep_str2 = c('Hertz' = 'hz', 'hertz'= 'hz', 'HZ' = 'hz', 'hz' = 'hz', '-hz' = 'hz',' hz' = 'hz', 'Hz' = 'hz')
#rep_str3 = c('/' = '', '-' = '')

for(i in 1:ncol(cleaneddata)){
  cleaneddata[,i] <- str_replace_all(cleaneddata[,i], rep_str)
  cleaneddata[,i] <- str_replace_all(cleaneddata[,i], rep_str2)
  #cleaneddata[,i] <- str_replace_all(cleaneddata[,i], rep_str3)
  cleaneddata[,i] <- tolower(cleaneddata[,i])
}

data <- cleaneddata

#get trainset and dataset
#split <- unique(sort(sample(1:1624, 1000, replace=TRUE))) 
#train <- cleaneddata[split,] 
#test <- cleaneddata[-split,]

############# LSH #################################

#choose the variables to be considered
titlewords <- list()
for(i in 1:nrow(data)){
  titlewords <- append(titlewords, unlist(strsplit(data$title[i], " +")))
}

adaptedtitlewords <- list()
for(i in (which(grepl("[[:alpha:]]", titlewords) & grepl("[[:digit:]]", titlewords)))|| which(grepl("[[:digit:]]", titlewords))){
  adaptedtitlewords <- append(adaptedtitlewords, unlist(titlewords[i]))
}
adaptedtitlewords <- unique(unlist(adaptedtitlewords))

#create input matrix of binary vectors for every product
input <- matrix(nrow=length(adaptedtitlewords), ncol=nrow(data))
rownames(input) <- adaptedtitlewords
colnames(input) <- rownames(data)

for(i in 1:nrow(input)){
  for(j in 1:ncol(input)){
    if(adaptedtitlewords[i] %in% unlist(strsplit(data$title[j], " +"))){
      input[i,j] <- 1
    }
    else{
      input[i,j] <- 0
    }
  }
}

#create permutations and fill signal matrix
n <- 4
perms <- matrix(nrow=n, ncol=nrow(input))
signal <- matrix(nrow=n, ncol=ncol(input))
colnames(signal) <- colnames(input)
non_zero_row_indexes <- apply(input, MARGIN = 2, FUN = function(x) which (x != 0) )

for(i in 1:nrow(perms)){
  perms[i,] <- sample(1:nrow(input), size = nrow(input))
  for (j in 1:ncol(input)){
    signal[i,j] <-  min(perms[i,non_zero_row_indexes[[j]]])
  }
}

for(i in 1:nrow(signal)){
  for(j in 1:ncol(signal)){
    vector <- c()
    for(k in 1:length(unlist(non_zero_row_indexes[j]))){
      vector <- append(vector, which(perms[i,]==unlist(non_zero_row_indexes[j])[k]))
    }
    signal[i,j] <- min(vector)
    
  }
}

#find possible duplicates
neighbours <- matrix(nrow=nrow(data), ncol=nrow(data))
rownames(neighbours) <- rownames(data)
colnames(neighbours) <- rownames(data)

for(i in 1:nrow(neighbours)){
  for(j in 1:ncol(neighbours)){
    if(i<j){
      counter <- 0
      for(k in 1:nrow(signal)){
        if(signal[k,i]==signal[k,j]){
          counter = counter + 1
        }
      }
      if(counter > 0){
        neighbours[i,j] <- 1
      }
      else{
        neighbours[i,j] <- 0
      }
    }
    else{
      neighbours[i,j] <- NA
    }
  }
  neighbours[i,i] <- 0
}

#create similarity matrix
similarity <- matrix(nrow=nrow(neighbours), ncol=ncol(neighbours))
rownames(similarity) <- rownames(neighbours)
colnames(similarity) <- colnames(neighbours)

#inf if products are on same webshop
#inf if products are not duplicates in neighbours matrix
#inf if different brands

brands <- unique(data$Brand)
brands <- brands[!is.na(brands)]
brands <- append(brands, c("tlc", "hisense", "insignia", "venturer", "dynex", "avue", "tcl", "optoma", "mitsubishi", "curtisyoung", "contex",  "pyle", "gpx", "viore", "elite", "azend", "hiteker"))

for(i in 1:nrow(data)){
  if(is.na(data$Brand[i])==TRUE){
    newtitlei <- tolower(str_remove_all(data$title[i]," "))
    for(j in 1:length(brands)){
      if(str_detect(newtitlei, brands[j])==TRUE){
        data$Brand[i] <- brands[j]
      }
    }
  }
}


for(i in 1:nrow(similarity)){
  for(j in 1:ncol(similarity)){
    if(i<j){
      if(neighbours[i,j] == 0){
        similarity[i,j] <- Inf
        similarity[j,i] <- Inf
      }
      if(data$shop[i] == data$shop[j]){
        similarity[i,j] <- Inf
        similarity[j,i] <- Inf
      }
      if(data$Brand[i]!=data$Brand[j]){
        similarity[i,j] <- Inf
        similarity[j,i] <- Inf
      }
      similarity[i,i] <- Inf
    }
  }
}


############### MSM ##################

mu <- 0.650
alpha <- 0.602
beta <- 0.000
gamma <- 0.756
numberpredictions <- 0
potentialduplicates <- 0

for(i in 1:nrow(similarity)){
  for(j in 1:ncol(similarity)){
    if(i>j){
      if(grepl("Inf", similarity[i,j])==FALSE){
        potentialduplicates <- potentialduplicates + 1
        numberpredictions <- numberpredictions + 1
        sim <- 0
        avgSim <- 0
        m <- 0
        w <- 0
        mwPerc <- 0
        
        indexesKVPstapi <- which(!is.na(data[i,]))
        indexesKVPi <- indexesKVPstapi[-c(1,2,3,4,length(indexesKVPstapi))]
        keysi <- colnames(data)[indexesKVPi]
        valuesi <- data[i,][!is.na(data[i,])][-c(1,2,3,4,length(indexesKVPstapi))]
        
        indexesKVPstapj <- which(!is.na(data[j,]))
        indexesKVPj <- indexesKVPstapj[-c(1,2,3,4,length(indexesKVPstapj))]
        keysj <- colnames(data)[indexesKVPj]
        valuesj <- data[j,][!is.na(data[j,])][-c(1,2,3,4,length(indexesKVPstapj))]
        
          for(k in 1:length(keysi)){
            for(l in 1:length(keysj)){
              keySim <- length(base::intersect(dimnames(qgrams(keysi[k], q=3))[[2]], dimnames(qgrams(keysj[l], q=3))[[2]]))/(length(dimnames(qgrams(keysi[k], q=3))[[2]]) + length(dimnames(qgrams(keysj[l], q=3))[[2]])-length(base::intersect(dimnames(qgrams(keysi[k], q=3))[[2]], dimnames(qgrams(keysj[l], q=3))[[2]])))
              if(!is.na(keySim)){
                if(keySim> gamma){
                  valueSim <- length(base::intersect(dimnames(qgrams(valuesi[k], q=3))[[2]], dimnames(qgrams(valuesj[l], q=3))[[2]]))/(length(dimnames(qgrams(valuesi[k], q=3))[[2]]) + length(dimnames(qgrams(valuesj[l], q=3))[[2]])-length(base::intersect(dimnames(qgrams(valuesi[k], q=3))[[2]], dimnames(qgrams(valuesj[l], q=3))[[2]])))
                  weight <- keySim
                  sim <- sim + weight * valueSim
                  m <- m+1
                  w <- w+1
                  keysi <- keysi[-k]
                  valuesi <- valuesi[-k]
                  keysj <- keysj[-l]
                  valuesj <- valuesj[-l]
                }
              }
            }
          }
            if(w>0){
              avgSim <- sim/w
            }
            mwPerc <- length(base::intersect(valuesi, valuesj))/(length(valuesi) + length(valuesj)-length(base::intersect(valuesi, valuesj))) 
        
          listtitlei <- strsplit(data$title[i], " ")
          listtitlej <- strsplit(data$title[j], " ")
          cosine <- length(base::intersect(unlist(listtitlei), unlist(listtitlej)))/(sqrt(length(unlist(listtitlei)))*sqrt(length(unlist(listtitlej))))
          levenshtein <- stringdist(data$title[i], data$title[j], method='lv')/max(length(unlist(listtitlei)), length(unlist(listtitlej)))
          TMWMSim <- alpha*cosine + levenshtein*beta
          titleSim <- 0
        
          if(TMWMSim > epsilon){
            titleSim <- 1
          }
          else{
            titleSim <- -1
          }
        
          if(titleSim == -1){
            theta1 <- m/min(length(valuesi), length(valuesj))
            theta2 <- 1-theta1
            hSim = theta1*avgSim + theta2*mwPerc
          }
          else{
            theta1 <- (1-mu)*(m/min(length(valuesi), length(valuesj)))
            theta2 <- 1-mu-theta1
            hSim <- theta1*avgSim + theta2*mwPerc + mu*titleSim
          }
        
          similarity[i,j] <- 1-hSim
      }
    }
  }
}

#### tussenstap #######

for(i in 1:nrow(similarity)){
  for(j in 1:ncol(similarity)){
    if(is.nan(similarity[i,j])||is.na(similarity[i,j])){
      similarity[i,j] <- 1
    }
    if(i<j){
      similarity[i,j] <- 1
    }
    if(similarity[i,j]==Inf){
      similarity[i,j] <- 1
    }
    similarity[i,i] <- 1
  }
}

############# clustering ######################

#create a list of all products in different clusters
duplicates <- list()

for(i in 1:nrow(similarity)){
  duplicates[i] <- rownames(data)[i]
}

adaptedsimilarity <- similarity

epsilon <- 0.522
while(min(adaptedsimilarity, na.rm=TRUE)<epsilon){
  inds = which(adaptedsimilarity == min(adaptedsimilarity, na.rm=TRUE), arr.ind=TRUE)
  rnames <- rownames(adaptedsimilarity)[inds[,1]]
  cnames <- colnames(adaptedsimilarity)[inds[,2]]

  duplicates[which(rownames(adaptedsimilarity)==rnames[1])] <- list(append(unlist(duplicates[which(rownames(adaptedsimilarity)==rnames[1])]),cnames[1]))
  duplicates[which(rownames(adaptedsimilarity)==cnames[1])] <- NULL

  mergedvector <- vector()
  for(k in 1:nrow(adaptedsimilarity)){
      mergedvector[k] <- min(adaptedsimilarity[k,which(colnames(adaptedsimilarity)==rnames[1])], adaptedsimilarity[k,which(colnames(adaptedsimilarity)==cnames[1])],na.rm=TRUE)
  }
  adaptedsimilarity[,which(colnames(adaptedsimilarity)==cnames[1])] <- mergedvector
  adaptedsimilarity[which(rownames(adaptedsimilarity)==cnames[1]),] <- mergedvector
  adaptedsimilarity <- adaptedsimilarity[,colnames(adaptedsimilarity)!=rnames[1]]
  adaptedsimilarity <- adaptedsimilarity[rownames(adaptedsimilarity)!=rnames[1],]
}

########## performance measure ###############
#how many duplicates found
counter <- 0
for(i in 1:length(duplicates)){
  if(length(unlist(duplicates[i]))>1){
    counter <- counter + length(unlist(duplicates[i]))
  }
}

#what are the real duplicates
actualduplicates <- 0
for(k in 1:length(names(which(table(data$modelID) > 1)))){
  actualduplicates <- actualduplicates + length(which(data$modelID==names(which(table(data$modelID) > 1))[k]))
}

#determine performance
pairquality <- counter/potentialduplicates
paircompleteness <- counter/actualduplicates
F1 <- potentialduplicates/(0.5*(nrow(similarity)-1)*(ncol(similarity)-1))
F1star <- (2*pairquality*paircompleteness)/(pairquality+paircompleteness)

print(paircompleteness)
print(pairquality)
print(F1)
print(F1star)

#create a plot against the treshhold of all measures
threshold <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.522, 0.6, 0.7, 0.8, 0.9, 0.999)
PQ <- c(0.006053391, 0.006180031, 0.006357327, 0.007547743, 0.01008054, 0.01033382, 0.01233473, 0.01557672, 0.01712173,0.01785624, 0.01790689)
PC <- c(0.3399716,0.3470839, 0.3570413, 0.4238976, 0.5661451, 0.5803698, 0.6927454, 0.8748222, 0.9615932,1,1)
F1 <- c(0.0590511, 0.0590511, 0.0590511, 0.0590511, 0.0590511, 0.0590511, 0.0590511, 0.0590511, 0.0590511, 0.0590511, 0.0590511)
F1star <- c(0.01189499,0.01214383,0.01249222,0.0148314,0.01980839,0.02030608,0.0242379,0.03060844,0.03364439,0.03508772,0.03518726)

plot(threshold, PQ, type = "l")  
plot(threshold, PC, type = "l")
plot(threshold, F1, type = "l")
plot(threshold, F1star, type = "l")


################## easy similarity #######################

#create similarity matrix
simplesimilarity <- matrix(nrow=nrow(neighbours), ncol=ncol(neighbours))
rownames(simplesimilarity) <- rownames(neighbours)
colnames(simplesimilarity) <- colnames(neighbours)

#inf if products are on same webshop
#inf if products are not duplicates in neighbours matrix
#inf if different brands

brands <- unique(data$Brand)
brands <- brands[!is.na(brands)]
brands <- append(brands, c("tlc", "hisense", "insignia", "venturer", "dynex", "avue", "tcl", "optoma", "mitsubishi", "curtisyoung", "contex",  "pyle", "gpx", "viore", "elite", "azend", "hiteker"))

for(i in 1:nrow(data)){
  if(is.na(data$Brand[i])==TRUE){
    newtitlei <- tolower(str_remove_all(data$title[i]," "))
    for(j in 1:length(brands)){
      if(str_detect(newtitlei, brands[j])==TRUE){
        data$Brand[i] <- brands[j]
      }
    }
  }
}


for(i in 1:nrow(simplesimilarity)){
  for(j in 1:ncol(simplesimilarity)){
    if(i<j){
      if(neighbours[i,j] == 0){
        simplesimilarity[i,j] <- Inf
        simplesimilarity[j,i] <- Inf
      }
      if(data$shop[i] == data$shop[j]){
        simplesimilarity[i,j] <- Inf
        simplesimilarity[j,i] <- Inf
      }
      if(data$Brand[i]!=data$Brand[j]){
        simplesimilarity[i,j] <- Inf
        simplesimilarity[j,i] <- Inf
      }
      simplesimilarity[i,i] <- Inf
    }
  }
}

simplepotentialduplicates <- 0
for(i in 1:nrow(simplesimilarity)){
  for(j in 1:ncol(simplesimilarity)){
    if(i>j){
      if(grepl("Inf", simplesimilarity[i,j])==FALSE){
        simplepotentialduplicates <- simplepotentialduplicates + 1
        titlei <- str_replace_all(data$title[i], "[[:punct:]]", "")
        titlei <- gsub(" ", "", titlei)
        titlej <- str_replace_all(data$title[j], "[[:punct:]]", "")
        titlej <- gsub(" ", "", titlej)
        simplesimilarity[i,j] <- 1- (length(base::intersect(dimnames(qgrams(titlei, q=3))[[2]], dimnames(qgrams(titlej, q=3))[[2]]))/(length(dimnames(qgrams(titlei, q=3))[[2]]) + length(dimnames(qgrams(titlej, q=3))[[2]])-length(base::intersect(dimnames(qgrams(titlei, q=3))[[2]], dimnames(qgrams(titlej, q=3))[[2]]))))
      }
    }
  }
}

for(i in 1:nrow(simplesimilarity)){
  for(j in 1:ncol(simplesimilarity)){
    if(is.nan(simplesimilarity[i,j])||is.na(simplesimilarity[i,j])){
      simplesimilarity[i,j] <- 1
    }
    if(i<j){
      simplesimilarity[i,j] <- 1
    }
    if(simplesimilarity[i,j]==Inf){
      simplesimilarity[i,j] <- 1
    }
    simplesimilarity[i,i] <- 1
  }
}

#create a list of all products in different clusters
simpleduplicates <- list()

for(i in 1:nrow(simplesimilarity)){
  simpleduplicates[i] <- rownames(data)[i]
}

simpleadaptedsimilarity <- simplesimilarity

epsilon <- 0.522
while(min(simpleadaptedsimilarity, na.rm=TRUE)<epsilon){
  inds = which(simpleadaptedsimilarity == min(simpleadaptedsimilarity, na.rm=TRUE), arr.ind=TRUE)
  rnames <- rownames(simpleadaptedsimilarity)[inds[,1]]
  cnames <- colnames(simpleadaptedsimilarity)[inds[,2]]
  
  simpleduplicates[which(rownames(simpleadaptedsimilarity)==rnames[1])] <- list(append(unlist(simpleduplicates[which(rownames(simpleadaptedsimilarity)==rnames[1])]),cnames[1]))
  simpleduplicates[which(rownames(simpleadaptedsimilarity)==cnames[1])] <- NULL
  
  simplemergedvector <- vector()
  for(k in 1:nrow(simpleadaptedsimilarity)){
    simplemergedvector[k] <- min(simpleadaptedsimilarity[k,which(colnames(simpleadaptedsimilarity)==rnames[1])], simpleadaptedsimilarity[k,which(colnames(simpleadaptedsimilarity)==cnames[1])],na.rm=TRUE)
  }
  simpleadaptedsimilarity[,which(colnames(simpleadaptedsimilarity)==cnames[1])] <- simplemergedvector
  simpleadaptedsimilarity[which(rownames(simpleadaptedsimilarity)==cnames[1]),] <- simplemergedvector
  simpleadaptedsimilarity <- simpleadaptedsimilarity[,colnames(simpleadaptedsimilarity)!=rnames[1]]
  simpleadaptedsimilarity <- simpleadaptedsimilarity[rownames(simpleadaptedsimilarity)!=rnames[1],]
}

########## performance measure ###############
#how many duplicates found
simplecounter <- 0
for(i in 1:length(simpleduplicates)){
  if(length(unlist(simpleduplicates[i]))>1){
    simplecounter <- simplecounter + length(unlist(simpleduplicates[i]))
  }
}

#what are the real duplicates
simpleactualduplicates <- 0
for(k in 1:length(names(which(table(data$modelID) > 1)))){
  simpleactualduplicates <- simpleactualduplicates + length(which(data$modelID==names(which(table(data$modelID) > 1))[k]))
}

#determine performance
simplepairquality <- simplecounter/simplepotentialduplicates
simplepaircompleteness <- simplecounter/simpleactualduplicates
simpleF1 <- simplepotentialduplicates/(0.5*(nrow(simplesimilarity)-1)*(ncol(simplesimilarity)-1))
simpleF1star <- (2*simplepairquality*simplepaircompleteness)/(simplepairquality+simplepaircompleteness)

print(simplepaircompleteness)
print(simplepairquality)
print(simpleF1)
print(simpleF1star)

#plot the relationships 
simplethreshold <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.522, 0.6, 0.7, 0.8, 0.9, 0.999)
simplePQ <- c(0,0,0,0.0020,0.0026,0.0037,0.0099,0.0171,0.0194,0.0172,0.0168)
simplePC <- c(0,0,0,0.0114,0.1465,0.2105,0.5533,0.9616,1,0.9644,0.9416)
simpleF1 <- c(0.0300,0.0300,0.0300,0.0300,0.0300,0.0300,0.0300,0.0300,0.0300,0.0300,0.0300)
simpleF1star <- c(0,0,0,0.0040,0.0051,0.0074,0.0194,0.0336,0.0382,0.0337,0.0329)

plot(simplethreshold, simplePQ, type = "l")  
plot(simplethreshold, simplePC, type = "l")
plot(simplethreshold, simpleF1, type = "l")
plot(simplethreshold, simpleF1star, type = "l")

teller <- 0
for(i in 1:nrow(similarity)){
  for(j in 1:ncol(similarity)){
    if(j>i){
      if(grepl("Inf", similarity[i,j])==TRUE){
        teller <- teller + 1
      }
    }
  }
}

