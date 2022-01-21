
dhh2007 = read.csv("./Data/dathh2007.csv",colClasses=c("idmen"="character"), header=TRUE)


aggData
read.csv(data[21])
length(unique(read.csv(data[21])$idind))

#e
# pipeline by idmen and year and count size of group (those in household in a given year)
aggDatae <- aggData %>% group_by(idmen,year) %>% summarise(count = n())
aggDatae


countOver4 <- 0
for (i in 1:nrow(aggDatae)){
  #count the number of households, in a given year, with over 4 people
  if (aggDatae[[i,3]] > 4){
    countOver4 <- countOver4 + 1
  }
}
countOver4

#f
#pipeline group by idmen and year and see if any individual, in the household for a given year, is Unemployed
# will return TRUE if atleast 1 person in Unemployed
aggDataf <- aggData %>% group_by(idmen,year) %>% summarize(UE = "Unemployed" %in% empstat)
aggDataf

countUE <- 0
for (i in 1:nrow(aggDataf)){
  #count occurrences of at least 1 unemployed in a given household in a given year
  if (aggDataf[[i,3]] == TRUE){
  countUE <- countUE + 1  
  }
}
countUE

#g
#piepline group data by idmen and year and see if individuals in household have matching professions
aggDatag <-  aggData %>% group_by(idmen,year) %>% summarize(SameProfession = anyDuplicated(profession))
#if there are any duplicate professions, value will be index of first duplicate. If no duplicates, it is 0
aggDatag

countSameProf <- 0
#check for instances of duplicate professions in a household in a given year
for (i in 1:nrow(aggDatag)){
  #count those occurrences
  if (aggDatag[[i,3]] != 0){
    countSameProf <- countSameProf + 1
  }
}
countSameProf

#h
countHHwithKids <- 0
# each row should represent an individual in aggregate data so simply checking if that person is in a household "Couple, with Kids"
for (i in 1:nrow(aggData)){
  if(aggData$mstatus[i] == "Couple, with Kids" & !is.na(aggData$mstatus[i])){
    countHHwithKids <- countHHwithKids + 1
  }
}
countHHwithKids  


#i
countInParis <- 0
#each row should represent individual so simply check if that person is in "Paris" 
for (i in 1:nrow(aggData)){
  
  if (!is.na(aggData$location[i])){
    if (aggData$location[i] == "Paris"){
    countInParis <- countInParis + 1
  }
    }
}
countInParis

#j
#group by household and year and measure ssize of group
aggDataj <- aggData %>% group_by(idmen,year) %>% summarise(count = n())
#find largest households in agiven year
bigFam <- max(aggDataj[,3])
#find index position of those
which(aggDataj[,3] == bigFam) #there were 2

#find indmen and year
idmen1Max <- aggDataj[[which(aggDataj[,3] == 14)[1],1]]
idmen2Max <- aggDataj[[which(aggDataj[,3] == 14)[2],1]]

#k - households present in 2010 and 2011 (present: at least one individiual in household has respondent = 1)
#group by idmen and year and check if at least 1 respondent in each household (and thus household present)
aggDatak <- aggData %>% group_by(idmen,year) %>% summarize(present = 1 %in% respondent)
aggDatak


countPresent <- 0
for (i in 1:nrow(aggDatak)){
  #count if household present (atleast 1 individual has respondent = 1 in that year)
  if (aggDatak[[i,3]] == TRUE & (aggDatak[[i,2]] == 2010 |aggDatak[[i,2]] == 2011)){
    countPresent <- countPresent + 1
  }
}
countPresent


test
test[[1]] <- c(1,2,4)
test[[2]] <- 15
test



# ncol(newAggData)
uniqueInd <- unique(newAggData$idind)
length(uniqueInd)
idAndChangeList <- list()


for (i in uniqueInd[1]:tail(uniqueInd,n=1)){
  index <- which(newAggData[,10] == uniqueInd[i]) # find all occurrences of this individual in aggregated data
  
  #only look at values of if individual repeated in data
  if (length(index)> 1){
    
    idAndChange <- uniqueInd[i]
    years <- newAggData[index,2] # find years
    
    for (j in index[2]:length(index)){
      #check if profession changed (compared to last year)
      if (newAggData[j,11] != newAggData[j-1,11] | newAggData[j,13] != newAggData[j-1,13]){
        idAndChange <- c(idAndChange,newAggData[j,2])
      }
    }
    
  }
  
}
index <- which(newAggData[,10] == uniqueInd[13]) # find all occurrences of this individual in aggregated data
index
years <- newAggData[index,2]
years

test <- list()
test[[1]] <- c(1,2,3,4)
newTest <- c(33,1,2,3)
test[[length(test)+1]] <- newTest
length(test)
test
val <- c()
for (i in newTest){
  val <-c(val,i) 
}
val

fread()
