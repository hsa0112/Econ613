# Hamzah Ahmed
# Code for Econ 613
# Assignment 1
# E:\Duke\Semester 2\Applied Micro\Assignments\A1

rm(list = ls())


#recommended package to install for the Assignment
# install.packages("tidyverse")
#recommended packages plus used in A0
library(tidyverse)

#see all the digits (hopefully)
options(digits = 22)
memory.limit(20000)


# Set Working Directory (in the )
setwd("E:/Duke/Semester 2/Applied Micro/Assignments/A1")
getwd()
# options(digits = 22) # so we can see all digits (and not just x.yze15)

# find all files and then import them separately
dataFiles = list.files("E:/Duke/Semester 2/Applied Micro/Assignments/A1",pattern="*.csv") #all data should be .csv files so this should get all the names (including .csv extension)
# for (i in 1:(length(dataFiles)/2)) assign(dataFiles[i], read.csv(dataFiles[i],colClasses = c("idmen" = "character"),header = TRUE))
# for (i in 1+(length(dataFiles)/2):length(dataFiles)) assign(dataFiles[i], read.csv(dataFiles[i],colClasses = c("idind" ="character","idmen" = "character"),header = TRUE))

for (i in 1:length(dataFiles)) assign(dataFiles[i], read.csv(dataFiles[i],colClasses = c("idind" ="character","idmen" = "character"),header = TRUE))
#got warnings about colClasses sometimes not existing - believe that is because idind is not in hhDatas

#read all IDs as characters as there were issues with repetition due to R not fully reading values (some truncation issue)

# The data
# From French Statistics and Resources on Living Condition from '04 to '19
# 2 surveys each year
# dathh - survey of households in France; data is longitudinal
  # household identifier - idmen; time identifier - year; last migration year - myear (until 2014); year of moving into dwelling - datent; dumm for marriage status - mstatus; location - Paris, Rural, Urban X to Y; categorical moved since last survey - move

# datind: longitudinal data of individuals 
  # individual identifier - idind; household identifier - idmen; time identifier - year; basic info (gender, age, wage); employment status - empstat, DV for responded by the individual respondent? - respondent; categorical variable - profession


################################################
# Excercise 1
################################################
#a - number of household in 2007 - in some data, there are uniqueness issues so I checked both ways. For the sake of my assumption, I assume any duplicates within a dataset are errors on part of the data collector
# and each entry is unique(no repeats). Just for the sake of practice, I have counted in two ways to make sure

numHH07 <- dathh2007.csv%>%group_by(idmen)%>%summarize(count= n())
sum(numHH07$count)
nrow(dathh2007.csv)
#output is 10498

#b - num of HH with marital status = Couple with kids in 05
table(dathh2005.csv$mstatus)

# output is 3374

#c - number of individuals surveyed in 2008
numIND08 <- datind2008.csv%>%group_by(idind)%>%summarize(count= n())
sum(numIND08$count)
nrow(datind2008.csv)

# output is 25510

#d - num of individuals between 25 and 35 in 2016
# table(datind2016.csv$age)
# made a condition that only selected ages between 25 and 35 and chose that subset of the corresponding variable in the right year. 
condition <-  datind2016.csv$age >= 25 & datind2016.csv$age <= 35 
#length gives us the number of people between those ages
length(datind2016.csv$age[condition])

# output is 2765

#e - cross=table gender/profession in 09

?table
table(datind2009.csv$gender,datind2009.csv$profession)
#profession is a categorical variable so it will be some numbers

#f - distrbiutions of wages in 2005 and 2019: mean, sd, D9/D1, and Gini
# unsure if meant to aggregate or separate so did both

?cumsum

# separately

  #2005
#cannot do certain calculations with missing data so choosing to omit that. Many 0's but keeping that in as it may be likely that many individuals don't have an income for one reason or another
  hist(datind2005.csv$wage[!is.na(datind2005.csv$wage & datind2005.csv$wage != 0)])
  mean(datind2005.csv$wage[!is.na(datind2005.csv$wage) & datind2005.csv$wage != 0]) # cannot do mean (returns NA) if we don't omit missing data
  
  sd(datind2005.csv$wage[!is.na(datind2005.csv$wage) & datind2005.csv$wage != 0]) # cannot do mean (returns NA) if we don't omit missing data  
  
  D1 <- quantile(datind2005.csv$wage[!is.na(datind2005.csv$wage)& datind2005.csv$wage != 0],.1)
  D1 
  D9 <- quantile(datind2005.csv$wage[!is.na(datind2005.csv$wage)& datind2005.csv$wage != 0],.9)
  D9 # is 32340.4
  idr05 <- D9/D1
  idr05 # is infinite as D1 is 0
  
  ####################
  #calculating Gini
  # for simplicity, I will assume that individuals in each "bucket" earn the same amount
  # ie if, in decile 5, the value is 500 - I assume everybody in the 10% of all make 500.
  # for extra simplicity (scaling), will assume 1 person in each bucket (so in theory, we could just scale up all wages but shouldn't change stuff)
  #get deciles (10% - 100%). Also showing Lorenz Curve and Equality Curves
  
  ####################
  
  GINI <- function(inputData){
    deciles <- quantile(inputData,seq(.1,1,by=.00001)) # really small increments so we can accurately get areas and get corresponding values for deciles
    
    
    #extract data into vector
    decile_Vector <- c()
    for (i in 1:length(deciles)){
      decile_Vector <- c(decile_Vector,deciles[[i]])
    }
    #find total income (needed for finding % of total income at an index point)
    totalIncome <- sum(decile_Vector) #finding total
    
    cumInc <- cumsum(decile_Vector)/totalIncome #find cumulative income at each of the decile levels
    
    #plot for visuals
    plot(cumInc, type = "l",lwd = 3, ylab = "Cum Income Share") #"Lorenz" Curve approximation
    abline(a=0,b=1/length(deciles), lwd = 3, lty = "dotted" ) # full equality line
    # find area between curves ratio
    
    library(pracma) # needed to find area
    underLorenz <- trapz(1:length(deciles),cumInc) # way to calculate area using trapezoidal approximation
    area <- length(deciles)*1*.5
    gini <- (area-underLorenz)/area
    return(gini)
    
  }
  
  #input into created function
  inputData05 <- datind2005.csv$wage[!is.na(datind2005.csv$wage) & datind2005.csv$wage != 0]
  GINI(inputData05) 
  
    #2019
  hist(datind2019.csv$wage[!is.na(datind2019.csv$wage) & datind2019.csv$wage !=0])
  mean(datind2019.csv$wage[!is.na(datind2019.csv$wage) & datind2019.csv$wage !=0]) 

  sd(datind2019.csv$wage[!is.na(datind2019.csv$wage) & datind2019.csv$wage !=0])

    D1 <- quantile(datind2019.csv$wage[!is.na(datind2019.csv$wage) & datind2019.csv$wage !=0],.1)
  D1 # is 0
  D9 <- quantile(datind2019.csv$wage[!is.na(datind2019.csv$wage) & datind2019.csv$wage !=0],.9)
  D9 # is 40267
  idr19 <- D9/D1
  idr19 # is infinite as D1 is 0

  
  inputData19 <- datind2019.csv$wage[!is.na(datind2019.csv$wage) & datind2019.csv$wage !=0]
  GINI(inputData19) 

  # together
  #combine wage data together into vector
  wagesIn05And19 <- c(datind2005.csv$wage[!is.na(datind2005.csv$wage) & datind2005.csv$wage !=0],datind2019.csv$wage[!is.na(datind2019.csv$wage) & datind2019.csv$wage !=0])
  hist(wagesIn05And19)
  #make sure no missing values
  # wagesIn05And19 <- wagesIn05And19[!is.na(wagesIn05And19)]  
  # wagesIn05And19
  mean(wagesIn05And19)
  sd(wagesIn05And19) 
  
  D1 <- quantile(wagesIn05And19,.1)
  D1
  D9 <- quantile(wagesIn05And19,.9)  
  D9 
  
  idr05And19 <- D9/D1
  idr05And19
  
 
  
  inputData05And19 <- wagesIn05And19
  GINI(inputData05And19)
  
  
#g - distribution of age in 2010
  
  ?histogram
  # prop.table(datind2010.csv$age)
  hist(datind2010.csv$age)# general
  # library(Hmisc)
  summary(datind2010.csv$age)
  #definiting condition so that we look at just Males (to compare to females)
  condition <-  datind2010.csv$gender == "Male"
  
  #shape of distributions are similar but not identical so there is a difference between men and women
  hist(datind2010.csv$age[condition])
  summary(datind2010.csv$age[condition])
  hist(datind2010.csv$age[!condition])
  summary(datind2010.csv$age[!condition])
  
#h - number of individuals in paris in 2011

  #creating a vector of house id (assumed all unique even if idind match for some reason)
  hhNum <- datind2011.csv$idmen
  hhNum  
  # dathh2011.csv[998,8] == "Paris" # should be NA
  #count occurrences of Paris
  
  countInParis <- 0
  for (i in 1:length(hhNum)){
    #find matching household
    index <- match(hhNum[i],dathh2011.csv[,2])
    # if household location is not NA and is Paris, add to count  
    if (!is.na(dathh2011.csv[index,8])){
      if(dathh2011.csv[index,8] == "Paris"){
        countInParis <- countInParis + 1
      }
      
    }
  }  
 

  countInParis # 3514

    #pipeline group individuals by household number and summarize number of people in each household
  anothereWay <- datind2011.csv %>% group_by(idmen) %>% summarize(count = n())
    
  anothereWay
  
  counter <- 0
  for (i in 1:nrow(anothereWay)){
    #extract house id
    hhID <- anothereWay[[i,1]]
    #find indexed match in household data
    index <- match(hhID,dathh2011.csv[,2])
    #if household in Paris, add total number of household people to current value
    if (dathh2011.csv[index,8] == "Paris" & !is.na(dathh2011.csv[index,8])){
      counter <- counter + anothereWay[[i,2]]
    }
  }
  counter
  #same answer!
  
  
  
  #######################################################
  #Ex 2
  ######################################################
  #a read all the data (previously done) and combined them to form aggregate individual data
  #merge individual data
  indData <- datind2004.csv
  indData <- rbind(indData,datind2005.csv)
  indData <- rbind(indData,datind2006.csv)
  indData <- rbind(indData,datind2007.csv)
  indData <- rbind(indData,datind2008.csv)
  indData <- rbind(indData,datind2009.csv)
  indData <- rbind(indData,datind2010.csv)
  indData <- rbind(indData,datind2011.csv)
  indData <- rbind(indData,datind2012.csv)
  indData <- rbind(indData,datind2013.csv)
  indData <- rbind(indData,datind2014.csv)
  indData <- rbind(indData,datind2015.csv)
  indData <- rbind(indData,datind2016.csv)
  indData <- rbind(indData,datind2017.csv)
  indData <- rbind(indData,datind2018.csv)
  indData <- rbind(indData,datind2019.csv)
  
  length(unique(indData$idind)) #42868 with numerics. 100160 with characters
  length(unique(indData$idmen)) #41086 with numerics. 41086 with characters
    
  #b similar process as above but did it for household data
  #merge household data (by row)
  hhData <- dathh2004.csv
  hhData <- rbind(hhData,dathh2005.csv)
  hhData <- rbind(hhData,dathh2006.csv)
  hhData <- rbind(hhData,dathh2007.csv)
  hhData <- rbind(hhData,dathh2008.csv)
  hhData <- rbind(hhData,dathh2009.csv)
  hhData <- rbind(hhData,dathh2010.csv)
  hhData <- rbind(hhData,dathh2011.csv)
  hhData <- rbind(hhData,dathh2012.csv)
  hhData <- rbind(hhData,dathh2013.csv)
  hhData <- rbind(hhData,dathh2014.csv)
  hhData <- rbind(hhData,dathh2015.csv)
  hhData <- rbind(hhData,dathh2016.csv)
  hhData <- rbind(hhData,dathh2017.csv)
  hhData <- rbind(hhData,dathh2018.csv)
  hhData <- rbind(hhData,dathh2019.csv)
  
  length(unique(hhData$idmen)) #41084 with numerics and characters
  
  #c 
  colnames(indData)
  colnames(hhData)  
  intersect(  colnames(indData),
              colnames(hhData))
  #matching variables: X, idmen, year
  
  #d 
  # creating subsets of data that have matching variables (X, idmen, and year)

  aggData <- merge(hhData,indData,by = c("idmen","year"))
  
####### REAL PROBLEMS - WITH aggData ##########

  aggData
  nrow(aggData)
  
  #e
  # pipeline by idmen and year and count size of group (those in household in a given year)
  aggDatae <- aggData %>% group_by(idmen,year) %>% summarise(count = n())
  aggDatae
  
  length(which(aggDatae[,3] > 4))
  
  # countOver4 <- 0
  # for (i in 1:nrow(aggDatae)){
  #   #count the number of households, in a given year, with over 4 people
  #   if (aggDatae[[i,3]] > 4){
  #     countOver4 <- countOver4 + 1
  #   }
  # }
  # countOver4
  # 

  
  
    #f
  #pipeline group by idmen and year and see if any individual, in the household for a given year, is Unemployed
  # will return TRUE if atleast 1 person in Unemployed
  aggDataf <- aggData %>% group_by(idmen,year) %>% summarise(UE = "Unemployed" %in% empstat)
  aggDataf
  
  length(which(aggDataf$UE == TRUE))
  
  # countUE <- 0
  # for (i in 1:nrow(aggDataf)){
  #   #count occurrences of at least 1 unemployed in a given household in a given year
  #   if (aggDataf[[i,3]] == TRUE){
  #     countUE <- countUE + 1  
  #   }
  # }
  # countUE
  # 
  
  
  
  
  #g 
  #piepline group data by idmen and year and see if individuals in household have matching professions
  
  actualProfessions <- unique(aggData$profession)
  length(actualProfessions)
  notActualProfessions <- actualProfessions[is.na(actualProfessions) | actualProfessions %in% c("")]
  length(notActualProfessions)
  
  
  #making sure that we don't include "" and missing data
  aggDatag <-  aggData %>% group_by(idmen,year) %>% summarise(SameProfession = anyDuplicated(profession[setdiff(profession,notActualProfessions)]))
  
  #if there are any duplicate professions, value will be index of first duplicate. If no duplicates, it is 0
  aggDatag
  
  length(which(aggDatag$SameProfession != 0))
  
  # 
  # countSameProf <- 0
  # #check for instances of duplicate professions in a household in a given year
  # for (i in 1:nrow(aggDatag)){
  #   #count those occurrences
  #   if (aggDatag[[i,3]] != 0){
  #     countSameProf <- countSameProf + 1
  #   }
  # }
  # countSameProf
  # 
  
  
  
  #h by row
  
  countHHwithKids <- 0
  # each row should represent an individual in aggregate data so simply checking if that person is in a household "Couple, with Kids"
  for (i in 1:nrow(aggData)){
    if(aggData$mstatus[i] == "Couple, with Kids" & !is.na(aggData$mstatus[i])){
      countHHwithKids <- countHHwithKids + 1
    }
  }
  countHHwithKids  
  
  
  
  #i  - 2 ways
  
  #if meant individual in a year - by row
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
  
  
  #if mean unique individuals ever in Paris
  uniqueIdind <- unique(aggData$idind)
  aggDatai = aggData %>% group_by(idind) %>% summarize(fromParis = "Paris" %in% location)
  length(which(aggDatai$fromParis == TRUE))
  
  
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
  idmen1Max
  idmen2Max
  
  #k - households present in 2010 and 2011 (present: at least one individiual in household has respondent = 1)
  #group by idmen and year and check if at least 1 respondent in each household (and thus household present)
  nrow(dathh2010.csv) + nrow(dathh2011.csv)
  
  # skipp the comments - aggDatak3 is where my solution came from
  # aggDatak <- aggData %>% group_by(idmen,year) %>% summarise(present = 1 %in% respondent)
  # aggDatak
  # 
  # 
  # countPresent <- 0
  # for (i in 1:nrow(aggDatak)){
  #   #count if household present (atleast 1 individual has respondent = 1 in that year)
  #   if (aggDatak[[i,3]] == TRUE & (aggDatak[[i,2]] == 2010 |aggDatak[[i,2]] == 2011)){
  #     countPresent <- countPresent + 1
  #   }
  # }
  # countPresent
  # 
  # #since i went by row and didn't use unique to filter out for repeats, my code seemed to be fine once i switched the id's to characters
  # #hopefully this luck persists in ex 3
  # 
  # 
  # # IF QUESTION MEANT HOW MANY HOUSEHOLDS WERE BOTH IN 2010 and 2011 (ie idmen in both 2010 and 2011) and respondent = 1 for at least 1 person
  # 
  # aggDatak2 <- aggData %>% group_by(idmen) %>% summarise(present2 = ((2010 %in% year & 2011 %in% year) & 1 %in% respondent))
  # aggDatak2  
  # 
  # countPresent2 <- 0
  # for (i in 1:nrow(aggDatak2)){
  #   if (aggDatak2[[i,2]] == TRUE){
  #     countPresent2 <- countPresent2 + 1
  # }
  # }
  # countPresent2
  
  
  
  
  # IF QUESTION MEANT HOW MANY HOUSEHOLDS WERE BOTH IN 2010 and 2011 (ie idmen in both 2010 and 2011) but nothing about respondents
  
  aggDatak3 <- aggData %>% group_by(idmen) %>% summarise(present3 = ((2010 %in% year & 2011 %in% year)))
  aggDatak3  
  
  
  length(which(aggDatak3$present3 == TRUE))
  
  
  
  # countPresent3 <- 0
  # for (i in 1:nrow(aggDatak3)){
  #   if (aggDatak3[[i,2]] == TRUE){
  #     countPresent3 <- countPresent3 + 1
  #   }
  # }
  # countPresent3
  # 
  
  
############ INITIALLY, I DID CODE WITHOUT MERGED DATA - DIDN'T WANT TO DELETE #######################
  ###################### CAN SKIP (UNCOMMENT IF WANT TO RUN)#######################################
  
#   # for all these problems, I am assuming that they are asking about it within a given year
# 
#   #e - household with more than 4 individuals (in a given year) 
#   table(indData$idmen>=4) #table tells you the occurrences of each household - the condition tells you for how many this is true
#   #pipeline grouping by idmen and year and counting size of groups(individuals in household in a year)
#   indDataByHHe <- indData %>% group_by(idmen, year) %>% summarize(count = n())
#   
#   
#   #finding total number of houses, in a year, have at least four individuals
#   totBH <- 0
#   for (i in 1:nrow(indDataByHHe)){
#     if (indDataByHHe[[i,3]] > 4){
#       totBH <- totBH + 1
#     }
#   }
#   
#   totBH
#   
#   #f - num of households in which at least one member is unemployed
#   
#   #summarize will show if "Unemployed" exists in each household
#   #will have TRUE if at least one is unemployed
#   indDataByHHf <- indData %>% group_by(idmen,year) %>% summarize(UEinHH = ("Unemployed" %in% empstat))
#   
#   #count occurrences of TRUE - Unemployed in Group  
#   totUE <- 0
#   for (i in 1:nrow(indDataByHHf)){
#     if (indDataByHHf[[i,3]] == TRUE){
#       totUE <- totUE+1
#     }
#   }
#   totUE  
#   # SLIGHTLY DIFFERENT ANSWER - UNSURE WHY
# # #subset check
# # subsetInd <- indDataByHH[1:30,]
# # totUE <- 0
# # for (i in 1:nrow(subsetInd)){
# #   if (subsetInd[[i,2]] == TRUE){
# #     totUE <- totUE+1
# #   }
# # }
# 
# 
#   #g - number of households with atleast 2 people in same profession
# 
#   #pipeline again by household and check for duplicated professions by group in a year
# 
#   #summarize will show 0 if no duplicates and > 2 if any duplicates exist
#   
#   indDatabyHHg <- indData %>% group_by(idmen,year)%>% summarize(sameProf <- anyDuplicated(profession))
#   
#   #sameProf = 0 means no duplicates
#   # we are looking for values that aren't zero (so duplicate professions exist)
#   
#   #calculating number of household with some duplicate professions
#   totDupProf <- 0
#   for (i in 1:nrow(indDatabyHHg)){
#     if (indDatabyHHg[[i,3]]!= 0 ){
#       totDupProf <- totDupProf+1
#     }
#   }
#   totDupProf 
#   
#   # # #subset check
#   # subsetInd <- indDatabyHHg[1:10,]
#   # totDupProf <- 0
#   # for (i in 1:nrow(subsetInd)){
#   #   if (subsetInd[[i,2]]!= 0 ){
#   #     totDupProf <- totDupProf+1
#   #   }
#   # }
#   # totDupProf # 
#   
#   
#   
#   #h - number of individuals in the panel that are from household- Couple with kids
#   ?n()
#   #pipeline which showed number of individuals in each household
#   indDatabyHHh <- indData %>% group_by(idmen,year) %>% summarize(count = n())
#   indDatabyHHh  
#   
#   #match to household data and add individuals in households that are categorized as couples, with kids
#   totIndinHHWithKids <- 0
#   for (i in (1:nrow(indDatabyHHh))){
#     #match idmen in individual data in the same year
#     
#     hhID <- indDatabyHHh[[i,1]]
#     hhYear <- indDatabyHHh[[i,2]]
#     
#     # find instances where household ID matches 
#     IDs <- which(hhData[,2] == hhID)
#     
#     # find year that matches
#     years <- which(hhData[,3] == hhYear)
#     
#     # should, in theory, just be one value,if at all, (since all values SHOULD be unique)
#     index <- intersect(IDs,years)
#     #check if couple with kids
#     if (!isempty(index)){
#         if(hhData[index,6] == "Couple, with Kids" & !is.na(hhData[index,6])){
#       totIndinHHWithKids <- totIndinHHWithKids + indDatabyHHh[[i,3]]
#         }
#   }
#     
#   }
#   totIndinHHWithKids
#   
# #   # #subset check
# #   subsetInd <- indDatabyHHh[1:10,]
# #   subsetInd
# #   tot <- 0
# #   for (i in 1:nrow(subsetInd)){
# #     index <- match(subsetInd[[i,1]],hhData[,2])
# #     if (hhData[index,6] == "Couple, with Kids" & !is.na(hhData[index,6])) {
# #       totIndinHHWithKids <- totIndinHHWithKids + subsetInd[[i,2]]
# #     }
# #   }
# # 
# #   
# # tot #203629
# 
#   #i - number of people from paris (similar treatment as above, just checking if household in Paris vs couple, with kids)
#   indDatabyHHi <- indData %>% group_by(idmen,year) %>% summarize(count = n())
#   indDatabyHHi 
#   
#   #count number of individuals in households that are in Paris
#   totIndinParis <- 0
#   
#   for (i in (1:nrow(indDatabyHHi))){
#     
#     hhID <- indDatabyHHi[[i,1]]
#     hhYear <- indDatabyHHi[[i,2]]
#     
#     # find instances where household ID matches 
#     IDs <- which(hhData[,2] == hhID)
#     
#     # find year that matches
#     years <- which(hhData[,3] == hhYear)
#     
#     index <- intersect(IDs,years)
#     
#     if (!isempty(index)){
#       if(hhData[index,8] == "Paris" & !is.na(hhData[index,8])){
#         totIndinParis <- totIndinParis + indDatabyHHi[[i,3]]
#     } 
#     }
#     
#   }
#   totIndinParis
#   
#   
#   #j - find household with most people
#   indDataByHHj <- indData %>% group_by(idmen,year) %>% summarize(count = n())
#   indDataByHHj
#   #find maximum number of people in a household
#   max(indDataByHHj[,3])
#   # got 14
#   #find index that corresponds to it (could be multiple)
#   which(indDataByHHj[,3] == 14)
#   # got 69655 and 107202
#   indDataByHHj[[69655 ,1]] # got idmen
#   indDataByHHj[[69655 ,2]] #checking year
#   indDataByHHj[[69655 ,3]]# confirming it corresponds to 14 family members
#   
#   indDataByHHj[[107202 ,1]] # got idmen
#   indDataByHHj[[107202 ,2]] #checking year
#   indDataByHHj[[107202 ,3]]# confirming it corresponds to 14 family members
#   
#   
#   #k - just finding number of households in 2010 and 2011 (seems relatively straightforward)
#   anyDuplicated(dathh2010.csv$idmen)
#   anyDuplicated(dathh2011.csv$idmen)
#   totHH10and11 <- nrow(dathh2010.csv) + nrow(dathh2011.csv)
#   totHH10and11
#   # will show TRUE if at least one respondent in the household - present if at least 1 respondent for each household (by year)
#   indDataByHHk <- indData %>% group_by(idmen,year) %>% summarize(present = 1 %in% respondent) 
#   indDataByHHk
#   nrow(indDataByHHk)
#   nrow(hhData)
#   # check if any False - there are
#   length(which(indDataByHHk[,3] == FALSE))
#   countPresent <- 0
#   # checking if there was at least 1 respondent for each household and also checcking if the years matched
#   for (i in 1:nrow(indDataByHHk)){
#     
#     if (indDataByHHk[[i,3]] == TRUE & (indDataByHHk[[i,2]] == 2010 |indDataByHHk[[i,2]]== 2011)){
#     #add if at least one respondent in household and years matched
#         countPresent <- countPresent + 1
#     }
#   }
#   
#   countPresent
# # SLIGHTLY DIFFERENT ANSWER - UNSURE WHY
#     
    ######################## end of Skipped Section ########################
  ################################# end of Ex 2 ###################################
  
  
    
  ########################################
  # Ex 3
  ########################################
  
  #a - find how long households stay in survey

  hhTimeInSurvey = aggData %>% group_by(idmen) %>% summarize(timeInSurvey = max(year) - min(year)+1)
  hist(hhTimeInSurvey$timeInSurvey)
  summary(hhTimeInSurvey$timeInSurvey)
  
  
  
  
  table(hhData$idmen)
  uniqueIdmen <- unique(aggData$idmen) #getting unique household IDs
  length(uniqueIdmen) #41084 unique households
  # nrow(hhData)


  #create Matrix with ID number in col1 and time in survey will be filled in col2
  # timeInSurveyMat <- matrix(c(uniqueIdmen,rep(0,length(uniqueIdmen)*2)),nrow=length(uniqueIdmen),ncol=3)
  # colnames(timeInSurveyMat) <- c("idmen","time in survey","left for a bit")
  # timeInSurveyMat
  # 
  # for (i in 1:length(uniqueIdmen)){
  #   ID <- timeInSurveyMat[i,1] #gets survey household number number
  #   
  #   occ <- which(aggData[,1] == ID) # find occurrences of years
  #   # one assumption I am making here is that the LAST occurrence of a household in the aggregate data set represents when they left. If for, some reason, they are missing for a year in between but come back later, I don't count that as an exit.
  #   # ie, if household 1 is present in 2004-2006 and 2008, its entry year will be 2004 and exit will be 2008 (as it was not captured in 2009 onward) and they were in the survey for 2008 - 2004 + 1 = 5 years 
  #   
  #   entranceYear <- aggData[occ[1],2] #find first year
  #   exitYear <- aggData[tail(occ,n=1),2] #find last year
  #   
  #   timeInSurvey <- exitYear - entranceYear + 1 # find years in survey
  #   timeInSurveyMat[i,2] <- timeInSurvey #add it to dataframe
  # }
  
  
  # for (i in 1:length(uniqueIdmen)){
  #   ID <- timeInSurveyMat[i,1] #gets survey household number number
  #   occ <- which(aggData[,1] == ID) # find occurrences of years
  #   # one assumption I am making here is that the LAST occurrence of a household in the aggregate data set represents when they left. If for, some reason, they are missing for a year in between but come back later, I don't count that as an exit.
  #   # ie, if household 1 is present in 2004-2006 and 2008, its entry year will be 2004 and exit will be 2008 (as it was not captured in 2009 onward) and they were in the survey for 2008 - 2004 + 1 = 5 years 
  #   
  #   entranceYear <- aggData[occ[1],2] #find first year
  #   exitYear <- aggData[tail(occ,n=1),2] #find last year
  #   
  #   timeInSurvey <- exitYear - entranceYear + 1 # find years in survey
  #   timeInSurveyMat[i,2] <- timeInSurvey #add it to dataframe
  #   # check1 <- length(occ)
  #   # yearS <- 1:tail(occ,n=1)
  #   # check2 <- length(yearS)
  #   # if (check1 != check2){
  #   #   timeInSurveyMat[i,3] == 1
  #   # }
  # }
  
  # which(timeInSurveyMat[,2] == "0") # checking that there weren't errors (ie output should be 0)
  # # View(timeInSurveyMat)
  # # which(timeInSurveyMat[,3]==1)
  # as.numeric(timeInSurveyMat[,2])
  # # as.numeric(timeInSurveyMat[,3])
  # 
  # # as.numeric(datind2008.csv$idind)
  # hist(as.numeric(timeInSurveyMat[,2])) #visual distribution (have to convert to numeric)
  # summary(as.numeric(timeInSurveyMat[,2]))
  # 
  
  #b - see if household moved into currrent dwelling at the year of survey & plot share of individuals in that situation
  # i am breaking this down into 2 questions using 2 sets of data
  
  #first, using hhData to find houses that have same year as datent (by year) - assuming each row is unique
  
  hhDataByYearb <- hhData %>% group_by(idmen,year) %>% summarise(movedAtYearOfSurvey = year == datent) # group by year and will be TRUE if year is same as datent
  View(hhDataByYearb)
  nrow(hhDataByYearb)
  
  nrow(hhDataByYearb)
  
  
  length(which(hhDataByYearb[,3] == TRUE))
  
    
  #second, i am using the aggData to find the number of individuals who are in that situation
  
  method1ByYearMigration <- aggData %>% group_by(year) %>% summarise(migration = length(which(year == datent)))
  method1ByYearTotal <- aggData %>% group_by(year) %>% summarise(count = n())
  
  shareofPopByYear <- method1ByYearMigration[,2]/method1ByYearTotal[,2]
  
  
  # shareOfPop <- c(count04,count05,count06,count07,count08,count09,count10,count11,count12,count13,count14,count15,count16,count17,count18,count19) / c(tot04,tot05,tot06,tot07,tot08,tot09,tot10,tot11,tot12,tot13,tot14,tot15,tot16,tot17,tot18,tot19)
  # shareOfPop
  # shareOfPopMat <- matrix(c(c(04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19),shareOfPop),ncol = 2,nrow=length(shareOfPop),byrow = FALSE)
  # shareOfPopMat
  # 
  shareOfPopdf <- data.frame(c(04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19),shareofPopByYear)
  colnames(shareOfPopdf) <- c("year","Proportion")
  ggplot(shareOfPopdf, aes(x= year,y = Proportion))+
    geom_bar(stat="identity")
  
  shareOfPopM1 <- shareofPopByYear  

  # in04 <- which(aggData[,2] == 2004)
  # 
  # in05 <- which(aggData[,2] == 2005)
  # 
  # in06 <- which(aggData[,2] == 2006)
  # 
  # in07 <- which(aggData[,2] == 2007)
  # 
  # in08 <- which(aggData[,2] == 2008)
  # 
  # in09 <- which(aggData[,2] == 2009)
  # 
  # in10 <- which(aggData[,2] == 2010)
  # 
  # in11 <- which(aggData[,2] == 2011)
  # 
  # in12 <- which(aggData[,2] == 2012)
  # 
  # in13 <- which(aggData[,2] == 2013)
  # 
  # in14 <- which(aggData[,2] == 2014)
  # 
  # in15 <- which(aggData[,2] == 2015)
  # 
  # in16 <- which(aggData[,2] == 2016)
  # 
  # in17 <- which(aggData[,2] == 2017)
  # 
  # in18 <- which(aggData[,2] == 2018)
  # 
  # in19 <- which(aggData[,2] == 2019)
  # 
  # count04 <- count05 <- count06 <- count07 <- count08 <- count09 <- count10 <- count11 <- count12 <- count13 <- count14 <- count15 <- count16 <- count17 <- count18 <- count19 <- 0
  # tot04 <- tot05 <- tot06 <- tot07 <- tot08 <- tot09 <- tot10 <- tot11 <- tot12 <- tot13 <- tot14<- tot15<- tot16<- tot17<- tot18<- tot19 <- 0 
  # 
  # 
  # for (i in 1:nrow(aggData)){
  #   
  #   #checking for missing values (skip if either is missing)
  #   if (!is.na(aggData[i,2]) & !is.na(aggData[i,4])){
  #     
  #     #check if in proper year
  #     if (i %in% in04){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count04 <- count04+1
  #       }
  #       #add individual
  #       tot04 <- tot04+1
  #     }
  #     
  #     #check if in proper year
  #     else if (i %in% in05){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count05 <- count05+1
  #       }
  #       #add individual
  #       tot05 <- tot05+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in06){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count06 <- count06+1
  #       }
  #       #add individual
  #       tot06 <- tot06+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in07){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count07 <- count07+1
  #       }
  #       #add individual
  #       tot07 <- tot07+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in08){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count08 <- count08+1
  #       }
  #       #add individual
  #       tot08 <- tot08+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in09){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count09 <- count09+1
  #       }
  #       #add individual
  #       tot09 <- tot09+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in10){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count10 <- count10+1
  #       }
  #       #add individual
  #       tot10 <- tot10+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in11){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count11 <- count11+1
  #       }
  #       #add individual
  #       tot11 <- tot11+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in12){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count12 <- count12+1
  #       }
  #       #add individual
  #       tot12 <- tot12+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in13){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count13 <- count13+1
  #       }
  #       #add individual
  #       tot13 <- tot13+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in14){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count14 <- count14+1
  #       }
  #       #add individual
  #       tot14 <- tot14+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in15){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count15 <- count15+1
  #       }
  #       #add individual
  #       tot15 <- tot15+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in16){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count16 <- count16+1
  #       }
  #       #add individual
  #       tot16 <- tot16+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in17){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count17 <- count17+1
  #       }
  #       #add individual
  #       tot17 <- tot17+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in18){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count18 <- count18+1
  #       }
  #       #add individual
  #       tot18 <- tot18+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in19){
  #       if (aggData[i,2] == aggData[i,4]){
  #         # individual has same year and datent
  #         count19 <- count19+1
  #       }
  #       #add individual
  #       tot19 <- tot19+1
  #     }
  #   }
  #   
  # }
  

  # c - basically asking for the same thing, but this time, using myear and move (before and after 2014)
  # breaking up into 2 chunks again
  
  # first part
  #assuming nobody leaves survey and comes back
  # condition <- if (year <= 2014){
  # 
  #     migration = year == myear # TRUE if year is equal to the move year
  # } else if(year > 2004){
  #   migration = move == 1 # TRUE if they moved since last year (ie they moved this year)
  # }
  # 
  
  # same grouping but now checking if either myear == year or move == 1 (both imply they moved)
  
  # first part - whether households migrated that year or not
  hhDataByYearc <- hhData %>% group_by(idmen,year) %>% summarise(
    migration = if (year <= 2014){
    migration = year == myear # TRUE if year is equal to the move year
  } 
  else if(year > 2014){
    migration = move == 2 # TRUE if they moved since last year (ie they moved this year)
  }
  )
  
  View(hhDataByYearc)  
  
  #second, i am using the aggData to find the number of individuals who are in that situation
  
  method2ByYearMigration <- aggData %>% group_by(year) %>% summarise(migration = length(which(
    if (year <= 2014){
    migration = year == myear # TRUE if year is equal to the move year
  } 
  else if(year > 2014){
    migration = move == 2 # TRUE if they moved since last year (ie they moved this year)
  })))
  
  method2ByYearTotal <- aggData %>% group_by(year) %>% summarise(count = n())
  
  shareofPopByYear <- method2ByYearMigration[,2]/method2ByYearTotal[,2]
  shareOfPopM2 <- shareofPopByYear
  
  # shareOfPop <- c(count04,count05,count06,count07,count08,count09,count10,count11,count12,count13,count14,count15,count16,count17,count18,count19) / c(tot04,tot05,tot06,tot07,tot08,tot09,tot10,tot11,tot12,tot13,tot14,tot15,tot16,tot17,tot18,tot19)
  # shareOfPop
  # shareOfPopMat <- matrix(c(c(04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19),shareOfPop),ncol = 2,nrow=length(shareOfPop),byrow = FALSE)
  # shareOfPopMat
  # 
  shareOfPopdf <- data.frame(c(04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19),shareofPopByYear)
  colnames(shareOfPopdf) <- c("year","Proportion")
  ggplot(shareOfPopdf, aes(x= year,y = Proportion))+
    geom_bar(stat="identity")
  
  # 
  in04 <- which(aggData[,2] == 2004)

  in05 <- which(aggData[,2] == 2005)

  in06 <- which(aggData[,2] == 2006)

  in07 <- which(aggData[,2] == 2007)

  in08 <- which(aggData[,2] == 2008)

  in09 <- which(aggData[,2] == 2009)

  in10 <- which(aggData[,2] == 2010)

  in11 <- which(aggData[,2] == 2011)

  in12 <- which(aggData[,2] == 2012)

  in13 <- which(aggData[,2] == 2013)

  in14 <- which(aggData[,2] == 2014)

  in15 <- which(aggData[,2] == 2015)

  in16 <- which(aggData[,2] == 2016)

  in17 <- which(aggData[,2] == 2017)

  in18 <- which(aggData[,2] == 2018)

  in19 <- which(aggData[,2] == 2019)

  # count04m <- count05m <- count06m <- count07m <- count08m <- count09m <- count10m <- count11m <- count12m <- count13m <- count14m <- count15m <- count16m <- count17m <- count18m <- count19m <- 0
  # tot04m <- tot05m <- tot06m <- tot07m <- tot08m <- tot09m <- tot10m <- tot11m <- tot12m <- tot13m <- tot14m<- tot15m<- tot16m<- tot17m<- tot18m<- tot19m <- 0 
  # 
  # for (i in 1:nrow(aggData)){
  # 
  #   #up to 2014, we are checking if myear is missing. AFter 2014, we check if move is missing
  #   
  #   #checking for missing values (skip if either is missing)
  #   if (!is.na(aggData[i,2]) & !is.na(aggData[i,5])){
  #     
  #     #check if in proper year
  #     if (i %in% in04){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count04m <- count04m+1
  #       }
  #       #add individual
  #       tot04m <- tot04m+1
  #     }
  #     
  #     #check if in proper year
  #     else if (i %in% in05){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count05m <- count05m+1
  #       }
  #       #add individual
  #       tot05m <- tot05m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in06){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count06m <- count06m+1
  #       }
  #       #add individual
  #       tot06m <- tot06m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in07){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count07m <- count07m+1
  #       }
  #       #add individual
  #       tot07m <- tot07m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in08){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count08m <- count08m+1
  #       }
  #       #add individual
  #       tot08m <- tot08m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in09){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count09m <- count09m+1
  #       }
  #       #add individual
  #       tot09m <- tot09m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in10){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count10m <- count10m+1
  #       }
  #       #add individual
  #       tot10m <- tot10m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in11){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count11m <- count11m+1
  #       }
  #       #add individual
  #       tot11m <- tot11m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in12){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count12m <- count12m+1
  #       }
  #       #add individual
  #       tot12m <- tot12m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in13){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count13m <- count13m+1
  #       }
  #       #add individual
  #       tot13m <- tot13m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in14){
  #       if (aggData[i,2] == aggData[i,5]){
  #         # individual has same year and myear
  #         count14m <- count14m+1
  #       }
  #       #add individual
  #       tot14m <- tot14m+1
  #     }}
  #   
  #   # # # # SHIFT IN CHECKS # # # #
  #   
  #   #after 2014, we are checking if move variable is missing and then using that if it is there
  #   
  #   # ASSUMED move == 2 means Migrated (new house)
  #   #skip over missing
  #   else if (!is.na(aggData[i,2]) & !is.na(aggData[i,7])){
  #     
  #       #check if in proper year
  #     if (i %in% in15){
  #       if (aggData[i,7] == 2){
  #         # individual has same year and myear
  #         count15m <- count15m+1
  #       }
  #       #add individual
  #       tot15m <- tot15m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in16){
  #       if (aggData[i,7] == 2){
  #         # individual has same year and myear
  #         count16m <- count16m+1
  #       }
  #       #add individual
  #       tot16m <- tot16m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in17){
  #       if (aggData[i,7] == 2){
  #         # individual has same year and myear
  #         count17m <- count17m+1
  #       }
  #       #add individual
  #       tot17m <- tot17m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in18){
  #       if (aggData[i,7] == 2){
  #         # individual has same year and myear
  #         count18m <- count18m+1
  #       }
  #       #add individual
  #       tot18m <- tot18m+1
  #     }
  #     #check if in proper year
  #     else if (i %in% in19){
  #       if (aggData[i,7] == 2){
  #         # individual has same year and myear
  #         count19m <- count19m+1
  #       }
  #       #add individual
  #       tot19m <- tot19m+1
  #     }
  #   }
  #   
  # }
  # 
  # shareOfPopm <- c(count04m,count05m,count06m,count07m,count08m,count09m,count10m,count11m,count12m,count13m,count14m,count15m,count16m,count17m,count18m,count19m) / c(tot04m,tot05m,tot06m,tot07m,tot08m,tot09m,tot10m,tot11m,tot12m,tot13m,tot14m,tot15m,tot16m,tot17m,tot18m,tot19m)
  # shareOfPopm
  # shareOfPopmMat <- matrix(c(c(04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19),shareOfPopm),ncol = 2,nrow=length(shareOfPop),byrow = FALSE)
  # shareOfPopmMat
  # shareOfPopmdf <- data.frame(c(04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19),shareOfPopm)
  # colnames(shareOfPopmdf) <- c("year","Proportion")
  # ggplot(shareOfPopmdf, aes(x= year,y = Proportion))+
  # geom_bar(stat="identity")
  
  #d
  
  # combinedData <- rbind(shareOfPop,shareOfPopm)
  method1 <- rep("Method1",16)
  method2 <- rep("Method2",16)
  method <- c(method1,method2)
  shareOfProportion <- rbind(shareOfPopM1,shareOfPopM2)
  
  
  combinedData <- data.frame(c(04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19),method,shareOfProportion)
  View(combinedData)
  colnames(combinedData) <- c("year","Method","Proportion")
  
  ggplot(combinedData, aes(x=year, y = Proportion, fill = Method)) +
    geom_bar(stat = "identity",position = "dodge")
  
  length(which(is.na(aggData[,2]))) #none missing
  length(which(is.na(aggData[,4])))
  #245 missing
  
  
  # below only work if you uncomment inXY from above
  until14 <- c(in04,in05,in06,in07,in08,in09,in10,in11,in12,in13,in14)
  after14 <- c(in15,in16,in17,in18,in19)  
  length(which(is.na(aggData[until14,5])))
  #6446 missing
  length(which(is.na(aggData[after14,7])))
  #25250 missing  
  
    
  
  
  #e - asked about migration so using Method2 data (Method1 said moved; Method2 said migrated)
  View(hhDataByYearc) #using method 2
  View(hhDataByYearb) #using method 1
  
  # find row for which household migrated (according to Method2)
  # migratedIndex <- which(hhDataByYearc$migration == TRUE)
  #create a new subset that contains only migrated households in a given year
  # hhDataByYearcMigrated <- hhDataByYearc[migratedIndex,,]
  # View(hhDataByYearcMigrated)
  
  
  ##############
  
  #via method 2
  newAggData <- merge(aggData,hhDataByYearc, by = c("idmen","year"))
  
    
  # UNCOMMENT FOR METHOD 1 INSTEAD
    # newAggData <- merge(aggData,hhDataByYearb, by = c("idmen","year"))
  
  View(newAggData)
  nrow(newAggData)
  #add column of 0s to newAggData to track job movements
  zeros <- rep(0,nrow(newAggData))
  newAggData <- cbind(newAggData,zeros) #add column of zeros at the end which will then be used for tracking job changes
  length(which(newAggData[,17]==TRUE))
  
  
  #rename last column
    
  names(newAggData)[names(newAggData) == "zeros"] <- "Change"
    
  # get houses that migrated as a subset of newAggData
  #Method 2
  migratedHouseData <- newAggData[ which(newAggData$migration == TRUE), ]
  
  ######### USE THIS FOR METHOD1 ##############
    # migratedHouseData <- newAggData[ which(newAggData$movedAtYearOfSurvey == TRUE), ]
  
  
  nrow(migratedHouseData)
  View(migratedHouseData)
  
  #extract unique people
  uniqueIdind <- unique(migratedHouseData$idind)
  length(uniqueIdind)
  
  #go through list
  for (i in 1:length(uniqueIdind)){
    ID <- uniqueIdind[i]
    #find indices corresponding to them
    index <- which(migratedHouseData[,10] == ID)
    
    #check if empstat or profesison changed    
    for (j in 1:length(index)){
      if (j > 1){
        
        #get index for years
        lastYearIndex <- index[j-1]
        thisYearIndex <- index[j]
        
    
        if(!is.na(migratedHouseData[lastYearIndex,13]) & !is.na(migratedHouseData[thisYearIndex,13])){
          if((migratedHouseData[lastYearIndex,13] != "" & migratedHouseData[thisYearIndex,13] != "") & (migratedHouseData[lastYearIndex,13] != migratedHouseData[thisYearIndex,13])){
            # if((migratedHouseData[lastYearIndex,13] != "" & migratedHouseData[thisYearIndex,13] != "") & (migratedHouseData[lastYearIndex,13] != migratedHouseData[thisYearIndex,13])){
            migratedHouseData[thisYearIndex,ncol(migratedHouseData)] <- 1    
          }
        }
      
        
          # check for change in empstat if profession doesnt change
      
          if (!is.na(migratedHouseData[lastYearIndex,11]) & !is.na(migratedHouseData[thisYearIndex,11])){
          if (migratedHouseData[lastYearIndex,11] != migratedHouseData[thisYearIndex,11]){
            migratedHouseData[thisYearIndex,ncol(migratedHouseData)] <- 1
          }
        }
        
      }
      
    }
  }
  # group and sum up changes (no changes in profession/employment would equal 0)
  jobChangeAndMove <- migratedHouseData %>% group_by(idmen,year) %>% summarise(jobChange = sum(Change) )
  length(which(jobChangeAndMove[,3] > 0))
  
  
  # # names(newAggData)[names(newAggData) == "zeros"] <- "Change In Job/Profession"
  # 
  #   # ncol(newAggData)
  # #extract unique idind values
  # uniqueInd <- unique(newAggData$idind)
  # length(uniqueInd)
  # 
  # 
  # #create an empty list
  # 
  # idAndChangeList <- list()
  # length(idAndChangeList)
  # #goal will be to have each index in the list be [idind years the profession changed] if the idind comes up more than once 
  # 
  # #loop through idind values
  #   for (i in uniqueInd){
  #   
  # 
  #   index <- which(newAggData[,10] == i) # find all occurrences of this individual in aggregated data
  #   
  #   #only look at values of if individual repeated in data
  #   if (length(index)> 1){
  #     
  #     #first entry in this vector will be the idind (if it had more than 1 occurrence)
  #     idAndChange <- c(i)
  #     # years <- newAggData[index,2] # find years
  #     
  #     #loop through the other (later values)
  #     #index[1] is first occurrence - we have nothing to compare it to for changes so it is skipped
  #     for (j in index[2]:index[length(index)]){
  #       
  #       #check if profession/empstat changed (compared to last year)
  #       
  #       #########CHOICE I MADE: ############
  #       
  #       #check that all requisite values are there for this year and previous year (ASSUMPTION I MADE)
  #       if ((!is.na(newAggData[j,11]) && !is.na(newAggData[j-1,11])) && (!is.na(newAggData[j,13]) &!is.na(newAggData[j-1,13]))){
  #         
  #         #if empstat or profession was different this year then last year, we add this year to the vector that will be a list entry 
  #         
  #         if (newAggData[j,11] != newAggData[j-1,11] | (newAggData[j,13] != newAggData[j-1,13]& (newAggData[j,13])!= "" & newAggData[j-1,13] != "" )){
  #           idAndChange <- c(idAndChange,newAggData[j,2]) # add year to vector if empstat or profession changed (from last year)
  #           
  #         }
  #       }
  #     }
  #     #once we have checked all years, the vector we created with idind and years of changes gets added to list
  #   idAndChangeList[[length(idAndChangeList)+1]] <- idAndChange
  #     }
  #   
  # }
  # 
  # View(idAndChangeList) # this contains a list where each entry is the idind and the years they switched jobs/professions (ie when the values were not equal between then and the year before)
  # 
  # length(idAndChangeList)
  # 
  # #loop through list
  # for (i in 1:length(idAndChangeList)){
  #   
  #   #extract idind
  #   id <- idAndChangeList[[i]][1] #extract ID from list
  #   #calculate how many changes there were (lenght of the entry in list - 1 since the first entry was idind)
  #   changes <- length(idAndChangeList[[i]])-1 #see how many years there were changes
  #   
  #   if (changes > 0){
  #     for (j in 1:changes){
  #       #extract each year (one by one)
  #       year <- as.numeric(idAndChangeList[[i]][j+1]) #get each year than status changed
  #       
  #       #find matching years
  #       yearS <- which(newAggData[,2] == year)
  #       #find matching idind
  #       IDs <- which(newAggData[,10] == id)
  #       
  #       #find matching id and year - in theory, there should only be 1 as idind SHOULD BE unique to individual (otherwise data collection error)
  #       index <- intersect(yearS,IDs)
  #       
  #       #update variable value to indicate a change in profession/employment - will stay 0 for individual if no such employment change that year
  #       newAggData[index,18] <- 1 
  #       
  #   }
  #   }
  # }
  # 
  # #changed it again because the title I gave it was too long
  # names(newAggData)[names(newAggData) == "Change In Job/Profession"] <- "Change"
  # View(newAggData)
  # 
  # 
#   condition = if (migration == TRUE & !is.na(migration)){
#     jobChange = 0 ==sum(Change)
#   }
  #pipeline data and group by household and year and summarize by adding all the Change variables (0 if no change and 1 if change in that year) so no changes would have sum 0

  
  # method2
  # newAggDatae <- newAggData %>% group_by(idmen,year) %>% summarise(jobChange = sum(Change),migrated = movedAtYearOfSurvey) 
    
  #method1
#   newAggDatae <- newAggData %>% group_by(idmen,year) %>% summarise(jobChange = sum(Change),migrated = movedAtYearOfSurvey)
#   
#   #counted the instances where there was job changes in a household (sum not 0) and when migration happened
#   countJobChange <- 0
#   nrow(newAggDatae)
#   for (i in 1:nrow(newAggDatae)){
#     #job change and migration
#     if (!is.na(newAggDatae[i,3]) & !is.na(newAggDatae[i,4])){
#       if(newAggDatae[i,3] != 0 & newAggDatae[i,4] == TRUE){
#       countJobChange <- countJobChange + 1
#     }
#     }
#   }
# countJobChange  






##########################################################
# Ex 4
##########################################################

# aggData4 <- aggData %>% group_by()
# length(uniqueIdind)
# dataFor4 <- c(uniqueIdind,rep(0,length(uniqueIdind)*2))
# individualEntryExit <- data.frame(matrix(dataFor4,nrow=length(uniqueIdind),ncol=3))
# colnames(individualEntryExit) <- c("idind","entry","exit")
# nrow(aggData)

minMaxAttrition <- aggData %>% group_by(idind) %>% summarise(entry = min(year),exit = max(year))
View(minMaxAttrition)  

# now we have entry and exit data for each individual

totalInd <- c()
attritionPerYear <- c()
for (i in 2004:2019){
  year <- i
  count <- 0  
  attrition <- 0
  count <- length(which(minMaxAttrition$entry <= i & i <= minMaxAttrition$exit))
  attrition <- length(which(minMaxAttrition$exit == i))
   
  
  totalInd <- c(totalInd,count)     
  attritionPerYear <- c(attritionPerYear,attrition)
}

totalInd
attritionPerYear
propAtrrition <- attritionPerYear/totalInd
years <- seq(2004,2019)
years
propAtrritionTable <- cbind(years,propAtrrition)
View(propAtrritionTable)



