# Hamzah Ahmed - A0
#Econ 613 - S22

rm(list = ls())

#############################################
# Ex 1
#############################################


#1 - Done Manually

#2
# create a vector with names of packages
packages <- c("Hmisc","gdata","boot","xtable","MASS","moments","snow","mvtnorm")
#install the packages
install.packages(packages)

#3
getwd() # find location and put there
# setwd(E:/Duke/Semester 2/Applied Micro/Assignments/A0) Shouldn't have spaces

#4
#content of directory
dir()
#content of environment
ls()

#5
#modular arithmetic to check divisibility - if divisible, it would ouput TRUE 
678%%9 == TRUE 
#Returned FALSE so not a multiple

#6
#save environment in current directory
save.image("misc1.RDATA")

#7
#use ?_ to look up documentation/help on _
?mean
?cut2
??cut2 #searches online

#8
#NaN: Not a Number
16%%0
log(-1)




######################################
# Ex 2
######################################

# mat.or.vec(nr,nc) creates a nr x nc zero matrix!!
#dim to find dimensions (row and column)
#length gives you total amount of shit

#this data set yields array that categorize all types of people on this boat (no repeats)
dimnames(Titanic)
Titanic

#1
#a - find total number of people regardless of any characteristic (Class, Sex, Age, Survived)
sum(Titanic)
#b - find total adults (characterized age)
sum(Titanic[,,"Adult",])
# sum(Titanic[,,"Child",])
#c - find total crew (characterized calss)
sum(Titanic["Crew",,,])
#d - total 3rd class children)(class and age)
sum(Titanic["3rd",,"Child",])
#e - total 2nd class adult female (class, age and sex)
sum(Titanic["2nd","Female","Adult",])
#f - total 1st class male children (class, age, and sex) 
sum(Titanic["1st","Male","Child",])
#g - Female Crew Surivivor (status, sex and survived)
sum(Titanic["Crew","Female",,"Yes"])
#h - 1st class adult male survivor
sum(Titanic["1st","Male",,"Yes"])

#2
?prop.table 
#finding table that gives proportions based on given characteristics
prop.table(Titanic)# General
#Specific
#a-d (nothing very different among them)
prop.table(Titanic["1st","Male","Adult",],)
prop.table(Titanic["1st","Female","Adult",])
prop.table(Titanic["1st","Male","Child",])
prop.table(Titanic["3rd","Female","Adult",])



######################################
#3
######################################

#1

#a in 3 ways
# as a vector (most direct)
c(1:50)
#as a sequence with fixed steps
seq(1,50,by=1)
#reverse a vector
rev(50:1)

#b in 3 ways
#as a vector
b <- c(50:1)
#as a sequence
seq(50,1, by = -1)
#backwards
rev(1:50)

#2
?rep

#create a vector and then create a new vector that repeats the original a fixed number of times
#a-b

a <- c(10,19,7)
a
repA = rep(a,15)
repA

b  <- c(1,2,5,6)
b
repB = rep(b,8)
repB

#3
#creating a vector that stores results of polynomials for specific x-values
x <- seq(3.1,3.6,by = .1)
x
y <- log(x)*(sin(x))
y


#4
?sample
#draw a sample from a given range, n times, with no replacement
sample1 = sample(0:100,90,FALSE)
sample1
mean(sample1)
#same but with replacement
sample2 = sample(0:100,90,TRUE)
sample2
mean(sample2)

#5
?sum #can't really do directly but can do via transposition of vectors

#a
#no for loop
# %*% does matrix multiplication (inner product)
a = 1:20
b = t(c(1:15))
sum(exp(sqrt(a))*log(a^5)/(5+cos(a)%*%sin(b)))

#with for loop - my way
a = 1:20
b = c(1:15)
initialVal = 0
for (i in a){
  for (j in b){
    initialVal <-  initialVal + (exp(sqrt(i))*log(i^5))/(5+cos(i)*sin(j))
  }
}
initialVal

#b

#How Prof Did It - I think it is WRONG !!!
a    = 1:20
curr = 0
for (i in a) {
  b = t(c(1:i))
  curr = curr + sum(exp(sqrt(a))*log(a^5)/(5+exp(a%*%b)*cos(a)%*%sin(b)))
}
curr

#My way
A = 1:20
curr = 0
val <- c()
ind <- c()
numom <- c()
denom <- c()
for (i in A){
  B <- 1:i
  B
  for (j in B){
    num <- exp(sqrt(i))*log(i^5)
    den <- 5 + exp(i*j)*cos(i)*sin(j)
    
    curr <- curr + num/den
    ind <- c(ind,c(i,j))
    val <- c(val,curr)
    numom <- c(numom,num)
    denom <- c(denom,den)
  }
}
curr


#6
x = seq(3.1,3.6, by = .1)
exp(x)%*%cos(x)
exp(x)*cos(x)
sum(exp(x)*cos(x))# should be same as inner product




#####################################
#Ex 4
#####################################



#1
#creating range of values to sample through
rng = c(0:999)
rng
#creating 2 vectors by randomly sampling through it
xVec = sample(rng,1000, replace = T)
yVec = sample(rng,1000, replace = T)
#2
#a
#creating empty data fram and then filling in as we go
zVec <- data.frame(matrix(ncol  = 999, nrow = 1))
for (i in 1:999){
  zVec[i] = yVec[i+1] - xVec[i]
}

#b
#creating empty vector then wil append to it
wVec = c()
for (i in 1:999){
  val = 0
  val = sin(yVec[i])/cos(xVec[i+1])
  wVec <- c(wVec,val)
}

#c
subX <- c()

#via for lopp to check which values work
for (i in 1:999){
  if (xVec[i]>= 200){
    subX <- c(subX,xVec[i])
  }
}
#can just write the condition directly in
subX = xVec[xVec >= 200]
subX

# can create/define a condition and then use it in [] of a vector of values and such (includes if Condition Holds (ie TRUE is output))
condition = xVec >= 200
subX = xVec[condition]
subX

#d
index <- c()

pos = 1
for (i in 1:999){
  if  (yVec[i] >= 600){
    pos = i
    index = c(index,pos)
  }
}
index

#shorter way to find indices where value is TRUE
?which
which(yVec >= 600)

###################################
#Ex 5
###################################


?matrix
#create a matrix and fill in by row (naturally does by column I think)
A <- matrix(c(1,1,3,5,2,6,-2,-1,-3),ncol=3,nrow=3,byrow = T)
# A = cbind(c(1,5,-2),c(1,2,-1),c(3,6,-3)) - can also be done by column binding the vectors as each vector is read as a column in r and not a row
A
# check if cubed is 0 vector

A^3
A^3 == 0
A^3==mat.or.vec(3,3)# zero vector that is 3x3
#sum firsta and third column
forthCol = A[,1]+ A[,3]
forthCol
#bind it column wise at the end to get fourth column
A <- cbind(A,forthCol)
A
A <- cbind(A,A[,1]+A[,3])
A

#change third row to sum of 1st and 2nd row
A[3,] <- A[1,]+A[2,]
A


#method to calculate averages by row and by column
rowMeans(A)
colMeans(A)


#2

#create 2 matrices that include coeff of LHS and coeff of RHS
lhs <- matrix(c(2,1,3,1,1,1,1,3,2),nrow=3,ncol=3,byrow=T)
rhs <- matrix(c(10,6,13),nrow=3,ncol=1,byrow=F)
rhs
lhs

united <- cbind(lhs,rhs)
united

#solve simply solves them - can also keep as one matrix and separate in solve
solve(united[,1:3],united[,4])
solve(lhs,rhs)

###########################
#Ex 6
###########################


#1
#define a function with specificed inputs - return(____) is what you will see
fun1 <- function(a,n){
  sumting=0
  for (i in 1:n){
    sumting = sumting + a^i/i
  }
  return(sumting)
}
fun1(3,2)

#2
# can create piecewise functions (just be careful of if statements and logical validity)
fun2 = function(x){
  result = 0
  if (x<0){
    result = x^2 + 2*x + abs(x)
  } 
  else if (x <2){
    result = x^2 + 3 +log(1+x)
  }
  else {
    result = x^2 + 4*x - 14
  }
  return(result)
}

fun2(-3)
fun2(0)
fun2(3)

###############################
# Ex 7
###############################

#create random vector
v1 <- sample(c(1:20),36, replace = T)
v1
# create subvectors that omit first value - can define any condition

condition = v1 > 19

v1[2:length(v1)]
v1[-1]
v1[condition]

#
v2 <- v1>5
condition = v1>5
v2 <- v1[condition]
v2

# CANT SEEM TO GET THIS TO WORK - ANOTHER EASIER WAY EXISTS
# for (i in 1:length(v1)){
#   if (isTrue(v1[i] == TRUE)){
#     v1[i] = 1
#   }
#   else {
#     v1[i] = 0
#   }
# }

# converts T/F to 1/0
?as.integer
as.integer(v2)


# create a 6x6 matrix and each row is v1
m1 <- matrix(v1,nrow=6,ncol=6,byrow=TRUE)
m1

# create a vector that combines the following stuff
x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))
x

#define a condition (or opposite) taht we want
#Goal is to filter out NA's and infinite values 
cond = is.na(x) + is.infinite(x) #determine the NAs and infintie values
#apply opposite condition to x
x[!condition]



##############################
#Ex 8
##############################

#load data
library(AER)
data("GSOEP9402", package = "AER")
#name data set
dat = GSOEP9402 # not necessary but simplifies it instead of calling it by true name


?type
typeof(dat)
class(dat)
ncol(dat)
nrow(dat)

dimnames(dat)
colnames(dat) # name of columns/variables
rownames(dat) # name of each row (basically number)


# requires for plotting
require(dplyr)
require(ggplot2)

ggplot(dat %>% group_by(year) %>% summarize(mean_income = mean(income)) , aes(x=year,y=mean_income)) + geom_line() +xlab("Year (AC)") + ylab("Average Income")
# data.frame name -> grouping you want -> summarize variable (define var and define it by group), axis: what you are plotting + line + info)


#Each is a data frame (grade, school, memployment) with created variable (mean_...)
#group by characteristic and find mean for each characteristic in general dataframe
# new dataframe name -> group by -> summarize by what
gender = dat %>% group_by(gender) %>% summarize(mean_income=mean(income))
genderSum = dat %>% group_by(gender) %>% summarize(sum_income=sum(income)) # summarize by sums
school = dat %>% group_by(school) %>% summarize(mean_income=mean(income))
schoolSD = dat %>% group_by(school) %>% summarize(sd_income=sd(income))
memployment = dat %>% group_by(memployment) %>% summarize(mean_income=mean(income))

incomes = c('male-female'=gender$mean_income[[1]]-gender$mean_income[[2]], 'gymnasium-hauptschule'=school$mean_income[[3]]-school$mean_income[[1]],'gymnasium-realschule'=school$mean_income[[3]]-school$mean_income[[2]], 'realschule-hauptschule'=school$mean_income[[2]]-school$mean_income[[1]],'none-fulltime'=memployment$mean_income[[3]]-memployment$mean_income[[1]], 'none-parttime'= memployment$mean_income[[3]]-memployment$mean_income[[2]],'fulltime-parttime'=memployment$mean_income[[1]]-memployment$mean_income[[2]])
#Creating vector that has name (differences) = actual values in each dataframe (we created dataframes above and defined these variables)
incomes

# save.image(("upto8.RData"))


######################################
#Ex 9
######################################


data("CASchools", package = "AER")
data=CASchools

?lm
rownames(data)
colnames(data)
