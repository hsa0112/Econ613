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
val <- c() #finds value at each iteration
ind <- c() #checks for correct indices
numom <- c()
denom <- c()
for (a in A){
  B <- 1:a
  B
  for (b in B){
    #calculate numerator and denominator
    num <- exp(sqrt(a))*log(a^5)
    den <- 5 + exp(a*b)*cos(a)*sin(b)
    # add to previous value
    curr <- curr + num/den
    
    #other checking stuff
    ind <- c(ind,c(a,b))
    val <- c(val,curr)
    numom <- c(numom,num)
    denom <- c(denom,den)
  }

}
#print value
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
xVec[which(xVec >= 200)]
#shorter way to find indices where value is TRUE
?which

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
apply(A, MARGIN = 1, FUN= mean)
colMeans(A)
apply(A,2,mean)
#2

#create 2 matrices that include coeff of LHS and coeff of RHS
lhs <- matrix(c(2,1,3,1,1,1,1,3,2),nrow=3,ncol=3,byrow=T)
rhs <- matrix(c(10,6,13),nrow=3,ncol=1,byrow=F)
rhs
lhs

united <- cbind(lhs,rhs)
united
idkunited <- rbind(lhs,c(0,0,0))
idkunited

#solve simply solves them - can also keep as one matrix and separate in solve
# solve by itself just does inverse when possible
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
?sample
v1 <- sample(c(1:20),36, replace = T)
v1
# create subvectors that omit first value - can define any condition

condition = v1 > 19

v1[2:length(v1)]
v1[-1]

#all should give you the same thing (subset that is above 19 - so only 20's show up)
v1[condition]
v1[which(v1>19)]
above19 <- which(v1>19)
v1[above19]

#
v2 <- v1>5 # tells you true false indices
condition = v1>5
v2 <- v1[condition] # gives you subset of v1 that follows condition
v2
as.numeric(v2) # change T/F to 1's and 0's
sum(v2) # total number that hold this condition
v2
length(v2[condition])


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
x = c(rnorm(10),NA,paste("d",1:16,sep=" "),NA,log(rnorm(10)))
x

#define a condition (or opposite) taht we want
#Goal is to filter out NA's and infinite values 
is.na(x)
is.infinite(x)
#values that aren't NA
cond = is.na(x) + is.infinite(x) #determine the NAs and infintie values
#apply opposite condition to x
#same ting
x[!condition]
x[which(!condition)]

as.numeric(!is.na(x))
as.numeric(!is.infinite(x))

##############################
#Ex 8
##############################

#load data
?library
library(AER)
?data
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


# requires for plotting ***************
library(dplyr)
library(ggplot2)


ggplot(dat %>% group_by(year) %>% summarise(mean_income = mean(income)) , aes(x=year,y=mean_income)) + geom_line() +xlab("Year (AC)") + ylab("Average Income")
# data.frame name -> grouping you want -> summarize variable (define var and define it by group), axis: what you are plotting + line + info)
?ggplot

#Each is a data frame (grade, school, memployment) with created variable (mean_...)
#group by characteristic and find mean for each characteristic in general dataframe
# new dataframe name -> group by -> summarize by what
gender = dat %>% group_by(gender) %>% summarise(mean_income=mean(income))
genderSum = dat %>% group_by(gender) %>% summarise(sum_income=sum(income)) # summarize by sums
school = dat %>% group_by(school) %>% summarise(mean_income=mean(income))
schoolSD = dat %>% group_by(school) %>% summarise(sd_income=sd(income))
memployment = dat %>% group_by(memployment) %>% summarise(mean_income=mean(income))

incomes = c('male-female'=gender$mean_income[[1]]-gender$mean_income[[2]], 'gymnasium-hauptschule'=school$mean_income[[3]]-school$mean_income[[1]],'gymnasium-realschule'=school$mean_income[[3]]-school$mean_income[[2]], 'realschule-hauptschule'=school$mean_income[[2]]-school$mean_income[[1]],'none-fulltime'=memployment$mean_income[[3]]-memployment$mean_income[[1]], 'none-parttime'= memployment$mean_income[[3]]-memployment$mean_income[[2]],'fulltime-parttime'=memployment$mean_income[[1]]-memployment$mean_income[[2]])
#Creating vector that has name (differences) = actual values in each dataframe (we created dataframes above and defined these variables)
incomes

# save.image(("upto8.RData"))


######################################
#Ex 9
######################################

#1 load data
data("CASchools", package = "AER")
dat1=CASchools

#2 regress read on a bunch of variables and store it as reg1
?lm
# rownames(dat1)
# colnames(dat1)

# regress on the var asked for
reg1 <- lm(formula = read ~ district + school + county + grades + students +  teachers  +  calworks +  lunch + lunch + computer + expenditure + income + english , data = dat1 )
#regress on everyrthing BUT not asked for - should be the same
reg1Alt <- lm(read ~ . - math, dat1)
summary(reg1)
summary(reg1Alt)

#his question messes around with variable school and uses factor - unclear why##
data$school= factor(data$school)
data$district= factor(data$district)
reg1 = lm(read ~ .-school, dat1)
summary(reg1)

#3 - new regression specification and only use first 200 observations
formula <-  y ~x.lm(formula)
subsetDat1 = dat1[1:200,]

#run new regression
reg2 <- lm(read~. - math, subsetDat1)
summary(reg2)




######################################
#10
######################################

#1 
# take 200 samples from a pareto distribution
# install.packages("actuar")

library(actuar)
lu = rpareto(200, 1, 1)  
lu#how many samples are over 10

over10 <- which(lu >= 10)
View(over10)
length(over10)
#replace draws from a logistic distribution

for (i in over10){
  lu[i] <- rlogis(1,6.5,.5)
  
}


#2
install.packages("truncnorm")
require(truncnorm)
de <- rnorm(200,1,2)
de <- log(de)
#finding indices for NaN and negative and how many of each
View(which(is.nan(de)))
View(which(de<0))
length(which(is.nan(de)))
length(which(de<0))
# ?rtruncnorm
# ??rtruncnorm

#replace these values with random from normal that are non-negative (lower bound is 0)
de[which(is.nan(de))] = rtruncnorm(length(which(is.nan(de))))
de[which(de<0)] = rtruncnorm(length(which(de<0)), a=0)

#3 - create 2 vectors that random sample 200 from uniform 0 to 1
orig <- runif(200,0,1)
dest <- runif(200,0,1)

#4 - create 2 matrices that are 200*200 draws from uniform again

hist <- matrix(runif(200*200,0,1),nrow=200,ncol=200,byrow = TRUE)
dist <- matrix(runif(200*200,0,1),nrow=200,ncol=200,byrow = TRUE)


#5 - Create two matrices for a given function

J <- length(orig)
J
L <- length(dest)
L
#going to make for loops to get every value
su_vector <- c()

for (j in (1:J)){
  
  for (l in (1:L)){
    
    numerator <- log(orig[j] + dest[l]+dist[j,l])
    denominator <- 1 + log(orig[j] + dest[l] + dist[j,l])
    term <- numerator/denominator
    su_vector <- c(su_vector,term)
  }
  
}
su <- matrix(su_vector,nrow=200,ncol=200,byrow = TRUE)

se_vector <- c()
for (j in (1:J)){
  
  for (l in (1:L)){
    numerator <- exp(orig[j] + dest[l]+hist[j,l])
    denominator <- (1+exp(orig[j]+dest[l]+hist[j,l]))
    term <- numerator/denominator
    se_vector <- c(se_vector,term)
  }
  
}
se <- matrix(se_vector,ncol=200,nrow=200,byrow=TRUE)

# # how prof did it
# int = outer(orig, dest, "+")+dist
# su1 = log(int)/(1+log(int))
# int = outer(orig, dest, "+")+hist
# se1 = exp(int)/(1+exp(int))

# #7
# 
# #how other student got it
# getQQ = function(w) {
# frac = outer(r+de, r+de, "/")
# one = frac * w
# 
# two = lu * log(w)
# three = lu * (1+log(w))
# mid = outer(two,three,"-")
# 
# su_four = frac*(rowSums(su)-diag(su))
# su_five = outer(rep(1,200),rowSums(su)-diag(su),"*")
# su_part = su_four - su_five
# se_four = frac*(rowSums(se)-diag(se))
# se_five = outer(rep(1,200),rowSums(se)-diag(se),"*")
# se_part = se_four - se_five
# return(one+mid+su_part+se_part)
# }
# #
# getQQ(9245)
r <- 0.05
getQ =function(w,j,l){
  
  first <- w*(r+de[j])/(r+de[l])
  second <- lu[j]*log(w)
  #subtract this
  third <- lu[l]*(1+log(w))
  
  fourthSum <- 0
  for (k in (1:J)){
    if (k != j){
      fourthSum <- fourthSum + su[j,k]
    }
  }
  fourth <- fourthSum*(r+de[j])/(r+de[l])
  
  #subtract this
  fifthSum <- 0
  for (k in (1:L)){
    if (k != l){
      fifthSum <- fifthSum + su[l,k]
    }
  }
  fifth <- fifthSum
  
  sixthSum <- 0
  for (k in (1:J)){
    if (k != j){
      sixthSum <- sixthSum + se[j,k]
    }
  }
  sixth <- sixthSum*(r+de[j])/(r+de[l])
  
  #subtract this
  seventhSum <- 0
  for (k in (1:L)){
    if (k != l){
      seventhSum <- seventhSum + se[l,k]
    }
  }
  seventh <- seventhSum
  total = first + second - third + fourth - fifth + sixth - seventh
  return(total)
}


getQ(9245,7,1)
#create matrix for all values
getQMatrix = function(w){
  emptyMatrix<- matrix(,ncol=200,nrow=200,byrow = TRUE)
  for (j in (1:J)){
    for (l in (1:L)){
      emptyMatrix[j,l] <- getQ(9245,j,l)
  }
  
}
  return(emptyMatrix)
}
getQMatrix(9245)


#prof method - not sure how he excluded indice stuff
r = 0.05
getQ1 = function(w) {
  frac = outer(r+de, r+de, "/") 
  one = frac * w
  two = lu * log(w)
  three = lu * (1+log(w))
  mid = outer(two,three,"-")
  four = frac * sum(su) - sum(su)
  five = frac * sum(se) - sum(se)
  return(frac * w + mid + four + five)
}
getQ1(9245)

# which(getQ1(9245) == getQ(9245,2,2))

#8 - create a grid
install.packages("pracma")
library(pracma)
gridw = seq(9100,55240,50)
View(gridw)

#9 - evaluate stuff and store output in an arrary in a systematic way
?sapply

#see how long it take - TAKES REALLY LONG WITH MY CODE BUT QUICK WITH PROF CODE
system.time(sapply(gridw, FUN=getQMatrix, simplify = F))

results <- sapply(gridw, FUN=getQMatrix, simplify = F)



#############################################
#11
#############################################

#1
# test if the vector is...
testing <- c(1,2,3)
is.array(testing)
is.vector(testing)# should be TRUE here
is.matrix(testing)

#2 - create tables to see occurrences
x0 <- rnorm(1000)
x0
#tabulate occurrences of certain condition of x0
table(x0>0)
table(x0>1)
table(x0>2)
table(x0>.5)
table(x0<1)
table(x0>-1)

#3
library(Hmisc)
?cut2
??cut2
?levels
#create levels - 10 intervals
#make x1 a vector of 100 draws from uniform distributions and break into 10 intervals
# now, it will show you which interval it is in (can rename intervals to be more clear)
x1 <- cut2(runif(100,0,1),g=10)
x1

x1Test <- runif(100,0,1)
x1Test
x1Test <- cut2(x1Test,g=10)
x1Test

#label levels/intervals so they aren't just ranges (more generally, they are categories)
levels(x1) <- paste("q",1:10,sep="")
x1
?is.factor()
?as.factor()

#4 - check if x1 is a factor
is.factor(x1)
class(x1)
#see levels/intervals - categories
levels(x1)

#5 - check q1 has 10 occurrences
table(x1=="q1")[[2]] == 10
table(x1=="q1")
length(which(x1=="q1"))

#6 - convert levels to integers
as.numeric(x1)
x1
typeof(x1)     
x1

#7
rand <- rnorm(1000)
rand

#8 - use which to find index of pos values
#which gives you index of positive values
which(rand >0)

#9 create w that consists of positive values of rand in 3 different ways - which(), subset(), and directly
randWhich <- rand[which(rand>0)]
randWhich
?subset
randSubset <- subset(rand,rand>0)
randSubset
randDirect <- rand[rand>0] #just define a condition and it will tell you which ones are valid for it (are TRUE)
randDirect


#########################
#12
########################
N <- readline(prompt = "Enter an Integer: ")
N <- as.integer(N)
N
u = function(N) {
  if (N==0|N==1) {return(1)}
  return(u(N-1)+u(N-2))
}
u(2)
u(3)
u(4)
u(N)

# evaluate 1^2 +...+400^2

totes <- 0
for (i in 1:400){
  totes <- totes + i^2
}
totes
sum(c(1:400)^2)
c(1:400)*c(1:400)
sum(c(1:400)*c(1:400))

#2 evaluate 1x2 + 2x3 + ... 249x250
# sum(outer(1:249,2:250,"*"))
sum(c(1:249) * c(2:250))

#3
#based on CRRA utility - should be log(c) if theta close to 1 (otherwise explodes)
crra = function(c,t){
  output <- (c^(1-t))/(1-t)
  
  if (t >= 0.97 & t <= 1.03){
    utility <- log(c)
    # output <- c(output,utility)
    return(utility)
  }
  return(output)
  
}

crra(15,1.01)

# #his code - seems similar, when theta is close to 1, ln(0) so NaN...
# crra = function(c,theta) {
#   op = c^(1-theta)/(1-theta)
#   if (0.97 <= theta & theta <=1.03) {return(log(op))}
#   return(op)
# }


#4 - factorial
fact = function(n){
  val <- 1
  if (n == 0){
    return(val)
  }
  else if (n >= 1){
    
    for (i in (1:n)){
      val <- val*i
      
    }
    return(val)
  }
 print(paste("No Factorial for",n)) 
}

fact(1)
fact(-3)
fact(5)

#just take product of vector values
factTest =function(n){
  result <- prod(1:n)
  return(result)
}
factTest(5)



##########################
#13
##########################

# creates 40 random draws - 20 from one distribution and 20 from another
m <- matrix(c(rnorm(20,0,10),rnorm(20,-1,10)),nrow=20,ncol=2)
#by row stuff
#apply can help you do functions by a margin (usually 1 or 2 for row and column)
apply(m,MARGIN = 1,mean)
apply(m,MARGIN = 1,median)
apply(m,MARGIN = 1,min)
apply(m,MARGIN = 1,max)
apply(m,MARGIN = 1,sd)

#by column stuff
apply(m,MARGIN = 2,mean)
apply(m,MARGIN = 2,median)
apply(m,MARGIN = 2,min)
apply(m,MARGIN = 2,max)
apply(m,MARGIN = 2,sd)

#2
#load dataset iris
#method 1
library(datasets)
data(iris)
#method 2
data("iris", package = "datasets")
library(dplyr)
library(ggplot2)
dimnames(iris)
#
#data.frame -> group -> thing you want to do (summarize(newVarName = function(variable(s)))) 
#similar stuff for plotting
SpeciesLength <- iris %>% group_by(Species) %>% summarise(mean_Sepal_Length = mean(Sepal.Length))
SpeciesLength
# ggplot(iris %>% group_by(Species) %>% summarise(mean_Sepal_Length = mean(Sepal.Length)),geom_line() +xlab("Year (AC)") + ylab("Average Income")

       
SpeciesWidth <- iris %>% group_by(Species) %>% summarise(sum_Sepal_Width = sum(Sepal.Width))
SpeciesWidth
sum(SpeciesWidth$sum_Sepal_Width)

#3
y1 <- NULL
for (i in 1:100){
  y1[i] <- exp(i)
}
y1
y2 <- exp(1:100)
y2
y3 <- sapply(1:100,exp)
y3


y1 <- NULL;
system.time(for (i in 1:100){
  y1[i] <- exp(i)
})
y1
system.time(y2 <- exp(1:100))
y2
system.time(y3 <- sapply(1:100,exp))
y3


##########################
#Ex 14
##########################
#1 
x <- rnorm(10000)
summary(x)
quantile(x,.1)
#2
min(x)
min
summary(x)[[4]]
#break into intervals between certain probabilities
quantile(x,seq(.1,.9,by=.05))

#summary gives you min, q1, med, mean, q3, max (1 through 6)
dsummary = function(x){
  min <- summary(x)[[1]]
  dec1 <- quantile(x,.1)[[1]]
  q1 <- quantile(x,.25)[[1]]
  med <- summary(x)[[3]]
  sd <- sd(x)
  q3 <- summary(x)[[5]]
  dec9 <- quantile(x,.9)[[1]]
  max <- summary(x)[[6]]
  return(c(min,dec1,q1,med,sd,13,dec9,max))
  
}
dsummary(x)


#3
#d pdf; p cdf ; q inverse
dnorm(.5,2,.25)
# dnorm(0.5, mean=2,sd=0.25)
pnorm(2.5,2,.25)
qnorm(.95,2,.25) # gets you value associated with .95 CDF
pnorm(qnorm(.95,2,.25),2,.25)  # gives you CDF of given value (that should be value associated with .95 CDF)

#4
dt(.5,5)
pt(2.5,5)
qt(.95,2)

#5

install.packages("Pareto")
library(actuar)
dpareto(.5,3,1)
ppareto(2.5,3,1)
qpareto(.95,3,1)

library(Pareto)
dPareto(.5,3,1)
pPareto(2.5,3,1)
qPareto(.95,3,1)




#######################
#15
#######################

V <- rnorm(100,-2,5)
#1
n <- length(V)
m <- sum(V)/n
m
# mean(V)
variance <- sum((V-m)^2)/(n-1)
variance
var(V)
skewn <- sum((V-m)^3)/(n*variance^3)
skewn
install.packages("moments")
require(moments)
skewness(V) # gives different value (divide by n-1)
kurt <- sum((V-m)^4)/(n*variance^4) - 3
kurt
kurtosis(V) # gives different value (unsure why)



#######################
#16
#######################

## MATRIX MULTIPLICATION

#1
?rbeta

X <- matrix(rbeta(1000*10,2,1),nrow=1000,ncol=10,byrow = TRUE)
X
length(which(X<0))

#2
sigmaSquared <- .5
beta <- rgamma(10,2,1)


#3
e <- rnorm(1000)
e

#4
#matrix multiply data by beta and then add variations so data not perfect

Y<- X%*%beta +sqrt(sigmaSquared)* e
Y

#5
#solve does inverse of matrix
betaHat <- solve(t(X)%*%X)%*%(t(X)%*%Y)
betaHat

#6
#find estimated error (from OLS and actual)
YHat <- X%*%betaHat
YHat
eHat <- yhat-Y
eHat
hist(eHat, col="gray")
# par(new=TRUE) - labeling issue
plot(density(eHat))

#7
n = 1000
k <- p <- 10
estSigmaHatSquared <- (t(eHat)%*%eHat)/(n-(k+1))
estSigmaHatSquared
# s = t(eHat) %*% eHat / (1000-10-1)
# s

v <- estSigmaHatSquared[1,1]* solve(t(X)%*%X)
v

#8
param <- cbind(beta,sqrt(v))
param

?lm
lm(Y~0+X)
# close but not exact

fit0 = lm(Y~0+X)
fit0
summary(fit0)

#9
confint(fit0)

########## redo with new sigma

#1
?rbeta

X <- matrix(rbeta(1000*10,2,1),nrow=1000,ncol=10,byrow = TRUE)
X
which(X<0)

#2
sigmaSquared <- .01
beta <- rgamma(10,2,1)


#3
e <- rnorm(1000)
e

#4
Y<- X%*%beta +sqrt(sigmaSquared)* e
Y

#5
betaHat <- solve(t(X)%*%X)%*%(t(X)%*%Y)
betaHat

#6
YHat <- X%*%betaHat
YHat
eHat <- yhat-Y
eHat
hist(eHat, col="gray")
# par(new=TRUE) - labeling issue
plot(density(eHat))

#7
estSigmaHatSquared <- (t(eHat)%*%eHat)/(1000-10-1)
estSigmaHatSquared
# s = t(eHat) %*% eHat / (1000-10-1)
# s

v <- estSigmaHatSquared[1,1]* solve(t(X)%*%X)
v

#8
param <- cbind(beta,sqrt(v))
param

?lm
lm(Y~0+X)

fit0 = lm(Y~0+X)
fit0
summary(fit0)

#9 constructing CI
confint(fit0,level = .95)

?confint


#9
# a lot tighter now as variation is much smaller
