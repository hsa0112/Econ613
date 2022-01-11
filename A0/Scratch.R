#Scratch
#1 load data
data("CASchools", package = "AER")
data=CASchools

?factor
#what does this do?
data$school= factor(data$school)
data$district= factor(data$district)

reg1Prof = lm(read ~ .-read - math, data)
summary(reg1Prof)
