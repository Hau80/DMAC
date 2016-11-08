rm(list=ls(all=TRUE))
mydata = read.table("IRIS.txt",header = FALSE, sep = ",", quote = "\"'")
I5 <- gsub("Iris-setosa",1, mydata[5])
nchar(I5)
substr(I5,1,450)
I5

for(j in 2:Y )
{
  maxFindex =  which(newdata[1:1000, j] == max(newdata[1:1000, j]))
  print(newdata[maxFindex,1])
  temp = rbind(temp, newdata[maxFindex,1])
  
}





I1= nrow(mydata[1])
I2= cbind(mydata[2])
I3= list(mydata[3])
I4= c(mydata[4])
I5 <- gsub("Iris-setosa",1, mydata[5])
I5 <- unlist(I5)
I1
I2
I3
I4
I5
temp = vector()

temp = rbind(I1, I5)
temp

finalData = data.frame()
data1 <- gsub("Iris-setosa",1, mydata[5])
data1
temp = vector()

temp = rbind(temp, data1)
if( i >= 2)
{
  finalData = cbind( finalData, data.frame(temp) )
}
else
{
  finalData = rbind( finalData, data.frame(temp) )
}

finalData

data2 = nrow(data1)
data2




temp = vector()

for(j in 2:Y )
{
  maxFindex =  which(newdata[1:1000, j] == max(newdata[1:1000, j]))
  print(newdata[maxFindex,1])
  temp = rbind(temp, newdata[maxFindex,1])
  
}

if( i >= 2)
{
  finalData = cbind( finalData, data.frame(temp) )
}
else
{
  finalData = rbind( finalData, data.frame(temp) )
}
} 




temp3 = rbind(temp3, data1)
temp3

data <- list(mydata[5])


  data1 <- gsub("Iris-setosa",1, data[i])


data1 <- gsub("setosa",1, data)

data2 <- split(data1,5)
data2

data2 <- cbind(v1,v2)
data2

data1 <- list(sub("Iris-setosa",1, data))




data <- cbind(gsub("Iris-setosa",1, mydata[5]))

data <- list(sub("Iris-setosa",1, mydata))

data

data <- data.frame(sub("Iris-setosa",1, mydata[5]))


data


sub()



Data <- data.frame(mydata)

Data[mydata == "Iris-setosa",] = 1

Data[Data[4] == "Iris-setosa"] <- 1


sapply(Data[4],switch,'Iris-setosa'=1,'Iris-versicolor'=2)


Data$Response[Data$Response == "Iris-setosa"] <- 1



Data <- data.frame(mydata) 

sapply(Data$Response,switch,'Iris-setosa'=1,'Iris-versicolor'=2)
sapply



Data$Response[Data$Response == "Iris-setosa"] <- 1

Data[Data$Response == "Iris-setosa",]$Response = 1


Data$words <- ifelse(Data$words == "Iris-setosa", 1, ifelse(Data$words == "Iris-versicolor", 2, ""))


cath <- data.frame(nmbrs = runif(10), words = sample(c("Iris-setos", "Iris-versicolor"), 10, replace = TRUE))

mydata$Response[mydata$Response == "Iris-setosa"] <- 1


d[d$Response == "Good",]$Response = 1


Iris-setosa
Iris-versicolor
Iris-virginica



d[d$Response == "Good",]$Response = 1
d[d$Response == "Bad",]$Response = -1
d[d$Response == "undefined",]$Response = ""

I1 <- c(0,0,1,1)
I2 <- c(0,1,0,1)
Oand <- c(0,0,0,1)
Oor <- c(0,1,1,1)
Delta1 <- 0
DW1 <- 0
DW2 <- 0
DTheta <- 0
DY <- 0
W1 <- 0.15
W2 <- 0.1

n = 10

Theta1 <- 0.35

for (a in 1:1000)
{
  
  for(i in 1:4)
  {
    X <- W1 * I1[i] + W2 * I2[i] - Theta1
    
    Y = 1 / (1.0 + exp(-X))
    
    Delta1[i] = Y * (1-Y) * (Oor[i] - Y)
    
    DW1[i] <-  n * Delta1[i] * I1[i]
    DW2[i] <-  n * Delta1[i] * I2[i]
    DTheta[i] <- (-1) * n * Delta1[i]
    DY[i] <- Y
  }
  W1 <- W1 + mean(DW1)
  W2 <- W2 + mean(DW2)
  Theta1 <- Theta1 + mean(DTheta)
  
}