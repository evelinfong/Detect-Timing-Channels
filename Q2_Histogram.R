# Set the working directory
setwd("Set your directory")

# Read CSV into R
# Make all 501 rows into mydata
options(max.print = 1000000)
mydata <- read.csv(file = "Traffic_data_orig.csv", header = TRUE, sep = ",",
                   quote = "\"", dec = ".", fill = TRUE, comment.char = "")

#Find difference of time intervals
trials = 501 #there are 501 data points
diff = numeric(trials)
diff_round = numeric(trials)

layout(1:2)

#OVERT

for(i in 1:trials)
{
  #Get the difference of 2 consecutive nubmers
  diff[i] = mydata$Time[i+1] - mydata$Time[i] 
  diff_round[i] = round(mydata$Time[i+1] - mydata$Time[i], digits = 3)
}

hist(diff_round, main = "Overt Packet Stream",xlab = "Time", ylab = "Packet Number")







# Set the working directory
setwd("C:\\Users\\Mariana Fan\\Desktop\\ECS 132\\Project\\Work")
library("BMS")

options(max.print = 1000000)
mydata <- read.csv(file = "Traffic_data_orig.csv", header = TRUE, sep = ",",
                   quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Make all 501 rows into mydata
options(max.print = 1000000)

#COVERT

#packet num vector
pacNum = c(1:128)

#total length in binary for the secret message
len = 128

#time vector
time = numeric(len)

#secret message into hex
secMess = c("74", "68", "69", "73", "2D", "69", "73", "2D", "61", "2D", "73", "65", "63", "72", "65", "74")
secMessb = numeric(16*8) #array for binary secret message

count = 0

for(i in 1:16)
{
  test = hex2bin(secMess[i])
  
  for(j in 1:8)
  {
    count = count + 1
    secMessb[count] = test[j]
  } #there are 8 characters for 2 hex digits
  
} #converting the characters from hex to bin


Time = numeric(128)
pacDelays = numeric(128)

timeCount = 0
pacDelays[1] = 0

for(i in 1:len)
  myData$Time[i] = i

for(i in 2:len)
{
  
  if(isTRUE(secMessb[i-1] == 1))
  {
    timeCount = timeCount + 0.75
    pacDelays[i] = timeCount
  }
  
  
  if(isTRUE(secMessb[i-1] == 0))
  {
    timeCount = timeCount + 0.25
    pacDelays[i] = timeCount
  }
  
} #encoding the secret message into time 

diff1 = numeric(128)
diff_round1 = numeric(128)

for(i in 1:128)
{
  #Get the difference of 2 consecutive nubmers
  diff1[i] = pacDelays[i+1] - pacDelays[i] 
  diff_round1[i] = round(pacDelays[i+1] - pacDelays[i], digits = 3)
}

#So far, got the above code CORRECT!
hist(diff_round1, main = "Covert Packet Stream",xlab = "Time", ylab = "Packet Number")

