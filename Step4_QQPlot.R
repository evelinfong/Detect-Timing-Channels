set_1= rnorm(30, mean=0 ,sd=1)
set_2= rnorm(30, mean=0 ,sd=1)
qqplot(set_1, set_2)
qqline(set_2,  distribution = qnorm)

set_3= rnorm(100, mean=0 ,sd=1)
set_4= rnorm(100, mean=0 ,sd=1)
qqplot(set_3, set_4)
qqline(set_4,  distribution = qnorm)

set_5= rnorm(1000, mean=0 ,sd=1)
set_6= rnorm(1000, mean=0 ,sd=1)
qqplot(set_5, set_6)
qqline(set_6,  distribution = qnorm)

set_7= rnorm(100, mean=5 ,sd=3)
set_8= rnorm(100, mean=5 ,sd=3)
qqplot(set_7, set_8)
qqline(set_8,  distribution = qnorm)

set_9=rexp(100, rate = 1)
set_10=rexp(100, rate = 1)
qqplot(set_9, set_10)
qqline(set_10,  distribution = qexp)

set_11=rexp(1000, rate = 1)
set_12=rexp(1000, rate = 1)
qqplot(set_11, set_12)
qqline(set_12,  distribution = qexp)

set_13=rexp(500, rate = 1)
set_14=rexp(500, rate = 1)
qqplot(set_13, set_14)
qqline(set_14,  distribution = qexp)

#Part 6
# Set the working directory
setwd("C:\\Users\\Mariana Fan\\Desktop\\ECS 132\\Project\\Work")

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

#Find mean
MEANO = mean(diff_round, trim = 0, na.rm = TRUE)
#DO qqplot
set_15 = rexp(128, rate=MEANO)
set_16 = rexp(128, rate=MEANO)
qqplot(set_15, set_16, main = "Overt Stream")
qqline(set_16,  distribution = qexp)

MEANC = mean(diff_round1, trim = 0, na.rm = TRUE)
set_17 = rexp(128, rate = MEANC)
set_18 = rexp(128, rate = MEANC)
qqplot(set_17, set_18, main = "Covert Stream")
qqline(set_18,  distribution = qexp)


