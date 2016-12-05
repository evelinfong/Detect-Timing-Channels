# Set the working directory
setwd("set your directory")

# Read CSV into R
# Make all 501 rows into mydata
options(max.print = 1000000)
mydata <- read.csv(file = "Traffic_data_orig.csv", header = TRUE, sep = ",",
                   quote = "\"", dec = ".", fill = TRUE, comment.char = "")


trials = 501

#Calculating the Max, Min, and Median
#Median
m = 0
m = median(mydata$Time, na.rm = FALSE)

#Maximum
maximum = 0
maximum = max(mydata$Time, na.rm = FALSE)

#Min of overt
minimum = 0
minimum = min(mydata$Time, na.rm = FALSE)


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


#Encoding covert packet stream
for(i in 1:len)
{
  
  Time[i] = i
  if(isTRUE(secMessb[i] == 1))
  {
    #x <- runif(1, m, maximum)
    x <- sample(m:maximum, 1, replace = FALSE)
    pacDelays[i] = x
  }
  
  
  if(isTRUE(secMessb[i] == 0))
  {
   # x <- runif(1, m, minimum)
    x <- sample(m:maximum, 1, replace = FALSE)
    
    pacDelays[i] = x
  }
  
} #encoding the secret message into time 

barplot(pacDelays)
