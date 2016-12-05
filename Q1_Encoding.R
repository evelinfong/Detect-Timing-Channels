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

encodings = rep(2:100, 2)

for(i in 1:trials)
{
  #Get the difference of 2 consecutive nubmers
  diff[i] = mydata$Time[i+1] - mydata$Time[i] 
  diff_round[i] = round(mydata$Time[i+1] - mydata$Time[i], digits = 2)
  
  #find the bit encodings
  if(diff_round[i] == 0.25)
  {
    encodings[i] = 0
  }
  
  else if(diff_round[i] == 0.75)
  {
    encodings[i] = 1
  }
}


