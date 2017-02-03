# Jason Laso                    #
# UFC Fight Card Simulation     #
# 2/2/17                        #
#################################

library(ggplot2)
library(dplyr)

# read in data
# This is a data set of all UFC fights and fighters from 1993 to January 2016.
fighters = read.csv("C://Users//ja041718//Downloads//ALL UFC FIGHTERS 2%2F23%2F2016 SHERDOG.COM - Sheet1.csv", header=T)
fights = read.csv("C://Users//ja041718//Downloads//ALL UFC FIGHTS 2%2F23%2F2016 SHERDOG.COM - Sheet1.csv", header=T)

# Remove URLs
fights=fights[,-c(1,8,9)]
fighters = fighters[, -1]

summary(fights)



# How many types of finishes could we expect to see on a typical 12-fight UFC card? This is my attempt to
# quantify that using hacker statistics.

# subset relevant finishes
methods=c("Submission", "TKO", "KO", "Decision")

# Subset fights table just for fights with relevant finishes
fights_method_subset = fights[fights$method %in% methods,]

#initialize vectors for simulation
dec_12=c()
sub_12=c()
ko_12 =c()
tko_12 = c()

# Simulate a typical 12-fight card for how many finishes will be on each show. Set the simulation parameters.
iterations=100000
set.seed(1)

# Set number of fights on the card
n_fights = 12

for (i in 1:iterations){
  
  # create sample 12-fight card  
  sample = sample(fights_method_subset$method, n_fights, replace=T)
  
  # assign no. of finishes of each type to a vector  
  for(method in methods){  
    x = sum(sample == method)
    
       if(method == methods[2]){
            tko_12[i] = x
        } else if(method == methods[1]){
            sub_12[i] = x
        } else if(method == methods[3]){
            ko_12[i] = x
        } else{
            dec_12[i] = x
        }
        
   print(i) #progress tracker
  }
}

# We need to tidy up the data frame for visualization purposes.

# change vectors into data frames
tko_12 = as.data.frame(tko_12);sub_12=as.data.frame(sub_12);ko_12= as.data.frame(ko_12);dec_12 =as.data.frame(dec_12)

#label the finishes in a new row for identification in tidy data frame
tko_12$finish = "tko"
ko_12$finish = "ko"
sub_12$finish = "sub"
dec_12$finish = "dec"

#rename the first column in all 4 data frames
colnames(tko_12)[1] = c("n"); colnames(sub_12)[1] = c("n"); colnames(dec_12)[1] = c("n"); colnames(ko_12)[1] = c("n")

#now we can row bind the results into a tidy data frame fit for visualization
card_12 = rbind(tko_12, ko_12, dec_12, sub_12)



                                          #### Data Analysis ####

# Take a look at the summary stats for each finish per card
summary(tko_12)
summary(ko_12)
summary(sub_12)
summary(dec_12)

# A list of the mean number of finishes of each method per show for possible use later
mean_finish = c(mean(tko_12$n),mean(sub_12$n),mean(ko_12$n),mean(dec_12$n))

# plot distributions (facet wrap)
ggplot(card_12, aes(x=n, y= ..density.., col=finish)) + geom_freqpoly(binwidth=1, lwd=2) + 
  scale_x_continuous(breaks=0:12) + facet_wrap(~finish)

# plot distributions (one plot)
ggplot(card_12, aes(x=n, y= ..density.., col=finish)) + geom_freqpoly(binwidth=1, lwd=2) + 
  scale_x_continuous(breaks=0:12)

    
# Calculate the frequency table for number of finishes
finishes_freq = card_12 %>% group_by(finish, n) %>% summarize(number = n(), 
  percent = number/iterations) %>% arrange(desc(percent))

#Filter for just decisions How likely is it that there are 8 decisions on a card?
dec_freq = finishes_freq %>% filter(finish == "dec")
