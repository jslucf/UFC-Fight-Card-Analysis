# Jason Laso                    #
# UFC Fight Card Simulation     #
# 2/2/17                        #
#################################

library(ggplot2)
library(dplyr)

# read in data
# This is a data set of all UFC fights and fighters from 1993 to January 2016.
fighters = read.csv("C://Users//Jason//Downloads//UFC-Fight-Card-Analysis-master//UFC-Fight-Card-Analysis-master//ALL UFC FIGHTERS 2%2F23%2F2016 SHERDOG.COM - Sheet1.csv", header=T)
fights = read.csv("C://Users//Jason//Downloads//UFC-Fight-Card-Analysis-master//UFC-Fight-Card-Analysis-master////ALL UFC FIGHTS 2%2F23%2F2016 SHERDOG.COM - Sheet1.csv", header=T)

# Remove URLs
fights=fights[,-c(1,8,9)]
fighters = fighters[, -1]

summary(fights)



### How many types of finishes could we expect to see on a typical UFC card? This is my attempt to
### quantify that using hacker statistics.

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
n_fights = 10

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

#Filter for just decisions How likely is it that there are 9 decisions on a card?
dec_freq = finishes_freq %>% filter(finish == "dec")

## Plot the distribution just for 10-fight shows and relate to UFC 208 probability
ggplot(subset(card_12, finish=="dec"), aes(x=n, y= ..density..)) + 
  geom_freqpoly(binwidth=1, lwd=2) + 
  scale_x_continuous(breaks=0:10) + 
  geom_vline(xintercept=9, col="red") + 
  geom_hline(yintercept=0, lwd=1.5) + 
  geom_text(aes(x=9, y=.2, label= paste("UFC 208 \n", dec_freq[dec_freq$n == 9, "percent"])), 
            hjust=-.25,  color="red", size=8)+
  ggtitle(paste("Probability of X Decisions on a", n_fights,  "Fight UFC Card")) +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  xlab(paste("Decisions (out of ", n_fights, ")", sep=""))

### We see that UFC 208's likelihood of occurring was .00272, or about 1:368. To put that into perspective
### UFC 208 was the 389th show in UFC's history, a once in 23-year event. A true outlier.

### Now let's create a new DF where we can keep track of the finishes at each actual UFC event ###

# subset the event names
event_names = unique(fights_method_subset$event_name)

#create new df to store data on each event
events = data.frame(event = event_names, submissions = 0, decisions = 0, ko = 0, tko = 0, other = 0)

#loop through each event in the fights df to count number of finishes per card
for(ev in event_names){
  
  for(row in 1:nrow(fights)){
 
    if(fights$event_name[row] == ev){
      
      if(fights$method[row] == "Submission"){
        events[events[,1] == ev, 2] = events[events[,1] == ev, 2] +1
        
      } else if(fights$method[row] == "Decision"){
        events[events[,1] == ev, 3] = events[events[,1] == ev, 3] +1
        
      } else if(fights$method[row] == "KO"){
        events[events[,1] == ev, 4] = events[events[,1] == ev, 4] +1
        
      } else if(fights$method[row] == "TKO"){
        events[events[,1] == ev, 5] = events[events[,1] == ev, 5] +1
        
      } else{events[events[,1] == ev, 6] = events[events[,1] == ev, 6] +1
      }
    } 

  }
}

#create new column for total fights on each card and percentage of decisions
events$total_fights = events$decisions + events$submissions + events$ko + events$tko + events$other
events$decisions_pct = events$decisions / events$total_fights

#now let's subset the events DF into a new df for only shows with 70% decisions or higher
events_high_dec = events %>% filter(decisions_pct >= .7) %>% arrange(desc(decisions_pct))

### To really put UFC 208's 90% decision rate into perspectve:
### Only 16/343 shows have seen at least 70% decisions and only 3 over 80%.

