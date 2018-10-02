#  BLAST SSL Analysis
#  Violet Kozloff
#  September 10th, 2018 
#  Adapted from mturk_ssl by An Nguyen
#  This script analyses reaction time for SSL files from the online session of the BLAST experiment
#  TO DO: Double check each step of the way that everything is still relevant...
#  TO DO: Set up checks for # of rows
#  ****************************************************************************

# Prepare workspace ------------------------------------------------------------

# Set directory
setwd("/Users/vkozloff/Documents/qlab/analysis/sl-web-analysis/blast-web-sl")
# Remove objects in environment
rm(list=ls())


#importing files
# TO DO: Make sure correct # each time for each trial
total_ssl_trial <- 48
language = list(1,1,2,1,1,1,2,2,2,2,1,1,1,2,2,1,2,2,1,1,2,1,2,1,2,1,2,1,1,2,2,2)

ssl <- read.csv("/Users/vkozloff/Documents/blast_adult_web_sl_data/clean/ssl_clean/ssl.csv")

#analysis on RT
# TO DO: Check this for each SL task for multiple participants. Why some start at different points???
fam_block <- ssl[which(ssl$trial_index<=539 & ssl$trial_index>=11),]
# TO DO: check that removing the correct ones for each SL task
fam_block <- fam_block[!(fam_block$stimulus=="ssl_instr7"),]
fam_block <- fam_block[!(fam_block$stimulus=="ssl_instr6"),]
# TO DO: What is this? Why?
 fam_block$targ <- paste(fam_block$targ)
 fam_block$stimulus <- paste(fam_block$stimulus)

#Extract the row number in which the stimulus is the target
target_rows <- which(fam_block$targ==fam_block$stimulus)


# Extract the response time and trial number when stimulus is the target------------------

# A valid response time comes from:
# A keypress during the 480 ms after the target stimulus is presented (on-target)
# A keypress during the 480 ms prior to the target stimulus presentation (anticipation)
# A keypress during the 480 ms following the target stimulus presentation (delay)


# Note: Each trial lasts 480 ms long, however the first 100 ms of a trial do not yet present the new stimulus
# There is a 100 second delay between the trial start and the audio stimulus
# Any keypresses during this time are recorded as negative values


# For test purposes, check the number of points that fall into each case
# 1: Anticipation after the preceding stimulus has been presented but before the target trial begins
# (positive RT recorded)
case1 <- NULL
# 2: Anticipation after the preceding stimulus has been presented and after the target trial begins
# (negative RT recorded)
case2 <- NULL
# 3: On-target after the target stimulus has been presented and before the following trial begins
# (positive RT recorded)
case3 <- NULL
# 4: On-target after the target stimulus has been presented and after the following trial begins
# (negative RT recorded)
case4 <- NULL
# 5: Delay after the following stimulus has been presented and before the next trial begins
# (positive RT recorded)
case5 <- NULL
# 6: Delay after the following stimulus has been presented and after the next trial begins
# (negative RT recorded)
case6 <- NULL
# 7: Test condition to make sure all points fall into the cases above; no points should fall into case 7
case7 <- NULL

# Variables to extract
rt_col <- NULL
id <- NULL
trial <- NULL
target <- NULL


for (i in target_rows){
  # Append the trial number and ID
  trial <- append(trial,paste(fam_block[i,][,"trial_index"]))
  id <- append(id,paste(fam_block[i,]$part_id))
  # Case 1 (anticipation, positive RT from preceding trial)
  if (i>1 && fam_block[i-1,]$rt > 0){
    rt_col <- append(rt_col, (fam_block[i-1,][,"rt"]-380))
    case1 <- append(case1, i)
    }
  # Case 2 (anticipation, negative RT from target trial)
  if (i>1 && fam_block[i,]$rt < 0){
    rt_col <- append(rt_col, (fam_block[i-1,][,"rt"]-380))
    case1 <- append(case1, i)
    }
}

  
  
  
  
  
  
  
  # ANTICIPATION
  # If the RT is less than 0 and during the correct trial and before the stimulus was presented, 
  # then they pressed the button in the preceding trial (before the target stimulus was presented)
  # Append it to the RT column
  # TO DO: Check this. Does this ever even occur? Put in a count here. Seems like only the previous case should exist?
  # TO DO: double check that && is correct here
  if (i>1 && fam_block[i,]$rt < 0 && fam_block[i-1,]$rt > 0){
    rt_col <- append(rt_col, fam_block[i-1,][,"rt"])
    case1 <- append(case1, i)
  }
}

if (i>1 && fam_block[i,]$rt < 0 && fam_block[i-1,]$rt > 0){
  rt_col <- append(rt_col, fam_block[i-1,][,"rt"])
  case1 <- append(case1, i)
}

  # ON-TARGET 1
  # If the RT occurs during the target and after the stimulus was presented (greater than 0), 
  # subtract 100 and append it to rt. 100 comes from the delay from JSPsych 
  # between when the trial starts and when the syllable plays
  # Note for the future: If this experiment is ever recreated JSPsych in version 6, no longer the 100 delay
  else if (fam_block[i,]$rt > 0){
    rt_col <- append(rt_col,fam_block[i,][,"rt"]-100)
    case2 <- append(case2, i)
  }
  
  # ON-TARGET 2
  # If the subject missed the target, but responded to the following trial
  # TO DO: Again, does this happen? Worth putting a counter here to see how many times
  else if (!(fam_block[i+1,][,"rt"]=-1000)
      # (with a negative RT, meaning that they pressed the button before the trial started, 
      # ie. within the 100 ms before the sound started)
      & fam_block[i+1,][,"rt"]<0){
    # Subtract the second trial's RT from 380. Replace the first RT with it.
    # Each block lasts 480, but take the 100 ms delay off to get to 380
    rt_col[(match(i,target_rows))] <- 380-fam_block[i+1,][,"rt"]
    case3 <- append(case3, i)
  }
  else{
    # This should never happen! Test value of case6 to be super sure.
    case6 <- append(case6, i)
  }
}
  
  # POST-TARGET
  # If the subject responded to the following trial
  else if (fam_block[i+1,][,"rt"]!=-1000 
      # After the following stimulus had been presented (so positive RT)
      & fam_block[i+1,][,"rt"]>0){
    # Add 480 to the second line's RT and replace the first RT with it.
    # 480 = time of following stimulus presentation - time of target stimulus presentation
    rt_col[(match(i,target_rows))] <- 480+fam_block[i+1,][,"rt"]
    case4 <- append(case4, i)
  }
}
  
  # TO DO: Check that first condition of && is necessary
  # TO DO: Check && vs. & and their conditions
  # TO DO: If this is correct, add to all scripts
  # TO DO: Shouldn't this be if the preceding rt is greater than -1000? And 2 cases, neg/ pos?
  # TO DO: What is this condition?
  else (i>1 && fam_block[i-1,][,"rt"]>0){
    rt_col[(match(i,target_rows))] <- 480-fam_block[i-1,][,"rt"]
    case5 <- append(case5, i)
  }
}
  

fam_trial <- data.frame(unlist(trial),unlist(rt_col),id)
colnames(fam_trial) <- c("trial","rt_col","id")

#Re-index the trial number of the response so that it ranges from 1-24 (because there are 24 stimuli in total for both auditory and visual)
a<-NULL
for (i in (unique(fam_trial$id))){a<- append(a,sum(fam_trial$id==i))}
reindex <- NULL
for (i in a) {reindex <- append(reindex,rep(1:i,1))}     

fam_trial$reindex <- reindex

# These 4 are used to calculate dprime
# hit rate: hit button when target is present
hit_rate <- NULL
# miss rate: didn't hit burron during target
miss_rate <- NULL
# when not target, don't press button
correct_rejection <- NULL
# When not target, press button
false_alarm <- NULL

mean_rt <- NULL
rt_slope <- NULL
#TO DO: Make sure no "timeline" for any script

#only accept answers in range of -1000 < x < 1000
# TO DO: Check with Zhenghan that this is still the right #
# Mean table takes out any time when the participant didn't respond to the target 
mean_table <- fam_trial[which(fam_trial$rt_col!=-1 & fam_trial$rt_col<1000 & fam_trial$rt_col>-1000), ] 

# TO DO: Fix this
# exclude people who only have one rt point, so rsslope cannot be computed
# mean_table <- mean_table[mean_table$id!="msslAG1213",]

list_ssl_id <- unique(mean_table$id)
# Fam block = all the familiarization
# Fam trial = only when target was presented
# mean table = only the targets they actually responded to

# TO DO: Make sure nothing missing from the mturk ssl script of An's
#Extract the mean response time, rt slope, hit rate, miss rat, correct rejection, and false alarm for each participant
for(id in list_ssl_id){
  mean_rt<-append(mean_rt,round(mean(mean_table$rt_col[mean_table$id==id]),digits=3))
  # Here we use mean table, because it's when they actually pressed
  rt_slope <-append(rt_slope,round(summary(lm(mean_table$rt_col[mean_table$id==id]~mean_table$reindex[mean_table$id==id]))$coefficient[2,1],digits=3))
  hit_rate<-append(hit_rate,round(sum(!is.na(mean_table$rt_col[mean_table$id==id]))/total_ssl_trial,digits =2))
  # -1 means that 
  # TO DO: Check for each file when it's -1000 and when it's -1
  # TO DO: Make sure that vsl and lsl follow mturk visual and that ssl and tsl follow mturk auditory
  # fam trial: stimulus was present. rt of -1000 means didn't press
  miss_rate<-append(miss_rate,round(sum(fam_trial$rt_col[fam_trial$id==id]==-1000)/total_ssl_trial,digits=2))
  # -1000 means didn't press
  # target =/= stimulus
  correct_rejection <- append(correct_rejection, round(sum(fam_block$rt[fam_block$part_id==id]==-1000 & fam_block$targ[fam_block$part_id==id]!=fam_block$stimulus[fam_block$part_id==id])/264,digits=2)) #264 is the total number of stimuli in the familiarization block
  false_alarm <- append(false_alarm, round(sum(fam_block$rt[fam_block$part_id==id]!=-1000 & fam_block$targ[fam_block$part_id==id]!=fam_block$stimulus[fam_block$part_id==id])/264,digits=2))
}

subj_table <- data.frame(list_ssl_id,mean_rt, rt_slope,hit_rate, miss_rate,correct_rejection,false_alarm)

# TO DO: Make sure this still works
lowerbound <- mean(subj_table$rt_slope) - 2.5*sd(subj_table$rt_slope)
upperbound <- mean(subj_table$rt_slope) + 2.5*sd(subj_table$rt_slope)
subj_table <- subj_table[subj_table$rt_slope>=lowerbound,]
subj_table <- subj_table[subj_table$rt_slope<=upperbound,]

#Extract the testing phase
#test block
# TO DO: Make sure these numbers are right for all participants... (this varies participant to participant, but won't vary more than 1)
# Check upper bound from each file from An
test_block <- ssl[which(ssl$trial_index<=815 & ssl$trial_index>=587),]
test_block <- test_block[!(test_block$stimulus==""),]
# TO DO: Make sure these are deleted from each SL activity
test_block <- test_block[!(test_block$stimulus=="ssl_instr11.wav"),]
test_block <- test_block[!(test_block$stimulus=="ssl_instr11"),]
# TO DO: Why lsl instructions in these files?? Check that these are the correct ones
test_block <- test_block[!(test_block$stimulus=="lsl_instr13.wav"),]
test_block <- test_block[!(test_block$stimulus=="lsl_instr13"),]
test_block <- test_block[!(test_block$stimulus=="silence" & test_block$key_press==-1),]
# TO DO: Figure out how to standardize this. Why did they press 174? Seems like they turned the volume up? Is it ok to just get rid of this trial?
test_block <- test_block[!(test_block$key_press==174),]


ans <- NULL
keyv <- NULL
subj <- NULL
cond<- NULL

#Extract rows in which the participant gives a response
#target_rowsv is just row number for the test block
target_rowsv <- which(test_block$key_press != -1 & test_block$stimulus=="silence")
for (i in target_rowsv){
  ans<-append(ans,test_block[i,]$key_press)
  subj <- append(subj,paste(test_block[i,]$part_id))
  cond <- append(cond,paste(test_block[i,]$cond))
}

# Create a data frame that contains the participants' responses
ssl_accuracy <- data.frame(ans,subj,cond)
ssl_cond <- NULL

#For whatever reason, this gives 22 (not 20). Check it.
for (i in seq(from=1,to=length(ssl_accuracy$cond),by=32)){ssl_cond<-append(ssl_cond,as.character(ssl_accuracy[i,]$cond))}

keyv<- NULL

#TO DO: Why is there one missing??
ssl_cond[22]<-"lang2"

i=0

# Combine the answer keys for the two language conditions that the participant saw
keyv <- rep(language, times = length(unique(ssl_accuracy$subj)))

# Find all of the IDs for the participants whose accuracy you're calculating
acc_id <- unique(ssl_accuracy$subj)


ssl_accuracy$key <- keyv

#Substitute the key press (37,39) with the answer (1,2)
# TO DO: Check TSL. Here this is correct!
ssl_accuracy$ans <- gsub(37,1,ssl_accuracy$ans)
ssl_accuracy$ans <- gsub(39,2,ssl_accuracy$ans)


#Loop through and count the correct answer
corr <- NULL
for (i in seq(from=1,to=length(ssl_accuracy$ans),by=1)) {corr<-append(corr,as.numeric(ssl_accuracy[i,]$ans==ssl_accuracy[i,]$key))}
ssl_accuracy$corr <- corr
subj_corr <- NULL
for (id in acc_id) {subj_corr <- append(subj_corr,round(sum(ssl_accuracy$corr[ssl_accuracy$subj==id])/32,digits=3))}
ssl_acc_table <- data.frame(acc_id,subj_corr)

