#  BLAST TSL Analysis
#  Violet Kozloff
#  September 10th, 2018 
#  Adapted from mturk_tsl by An Nguyen
#  This script analyses reaction time for tsl files from the online session of the BLAST experiment
#  TO DO: Double check each step of the way that everything is still relevant...
#  TO DO: Set up checks for # of rows
#  ****************************************************************************

# Prepare workspace ------------------------------------------------------------

# Set directory
setwd("/Users/vkozloff/Documents/qlab/analysis/blast-adults-online")
# Remove objects in environment
rm(list=ls())


#importing files
total_tsl_trial <- 24
language_1 = list(1,2,2,2,1,1,2,1,1,2,1,2,1,1,2,2,1,1,2,1,2,2,1,2,2,2,1,2,1,2,1,1)
language_2 = list(1,1,2,1,1,1,2,2,2,2,1,1,1,2,2,1,2,2,1,1,2,1,2,1,2,1,2,1,1,2,2,2)

tsl <- read.csv("/Users/vkozloff/Documents/blast_adult_online_data/clean/tsl/tsl.csv")

#analysis on RT
fam_block <- tsl[which(tsl$trial_index<=600 & tsl$trial_index>=24),]
fam_block <- fam_block[!(fam_block$stimulus=="../../tone_instr/instr_8"),]
fam_block <- fam_block[!(fam_block$stimulus=="../../tone_instr/fixation_instr"),]
fam_block$targ <- paste(fam_block$targ)
fam_block$stimulus <- paste(fam_block$stimulus)

#TO DO: delete this
fam_block$stimulus<- gsub("../../tones/","",fam_block$stimulus)


rt_col <- NULL
id <- NULL
trial <- NULL
target <- NULL

#Extract the row number in which the stimulus is the target
row_number <- which(fam_block$targ==fam_block$stimulus)

#Extract the response time and trial number when stimulus is the target
for (i in row_number){
  if (fam_block[i,]$rt > 0){rt_col <- append(rt_col,fam_block[i,][,"rt"]-100)} 
  if (fam_block[i,]$rt < 0){rt_col <- append(rt_col,fam_block[i,][,"rt"])}
  trial <- append(trial,paste(fam_block[i,][,"trial_index"]))
  id <- append(id,paste(fam_block[i,]$part_id))
  if (fam_block[i+1,][,"rt"]!=-1000 & fam_block[i+1,][,"rt"]<0){
    rt_col[(match(i,row_number))] <- 380-fam_block[i+1,][,"rt"]}
  
  if (fam_block[i+1,][,"rt"]!=-1000 & fam_block[i+1,][,"rt"]>0){
    rt_col[(match(i,row_number))] <- 580+fam_block[i+1,][,"rt"]}
  
  if (fam_block[i-1,][,"rt"]>0){
    rt_col[(match(i,row_number))] <- 480-fam_block[i-1,][,"rt"]
  }}

fam_trial <- data.frame(unlist(trial),unlist(rt_col),id)
colnames(fam_trial) <- c("trial","rt_col","id")

#Re-index the trial number of the response so that it ranges from 1-24 (because there are 24 stimuli in total)
reindex <- rep(1:total_tsl_trial,length(fam_trial$trial)/24)
fam_trial$reindex <- reindex

hit_rate <- NULL
miss_rate <- NULL
correct_rejection <- NULL
false_alarm <- NULL
mean_rt <- NULL
rt_slope <- NULL
timeline <- c(rep("first half",total_tsl_trial/2),rep("second half",total_tsl_trial/2))
timeline <- rep(timeline,length(fam_trial$trial)/24)
fam_trial$timeline <- timeline
mean_table <- fam_trial[which(fam_trial$rt_col!=-1 & fam_trial$rt_col<1000 & fam_trial$rt_col>-1000), ] #only accept answers in range of -1000 < x < 1000

# TO DO: Fix this
# exclude people who only have one rt point, so rtslope cannot be computed
# mean_table <- mean_table[mean_table$id!="mtslAG1213",]

#tsl2
#mean_table <- mean_table[mean_table$id!="A1FDP7EMSL9T9F",]
#mean_table <- mean_table[mean_table$id!="mtslen0591",]
#mean_table <- mean_table[mean_table$id!="mtslmd1085",]

list_tsl_id <- unique(mean_table$id)

#Extract the mean response time, rt slope, hit rate, miss rat, correct rejection, and false alarm for each participant
for(id in list_tsl_id){
  mean_rt<-append(mean_rt,round(mean(mean_table$rt_col[mean_table$id==id]),digits=3))
  rt_slope <-append(rt_slope,round(summary(lm(mean_table$rt_col[mean_table$id==id]~mean_table$reindex[mean_table$id==id]))$coefficient[2,1],digits=3))
  hit_rate<-append(hit_rate,round(sum(!is.na(mean_table$rt_col[mean_table$id==id]))/total_tsl_trial,digits =2))
  miss_rate<-append(miss_rate,round(sum(fam_trial$rt_col[fam_trial$id==id]==-1)/total_tsl_trial,digits=2))
  correct_rejection <- append(correct_rejection, round(sum(fam_block$rt[fam_block$part_id==id]==-1 & fam_block$targ[fam_block$part_id==id]!=fam_block$stimulus[fam_block$part_id==id])/264,digits=2)) #264 is the total number of stimuli in the familiarization block
  false_alarm <- append(false_alarm, round(sum(fam_block$rt[fam_block$part_id==id]!=-1 & fam_block$targ[fam_block$part_id==id]!=fam_block$stimulus[fam_block$part_id==id])/264,digits=2))
}

subj_table <- data.frame(list_tsl_id,mean_rt, rt_slope,hit_rate, miss_rate,correct_rejection,false_alarm)
#dprime<-NULL
#for (i in seq(from=1,to=length(subj_table$list_tsl_id),by=1)){dprime<-append(dprime,qnorm(subj_table[i,]$hit_rate-0.00000001)-qnorm(subj_table[i,]$false_alarm+0.000000001))} #minus 0.000000001 to avoid perfect hit rate
#subj_table$dprime <- round(dprime,3)

# TO DO: What is this?
#lowerbound <- mean(subj_table$rt_slope) - 2.5*sd(subj_table$rt_slope)
#upperbound <- mean(subj_table$rt_slope) + 2.5*sd(subj_table$rt_slope)
#subj_table <- subj_table[subj_table$rt_slope>=lowerbound,]
#subj_table <- subj_table[subj_table$rt_slope<=upperbound,]

#Extract the testing phase
#test block
# TO DO: Make sure these numbers are right for all participants...
test_block <- tsl[which(tsl$trial_index<=827 & tsl$trial_index>=603),]
test_block <- test_block[!(test_block$stimulus==""),]
test_block <- test_block[!(test_block$stimulus=="silent" & test_block$key_press==-1),]

ans <- NULL
keyv <- NULL
subj <- NULL
cond<- NULL

#Extract rows in which the participant gives a response
row_numberv <- which(test_block$key_press != -1 & test_block$stimulus=="silent")
for (i in row_numberv){
  ans<-append(ans,test_block[i,]$key_press)
  subj <- append(subj,paste(test_block[i,]$part_id))
  cond <- append(cond,paste(test_block[i,]$cond))
}

# Create a data frame that contains the participants' responses
tsl_accuracy <- data.frame(ans,subj,cond)
tsl_accuracy <- tsl_accuracy[!(tsl_accuracy$ans==32),]
tsl_cond <- NULL

for (i in seq(from=1,to=length(tsl_accuracy$cond),by=32)){
  tsl_cond<-append(tsl_cond,as.character(tsl_accuracy[i,]$cond))
}

keyv<- NULL

# Combine the answer keys for the two language conditions that the participant saw
for(cond in tsl_cond){
  # TO DO: Double check that this is relevant
  if (cond=="lang1"){keyv<-append(keyv,language_1)}
  else if (cond=="lang2"){keyv<-append(keyv,language_2)}}


# Find all of the IDs for the participants whose accuracy you're calculating
acc_id <- unique(tsl_accuracy$subj)


tsl_accuracy$key <- keyv

# TO DO: Check correct substitution
#Substitute the key press (49,50) with the answer (1,2)
tsl_accuracy$ans <- gsub(39,2,tsl_accuracy$ans)
tsl_accuracy$ans <- gsub(37,1,tsl_accuracy$ans)

#Loop through and count the correct answer
corr <- NULL
for (i in seq(from=1,to=length(tsl_accuracy$ans),by=1)) {corr<-append(corr,as.numeric(tsl_accuracy[i,]$ans==tsl_accuracy[i,]$key))}
tsl_accuracy$corr <- corr
subj_corr <- NULL
for (id in acc_id) {subj_corr <- append(subj_corr,round(sum(tsl_accuracy$corr[tsl_accuracy$subj==id])/32,digits=3))}
tsl_acc_table <- data.frame(acc_id,subj_corr,tsl_cond)
