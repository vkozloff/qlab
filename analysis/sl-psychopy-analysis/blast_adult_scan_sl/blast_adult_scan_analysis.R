#  NOVEMBER PILOT RT SLOPE ANALYSIS
#  Violet Kozloff
#  April 7, 2017
#  This script analyzes structured and random blocks across four tasks: auditory (speech and tones) and visual (letters and images).
#  It measures the mean reaction time and the slope of the reaction time for each participant for each condition.
#  NOTE: relevant columns pre-selected through fmri_data_cleaning.Rmd
#  ****************************************************************************

# TO DO: exclude participants who did not respond in a condition
# TO DO: Remove points outside 2.5 stdev of mean


# ******************** I. PREPARE FILES *************************



# Prepare workspace ------------------------------------------------------------------------------------------------------

#Set correct working directory
setwd("/Users/vkozloff/Documents/qlab/analysis/sl-psychopy-analysis/blast_adult_scan_sl")

# Remove objects in environment
rm(list=ls())


# Prepare paths for files --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

auditory_data_frame <- read.csv("../../../../blast_adult_scan_sl_data/clean/auditory_clean/auditory.csv")
visual_data_frame <- read.csv("../../../../blast_adult_scan_sl_data/clean/visual_clean/visual.csv")


# Convert targets and soundFile from factors to atomic variables 
auditory_data_frame$starget<-as.character(auditory_data_frame$starget)
auditory_data_frame$ttarget<-as.character(auditory_data_frame$ttarget)
auditory_data_frame$stimulus<-as.character(auditory_data_frame$stimulus)
visual_data_frame$ltarget<-as.character(visual_data_frame$ltarget)
visual_data_frame$vtarget<-as.character(visual_data_frame$vtarget)
visual_data_frame$stimulus<-as.character(visual_data_frame$stimulus)



# ******************** II. EXTRACT RELEVANT AUDITORY DATA (ID, MODALITY, DOMAIN, TYPE, RT_SLOPE, MEAN_RT) BY CONDITION, THEN COMBINE 4 CONDITIONS TOGETHER**************************

# ******************** CONDITION 1: TSL*******************


# Identify response times to target stimuli. Include times when participant responded while target was displayed,
# or during preceding/ following stimulus -----------------------------------------------------------------------

# Set up variables to loop through participants by trials and track the target
rt_col <- NULL
id <- NULL
trial <-NULL
target <- NULL
condition <-NULL

# Identify the rows when this condition's target was presented
tsl_targets <- which(tolower(auditory_data_frame$ttarget)==tolower(auditory_data_frame$stimulus))

# Isolate participants' response times.
# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in tsl_targets) {
  # Isolate the ID number
  id <- append(id, paste(auditory_data_frame[i,]$part_id))
  # Isolate the condition
  condition <- append(condition, paste(auditory_data_frame[i,]$condition))
  # If the participant responded while the target was presented
  if (!is.na(auditory_data_frame[i,] [,"tsl_rt"])){
    # Count their response time from the target stimulus
    rt_col <- append (rt_col, auditory_data_frame[i,][,"tsl_rt"])}
  # If the participant responded during the stimulus following the target
  else if (!is.na(auditory_data_frame[i+1,] [,"tsl_rt"])){
    # Count their response time as the duration that the target was presented (480 ms) plus the response time to the following stimulus 
    rt_col[(match(i, tsl_targets))] <- .48+auditory_data_frame[i+1, ][,"tsl_rt"]}
  # Check if you are looking at the first target, which is always the first stimulus. If so, it does not have a preceeding target
  else if (i>0){ 
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, auditory_data_frame[i,][,"tsl_rt"])}   
  # If the participant responded during the stimulus preceding the target
  else if (!is.na(auditory_data_frame[i-1,] [,"tsl_rt"])){
    # Count their response time as how much sooner they responded than when the stimulus was presented
    rt_col[match(i, tsl_targets)] <- 0-auditory_data_frame[i-1,][,"tsl_rt"]}
  # If the participant did not respond within 1 stimulus, 
  else if (is.na(auditory_data_frame[i,] [,"tsl_rt"])){
    # Copy their response time of NA
    rt_col <- append (rt_col, visual_data_frame[i,][,"tsl_rt"])}
}

# Match id, condition, and response times
tsl_extracted <- data.frame(id,condition,rt_col)

random_tsl<- tsl_extracted[which(tsl_extracted$condition=="R"),]
structured_tsl<- tsl_extracted[which(tsl_extracted$condition=="S"),]


# Random TSL analysis

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
list_part_id <- unique(random_tsl$id)
# TO DO: what is the weird extra NA one?

# Test: find the number of targets shown to each participant
# TO DO: These should all be the same, right? How many?
a <- NULL
for(i in list_part_id){a <- append(a,sum(random_tsl$id==i))}

# For each participant, index the targets
reindex <- NULL
for (i in a) {reindex <- append (reindex, rep(1:i, 1))}

# Add the targets' indices
random_tsl$reindex <- reindex

# Remove any values lower than -.48, higher than .96, or of NA
# TO DO: Still these values?
random_tsl <- random_tsl[random_tsl$rt_col<=0.96 & random_tsl$rt_col>= -.48 & !is.na(random_tsl$rt_col),]


# Calculate random mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
domain <- NULL
modality <- NULL
condition <- NULL
type <- NULL
task<- NULL
rtsl_range <- NULL
upper_bound <- NULL
lower_bound <- NULL

# For each participant, extract id
# Assign domain, type, and modality
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in list_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "auditory")
  task <- append(task, "tsl")
  type <- append (type, "random")
  modality <- append (modality, "non-linguistic")
  mean_rt <- append(mean_rt, round(mean(random_tsl$rt_col[random_tsl$id==id]),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(random_tsl$rt_col[random_tsl$id==id]~random_tsl$reindex[random_tsl$id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (random_tsl[ which(random_tsl$id==id),])
  this_range<- range(data_this_id$rt_col, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

rtsl<- NULL

# Combine data for each participant
rtsl <- data.frame(part_id, task, domain,type,modality,mean_rt, upper_bound, lower_bound, rt_slope)


# structured TSL analysis

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
list_part_id <- unique(structured_tsl$id)

# Test: find the number of targets shown to each participant
# TO DO: These should all be the same, right? How many?
a <- NULL
for(i in list_part_id){a <- append(a,sum(structured_tsl$id==i))}

# For each participant, index the targets
reindex <- NULL
for (i in a) {reindex <- append (reindex, rep(1:i, 1))}

# Add the targets' indices
structured_tsl$reindex <- reindex

# Remove any values lower than -.48, higher than .96, or of NA
# TO DO: Still these values?
structured_tsl <- structured_tsl[structured_tsl$rt_col<=0.96 & structured_tsl$rt_col>= -.48 & !is.na(structured_tsl$rt_col),]


# Calculate structured mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
domain <- NULL
modality <- NULL
condition <- NULL
type <- NULL
task<- NULL
stsl_range <- NULL
upper_bound <- NULL
lower_bound <- NULL

# For each participant, extract id
# Assign domain, type, and modality
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in list_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "auditory")
  task <- append(task, "tsl")
  type <- append (type, "structured")
  modality <- append (modality, "non-linguistic")
  mean_rt <- append(mean_rt, round(mean(structured_tsl$rt_col[structured_tsl$id==id]),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(structured_tsl$rt_col[structured_tsl$id==id]~structured_tsl$reindex[structured_tsl$id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (structured_tsl[ which(structured_tsl$id==id),])
  this_range<- range(data_this_id$rt_col, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

stsl<- NULL

# Combine data for each participant
stsl <- data.frame(part_id, task, domain,type,modality,mean_rt, upper_bound, lower_bound, rt_slope)

# for internal checking only: find mean rt_slope. This should be negative.
mean_stsl_rt_slope <- mean (stsl$rt_slope)



# ******************** CONDITION 2: ssl*******************


# Identify response times to target stimuli. Include times when participant responded while target was displayed,
# or during preceding/ following stimulus -----------------------------------------------------------------------

# Set up variables to loop through participants by trials and track the target
rt_col <- NULL
id <- NULL
trial <-NULL
target <- NULL
condition <-NULL

# Identify the rows when this condition's target was presented
ssl_targets <- which(tolower(auditory_data_frame$starget)==tolower(auditory_data_frame$stimulus))

# Isolate participants' response times.
# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in ssl_targets) {
  # Isolate the ID number
  id <- append(id, paste(auditory_data_frame[i,]$part_id))
  # Isolate the condition
  condition <- append(condition, paste(auditory_data_frame[i,]$condition))
  # If the participant responded while the target was presented
  if (!is.na(auditory_data_frame[i,] [,"ssl_rt"])){
    # Count their response time from the target stimulus
    rt_col <- append (rt_col, auditory_data_frame[i,][,"ssl_rt"])}
  # If the participant responded during the stimulus following the target
  else if (!is.na(auditory_data_frame[i+1,] [,"ssl_rt"])){
    # Count their response time as the duration that the target was presented (480 ms) plus the response time to the following stimulus 
    rt_col[(match(i, ssl_targets))] <- .48+auditory_data_frame[i+1, ][,"ssl_rt"]}
  # Check if you are looking at the first target, which is always the first stimulus. If so, it does not have a preceeding target
  else if (i>0){ 
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, auditory_data_frame[i,][,"ssl_rt"])}   
  # If the participant responded during the stimulus preceding the target
  else if (!is.na(auditory_data_frame[i-1,] [,"ssl_rt"])){
    # Count their response time as how much sooner they responded than when the stimulus was presented
    rt_col[match(i, ssl_targets)] <- 0-auditory_data_frame[i-1,][,"ssl_rt"]}
  # If the participant did not respond within 1 stimulus, 
  else if (is.na(auditory_data_frame[i,] [,"ssl_rt"])){
    # Copy their response time of NA
    rt_col <- append (rt_col, visual_data_frame[i,][,"ssl_rt"])}
}

# Match id, condition, and response times
ssl_extracted <- data.frame(id,condition,rt_col)

random_ssl<- ssl_extracted[which(ssl_extracted$condition=="R"),]
structured_ssl<- ssl_extracted[which(ssl_extracted$condition=="S"),]


# Random ssl analysis

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
list_part_id <- unique(random_ssl$id)

# Test: find the number of targets shown to each participant
# TO DO: These should all be the same, right? How many?
a <- NULL
for(i in list_part_id){a <- append(a,sum(random_ssl$id==i))}

# For each participant, index the targets
reindex <- NULL
for (i in a) {reindex <- append (reindex, rep(1:i, 1))}

# Add the targets' indices
random_ssl$reindex <- reindex

# Remove any values lower than -.48, higher than .96, or of NA
# TO DO: Still these values?
random_ssl <- random_ssl[random_ssl$rt_col<=0.96 & random_ssl$rt_col>= -.48 & !is.na(random_ssl$rt_col),]


# Calculate random mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
domain <- NULL
modality <- NULL
condition <- NULL
type <- NULL
task<- NULL
rssl_range <- NULL
upper_bound <- NULL
lower_bound <- NULL

# For each participant, extract id
# Assign domain, type, and modality
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in list_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "auditory")
  task <- append(task, "ssl")
  type <- append (type, "random")
  modality <- append (modality, "linguistic")
  mean_rt <- append(mean_rt, round(mean(random_ssl$rt_col[random_ssl$id==id]),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(random_ssl$rt_col[random_ssl$id==id]~random_ssl$reindex[random_ssl$id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (random_ssl[ which(random_ssl$id==id),])
  this_range<- range(data_this_id$rt_col, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

rssl<- NULL

# Combine data for each participant
rssl <- data.frame(part_id, task, domain,type,modality,mean_rt, upper_bound, lower_bound, rt_slope)



# structured ssl analysis

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
list_part_id <- unique(structured_ssl$id)

# Test: find the number of targets shown to each participant
# TO DO: These should all be the same, right? How many?
a <- NULL
for(i in list_part_id){a <- append(a,sum(structured_ssl$id==i))}

# For each participant, index the targets
reindex <- NULL
for (i in a) {reindex <- append (reindex, rep(1:i, 1))}

# Add the targets' indices
structured_ssl$reindex <- reindex

# Remove any values lower than -.48, higher than .96, or of NA
# TO DO: Still these values?
structured_ssl <- structured_ssl[structured_ssl$rt_col<=0.96 & structured_ssl$rt_col>= -.48 & !is.na(structured_ssl$rt_col),]


# Calculate structured mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
domain <- NULL
modality <- NULL
condition <- NULL
type <- NULL
task<- NULL
sssl_range <- NULL
upper_bound <- NULL
lower_bound <- NULL

# For each participant, extract id
# Assign domain, type, and modality
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in list_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "auditory")
  task <- append(task, "ssl")
  type <- append (type, "structured")
  modality <- append (modality, "linguistic")
  mean_rt <- append(mean_rt, round(mean(structured_ssl$rt_col[structured_ssl$id==id]),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(structured_ssl$rt_col[structured_ssl$id==id]~structured_ssl$reindex[structured_ssl$id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (structured_ssl[ which(structured_ssl$id==id),])
  this_range<- range(data_this_id$rt_col, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

sssl<- NULL

# Combine data for each participant
sssl <- data.frame(part_id, task, domain,type,modality,mean_rt, upper_bound, lower_bound, rt_slope)

# for internal checking only: find mean rt_slope. This should be negative.
# TO DO: It's not negative. Why?
mean_sssl_rt_slope <- mean (sssl$rt_slope)




# ******************** III. EXTRACT RELEVANT VISUAL DATA (ID, MODALITY, DOMAIN, TYPE, RT_SLOPE, MEAN_RT) BY CONDITION, THEN COMBINE 4 CONDITIONS TOGETHER**************************


# ******************** CONDITION 1: STRUCTURED_LSL*******************


# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ following stimulus ---------------------------------------------

# Set up variables to loop through participants by trials and track the target
rt_col <- NULL
id <- NULL
trial <-NULL
target <- NULL

# Identify the rows when this condition's target was presented
structured_lsl_targets <- which(tolower(visual_data_frame$ltarget)==tolower(visual_data_frame$stimulus))
structured_lsl_targets <- which((visual_data_frame$condition)=="S")

# Isolate participants' response times.
# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in structured_lsl_targets) {
  # Isolate the ID number
  id <- append(id, paste(visual_data_frame[i,]$part_id))
  # If the participant responded while the target was presented
  if (!is.na(visual_data_frame[i,] [,"lsl_rt"])){
    # Count their response time from the target stimulus
    rt_col <- append (rt_col, visual_data_frame[i,][,"lsl_rt"])}
  # Check if you are looking at the first target, which is always the first stimulus. If so, it does not have a preceeding target
  else if (i>0){ 
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, visual_data_frame[i,][,"lsl_rt"])}    
  # If the participant responded during the stimulus preceding the target
  else if (!is.na(visual_data_frame[i-1,] [,"lsl_rt"])){
    # Count their response time as how much sooner they responded than when the stimulus was presented
    rt_col[match(i, structured_lsl_targets)] <- 0-visual_data_frame[i-1,][,"lsl_rt"]}
  # If the participant did not respond to the target within the correct time frame
  else if (is.na(visual_data_frame[i,] [,"lsl_rt"])){
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, visual_data_frame[i,][,"lsl_rt"])}
}

# Match id and response times
structured_lsl_extracted <- data.frame(id,rt_col)


# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
list_part_id <- unique(structured_lsl_extracted$id)

# Find the number of targets shown to each participant
a <- NULL
# TO DO: This should be the same for everyone, right? Test and see what's wrong.
for(i in list_part_id){a <- append(a,sum(structured_lsl_extracted$id==i))}

# For each participant, index the targets
reindex <- NULL
for (i in a) {reindex <- append (reindex, rep(1:i, 1))}

# Add the targets' indices
structured_lsl_extracted$reindex <- reindex

# Remove targets with no response time
structured_lsl_extracted <- structured_lsl_extracted[!is.na(structured_lsl_extracted$rt_col),]


# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
domain <- NULL
modality <- NULL
type <- NULL
task <- NULL
RLSL<- NULL
slsl_range <- NULL
upper_bound <- NULL
lower_bound <- NULL

# For each participant, extract id
# Assign domain, type, and modality
# Calculate and record mean_rt and rt_slope
for(id in list_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "visual")
  type <- append (type, "structured")
  modality <- append (modality, "linguistic")
  task <- append (task, "LSL")
  mean_rt <- append(mean_rt, round(mean(structured_lsl_extracted$rt_col[structured_lsl_extracted$id==id]),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(structured_lsl_extracted$rt_col[structured_lsl_extracted$id==id]~structured_lsl_extracted$reindex[structured_lsl_extracted$id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (structured_lsl_extracted[ which(structured_lsl_extracted$id==id),])
  this_range<- range(data_this_id$rt_col, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
SLSL <- data.frame(part_id, task, domain,type,modality,mean_rt, upper_bound, lower_bound, rt_slope)

# for internal checking only: find mean rt_slope
# mean_slsl_rt_slope <- mean (SLSL$rt_slope)

# TO DO: Repeat this for everything
# slsl_test<-count(structured_lsl_extracted, "id")
# slsl_test$task<- "structured lsl"

# ******************** CONDITION 2: STRUCTURED_VSL*******************


# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ following stimulus ---------------------------------------------

# Set up variables to loop through participants by trials and track the target
rt_col <- NULL
id <- NULL
trial <-NULL
target <- NULL

# Identify the rows when this condition's target was presented
structured_vsl_targets <- which(tolower(visual_data_frame$vtarget)==tolower(visual_data_frame$stimulus))
structured_vsl_targets <- which((visual_data_frame$condition)=="S")


# Isolate participants' response times.
# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in structured_vsl_targets) {
  # Isolate the ID number
  id <- append(id, paste(visual_data_frame[i,]$part_id))
  # If the participant responded while the target was presented
  if (!is.na(visual_data_frame[i,] [,"vsl_rt"])){
    # Count their response time from the target stimulus
    rt_col <- append (rt_col, visual_data_frame[i,][,"vsl_rt"])}
  # Check if you are looking at the first target, which is always the first stimulus. If so, it does not have a preceeding target
  else if (i>0){ 
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, visual_data_frame[i,][,"vsl_rt"])}    
  # If the participant responded during the stimulus preceding the target
  else if (!is.na(visual_data_frame[i-1,] [,"vsl_rt"])){
    # Count their response time as how much sooner they responded than when the stimulus was presented
    rt_col[match(i, structured_vsl_targets)] <- 0-visual_data_frame[i-1,][,"vsl_rt"]}
  # If the participant did not respond to the target within the correct time frame
  else if (is.na(visual_data_frame[i,] [,"vsl_rt"])){
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, visual_data_frame[i,][,"vsl_rt"])}
}

# Match id and response times
structured_vsl_extracted <- data.frame(id,rt_col)

# For internal checking only: Make sure that there are 24 per participant

#svsl_test<-count(structured_vsl_extracted, "id")
#svsl_test$task<- "structured vsl"

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
list_part_id <- unique(structured_vsl_extracted$id)

# Find the number of targets shown to each participant
a <- NULL
for(i in list_part_id){a <- append(a,sum(structured_vsl_extracted$id==i))}

# For each participant, index the targets
reindex <- NULL
for (i in a) {reindex <- append (reindex, rep(1:i, 1))}

# Add the targets' indices
structured_vsl_extracted$reindex <- reindex

# Remove targets with no response time
structured_vsl_extracted <- structured_vsl_extracted[!is.na(structured_vsl_extracted$rt_col),]


# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
domain <- NULL
task<- NULL
modality <- NULL
type <- NULL
SVSL<- NULL
svsl_range <- NULL
upper_bound <- NULL
lower_bound <- NULL

# For each participant, extract id
# Assign domain, type, and modality
# Calculate and record mean_rt and rt_slope
for(id in list_part_id){
  task <- append (task, "VSL")
  part_id <- append(part_id, id)
  domain <- append(domain, "visual")
  type <- append (type, "structured")
  modality <- append (modality, "non-linguistic")
  mean_rt <- append(mean_rt, round(mean(structured_vsl_extracted$rt_col[structured_vsl_extracted$id==id]),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(structured_vsl_extracted$rt_col[structured_vsl_extracted$id==id]~structured_vsl_extracted$reindex[structured_vsl_extracted$id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (structured_vsl_extracted[ which(structured_vsl_extracted$id==id),])
  this_range<- range(data_this_id$rt_col, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
SVSL <- data.frame(part_id,task, domain,type,modality,mean_rt,upper_bound, lower_bound,rt_slope)

# for internal checking only: find mean rt_slope
mean_svsl_rt_slope <- mean (SVSL$rt_slope)
#svsl_test<-count(structured_vsl_extracted, "id")
#svsl_test$task<-"structured vsl"


# ******************** CONDITION 3: RANDOM_LSL*******************


# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ following stimulus ---------------------------------------------

# Set up variables to loop through participants by trials and track the target
rt_col <- NULL
id <- NULL
trial <-NULL
target <- NULL

# Identify the rows when this condition's target was presented
random_lsl_targets <- which(tolower(visual_data_frame$ltarget)==tolower(visual_data_frame$image))
random_lsl_targets <- which((visual_data_frame$condition)=="R")


# Isolate participants' response times.
# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in random_lsl_targets) {
  # Isolate the ID number
  id <- append(id, paste(visual_data_frame[i,]$part_id))
  # If the participant responded while the target was presented
  if (!is.na(visual_data_frame[i,] [,"lsl_rt"])){
    # Count their response time from the target stimulus
    rt_col <- append (rt_col, visual_data_frame[i,][,"lsl_rt"])}
  # Check if you are looking at the first target, which is always the first stimulus. If so, it does not have a preceeding target
  else if (i>0){ 
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, visual_data_frame[i,][,"lsl_rt"])}    
  # If the participant responded during the stimulus preceding the target
  else if (!is.na(visual_data_frame[i-1,] [,"lsl_rt"])){
    # Count their response time as how much sooner they responded than when the stimulus was presented
    rt_col[match(i, random_lsl_targets)] <- 0-visual_data_frame[i-1,][,"lsl_rt"]}
  # If the participant did not respond to the target within the correct time frame
  else if (is.na(visual_data_frame[i,] [,"lsl_rt"])){
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, visual_data_frame[i,][,"lsl_rt"])}
}

# Match id and response times
random_lsl_extracted <- data.frame(id,rt_col)

# For internal checking only: Make sure that there are 24 per participant

#rlsl_test<-count(random_lsl_extracted, "id")
#rlsl_test$task<- "random lsl"

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
list_part_id <- unique(random_lsl_extracted$id)

# Find the number of targets shown to each participant
a <- NULL
for(i in list_part_id){a <- append(a,sum(random_lsl_extracted$id==i))}

# For each participant, index the targets
reindex <- NULL
for (i in a) {reindex <- append (reindex, rep(1:i, 1))}

# Add the targets' indices
random_lsl_extracted$reindex <- reindex

# Remove targets with no response time
random_lsl_extracted <- random_lsl_extracted[!is.na(random_lsl_extracted$rt_col),]


# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
domain <- NULL
modality <- NULL
type <- NULL
task <- NULL
RLSL<- NULL
rlsl_range <- NULL
upper_bound <- NULL
lower_bound <- NULL

# For each participant, extract id
# Assign domain, type, and modality
# Calculate and record mean_rt and rt_slope
for(id in list_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "visual")
  type <- append (type, "random")
  modality <- append (modality, "linguistic")
  mean_rt <- append(mean_rt, round(mean(random_lsl_extracted$rt_col[random_lsl_extracted$id==id]),digits=3))
  task <- append (task, "LSL")
  rt_slope <- append (rt_slope, round(summary(lm(random_lsl_extracted$rt_col[random_lsl_extracted$id==id]~random_lsl_extracted$reindex[random_lsl_extracted$id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (random_lsl_extracted[ which(random_lsl_extracted$id==id),])
  rlsl_range<- range(data_this_id$rt_col, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
  
}

# Combine data for each participant
RLSL <- data.frame(part_id,task, domain,type,modality,mean_rt, upper_bound, lower_bound, rt_slope)


# for internal checking only: find mean rt_slope
mean_rlsl_rt_slope <- mean (RLSL$rt_slope)


# ******************** CONDITION 4: RANDOM_VSL*******************


# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ following stimulus ---------------------------------------------

# Set up variables to loop through participants by trials and track the target
rt_col <- NULL
id <- NULL
trial <-NULL
target <- NULL

# Identify the rows when this condition's target was presented
random_vsl_targets <- which(tolower(visual_data_frame$vtarget)==tolower(visual_data_frame$image))
random_vsl_targets <- which((visual_data_frame$condition)=="R")


# Isolate participants' response times.
# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in random_vsl_targets) {
  # Isolate the ID number
  id <- append(id, paste(visual_data_frame[i,]$part_id))
  # If the participant responded while the target was presented
  if (!is.na(visual_data_frame[i,] [,"vsl_rt"])){
    # Count their response time from the target stimulus
    rt_col <- append (rt_col, visual_data_frame[i,][,"vsl_rt"])}
  # Check if you are looking at the first target, which is always the first stimulus. If so, it does not have a preceeding target
  else if (i>0){ 
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, visual_data_frame[i,][,"vsl_rt"])}    
  # If the participant responded during the stimulus preceding the target
  else if (!is.na(visual_data_frame[i-1,] [,"vsl_rt"])){
    # Count their response time as how much sooner they responded than when the stimulus was presented
    rt_col[match(i, random_vsl_targets)] <- 0-visual_data_frame[i-1,][,"vsl_rt"]}
  # If the participant did not respond to the target within the correct time frame
  else if (is.na(visual_data_frame[i,] [,"vsl_rt"])){
    # Count their response time from the target stimulus (NA)
    rt_col <- append (rt_col, visual_data_frame[i,][,"vsl_rt"])}
}

# Match id and response times
random_vsl_extracted <- data.frame(id,rt_col)


# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
list_part_id <- unique(random_vsl_extracted$id)

# Find the number of targets shown to each participant
a <- NULL
for(i in list_part_id){a <- append(a,sum(random_vsl_extracted$id==i))}

# For each participant, index the targets
reindex <- NULL
for (i in a) {reindex <- append (reindex, rep(1:i, 1))}

# Add the targets' indices
random_vsl_extracted$reindex <- reindex

# Remove targets with no response time
random_vsl_extracted <- random_vsl_extracted[!is.na(random_vsl_extracted$rt_col),]


# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
domain <- NULL
modality <- NULL
type <- NULL
task <-NULL
RVSL <- NULL
rvsl_range <- NULL
upper_bound <- NULL
lower_bound <- NULL

# For each participant, extract id
# Assign domain, type, and modality
# Calculate and record mean_rt and rt_slope
for(id in list_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "visual")
  type <- append (type, "random")
  task <- append (task, "VSL")
  modality <- append (modality, "non-linguistic")
  mean_rt <- append(mean_rt, round(mean(random_vsl_extracted$rt_col[random_vsl_extracted$id==id]),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(random_vsl_extracted$rt_col[random_vsl_extracted$id==id]~random_vsl_extracted$reindex[random_vsl_extracted$id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (random_vsl_extracted[ which(random_vsl_extracted$id==id),])
  this_range<- range(data_this_id$rt_col, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
RVSL <- data.frame(part_id, task, domain, type,modality,mean_rt,upper_bound, lower_bound, rt_slope)

# for internal checking only: find mean rt_slope
mean_rvsl_rt_slope <- mean (RVSL$rt_slope)


# Bind conditions together--------------------------------------------------------------------------------------------------------------------------------------------------

# Bind visual cconditions
visual_rt<- rbind(RLSL, RVSL, SLSL, SVSL)
# Bind auditory conditions
auditory_rt<- rbind(rtsl, rssl, stsl, sssl)
# Bind all conditions
indiv_rt <- rbind(auditory_rt, visual_rt)

write.csv(indiv_rt, "blast_adult_scan_rt_slope_indiv.csv")

