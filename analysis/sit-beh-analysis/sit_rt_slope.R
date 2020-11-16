#  SIT Reaction Time Analysis
#  Violet Kozloff
#  Adapted from extraction files produced by An Nguyen
#  Last modified Nov 15, 2020
#  This script extracts mean reaction time and reaction time slope for statistical learning tasks involving structured and random triplets of letters and images
#  NOTE: relevant columns have been pre-selected through sit_cleaning.R
#  NOTE: Excludes any trials where participant responded to less than 50% of the targets (or responded to a different image than the target)

# ******************** I. PREPARE FILES *************************


# Prepare workspace ------------------------------------------------------------------------------------------------------


#Install packages if they aren't already installed
if (!("reshape" %in% installed.packages())) install.packages("reshape")
if (!("tidyverse" %in% installed.packages())) install.packages("tidyverse")
if (!("corrplot" %in% installed.packages())) install.packages("corrplot")
if (!("here" %in% installed.packages())) install.packages("here")

require(reshape)
require(tidyverse)
require(corrplot)
require(here)

# Remove objects in environment
rm(list=ls())

# Detect OS
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

os <- get_os()

# Set directory based on OS
ifelse (os == "osx", setwd("/Volumes/data/projects/completed_projects/sit/analysis/data/clean/vocab_clean/"), setwd("Z:/projects/completed_projects/sit/analysis/data/clean/vocab_clean"))


# Read in picture vocabulary scores --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
picture_vocab <- read.csv("vocab_clean.csv")

# Read in ll files and combine them into one data frame -----------------------------------------------------------------------------------------------------------------------------------

# Set directory based on OS
ifelse (os == "osx", setwd("/Volumes/data/projects/completed_projects/sit/analysis/data/clean/ll_clean"), setwd("Z:/projects/completed_projects/sit/analysis/data/clean/ll_clean"))

ll_files <- list.files(pattern=("*.csv"))
ll_data <- NULL

for (file in ll_files)
{
  current_file <- read.csv(file)
  ll_data <- plyr::rbind.fill (ll_data, current_file)
}

# Read "f_not_false" as "F"
levels(ll_data$structured_targ)[levels(ll_data$structured_targ)=="f_not_false"] <- "F"
levels(ll_data$random_targ)[levels(ll_data$random_targ)=="f_not_false"] <- "F"

# Convert reaction times from milliseconds to seconds
ll_data$l_rt<-ll_data$l_rt*1000

# Remove extensions from image names
ll_data$image <- gsub (".png", "", ll_data$image, ignore.case=TRUE)
ll_data$image <- gsub (".bmp", "", ll_data$image, ignore.case=TRUE)
ll_data$random_targ <- gsub (".png", "", ll_data$random_targ, ignore.case=TRUE)
ll_data$structured_targ <- gsub (".png", "", ll_data$structured_targ, ignore.case=TRUE)
ll_data$random_targ <- gsub (".bmp", "", ll_data$random_targ, ignore.case=TRUE)
ll_data$structured_targ <- gsub (".bmp", "", ll_data$structured_targ, ignore.case=TRUE)


# Read in lv files and combine them into one data frame -----------------------------------------------------------------------------------------------------------------------------------

# Set directory based on OS
ifelse (os == "osx", setwd("/Volumes/data/projects/completed_projects/sit/analysis/data/clean/lv_clean"), setwd("Z:/projects/completed_projects/sit/analysis/data/clean/lv_clean"))

lv_files <- list.files(pattern=("*.csv"))
lv_data <- NULL

for (file in lv_files)
{
  current_file <- read.csv(file)
  lv_data <- plyr::rbind.fill (lv_data, current_file)
}

# Read "f_not_false" as "F"
levels(lv_data$structured_targ)[levels(lv_data$structured_targ)=="f_not_false"] <- "F"
levels(lv_data$random_targ)[levels(lv_data$random_targ)=="f_not_false"] <- "F"

# Convert reaction times from milliseconds to seconds
lv_data$l_rt <- lv_data$l_rt*1000
lv_data$v_rt <- lv_data$v_rt*1000

# Remove extensions from image names
lv_data$image <- gsub (".png", "", lv_data$image, ignore.case=TRUE)
lv_data$image <- gsub (".bmp", "", lv_data$image, ignore.case=TRUE)
lv_data$random_targ <- gsub (".png", "", lv_data$random_targ, ignore.case=TRUE)
lv_data$structured_targ <- gsub (".png", "", lv_data$structured_targ, ignore.case=TRUE)
lv_data$random_targ <- gsub (".bmp", "", lv_data$random_targ, ignore.case=TRUE)
lv_data$structured_targ <- gsub (".bmp", "", lv_data$structured_targ, ignore.case=TRUE)


# Read in vl files and combine them into one data frame -----------------------------------------------------------------------------------------------------------------------------------

# Set directory based on OS
ifelse (os == "osx", setwd("/Volumes/data/projects/completed_projects/sit/analysis/data/clean/vl_clean"), setwd("Z:/projects/completed_projects/sit/analysis/data/clean/vl_clean"))
vl_files <- list.files(pattern=("*.csv"))
vl_data <- NULL

for (file in vl_files)
{
  current_file <- read.csv(file)
  vl_data <- plyr::rbind.fill (vl_data, current_file)
}

# Read "f_not_false" as "F"
levels(vl_data$structured_targ)[levels(vl_data$structured_targ)=="f_not_false"] <- "F"
levels(vl_data$random_targ)[levels(vl_data$random_targ)=="f_not_false"] <- "F"

# Convert reaction times from milliseconds to seconds
vl_data$l_rt <- vl_data$l_rt*1000
vl_data$v_rt <- vl_data$v_rt*1000

# Remove extensions from image names
vl_data$image <- gsub (".png", "", vl_data$image, ignore.case=TRUE)
vl_data$image <- gsub (".bmp", "", vl_data$image, ignore.case=TRUE)
vl_data$random_targ <- gsub (".png", "", vl_data$random_targ, ignore.case=TRUE)
vl_data$structured_targ <- gsub (".png", "", vl_data$structured_targ, ignore.case=TRUE)
vl_data$random_targ <- gsub (".bmp", "", vl_data$random_targ, ignore.case=TRUE)
vl_data$structured_targ <- gsub (".bmp", "", vl_data$structured_targ, ignore.case=TRUE)


# Read in vv files and combine them into one data frame -----------------------------------------------------------------------------------------------------------------------------------

# Set directory based on OS
ifelse (os == "osx", setwd("/Volumes/data/projects/completed_projects/sit/analysis/data/clean/vv_clean"), setwd("Z:/projects/completed_projects/sit/analysis/data/clean/vv_clean"))

vv_files <- list.files(pattern=("*.csv"))
vv_data <- NULL

for (file in vv_files)
{
  current_file <- read.csv(file)
  vv_data <- plyr::rbind.fill (vv_data, current_file)
}

# Convert response times to milliseconds
vv_data$v_rt <- vv_data$v_rt*1000

# Remove extensions from image names
vv_data$image <- gsub (".png", "", vv_data$image, ignore.case=TRUE)
vv_data$image <- gsub (".bmp", "", vv_data$image, ignore.case=TRUE)
vv_data$random_targ <- gsub (".png", "", vv_data$random_targ, ignore.case=TRUE)
vv_data$structured_targ <- gsub (".png", "", vv_data$structured_targ, ignore.case=TRUE)
vv_data$random_targ <- gsub (".bmp", "", vv_data$random_targ, ignore.case=TRUE)
vv_data$structured_targ <- gsub (".bmp", "", vv_data$structured_targ, ignore.case=TRUE)



# ******************** CONDITION 1: RANDOM LL*******************


# Separate random condition
random_ll <- filter(ll_data, condition== "R") %>%
  tibble::rowid_to_column("index")

## Index the targets -----------------------------------------------

# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding stimulus ---------------------------------------------

# Identify the rows when this condition's target was presented
random_ll_targets <- filter(random_ll, random_targ==image)

# Find all the participant IDs
list_part_id <- distinct(random_ll_targets, part_id)

# There should be 32 participants
nrow(list_part_id)

# Each participant should have 288 lines — if not, identify the participant
random_ll %>% 
  group_by(part_id) %>%
  summarise(n = n()) %>%
  filter (n != 288)

# Each participant should have 24 targets — if not, identify the participant
random_ll_targets %>% 
  group_by(part_id) %>%
  summarise(n = n()) %>%
  filter (n != 24)

# Identify the random target for each participant
rll_targs <- distinct(random_ll_targets %>%
  group_by(part_id) %>%
  summarise(random_targ = random_targ))

# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ following stimulus ---------------------------------------------

# Initialize variables to track participant ID, condition, modality, task, and reaction time (RT)
rll_part_id <- NULL
rll_rt <- NULL

# Track the cases for calculating each type of reaction time
# NOTE: These variables are for internal checking only and can be commented out below in case of bugs
# Case 1: The participant responds during the target, which is the first trial in a block
rll_case1 <- NULL
# Case 2: The participant responds to the trial directly following the target, and the target is the first trial in a block
rll_case2 <- NULL
# Case 3: Anticipation of target, participant responded to stimulus directly preceding target
rll_case3 <- NULL
# Case 4: Response to target during the target trial
rll_case4 <- NULL
# Case 5: Delay from target, participant responded to stimulus directly following target
rll_case5 <- NULL
# Case 6: Missed target, record NA reaction time
rll_case6 <- NULL

# Isolate participants' response times.

# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in random_ll_targets$index) {
  # Isolate the ID number
  rll_part_id <-
    append(rll_part_id, paste(random_ll[i, ]$part_id))
  
  # Check if you are looking at the first trial in the block. If so, the target does not have a preceding target
  if ((random_ll[i, ]$trial_num %% 48 == 1)
      # Check if the participant responded during the target trial
      & !is.na(random_ll[i, ]$l_rt)) {
    # If so, count the response time from the target stimulus
    rll_rt <- append (rll_rt, random_ll[i, ][, "l_rt"])
    rll_case1 <- append (rll_case1, i)
    } 
  
  # If it's the first trial and there was no target keypress
  else if (((random_ll[i, ]$trial_num %%48 == 1) & (is.na(random_ll[i,]$l_rt)))
           # Check that the following stimulus was not also a target from the same block (to avoid counting the same keypress twice)
           & !((i + 1 %in% random_ll_targets) & floor(random_ll[i + 1, ]$trial_num/48)== floor(random_ll[i,]$trial_num/48))) {
    # Then count the response time from the following stimulus
    rll_rt <- append (rll_rt, 1000 + (random_ll[i + 1, ][, "l_rt"]))
    rll_case2 <- append (rll_case2, i)
  }
  
  # Otherwise, if the participant responded during the stimulus preceding the target
  else if (!is.na(random_ll[i - 1, ] [, "l_rt"])
           # and the preceding stimulus was not also a target
           & !((random_ll[i - 1, ][, "random_targ"] == (random_ll[i - 1, ][, "image"])))
           #  and two stimuli prior was not also a target from the same block
           & ! ((random_ll[i - 2, ][, "random_targ"] == random_ll[i - 2, ][, "image"])
               & floor(random_ll[i,]$trial_num/48) == floor(random_ll[i-2,]$trial_num/48))
           # and the preceding stimulus came from the same block
           & floor((random_ll[i, ])$trial_num/48) == floor(random_ll[i - 1, ]$trial_num/48)
           # and, because this is a random block
           & 
           # EITHER the target is a standalone, with no target within the two preceding trials or two following trials, and no keypress during or directly following
           (
             (!(i - 1) %in% random_ll_targets 
             & floor(random_ll[i - 1, ]$trial_num/48) == floor(random_ll[i, ]$trial_num/48)
             & !(i - 2) %in% random_ll_targets 
             & floor(random_ll[i - 2, ]$trial_num/48) == floor(random_ll[i, ]$trial_num/48)
             & is.na(random_ll[i, ]$l_rt)
             & !(i + 1) %in% random_ll_targets 
             & floor(random_ll[i + 1, ]$trial_num/48) == floor(random_ll[i, ]$trial_num/24) 
             & is.na(random_ll[i + 1, ]$l_rt)
             & !(i + 2) %in% random_ll_targets 
             & floor(random_ll[i + 2, ]$trial_num/48) == floor(random_ll[i, ]$trial_num/48)
             )
            # OR just the following stimulus is not a target from the same block,
            | (!(i + 1) %in% random_ll_targets 
               & floor(random_ll[i + 1, ]$trial_num/48) != floor(random_ll[i, ]$trial_num/48)
               # OR the following stimulus is a target from the same block, but has neither an on-target keypress
               | ((i + 1) %in% random_ll_targets 
                  & floor(random_ll[i + 1, ]$trial_num/48) == floor(random_ll[i, ]$trial_num/48) &
                  is.na(random_ll[i + 1, ]$l_rt)
                  # nor a delay from the same block
                  & (floor(random_ll[i + 2, ]$trial_num/48) != floor(random_ll[i, ]$trial_num/48) |
                     is.na(random_ll[i + 2, ]$l_rt)
                     )
                  )
               )
            )
           )
    
    {
    # Count the response time as how much sooner they responded than when the stimulus was presented (anticipation)
    rll_rt <- append(rll_rt, (random_ll[i - 1, ][, "l_rt"] - 1000))
    rll_case3 <- append (rll_case3, i)  }
  
  # Otherwise, if the participant responded during the target
  else if (!is.na(random_ll[i, ] [, "l_rt"])
           # and, because random, the previous stimulus was not also a target with no keypress, followed by a distractor with a keypress
           & !(((i - 1) %in% random_ll_targets &
                is.na(random_ll[i - 1, ] [, "l_rt"]) &
                !(i + 1) %in% random_ll_targets &
                !is.na(random_ll[i + 1, ] [, "l_rt"])))) {
    # Count their response time as the keypress
    rll_rt <-
      append(rll_rt, (random_ll[i, ][, "l_rt"]))
    rll_case4 <- append (rll_case4, i)
  }
  
  # Otherwise, if the participant responded after the target
  else if (!is.na(random_ll[i + 1, ]$l_rt > 0)
           # And the following trial came from the same block
           & floor(random_ll[i, ]$trial_num/48) == floor(random_ll[i + 1, ]$trial_num/48)
           # Check that EITHER the following stimulus either was also not a target
           & (
             !((i + 1) %in% random_ll_targets) |
             # OR, if the following stimulus was also a target, that it also had a delay
             (((i + 1) %in% random_ll_targets) &
              !is.na(random_ll[i + 2, ]$l_rt) &
              floor(random_ll[i, ]$trial_num/48) == floor(random_ll[i + 2, ]$trial_num/48)
             )
           )
           # Also check that EITHER two stimuli following was not also a target from the same block (to avoid counting the same keypress twice)
           &
           (
             !((i + 2) %in% random_ll_targets) |
             floor(random_ll[i, ]$trial_num/48) == floor(random_ll[i + 2, ]$trial_num/48) |
             # OR, if two stimuli following was also a target (from the same block),
             ((i + 2) %in% random_ll_targets) &
             floor(random_ll[i, ]$trial_num/48) == floor(random_ll[i + 2, ]$trial_num/48)
             # that it also had a delay from the same block
             &
             !is.na(
               random_ll[i + 3, ]$l_rt &
               floor(random_ll[i, ]$trial_num/48) == floor(random_ll[i + 3, ]$trial_num/48)
             )
           )) {
    # Count their response time as how much later they responded than when the stimulus was presented
    rll_rt <-
      append(rll_rt, (1000 + random_ll[i + 1, ][, "l_rt"]))
    rll_case5 <- append (rll_case5, i)
    
    # Otherwise, record the miss with a reaction time of NA
  } else {
    rll_rt <- append(rll_rt, NA)
    rll_case6 <- append (rll_case6, i)
  }
}

# Match id and response times
random_ll_extracted <- data.frame(rll_part_id, rll_rt)

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
extracted_part_id <- unique(random_ll_extracted$rll_part_id)

# Find the number of targets shown to each participant
target_sum <- NULL
for(i in extracted_part_id){target_sum <- append(target_sum,sum(random_ll_extracted$rll_part_id==i))}

# TEST: This should be equal to 32
length (target_sum)

# TEST: This should contain a vector full of 24s
target_sum

# For each participant, index the targets
targ_index <- NULL
for (i in target_sum) {targ_index <- append (targ_index, rep(1:i, 1))}

# Add the targets' indices
random_ll_extracted$targ_index <- targ_index

# Remove any values of NA
random_ll_extracted <- random_ll_extracted[!is.na(random_ll_extracted$rll_rt),]

# Add the target 
random_ll_extracted <- left_join(random_ll_extracted, rll_targs, by = c("rll_part_id" = "part_id"))


# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

#There are 24 targets for each participant. Some may have a low hit rate (responded to 12 targets or less)
low_hits<-NULL
hits<-NULL
# Find people with a low hit rate
for (id in extracted_part_id){
  if (length(random_ll_extracted[which(random_ll_extracted$rll_part_id==id & !is.na(random_ll_extracted$rll_rt)),]$rll_rt)<13)
  {low_hits<-append(low_hits, id)
  hits<-append(hits, length(random_ll_extracted[which(random_ll_extracted$rll_part_id==id),]$rll_part_id))
  }
}

# Remove people with low hit rate
random_ll_extracted <- random_ll_extracted[! random_ll_extracted$rll_part_id %in% low_hits, ]
# Find only participants with over 50% hit rate
extracted_part_id <- unique(random_ll_extracted$rll_part_id)

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
type <- NULL
task<- NULL
rll<- NULL
domain <- NULL
range <- NULL
upper_bound <- NULL
lower_bound <- NULL
this_range <- NULL
same_or_diff <- NULL
test_phase <- NULL
number_rts <- NULL

# For each participant, extract id
# Assign domain and type
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in extracted_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "linguistic")
  task <- append(task, "ll")
  type <- append (type, "random")
  same_or_diff <- append (same_or_diff, "same")
  test_phase <- append (test_phase, "lsl")
  number_rts <- append(number_rts, length(!is.na(random_ll_extracted$rll_rt[random_ll_extracted$rll_part_id==id])))
  mean_rt <- append(mean_rt, round(mean(random_ll_extracted$rll_rt[random_ll_extracted$rll_part_id==id], na.rm = TRUE),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(random_ll_extracted$rll_rt[random_ll_extracted$rll_part_id==id]~random_ll_extracted$targ_index[random_ll_extracted$rll_part_id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (random_ll_extracted[ which(random_ll_extracted$rll_part_id==id),])
  this_range<- range(data_this_id$rll_rt, na.rm = TRUE)
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
  range <- append (range, (this_range[2]-this_range[1]))
}

length(number_rts)
mean(number_rts)
sd(number_rts)

# Combine data for each participant
rll <- data.frame(part_id, task, same_or_diff, test_phase, domain, type, mean_rt, range, upper_bound, lower_bound, rt_slope) 

# ******************** CONDITION 2: RANDOM LV *******************


# Separate random and structured conditions
random_lv <- lv_data[ which(lv_data$condition== "R"),]

# Index the rows
random_lv <- tibble::rowid_to_column(random_lv, "index")


# Index the targets -----------------------------------------------

# Identify the rows when this condition's target was presented
random_lv_targets <- random_lv[which(random_lv$random_targ==random_lv$image),]

# TEST: Create a data frame to check the number of lines per participant
list_part_id <- unique(random_lv_targets$part_id)
part_id <- NULL
total_lines <- NULL
for(id in list_part_id){
  part_id <- append(part_id, id)
  total_lines <- append(total_lines, nrow(random_lv_targets[which(random_lv$part_id==id),]))
}
rlv_line_number <- data.frame(part_id, total_lines)
# TEST: There should be 32 entries
length(rlv_line_number$part_id)
# TEST: They should all contain 288 lines
rlv_line_number$total_lines

# Identify the random target for each participant
rlv_targs <- distinct(random_lv_targets %>%
                        group_by(part_id) %>%
                        summarise(random_targ = random_targ))

# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ following stimulus ---------------------------------------------

# Initialize variables to track participant ID, condition, modality, task, and reaction time (RT)
rlv_part_id <- NULL
rlv_rt <- NULL

# Track the cases for calculating each type of reaction time
# NOTE: These variables are for internal checking only and can be commented out below in case of bugs
# Case 1: The participant responds during the target, which is the first trial in a block
rlv_case1 <- NULL
# Case 2: The participant responds to the trial directly folvowing the target, and the target is the first trial in a block
rlv_case2 <- NULL
# Case 3: Anticipation of target, participant responded to stimulus directly preceding target
rlv_case3 <- NULL
# Case 4: Response to target during the target trial
rlv_case4 <- NULL
# Case 5: Delay from target, participant responded to stimulus directly folvowing target
rlv_case5 <- NULL
# Case 6: Missed target, record NA reaction time
rlv_case6 <- NULL

# Isolate participants' response times.

# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in random_lv_targets$index) {
  # Isolate the ID number
  rlv_part_id <-
    append(rlv_part_id, paste(random_lv[i, ]$part_id))
  
  # Check if you are looking at the first trial in the block. If so, the target does not have a preceding target
  if ((random_lv[i, ]$trialnum %% 48 == 1)
      # Check if the participant responded during the target trial
      & !is.na(random_lv[i, ]$v_rt)) {
    # If so, count the response time from the target stimulus
    rlv_rt <- append (rlv_rt, random_lv[i, ][, "v_rt"])
    rlv_case1 <- append (rlv_case1, i)
  } 
  
  # If it's the first trial and there was no target keypress
  else if (((random_lv[i, ]$trialnum %%48 == 1) & (is.na(random_lv[i,]$v_rt)))
           # Check that the folvowing stimulus was not also a target from the same block (to avoid counting the same keypress twice)
           & !((i + 1 %in% random_lv_targets) & floor(random_lv[i + 1, ]$trialnum/48)== floor(random_lv[i,]$trialnum/48))) {
    # Then count the response time from the folvowing stimulus
    rlv_rt <- append (rlv_rt, 1000 + (random_lv[i + 1, ][, "v_rt"]))
    rlv_case2 <- append (rlv_case2, i)
  }
  
  # Otherwise, if the participant responded during the stimulus preceding the target
  else if (!is.na(random_lv[i - 1, ] [, "v_rt"])
           # and the preceding stimulus was not also a target
           & !((random_lv[i - 1, ][, "random_targ"] == (random_lv[i - 1, ][, "image"])))
           #  and two stimuli prior was not also a target from the same block
           & ! ((random_lv[i - 2, ][, "random_targ"] == random_lv[i - 2, ][, "image"])
                & floor(random_lv[i,]$trialnum/48) == floor(random_lv[i-2,]$trialnum/48))
           # and the preceding stimulus came from the same block
           & floor((random_lv[i, ])$trialnum/48) == floor(random_lv[i - 1, ]$trialnum/48)
           # and, because this is a random block
           & 
           # EITHER the target is a standalone, with no target within the two preceding trials or two folvowing trials, and no keypress during or directly folvowing
           (
             (!(i - 1) %in% random_lv_targets 
              & floor(random_lv[i - 1, ]$trialnum/48) == floor(random_lv[i, ]$trialnum/48)
              & !(i - 2) %in% random_lv_targets 
              & floor(random_lv[i - 2, ]$trialnum/48) == floor(random_lv[i, ]$trialnum/48)
              & is.na(random_lv[i, ]$v_rt)
              & !(i + 1) %in% random_lv_targets 
              & floor(random_lv[i + 1, ]$trialnum/48) == floor(random_lv[i, ]$trialnum/24) 
              & is.na(random_lv[i + 1, ]$v_rt)
              & !(i + 2) %in% random_lv_targets 
              & floor(random_lv[i + 2, ]$trialnum/48) == floor(random_lv[i, ]$trialnum/48)
             )
             # OR just the folvowing stimulus is not a target from the same block,
             | (!(i + 1) %in% random_lv_targets 
                & floor(random_lv[i + 1, ]$trialnum/48) != floor(random_lv[i, ]$trialnum/48)
                # OR the folvowing stimulus is a target from the same block, but has neither an on-target keypress
                | ((i + 1) %in% random_lv_targets 
                   & floor(random_lv[i + 1, ]$trialnum/48) == floor(random_lv[i, ]$trialnum/48) &
                   is.na(random_lv[i + 1, ]$v_rt)
                   # nor a delay from the same block
                   & (floor(random_lv[i + 2, ]$trialnum/48) != floor(random_lv[i, ]$trialnum/48) |
                      is.na(random_lv[i + 2, ]$v_rt)
                   )
                )
             )
           )
  )
  
  {
    # Count the response time as how much sooner they responded than when the stimulus was presented (anticipation)
    rlv_rt <- append(rlv_rt, (random_lv[i - 1, ][, "v_rt"] - 1000))
    rlv_case3 <- append (rlv_case3, i)  }
  
  # Otherwise, if the participant responded during the target
  else if (!is.na(random_lv[i, ] [, "v_rt"])
           # and, because random, the previous stimulus was not also a target with no keypress, folvowed by a distractor with a keypress
           & !(((i - 1) %in% random_lv_targets &
                is.na(random_lv[i - 1, ] [, "v_rt"]) &
                !(i + 1) %in% random_lv_targets &
                !is.na(random_lv[i + 1, ] [, "v_rt"])))) {
    # Count their response time as the keypress
    rlv_rt <-
      append(rlv_rt, (random_lv[i, ][, "v_rt"]))
    rlv_case4 <- append (rlv_case4, i)
  }
  
  # Otherwise, if the participant responded after the target
  else if (!is.na(random_lv[i + 1, ]$v_rt > 0)
           # And the folvowing trial came from the same block
           & floor(random_lv[i, ]$trialnum/48) == floor(random_lv[i + 1, ]$trialnum/48)
           # Check that EITHER the folvowing stimulus either was also not a target
           & (
             !((i + 1) %in% random_lv_targets) |
             # OR, if the folvowing stimulus was also a target, that it also had a delay
             (((i + 1) %in% random_lv_targets) &
              !is.na(random_lv[i + 2, ]$v_rt) &
              floor(random_lv[i, ]$trialnum/48) == floor(random_lv[i + 2, ]$trialnum/48)
             )
           )
           # Also check that EITHER two stimuli folvowing was not also a target from the same block (to avoid counting the same keypress twice)
           &
           (
             !((i + 2) %in% random_lv_targets) |
             floor(random_lv[i, ]$trialnum/48) == floor(random_lv[i + 2, ]$trialnum/48) |
             # OR, if two stimuli folvowing was also a target (from the same block),
             ((i + 2) %in% random_lv_targets) &
             floor(random_lv[i, ]$trialnum/48) == floor(random_lv[i + 2, ]$trialnum/48)
             # that it also had a delay from the same block
             &
             !is.na(
               random_lv[i + 3, ]$v_rt &
               floor(random_lv[i, ]$trialnum/48) == floor(random_lv[i + 3, ]$trialnum/48)
             )
           )) {
    # Count their response time as how much later they responded than when the stimulus was presented
    rlv_rt <-
      append(rlv_rt, (1000 + random_lv[i + 1, ][, "v_rt"]))
    rlv_case5 <- append (rlv_case5, i)
    
    # Otherwise, record the miss with a reaction time of NA
  } else {
    rlv_rt <- append(rlv_rt, NA)
    rlv_case6 <- append (rlv_case6, i)
  }
}

# Match id and response times
random_lv_extracted <- data.frame(rlv_part_id, rlv_rt)
# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
extracted_part_id <- unique(random_lv_extracted$rlv_part_id)

# Find the number of targets shown to each participant
target_sum <- NULL
for(i in extracted_part_id){target_sum <- append(target_sum,sum(random_lv_extracted$rlv_part_id==i))}

# For each participant, index the targets
targ_index <- NULL
for (i in target_sum) {targ_index <- append (targ_index, rep(1:i, 1))}

# TEST: This should be equal to 32
length (target_sum)
# TEST: This should contain a vector full of 24s
target_sum

# Add the targets' indices
random_lv_extracted$targ_index <- targ_index

# Remove any values of NA
random_lv_extracted <- random_lv_extracted[!is.na(random_lv_extracted$rlv_rt),]

# Add the target 
random_lv_extracted <- left_join(random_lv_extracted, rlv_targs, by = c("rlv_part_id" = "part_id"))

# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

#There are 24 targets for each participant. Some may have a low hit rate (responded to 12 targets or less)
low_hits<-NULL
# Find people with a low hit rate
for (id in extracted_part_id){
  if (length(random_lv_extracted[which(random_lv_extracted$rlv_part_id==id),]$rlv_rt)<13)
  {low_hits<-append(low_hits, id)}
}
# Remove people with low hit rate
random_lv_extracted <- random_lv_extracted[! random_lv_extracted$rlv_part_id %in% low_hits, ]
# Find only participants with over 50% hit rate
extracted_part_id <- unique(random_lv_extracted$rlv_part_id)

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
type <- NULL
task<- NULL
rlv<- NULL
domain <- NULL
range <- NULL
upper_bound <- NULL
lower_bound <- NULL
this_range <- NULL
same_or_diff <- NULL
test_phase <- NULL
number_rts <- NULL

# For each participant, extract id
# Assign domain and type
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in extracted_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "non-linguistic")
  task <- append(task, "lv")
  type <- append (type, "random")
  same_or_diff <- append (same_or_diff, "different")
  test_phase <- append (test_phase, "lsl")
  mean_rt <- append(mean_rt, round(mean(random_lv_extracted$rlv_rt[random_lv_extracted$rlv_part_id==id]),digits=3))
  number_rts <- append(number_rts, length(!is.na(random_lv_extracted$rlv_rt[random_lv_extracted$rlv_part_id==id])))
  rt_slope <- append (rt_slope, round(summary(lm(random_lv_extracted$rlv_rt[random_lv_extracted$rlv_part_id==id]~random_lv_extracted$targ_index[random_lv_extracted$rlv_part_id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (random_lv_extracted[ which(random_lv_extracted$rlv_part_id==id),])
  this_range<- range(data_this_id$rlv_rt, na.rm = TRUE)
  range <- append (range, (this_range[2]-this_range[1]))
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
rlv <- data.frame(part_id, task, same_or_diff, test_phase, domain,type, mean_rt, range, upper_bound, lower_bound, rt_slope)

number_rts
mean(number_rts)
sd(number_rts)




# ******************** CONDITION 3: RANDOM VL*******************

# Separate random and structured conditions
random_vl <- vl_data[ which(vl_data$condition== "R"),]

# Index the rows
random_vl <- tibble::rowid_to_column(random_vl, "index")


# Index the targets -----------------------------------------------

# Identify the rows when this condition's target was presented

random_vl_targets <- random_vl[which(random_vl$random_targ==random_vl$image),]

# TEST: Create a data frame to check the number of lines per participant
list_part_id <- unique(random_vl_targets$part_id)
part_id <- NULL
total_lines <- NULL
for(id in list_part_id){
  part_id <- append(part_id, id)
  total_lines <- append(total_lines, nrow(random_vl_targets[which(random_vl$part_id==id),]))
}
rvl_line_number <- data.frame(part_id, total_lines)
# There should be 32 entries
length(rvl_line_number$part_id)
# They should all contain 288 lines
rvl_line_number$total_lines

# Identify the random target for each participant
rvl_targs <- distinct(random_vl_targets %>%
                        group_by(part_id) %>%
                        summarise(random_targ = random_targ))

# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ fovlowing stimulus ---------------------------------------------
# Initialize variables to track participant ID, condition, modality, task, and reaction time (RT)
rvl_part_id <- NULL
rvl_rt <- NULL

# Track the cases for calculating each type of reaction time
# NOTE: These variables are for internal checking only and can be commented out below in case of bugs
# Case 1: The participant responds during the target, which is the first trial in a block
rvl_case1 <- NULL
# Case 2: The participant responds to the trial directly fovlowing the target, and the target is the first trial in a block
rvl_case2 <- NULL
# Case 3: Anticipation of target, participant responded to stimulus directly preceding target
rvl_case3 <- NULL
# Case 4: Response to target during the target trial
rvl_case4 <- NULL
# Case 5: Delay from target, participant responded to stimulus directly fovlowing target
rvl_case5 <- NULL
# Case 6: Missed target, record NA reaction time
rvl_case6 <- NULL

# Isolate participants' response times.

# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in random_vl_targets$index) {
  # Isolate the ID number
  rvl_part_id <-
    append(rvl_part_id, paste(random_vl[i, ]$part_id))
  
  # Check if you are looking at the first trial in the block. If so, the target does not have a preceding target
  if ((random_vl[i, ]$trial_num %% 48 == 1)
      # Check if the participant responded during the target trial
      & !is.na(random_vl[i, ]$l_rt)) {
    # If so, count the response time from the target stimulus
    rvl_rt <- append (rvl_rt, random_vl[i, ][, "l_rt"])
    rvl_case1 <- append (rvl_case1, i)
  } 
  
  # If it's the first trial and there was no target keypress
  else if (((random_vl[i, ]$trial_num %%48 == 1) & (is.na(random_vl[i,]$l_rt)))
           # Check that the fovlowing stimulus was not also a target from the same block (to avoid counting the same keypress twice)
           & !((i + 1 %in% random_vl_targets) & floor(random_vl[i + 1, ]$trial_num/48)== floor(random_vl[i,]$trial_num/48))) {
    # Then count the response time from the fovlowing stimulus
    rvl_rt <- append (rvl_rt, 1000 + (random_vl[i + 1, ][, "l_rt"]))
    rvl_case2 <- append (rvl_case2, i)
  }
  
  # Otherwise, if the participant responded during the stimulus preceding the target
  else if (!is.na(random_vl[i - 1, ] [, "l_rt"])
           # and the preceding stimulus was not also a target
           & !((random_vl[i - 1, ][, "random_targ"] == (random_vl[i - 1, ][, "image"])))
           #  and two stimuli prior was not also a target from the same block
           & ! ((random_vl[i - 2, ][, "random_targ"] == random_vl[i - 2, ][, "image"])
                & floor(random_vl[i,]$trial_num/48) == floor(random_vl[i-2,]$trial_num/48))
           # and the preceding stimulus came from the same block
           & floor((random_vl[i, ])$trial_num/48) == floor(random_vl[i - 1, ]$trial_num/48)
           # and, because this is a random block
           & 
           # EITHER the target is a standalone, with no target within the two preceding trials or two fovlowing trials, and no keypress during or directly fovlowing
           (
             (!(i - 1) %in% random_vl_targets 
              & floor(random_vl[i - 1, ]$trial_num/48) == floor(random_vl[i, ]$trial_num/48)
              & !(i - 2) %in% random_vl_targets 
              & floor(random_vl[i - 2, ]$trial_num/48) == floor(random_vl[i, ]$trial_num/48)
              & is.na(random_vl[i, ]$l_rt)
              & !(i + 1) %in% random_vl_targets 
              & floor(random_vl[i + 1, ]$trial_num/48) == floor(random_vl[i, ]$trial_num/24) 
              & is.na(random_vl[i + 1, ]$l_rt)
              & !(i + 2) %in% random_vl_targets 
              & floor(random_vl[i + 2, ]$trial_num/48) == floor(random_vl[i, ]$trial_num/48)
             )
             # OR just the fovlowing stimulus is not a target from the same block,
             | (!(i + 1) %in% random_vl_targets 
                & floor(random_vl[i + 1, ]$trial_num/48) != floor(random_vl[i, ]$trial_num/48)
                # OR the fovlowing stimulus is a target from the same block, but has neither an on-target keypress
                | ((i + 1) %in% random_vl_targets 
                   & floor(random_vl[i + 1, ]$trial_num/48) == floor(random_vl[i, ]$trial_num/48) &
                   is.na(random_vl[i + 1, ]$l_rt)
                   # nor a delay from the same block
                   & (floor(random_vl[i + 2, ]$trial_num/48) != floor(random_vl[i, ]$trial_num/48) |
                      is.na(random_vl[i + 2, ]$l_rt)
                   )
                )
             )
           )
  )
  
  {
    # Count the response time as how much sooner they responded than when the stimulus was presented (anticipation)
    rvl_rt <- append(rvl_rt, (random_vl[i - 1, ][, "l_rt"] - 1000))
    rvl_case3 <- append (rvl_case3, i)  }
  
  # Otherwise, if the participant responded during the target
  else if (!is.na(random_vl[i, ] [, "l_rt"])
           # and, because random, the previous stimulus was not also a target with no keypress, followed by a distractor with a keypress
           & !(((i - 1) %in% random_vl_targets &
                is.na(random_vl[i - 1, ] [, "l_rt"]) &
                !(i + 1) %in% random_vl_targets &
                !is.na(random_vl[i + 1, ] [, "l_rt"])))) {
    # Count their response time as the keypress
    rvl_rt <-
      append(rvl_rt, (random_vl[i, ][, "l_rt"]))
    rvl_case4 <- append (rvl_case4, i)
  }
  
  # Otherwise, if the participant responded after the target
  else if (!is.na(random_vl[i + 1, ]$l_rt > 0)
           # And the fovlowing trial came from the same block
           & floor(random_vl[i, ]$trial_num/48) == floor(random_vl[i + 1, ]$trial_num/48)
           # Check that EITHER the fovlowing stimulus either was also not a target
           & (
             !((i + 1) %in% random_vl_targets) |
             # OR, if the fovlowing stimulus was also a target, that it also had a delay
             (((i + 1) %in% random_vl_targets) &
              !is.na(random_vl[i + 2, ]$l_rt) &
              floor(random_vl[i, ]$trial_num/48) == floor(random_vl[i + 2, ]$trial_num/48)
             )
           )
           # Also check that EITHER two stimuli fovlowing was not also a target from the same block (to avoid counting the same keypress twice)
           &
           (
             !((i + 2) %in% random_vl_targets) |
             floor(random_vl[i, ]$trial_num/48) == floor(random_vl[i + 2, ]$trial_num/48) |
             # OR, if two stimuli fovlowing was also a target (from the same block),
             ((i + 2) %in% random_vl_targets) &
             floor(random_vl[i, ]$trial_num/48) == floor(random_vl[i + 2, ]$trial_num/48)
             # that it also had a delay from the same block
             &
             !is.na(
               random_vl[i + 3, ]$l_rt &
               floor(random_vl[i, ]$trial_num/48) == floor(random_vl[i + 3, ]$trial_num/48)
             )
           )) {
    # Count their response time as how much later they responded than when the stimulus was presented
    rvl_rt <-
      append(rvl_rt, (1000 + random_vl[i + 1, ][, "l_rt"]))
    rvl_case5 <- append (rvl_case5, i)
    
    # Otherwise, record the miss with a reaction time of NA
  } else {
    rvl_rt <- append(rvl_rt, NA)
    rvl_case6 <- append (rvl_case6, i)
  }
}

# Match id and response times
random_vl_extracted <- data.frame(rvl_part_id, rvl_rt)

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
extracted_part_id <- unique(random_vl_extracted$rvl_part_id)

# Find the number of targets shown to each participant
target_sum <- NULL
for(i in extracted_part_id){target_sum <- append(target_sum,sum(random_vl_extracted$rvl_part_id==i))}

# TEST: This should be equal to 32
length (target_sum)
# TEST: This should contain a vector full of 24s
target_sum

# For each participant, index the targets
targ_index <- NULL
for (i in target_sum) {targ_index <- append (targ_index, rep(1:i, 1))}

# Add the targets' indices
random_vl_extracted$targ_index <- targ_index

# Remove any values of NA
random_vl_extracted <- random_vl_extracted[!is.na(random_vl_extracted$rvl_rt),]

# Add the target 
random_vl_extracted <- left_join(random_vl_extracted, rvl_targs, by = c("rvl_part_id" = "part_id"))


# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

#There are 24 targets for each participant. Some may have a low hit rate (responded to 12 targets or less)
low_hits<-NULL
# Find people with a low hit rate
for (id in extracted_part_id){
  if (length(random_vl_extracted[which(random_vl_extracted$rvl_part_id==id),]$rvl_rt)<13)
  {low_hits<-append(low_hits, id)}
}
# Remove people with low hit rate
random_vl_extracted <- random_vl_extracted[! random_vl_extracted$rvl_part_id %in% low_hits, ]
# Find only participants with over 50% hit rate
extracted_part_id <- unique(random_vl_extracted$rvl_part_id)


# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
type <- NULL
task<- NULL
same_or_diff <- NULL
test_phase <- NULL
rvl<- NULL
domain <- NULL
range <- NULL
upper_bound <- NULL
lower_bound <- NULL
this_range <- NULL
number_rts <- NULL

# For each participant, extract id
# Assign domain and type
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in extracted_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "linguistic")
  task <- append(task, "vl")
  type <- append (type, "random")
  same_or_diff <- append (same_or_diff, "different")
  test_phase <- append (test_phase, "vsl")
  mean_rt <- append(mean_rt, round(mean(random_vl_extracted$rvl_rt[random_vl_extracted$rvl_part_id==id]),digits=3))
  number_rts <- append(number_rts, length(!is.na(random_vl_extracted$rvl_rt[random_vl_extracted$rvl_part_id==id])))
  rt_slope <- append (rt_slope, round(summary(lm(random_vl_extracted$rvl_rt[random_vl_extracted$rvl_part_id==id]~random_vl_extracted$targ_index[random_vl_extracted$rvl_part_id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (random_vl_extracted[ which(random_vl_extracted$rvl_part_id==id),])
  this_range<- range(data_this_id$rvl_rt, na.rm = TRUE)
  range <- append (range, (this_range[2]-this_range[1]))
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
rvl <- data.frame(part_id, task, same_or_diff, test_phase, domain,type,mean_rt, range, upper_bound, lower_bound, rt_slope)

number_rts
mean(number_rts)
sd(number_rts)



# for internal checking only: find mean rt_slope
mean_rvl_rt_slope <- mean (rvl$rt_slope)


# ******************** CONDITION 4: RANDOM VV *******************

# Separate random and structured conditions
random_vv <- vv_data[ which(vv_data$condition== "R"),]

# Index the rows
random_vv <- tibble::rowid_to_column(random_vv, "index")


# Index the targets -----------------------------------------------

# Identify the rows when this condition's target was presented
random_vv_targets <- random_vv[which(random_vv$random_targ==random_vv$image),]

# TEST: Create a data frame to check the number of lines per participant
list_part_id <- unique(random_vv_targets$part_id)
part_id <- NULL
total_lines <- NULL
for(id in list_part_id){
  part_id <- append(part_id, id)
  total_lines <- append(total_lines, nrow(random_vv_targets[which(random_vv$part_id==id),]))
}
rvv_line_number <- data.frame(part_id, total_lines)
# There should be 32 entries
length(rvv_line_number$part_id)
# They should all contain 288 lines
rvv_line_number$total_lines

# Identify the random target for each participant
rvv_targs <- distinct(random_vv_targets %>%
                        group_by(part_id) %>%
                        summarise(random_targ = random_targ))


# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ fovlowing stimulus ---------------------------------------------
# Initialize variables to track participant ID, condition, modality, task, and reaction time (RT)
rvv_part_id <- NULL
rvv_rt <- NULL

# Track the cases for calculating each type of reaction time
# NOTE: These variables are for internal checking only and can be commented out below in case of bugs
# Case 1: The participant responds during the target, which is the first trial in a block
rvv_case1 <- NULL
# Case 2: The participant responds to the trial directly fovvowing the target, and the target is the first trial in a block
rvv_case2 <- NULL
# Case 3: Anticipation of target, participant responded to stimulus directly preceding target
rvv_case3 <- NULL
# Case 4: Response to target during the target trial
rvv_case4 <- NULL
# Case 5: Delay from target, participant responded to stimulus directly fovvowing target
rvv_case5 <- NULL
# Case 6: Missed target, record NA reaction time
rvv_case6 <- NULL

# Isolate participants' response times.

# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in random_vv_targets$index) {
  # Isolate the ID number
  rvv_part_id <-
    append(rvv_part_id, paste(random_vv[i, ]$part_id))
  
  # Check if you are looking at the first trial in the block. If so, the target does not have a preceding target
  if ((random_vv[i, ]$trial_num %% 48 == 1)
      # Check if the participant responded during the target trial
      & !is.na(random_vv[i, ]$v_rt)) {
    # If so, count the response time from the target stimulus
    rvv_rt <- append (rvv_rt, random_vv[i, ][, "v_rt"])
    rvv_case1 <- append (rvv_case1, i)
  } 
  
  # If it's the first trial and there was no target keypress
  else if (((random_vv[i, ]$trial_num %%48 == 1) & (is.na(random_vv[i,]$v_rt)))
           # Check that the fovvowing stimulus was not also a target from the same block (to avoid counting the same keypress twice)
           & !((i + 1 %in% random_vv_targets) & floor(random_vv[i + 1, ]$trial_num/48)== floor(random_vv[i,]$trial_num/48))) {
    # Then count the response time from the fovvowing stimulus
    rvv_rt <- append (rvv_rt, 1000 + (random_vv[i + 1, ][, "v_rt"]))
    rvv_case2 <- append (rvv_case2, i)
  }
  
  # Otherwise, if the participant responded during the stimulus preceding the target
  else if (!is.na(random_vv[i - 1, ] [, "v_rt"])
           # and the preceding stimulus was not also a target
           & !((random_vv[i - 1, ][, "random_targ"] == (random_vv[i - 1, ][, "image"])))
           #  and two stimuli prior was not also a target from the same block
           & ! ((random_vv[i - 2, ][, "random_targ"] == random_vv[i - 2, ][, "image"])
                & floor(random_vv[i,]$trial_num/48) == floor(random_vv[i-2,]$trial_num/48))
           # and the preceding stimulus came from the same block
           & floor((random_vv[i, ])$trial_num/48) == floor(random_vv[i - 1, ]$trial_num/48)
           # and, because this is a random block
           & 
           # EITHER the target is a standalone, with no target within the two preceding trials or two fovvowing trials, and no keypress during or directly fovvowing
           (
             (!(i - 1) %in% random_vv_targets 
              & floor(random_vv[i - 1, ]$trial_num/48) == floor(random_vv[i, ]$trial_num/48)
              & !(i - 2) %in% random_vv_targets 
              & floor(random_vv[i - 2, ]$trial_num/48) == floor(random_vv[i, ]$trial_num/48)
              & is.na(random_vv[i, ]$v_rt)
              & !(i + 1) %in% random_vv_targets 
              & floor(random_vv[i + 1, ]$trial_num/48) == floor(random_vv[i, ]$trial_num/24) 
              & is.na(random_vv[i + 1, ]$v_rt)
              & !(i + 2) %in% random_vv_targets 
              & floor(random_vv[i + 2, ]$trial_num/48) == floor(random_vv[i, ]$trial_num/48)
             )
             # OR just the fovvowing stimulus is not a target from the same block,
             | (!(i + 1) %in% random_vv_targets 
                & floor(random_vv[i + 1, ]$trial_num/48) != floor(random_vv[i, ]$trial_num/48)
                # OR the fovvowing stimulus is a target from the same block, but has neither an on-target keypress
                | ((i + 1) %in% random_vv_targets 
                   & floor(random_vv[i + 1, ]$trial_num/48) == floor(random_vv[i, ]$trial_num/48) &
                   is.na(random_vv[i + 1, ]$v_rt)
                   # nor a delay from the same block
                   & (floor(random_vv[i + 2, ]$trial_num/48) != floor(random_vv[i, ]$trial_num/48) |
                      is.na(random_vv[i + 2, ]$v_rt)
                   )
                )
             )
           )
  )
  
  {
    # Count the response time as how much sooner they responded than when the stimulus was presented (anticipation)
    rvv_rt <- append(rvv_rt, (random_vv[i - 1, ][, "v_rt"] - 1000))
    rvv_case3 <- append (rvv_case3, i)  }
  
  # Otherwise, if the participant responded during the target
  else if (!is.na(random_vv[i, ] [, "v_rt"])
           # and, because random, the previous stimulus was not also a target with no keypress, fovvowed by a distractor with a keypress
           & !(((i - 1) %in% random_vv_targets &
                is.na(random_vv[i - 1, ] [, "v_rt"]) &
                !(i + 1) %in% random_vv_targets &
                !is.na(random_vv[i + 1, ] [, "v_rt"])))) {
    # Count their response time as the keypress
    rvv_rt <-
      append(rvv_rt, (random_vv[i, ][, "v_rt"]))
    rvv_case4 <- append (rvv_case4, i)
  }
  
  # Otherwise, if the participant responded after the target
  else if (!is.na(random_vv[i + 1, ]$v_rt > 0)
           # And the fovvowing trial came from the same block
           & floor(random_vv[i, ]$trial_num/48) == floor(random_vv[i + 1, ]$trial_num/48)
           # Check that EITHER the fovvowing stimulus either was also not a target
           & (
             !((i + 1) %in% random_vv_targets) |
             # OR, if the fovvowing stimulus was also a target, that it also had a delay
             (((i + 1) %in% random_vv_targets) &
              !is.na(random_vv[i + 2, ]$v_rt) &
              floor(random_vv[i, ]$trial_num/48) == floor(random_vv[i + 2, ]$trial_num/48)
             )
           )
           # Also check that EITHER two stimuli fovvowing was not also a target from the same block (to avoid counting the same keypress twice)
           &
           (
             !((i + 2) %in% random_vv_targets) |
             floor(random_vv[i, ]$trial_num/48) == floor(random_vv[i + 2, ]$trial_num/48) |
             # OR, if two stimuli fovvowing was also a target (from the same block),
             ((i + 2) %in% random_vv_targets) &
             floor(random_vv[i, ]$trial_num/48) == floor(random_vv[i + 2, ]$trial_num/48)
             # that it also had a delay from the same block
             &
             !is.na(
               random_vv[i + 3, ]$v_rt &
               floor(random_vv[i, ]$trial_num/48) == floor(random_vv[i + 3, ]$trial_num/48)
             )
           )) {
    # Count their response time as how much later they responded than when the stimulus was presented
    rvv_rt <-
      append(rvv_rt, (1000 + random_vv[i + 1, ][, "v_rt"]))
    rvv_case5 <- append (rvv_case5, i)
    
    # Otherwise, record the miss with a reaction time of NA
  } else {
    rvv_rt <- append(rvv_rt, NA)
    rvv_case6 <- append (rvv_case6, i)
  }
}

# Match id and response times
random_vv_extracted <- data.frame(rvv_part_id, rvv_rt)
# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
extracted_part_id <- unique(random_vv_extracted$rvv_part_id)

# Find the number of targets shown to each participant
target_sum <- NULL
for(i in extracted_part_id){target_sum <- append(target_sum,sum(random_vv_extracted$rvv_part_id==i))}

# TEST: This should be equal to 32 (for each of the 32 participants)
length (target_sum)
# TEST: This should contain a vector full of 24s (for the 24 targets each participant saw)
target_sum

# Note: sit_a_054 only saw 16 before psychopy quit, so consider removing them

# For each participant, index the targets
targ_index <- NULL
for (i in target_sum) {targ_index <- append (targ_index, rep(1:i, 1))}

# Add the targets' indices
random_vv_extracted$targ_index <- targ_index

# Remove any values of NA. 
# We do this a second time here to remove any participants who did not respond during the trial.
random_vv_extracted <- random_vv_extracted[!is.na(random_vv_extracted$rvv_rt),]

# Add the target 
random_vv_extracted <- left_join(random_vv_extracted, rvv_targs, by = c("rvv_part_id" = "part_id"))

# List unique participant IDs for this condition
extracted_part_id <- unique(random_vv_extracted$rvv_part_id)

# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

#There are 24 targets for each participant. Some may have a low hit rate (responded to 12 targets or less)
low_hits<-NULL
# Find people with a low hit rate
for (id in extracted_part_id){
  if (length(random_vv_extracted[which(random_vv_extracted$rvv_part_id==id),]$rvv_rt)<13)
  {low_hits<-append(low_hits, id)}
}
# Remove people with low hit rate
random_vv_extracted <- random_vv_extracted[! random_vv_extracted$rvv_part_id %in% low_hits, ]
# Find only participants with over 50% hit rate
extracted_part_id <- unique(random_vv_extracted$rvv_part_id)


# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
type <- NULL
same_or_diff <- NULL
test_phase <- NULL
task<- NULL
rvv<- NULL
domain <- NULL
range <- NULL
upper_bound <- NULL
lower_bound <- NULL
this_range <- NULL
number_rts <- NULL

# For each participant, extract id
# Assign domain and type
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in extracted_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "non-linguistic")
  task <- append(task, "vv")
  type <- append (type, "random")
  same_or_diff <- append (same_or_diff, "same")
  test_phase <- append (test_phase, "vsl")
  mean_rt <- append(mean_rt, round(mean(random_vv_extracted$rvv_rt[random_vv_extracted$rvv_part_id==id]),digits=3))
  number_rts <- append(number_rts, length(!is.na(random_vv_extracted$rvv_rt[random_vv_extracted$rvv_part_id==id])))
  rt_slope <- append (rt_slope, round(summary(lm(random_vv_extracted$rvv_rt[random_vv_extracted$rvv_part_id==id]~random_vv_extracted$targ_index[random_vv_extracted$rvv_part_id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (random_vv_extracted[ which(random_vv_extracted$rvv_part_id==id),])
  this_range<- range(data_this_id$rvv_rt, na.rm = TRUE)
  range <- append (range, (this_range[2]-this_range[1]))
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
rvv <- data.frame(part_id, task, same_or_diff, test_phase, domain,type,mean_rt, range, upper_bound, lower_bound, rt_slope)

number_rts
length(number_rts)
mean(number_rts)
sd(number_rts)



# ******************** CONDITION 5: STRUCTURED LL*******************

# Separate random and structured conditions
structured_ll <- ll_data[ which(ll_data$condition== "S"),]

# Index the rows
structured_ll <- tibble::rowid_to_column(structured_ll, "index")

# Find all of the triplets presented
structured_ll$triplet <- rep (do.call(paste, as.data.frame(t(matrix(structured_ll$image, 3)), stringsAsFactors=FALSE)), each = 3)

# Identify the rows when this condition's target was presented
structured_ll_targets <- structured_ll[which(structured_ll$structured_targ==structured_ll$image),]

# Identify the structured target for each participant
sll_targs <- distinct(structured_ll_targets %>%
                        group_by(part_id) %>%
                        summarise(structured_targ = structured_targ))

# TEST: Create a data frame to check the number of lines per participant
list_part_id <- unique(structured_ll_targets$part_id)
part_id <- NULL
total_lines <- NULL
for(id in list_part_id){
  part_id <- append(part_id, id)
  total_lines <- append(total_lines, nrow(structured_ll_targets[which(structured_ll$part_id==id),]))
}
sll_line_number <- data.frame(part_id, total_lines)
# TEST: There should be 32 entries (for the 32 participants)
length(sll_line_number$part_id)
# TEST: They should all contain 288 lines (for the 288 lines each participant saw)
sll_line_number$total_lines

# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ fovlowing stimulus ---------------------------------------------
# Initialize variables to track participant ID, condition, modality, task, and reaction time (RT)
sll_part_id <- NULL
sll_rt <- NULL

# Track the cases for calculating each type of reaction time
# NOTE: These variables are for internal checking only and can be commented out below in case of bugs
# Case 1: The participant responds during the target, which is the first trial in a block
sll_case1 <- NULL
# Case 2: The participant responds to the trial directly fovvowing the target, and the target is the first trial in a block
sll_case2 <- NULL
# Case 3: Anticipation of target, participant responded to stimulus directly preceding target
sll_case3 <- NULL
# Case 4: Response to target during the target trial
sll_case4 <- NULL
# Case 5: Delay from target, participant responded to stimulus directly fovvowing target
sll_case5 <- NULL
# Case 6: Missed target, record NA reaction time
sll_case6 <- NULL

# Isolate participants' response times.

# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in structured_ll_targets$index) {
  # Isolate the ID number
  sll_part_id <-
    append(sll_part_id, paste(structured_ll[i, ]$part_id))
  
  # Check if you are looking at the first trial in the block. If so, the target does not have a preceding target
  if ((structured_ll[i, ]$trial_num %% 48 == 1)
      # Check if the participant responded during the target trial
      & !is.na(structured_ll[i, ]$l_rt)) {
    # If so, count the response time from the target stimulus
    sll_rt <- append (sll_rt, structured_ll[i, ][, "l_rt"])
    sll_case1 <- append (sll_case1, i)
  } 
  
  # If it's the first trial and there was no target keypress
  else if (((structured_ll[i, ]$trial_num %%48 == 1) & (is.na(structured_ll[i,]$l_rt)))
           # Check that the following stimulus was not also a target from the same block (to avoid counting the same keypress twice)
           & !((i + 1 %in% structured_ll_targets) & floor(structured_ll[i + 1, ]$trial_num/48)== floor(structured_ll[i,]$trial_num/48))) {
    # Then count the response time from the following stimulus
    sll_rt <- append (sll_rt, 1000 + (structured_ll[i + 1, ][, "l_rt"]))
    sll_case2 <- append (sll_case2, i)
  }
  
  # Otherwise, if the participant responded during the stimulus preceding the target
  else if (!is.na(structured_ll[i - 1, ] [, "l_rt"])
           # and the preceding stimulus was not also a target
           & !((structured_ll[i - 1, ][, "structured_targ"] == (structured_ll[i - 1, ][, "image"])))
           #  and two stimuli prior was not also a target from the same block
           & ! ((structured_ll[i - 2, ][, "structured_targ"] == structured_ll[i - 2, ][, "image"])
                & floor(structured_ll[i,]$trial_num/48) == floor(structured_ll[i-2,]$trial_num/48))
           # and the preceding stimulus came from the same block
           & floor((structured_ll[i, ])$trial_num/48) == floor(structured_ll[i - 1, ]$trial_num/48)
           )
  {
    # Count the response time as how much sooner they responded than when the stimulus was presented (anticipation)
    sll_rt <- append(sll_rt, (structured_ll[i - 1, ][, "l_rt"] - 1000))
    sll_case3 <- append (sll_case3, i)  }
  
  # Otherwise, if the participant responded during the target
  else if (!is.na(structured_ll[i, ] [, "l_rt"])
           # and, because structured, the previous trial had no keypress
           &
           (is.na(structured_ll[i - 1, ][, "l_rt"]))){
    # Count their response time as the keypress
    sll_rt <-
      append(sll_rt, (structured_ll[i, ][, "l_rt"]))
    sll_case4 <- append (sll_case4, i)
  }
  
  # Otherwise, if the participant responded after the target
  else if (!is.na(structured_ll[i + 1, ]$l_rt > 0)
           # And the following trial came from the same block
           & floor(structured_ll[i, ]$trial_num/48) == floor(structured_ll[i + 1, ]$trial_num/48)
           # Check that EITHER the following stimulus either was also not a target
           & (
             !((i + 1) %in% structured_ll_targets) |
             # OR, if the following stimulus was also a target, that it also had a delay
             (((i + 1) %in% structured_ll_targets) &
              !is.na(structured_ll[i + 2, ]$l_rt) &
              floor(structured_ll[i, ]$trial_num/48) == floor(structured_ll[i + 2, ]$trial_num/48)
             )
           )
           # Also check that EITHER two stimuli following was not also a target from the same block (to avoid counting the same keypress twice)
           &
           (
             !((i + 2) %in% structured_ll_targets) |
             floor(structured_ll[i, ]$trial_num/48) == floor(structured_ll[i + 2, ]$trial_num/48) |
             # OR, if two stimuli following was also a target (from the same block),
             ((i + 2) %in% structured_ll_targets) &
             floor(structured_ll[i, ]$trial_num/48) == floor(structured_ll[i + 2, ]$trial_num/48)
             # that it also had a delay from the same block
             &
             !is.na(
               structured_ll[i + 3, ]$l_rt &
               floor(structured_ll[i, ]$trial_num/48) == floor(structured_ll[i + 3, ]$trial_num/48)
             )
           )) {
    # Count their response time as how much later they responded than when the stimulus was presented
    sll_rt <-
      append(sll_rt, (1000 + structured_ll[i + 1, ][, "l_rt"]))
    sll_case5 <- append (sll_case5, i)
    
    # Otherwise, record the miss with a reaction time of NA
  } else {
    sll_rt <- append(sll_rt, NA)
    sll_case6 <- append (sll_case6, i)
  }
}

# Match id and response times
structured_ll_extracted <- data.frame(sll_part_id, sll_rt)


# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
extracted_part_id <- unique(structured_ll_extracted$sll_part_id)

# Find the number of targets shown to each participant
target_sum <- NULL
for(i in extracted_part_id){target_sum <- append(target_sum,sum(structured_ll_extracted$sll_part_id==i))}

# TO TEST: This should be 32 (for the 32 participants)
length(target_sum)
# This should all contain values of 24 (for the 24 targets per participant)
target_sum

# For each participant, index the targets
targ_index <- NULL
for (i in target_sum) {targ_index <- append (targ_index, rep(1:i, 1))}

# Add the targets' indices
structured_ll_extracted$targ_index <- targ_index

# Remove any values of NA
structured_ll_extracted <- structured_ll_extracted[!is.na(structured_ll_extracted$sll_rt),]

# Add the target 
structured_ll_extracted <- left_join(structured_ll_extracted, sll_targs, by = c("sll_part_id" = "part_id"))

# List unique participant IDs for this condition
extracted_part_id <- unique(structured_ll_extracted$sll_part_id)

# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

#There are 24 targets for each participant. Some may have a low hit rate (responded to 12 targets or less)
low_hits<-NULL
# Find people with a low hit rate
for (id in extracted_part_id){
  if (length(structured_ll_extracted[which(structured_ll_extracted$sll_part_id==id),]$sll_rt)<13)
  {low_hits<-append(low_hits, id)}
}
# Remove people with low hit rate
structured_ll_extracted <- structured_ll_extracted[! structured_ll_extracted$sll_part_id %in% low_hits, ]
# Find only participants with over 50% hit rate
extracted_part_id <- unique(structured_ll_extracted$sll_part_id)

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
type <- NULL
task<- NULL
same_or_diff <- NULL
test_phase <- NULL
sll<- NULL
domain <- NULL
range <- NULL
upper_bound <- NULL
lower_bound <- NULL
this_range <- NULL
number_rts <- NULL

# For each participant, extract id
# Assign domain and type
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in extracted_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "linguistic")
  task <- append(task, "ll")
  type <- append (type, "structured")
  same_or_diff <- append (same_or_diff, "same")
  test_phase <- append (test_phase, "lsl")
  mean_rt <- append(mean_rt, round(mean(structured_ll_extracted$sll_rt[structured_ll_extracted$sll_part_id==id]),digits=3))
  number_rts <- append(number_rts, length(!is.na(structured_ll_extracted$sll_rt[structured_ll_extracted$sll_part_id==id])))
  rt_slope <- append (rt_slope, round(summary(lm(structured_ll_extracted$sll_rt[structured_ll_extracted$sll_part_id==id]~structured_ll_extracted$targ_index[structured_ll_extracted$sll_part_id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (structured_ll_extracted[ which(structured_ll_extracted$sll_part_id==id),])
  this_range<- range(data_this_id$sll_rt, na.rm = TRUE)
  range <- append (range, (this_range[2]-this_range[1]))
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
sll <- data.frame(part_id, task, same_or_diff, test_phase, domain,type,mean_rt, range, upper_bound, lower_bound, rt_slope)

length(number_rts)
number_rts
mean(number_rts)
sd(number_rts)


# TEST: find mean rt_slope
mean_sll_rt_slope <- mean (sll$rt_slope)
# This should be negative
mean_sll_rt_slope



# ******************** CONDITION 6: lv structured*******************

# Separate structured and structured conditions
structured_lv <- lv_data[ which(lv_data$condition== "S"),]

# Index the rows
structured_lv <- tibble::rowid_to_column(structured_lv, "index")

# Find all of the triplets presented
structured_lv$triplet <- rep (do.call(paste, as.data.frame(t(matrix(structured_lv$image, 3)), stringsAsFactors=FALSE)), each = 3)

# Identify the rows when this condition's target was presented
structured_lv_targets <- structured_lv[which(structured_lv$structured_targ==structured_lv$image),]

# Identify the structured target for each participant
slv_targs <- distinct(structured_lv_targets %>%
                        group_by(part_id) %>%
                        summarise(structured_targ = structured_targ))

# TEST: Create a data frame to check the number of lines per participant
list_part_id <- unique(structured_lv_targets$part_id)
part_id <- NULL
total_lines <- NULL
for(id in list_part_id){
  part_id <- append(part_id, id)
  total_lines <- append(total_lines, nrow(structured_lv_targets[which(structured_lv$part_id==id),]))
}

slv_line_number <- data.frame(part_id, total_lines)
# There should be 31 entries
length(slv_line_number$part_id)
# They should all contain 288 lines
slv_line_number$total_lines

# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ fovlowing stimulus ---------------------------------------------
# Initialize variables to track participant ID, condition, modality, task, and reaction time (RT)
slv_part_id <- NULL
slv_rt <- NULL

# Track the cases for calculating each type of reaction time
# NOTE: These variables are for internal checking only and can be commented out below in case of bugs
# Case 1: The participant responds during the target, which is the first trial in a block
slv_case1 <- NULL
# Case 2: The participant responds to the trial directly fovvowing the target, and the target is the first trial in a block
slv_case2 <- NULL
# Case 3: Anticipation of target, participant responded to stimulus directly preceding target
slv_case3 <- NULL
# Case 4: Response to target during the target trial
slv_case4 <- NULL
# Case 5: Delay from target, participant responded to stimulus directly fovvowing target
slv_case5 <- NULL
# Case 6: Missed target, record NA reaction time
slv_case6 <- NULL

# Isolate participants' response times.

# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in structured_lv_targets$index) {
  # Isolate the ID number
  slv_part_id <-
    append(slv_part_id, paste(structured_lv[i, ]$part_id))
  
  # Check if you are looking at the first trial in the block. If so, the target does not have a preceding target
  if ((structured_lv[i, ]$trialnum %% 48 == 1)
      # Check if the participant responded during the target trial
      & !is.na(structured_lv[i, ]$l_rt)) {
    # If so, count the response time from the target stimulus
    slv_rt <- append (l_rt, structured_lv[i, ][, "l_rt"])
    slv_case1 <- append (slv_case1, i)
  } 
  
  # If it's the first trial and there was no target keypress
  else if (((structured_lv[i, ]$trialnum %%48 == 1) & (is.na(structured_lv[i,]$l_rt)))
           # Check that the following stimulus was not also a target from the same block (to avoid counting the same keypress twice)
           & !((i + 1 %in% structured_lv_targets) & floor(structured_lv[i + 1, ]$trialnum/48)== floor(structured_lv[i,]$trialnum/48))) {
    # Then count the response time from the following stimulus
    slv_rt <- append (slv_rt, 1000 + (structured_lv[i + 1, ][, "l_rt"]))
    slv_case2 <- append (slv_case2, i)
  }
  
  # Otherwise, if the participant responded during the stimulus preceding the target
  else if (!is.na(structured_lv[i - 1, ] [, "l_rt"])
           # and the preceding stimulus was not also a target
           & !((structured_lv[i - 1, ][, "structured_targ"] == (structured_lv[i - 1, ][, "image"])))
           #  and two stimuli prior was not also a target from the same block
           & ! ((structured_lv[i - 2, ][, "structured_targ"] == structured_lv[i - 2, ][, "image"])
                & floor(structured_lv[i,]$trialnum/48) == floor(structured_lv[i-2,]$trialnum/48))
           # and the preceding stimulus came from the same block
           & floor((structured_lv[i, ])$trialnum/48) == floor(structured_lv[i - 1, ]$trialnum/48)
  )
  {
    # Count the response time as how much sooner they responded than when the stimulus was presented (anticipation)
    slv_rt <- append(slv_rt, (structured_lv[i - 1, ][, "l_rt"] - 1000))
    slv_case3 <- append (slv_case3, i)  }
  
  # Otherwise, if the participant responded during the target
  else if (!is.na(structured_lv[i, ] [, "l_rt"])
           # and, because structured, the previous trial had no keypress
           &
           (is.na(structured_lv[i - 1, ][, "l_rt"]))){
    # Count their response time as the keypress
    slv_rt <-
      append(slv_rt, (structured_lv[i, ][, "l_rt"]))
    slv_case4 <- append (slv_case4, i)
  }
  
  
  # Otherwise, if the participant responded after the target
  else if (!is.na(structured_lv[i + 1, ]$l_rt > 0)
           # And the following trial came from the same block
           & floor(structured_lv[i, ]$trialnum/48) == floor(structured_lv[i + 1, ]$trialnum/48)
           # Check that EITHER the following stimulus either was also not a target
           & (
             !((i + 1) %in% structured_lv_targets) |
             # OR, if the following stimulus was also a target, that it also had a delay
             (((i + 1) %in% structured_lv_targets) &
              !is.na(structured_lv[i + 2, ]$l_rt) &
              floor(structured_lv[i, ]$trialnum/48) == floor(structured_lv[i + 2, ]$trialnum/48)
             )
           )
           # Also check that EITHER two stimuli following was not also a target from the same block (to avoid counting the same keypress twice)
           &
           (
             !((i + 2) %in% structured_lv_targets) |
             floor(structured_lv[i, ]$trialnum/48) == floor(structured_lv[i + 2, ]$trialnum/48) |
             # OR, if two stimuli following was also a target (from the same block),
             ((i + 2) %in% structured_lv_targets) &
             floor(structured_lv[i, ]$trialnum/48) == floor(structured_lv[i + 2, ]$trialnum/48)
             # that it also had a delay from the same block
             &
             !is.na(
               structured_lv[i + 3, ]$l_rt &
               floor(structured_lv[i, ]$trialnum/48) == floor(structured_lv[i + 3, ]$trialnum/48)
             )
           )) {
    # Count their response time as how much later they responded than when the stimulus was presented
    slv_rt <-
      append(slv_rt, (1000 + structured_lv[i + 1, ][, "l_rt"]))
    slv_case5 <- append (slv_case5, i)
    
    # Otherwise, record the miss with a reaction time of NA
  } else {
    slv_rt <- append(slv_rt, NA)
    slv_case6 <- append (slv_case6, i)
  }
}

# Match id and response times
structured_lv_extracted <- data.frame(slv_part_id, slv_rt)

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
extracted_part_id <- unique(structured_lv_extracted$slv_part_id)

# Find the number of targets shown to each participant
target_sum <- NULL
for(i in extracted_part_id){target_sum <- append(target_sum,sum(structured_lv_extracted$slv_part_id==i))}

# TEST: This should be equal to 32
length (target_sum)
# TEST: This should contain a vector full of 24s
target_sum


# For each participant, index the targets
targ_index <- NULL
for (i in target_sum) {targ_index <- append (targ_index, rep(1:i, 1))}

# Add the targets' indices
structured_lv_extracted$targ_index <- targ_index

# Remove any values of NA
structured_lv_extracted <- structured_lv_extracted[!is.na(structured_lv_extracted$slv_rt),]

# Add the target 
structured_lv_extracted <- left_join(structured_lv_extracted, slv_targs, by = c("slv_part_id" = "part_id"))

# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

#There are 24 targets for each participant. Some may have a low hit rate (responded to 12 targets or less)
low_hits<-NULL
# Find people with a low hit rate
for (id in extracted_part_id){
  if (length(structured_lv_extracted[which(structured_lv_extracted$slv_part_id==id),]$slv_part_id)<13)
  {low_hits<-append(low_hits, id)}
}



# Remove people with low hit rate
structured_lv_extracted <- structured_lv_extracted[! structured_lv_extracted$slv_part_id %in% low_hits, ]
# Find only participants with over 50% hit rate
extracted_part_id <- unique(structured_lv_extracted$slv_part_id)

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
type <- NULL
task<- NULL
same_or_diff <- NULL
test_phase <- NULL
slv<- NULL
domain <- NULL
range <- NULL
upper_bound <- NULL
lower_bound <- NULL
this_range <- NULL
number_rts <- NULL

# For each participant, extract id
# Assign domain and type
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in extracted_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "linguistic")
  task <- append(task, "lv")
  type <- append (type, "structured")
  same_or_diff <- append (same_or_diff, "different")
  test_phase <- append (test_phase, "lsl")
  mean_rt <- append(mean_rt, round(mean(structured_lv_extracted$slv_rt[structured_lv_extracted$slv_part_id==id]),digits=3))
  number_rts <- append(number_rts, length(!is.na(structured_lv_extracted$slv_rt[structured_lv_extracted$slv_part_id==id])))
  rt_slope <- append (rt_slope, round(summary(lm(structured_lv_extracted$slv_rt[structured_lv_extracted$slv_part_id==id]~structured_lv_extracted$targ_index[structured_lv_extracted$slv_part_id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (structured_lv_extracted[ which(structured_lv_extracted$slv_part_id==id),])
  this_range<- range(data_this_id$slv_rt, na.rm = TRUE)
  range <- append (range, (this_range[2]-this_range[1]))
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
slv <- data.frame(part_id, task, same_or_diff, test_phase, domain,type,mean_rt, range, upper_bound, lower_bound, rt_slope)

number_rts
length(number_rts)
mean(number_rts)
sd(number_rts)



# TEST: find mean rt_slope
mean_slv_rt_slope <- mean (slv$rt_slope)
# It should be negative
mean_slv_rt_slope


# ******************** CONDITION 7: vl structured*******************

# Separate structured and structured conditions
structured_vl <- vl_data[ which(vl_data$condition== "S"),]

# Find all of the triplets presented
structured_vl$triplet <- rep (do.call(paste, as.data.frame(t(matrix(structured_vl$image, 3)), stringsAsFactors=FALSE)), each = 3)
structured_vl$triplet <- gsub("Alien", "", structured_vl$triplet)

# Remove the mistaken 10-21-22 triplet
structured_vl <- (dplyr::filter(structured_vl, triplet!="10 11 22"))

# Index the rows
structured_vl <- tibble::rowid_to_column(structured_vl, "index")

# Identify the rows when this condition's target was presented
structured_vl_targets <- structured_vl[which(structured_vl$structured_targ==structured_vl$image),]

# Identify the structured target for each participant
svl_targs <- distinct(structured_vl_targets %>%
                        group_by(part_id) %>%
                        summarise(structured_targ = structured_targ))


## Index the images by structured/ structured-----------------------------------------------

# TEST: Create a data frame to check the number of lines per participant
list_part_id <- unique(structured_vl_targets$part_id)
part_id <- NULL
total_lines <- NULL
for(id in list_part_id){
  part_id <- append(part_id, id)
  total_lines <- append(total_lines, nrow(structured_vl_targets[which(structured_vl$part_id==id),]))
}
svl_line_number <- data.frame(part_id, total_lines)
# There should be 32 entries
length(svl_line_number$part_id)
# They should all contain 276 lines = 288 - (4 triplets with "10 11 22" x 3 stimuli)
svl_line_number$total_lines

# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ fovlowing stimulus ---------------------------------------------
# Initialize variables to track participant ID, condition, modality, task, and reaction time (RT)
svl_part_id <- NULL
svl_rt <- NULL

# Track the cases for calculating each type of reaction time
# NOTE: These variables are for internal checking only and can be commented out below in case of bugs
# Case 1: The participant responds during the target, which is the first trial in a block
svl_case1 <- NULL
# Case 2: The participant responds to the trial directly fovvowing the target, and the target is the first trial in a block
svl_case2 <- NULL
# Case 3: Anticipation of target, participant responded to stimulus directly preceding target
svl_case3 <- NULL
# Case 4: Response to target during the target trial
svl_case4 <- NULL
# Case 5: Delay from target, participant responded to stimulus directly fovvowing target
svl_case5 <- NULL
# Case 6: Missed target, record NA reaction time
svl_case6 <- NULL

# Isolate participants' response times.

# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in structured_vl_targets$index) {
  # Isolate the ID number
  svl_part_id <-
    append(svl_part_id, paste(structured_vl[i, ]$part_id))
  
  # Check if you are looking at the first trial in the block. If so, the target does not have a preceding target
  if ((structured_vl[i, ]$trial_num %% 48 == 1)
      # Check if the participant responded during the target trial
      & !is.na(structured_vl[i, ]$v_rt)) {
    # If so, count the response time from the target stimulus
    svl_rt <- append (svl_rt, structured_vl[i, ][, "v_rt"])
    svl_case1 <- append (svl_case1, i)
  } 
  
  # If it's the first trial and there was no target keypress
  else if (((structured_vl[i, ]$trial_num %%48 == 1) & (is.na(structured_vl[i,]$v_rt)))
           # Check that the following stimulus was not also a target from the same block (to avoid counting the same keypress twice)
           & !((i + 1 %in% structured_vl_targets) & floor(structured_vl[i + 1, ]$trial_num/48)== floor(structured_vl[i,]$trial_num/48))) {
    # Then count the response time from the following stimulus
    svl_rt <- append (svl_rt, 1000 + (structured_vl[i + 1, ][, "v_rt"]))
    svl_case2 <- append (svl_case2, i)
  }
  
  # Otherwise, if the participant responded during the stimulus preceding the target
  else if (!is.na(structured_vl[i - 1, ] [, "v_rt"])
           # and the preceding stimulus was not also a target
           & !((structured_vl[i - 1, ][, "structured_targ"] == (structured_vl[i - 1, ][, "image"])))
           #  and two stimuli prior was not also a target from the same block
           & ! ((structured_vl[i - 2, ][, "structured_targ"] == structured_vl[i - 2, ][, "image"])
                & floor(structured_vl[i,]$trial_num/48) == floor(structured_vl[i-2,]$trial_num/48))
           # and the preceding stimulus came from the same block
           & floor((structured_vl[i, ])$trial_num/48) == floor(structured_vl[i - 1, ]$trial_num/48)
  )
  {
    # Count the response time as how much sooner they responded than when the stimulus was presented (anticipation)
    svl_rt <- append(svl_rt, (structured_vl[i - 1, ][, "v_rt"] - 1000))
    svl_case3 <- append (svl_case3, i)  }
  
  # Otherwise, if the participant responded during the target
  else if (!is.na(structured_vl[i, ] [, "v_rt"])
           # and, because structured, the previous trial had no keypress
           &
           (is.na(structured_vl[i - 1, ][, "v_rt"]))){
    # Count their response time as the keypress
    svl_rt <-
      append(svl_rt, (structured_vl[i, ][, "v_rt"]))
    svl_case4 <- append (svl_case4, i)
  }
  
  # Otherwise, if the participant responded after the target
  else if (!is.na(structured_vl[i + 1, ]$v_rt > 0)
           # And the following trial came from the same block
           & floor(structured_vl[i, ]$trial_num/48) == floor(structured_vl[i + 1, ]$trial_num/48)
           # Check that EITHER the following stimulus either was also not a target
           & (
             !((i + 1) %in% structured_vl_targets) |
             # OR, if the following stimulus was also a target, that it also had a delay
             (((i + 1) %in% structured_vl_targets) &
              !is.na(structured_vl[i + 2, ]$v_rt) &
              floor(structured_vl[i, ]$trial_num/48) == floor(structured_vl[i + 2, ]$trial_num/48)
             )
           )
           # Also check that EITHER two stimuli following was not also a target from the same block (to avoid counting the same keypress twice)
           &
           (
             !((i + 2) %in% structured_vl_targets) |
             floor(structured_vl[i, ]$trial_num/48) == floor(structured_vl[i + 2, ]$trial_num/48) |
             # OR, if two stimuli following was also a target (from the same block),
             ((i + 2) %in% structured_vl_targets) &
             floor(structured_vl[i, ]$trial_num/48) == floor(structured_vl[i + 2, ]$trial_num/48)
             # that it also had a delay from the same block
             &
             !is.na(
               structured_vl[i + 3, ]$v_rt &
               floor(structured_vl[i, ]$trial_num/48) == floor(structured_vl[i + 3, ]$trial_num/48)
             )
           )) {
    # Count their response time as how much later they responded than when the stimulus was presented
    svl_rt <-
      append(svl_rt, (1000 + structured_vl[i + 1, ][, "v_rt"]))
    svl_case5 <- append (svl_case5, i)
    
    # Otherwise, record the miss with a reaction time of NA
  } else {
    svl_rt <- append(svl_rt, NA)
    svl_case6 <- append (svl_case6, i)
  }
}

# Match id and response times
structured_vl_extracted <- data.frame(svl_part_id, svl_rt)

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
extracted_part_id <- unique(structured_vl_extracted$svl_part_id)

# Find the number of targets shown to each participant
target_sum <- NULL
for(i in extracted_part_id){target_sum <- append(target_sum,sum(structured_vl_extracted$svl_part_id==i))}

# TEST: This should be equal to 32
length (target_sum)
# TEST: This should contain a vector full of 24s (if the target was not "Alien22") and 20s (if it was)
target_sum

# For each participant, index the targets
targ_index <- NULL
for (i in target_sum) {targ_index <- append (targ_index, rep(1:i, 1))}

# Add the targets' indices
structured_vl_extracted$targ_index <- targ_index

# Remove any values of NA
structured_vl_extracted <- structured_vl_extracted[!is.na(structured_vl_extracted$svl_rt),]

# Add the target 
structured_vl_extracted <- left_join(structured_vl_extracted, svl_targs, by = c("svl_part_id" = "part_id"))


# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

# Each participant should have seen 24 targets total (though some saw only 20).
# Regardless, ome may have a low hit rate (responded to < 13 targets, ie. < half of the total targets they should have seen)
low_hits<-NULL
# Find people with a low hit rate
for (id in extracted_part_id){
  if (length(structured_vl_extracted[which(structured_vl_extracted$svl_part_id==id),]$svl_rt)<13)
  {low_hits<-append(low_hits, id)}
}
# Remove people with low hit rate
structured_vl_extracted <- structured_vl_extracted[! structured_vl_extracted$svl_part_id %in% low_hits, ]
# Find only participants with over 50% hit rate
extracted_part_id <- unique(structured_vl_extracted$svl_part_id)


# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
type <- NULL
task<- NULL
same_or_diff <- NULL
test_phase <- NULL
svl<- NULL
domain <- NULL
range <- NULL
upper_bound <- NULL
lower_bound <- NULL
this_range <- NULL
number_rts <- NULL

# For each participant, extract id
# Assign domain and type
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in extracted_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "non-linguistic")
  task <- append(task, "vl")
  type <- append (type, "structured")
  same_or_diff <- append (same_or_diff, "different")
  test_phase <- append (test_phase, "vsl")
  mean_rt <- append(mean_rt, round(mean(structured_vl_extracted$svl_rt[structured_vl_extracted$svl_part_id==id]),digits=3))
  number_rts <- append(number_rts, length(!is.na(structured_vl_extracted$svl_rt[structured_vl_extracted$svl_part_id==id])))
  rt_slope <- append (rt_slope, round(summary(lm(structured_vl_extracted$svl_rt[structured_vl_extracted$svl_part_id==id]~structured_vl_extracted$targ_index[structured_vl_extracted$svl_part_id==id]))$coefficient[2,1],digits = 4))
  data_this_id <- (structured_vl_extracted[ which(structured_vl_extracted$svl_part_id==id),])
  this_range<- range(data_this_id$svl_rt, na.rm = TRUE)
  range <- append (range, (this_range[2]-this_range[1]))
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

# Combine data for each participant
svl <- data.frame(part_id, task, same_or_diff, test_phase, domain,type,mean_rt, range, upper_bound, lower_bound, rt_slope)

number_rts
mean(number_rts)
sd(number_rts)
length(number_rts)



# TEST: find mean rt_slope
mean_svl_rt_slope <- mean (svl$rt_slope)
# It should be negative
mean_svl_rt_slope



# ******************** CONDITION 8: STRUCTURED VV *******************

# Separate structured and structured conditions
structured_vv <- vv_data[ which(vv_data$condition== "S"),]

# Find all of the triplets presented
structured_vv$triplet <- rep (do.call(paste, as.data.frame(t(matrix(structured_vv$image, 3)), stringsAsFactors=FALSE)), each = 3)
structured_vv$triplet <- gsub("Alien", "", structured_vv$triplet)

# Remove the mistaken 10-21-22 triplet
structured_vv <- (dplyr::filter(structured_vv, triplet!="10 11 22"))

# Index the rows
structured_vv <- tibble::rowid_to_column(structured_vv, "index")

# Identify the rows when this condition's target was presented
structured_vv_targets <- structured_vv[which(structured_vv$structured_targ==structured_vv$image),]

# Identify the structured target for each participant
svv_targs <- distinct(structured_vv_targets %>%
                        group_by(part_id) %>%
                        summarise(structured_targ = structured_targ))

# TEST: Create a data frame to check the number of lines per participant
list_part_id <- unique(structured_vv_targets$part_id)
part_id <- NULL
total_lines <- NULL
for(id in list_part_id){
  part_id <- append(part_id, id)
  total_lines <- append(total_lines, nrow(structured_vv_targets[which(structured_vv$part_id==id),]))
}
svv_line_number <- data.frame(part_id, total_lines)
# There should be 32 entries (for the 32 participants)
length(svv_line_number$part_id)
# They should all contain 276 lines = 288 - (4 triplets with "10 11 22" x 3 stimuli)
svv_line_number$total_lines

# Identify response times to target stimuli. Include times when participant responded while target was displayed, or during preceding/ fovvowing stimulus ---------------------------------------------
# Initialize variables to track participant ID, condition, modality, task, and reaction time (RT)
svv_part_id <- NULL
svv_rt <- NULL

# Track the cases for calculating each type of reaction time
# NOTE: These variables are for internal checking only and can be commented out below in case of bugs
# Case 1: The participant responds during the target, which is the first trial in a block
svv_case1 <- NULL
# Case 2: The participant responds to the trial directly fovvowing the target, and the target is the first trial in a block
svv_case2 <- NULL
# Case 3: Anticipation of target, participant responded to stimulus directly preceding target
svv_case3 <- NULL
# Case 4: Response to target during the target trial
svv_case4 <- NULL
# Case 5: Delay from target, participant responded to stimulus directly fovvowing target
svv_case5 <- NULL
# Case 6: Missed target, record NA reaction time
svv_case6 <- NULL

# Isolate participants' response times.

# Include rows when the participant responded to stimuli adjacent to the target (i.e. any time that the participant pressed the button within one stimulus before or after the target)
for (i in structured_vv_targets$index) {
  # Isolate the ID number
  svv_part_id <-
    append(svv_part_id, paste(structured_vv[i, ]$part_id))
  
  # Check if you are looking at the first trial in the block. If so, the target does not have a preceding target
  if ((structured_vv[i, ]$trial_num %% 48 == 1)
      # Check if the participant responded during the target trial
      & !is.na(structured_vv[i, ]$v_rt)) {
    # If so, count the response time from the target stimulus
    svv_rt <- append (svv_rt, structured_vv[i, ][, "v_rt"])
    svv_case1 <- append (svv_case1, i)
  } 
  
  # If it's the first trial and there was no target keypress
  else if (((structured_vv[i, ]$trial_num %%48 == 1) & (is.na(structured_vv[i,]$v_rt)))
           # Check that the following stimulus was not also a target from the same block (to avoid counting the same keypress twice)
           & !((i + 1 %in% structured_vv_targets) & floor(structured_vv[i + 1, ]$trial_num/48)== floor(structured_vv[i,]$trial_num/48))) {
    # Then count the response time from the following stimulus
    svv_rt <- append (svv_rt, 1000 + (structured_vv[i + 1, ][, "v_rt"]))
    svv_case2 <- append (svv_case2, i)
  }
  
  # Otherwise, if the participant responded during the stimulus preceding the target
  else if (!is.na(structured_vv[i - 1, ] [, "v_rt"])
           # and the preceding stimulus was not also a target
           & !((structured_vv[i - 1, ][, "structured_targ"] == (structured_vv[i - 1, ][, "image"])))
           #  and two stimuli prior was not also a target from the same block
           & ! ((structured_vv[i - 2, ][, "structured_targ"] == structured_vv[i - 2, ][, "image"])
                & floor(structured_vv[i,]$trial_num/48) == floor(structured_vv[i-2,]$trial_num/48))
           # and the preceding stimulus came from the same block
           & floor((structured_vv[i, ])$trial_num/48) == floor(structured_vv[i - 1, ]$trial_num/48)
  )
  {
    # Count the response time as how much sooner they responded than when the stimulus was presented (anticipation)
    svv_rt <- append(svv_rt, (structured_vv[i - 1, ][, "v_rt"] - 1000))
    svv_case3 <- append (svv_case3, i)  }
  
  # Otherwise, if the participant responded during the target
  else if (!is.na(structured_vv[i, ] [, "v_rt"])
           # and, because structured, the previous trial had no keypress
           &
           (is.na(structured_vv[i - 1, ][, "v_rt"]))){
    # Count their response time as the keypress
    svv_rt <-
      append(svv_rt, (structured_vv[i, ][, "v_rt"]))
    svv_case4 <- append (svv_case4, i)
  }
  
  # Otherwise, if the participant responded after the target
  else if (!is.na(structured_vv[i + 1, ]$v_rt > 0)
           # And the following trial came from the same block
           & floor(structured_vv[i, ]$trial_num/48) == floor(structured_vv[i + 1, ]$trial_num/48)
           # Check that EITHER the following stimulus either was also not a target
           & (
             !((i + 1) %in% structured_vv_targets) |
             # OR, if the following stimulus was also a target, that it also had a delay
             (((i + 1) %in% structured_vv_targets) &
              !is.na(structured_vv[i + 2, ]$v_rt) &
              floor(structured_vv[i, ]$trial_num/48) == floor(structured_vv[i + 2, ]$trial_num/48)
             )
           )
           # Also check that EITHER two stimuli following was not also a target from the same block (to avoid counting the same keypress twice)
           &
           (
             !((i + 2) %in% structured_vv_targets) |
             floor(structured_vv[i, ]$trial_num/48) == floor(structured_vv[i + 2, ]$trial_num/48) |
             # OR, if two stimuli following was also a target (from the same block),
             ((i + 2) %in% structured_vv_targets) &
             floor(structured_vv[i, ]$trial_num/48) == floor(structured_vv[i + 2, ]$trial_num/48)
             # that it also had a delay from the same block
             &
             !is.na(
               structured_vv[i + 3, ]$v_rt &
               floor(structured_vv[i, ]$trial_num/48) == floor(structured_vv[i + 3, ]$trial_num/48)
             )
           )) {
    # Count their response time as how much later they responded than when the stimulus was presented
    svv_rt <-
      append(svv_rt, (1000 + structured_vv[i + 1, ][, "v_rt"]))
    svv_case5 <- append (svv_case5, i)
    
    # Otherwise, record the miss with a reaction time of NA
  } else {
    svv_rt <- append(svv_rt, NA)
    svv_case6 <- append (svv_case6, i)
  }
}

# Match id and response times
structured_vv_extracted <- data.frame(svv_part_id, svv_rt)

# Reindex the trial numbers for only trials with response times -----------------------------------------------------------------------------------------------------

# List unique participant IDs for this condition
extracted_part_id <- unique(structured_vv_extracted$svv_part_id)

# Find the number of targets shown to each participant
target_sum <- NULL
for(i in extracted_part_id){target_sum <- append(target_sum,sum(structured_vv_extracted$svv_part_id==i))}

# TEST: This should be equal to 32 (for the 32 participants)
length (target_sum)
# TEST: This should contain a vector full of 24s (if the target was not "Alien22") and 20s (if it was)
# note that sit_a_054's file is cut off midway through the activity and only saw 16 targets
target_sum

# For each participant, index the targets
targ_index <- NULL
for (i in target_sum) {targ_index <- append (targ_index, rep(1:i, 1))}

# Add the targets' indices
structured_vv_extracted$targ_index <- targ_index

# Remove any values of NA
# NOTE: This removes sit_a_010 who has no keypresses for the target
structured_vv_extracted <- structured_vv_extracted[!is.na(structured_vv_extracted$svv_rt),]

# Add the target 
structured_vv_extracted <- left_join(structured_vv_extracted, svv_targs, by = c("svv_part_id" = "part_id"))

# List unique participant IDs for this condition
extracted_part_id <- unique(structured_vv_extracted$svv_part_id)

# Calculate mean rt and rt_slope  -----------------------------------------------------------------------------------------------------

#There are 24 targets for each participant. Some may have a low hit rate (responded to 12 targets or less)
low_hits<-NULL
# Find people with a low hit rate
for (id in extracted_part_id){
  if (length(!is.na(structured_vv_extracted[which(structured_vv_extracted$svv_part_id==id),]$svv_rt))<13)
  {low_hits<-append(low_hits, id)}
}

# Remove people with low hit rate
structured_vv_extracted <- structured_vv_extracted[! structured_vv_extracted$svv_part_id %in% low_hits, ]
# Find only participants with over 50% hit rate
extracted_part_id <- unique(structured_vv_extracted$svv_part_id)

# Define variables
mean_rt <- NULL
rt_slope <- NULL
part_id <- NULL
type <- NULL
task<- NULL
same_or_diff <- NULL
test_phase <- NULL
svv<- NULL
domain <- NULL
range <- NULL
upper_bound <- NULL
lower_bound <- NULL
this_range <- NULL
number_rts <- NULL

# For each participant, extract id
# Assign domain and type
# Calculate and record mean_rt, rt_slope, upper bound, and lower bound
for(id in extracted_part_id){
  part_id <- append(part_id, id)
  domain <- append(domain, "non-linguistic")
  task <- append(task, "vv")
  type <- append (type, "structured")
  same_or_diff <- append (same_or_diff, "same")
  test_phase <- append (test_phase, "vsl")
  mean_rt <- append(mean_rt, round(mean(structured_vv_extracted$svv_rt[structured_vv_extracted$svv_part_id==id]),digits=3))
  rt_slope <- append (rt_slope, round(summary(lm(structured_vv_extracted$svv_rt[structured_vv_extracted$svv_part_id==id]~structured_vv_extracted$targ_index[structured_vv_extracted$svv_part_id==id]))$coefficient[2,1],digits = 4))
  number_rts <- append(number_rts, length(!is.na(structured_vv_extracted$svv_rt[structured_vv_extracted$svv_part_id==id])))
  data_this_id <- (structured_vv_extracted[ which(structured_vv_extracted$svv_part_id==id),])
  this_range<- range(data_this_id$svv_rt, na.rm = TRUE)
  range <- append (range, (this_range[2]-this_range[1]))
  upper_bound <- append (upper_bound,this_range[1])
  lower_bound <- append (lower_bound,this_range[2])
}

number_rts
mean(number_rts)
sd(number_rts)
length(number_rts)

# Combine data for each participant
svv <- data.frame(part_id, task, same_or_diff, test_phase, domain,type,mean_rt, range, upper_bound, lower_bound, rt_slope)


# TEST: find mean rt_slope
mean_svv_rt_slope <- mean (svv$rt_slope)
# It should be negative
mean_svv_rt_slope


# Bind conditions together--------------------------------------------------------------------------------------------------------------------------------------------------

# Bind conditions
indiv_rt_slope<- data.frame(rbind(rll, rlv, rvl, rvv, sll, slv, svl, svv))

write.csv(indiv_rt_slope, "/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_indiv_rt_slope.csv")


sd_rll_rt_slope <- sd(rll$rt_slope)
sd_rlv_rt_slope <- sd(rlv$rt_slope)
sd_rvl_rt_slope <- sd(rvl$rt_slope)
sd_rvv_rt_slope <- sd(rvv$rt_slope)
sd_sll_rt_slope <- sd(sll$rt_slope)
sd_slv_rt_slope <- sd(slv$rt_slope)
sd_svl_rt_slope <- sd(svl$rt_slope)
sd_svv_rt_slope <- sd(svv$rt_slope)



sd_slv_mean_rt <- sd(slv$mean_rt)
mean_slv_mean_rt <- mean(slv$mean_rt)
sd_rvl_mean_rt <- sd(rvl$mean_rt)
mean_rvl_mean_rt <- mean(rvl$mean_rt)
sd_svl_mean_rt <- sd(svl$mean_rt)
mean_svl_mean_rt <- mean(svl$mean_rt)
sd_rlv_mean_rt <- sd(rlv$mean_rt)
mean_rlv_mean_rt <- mean(rlv$mean_rt)

sd_svv_mean_rt <- sd(svv$mean_rt)
mean_svv_mean_rt <- mean(svv$mean_rt)
sd_rvv_mean_rt <- sd(rvv$mean_rt)
mean_rvv_mean_rt <- mean(rvv$mean_rt)
sd_rll_mean_rt <- sd(rll$mean_rt)
mean_rll_mean_rt <- mean(rll$mean_rt)
sd_sll_mean_rt <- sd(sll$mean_rt)
mean_sll_mean_rt <- mean(sll$mean_rt)


# Summarize rt_slope
indiv_rt_slope_wide <- cast(indiv_rt_slope, part_id ~ task, mean, value = 'rt_slope')
indiv_rt_slope_wide<- merge(indiv_rt_slope_wide, picture_vocab, by = "part_id", all=TRUE)
indiv_rt_slope_wide <- cbind(indiv_rt_slope_wide, "same_or_diff")
colnames(indiv_rt_slope_wide)[9] <- "same_or_diff"
all_same <- indiv_rt_slope_wide[ which(indiv_rt_slope_wide$ll>0), ]
all_same$same_or_diff <- ("same")
all_diff <- indiv_rt_slope_wide[ which(indiv_rt_slope_wide$lv>0), ]
all_diff$same_or_diff <- ("different")
indiv_rt_slope_wide <- rbind(all_same, all_diff)

# Write rt_slope summary into a file
write.csv(indiv_rt_slope_wide, "/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_rts_vocab_wide.csv")

# Summarize mean_rt
indiv_rt_wide <- cast(indiv_rt_slope, part_id ~ task, mean, value = 'mean_rt')
indiv_rt_wide<- merge(indiv_rt_wide, picture_vocab, by = "part_id", all=TRUE)
indiv_rt_wide <- cbind(indiv_rt_wide, "same_or_diff")
colnames(indiv_rt_wide)[9] <- "same_or_diff"
all_same <- indiv_rt_wide[ which(indiv_rt_wide$ll>0), ]
all_same$same_or_diff <- ("same")
all_diff <- indiv_rt_wide[ which(indiv_rt_wide$lv>0), ]
all_diff$same_or_diff <- ("different")
indiv_rt_wide <- rbind(all_same, all_diff)

# Write mean_rt summary into a file
write.csv(indiv_rt_wide, "/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_mean_rt_vocab.csv")


# Find group-level mean rt_slope accross tasks------------------------------------------------------------------------------------

group_rt_slope <- NULL
mean_struct_rt_slope <- NULL
mean_rand_rt_slope <- NULL
task <- NULL
mean_struct_rt <- NULL
mean_rand_rt <- NULL

# Find mean ll rt slope across participants
task <- append (task, paste ("ll"))
mean_struct_rt_slope <- append(mean_struct_rt_slope, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="structured" 
                                                                                      & indiv_rt_slope$task== "ll"), ]$rt_slope), digits =3))
mean_rand_rt_slope <- append(mean_rand_rt_slope, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="random" 
                                                                                  & indiv_rt_slope$task== "ll"), ]$rt_slope), digits =3))
mean_struct_rt <- append(mean_struct_rt, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="structured" 
                                                                          & indiv_rt_slope$task== "ll"), ]$mean_rt), digits =3))
mean_rand_rt <- append(mean_rand_rt, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="random" 
                                                                      & indiv_rt_slope$task== "ll"), ]$mean_rt), digits =3))

# Find mean lv rt slope across participants
task <- append (task, paste ("lv"))
mean_struct_rt_slope <- append(mean_struct_rt_slope, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="structured" 
                                                                                      & indiv_rt_slope$task== "lv"), ]$rt_slope), digits =3))
mean_rand_rt_slope <- append(mean_rand_rt_slope, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="random" 
                                                                                  & indiv_rt_slope$task== "lv"), ]$rt_slope), digits =3))
mean_struct_rt <- append(mean_struct_rt, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="structured" 
                                                                          & indiv_rt_slope$task== "lv"), ]$mean_rt), digits =3))
mean_rand_rt <- append(mean_rand_rt, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="random" 
                                                                      & indiv_rt_slope$task== "lv"), ]$mean_rt), digits =3))


# Find mean vl rt slope across participants
task <- append (task, paste ("vl"))
mean_struct_rt_slope <- append(mean_struct_rt_slope, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="structured" 
                                                                                      & indiv_rt_slope$task== "vl"), ]$rt_slope), digits =3))
mean_rand_rt_slope <- append(mean_rand_rt_slope, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="random" 
                                                                                  & indiv_rt_slope$task== "vl"), ]$rt_slope), digits =3))
mean_struct_rt <- append(mean_struct_rt, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="structured" 
                                                                          & indiv_rt_slope$task== "vl"), ]$mean_rt), digits =3))
mean_rand_rt <- append(mean_rand_rt, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="random" 
                                                                      & indiv_rt_slope$task== "vl"), ]$mean_rt), digits =3))


# Find mean vv rt slope across participants
task <- append (task, paste ("vv"))
mean_struct_rt_slope <- append(mean_struct_rt_slope, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="structured" 
                                                                                      & indiv_rt_slope$task== "vv"), ]$rt_slope), digits =3))
mean_rand_rt_slope <- append(mean_rand_rt_slope, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="random" 
                                                                                  & indiv_rt_slope$task== "vv"), ]$rt_slope), digits =3))
mean_struct_rt <- append(mean_struct_rt, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="structured" 
                                                                          & indiv_rt_slope$task== "vv"), ]$mean_rt), digits =3))
mean_rand_rt <- append(mean_rand_rt, round(mean(indiv_rt_slope[ which(indiv_rt_slope$type=="random" 
                                                                      & indiv_rt_slope$task== "vv"), ]$mean_rt), digits =3))


# Combine group rt data into one data frame
group_rt_slope <- data.frame(cbind(task, mean_rand_rt_slope, mean_struct_rt_slope, mean_struct_rt, mean_rand_rt))

# Write rt slope data into a file
write.csv(group_rt_slope, "/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_rt_slope_group.csv")

# Write RT data for each individual target into one file
random_ll_points <- random_ll_extracted %>%
  dplyr::rename("part_id" = "rll_part_id") %>%
  dplyr::rename("target_rt" = "rll_rt") %>%
  dplyr::rename("target" = "random_targ") %>%
  mutate(type = "random") %>%
  mutate(domain = "linguistic") %>%
  mutate(same_or_diff = "same")

random_lv_points <- random_lv_extracted %>%
  dplyr::rename("part_id" = "rlv_part_id") %>%
  dplyr::rename("target_rt" = "rlv_rt") %>%
  dplyr::rename("target" = "random_targ") %>%
  mutate(type = "random") %>%
  mutate(domain = "non-linguistic") %>%
  mutate(same_or_diff = "different")

random_vl_points <- random_vl_extracted %>%
  dplyr::rename("part_id" = "rvl_part_id") %>%
  dplyr::rename("target_rt" = "rvl_rt") %>%
  dplyr::rename("target" = "random_targ") %>%
  mutate(type = "random") %>%
  mutate(domain = "linguistic") %>%
  mutate(same_or_diff = "different")

random_vv_points <- random_vv_extracted %>%
  dplyr::rename("part_id" = "rvv_part_id") %>%
  dplyr::rename("target_rt" = "rvv_rt") %>%
  dplyr::rename("target" = "random_targ") %>%
  mutate(type = "random") %>%
  mutate(domain = "non-linguistic") %>%
  mutate(same_or_diff = "same")

structured_ll_points <- structured_ll_extracted %>%
  dplyr::rename("part_id" = "sll_part_id") %>%
  dplyr::rename("target_rt" = "sll_rt") %>%
  dplyr::rename("target" = "structured_targ") %>%
  mutate(type = "structured") %>%
  mutate(domain = "linguistic") %>%
  mutate(same_or_diff = "same")

structured_lv_points <- structured_lv_extracted %>%
  dplyr::rename("part_id" = "slv_part_id") %>%
  dplyr::rename("target_rt" = "slv_rt") %>%
  dplyr::rename("target" = "structured_targ") %>%
  mutate(type = "structured") %>%
  mutate(domain = "linguistic") %>%
  mutate(same_or_diff = "different")

structured_vl_points <- structured_vl_extracted %>%
  dplyr::rename("part_id" = "svl_part_id") %>%
  dplyr::rename("target_rt" = "svl_rt") %>%
  dplyr::rename("target" = "structured_targ") %>%
  mutate(type = "structured") %>%
  mutate(domain = "non-linguistic") %>%
  mutate(same_or_diff = "different")

structured_vv_points <- structured_vv_extracted %>%
  dplyr::rename("part_id" = "svv_part_id") %>%
  dplyr::rename("target_rt" = "svv_rt") %>%
  dplyr::rename("target" = "structured_targ") %>%
  mutate(type = "structured") %>%
  mutate(domain = "non-linguistic") %>%
  mutate(same_or_diff = "same")


# Combine individual RTs into one data frame
indiv_rt_points <- data.frame(rbind(random_ll_points, random_lv_points, random_vv_points, random_vl_points, 
                                    structured_ll_points, structured_lv_points, structured_vl_points, structured_vv_points)) %>%
  dplyr::rename("rt" = "target_rt")

if(os == "osx") {write.csv(indiv_rt_points, "/Volumes/data/projects/completed_projects/sit/analysis/summaries/indiv_rts.csv")
  } else {write.csv(indiv_rt_points, here("../summaries/indiv_rts.csv"))}



