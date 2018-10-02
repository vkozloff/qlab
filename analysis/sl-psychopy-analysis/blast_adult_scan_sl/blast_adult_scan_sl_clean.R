#  BLAST Online Session Data Cleaning
#  Violet Kozloff
#  September 25th, 2018 
#  This script cleans adult auditory and visual files from the MRI session of the BLAST experiment
#  ****************************************************************************


# Prepare workspace ------------------------------------------------------------

# Set directory
setwd("/Users/vkozloff/Documents/qlab/analysis/sl-psychopy-analysis/blast_adult_scan_sl")
# Remove objects in environment
rm(list=ls())

# Set up input paths
auditory_input <- ("../../../../blast_adult_scan_sl_data/original/auditory_original/")
visual_input <- ("../../../../blast_adult_scan_sl_data/original/visual_original/")

# Set up output paths
auditory_output <- ("../../../../blast_adult_scan_sl_data/clean/auditory_clean/")
visual_output <- ("../../../../blast_adult_scan_sl_data/clean/visual_clean/")

# List files in input paths
auditory_original_files <- list.files(path=auditory_input, pattern="*.csv") 
visual_original_files <- list.files(path=visual_input, pattern="*.csv") 


# Auditory cleaning ----------------------------------------------------

# Create a new file containing only the relevant columns in the output folder
auditory_clean <- function(file) {
# test
 # this_file <- read.csv("/Users/vkozloff/Documents/blast_adult_scan_sl_data/original/auditory_original/blast_a_022_auditory_2.csv")
    # Read files
  this_file <- read.csv(file)
  # Select relevant columns
  value <- c("expName", "Run", "condition", "trialnum", "soundFile", "PartID",
             "starget", "sound_block_key_resp.rt", 
             "ttarget", "tone_block_key_resp.rt")
  newdata <- this_file[value]  
  names(newdata)[names(newdata) == 'expName'] <- 'modality'
  names(newdata)[names(newdata) == 'Run'] <- 'run'
  names(newdata)[names(newdata) == 'trialnum'] <- 'trial_num'
  names(newdata)[names(newdata) == 'soundFile'] <- 'stimulus'
  names(newdata)[names(newdata) == 'PartID'] <- 'part_id'
  names(newdata)[names(newdata) == 'sound_block_key_resp.rt'] <- 'ssl_rt'
  names(newdata)[names(newdata) == 'tone_block_key_resp.rt'] <- 'tsl_rt'
  # Write file
  this_path<-file.path(auditory_output, basename(file))
  write.csv(newdata, file=(this_path))
}

# Apply function to all tsl files
for (file in auditory_original_files)
{
  auditory_clean(paste0(auditory_input,file))
}

# Create one file with all tsl information
auditory <- list()
auditory_clean_files <- list.files(path=auditory_output, pattern="*.csv") 
for(file in auditory_clean_files)
{
  assign(
    gsub(" ","",file), 
    read.csv(paste(auditory_output,file,sep="")))
}

for(file in auditory_clean_files){auditory <- append(auditory,list(eval(parse(text=file))))}

auditory <- do.call(rbind.data.frame, auditory)

# Standardize stimulus names
auditory$stimulus<- gsub(".wav","",auditory$stimulus)


write.csv(auditory,"../../../../blast_adult_scan_sl_data/clean/auditory_clean/auditory.csv")


# Visual cleaning ----------------------------------------------------

# Create a new file containing only the relevant columns in the output folder
visual_clean <- function(file) {
  # test
   # this_file <- read.csv("/Users/vkozloff/Documents/blast_adult_scan_sl_data/original/visual_original/blast_a_001_visual_1.csv")
  # Read files
  this_file <- read.csv(file)
  # Select relevant columns
  value <- c("expName", "Run", "condition", "trialnum", "image", "PartID",
             "ltarget", "l_block_trial_key_resp.rt",
             "vtarget", "v_block_trial_key_resp.rt")
  newdata <- this_file[value]  
  names(newdata)[names(newdata) == 'expName'] <- 'modality'
  names(newdata)[names(newdata) == 'Run'] <- 'run'
  names(newdata)[names(newdata) == 'trialnum'] <- 'trial_num'
  names(newdata)[names(newdata) == 'image'] <- 'stimulus'
  names(newdata)[names(newdata) == 'PartID'] <- 'part_id'
  names(newdata)[names(newdata) == 'l_block_trial_key_resp.rt'] <- 'lsl_rt'
  names(newdata)[names(newdata) == 'v_block_trial_key_resp.rt'] <- 'vsl_rt'
  # Write file
  this_path<-file.path(visual_output, basename(file))
  write.csv(newdata, file=(this_path))
}

# Apply function to all visual files
for (file in visual_original_files)
{
  visual_clean(paste0(visual_input,file))
}

# Create one file with all visual information
visual <- list()
visual_clean_files <- list.files(path=visual_output, pattern="*.csv") 
for(file in visual_clean_files)
{
  assign(
    gsub(" ","",file), 
    read.csv(paste(visual_output,file,sep="")))
}

for(file in visual_clean_files){visual <- append(visual,list(eval(parse(text=file))))}

visual <- do.call(rbind.data.frame, visual)

# Standardize stimulus names
visual$stimulus<- gsub(".png","",visual$stimulus)
visual$stimulus<- gsub(".bmp","",visual$stimulus)
visual$stimulus<- gsub("Alien","",visual$stimulus)
# TO DO: Make sure that the reason that stimulus appears as false is because it's F
visual$ltarget<- gsub("FALSE","f",visual$stimulus)

write.csv(visual,"../../../../blast_adult_scan_sl_data/clean/visual_clean/visual.csv")



