#  BLAST Online Session Data Cleaning
#  Violet Kozloff
#  September 10th, 2018 
#  Adapted from lsl_clean_file and mturk_combine_raw by An Nguyen
#  This script cleans LSL, SSL, TSL, and VSL files from the online session of the BLAST experiment
#  NOTE: Before running, make sure all files contain "responses" column (may need to be added manually)

# TO DO: Clean LSL
# TO DO: Test VSL for 2 files and see what happens
# TO DO: Make sure it works with multiple TSL, SSL, VSL
# TO DO: Make sure it can accept the weird files with no "responses" column
#  ****************************************************************************


# Prepare workspace ------------------------------------------------------------

# Set directory
setwd("/Users/vkozloff/Documents/qlab/analysis/blast-adults-online")
# Remove objects in environment
rm(list=ls())


# Prepare files ------------------------------------------------------------

# Set up input paths
lsl_input <- ("../../../blast_adult_online_data/original/lsl/")
ssl_input <- ("../../../blast_adult_online_data/original/ssl/")
tsl_input <- ("../../../blast_adult_online_data/original/tsl/")
vsl_input <- ("../../../blast_adult_online_data/original/vsl/")

# Set up output paths
lsl_output <- ("../../../blast_adult_online_data/clean/lsl/")
ssl_output <- ("../../../blast_adult_online_data/clean/ssl/")
tsl_output <- ("../../../blast_adult_online_data/clean/tsl/")
vsl_output <- ("../../../blast_adult_online_data/clean/vsl/")

# List files in input paths
lsl_original_files <- list.files(path=lsl_input, pattern="*.csv") 
ssl_original_files <- list.files(path=ssl_input, pattern="*.csv") 
tsl_original_files <- list.files(path=tsl_input, pattern="*.csv") 
vsl_original_files <- list.files(path=vsl_input, pattern="*.csv") 

# Clean TSL files ———————————————————————————————————

# Create a new file containing only the relevant columns in the output folder
tsl_clean <- function(file) {
  # Read files
  this_file <- read.csv(file)
  # Select relevant columns
  value <- c("rt", "trial_index","cond","targ","key_press","stimulus")
  newdata <- this_file[value]  
  # Create a column populated with the participant ID
  this_file["responses"] <- lapply(this_file["responses"], as.character)
  part_id <- substr(as.character(this_file["responses"[1]]), 14, 24)
  newdata$part_id <- part_id
  # Write clean file
  this_path<-file.path(tsl_output, basename(file))
  write.csv(newdata, file=(this_path))
}

# Apply function to all tsl files
for (file in tsl_original_files)
{
  tsl_clean(paste0(tsl_input,file))
}

# Create one file with all tsl information
tsl <- list()
tsl_clean_files <- list.files(path=tsl_output, pattern="*.csv") 
for(file in tsl_clean_files)
{
  assign(
    gsub(" ","",file), 
    read.csv(paste(tsl_output,file,sep="")))
}

for(file in tsl_clean_files){tsl <- append(tsl,list(eval(parse(text=file))))}

tsl <- do.call(rbind.data.frame, tsl)

# Standardize stimulus names
tsl$stimulus<- gsub(".wav","",tsl$stimulus)
tsl$stimulus<- gsub("../../tones/","",tsl$stimulus)


write.csv(tsl,"../../../blast_adult_online_data/clean/tsl/tsl.csv")

# Clean ssl files ———————————————————————————————————

# Create a new file containing only the relevant columns in the output folder
ssl_clean <- function(file) {
  # Read files
  this_file <- read.csv(file)
  # Select relevant columns
  value <- c("rt", "trial_index","cond","targ","key_press","stimulus")
  newdata <- this_file[value]  
  # Create a column populated with the participant ID
  this_file["responses"] <- lapply(this_file["responses"], as.character)
  part_id <- substr(as.character(this_file["responses"[1]]), 14, 24)
  newdata$part_id <- part_id
  # Write clean file
  this_path<-file.path(ssl_output, basename(file))
  write.csv(newdata, file=(this_path))
}

# Apply function to all ssl files
for (file in ssl_original_files)
{
  ssl_clean(paste0(ssl_input,file))
}

# Create one file with all ssl information
ssl <- list()
ssl_clean_files <- list.files(path=ssl_output, pattern="*.csv") 
for(file in ssl_clean_files)
{
  assign(
    gsub(" ","",file), 
    read.csv(paste(ssl_output,file,sep="")))
}

for(file in ssl_clean_files){ssl <- append(ssl,list(eval(parse(text=file))))}

ssl <- do.call(rbind.data.frame, ssl)

# Standardize stimulus names
ssl$stimulus<- gsub(".wav","",ssl$stimulus)
ssl$stimulus<- gsub("sound/","",ssl$stimulus)

write.csv(ssl,"../../../blast_adult_online_data/clean/ssl/ssl.csv")


# Clean vsl files ———————————————————————————————————

# Create a new file containing only the relevant columns in the output folder
vsl_clean <- function(file) {
  # Read files
  this_file <- read.csv(file)
  # Select relevant columns
  value <- c("rt", "trial_index","cond","targ","key_press","stimulus")
  newdata <- this_file[value]  
  # Create a column populated with the participant ID
  this_file["responses"] <- lapply(this_file["responses"], as.character)
  part_id <- substr(as.character(this_file["responses"[1]]), 14, 24)
  newdata$part_id <- part_id
  # Write clean file
  this_path<-file.path(vsl_output, basename(file))
  write.csv(newdata, file=(this_path))
}

# Apply function to all vsl files
for (file in vsl_original_files)
{
  vsl_clean(paste0(vsl_input,file))
}

# Create one file with all vsl information
vsl <- list()
vsl_clean_files <- list.files(path=vsl_output, pattern="*.csv") 

for(file in vsl_clean_files)
{
  assign(
    gsub(" ","",file), 
    read.csv(paste(vsl_output,file,sep="")))
}

for(file in vsl_clean_files){vsl <- append(vsl,list(eval(parse(text=file))))}

vsl <- do.call(rbind.data.frame, vsl)

# Standardize stimulus names
vsl$stimulus<- gsub(".jpg","",vsl$stimulus)
vsl$stimulus<- gsub(".png","",vsl$stimulus)
vsl$stimulus<- gsub("../../images/","",vsl$stimulus)

write.csv(vsl,"../../../blast_adult_online_data/clean/vsl/vsl.csv")



# Clean lsl files ———————————————————————————————————

# Set up input paths
#lsl_input 
path <- ("../../../blast_adult_online_data/original/lsl/")
# path <- "C:/Users/Qlab/Downloads/sayako/lsl/"

# Set up output paths
lsl_output <- ("../../../blast_adult_online_data/clean/lsl/")

# List files in input paths
#lsl_original_files
#files <- list.files(path=lsl_input, pattern="*.csv") 
files <- list.files(path=path, pattern="*.csv") 

file <- ("Documents/blast_adult_online_data/clean/lsl/blast_a_001.lsl")


# files <- list.files(path=path, pattern="*.csv") 

# Create a new file containing only the relevant columns in the output folder
lsl_clean <- function(file) {
 
   # From An's lsl_clean_file script  
  
  #Since LSL uses animation plug-in from jsPsych to make sure the present rate is smooth, the data layout is a bit different. This script is to extract the response time for LSL tasks. 
  # Extract 2 things: response time in "responses" and animation_sequence
  cleandata <- function(file) {
    
    lsl <- read.csv(file)
    #  extract the responses
    if(lsl[10,2]!=""){rep <- unlist(strsplit(paste(lsl[10,2]), split=',', fixed=TRUE))
    } else {
      # Separates into keypress, rt, and stimulus
      rep <- unlist(strsplit(paste(lsl[9,2]), split=',', fixed=TRUE))
    }
    #extract animation
    # TO DO: Make sure it's really here
    if(lsl[10,11]!=""){
      # Splits stimulus from time: the whole animation sequence that they saw (as an array)
      ani_stim <- unlist(strsplit(paste(lsl[10,11]), split=',', fixed=TRUE))
    } else{
      ani_stim <- unlist(strsplit(paste(lsl[9,12]), split=',', fixed=TRUE))
    }
    
    key= NULL
    rt  <- NULL
    stim_press <- NULL
    stim_disp <- NULL
    time <- NULL
    
    # Takes the cell in "responses" and splits it based on colon so that it is separate into keypress, rt, and stimulus
    for (j in seq(from=2, to=length(rep), by=3)) {
      # Just extracts RT from "rep", which has kepyrss, rt, and stimulus for each stimulus
      rt<- append(rt,as.numeric(unlist(strsplit(paste(rep[j]), split=':'))[2]))
    }
    
    
    for (k in seq(from=3, to=length(rep), by=3)) {
      # stim_press is all the stimuli where the participant responded
      stim_press<- append(stim_press, gsub('.{2}$','',unlist(strsplit(paste(rep[k]), split=':\"', fixed=TRUE))[2]))
      
    }
    
    for (i in seq(from=1, to=length(ani_stim), by=2)) {
      # Create a column of the stimuli that occurred during lsl exposure. Changes ani_stim from array to column.
      stim_disp<- rbind(stim_disp, gsub('.{1}$','',unlist(strsplit(paste(ani_stim[i]), split=':\"', fixed=TRUE))[2]))}
    
    # Extract the time from the ani_stim
    for (n in seq(from=2, to=length(ani_stim), by=2)) {
      time<- rbind(time, gsub('.{1}$','',unlist(strsplit(paste(ani_stim[n]), split=':', fixed=TRUE))[2]))}
    
    # these 2 tables are not the same length. 
    #Table 1 has all reaction times to all stimulus they responded to
    table1 <- data.frame(rt,stim_press)
    
    #Table 2 has info from animation sequence — when each stimulus was presented
    table2<-data.frame(stim_disp,time)
    
    # Trying to fit table 1 into table 2 depending on reaction time
    table2 <- table2[1:576,]
    # Col2 is stimpres from table 1
    table2$col2 <- "NA"
    #col1 signifies reaction time from table 1
    table2$col1 <- "NA"
    table1$stim_press <- paste(table1$stim_press)
    
    # stim_disp is the stimulus that was seen during the animation sequence
    table2$stim_disp <- paste(table2$stim_disp)
    table2$time <- as.numeric(paste(table2$time))
    for (i in seq(from=1,to =length(table1$stim_press),by=1)) {
      temp1 <- table1[i,]
      index <- 0
      min <- .Machine$integer.max
      
      
      # Difference between time and rt: time is when it appeared, rt is when they responded
      # If they press during blank, subtract the blank RT from the time the previous stimulus appeared
      # Always compare to when the target was appeared. If they respond during H, subtract the H rt 
      
      for  (j in seq(from=1,to =length(table2$stim_disp),by=1)) {
        temp2 <- table2[j,]
        if (temp2[3]=="NA") {
          if (temp1[2]==temp2[1]){
            if (abs(temp1[1]-temp2[2]) < min){
              min <- abs(temp2[2]-temp1[1])
              index <- j}
          }
        }
      }
      table2$col1[index] <- temp1[1]
      table2$col2[index] <- temp1[2]}
    table2$target <- lsl$targ[1]
    table2$cond <- lsl$cond[1]
    table2$par_id <- rep(gsub('.{2}$','',unlist(strsplit(paste(lsl$response[1]), split=':"', fixed=TRUE))[2]),length(lsl$responses[1]))
    table2<-as.matrix(table2)
    write.csv(table2,file=as.character(file))
  }
  
  
  }

# Apply function to all lsl files
for (file in files)
{
  lsl_clean(paste0(path,file))
}


# List files in output paths
lsl_clean_files <- list.files(path=lsl_output, pattern="*.csv") 


