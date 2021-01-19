#  SIT PLOTTING
#  Violet Kozloff
#  Created with support from Zhenghan Qi and An Nguyen
#  Last modified December 23rd, 2020
#  This script creates visualizations for accuracy and reaction time
#  NOTE: Accuracies have been previously calculated in sit_accuracy.R
#  NOTE: Reaction time means and slopes have been previously calculated in sit_rt_slope.R 

# ************ PLOT MEAN ACCURACY BY TASK AND GROUP ************ 


# install.packages("afex")
# install.packages("ggplot2")
# install.packages("cowplot")
# install.packages("emmeans")
# install.packages("doBy")
require("ggplot2")  
library("cowplot")
library("doBy")

# Remove objects in environment
rm(list=ls())

ggplot2::theme_set(theme_grey())

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


if(os == "osx") {accuracies <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_accuracy_long.csv")
} else { accuracies <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/sit_accuracy_long.csv")}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  library(doBy)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

accuracies$Group<- accuracies$same_or_diff

accuracies$Stimulus<- ifelse(accuracies$test_phase=="lsl", "Letter", "Image")

# Remove one participant's data from one task
accuracies<-accuracies[which(accuracies$part_id!="sit_a_054"),]

acc <- summarySE(accuracies, measurevar="accuracy", groupvars=c("Group", "Stimulus"))


#set group 1 = Same, 2 = Different (line graph requires continuous variable)
acc$Group<- as.numeric(ifelse(acc$Group=="same", "1", "2"))


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(acc, aes(x=Group, y=accuracy, colour=Stimulus, group=Stimulus, shape = Stimulus)) +
  geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd, aes(linetype = Stimulus)) +
  geom_point(fill = "white", position=pd, size=5) +
  xlab("Group") +
  ylab("Accuracy") +
  scale_colour_hue(l = 50) +
  scale_x_discrete( limits=c("Same","Different")) +
  theme_classic() +
  theme(plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30)) +
  ylim(0.50, 0.75)



ggplot(acc, aes(x=Stimulus, y=accuracy, colour=Stimulus, shape = Stimulus)) +
  geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd, aes(linetype = Stimulus)) +
  geom_point(fill = "white", position=pd, size=5) +
  facet_wrap(~Group) +
  xlab("Stimulus") +
  ylab("Accuracy") +
  scale_colour_hue(l = 50) +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  ylim(0.50, 0.75)




# ************ PLOT MEAN RTS BY BLOCK TYPE AND GROUP ************ 

if(os == "osx") {indiv_rts <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_indiv_rt_slope.csv")
} else { indiv_rts <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/sit_indiv_rt_slope.csv")}

same <- dplyr::filter(indiv_rts, same_or_diff=="same")

same$Stimulus<- ifelse(same$domain=="linguistic", "Letter", "Image")

samec <- summarySE(same, measurevar="mean_rt", groupvars=c("Stimulus", "type"))


# Also plot by trial index

if(os == "osx") {indiv_rt_data <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/indiv_rts.csv")
} else { indiv_rt_data <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/indiv_rts.csv")}


same_ling <- dplyr::filter(indiv_rt_data, domain=="linguistic", same_or_diff == "same")
same_ling_plot<-dplyr::summarise(dplyr::group_by(same_ling,type,targ_index), n = dplyr::n(),
                                 mean=mean(rt,na.rm = T), sd=sd(rt,na.rm = T),se = sd/sqrt(n))
ggplot(data = same_ling_plot, aes(x=targ_index, y=mean, color = type))+
  geom_line() +geom_point()  +
  scale_color_manual(values=c('red','blue'))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(2,24,2))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  +
  labs(x = "Trials", y = "Response Time (ms)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))

ggsave("/Volumes/data/projects/completed_projects/sit/analysis/figures/same_ling.png")

same_nonling <- dplyr::filter(indiv_rt_data, domain=="non-linguistic", same_or_diff == "same")
same_nonling_plot<-dplyr::summarise(dplyr::group_by(same_nonling,type,targ_index), n = dplyr::n(),
                                         mean=mean(rt,na.rm = T), sd=sd(rt,na.rm = T),se = sd/sqrt(n))
ggplot(data = same_nonling_plot, aes(x=targ_index, y=mean, color = type))+
  geom_line() +geom_point()  +
  scale_color_manual(values=c('red','blue'))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(2,24,2))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  +
  labs(x = "Trials", y = "Response Time (ms)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))

ggsave("/Volumes/data/projects/completed_projects/sit/analysis/figures/same_nonling.png")

different_ling <- dplyr::filter(indiv_rt_data, domain=="linguistic", same_or_diff == "different")
different_ling_plot<-dplyr::summarise(dplyr::group_by(different_ling,type,targ_index), n = dplyr::n(),
                                 mean=mean(rt,na.rm = T), sd=sd(rt,na.rm = T),se = sd/sqrt(n))
ggplot(data = different_ling_plot, aes(x=targ_index, y=mean, color = type))+
  geom_line() +geom_point()  +
  scale_color_manual(values=c('red','blue'))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(2,24,2))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  +
  labs(x = "Trials", y = "Response Time (ms)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))

ggsave("/Volumes/data/projects/completed_projects/sit/analysis/figures/different_ling.png")

different_nonling <- dplyr::filter(indiv_rt_data, domain=="non-linguistic", same_or_diff == "different")
different_nonling_plot<-dplyr::summarise(dplyr::group_by(different_nonling,type,targ_index), n = dplyr::n(),
                                      mean=mean(rt,na.rm = T), sd=sd(rt,na.rm = T),se = sd/sqrt(n))
ggplot(data = different_nonling_plot, aes(x=targ_index, y=mean, color = type))+
  geom_line() +geom_point()  +
  scale_color_manual(values=c('red','blue'))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(2,24,2))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  +
  labs(x = "Trials", y = "Response Time (ms)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))

ggsave("/Volumes/data/projects/completed_projects/sit/analysis/figures/different_nonling.png")



both_ling <- dplyr::filter(indiv_rt_data, domain=="linguistic")
both_ling_plot<-dplyr::summarise(dplyr::group_by(both_ling,type,targ_index), n = dplyr::n(),
                                 mean=mean(rt,na.rm = T), sd=sd(rt,na.rm = T),se = sd/sqrt(n))
ggplot(data = both_ling_plot, aes(x=targ_index, y=mean, color = type))+
  geom_line() +geom_point()  +
  scale_color_manual(values=c('red','blue'))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(2,24,2))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  +
  labs(x = "Trials", y = "Response Time (ms)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))

ggsave("/Volumes/data/projects/completed_projects/sit/analysis/figures/both_ling.png")



both_nonling <- dplyr::filter(indiv_rt_data, domain=="non-linguistic")
both_nonling_plot<-dplyr::summarise(dplyr::group_by(both_nonling,type,targ_index), n = dplyr::n(),
                                         mean=mean(rt,na.rm = T), sd=sd(rt,na.rm = T),se = sd/sqrt(n))
ggplot(data = both_nonling_plot, aes(x=targ_index, y=mean, color = type))+
  geom_line() +geom_point()  +
  scale_color_manual(values=c('red','blue'))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(2,24,2))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  +
  labs(x = "Trials", y = "Response Time (ms)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))

ggsave("/Volumes/data/projects/completed_projects/sit/analysis/figures/both_nonling.png")



# set type 2 = random, type 1 = structured (line graph requires continuous variable)
samec$type<- as.numeric(ifelse(samec$type=="random", 2, 1))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(samec, aes(x=type, y=mean_rt, colour=Stimulus, group=Stimulus, shape = Stimulus)) +
  geom_errorbar(aes(ymin=mean_rt-se, ymax=mean_rt+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd, aes(linetype = Stimulus)) +
  geom_point(fill = "white", position=pd, size=5) +
  xlab("Block Type") +
  ylab("Mean Reaction Time (ms)") +
  scale_colour_hue(l = 50) +
  scale_x_discrete( limits=c("structured","random")) +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))



diff <- dplyr::filter(indiv_rts, same_or_diff=="different")

diff$Stimulus<- ifelse(diff$domain=="linguistic", "Letter", "Image")

diffc <- summarySE(diff, measurevar="mean_rt", groupvars=c("Stimulus", "type"))


# set type 2 = random, type 1 = structured (line graph requires continuous variable)
diffc$type<- as.numeric(ifelse(diffc$type=="random", 2, 1))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(diffc, aes(x=type, y=mean_rt, colour=Stimulus, group=Stimulus, shape = Stimulus)) +
  geom_errorbar(aes(ymin=mean_rt-se, ymax=mean_rt+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd, aes(linetype = Stimulus)) +
  geom_point(fill = "white", position=pd, size=5) +
  xlab("Block Type") +
  ylab("Mean Reaction Time (ms)") +
  scale_colour_hue(l = 50) +
  scale_x_discrete( limits=c("structured","random")) +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))


# ************ PLOT MEAN RT SLOPE BY TASK AND GROUP ************ 

rt_slope <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_indiv_rt_slope.csv")


same <- dplyr::filter(rt_slope, same_or_diff=="same")

same$Stimulus<- ifelse(same$domain=="linguistic", "Letter", "Image")

samec <- summarySE(same, measurevar="rt_slope", groupvars=c("Stimulus", "type"))


# set type 2 = random, type 1 = structured (line graph requires continuous variable)
samec$type<- as.numeric(ifelse(samec$type=="random", 2, 1))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(samec, aes(x=type, y=rt_slope, colour=Stimulus, group=Stimulus, shape = Stimulus)) +
  geom_errorbar(aes(ymin=rt_slope-se, ymax=rt_slope+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd, aes(linetype = Stimulus)) +
  geom_point(fill = "white", position=pd, size=5) +
  xlab("Block Type") +
  ylab("Reaction Time Slope") +
  scale_colour_hue(l = 50) +
  #ggtitle("Same Group: Reaction Time Slope Across \nStimulus Types") +
  scale_x_discrete( limits=c("structured","random")) +
  theme_classic() +
  theme(plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30))

different <- dplyr::filter(rt_slope, same_or_diff=="different")

different$Stimulus<- ifelse(different$domain=="linguistic", "Letter", "Image")

differentc <- summarySE(different, measurevar="rt_slope", groupvars=c("Stimulus", "type"))

# set type 2 = random, type 1 = structured (line graph requires continuous variable)
differentc$type<- as.numeric(ifelse(differentc$type=="random", 2, 1))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(differentc, aes(x=type, y=rt_slope, colour=Stimulus, group=Stimulus, shape = Stimulus)) +
  geom_errorbar(aes(ymin=rt_slope-se, ymax=rt_slope+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd, aes(linetype = Stimulus)) +
  geom_point(fill = "white", position=pd, size=5) +
  xlab("Block Type") +
  ylab("Reaction Time Slope") +
  scale_colour_hue(l = 50) +
  #ggtitle("different Group: Reaction Time Slope Across \nStimulus Types") +
  scale_x_discrete( limits=c("structured","random")) +
  theme_classic() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
