#  SIT STATISTICAL ANALYSIS
#  Violet Kozloff
#  Created with support from Zhenghan Qi
#  Last modified January 14th, 2021
#  This script finds and analyzes measures of statistical learning tasks involving structured and random triplets of letters and images
#  NOTE: Accuracies have been previously calculated in sit_accuracy.R
#  NOTE: Reaction time means and slopes have been previously calculated in sit_rt_slope.R 
#  ****************************************************************************

if(!("tidyverse" %in% installed.packages())) {install.packages("tidyverse")}
if(!("lme4" %in% installed.packages())) {install.packages("lme4")}
if(!("lmerTest" %in% installed.packages())) {install.packages("lmerTest")}
if(!("optimx" %in% installed.packages())) {install.packages("optimx")}
if(!("reshape" %in% installed.packages())) {install.packages("reshape")}
if(!("reshape2" %in% installed.packages())) {install.packages("reshape2")}
if(!("car" %in% installed.packages())) {install.packages("car")}
if(!("ez" %in% installed.packages())) {install.packages("ez")}
if(!("afex" %in% installed.packages())) {install.packages("afex")}
if(!("sjPlot" %in% installed.packages())) {install.packages("sjPlot")}
if(!("arm" %in% installed.packages())) {install.packages("arm")}

require("tidyverse")
require ("lme4")
require("lmerTest")
require("optimx")
require ("reshape2")
require("car")
require ("ez")
require("here")
require("afex")
require("sjPlot")
require("arm") #Andrew Gelman

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

# ************************ SEE IF GROUPS ARE MATCHED FOR DEMOGRAPHICS WITH CHI-SQUARE TEST AND T-TESTS ************************

if(os == "osx") {chi_square_data <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_accuracy_vocab_wide.csv")
} else { chi_square_data <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/sit_accuracy_vocab_wide.csv")}

if(os == "osx") {picture_vocab <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/data/clean/vocab_clean/vocab_clean.csv")
} else { picture_vocab <- read.csv("Z:/projects/completed_projects/sit/analysis/data/clean/vocab_clean/vocab_clean.csv")}

if(os == "osx") {rt_data <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/indiv_rts.csv")
} else { rt_data <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/indiv_rts.csv")}

# Subset only relevant data about the population
matched_data <- chi_square_data %>% dplyr::select (part_id, age, sex, score, same_or_diff)

# Chi-square test for gender
gender_table <- reshape::cast(matched_data,sex~same_or_diff,value = "score",length)
chisq.test(gender_table)

# T-test for vocab score by group
t.test(score~same_or_diff, data=matched_data) 

# Find means for each group
same_vocab<- mean((filter(chi_square_data, same_or_diff=="same"))$score, na.rm=TRUE)
diff_vocab<- mean((filter(chi_square_data, same_or_diff=="different"))$score, na.rm=TRUE)

# T-test for age by group 
t.test(age~same_or_diff, data=matched_data)

# *************************** ACCURACY ANALYSES *******************

ifelse(os == "osx", indiv_accuracies <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_accuracy_long.csv"), indiv_accuracies <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/sit_accuracy_long.csv"))

# Separate data by task and group
indiv_ll_accuracies <- dplyr::filter(indiv_accuracies, task =="ll")
indiv_vv_accuracies <- dplyr::filter(indiv_accuracies, task =="vv")
indiv_lv_accuracies <- dplyr::filter(indiv_accuracies, task =="lv")
indiv_vl_accuracies <- dplyr::filter(indiv_accuracies, task =="vl")
# Remove accuracies for participants with no accuracy score for one task
indiv_ll_accuracies<-indiv_vv_accuracies[which(indiv_ll_accuracies$part_id!="sit_a_054"),]
indiv_vv_accuracies<-indiv_vv_accuracies[which(indiv_vv_accuracies$part_id!="sit_a_054"),]


# Test whether group performance on each task was above chance  -------------------------------------------------------------------------------------------------------------------------------------

t.test(indiv_ll_accuracies$accuracy, alternative= "greater", mu=0.5)
t.test(indiv_vv_accuracies$accuracy, alternative= "greater", mu=0.5)
t.test(indiv_lv_accuracies$accuracy, alternative= "greater", mu=0.5)
t.test(indiv_vl_accuracies$accuracy, alternative= "greater", mu=0.5)

# Correlations for accuracy -------------------------------------------------------------------------------------------------------------------------------------

# Reformat and combine relevant data from indiv_accuracies and picture_vocab
acc_corr_data <- reshape::cast(indiv_accuracies, part_id ~ task, mean, value = 'accuracy')
acc_corr_data <- merge(acc_corr_data, picture_vocab, by = "part_id", all=TRUE)
acc_corr_data$lsl <- ifelse(!is.na(acc_corr_data$ll), acc_corr_data$ll, acc_corr_data$lv)
acc_corr_data$vsl <- ifelse(!is.na(acc_corr_data$vv), acc_corr_data$vv, acc_corr_data$vl)
acc_corr_data <- dplyr::rename(acc_corr_data, same_or_diff = group)

# Correlations collapsed across groups
vsl_acc_corr <- cor.test(acc_corr_data$vsl, acc_corr_data$score) 
lsl_acc_corr <- cor.test(acc_corr_data$lsl, acc_corr_data$score) 


# Correlations by group
same_acc_corr <- dplyr::select(dplyr::filter(acc_corr_data, same_or_diff == "same"), ll, vv, score)
diff_acc_corr <- dplyr::select(dplyr::filter(acc_corr_data, same_or_diff == "different"), lv, vl, score)

ll_acc_corr <- cor.test(same_acc_corr$ll, same_acc_corr$score, alternative = "greater")
ll_acc_corr
vv_acc_corr <- cor.test(same_acc_corr$vv, same_acc_corr$score, alternative = "greater")
vv_acc_corr

vl_acc_corr <- cor.test(diff_acc_corr$vl, diff_acc_corr$score, alternative = "greater")
vl_acc_corr
lv_acc_corr <- cor.test(diff_acc_corr$lv, diff_acc_corr$score, alternative = "greater")
lv_acc_corr

ifelse(os == "osx", item_accuracy_data <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/item_accuracies.csv"), item_accuracy_data <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/item_accuracies.csv"))

# Dummy code group so that "same" is the reference level
item_accuracy_data$group <- ifelse(item_accuracy_data$group == "same", 0, 1)
# Dummy code stimlus type so that "image" is the reference level
item_accuracy_data$stimulus_type <- ifelse(item_accuracy_data$stimulus_type == "image", 0, 1)


# Item-level glmer (both groups)

# Maximal model
item_acc_full.mod <- glmer (corr_resp ~ 1 + group * stimulus_type + (1 + stimulus_type | part_id) 
                            + (1 + group | trial), family = "binomial", data = item_accuracy_data) 

summary(item_acc_full.mod) # The full model converges, but gives a singular fit (no warning), which seems to come from the by-trial random effects, so try setting those correlations to 0

item_acc.mod <- glmer (corr_resp ~ 1 + group * stimulus_type + (1 + stimulus_type | part_id)
                       + (0 + group | trial) + (1|trial), family = "binomial", data = item_accuracy_data) # produces singular fit warning

summary(item_acc.mod) # It looks like the singular fit comes from the correlations for by-participant random effects, so set this to 0

item_acc.mod <- glmer (corr_resp ~ 1 + group * stimulus_type + (0 + stimulus_type | part_id) + (1 | part_id)
                       + (0 + group | trial) + (1|trial), family = "binomial", data = item_accuracy_data) # It seems like there's still a singular fit warning, so try other optimizers

all_fit(item_acc.mod) #nmkbw listed as "OK"

item_acc.mod <- glmer (corr_resp ~ 1 + group * stimulus_type + (0 + stimulus_type | part_id) + (1 | part_id)
                       + (0 + group | trial) + (1|trial), family = "binomial", data = item_accuracy_data,
                       control = glmerControl(optimizer = "nmkbw")) # This works!

summary(item_acc.mod) # no issues with singularity!
tab_model(item_acc.mod, show.se = TRUE)

# Item-level glmer ("same" group only)
# Maximal model
item_acc_same_full.mod <- glmer (corr_resp ~ 1 + stimulus_type + (1 + stimulus_type | part_id) + (1 | trial), 
                                 family = "binomial", data = filter(item_accuracy_data, group == 0)) # This converges
summary(item_acc_same_full.mod) # No singular fit
sjPlot::tab_model(item_acc_same_full.mod, show.se = TRUE)



# Item-level glmer ("different" group only)
# Maximal model
item_acc_diff_full.mod <- glmer (corr_resp ~ 1 + stimulus_type + (1 + stimulus_type | part_id) + (1 | trial), 
                                 family = "binomial", data = filter(item_accuracy_data, group == 1)) # This converges
summary(item_acc_diff_full.mod) # No singular fit
sjPlot::tab_model(item_acc_diff_full.mod, show.se = TRUE)


# *************************** INDIVIDUAL RT ANALYSES *******************

ifelse(os == "osx", indiv_rt_data <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/indiv_rts.csv"), indiv_rt_data <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/indiv_rts.csv"))

# Dummy code group so that "same" is the reference level
indiv_rt_data$same_or_diff <- ifelse(indiv_rt_data$same_or_diff == "same", 0, 1)
# Dummy code stimlus type so that "image" is the reference level
indiv_rt_data$domain <- ifelse(indiv_rt_data$domain == "non-linguistic", 0, 1)
# Dummy code block type so that "random" is the reference level
indiv_rt_data$type <- ifelse(indiv_rt_data$type == "random", 0, 1)

# Both groups
rt_both.mod_reduced <- lmer(rt ~ 1 + (domain * type * targ_index) + (domain * type * same_or_diff) + 
                               (domain * targ_index * same_or_diff) + (type * targ_index * same_or_diff) +
                       (1 + domain * type | part_id),
                    data = indiv_rt_data,
                    REML = FALSE)
print(rt_both.mod_reduced, correlation=TRUE)
tab_model(rt_both.mod_reduced, show.se = TRUE)

# Same group
rt_same.mod <- lmerTest::lmer(rt ~ 1 + domain * type * targ_index +
                                 (1 + domain * type | part_id),
                              data = filter(indiv_rt_data, same_or_diff == 0),
                              REML = FALSE)
summary(rt_same.mod)
tab_model(rt_same.mod, show.se = TRUE)

# post hoc
rt_same_image.mod <- lmerTest::lmer(rt ~ 1 + type * targ_index +
                                 (1 + type | part_id),
                              data = filter(indiv_rt_data, same_or_diff == 0, domain == 0),
                              REML = FALSE)
summary(rt_same_image.mod)
tab_model(rt_same_image.mod, show.se = TRUE, show.std = TRUE)

rt_same_letter.mod <- lmerTest::lmer(rt ~ 1 + type * targ_index +
                                       (1 + type | part_id),
                                    data = filter(indiv_rt_data, same_or_diff == 0, domain == 1),
                                    REML = FALSE)
summary(rt_same_letter.mod)
tab_model(rt_same_letter.mod, show.std = TRUE, show.se = TRUE)




rt_different.mod <- lmerTest::lmer(rt ~ 1 + domain * type * targ_index +
                                      (1 + domain * type | part_id),
                                   data = filter(indiv_rt_data, same_or_diff == 1),
                                   REML = FALSE)
summary(rt_different.mod)
tab_model(rt_different.mod, show.se = TRUE)




# *************************** RT SLOPE ANALYSES *******************

ifelse(os=="osx", indiv_rt_slope <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_indiv_rt_slope.csv"),
       indiv_rt_slope <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/sit_indiv_rt_slope.csv"))


# Model to test effects of type (random/ structured), test phase and group (same/ different) on RT Slope----------------------

slope_same_full.mod <- lm (rt_slope ~ domain * type, 
                           data = filter(indiv_rt_slope, same_or_diff == "same"))

summary(slope_same_full.mod)
sjPlot::tab_model(slope_same_full.mod, show.se = TRUE)


slope_different_full.mod <- lm (rt_slope ~ domain * type, 
                                data = filter(indiv_rt_slope, same_or_diff == "different"))

summary(slope_different_full.mod)
tab_model(slope_different_full.mod, show.se = TRUE)

# *************************** SUMMARIZE RT SLOPE *******************

# Find mean RT slope for each group and task ------------------------------------------------------------------------------------

mean(filter(indiv_rt_slope, same_or_diff=="different")$rt_slope)
mean(filter(indiv_rt_slope, same_or_diff=="same")$rt_slope)
mean(filter(indiv_rt_slope, type=="random")$rt_slope)
mean(filter(indiv_rt_slope, type=="structured")$rt_slope)


indiv_rt_slope %>%
   group_by(task, domain, type) %>%
   summarise(mean_rt_slope = mean(rt_slope), mean_rt_mean = paste0(round(mean(mean_rt), digits = 2)," (",round(sd(mean_rt), digits = 2),")"), n = n())

indiv_rt_slope %>%
   group_by(task, domain, type) %>%
   summarise(mean_rt_slope = mean(rt_slope), mean_rt_mean = paste0(round(mean(mean_rt), digits = 2)," (",round(sd(mean_rt), digits = 2),")"), n = n())




# #  ************* SEE WHETHER RT slope is below zero: T-TESTS *************

sll <- filter (indiv_rt_slope, type =="structured" &  task == "ll")
svv <- filter (indiv_rt_slope, type =="structured" &  task == "vv")
slv <- filter (indiv_rt_slope, type =="structured" &  task == "lv")
svl <- filter (indiv_rt_slope, type =="structured" &  task == "vl")

rll <- filter (indiv_rt_slope, type =="random" &  task == "ll")
rlv <- filter (indiv_rt_slope, type =="random" &  task == "lv")
rvl <- filter (indiv_rt_slope, type =="random" &  task == "vl")
rvv <- filter (indiv_rt_slope, type =="random" &  task == "vv")


# test whether in each condition rt slope was significantly less than zero.
t.test(sll$rt_slope, alternative="less", mu=0)

t.test(svv$rt_slope, alternative="less", mu=0) 

t.test(slv$rt_slope, alternative="less", mu=0)

t.test(svl$rt_slope, alternative="less", mu=0)



