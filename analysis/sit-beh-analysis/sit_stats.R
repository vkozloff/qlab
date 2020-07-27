#  SIT STATISTICAL ANALYSIS
#  Violet Kozloff
#  Created with support from Zhenghan Qi
#  Last modified July 24th, 2020
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

require("tidyverse")
require ("lme4")
require("lmerTest")
require("optimx")
require ("reshape2")
require("car")
require ("ez")
require("here")

rm(list=ls())


# ************************ SEE IF GROUPS ARE MATCHED FOR DEMOGRAPHICS WITH CHI-SQUARE TEST AND T-TESTS ************************

# For Mac
# chi_square_data <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_accuracy_vocab_wide.csv")
# picture_vocab <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/data/clean/vocab_clean/vocab_clean.csv")

# For PC
chi_square_data <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/sit_accuracy_vocab_wide.csv")
picture_vocab <- read.csv("Z:/projects/completed_projects/sit/analysis/data/clean/vocab_clean/vocab_clean.csv")

rt_data <- read.csv(here("../summaries/indiv_rts.csv"))


# Subset only relevant data about the population
matched_data <- chi_square_data %>% select (part_id, age, sex, score, same_or_diff)

# Chi-square test for gender
gender_table <- cast(matched_data,sex~same_or_diff,value = "score",length)
chisq.test(gender_table)

# T-test for vocab score by group
t.test(score~same_or_diff, data=matched_data) 

# Find means for each group
same_vocab<- mean((filter(chi_square_data, same_or_diff=="same"))$score, na.rm=TRUE)
diff_vocab<- mean((filter(chi_square_data, same_or_diff=="different"))$score, na.rm=TRUE)


# T-test for age by group 
t.test(age~same_or_diff, data=matched_data)



# *************************** ACCURACY ANALYSES *******************

# For Mac
# indiv_accuracies <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_accuracy_long.csv")
# For PC
indiv_accuracies <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/sit_accuracy_long.csv")


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


# Linear mixed effects model  -------------------------------------------------------------------------------------------------------------------------------------

# acc_lmer <- lmer(accuracy ~ same_or_diff + (1 | part_id), data = indiv_accuracies, REML = FALSE)


# Task-level accuracy for both groups
# task_acc_lmer <- lmer(accuracy ~ test_phase * same_or_diff + (1 | part_id), data = indiv_accuracies, REML = FALSE)
# summary(task_acc_lmer)


# Correlations for accuracy -------------------------------------------------------------------------------------------------------------------------------------

# Reformat and combine relevant data from indiv_accuracies and picture_vocab
acc_corr_data <- cast(indiv_accuracies, part_id ~ task, mean, value = 'accuracy')
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
vv_acc_corr <- cor.test(same_acc_corr$vv, same_acc_corr$score, alternative = "greater")
vl_acc_corr <- cor.test(diff_acc_corr$vl, diff_acc_corr$score, alternative = "greater")
lv_acc_corr <- cor.test(diff_acc_corr$lv, diff_acc_corr$score, alternative = "greater")



# *************************** RT SLOPE ANALYSES *******************

# For Mac
# indiv_rt_slope <- read.csv("/Volumes/data/projects/completed_projects/sit/analysis/summaries/sit_indiv_rt_slope.csv")
# For PC
indiv_rt_slope <- read.csv("Z:/projects/completed_projects/sit/analysis/summaries/sit_indiv_rt_slope.csv")


# Model to test effects of type (random/ structured), test phase and group (same/ different) on RT Slope----------------------

# For both groups

# Full model
# This model gives an error message that there fewer observations than random effects
rt_slope_both.full <- lmer(rt_slope ~ 1 + same_or_diff * domain * type + (1 + domain * type | part_id), data = indiv_rt_slope,  REML = FALSE)
summary(rt_slope_same.full)

# I simplified the random effects structure
rt_slope_both.mod1 <- lmer(rt_slope ~ 1 + same_or_diff * domain * type + (1 + domain + type | part_id), data = indiv_rt_slope,  REML = FALSE)
summary(rt_slope_both.mod1)

# This model failed to converge, so here it is with increased number of iterations
rt_slope_both.mod2 <- lmer(rt_slope ~ 1 + same_or_diff * domain * type + (1 + domain + type | part_id), data = indiv_rt_slope,  REML = FALSE, control = lmerControl (optimizer = "bobyqa", optCtrl = list(maxfun=1e9)))
summary(rt_slope_both.mod2)

# This model had a perfect correlation between random intercept of participant ID  and random slopes of domain and type
# Here is the model with the correlation between participant intercept and domain slope removed
rt_slope_both.mod3 <- lmer(rt_slope ~ 1 + same_or_diff * domain * type + (1 + type | part_id) + (0 + domain + type | part_id), data = indiv_rt_slope,  REML = FALSE, control = lmerControl (optimizer = "bobyqa", optCtrl = list(maxfun=1e9)))
summary(rt_slope_both.mod3)

rt_slope_both.mod4 <- lmer(rt_slope ~ 1 + same_or_diff * domain * type + (1 | part_id), data = indiv_rt_slope,  REML = FALSE, control = lmerControl (optimizer = "bobyqa", optCtrl = list(maxfun=1e9)))


# test for an effect of domain via a likelihood ratio test
anova(rt_slope_both.mod2, rt_slope_both.mod3)

# Since model 3 still had a perfect correlation between random intercept of participant and random slope of type, I forced that to 0 as well
rt_slope_both.mod4 <- lmer(rt_slope ~ 1 + same_or_diff * domain * type + (1 | part_id) + (0 + type + domain | part_id), data = indiv_rt_slope,  REML = FALSE, control = lmerControl (optimizer = "bobyqa", optCtrl = list(maxfun=1e9)))
summary(rt_slope_both.mod4)







# For "different" group
rt_slope_diff.full <- lmer(rt_slope ~ 1 + domain * type + (1 + domain + type | part_id), data = filter (indiv_rt_slope, same_or_diff == "different"),  REML = FALSE)
summary(rt_slope_diff.full)


# Effects by group
same_slope_data <- filter (indiv_rt_slope, same_or_diff == "same")
diff_slope_data <- filter (indiv_rt_slope, same_or_diff == "different")

same_slope_lmer <- lmer(rt_slope ~ domain * type + (1|part_id) + (1+(domain+type)|part_id), data = same_slope_data, 
                        control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE), check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
summary(same_slope_lmer)
diff_slope_lmer <- lmer(rt_slope ~ domain * type + (1|part_id) + (1+(domain+type)|part_id), data = diff_slope_data, 
                        control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE), check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
summary(diff_slope_lmer)

#same_mean_lmer <- lmer(mean_rt ~ domain * type + (1|part_id) + (1+(domain+type)|part_id), data = same_slope_data, 
                        control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE), check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))


#diff_mean_lmer <- lmer(mean_rt ~ domain * type + (1|part_id) + (0+(domain+type)|part_id), data = diff_slope_data, 
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE), check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
summary(diff_mean_lmer)








accuracy_item_lmer <- glmer(corr_resp ~ 1 + stimulus_type + (1|part_id) + (0 + domain*stimulus_type|part_id) + (1| trial), family = binomial, data = acc_lmer_data,control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE),  check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))



# mean_rt_lmer <- glmer(corr_resp ~ 1 + stimulus_type + (0 + domain*stimulus_type|part_id) + (1| trial), family = binomial, data = diff_lmer_data)

 # Find mean RT slope for each group and task ------------------------------------------------------------------------------------

mean(filter(indiv_rt_slope, same_or_diff=="different")$rt_slope)
mean(filter(indiv_rt_slope, same_or_diff=="same")$rt_slope)
mean(filter(indiv_rt_slope, type=="random")$rt_slope)
mean(filter(indiv_rt_slope, type=="structured")$rt_slope)


indiv_rt_slope %>%
  group_by(task, domain, type) %>%
  summarise(mean_rt_slope = mean(rt_slope), mean_rt_mean = paste0(round(mean(mean_rt), digits = 2)," (",round(sd(mean_rt), digits = 2),")"), n = n())

# Model to test effects of type (random/ structured), test phase and group (same/ different) on reaction time ----------------------
# indiv_rts <- dplyr::select(read.csv("Y:/projects/completed_projects/sit/analysis/summaries/indiv_rts.csv"), -X)
# indiv_rt_lmer <- lmer(rt ~ domain * type * same_or_diff + (1 | part_id) + (1|target_item), data = indiv_rts, REML = FALSE)
# summary(indiv_rt_lmer)


# *************************** CORRELATIONS FOR RT SLOPE/ MEAN RT **********************************
 

# RT Slope Correlation matrices-------------------------------------------------------------------------------------------------------------------------------------

# Extract relevant data from indiv_rt_slope and picture_vocab
slope_corr_data <- cast(indiv_rt_slope, part_id ~ task*type, mean, value = 'rt_slope')
slope_corr_data <- merge(slope_corr_data, picture_vocab, by = "part_id", all=TRUE)

slope_corr_data$rand_lsl <- ifelse(!is.na(slope_corr_data$ll_random), slope_corr_data$ll_random, slope_corr_data$lv_random)
slope_corr_data$rand_vsl <- ifelse(!is.na(slope_corr_data$vv_random), slope_corr_data$vv_random, slope_corr_data$vl_random)
slope_corr_data$struct_lsl <- ifelse(!is.na(slope_corr_data$ll_structured), slope_corr_data$ll_structured, slope_corr_data$lv_structured)
slope_corr_data$struct_vsl <- ifelse(!is.na(slope_corr_data$vv_structured), slope_corr_data$vv_structured, slope_corr_data$vl_structured)

slope_corr_data <- mutate(slope_corr_data, vsl_diffscore = ifelse(group == "different", vl_structured - lv_random, vv_structured-vv_random))
slope_corr_data <- mutate(slope_corr_data, lsl_diffscore = ifelse(group == "different", lv_structured - vl_random, ll_structured-ll_random))

# Correlations collapsed across groups
struct_vsl_slope_corr <- cor.test(slope_corr_data$struct_vsl, slope_corr_data$score, alternative = "less")
struct_lsl_slope_corr <- cor.test(slope_corr_data$struct_lsl, slope_corr_data$score, alternative = "less")
vsl_diffscore_slope_corr <- cor.test(slope_corr_data$vsl_diffscore, slope_corr_data$score)
lsl_diffscore_slope_corr <- cor.test(slope_corr_data$lsl_diffscore, slope_corr_data$score)

# Correlations by group
same_slope_corr <- dplyr::select(dplyr::filter(slope_corr_data, group == "same"), part_id, ll_random, ll_structured, vv_random, vv_structured, vsl_diffscore, lsl_diffscore, score)
diff_slope_corr <- dplyr::select(dplyr::filter(slope_corr_data, group == "different"), part_id, lv_random, lv_structured, vl_random, vl_structured, vsl_diffscore, lsl_diffscore, score)

ll_struct_slope_corr <- cor.test(same_slope_corr$ll_structured, same_slope_corr$score, alternative = "less")
vv_struct_slope_corr <- cor.test(same_slope_corr$vv_structured, same_slope_corr$score, alternative = "less")
vl_struct_slope_corr <- cor.test(diff_slope_corr$vl_structured, diff_slope_corr$score, alternative = "less")
lv_struct_slope_corr <- cor.test(diff_slope_corr$lv_structured, diff_slope_corr$score, alternative = "less")

diff_vsl_diffscore_corr <- cor.test(diff_slope_corr$vsl_diffscore, diff_slope_corr$score, alternative = "less")
diff_lsl_diffscore_corr <- cor.test(diff_slope_corr$lsl_diffscore, diff_slope_corr$score, alternative = "less")

same_vsl_diffscore_corr <- cor.test(same_slope_corr$vsl_diffscore, same_slope_corr$score, alternative = "less")
same_lsl_diffscore_corr <- cor.test(same_slope_corr$lsl_diffscore, same_slope_corr$score, alternative = "less")


# Mean RT Correlation matrices-------------------------------------------------------------------------------------------------------------------------------------

# Extract relevant data from indiv_rt_slope and picture_vocab
rtm_corr_data <- cast(indiv_rt_slope, part_id ~ task*type, mean, value = 'mean_rt')
rtm_corr_data <- merge(rtm_corr_data, picture_vocab, by = "part_id", all=TRUE)

rtm_corr_data$rand_lsl <- ifelse(!is.na(rtm_corr_data$ll_random), rtm_corr_data$ll_random, rtm_corr_data$lv_random)
rtm_corr_data$rand_vsl <- ifelse(!is.na(rtm_corr_data$vv_random), rtm_corr_data$vv_random, rtm_corr_data$vl_random)
rtm_corr_data$struct_lsl <- ifelse(!is.na(rtm_corr_data$ll_structured), rtm_corr_data$ll_structured, rtm_corr_data$lv_structured)
rtm_corr_data$struct_vsl <- ifelse(!is.na(rtm_corr_data$vv_structured), rtm_corr_data$vv_structured, rtm_corr_data$vl_structured)

rtm_corr_data <- mutate(rtm_corr_data, vsl_diffscore = ifelse(group == "different", vl_structured - lv_random, vv_structured-vv_random))
rtm_corr_data <- mutate(rtm_corr_data, lsl_diffscore = ifelse(group == "different", lv_structured - vl_random, ll_structured-ll_random))

# Correlations collapsed across groups
struct_vsl_rtm_corr <- cor.test(rtm_corr_data$struct_vsl, rtm_corr_data$score, alternative = "less")
struct_lsl_rtm_corr <- cor.test(rtm_corr_data$struct_lsl, rtm_corr_data$score, alternative = "less")
vsl_diffscore_rtm_corr <- cor.test(rtm_corr_data$vsl_diffscore, rtm_corr_data$score)
lsl_diffscore_rtm_corr <- cor.test(rtm_corr_data$lsl_diffscore, rtm_corr_data$score)


# Correlations by group
same_rtm_corr <- dplyr::select(dplyr::filter(rtm_corr_data, group == "same"), part_id, ll_random, ll_structured, vv_random, vv_structured, vsl_diffscore, lsl_diffscore, score)
diff_rtm_corr <- dplyr::select(dplyr::filter(rtm_corr_data, group == "different"), part_id, lv_random, lv_structured, vl_random, vl_structured, vsl_diffscore, lsl_diffscore, score)

ll_struct_rtm_corr <- cor.test(same_rtm_corr$ll_structured, same_rtm_corr$score, alternative = "less")
vv_struct_rtm_corr <- cor.test(same_rtm_corr$vv_structured, same_rtm_corr$score, alternative = "less")
vl_struct_rtm_corr <- cor.test(diff_rtm_corr$vl_structured, diff_rtm_corr$score, alternative = "less")
lv_struct_rtm_corr <- cor.test(diff_rtm_corr$lv_structured, diff_rtm_corr$score, alternative = "less")

diff_vsl_diffscore_corr <- cor.test(diff_rtm_corr$vsl_diffscore, diff_rtm_corr$score, alternative = "less")
diff_lsl_diffscore_corr <- cor.test(diff_rtm_corr$lsl_diffscore, diff_rtm_corr$score, alternative = "less")

same_vsl_diffscore_corr <- cor.test(same_rtm_corr$vsl_diffscore, same_rtm_corr$score, alternative = "less")
same_lsl_diffscore_corr <- cor.test(same_rtm_corr$lsl_diffscore, same_rtm_corr$score, alternative = "less")




# Correlations collapsed across groups
# rand_vsl_rtm_corr <- cor.test(rtm_corr_data$rand_vsl, rtm_corr_data$score)
# rand_lsl_rtm_corr <- cor.test(rtm_corr_data$rand_lsl, rtm_corr_data$score)
# struct_vsl_rtm_corr <- cor.test(rtm_corr_data$struct_vsl, rtm_corr_data$score)
# struct_lsl_rtm_corr <- cor.test(rtm_corr_data$struct_lsl, rtm_corr_data$score)
# 
# 
# # Correlations by group
# same_rtm_corr <- dplyr::select(dplyr::filter(rtm_corr_data, group == "same"), ll_random, ll_structured, vv_random, vv_structured, lsl_avg, vsl_avg, struct_avg, rand_avg, score)
# diff_rtm_corr <- dplyr::select(dplyr::filter(rtm_corr_data, group == "different"), lv_random, lv_structured, vl_random, vl_structured, lsl_avg, vsl_avg, struct_avg, rand_avg, score)
# 
# ll_rand_rtm_corr <- cor.test(same_rtm_corr$ll_random, same_rtm_corr$score)
# vv_rand_rtm_corr <- cor.test(same_rtm_corr$vv_random, same_rtm_corr$score)
# vl_rand_rtm_corr <- cor.test(diff_rtm_corr$vl_random, diff_rtm_corr$score)
# lv_rand_rtm_corr <- cor.test(diff_rtm_corr$lv_random, diff_rtm_corr$score)
# 
# ll_struct_rtm_corr <- cor.test(same_rtm_corr$ll_structured, same_rtm_corr$score)
# vv_struct_rtm_corr <- cor.test(same_rtm_corr$vv_structured, same_rtm_corr$score)
# vl_struct_rtm_corr <- cor.test(diff_rtm_corr$vl_structured, diff_rtm_corr$score)
# lv_struct_rtm_corr <- cor.test(diff_rtm_corr$lv_structured, diff_rtm_corr$score)
# 
# lsl_avg_rtm_corr <- cor.test(rtm_corr_data$lsl_avg, rtm_corr_data$score)
# vsl_avg_rtm_corr <- cor.test(rtm_corr_data$vsl_avg, rtm_corr_data$score)
# same_lsl_avg_rtm_corr <- cor.test(same_rtm_corr$lsl_avg, same_rtm_corr$score)
# diff_vsl_avg_rtm_corr <- cor.test(diff_rtm_corr$vsl_avg, diff_rtm_corr$score)
# diff_lsl_avg_rtm_corr <- cor.test(diff_rtm_corr$lsl_avg, diff_rtm_corr$score)
# same_vsl_avg_rtm_corr <- cor.test(same_rtm_corr$vsl_avg, same_rtm_corr$score)
# 
# struct_avg_rtm_corr <- cor.test(rtm_corr_data$struct_avg, rtm_corr_data$score)
# rand_avg_rtm_corr <- cor.test(rtm_corr_data$rand_avg, rtm_corr_data$score)
# same_struct_avg_rtm_corr <- cor.test(same_rtm_corr$struct_avg, same_rtm_corr$score)
# diff_rand_avg_rtm_corr <- cor.test(diff_rtm_corr$rand_avg, diff_rtm_corr$score)
# diff_struct_avg_rtm_corr <- cor.test(diff_rtm_corr$struct_avg, diff_rtm_corr$score)
# same_rand_avg_rtm_corr <- cor.test(same_rtm_corr$rand_avg, same_rtm_corr$score)














# RT Slope Correlation matrices-------------------------------------------------------------------------------------------------------------------------------------
 
# Extract relevant data from indiv_rt_slope and picture_vocab
slope_corr_data <- cast(indiv_rt_slope, part_id ~ task*type, mean, value = 'rt_slope')
slope_corr_data <- merge(slope_corr_data, picture_vocab, by = "part_id", all=TRUE)

slope_corr_data$lsl_rand <- ifelse(!is.na(slope_corr_data$ll_random), slope_corr_data$ll_random, slope_corr_data$lv_random)
slope_corr_data$lsl_struct <- ifelse(!is.na(slope_corr_data$ll_structured), slope_corr_data$ll_structured, slope_corr_data$lv_structured)
slope_corr_data$vsl_rand <- ifelse(!is.na(slope_corr_data$vv_random), slope_corr_data$vv_random, slope_corr_data$vl_random)
slope_corr_data$vsl_struct <- ifelse(!is.na(slope_corr_data$vv_structured), slope_corr_data$vv_structured, slope_corr_data$vl_structured)

# Correlations for RT slope by stimulus type and block type, collapsed across groups
lsl_rand_slope_corr <- cor.test(slope_corr_data$lsl_rand, slope_corr_data$score)
lsl_struct_slope_corr <- cor.test(slope_corr_data$lsl_struct, slope_corr_data$score)
vsl_rand_slope_corr <- cor.test(slope_corr_data$vsl_rand, slope_corr_data$score)
vsl_struct_slope_corr <- cor.test(slope_corr_data$vsl_struct, slope_corr_data$score)

# Correlations for RT slope collapsed across groups
slope_corr_data <- mutate(slope_corr_data, lsl_avg = rowMeans(select(slope_corr_data, ends_with("lsl")), na.rm = TRUE))
slope_corr_data <- mutate(slope_corr_data, vsl_avg = rowMeans(select(slope_corr_data, ends_with("vsl")), na.rm = TRUE))
slope_corr_data <- mutate(slope_corr_data, rand_avg = rowMeans(select(slope_corr_data, ends_with("random")), na.rm = TRUE))
slope_corr_data <- mutate(slope_corr_data, struct_avg = rowMeans(select(slope_corr_data, ends_with("structured")), na.rm = TRUE))


# Add corr_data's groups of same/ different
all_same <- filter(slope_corr_data, group=="same")
all_diff <- filter(slope_corr_data, group=="different")

# Test correlations for different condition: these are the individual rt slopes against vocab
#lv_corr<-cor.test(slope_corr_data$lv,slope_corr_data$score)
#lv_corr

#vl_corr<-cor.test(slope_corr_data$vl,slope_corr_data$score)
#vl_corr

# Test correlations for same condition
#ll_corr<-cor.test(slope_corr_data$ll,slope_corr_data$score)
#ll_corr

#vv_corr<-cor.test(slope_corr_data$vv,slope_corr_data$score)
#vv_corr

# calculate the difference scores between structured condition and random condition within linguistic and non-linguistic domains.
rt_slope_diff = cast(indiv_rt_slope,part_id+same_or_diff+domain~type,value = "rt_slope")
rt_slope_diff$slope_diff = rt_slope_diff$structured-rt_slope_diff$random
rt_slope_diff = merge(rt_slope_diff,picture_vocab,id=1)
rt_slope_diff = cast(rt_slope_diff,part_id+score+same_or_diff~domain,value="slope_diff")

#Test for correlation between RT slope and vocab
cor.test(rt_slope_diff$linguistic,rt_slope_diff$score)
cor.test(rt_slope_diff$`non-linguistic`,rt_slope_diff$score)


# RT slope difference
rt_slope_diff_diff = subset(rt_slope_diff,same_or_diff=="diff")
rt_slope_diff_diff_complete = rt_slope_diff_diff[complete.cases(rt_slope_diff_diff),]
 
# Test the correlation between the same condition's difference scores and vocabulary 
rt_slope_diff_same = subset(rt_slope_diff,same_or_diff=="same")
rt_slope_diff_same_complete = rt_slope_diff_same[complete.cases(rt_slope_diff_same),]
cor.test(rt_slope_diff_same_complete$linguistic,rt_slope_diff_same_complete$score,alternative ="less",method="pearson")

colnames(rt_slope_diff_same_complete)[5] <- "non_linguistic"
cor.test(rt_slope_diff_same_complete$non_linguistic,rt_slope_diff_same_complete$score,alternative ="less",method="pearson") 

# Test the correlation between the different condition's difference scores and vocabulary 
rt_slope_diff_diff = subset(rt_slope_diff,same_or_diff=="different")
rt_slope_diff_diff_complete = rt_slope_diff_diff[complete.cases(rt_slope_diff_diff),]
cor.test(rt_slope_diff_diff_complete$linguistic,rt_slope_diff_diff_complete$score,alternative = "less", method="pearson")


vocab_corr<- rt_slope_diff_diff_complete
vocab_corr <- dplyr::rename(vocab_corr, image = `non-linguistic`)
vocab_corr <- dplyr::rename(vocab_corr, letter = linguistic)
vocab_corr <- dplyr::rename(vocab_corr, group = same_or_diff)
vocab_corr <- melt(data = data.frame(vocab_corr), id.vars = c("part_id", "group", "score"), measure.vars = c("image", "letter"))
vocab_corr <- dplyr::rename(vocab_corr, rt_slope_diff = value)
vocab_corr <- dplyr::rename(vocab_corr, Stimulus = variable)

# Plot vocabulary correlation
ggplot(data=vocab_corr, aes(x=rt_slope_diff,y=score,color=Stimulus, shape=Stimulus)) + 
   geom_point(size=10) + 
   geom_smooth(method=lm, se=FALSE, aes(linetype = Stimulus)) + 
   ylab(label="Vocabulary Score") +
   theme(panel.border = element_rect(colour='black', fill=NA),  panel.background = element_blank()) + 
   xlab(label="Difference in RT Slope Between \nStructured and Random Condition") +
   scale_colour_hue(l = 50) +
   theme_classic() +
   theme(plot.title = element_text(size = 30),
         axis.text.x = element_text(size = 30),
         axis.text.y = element_text(size = 30),
         axis.title.x = element_text(size = 30),
         axis.title.y = element_text(size = 30),
         legend.title = element_text(size = 30),
         legend.text = element_text(size = 30))


colnames(rt_slope_diff_diff_complete)[5]<- "non_linguistic"
cor.test(rt_slope_diff_diff_complete$non_linguistic,rt_slope_diff_diff_complete$score,alternative = "less", method="pearson")

heading_names <- c("score", "linguistic", "non_linguistic")
test_same <- data.frame(rt_slope_diff_same_complete[heading_names])
test_diff <- data.frame(rt_slope_diff_diff_complete[heading_names])
names(test_same) <- gsub ("linguistic", "linguistic_same", names(test_same))
names(test_diff) <- gsub ("linguistic", "linguistic_diff", names(test_diff))
test_same[,"linguistic_diff"] <- NA
test_same[,"non_linguistic_diff"] <- NA
test_diff[,"linguistic_same"] <- NA
test_diff[,"non_linguistic_same"] <- NA
test<-(rbind(test_same,test_diff))
test[is.na(test)] <- 0
names(test)<-gsub("_", " ", names(test))
 
test$part_id <- NULL
rt_slope_correlations <- cor(test, use="pairwise.complete.obs")

# Mean RT Correlations-------------------------------------------------------------------------------------------------------------------------------------
# 
# Extract relevant data from indiv_rt_slope and picture_vocab
mean_rt_data <- cast(indiv_rt_slope, part_id ~ task, mean, value = 'mean_rt')
mean_rt_data <- merge(mean_rt_data, picture_vocab, by = "part_id", all=TRUE)

# Correlations for accuracy collapsed across groups
mean_rt_data$lsl <- ifelse(!is.na(mean_rt_data$ll), mean_rt_data$ll, mean_rt_data$lv)
mean_rt_data$vsl <- ifelse(!is.na(mean_rt_data$vv), mean_rt_data$vv, mean_rt_data$vl)
vsl_mean_rt_corr <- cor.test(mean_rt_data$vsl, mean_rt_data$score) 
lsl_mean_rt_corr <- cor.test(mean_rt_data$lsl, mean_rt_data$score) 

# Add mean_rt_data's groups of same/ different
mean_rt_data <- cbind(mean_rt_data, "same_or_diff")
colnames(mean_rt_data)[9] <- "same_or_diff"
all_same <- mean_rt_data[ which(!is.na(mean_rt_data$ll)), ]
all_same$same_or_diff <- ("same")
all_diff <- mean_rt_data[ which(!is.na(mean_rt_data$lv>0)), ]
all_diff$same_or_diff <- ("different")
mean_rt_data <- rbind(all_same, all_diff)
mean_rt_data <- mean_rt_data[ which(mean_rt_data$score>0), ]
 
# Separate mean_rt_data into groups by same/ different
same_corr <- mean_rt_data[ which(!is.na(mean_rt_data$ll)), ]
same_corr <- same_corr[, c(2, 5, 6)]
diff_corr <- mean_rt_data[ which(!is.na(mean_rt_data$lv)), ]
diff_corr <- diff_corr[, c(3, 4, 6)]
 
# Create correlation matrices for different condition
diff <- cor(diff_corr, method = c("pearson"),use="pairwise.complete.obs")
 
# Test p-values of correlation matrices for different condition
lv_corr<-cor.test(diff_corr$lv,diff_corr$score)
lv_corr

vl_corr<-cor.test(diff_corr$vl,diff_corr$score)
vl_corr

# Create correlation matrices for same condition
same <- cor(same_corr, method = c("pearson"),use="pairwise.complete.obs")
 
# Test p-values of correlation matrices for different condition
ll_corr<-cor.test(same_corr$ll, same_corr$score)
ll_corr
 
vv_corr<-cor.test(same_corr$vv, same_corr$score)
vv_corr

# calculate the mean rt difference scores between structured condition and random condition 
# within linguistic and non-linguistic domainsfor individual difference analyses
rt_diff = cast(indiv_rt_slope,part_id+same_or_diff+domain~type,value = "mean_rt")
rt_diff$meanrt_diff = rt_diff$structured-rt_diff$random
rt_diff = merge(rt_diff,picture_vocab,id=1)
rt_diff = cast(rt_diff,part_id+score+same_or_diff~domain,value="meanrt_diff")
colnames(rt_diff)[5]="non_linguistic"
rt_diff_same = subset(rt_diff,same_or_diff=="same")
rt_diff_same_complete = rt_diff_same[complete.cases(rt_diff_same),]
cor.test(rt_diff_same_complete$linguistic,rt_diff_same_complete$score,alternative = "greater", method="pearson")
cor.test(rt_diff_same_complete$non_linguistic,rt_diff_same_complete$score,alternative = "greater", method="pearson")
rt_diff_diff = subset(rt_diff,same_or_diff=="different")
cor.test(rt_diff_diff$linguistic,rt_diff_diff$score, alternative = "greater", method="pearson")
cor.test(rt_diff_diff$non_linguistic,rt_diff_diff$score, alternative = "less", method="pearson")

 
# #  ************* SEE WHETHER RT slope is below zero: T-TESTS *************

sll <- filter (indiv_rt_slope, type =="structured" &  task == "ll")
slv <- filter (indiv_rt_slope, type =="structured" &  task == "lv")
svl <- filter (indiv_rt_slope, type =="structured" &  task == "vl")
svv <- filter (indiv_rt_slope, type =="structured" &  task == "vv")
rll <- filter (indiv_rt_slope, type =="random" &  task == "ll")
rlv <- filter (indiv_rt_slope, type =="random" &  task == "lv")
rvl <- filter (indiv_rt_slope, type =="random" &  task == "vl")
rvv <- filter (indiv_rt_slope, type =="random" &  task == "vv")


# test whether in each condition rt slope was significantly less than zero.
t.test(sll$rt_slope, alternative="less", mu=0)

t.test(svv$rt_slope, alternative="less", mu=0) 

t.test(slv$rt_slope, alternative="less", mu=0)

t.test(svl$rt_slope, alternative="less", mu=0)






# Unaggregated RT mixed effects models

# For both groups

# Full model
# This model fails to converge
rt_both.full <- lmer(rt ~ 1 + same_or_diff * domain * type + (1 + domain * type | part_id), data = rt_data,  REML = FALSE)

# Increase the number of iterations
rt_both.mod <- lmer(rt ~ 1 + same_or_diff * domain * type + (1 + domain * type | part_id), data = rt_data,  REML = FALSE, control = lmerControl (optimizer = "bobyqa", optCtrl = list(maxfun=1e9)))
summary(rt_both.mod)

# For same group only

# Full model
rt_same.full <- lmer(rt ~ 1 + domain * type + (1 + domain * type | part_id), data = filter(rt_data, same_or_diff == "same"),  REML = FALSE)
summary(rt_same.full)

# For different group only

# Maximal model:
# This model fails to converge
rt_diff_full <- lmer(rt ~ 1 + domain * type + (1 + domain * type | part_id), data = filter(rt_data, same_or_diff == "different"),  REML = FALSE)

rt_diff_mod <- lmer(rt ~ 1 + domain * type + (1 + domain * type | part_id), data = filter(rt_data, same_or_diff == "different"),  REML = FALSE, control = lmerControl (optimizer = "bobyqa", optCtrl = list(maxfun=1e9)))
summary(rt_diff_mod)


