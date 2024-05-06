#Children Compositionality

#load packages and data
{
  if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
  if (!require(lmerTest)) install.packages("lmerTest"); library(lmerTest)
  if (!require(car)) install.packages("car"); library(car)
  if (!require(psych)) install.packages("psych"); library(psych)
  if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
  if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
  if (!require(rstatix)) install.packages("rstatix"); library(rstatix)
  if (!require(lme4)) install.packages("lme4"); library(lme4)
  if (!require(coin)) install.packages("coin"); library(coin)
  if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
  if (!require(DescTools)) install.packages("DescTools") ; library(DescTools)
  if (!require(binom)) install.packages("binom"); library(binom)
  data <- read.csv("Children.Compositionality.Experiment.csv")
  
}
  #library(lm.beta)
  #library(broom.mixed)
  #library(metafor)


#create variables age.in.years (with decimal) and age.year (arbitrary age group); 
#create variable accu.training measuring proportion trials participants got correct in training (# correct training trials / # of all training trials);
#create new age category(age<7.5:"0", age>7.5:"1") 
#create new data frame "data_pass" with only participants who passed training
#create new data frames "data_younger" and "data_older" that split up participants into <7.5 age group and >7.5 age group
{
  data <- data %>% mutate(age.in.years = age.in.days/365.25, age.year = factor(floor(age.in.years))) %>% 
    mutate(accu.testing = total.correct/6) %>% 
    mutate(accu.training = (p1.earl.correct+p2.earl.correct+p1.wally.correct+p2.wally.correct+p1.earl.repeat.correct+p2.earl.repeat.correct+p1.wally.repeat.correct+p2.wally.repeat.correct)/(4+p1.earl.repeat+p2.earl.repeat+p1.wally.repeat+p2.wally.repeat)) %>%
    mutate(repeat.trial.count = p1.earl.repeat + p2.earl.repeat + p1.wally.repeat + p2.wally.repeat) %>%
    mutate (age_category = ifelse(age.in.years > 7.5, "1", "0"))
  data_pass <- data %>% filter(data$excluded == "No") #20 in each age group, median = 7.508
  data_younger <- data_pass %>% filter(age_category=="0")
  data_older <- data_pass %>% filter(age_category=="1")
}

# sex distribution
{
  data_pass %>% group_by(sex) %>% summarise(row_count = n())
}

# age distribution 
{
  describe(data_pass$age.in.years, trim = 0, na.rm = TRUE)
}

# if participants passed training trials 
{
  (pass_training_count <- length(data$pass.training[data$pass.training == "Yes"]))
  (pass_training_rate <- length(data$pass.training[data$pass.training == "Yes"])/length(data$pass.training))
  (pass_earl_training <- data %>% filter(p1.earl.pass == 1, p2.earl.pass ==1) %>%
    summarise(row_count = n()))
  (pass_wally_training <- data %>% filter(p1.wally.pass == 1, p2.wally.pass ==1) %>%
    summarise(row_count = n()))
}

# if participants required repeat trials
{
  (repeat_earl1 <- data_pass %>% filter(p1.earl.repeat == 1 | p2.earl.repeat == 1) %>% summarise(row_count = n()))
  (repeat_earl2 <- data_pass %>% filter(p1.earl.repeat == 1, p2.earl.repeat == 1) %>% summarise(row_count = n()))
  (repeat_wally1 <- data_pass %>% filter(p1.wally.repeat == 1 | p2.wally.repeat == 1) %>% summarise(row_count = n()))
  (repeat_wally2 <- data_pass %>% filter(p1.wally.repeat == 1, p2.wally.repeat == 1) %>% summarise(row_count = n()))
}

# summarize test trial accuracy
{
  (describe(data_pass$total.correct, trim = 0, na.rm = TRUE))
  (describe(data_younger$total.correct, trim = 0, na.rm = TRUE))
  (describe(data_older$total.correct, , trim = 0, na.rm = TRUE))
}

# predicting testing trial accuracy
# predictors include age category (<7.5, >7.5), sex, test trial order, trial group (1-3, 4-6)
# create new trial_group category(Trials 1-3, Trials 4-6) to test learning effect
# table of estimates with 95% CI
{ 
  set.seed(42)
  data_glmer <- data_pass %>% 
    mutate(ID = 1:40) %>%
    pivot_longer(cols = c("t1.correct", "t2.correct", "t3.correct", "t4.correct", "t5.correct", "t6.correct"), names_to = "trial", values_to = "trial_correct")
  data_glmer$trial_number <- as.numeric(gsub("t(\\d+)\\.correct", "\\1", data_glmer$trial))
  data_glmer$trial_group <- ifelse(data_glmer$trial_number <= 3, "First3", "Last3")
  mdl_glmer <- glmer(trial_correct ~ sex + test.order + trial_group + age_category + (1|ID), data = data_glmer, family = binomial)
  summary(mdl_glmer)
  se <- sqrt(diag(vcov(mdl_glmer)))
  # table of estimates with 95% CI
  (tab <- cbind(Est = fixef(mdl_glmer), LL = fixef(mdl_glmer) - 1.96 * se, UL = fixef(mdl_glmer) + 1.96 *se))
  exp(tab)
}

# plot glmer result 
{
  data_glmerplot <- data.frame(
    Variable = c("Intercept", "Sex (Male)", "Test order (Order B)", 
                 "First/Last 3 trials (Last 3)", "Age category (>7.5 years old)"),
    OR = c(0.37, 3.86, 1.69, 1.07, 11.39), # Odds Ratio
    LowerCI = c(0.07, 0.62, 0.29, 0.52, 1.60), # Lower bound of Confidence Interval
    UpperCI = c(2.07, 24.03, 10.05, 2.23, 81.08) # Upper bound of Confidence Interval
  )
  # Adding a column for the log of the odds ratio (for plotting)
  data_glmerplot <- data_glmerplot %>%
    mutate(logOR = log(OR),
           logLowerCI = log(LowerCI),
           logUpperCI = log(UpperCI))
  # Creating the forest plot
  ggplot(data_glmerplot, aes(y = Variable, x = logOR)) +
    geom_point() +
    geom_errorbarh(aes(xmin = logLowerCI, xmax = logUpperCI), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    xlab("Log Odds Ratio") + ylab("") +
    ggtitle("Forest Plot of Odds Ratios with Confidence Intervals") +
    theme_minimal()
}

# compare mean age in each age category (<7.5, >7.5)
{
  (describe(data_younger$age.in.year, trim = 0, na.rm = TRUE))
  (describe(data_older$age.in.year, trim = 0, na.rm = TRUE))
}

# compare mean accuracy in each age category (<7.5, >7.5)
{
  (describe(data_younger$accu.testing, trim = 0, na.rm = TRUE))
  (describe(data_older$accu.testing, trim = 0, na.rm = TRUE))
}

# comparing success rate in test trials to chance in each age category (<7.5, >7.5)
#Wilcoxon signed-rank test
{
  (chance.test1 <- rstatix ::  wilcox_test(accu.testing ~ 1, mu = 1/2, data = data_younger))
  (chance.test2 <- rstatix ::  wilcox_test(accu.testing ~ 1, mu = 1/2, data = data_older))
  #calculate effect size 
  (chance.effect1 <- wilcox_effsize(accu.testing ~ 1, mu = 1/2, data = data_younger))
  (chance.effect2 <- wilcox_effsize(accu.testing ~ 1, mu = 1/2, data = data_older))
}

# plot test trial performance 
{
  data_plot <- data_pass
  data_plot$age_category <- factor(data_pass$age_category, levels = c('0', '1'),
                          labels = c('Younger than 7.5', 'Older than 7.5'))
  data_glmer$age_category <- factor(data_glmer$age_category, levels = c('0', '1'),
                                   labels = c('Younger than 7.5', 'Older than 7.5'))
  
  (mean_ci <- data_glmer %>%
    group_by(age_category) %>%
    summarise(
      mean = mean(trial_correct),
      lower_ci = binom.confint(sum(trial_correct), n(), methods = "wilson")$lower,
      upper_ci = binom.confint(sum(trial_correct), n(), methods = "wilson")$upper))
  
      
  (compositionality.plot <- ggplot() +
    geom_dotplot(data = data_plot, aes(x = age_category, y = accu.testing), binaxis = "y", stackdir = "center", dotsize = 1,
                 binwidth = 1/48, color = "white", fill = "black")+
    geom_errorbar(data = mean_ci %>% as_tibble(),
                  aes(x = age_category, ymin = lower_ci, ymax = upper_ci), width = .05, color = "cornflowerblue")+
    geom_point(data = mean_ci %>% as_tibble(), aes(x = age_category, y = mean),
               shape = 22, color = "cornflowerblue", fill = "white") +
    theme(text = element_text(size=15, family = "serif")) +
    labs(y = "Participants' Proportion Correct", x = "Age")
  )

}

#Plotting performance against age
{
  ggplot(data = data_plot, aes(x = age.in.years, y = accu.testing)) +
    geom_point(color ="black", shape = 16)+
    geom_smooth(method = "lm", col = "red") + 
    theme(text = element_text(size=15, family = "serif")) +
    labs(y = "Participants' Proportion Correct", x = "Age")
}

#Get p-value for slides comparison
online.slides.means <- emmeans(online.slides.mods, specs = "study", type = "response")
pairs(online.slides.means)
# Exploratory analysis

# summarize training accuracy by age groups 
{
  data_age <- data %>% 
    group_by(age.year) %>% 
    mutate(pass.train.rate = (length(pass.training[pass.training=="Yes"])/length(pass.training))) %>% 
    mutate(age.mean.accu = mean(accu.training)) %>% mutate(age.mean.test = mean(accu.testing)) %>% ungroup()
  (df_age <- data_age %>% select(age.year, age.mean.accu, pass.train.rate, age.mean.test) %>% group_by(age.year) %>% summarize(count=n(), age.mean.accu = mean(age.mean.accu), pass.train.rate = mean(pass.train.rate), age.mean.test = mean(age.mean.test)))
}

# testing trial accuracy dot plot 
{
  ggplot(data = data_pass, aes(x = age.in.years, y = accu.testing, color = sex)) +
    geom_point() +
    geom_point( size = 3) +
    scale_color_manual(values = c("M" = "skyblue", "F" = "gold")) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(3, 11), breaks = 3:11)
  }

#testing trial accuracy by trial
{
  over_mean_accu <- mean(c( mean(data_pass$t6.correct), mean(data_pass$t5.correct), mean(data_pass$t4.correct), mean(data_pass$t3.correct),mean(data_pass$t2.correct), mean(data_pass$t1.correct)))
  df_accu_test_pass <- data.frame(Trial = c("Overall", "t6", "t5", "t4", "t3", "t2", "t1"), Accuracy = c(over_mean_accu, mean(data_pass$t6.correct), mean(data_pass$t5.correct), mean(data_pass$t4.correct), mean(data_pass$t3.correct),mean(data_pass$t2.correct), mean(data_pass$t1.correct)))
  barplot(df_accu_test_pass$Accuracy, names.arg = df_accu_test_pass$Trial, horiz = TRUE, col = "skyblue",
          main = "Testing Trials Accuracy", xlab = "Accuracy", xlim = c(0, 1),  cex.names = 0.8, cex.axis = 0.8, las = 1) +
    abline(v = 0.5, col = "red", lty = "dashed")
}

#Age <7.5(young) and > 7.5(old) and incremental testing accuracy 
{
  data_young <- data_pass %>% filter(age_category == 0)
  over_mean_accu_y <- mean(c( mean(data_young$t6.correct), mean(data_young$t5.correct), mean(data_young$t4.correct), mean(data_young$t3.correct),mean(data_young$t2.correct), mean(data_young$t1.correct)))
  df_accu_test_pass_y <- data.frame(Trial = c("Overall", "t6", "t5", "t4", "t3", "t2", "t1"), Accuracy = c(over_mean_accu_y, mean(data_young$t6.correct), mean(data_young$t5.correct), mean(data_young$t4.correct), mean(data_young$t3.correct),mean(data_young$t2.correct), mean(data_young$t1.correct)))
  barplot(df_accu_test_pass_y$Accuracy, names.arg = df_accu_test_pass_y$Trial, horiz = TRUE, col = "skyblue",
          main = "Testing Trials Accuracy", xlab = "Accuracy", xlim = c(0, 1),  cex.names = 0.8, cex.axis = 0.8, las = 1) +
    abline(v = 0.5, col = "red", lty = "dashed")
  data_old <- data_pass %>% filter(age_category == 1)
  over_mean_accu_o <- mean(c( mean(data_old$t6.correct), mean(data_old$t5.correct), mean(data_old$t4.correct), mean(data_old$t3.correct),mean(data_old$t2.correct), mean(data_old$t1.correct)))
  df_accu_test_pass_o <- data.frame(Trial = c("Overall", "t6", "t5", "t4", "t3", "t2", "t1"), Accuracy = c(over_mean_accu_o, mean(data_old$t6.correct), mean(data_old$t5.correct), mean(data_old$t4.correct), mean(data_old$t3.correct),mean(data_old$t2.correct), mean(data_old$t1.correct)))
  barplot(df_accu_test_pass_o$Accuracy, names.arg = df_accu_test_pass_o$Trial, horiz = TRUE, col = "skyblue",
          main = "Testing Trials Accuracy", xlab = "Accuracy", xlim = c(0, 1),  cex.names = 0.8, cex.axis = 0.8, las = 1) +
    abline(v = 0.5, col = "red", lty = "dashed")
}
