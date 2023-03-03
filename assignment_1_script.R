# Instructions Questions 1 ----

# Participants are to respond to words appearing in contexts of:
# Neutral, Negative, and Positive

# n = 24
# item = 24 for each context
# Repeated Measures Design

# Does context influence the response time (dv = ms)?

library(styler)

# Load package ----

library(tidyverse) # for various commands like read.csv and mutate to handle data

library(visdat) # for missing value visualization

library(afex) # for anova analysis

library(lme4) # for building mixed models

library(emmeans) # for post hoc tests

library(blme) # for Bayesian statistics model 

library(performance) # to check for normality

library(fitdistrplus) # to check for distribution fit

library(betareg) # for building beta distributions

# Read in the data ----

raw_data_1 <- read.csv("assignment1_data1.csv")

str(raw_data_1)

# Test/Visualize Missing Values ----

vis_miss(raw_data_1)

  # there is none that is missing, also simpler may be is.na

any(is.na(raw_data_1$DV))

  # False

# Tidy the data ----

# Make all letters, conditions, column titles snake case
# Make condition, item, and subject factors for ANOVA

tidied_data_1 <- raw_data_1 %>%
  rename(
    "subject" = "subj",
    "response_time" = "DV"
    ) %>%
  mutate(
    condition = factor(tolower(condition)
                       )
    )
         
# renames variables to complete meaningful words, and snake case per tidyverse format

# change condition variable type

# conditions changed to lower case to match snake case format

tidied_data_1$subject <- as.factor(gsub("S", "", tidied_data_1$subject)) # appends values of subject specifically

# gsub parameters: (letter to replace, replace with nothing, look for letter in subject)

tidied_data_1$item <- as.factor(gsub("I", "", tidied_data_1$item)) # appends values of item specifically

# subject and item changed to factor with a different method because gsub would otherwise revert column to character if done in a previous line

str(tidied_data_1)

# Dealing with Unequal Sample Size ----

tidied_data_1 %>%
  count(condition, subject) # frequency of each condition

tidied_data_1 %>%
  group_by(subject) %>%
  select(subject, condition) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = condition,
              values_from = n) %>%
  view()

# I found that there is 2 less data point; neutral participant 18, negative participant 9
# Unequal sample size can increase the type 1 error rate in ANOVA's

# Does it matter? 

  # It can increase type I error when unequal sample size is added with unequal variance

# Why does unequal variance matter?

f_result <- var.test(response_time ~ condition, data = tidied_data_1) # can only compare 2 different groups

# Descriptives & Visualizations ----

# Mean and standard deviation
tidied_data_1 %>%
  group_by(condition) %>%
  summarise(mean = mean(response_time),
            standard_deviation = sd(response_time)
            ) %>%
  arrange(mean)

# Violin distribution plot

set.seed(1234)
tidied_data_1 %>%
  ggplot(aes(x = fct_reorder(condition, response_time), y = response_time, color = condition)) +
  geom_violin(alpha = .5) +
  geom_jitter(alpha = .5, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", color = "black") +
  guides(color = "none") +
  labs(x = "Context",
       y = "Response Time (ms)",
       title = "Effect of Word Context on Response Time") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5))

  # Each condition differs by about 100 milliseconds from negative, neutral, to positive

# Dot plot
tidied_data_1 %>%
  ggplot(aes(x = condition, y = response_time, fill = condition)) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center", 
               stackratio = .5, 
               dotsize = 0.5,
               alpha = .5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = .5) +
  guides(fill = "none") +
  labs(x = "Contexts",
       y = "Response Time (ms)",
       title = "Effect of Word Context on Response Time") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5))

# Data distribution
tidied_data_1 %>%
  group_by(condition) %>%
  summarise(min = min(response_time),
            median = median(response_time),
            max = max(response_time),
            range = max(response_time) - min(response_time)
            ) %>%
  arrange(median)

# Box plot
tidied_data_1 %>%
  ggplot(aes(x = condition, y = response_time, color = condition)) +
  geom_boxplot(alpha = .8) +
  guides(color = "none") +
  labs(x = "Context",
       y = "Response Time (ms)",
       title = "Effect of Word Context on Response Time"
       ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5)
        )

# Check assumptions ----

check_model(mixed_model_1)

# Check visually
# Posterior Predictive Check has a bell shaped curve
# Linearity: relatively distributed around, more spread further to the right
# Homogeneity: relatively distributed around the line, more spread out further to the right
# Normality of Response Time: It is close the line with a little departure towards the end
# Normality of random effects for both subject and item: both stick close to the line so are normal

# Building mixed model ----
# In mixed models subjects are random effect so we are saying that we care about the general population as represented by these specific subjects

mixed_model_1 <- lmer(response_time ~ condition + (1 | subject) + (1 | item), data = tidied_data_1)

summary(mixed_model_1)

  # We find both neutral and positive is significantly different from negative 

# Compare with model with no fixed effect? ----

mixed_model_1_null <- lmer(response_time ~ (1 | subject) + (1 | item), data = tidied_data_1)

anova(mixed_model_1, mixed_model_1_null)

  # smalled value of AIC and BIC with model with fixed effect so it is a better fit

# Compare with ANOVA score? ----
# In ANOVA subjects are not a random effect so we are saying we care about the specific subjects

anova_model_1 <- aov_4(response_time ~ condition + (1 + condition | subject), data = tidied_data_1)

summary(anova_model_1)

  # We Find that there is significance
  # Test of sphericity is above .5 so it is good 

# Post Hoc

emmeans(anova_model_1, pairwise ~ condition, adjust = "bonferroni")

  # we find that there is only a difference between the negative and positive condition
  # so which one, anova, or mixed models is better

# Use Bayesian? ----

bay_model_1 <- blmer(response_time ~ condition + (1 | subject) + (1 | item), data = tidied_data_1)

summary(bay_model_1)

# Instructions Question 2 ----

# 2 x 2 repeated measures design
# Emotional face as presenting: Anger / Fear
# Story as prime: Angry / Fearful situation described

# n = 32
# items = 32
# DV = response time in milliseconds

# How does Story Emotion and Face expression affect people's response times?

# Read in data ----

raw_data_2 <- read.csv("assignment1_data2.csv")

# Tidy data ----

str(raw_data_2)

(is.na(raw_data_2$RT))

tidied_data_2 <- raw_data_2 %>%
  rename("subject" = "Subject",
         "vignette" = "Vignette",
         "story_emotion" = "StoryEmotion",
         "face_expression" = "FaceExpression",
         "reaction_time" = "RT"
         ) %>%
  mutate(story_emotion = factor(tolower(story_emotion)),
         face_expression = factor(tolower(face_expression))
         )

# Descriptives and Visualization ----

# Factor 1 only
tidied_data_2 %>%
  group_by(story_emotion) %>%
  summarise(mean = mean(reaction_time),
            standard_deviation = sd(reaction_time)
            )

  # we see that fear takes slightly longer to process

# Factor 2 only

tidied_data_2 %>%
  group_by(face_expression) %>%
  summarise(mean = mean(reaction_time),
            standard_deviation = sd(reaction_time)
  )

  # we see that fear primes makes reaction times longer 

# Both

tidied_data_2 %>%
  group_by(story_emotion, face_expression) %>%
  summarise(mean = mean(reaction_time),
            standard_deviation = sd(reaction_time)
  ) %>%
  arrange(.by_group = TRUE, -mean) # organize reaction time mean from longest to fastest; default would be from fastest to longest, "-" inverts the order
                                  # due to how arrange by default ignores groupings I had to first set groupings to TRUE

# congruent trials are faster for both primes than incongruent

# Violin distribution plot

prime_label <- c(anger = "Angry Prime", 
                 fear = "Fearful Prime"
                 ) # create character vector as lookup table 

set.seed(1234) # allows random aspects fo "geom_jitter" to be reproduced
tidied_data_2 %>%
  ggplot(aes(
    x = story_emotion:face_expression, # set x values by both conditions
    y = reaction_time, # set y values
    color = story_emotion:face_expression # separate groups by colour
    ) 
    ) +
  geom_violin(alpha = .5) + # create violin distribution; increase transparency
  geom_jitter(alpha = .5, # show jittered data points; increase transparency
              width = .1 # increase space between points
              ) + 
  stat_summary(fun.data = "mean_cl_boot", # show 95% confidence level
               color = "black" # change colour to black
               ) +
  guides(color = "none") + # removes legend
  labs(x = "Facial Expression", # sets x, y, and title labels
       y = "Reaction Time (ms)",
       title = "Reaction time of Congruency of Primed Emotion on Emotional Facial Expression"
       ) +
  facet_wrap(~ story_emotion, # wraps plot into separate panels 
             scale = "free_x", # scale for x in each panel is free
             labeller = labeller(story_emotion = prime_label) # changes label based on lookup table
             ) + 
  scale_x_discrete(labels = c("Anger", "Fear", "Anger", "Fear")) + # change x tick labels
  theme(plot.title = element_text(hjust = .5)) # center title

# Setting up contrast ----

# Next we are going to build linear models where we assume normal distribution

contrasts(tidied_data_2$story_emotion) <- matrix(c(.5, -.5)) 

contrasts(tidied_data_2$face_expression) <- matrix(c(.5, -.5))

  # contrast set so we look at main effects instead of simple effects
  # What does main effect and simple effect mean?
  # What does the matrix actually do?; what do the values (.5, -.5) mean?

# Build model linear ----

mixed_model_2 <- lmer(reaction_time ~ story_emotion * face_expression +
                              (1 + face_expression | subject) +
                              (1 | vignette),
                            data = tidied_data_2
                            )

# Check model assumptions ----

check_model(mixed_model_2)

# Is normal with falling off at the end, bell curved shaped
# Linearity and homogeneity met
# Collinearity is low
# Normality of random effects for subjects and vignettes are low

# Interpret Results and Post Hoc Tests ----

# Results of linear model
summary(mixed_model_2)

  # significance of interaction effect from grand mean only

emmeans(mixed_model_2, pairwise ~ story_emotion * face_expression, adjust = "bonferroni") 

  # Anger Anger - Anger Fear : is significant <.001; congruent (1865) is faster than incongruent (2678)
  # Fear Fear - Fear Anger : is significant =.001; congruent (2034) is faster than incongruent (2591)

  # There is greater difference in reaction time for the angry prime conditions 

# Alternate model (beta) ----

descdist(tidied_data_2$reaction_time)

# We see distribution closer to a beta distribution than normal

beta_model <- betamix(reaction_time ~ story_emotion + face_expression, data = tidied_data_2)

summary(beta_model)

# Alternate model (gamma) ----

gamma_model <- glmer(reaction_time ~ story_emotion * face_expression + (1 | subject),
                     family = Gamma, data = tidied_data_2)

summary(gamma_model)

emmeans(gamma_model, pairwise ~ story_emotion * face_expression, adjust = "bonferroni") 

  # we see same thing in gamma distributed model as linear model 

## Test for Individual Differences ----

tidied_data_1_wider <- tidied_data_1 %>%
  select(subject, response_time, item) %>%
  pivot_wider(names_from = "subject",
              values_from = "response_time")
