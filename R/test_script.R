# Inspired from https://joachimgoedhart.github.io/DataViz-protocols/plotting-the-data.html#discrete-conditions sections: 3.2 / 3.3

# This was used as a template to create the Notebook, it is not functional

# Required libraries loading

library(tidyverse)
library(here)
library(shiny)
library(dplyr)

# Setting up Here package

here::i_am("Stats_from_non_tidy_data.Rproj")


# Load datafile

df <- read.csv('some_data.csv')
head(df)

# Really basic plot
# Attribute variables to axis 

p <- ggplot(data=df, mapping = aes(x=Target, y=Intensity))

p# Generate graph
# With jittered distribution and colors

p <- p + geom_jitter(aes(colour = Strand))
p

# Add mean value layer

p <- p + stat_summary(fun.min=mean, fun.max=mean, geom='errorbar', width=0.6, size=1)

p

# Add SD bars

p <- p + stat_summary(fun.min=function(y) {mean(y) - sd(y)}, fun.max=function(y) {mean(y) + sd(y)}, geom='errorbar', width=0.3, size =1)

p


###------------###


# Get basic statistics

df_summary <- df %>% group_by(Target) %>% summarise(n=n(), mean=mean(Intensity), sd=sd(Intensity))

head(df_summary)

# Get more statistics (MAD, CI etc...) 


Confidence_level = 0.95

df_summary <- df %>%
  group_by(Target) %>%
  summarise(n=n(), mean=mean(Intensity), median=median(Intensity), sd=sd(Intensity)) %>%
  mutate(sem=sd/sqrt(n-1),
         mean_CI_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
         mean_CI_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem
  )
head(df_summary)

# Write .csv file with statistic results

write.csv(df_summary, 'some_stats.csv')

###------------###

# statistical analysis
# Aim is to perform two-sided paired t-test.
# Note: in a two-tailed test the hypothesis to be tested is for two groups A & B,
# H0: mean(A) = mean(B)

# Some preliminary tests are required before proceeding with two-sided paired t-test:

# Shapiro-Wilk test for normally distribution of the two groups.

# Visual interpretation of normal distribution using ggdensity from ggpubr:
ggdensity(df, x = "Intensity", add = "mean", color = "Target", rug = TRUE)
ggdensity(df, x = "Intensity", add = "mean", color = "Target", rug = TRUE) + facet_wrap(~Target)





# F-test verify that the variances of the two groups are equal.





# Open csv file and proceed with statistical analysis  (main one is the two-sided paired t-test.)

stats_df <- read.csv('some_stats.csv')
head(stats_df)


## TODO
## Implement statistical tests on the newly created csv file
## shiny app / interactive way to decide what to compare (which conditions etc...)?