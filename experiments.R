# Import libraries
library(tidyverse)
library(corrplot)
library(modelr)
library(broom)
library(FactoMineR)
library(factoextra)
library(cluster)
library(plotly)

###################
### IMPORT DATA ###
###################

# Read in abalone dataset (data from https://archive.ics.uci.edu/ml/datasets/abalone)
#data <- read.csv("data/abalone.data", header = FALSE) %>% as.data.frame()
#colnames(data) <- c("sex", "length", "diameter", "height", "whole.weight", "shucked.weight", "viscera.weight", "shell.weight", "rings")

#write.csv(data, "data/abalone.csv", row.names = FALSE)

data <- read.csv("abalone.csv", header = TRUE)

# make "sex" variable a factor (I = infant)
# multiple measurements by 200 (see abalone.names)
# add an "age" variable based on rings (rings + 1.5)
# for the sake of neatness we will use (rings + 1)
data <- data %>% 
  mutate(sex = case_when(sex == "M" ~ "male",
                         sex == "F" ~ "female",
                         sex == "I" ~ "infant")) %>% 
  mutate(sex = factor(sex, levels = c("male", "female", "infant")),
         across(.cols = colnames(.)[2:8], ~ .x*200),
         age = rings + 1) %>% 
  select(-rings)


##########################
### EXPLORE/CLEAN DATA ###
##########################

# data summary
summary(data)

# number of observations
nrow(data)

# variable classes
data %>% sapply(class)

# check if there are any entries with missing values
data %>% filter(if_any(.fns = is.na))

# alternatively, count missing values for each variable
data %>%
  mutate(across(.fns = is.na)) %>% 
  mutate(across(.fns = as.numeric)) %>% 
  colSums()

# select distinct
data %>% distinct() %>% nrow()

######################
### VISUALIZE DATA ###
######################

# age histogram
data %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 1, color = "black", alpha = 0.5) +
  labs(x = "Age (years)", y = "Frequency", title = "Abalone Age Distribution",
       subtitle = paste0("For n = ", nrow(data), " observations"))

# age area plots for gender
data %>% 
  ggplot(aes(x = age, group = sex, fill = sex, color = sex)) +
  geom_area(stat = "bin", alpha = 1/2, binwidth = 1)

data %>% 
  ggplot(aes(x = age, group = sex, fill = sex, color = sex)) +
  geom_histogram(alpha = 1/2, binwidth = 1)

# length ~ age
data %>% 
  ggplot(aes(x = age, y = length)) + 
  geom_point(alpha = 0.1)

# diameter ~ age
data %>% 
  ggplot(aes(x = age, y = diameter)) + 
  geom_point(alpha = 0.1)

# height ~ age
data %>% 
  filter(height < 60) %>% 
  ggplot(aes(x = age, y = height)) +
  geom_point(alpha = 0.1)

# histograms for size measurements (unscaled)
data %>% 
  filter(height < 60) %>% 
  pivot_longer(cols = 2:4, names_to = "measurement.type", values_to = "measurement") %>%
  mutate(measurement.type = as.factor(measurement.type)) %>% 
  ggplot(aes(x = measurement, group = measurement.type, fill = measurement.type, color = measurement.type)) + 
  geom_histogram(alpha = 1/2, binwidth = 2, position = "identity") +
  #geom_area(mapping = aes(x = measurement, color = measurement.type), stat = "bin", 
  #          binwidth = 4, position = "identity", inherit.aes = FALSE, alpha = 0, size = 1) +
  labs(x = "Measurement (mm)", y = "Count", title = "Length, Diameter, and Height Measurement Distributions", 
       subtitle = paste0("For n = ", nrow(filter(data, data$height < 60)), " observations (height outliers removed)"))

# whole.weight ~ age
data %>% 
  ggplot(aes(x = age, y = whole.weight)) + 
  geom_point(alpha = 0.1)

# shucked.weight ~ age
data %>% 
  ggplot(aes(x = age, y = shucked.weight)) + 
  geom_point(alpha = 0.1)

# viscera.weight ~ age
data %>% 
  ggplot(aes(x = age, y = viscera.weight)) + 
  geom_point(alpha = 0.1)

# shell.weight ~ age
data %>% 
  mutate(adult = case_when(sex == "infant" ~ FALSE, TRUE ~ TRUE)) %>% 
  ggplot(aes(x = age, y = shell.weight, color = adult)) + 
  geom_point(alpha = 0.1, position = "jitter")
  
# histograms for weight measurements (unscaled)
data %>% 
  ggplot() + 
  geom_histogram(mapping = aes(x = whole.weight),   alpha = 1/4, binwidth = 5, fill = "limegreen") + 
  geom_histogram(mapping = aes(x = shucked.weight), alpha = 1/4, binwidth = 5, fill = "brown1") +
  geom_histogram(mapping = aes(x = viscera.weight), alpha = 1/4, binwidth = 5, fill = "cyan") +
  geom_histogram(mapping = aes(x = shell.weight),   alpha = 1/4, binwidth = 5, fill = "darkgoldenrod") +
  labs(x = "Weight (g)", y = "Count", title = "Weight (Whole, Shucked, Viscera, Shell) Distributions", 
       subtitle = paste0("For n = ", nrow(data), " observations"))

# area plots for weight measurements (unscaled)
data %>% 
  pivot_longer(cols = 5:8, names_to = "weight.type", values_to = "weight") %>%
  mutate(weight.type = case_when(weight.type == "whole.weight" ~ "whole",
                                 weight.type == "shucked.weight" ~ "shucked",
                                 weight.type == "viscera.weight" ~ "viscera",
                                 weight.type == "shell.weight" ~ "shell",)) %>% 
  mutate(weight.type = as.factor(weight.type)) %>% 
  ggplot(aes(x = weight, group = weight.type, fill = weight.type, color = weight.type)) + 
  geom_area(stat = "bin", alpha = 1/3, binwidth = 4) + 
  labs(x = "Weight (g)", y = "Count", title = "Weight (Whole, Shucked, Viscera, Shell) Distributions", 
       subtitle = paste0("For n = ", nrow(data), " observations"), fill = "Weight\nType", color = "Weight\nType") +
  scale_x_continuous(breaks = seq(0,600,100), limits = c(0,600))

data %>% 
  ggplot() + 
  geom_area(mapping = aes(x = whole.weight),   stat = "bin", alpha = 1/3, binwidth = 5, fill = "limegreen") + 
  geom_area(mapping = aes(x = shucked.weight), stat = "bin", alpha = 1/3, binwidth = 5, fill = "brown1") +
  geom_area(mapping = aes(x = viscera.weight), stat = "bin", alpha = 1/3, binwidth = 5, fill = "cyan") +
  geom_area(mapping = aes(x = shell.weight),   stat = "bin", alpha = 1/3, binwidth = 5, fill = "darkgoldenrod") +
  labs(x = "Weight (g)", y = "Count", title = "Weight (Whole, Shucked, Viscera, Shell) Distributions", 
       subtitle = paste0("For n = ", nrow(data), " observations"))

# check correlation matrix
data %>% 
  select(-sex) %>% 
  scale() %>% 
  cor(method = "spearman") %>% 
  corrplot(method = "color", col.lim = c(0,1))

p <- data %>% 
  filter(height < 60) %>% 
  ggplot(aes(x = diameter, y = shell.weight, fill = age)) + 
  geom_point(shape = 21, alpha = 0.3, color = "white", size = 2) +
  scale_fill_gradientn(colors = rainbow(5)) +
  labs(x = "Diameter (mm)", y = "Shell Weight (g)", fill = "Age (yrs)",
       title = "Abalone Age ~ Diameter + Shell Weight"); ggplotly(p)

data %>% 
  filter(height < 60) %>% 
  mutate(age.class = case_when(sex == "infant" ~ "infant", TRUE ~ "adult")) %>% 
  ggplot(aes(x = age, y = height)) + 
  geom_point(alpha = 0.5, shape = 21) +
  facet_wrap(~ age.class)

############################
### STATISTICAL ANALYSIS ###
############################

# two sample t-test
t.test(scale(data$length) - scale(data$diameter))

model_l <- data %>% lm(age ~ length, data = .)
model_l %>% glance

model_d <- data %>% lm(age ~ diameter, data = .)
model_d %>% glance

model_h <- data %>% lm(age ~ height, data = .)
model_h %>% glance

model_dh <- data %>% lm(age ~ diameter + height, data = .)
model_dh %>% glance

model_ldh <- data %>% lm(age ~ length + diameter + height, data = .)
model_ldh %>% glance

model_vol <- data %>% 
  mutate(volume = length*diameter*height) %>% 
  mutate(volume_n = volume/max(volume)) %>% 
  lm(age ~ volume, data = .)
model_vol %>% glance

model_s <- data %>% lm(age ~ sex, data = .)
model_s %>% tidy; model_s %>% glance

model_w1 <- data %>% lm(age ~ whole.weight, data = .)
model_w1 %>% tidy; model_w1 %>% glance

model_w2 <- data %>% lm(age ~ shucked.weight, data = .)
model_w2 %>% tidy; model_w2 %>% glance

model_w3 <- data %>% lm(age ~ viscera.weight, data = .)
model_w3 %>% tidy; model_w3 %>% glance

model_w4 <- data %>% lm(age ~ shell.weight, data = .)
model_w4 %>% tidy; model_w4 %>% glance

model <- data %>% 
  mutate(age.group = case_when(sex == "infant" ~ "infant", TRUE ~ "adult")) %>% 
  lm(age ~ age.group + height + shell.weight, data = .)
model %>% tidy; model %>% glance

data.new <- data %>% 
  mutate(age.group = case_when(sex == "infant" ~ "infant", TRUE ~ "adult")) %>% 
  select(age.group, height, shell.weight, age)

ab.mca <- MCA(data.new, quanti.sup = 2:3, quali.sup = 1, graph = FALSE)














