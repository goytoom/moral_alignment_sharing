rm(list = ls())
library(tidyverse)

#################### Main Analyses (Binding/Individualizing values)
#### Retweets
comparison_rt_binding <- readRDS("results/binding/comparison_rt.rds") #compares models out-of-sample prediction performance
results_m2_rt_binding <- readRDS("results/binding/m2_rt_effects.rds") #summarizes results for the main model

#### Favourite count
comparison_fv_binding <- readRDS("results/binding/comparison_fv.rds") #compares models out-of-sample prediction performance
results_m2_fv_binding <- readRDS("results/binding/m2_fv_effects.rds") #summarizes results for the main model


#################### Additional Analyses (All moral foundations)
#### Retweets
comparison_rt_full <- readRDS("results/full/comparison_rt.rds") #compares models out-of-sample prediction performance
results_m2_rt_full <- readRDS("results/full/m2_rt_effects.rds") #summarizes results for the main model

#### Favourite count
comparison_fv_full <- readRDS("results/full/comparison_fv.rds") #compares models out-of-sample prediction performance
results_m2_fv_full <- readRDS("results/full/m2_fv_effects.rds") #summarizes results for the main model


############## Check inter-correlation of tweet classifier (Are the same tweets classified as binding/individualizing?)
# Import data
df_anti_binding <- read_csv("../results/stance/antivax_binding.csv") %>% select(-c(full_text)) 
df_pro_binding <- read_csv("../results/stance/provax_binding.csv") %>% select(-c(full_text))

# Prepare data (filter, merge, convert data type)
df_total_binding <- rbind(df_anti_binding, df_pro_binding) %>% mutate(
  binding = factor(binding),
  individual = factor(individual),
  stance = factor(stance_label, labels = c("anti-vax", "pro-vax"))
)

individual = df_total_binding$individual
individual = as.numeric(levels(individual))[individual]
binding = df_total_binding$binding
binding = as.numeric(levels(binding))[binding]

# calculate inter-correlation
cor.test(individual, binding) #low inter-correlation (meaning that the classifier did not simply classify the same posts as binding and individualizing)
