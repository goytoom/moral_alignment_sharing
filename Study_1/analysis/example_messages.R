rm(list = ls())
library(tidyverse)
library(brms)

# add all foundations! (create new file)
# write slurm code
# install r/packages on cluster


#check tweets and create 100 examples (all foundations)


df_anti_full <- read_csv("../results/stance/antivax_full.csv")
df_pro_full <- read_csv("../results/stance/provax_full.csv")

df_total_full <- rbind(df_anti_full, df_pro_full) %>%
  mutate(stance = factor(stance_label, labels = c("anti-vax", "pro-vax")))


#select random example of 20 for each foundation, potentially manually look for better matches
examples <- df_total_full %>% arrange(desc(favorite_count)) %>% mutate(moral_count = rowSums(select(., c(care, fairness, loyalty, authority, purity)))) %>% 
  filter(moral_count == 1 | moral_count == 0) %>% mutate(non_moral = ifelse(moral_count==0, 1, 0)) %>% 
  pivot_longer(c(care, fairness, loyalty, authority, purity, non_moral), names_to = "concern", values_to = "value")  %>% filter(value==1) %>% 
  group_by(concern) %>% do(head(., n=10)) %>% select(-stance, -value, -moral_count, -stance_label)
