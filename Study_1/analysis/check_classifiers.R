rm(list = ls())
library(tidyverse)
library(rstudioapi)

### Manually Check Stance Detection
antivax <- read_csv("../data/stance/antivax_stance_tweets.csv")
provax <- read_csv("../data/stance/provax_stance_tweets.csv")

u_anti <- antivax %>% distinct(user_id, .keep_all = T)
u_pro <- provax %>% distinct(user_id, .keep_all = T)

set.seed(0)
sample_anti <- antivax %>% distinct(user_id) %>%
       sample_n(500) %>% left_join(antivax)

write.csv(sample_anti, "verify_stance_anti.csv")

sample_pro <- provax %>% distinct(user_id) %>%
  sample_n(500) %>% left_join(provax)

write.csv(sample_pro, "verify_stance_pro.csv") #manual check of stance detection!


########### Analyse data independent of stance information
df_binding <- read_csv("../results/final_tweets_binding.csv") %>% 
  dplyr::select(-c(full_text, label)) %>% mutate(binding = factor(binding), 
                                                 individual = factor(individual))%>% drop_na()
df_moral <- read_csv("../results/final_tweets_moral.csv") %>% 
  dplyr::select(-c(full_text, label)) %>% mutate(moral = factor(moral, 
                                                                labels = c("non-moral", "moral"))) %>% drop_na()
df_full <- read_csv("../results/final_tweets_full.csv") %>% 
  dplyr::select(-c(full_text, label)) %>% mutate(care = factor(care), fairness = factor(fairness),
                                                 authority = factor(authority), purity = factor(purity),
                                                 loyalty = factor(loyalty)) %>% drop_na()

##### Distribution of moral values:

## moral model
means_moral <- mean(as.numeric(df_moral$moral))-1
means_moral
# 25% moral tweets vs 75 non moral tweets

## binding model
means_binding <- colMeans(sapply(df_binding[, (ncol(df_binding)-1):ncol(df_binding)], as.numeric))-1
means_binding
#6% of all tweets are binding
#18% of all tweets are individualizing

means_binding/means_moral
#23% of the moral messages are binding
#72% of the moral messages are individualizing

### all foundations model:
means_all <- colMeans(sapply(df_full[, (ncol(df_full)-4):ncol(df_full)], as.numeric))-1
means_all
# 12% care
# 6% fairness
# 0.8% loyalty
# 1.1% authority
# 0.4% purity

means_all/means_moral
# 48% care
# 23% fairness
# 3% loyalty
# 4.5% authority
# 1.5% purity










