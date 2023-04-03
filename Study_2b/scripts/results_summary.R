rm(list = ls())
library(tidyverse)

#### Load Model comparisons
results_comparison <- readRDS("../results/results_comparison.rds") #compares models out-of-sample prediction performance

#### Main Model Summaries
results_m4 <- readRDS("../results/m4_effects.rds") #summarizes results for the main model
results_m6 <- readRDS("../results/m6_effects.rds") #summarizes results for the main model (controlled for veracity)
results_mediation <- readRDS("../results/results_mediation.rds") #shows that the effect of matching moral values and post framing is mediated by perceived alignment and agreement with a post

######## Evaluate indices
library(ltm)
# Load data
dl <- read_rds("../data/dl_R.rds")

# Get Post Share Index
dl <- dl %>% mutate(
  post_share_index = (post_share_public + post_like_public + post_share_private + post_share_offline)/4,
  post_deliberation_index = (post_consideration + post_knowledge + post_thought + post_self)/4
) %>% drop_na(-strata, -source_text)

# calculate alpha
cronbach.alpha(dl[, c("post_share_public", "post_like_public", "post_share_private", "post_share_offline")], 1, 1)
cronbach.alpha(dl[, c("post_thought", "post_consideration", "post_knowledge", "post_self")], 1, 1)
