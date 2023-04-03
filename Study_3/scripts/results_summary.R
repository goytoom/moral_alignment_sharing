rm(list = ls())

# Notes -------------------------------------------------------------------
# This scripts loads and outputs summaries for the main models and the mediations (Study 3)
# poentially split files (summary, plots, ...)
# Library -----------------------------------------------------------------

library(tidyverse)

#### Load Model comparisons
results_comparison <- readRDS("../results/results_comparison.rds")

# Show main model summaries (moral alignment & mediation)
results_m4 <- readRDS("../results/m4_effects.rds")
results_m6 <- readRDS("../results/m6_effects.rds")
results_mediation <- readRDS("../mediations/results_mediation_del.rds")
results_mediation_alt <- readRDS("../mediations/results_mediation_RT.rds") # mediation with alternative mediator (response time)

########### Additional analyses
#### Additional analyses summaries
#m10 shows that for non-moral stimuli, analytical thinking increases truth discernment (reduced sharing of fake-news, compared to true-news)
results_m10 <- readRDS("../results/m10_effects.rds") #positive crt & veracity interaction

### Models show no interaction of CRT and veracity, deliberation & veracity, crt & believability, 
# meaning that analytical thinking and deliberation over sharing a post do not increase truth discernment & plausibility concerns
# (less sharing of fakenews compared to true news; increased effect of headline believability)
results_m7 <- readRDS("../results/m7_effects.rds") #no crt x veracity interaction
results_m8 <- readRDS("../results/m8_effects.rds") #no deliberation x veracity interaction
results_m9 <- readRDS("../results/m9_effects.rds") #no crt x believability interaction

### No interaction of CRT and moral alignment (x_post_framing & mfq) => analytical thinkers are not less susceptible to moral alignment
results_m11 <- readRDS("../results/m11_effects.rds") #no moderation of moral alignment effect by analytical thinking


#replication confirms the mediation of the effect of matching moral values and framing on sharing via perceived alignment and agreement with a post
results_mediation_replication <- readRDS("../mediations/results_mediation_replication.rds")


######## Evaluate indices
# post_share_index:
library(ltm)

# Load data
dl <- read_rds("../data/dl_R.rds")

# Calculate Post: Share Index
dl <- dl %>% mutate(
  post_share_index = (post_share_public + post_like_public + post_share_private + post_share_offline)/4,
  post_deliberation_index = (post_consideration + post_knowledge + post_thought + post_self)/4
) %>% drop_na(-strata, -source_text)

# calculate alpha
cronbach.alpha(dl[, c("post_share_public", "post_like_public", "post_share_private", "post_share_offline")], 1, 1)
cronbach.alpha(dl[, c("post_thought", "post_consideration", "post_knowledge", "post_self")], 1, 1)


