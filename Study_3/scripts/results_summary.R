rm(list = ls())

# Notes -------------------------------------------------------------------
# This scripts loads and outputs summaries for the main models and the mediations (Study 3)
# poentially split files (summary, plots, ...)
# Library -----------------------------------------------------------------

library(tidyverse)
library(hrbrthemes)

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

## plot for m10
df_m10 <- data.frame(veracity = character(),    # Create empty data frame
                         effect = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         stringsAsFactors = FALSE)

df_m10[1,] <- list("False", -0.10, -0.17,  -0.02)
df_m10[2,] <- list("True", -0.04, -0.14,  0.05)

ggplot(df_m10, aes(x = veracity, y=effect)) + geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width = 0.5, position=position_dodge(width=0.9)) + 
  ggtitle("Effect of analytical thinking on nonmoral posts") + xlab("Headline veracity") + ylab("Effect size") + 
  scale_fill_discrete(name = "Framing") + theme_minimal() + 
  theme(axis.text.x = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),  
        axis.title.x = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"),
        axis.title.y = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"))


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

##### Plots #########
df_results <- data.frame(values = character(),    # Create empty data frame
                         framing = character(),
                         effect = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         stringsAsFactors = FALSE)

df_results[1,] <- list("Binding", "Binding", 0.26,  0.17,   0.34)
df_results[2,] <- list("Binding", "Individualizing", 0.11,  0.02,   0.20)
df_results[3,] <- list("Binding", "Nonmoral", 0.15,  0.06,   0.24)
df_results[4,] <- list("Individualizing", "Binding", 0.11,  0.03,   0.19)
df_results[5,] <- list("Individualizing", "Individualizing", 0.26,  0.18,   0.34)
df_results[6,] <- list("Individualizing", "Nonmoral", 0.13,  0.06,   0.21)
df_results[7,] <- list("Proportional", "Binding", 0.01,  -0.08,  0.10)
df_results[8,] <- list("Proportional", "Individualizing", -0.01, -0.10,  0.08)
df_results[9,] <- list("Proportional", "Nonmoral", 0.02,  -0.07,  0.11)

df_results[10,] <- list("Binding", "Individualizing", 0.14,  0.05,   0.23)
df_results[11,] <- list("Binding", "Nonmoral", 0.11,  0.02,   0.19 )
df_results[12,] <- list("Individualizing", "Binding", 0.15,  0.08,   0.22)
df_results[13,] <- list("Individualizing", "Nonmoral", 0.13,  0.05,   0.20)


# within-groups
ggplot(df_results[1:6,], aes(x = values, y=effect, fill=framing)) + geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width = 0.5, position=position_dodge(width=0.9)) + 
  ggtitle("Moral values across framing conditions") + xlab("Moral Values") + ylab("Effect size") + 
  scale_fill_discrete(name = "Framing") + theme_minimal() + 
  theme(axis.text.x = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),  
        axis.title.x = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"),
        axis.title.y = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"))

# between-groups
ggplot(df_results[10:13,], aes(x = values, y=effect, fill=framing)) + geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.3, size=0.5, position=position_dodge(width=0.9)) + 
  ggtitle("Moral alignment vs misalignment") + xlab("Moral Values") + ylab("Effect size difference") + 
  scale_fill_discrete(name = "Framing Contrast") + theme_minimal() + 
  theme(axis.text.x = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),  
        axis.title.x = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"),
        axis.title.y = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"))


