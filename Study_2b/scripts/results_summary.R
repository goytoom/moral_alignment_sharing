rm(list = ls())
library(tidyverse)

#### Load Model comparisons
results_comparison <- readRDS("../results/results_comparison.rds") #compares models out-of-sample prediction performance

#### Main Model Summaries
results_m4 <- readRDS("../results/m4_effects.rds") #summarizes results for the main model
results_m6 <- readRDS("../results/m6_effects.rds") #summarizes results for the main model (controlled for veracity)
results_mediation <- readRDS("../results/results_mediation.rds") #shows that the effect of matching moral values and post framing is mediated by perceived alignment and agreement with a post

##### Plots #########
df_results <- data.frame(values = character(),    # Create empty data frame
                         framing = character(),
                         effect = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         stringsAsFactors = FALSE)

df_results[1,] <- list("Binding", "Binding", 0.26,  0.16,   0.36)
df_results[2,] <- list("Binding", "Individualizing", 0.14,  0.04,  0.24)
df_results[3,] <- list("Binding", "Nonmoral", 0.20,  0.10,   0.30)
df_results[4,] <- list("Individualizing", "Binding", 0.07,  -0.01,  0.15)
df_results[5,] <- list("Individualizing", "Individualizing", 0.23,  0.16,   0.31)
df_results[6,] <- list("Individualizing", "Nonmoral", -0.05, -0.14,  0.04)
df_results[7,] <- list("Proportional", "Binding", -0.00, -0.09,  0.09)
df_results[8,] <- list("Proportional", "Individualizing", -0.05, -0.14,  0.04)
df_results[9,] <- list("Proportional", "Nonmoral", -0.03, -0.12,  0.06)

df_results[10,] <- list("Binding", "Individualizing", 0.12,  0.03,   0.21)
df_results[11,] <- list("Binding", "Nonmoral", 0.06,  -0.04,  0.15)
df_results[12,] <- list("Individualizing", "Binding", 0.16,  0.09,   0.24)
df_results[13,] <- list("Individualizing", "Nonmoral", 0.10,  0.01,   0.18)


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
