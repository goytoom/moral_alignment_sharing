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

########## Plots
df_results <- data.frame(group = character(),    # Create empty data frame
                         framing = character(),
                         outcome = character(),
                         effect = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         stringsAsFactors = FALSE)

df_results[1,] <- list("Anti-vax", "Individualizing vs Binding", "Retweets", -0.02, -0.15, 0.11)
df_results[2,] <- list("Pro-vax", "Individualizing vs Binding", "Retweets", 0.41, 0.21, 0.60)
df_results[3,] <- list("Anti-vax vs Pro-vax", "Binding", "Retweets", 0.23, 0.03, 0.42)
df_results[4,] <- list("Anti-vax vs Pro-vax", "Individualizing", "Retweets", -0.20, -0.39, -0.09)

df_results[5,] <- list("Anti-vax", "Individualizing vs Binding", "Likes", -0.39, -0.70,  -0.06)
df_results[6,] <- list("Pro-vax", "Individualizing vs Binding", "Likes", 0.80,  0.20,   1.36)
df_results[7,] <- list("Anti-vax vs Pro-vax", "Binding", "Likes", 0.10,  -0.41,  0.60)
df_results[8,] <- list("Anti-vax vs Pro-vax", "Individualizing", "Likes", -1.07,  -1.38, -0.78)


# between-groups
ggplot(df_results[grepl("vs", df_results$group),], aes(x = framing, y=effect, fill=outcome)) + geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width = 0.5, position=position_dodge(width=0.9)) + 
  ggtitle("Anti-vax vs Pro-vax") + xlab("Moral Framing") + ylab("Effect size difference") +
  scale_fill_discrete(name = "Engagement Metric") + theme_minimal() + 
  theme(axis.text.x = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),  
        axis.title.x = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"),
        axis.title.y = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"))

# within-groups
ggplot(df_results[!grepl("vs", df_results$group),], aes(x = group, y=effect, fill=outcome)) + geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width = 0.5, position=position_dodge(width=0.9)) + 
  ggtitle("Individualizing vs Binding Framing") + xlab("Stance") + ylab("Effect size difference") +
  scale_fill_discrete(name = "Engagement Metric") + theme_minimal() + 
  theme(axis.text.x = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),  
        axis.title.x = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"),
        axis.title.y = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"))


# show the raw effects (to show that moral framing has positive effect; other plots show group differences)
df_results2 <- data.frame(stance = character(),    # Create empty data frame
                         framing = character(),
                         outcome = character(),
                         effect = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         stringsAsFactors = FALSE)

df_results2[1,] <- list("Anti-vax", "Binding", "Likes", 0.49, 0.25, 0.73)
df_results2[2,] <- list("Pro-vax", "Binding", "Likes", 0.38, -0.05, 0.83)
df_results2[3,] <- list("Anti-vax", "Individualizing", "Likes", 0.10, -0.05, 0.26)
df_results2[4,] <- list("Pro-vax", "Individualizing", "Likes", 1.18, 0.93, 1.44)

# between-groups
ggplot(df_results2, aes(x = framing, y=effect, fill = stance)) + geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width = 0.5, position=position_dodge(width=0.9)) + 
  ggtitle("Effect of moral framing on sharing") + xlab("Moral framing") + ylab("Effect size") +
  scale_fill_discrete(name = "Stance") + theme_minimal() + 
  theme(axis.text.x = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 14, hjust = .5, vjust = .5, face = "bold"),  
        axis.title.x = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"),
        axis.title.y = element_text(size = 18, hjust = .5, vjust = .5, face = "bold"))




