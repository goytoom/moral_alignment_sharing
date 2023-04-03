rm(list = ls())
# add all foundations

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  library(brms)
  library(future)
  library(numform)

  # Options
  options(
    mc.cores = 10,
    brms.backend = "cmdstanr",
    future.globals.maxSize = 2147483648
  )


# Prepare -----------------------------------------------------------------

  # Import data
  df_anti_full <- read_csv("antivax_full.csv") %>% 
    select(-c(full_text)) 
  df_pro_full <- read_csv("provax_full.csv") %>% 
    select(-c(full_text))

  # Prepare data
  df_total_full <- rbind(df_anti_full, df_pro_full) %>%
    mutate(
      care = factor(care),
      fairness = factor(fairness),
      loyalty = factor(loyalty),
      authority = factor(authority),
      purity = factor(purity),
      stance = factor(stance_label, labels = c("anti-vax", "pro-vax"))
    )


# Estimate ----------------------------------------------------------------
path0 <- "../results/full/m0_fit_rt.rds"
path1 <- "../results/full/m1_fit_rt.rds"
path2 <- "../results/full/m2_fit_rt.rds"

if(!file.exists(path0)){ # if file already exists, load it instead of running it

# Model 0: Only random intercepts on the user level
  m0_fit <- brm(
    favorite_count ~ stance + (1|user_id), 
    family = "negbinomial", 
    data = df_total_full,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )	

  # Save results as .rds
  write_rds(m0_fit, path0)
} else{
	m0_fit <- readRDS(path0)
}
  
if(!file.exists(path1)){
  # Model 1: Add moral framings
  m1_fit <- brm(
    favorite_count ~ loyalty + authority + purity + care + fairness + stance + (1|user_id),    family = "negbinomial", 
    data = df_total_full,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )

  # Save results as .rds
  write_rds(m1_fit, path1)
} else{
	m1_fit <- readRDS(path1)
}

if(!file.exists(path2)){  
  # Model 2: Add interaction
  m2_fit <- brm(
    favorite_count ~ (loyalty + authority + purity + care + fairness)*stance + (1|user_id),
    family = "negbinomial", 
    data = df_total_full,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )

  # Save results as .rds
  write_rds(m2_fit, path2)
} else{
	m2_fit <- readRDS(path2)
}

# Compare -----------------------------------------------------------------

  # Run 10-fold cross-validation
  plan(multisession)
  m0_fit <- add_criterion(
    m0_fit,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m1_fit <- add_criterion(
    m1_fit,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m2_fit <- add_criterion(
    m2_fit,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  plan("default")

  # Calculate Bayesian R2
  m0_fit <- add_criterion(
    m0_fit, 
    criterion = "bayes_R2"
  )
  m1_fit <- add_criterion(
    m1_fit, 
    criterion = "bayes_R2"
  )
  m2_fit <- add_criterion(
    m2_fit, 
    criterion = "bayes_R2"
  )
  

# Export ------------------------------------------------------------------
  
  # Save results as .rds
  write_rds(m0_fit, path0)
  write_rds(m1_fit, path1)
  write_rds(m2_fit, path2)
  
# Main Results ------------------------------------------------------------
  ######## Extract coefficients
  m2_rt_results <- m2_fit %>% 
    spread_draws(
      `b_care1`,
      `b_fairness1`,
      `b_loyalty1`,
      `b_authority1`,
      `b_purity1`,
      `b_care1:stanceproMvax`,
      `b_fairness1:stanceproMvax`,
      `b_loyalty1:stanceproMvax`,
      `b_authority1:stanceproMvax`,
      `b_purity1:stanceproMvax`
    ) %>% 
    transmute(
      .chain, .iteration, .draw,
      #foundation effects antivax
      b_care_anti = `b_care1`,
      b_fair_anti = `b_fairness1`,
      b_loya_anti = `b_loyalty1`,
      b_auth_anti = `b_authority1`,
      b_puri_anti = `b_purity1`,
      #foundation effects provax
      b_care_pro = `b_care1`       + 1 * `b_care1:stanceproMvax`,
      b_fair_pro = `b_fairness1`   + 1 * `b_fairness1:stanceproMvax`,
      b_loya_pro = `b_loyalty1`    + 1 * `b_loyalty1:stanceproMvax`,
      b_auth_pro = `b_authority1`  + 1 * `b_authority1:stanceproMvax`,
      b_puri_pro = `b_purity1`     + 1 * `b_purity1:stanceproMvax`,
      #differences foundations antivax vs provax (group comparison)
      d_care_anti_pro = b_care_anti - b_care_pro,
      d_fair_anti_pro = b_fair_anti - b_fair_pro,
      d_loya_anti_pro = b_loya_anti - b_loya_pro,
      d_auth_anti_pro = b_auth_anti - b_auth_pro,
      d_puri_anti_pro = b_puri_anti - b_puri_pro
    ) %>% 
    pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
    group_by(name) %>% 
    median_qi(value) %>% 
    mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
  
  write_rds(m2_rt_results, "../results/full/m2_rt_effects.rds")
  
  # Model comparison ----------------------------------------------------------
  
  # Load results
  m0 <- read_rds("../results/full/m0_fit_rt.rds")
  m1 <- read_rds("../results/full/m1_fit_rt.rds")
  m2 <- read_rds("../results/full/m2_fit_rt.rds")
  
  # Initialize models
  results <- crossing(
    model0 = paste0("M", 0:2),
    model1 = paste0("M", 0:2)
  ) %>% 
    mutate(
      fit0 = map(model0, ~read_rds(paste0("../results/full/", str_to_lower(.), "_fit_rt.rds"))),
      fit1 = map(model1, ~read_rds(paste0("../results/full/", str_to_lower(.), "_fit_rt.rds")))
    )
  
  # Calculate ELPD differences
  results <- results %>% 
    mutate(
      ELPD = map2(
        fit0, 
        fit1, 
        ~loo_compare(.x, .y, criterion = "kfold") %>% 
          as.data.frame() %>%
          rownames_to_column("model")
      )
    ) %>% 
    unnest(ELPD) %>% 
    group_by(model0, model1) %>% 
    dplyr::summarize(
      elpd_diff = elpd_diff[model == ".x"] - elpd_diff[model == ".y"],
      elpd_se_diff = max(se_diff)
    ) %>% 
    ungroup() %>% 
    left_join(
      results %>% 
        mutate(
          R2 = map(
            fit0, 
            ~bayes_R2(., robust = T) %>% as_tibble()
          )
        ) %>% 
        unnest(R2) %>% 
        transmute(
          model0,
          r2 = Estimate,
          r2_l95 = Q2.5,
          r2_u95 = Q97.5
        ) %>% 
        distinct(),
      by = "model0"
    )
  
  #combine comparisons
  results_comparison <- results %>% mutate(z = elpd_diff / elpd_se_diff)
  
  # Save results as .rds
  write_rds(results_comparison, "../results/full/comparison_rt.rds")
