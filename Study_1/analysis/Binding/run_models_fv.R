rm(list = ls())

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
  df_anti_binding <- read_csv("antivax_binding.csv") %>% 
    select(-c(full_text)) 
  df_pro_binding <- read_csv("provax_binding.csv") %>% 
    select(-c(full_text))

  # Prepare data
  df_total_binding <- rbind(df_anti_binding, df_pro_binding) %>%
    mutate(
      binding = factor(binding),
      individual = factor(individual),
      stance = factor(stance_label, labels = c("anti-vax", "pro-vax"))
    )


# Estimate ----------------------------------------------------------------

  # Model 0: Only random intercepts on the user level
  m0_fit <- brm(
    favorite_count ~ stance + (1|user_id), 
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )

  # Model 1: Add moral framings
  m1_fit <- brm(
    favorite_count ~ binding + individual + stance + (1|user_id),    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  # Model 2: Add interaction
  m2_fit <- brm(
    favorite_count ~ (binding + individual)*stance + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  # Save results as .rds
  write_rds(m0_fit, "results/m0_fit_fv.rds")
  write_rds(m1_fit, "results/m1_fit_fv.rds")
  write_rds(m2_fit, "results/m2_fit_fv.rds")
  

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
  write_rds(m0_fit, "results/binding/m0_fit_fv.rds")
  write_rds(m1_fit, "results/binding/m1_fit_fv.rds")
  write_rds(m2_fit, "results/binding/m2_fit_fv.rds")
  
# Main Results ------------------------------------------------------------
######## Extract coefficients
  m2_fv_results <- m2_fit %>% 
    spread_draws(
      `b_binding1`,
      `b_individual1`,
      `b_individual1:stanceproMvax`,
      `b_binding1:stanceproMvax`,
    ) %>% 
    transmute(
      .chain, .iteration, .draw,
      b_indi_anti = `b_individual1` + 0 * `b_individual1:stanceproMvax` + 0 * `b_binding1:stanceproMvax`,
      b_indi_pro = `b_individual1` + 1 * `b_individual1:stanceproMvax` + 0 * `b_binding1:stanceproMvax`,
      b_bind_anti = `b_binding1` + 0 * `b_individual1:stanceproMvax` + 0 * `b_binding1:stanceproMvax`,
      b_bind_pro = `b_binding1` + 0 * `b_individual1:stanceproMvax` + 1 * `b_binding1:stanceproMvax`,
      d_anti_indi_bind = b_indi_anti - b_bind_anti,
      d_pro_indi_bind = b_indi_pro - b_bind_pro,
      d_bind_anti_pro = b_bind_anti - b_bind_pro,
      d_indi_pro_anti = b_indi_pro - b_indi_anti
    ) %>% 
    pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
    group_by(name) %>% 
    median_qi(value) %>% 
    mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
  
  write_rds(m2_fv_results, "../results/binding/m2_fv_effects.rds")
  
# Model comparison ----------------------------------------------------------

  # Load results
  m0 <- read_rds("../results/binding/m0_fit_fv.rds")
  m1 <- read_rds("../results/binding/m1_fit_fv.rds")
  m2 <- read_rds("../results/binding/m2_fit_fv.rds")
  
# Initialize models
  results <- crossing(
    model0 = paste0("M", 0:2),
    model1 = paste0("M", 0:2)
  ) %>% 
    mutate(
      fit0 = map(model0, ~read_rds(paste0("../results/binding/", str_to_lower(.), "_fit_fv.rds"))),
      fit1 = map(model1, ~read_rds(paste0("../results/binding/", str_to_lower(.), "_fit_fv.rds")))
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
  write_rds(results_comparison, "../results/binding/comparison_fv_binding.rds")
  


