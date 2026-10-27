#  sensitivity analyses
#
# sensitivity analyses include the following:
# 1. Primary mixed-effects model
# 2. Complete third-trimester data sensitivity analysis
# 3. Adjustment for missing third-trimester CUP status
# 4. Adjustment for missing third-trimester insomnia symptoms
# 5. Adjustment for both missingness indicators

#Additionally, the complete-T3 analysis excludes the entire longitudinal record of individuals missing either: T3 CUP status, or
 #T3 insomnia symptoms

library(dplyr)
library(lmerTest)
library(emmeans)
library(tibble)


# confirm variables 
var_needed <- c(
  "record_id",
  "trimester",
  "Cannabis",
  "whirs",
  "agec",
  "income_needs_demo_average_c",
  "COTany"
)

miss_var <- setdiff(
  var_needed,
  names(cuddel6)
)

if (length(miss_var) > 0) {
  stop(
    paste(
      "NF",
      paste(miss_var, collapse = ", ")
    )
  )
}


#organize data

analysis_data <- cuddel6 %>%
  mutate(
    record_id = factor(record_id),
    
    trimester = factor(
      trimester,
      levels = c(
        "trimester1",
        "trimester2",
        "trimester3"
      )
    ),
    
    Cannabis = factor(Cannabis)
  )


# Create T3 missingness indicators per particpant 
#

t3_missingness <- analysis_data %>%
  group_by(record_id) %>%
  summarise(
    t3_cup_observed = any(
      trimester == "trimester3" &
        !is.na(Cannabis),
      na.rm = TRUE
    ),
    
    t3_whiirs_observed = any(
      trimester == "trimester3" &
        !is.na(whirs),
      na.rm = TRUE
    ),
    
    .groups = "d"
  ) %>%
  mutate(
    missing_t3_cup = as.integer(
      !t3_cup_observed
    ),
    
    missing_t3_whiirs = as.integer(
      !t3_whiirs_observed
    ),
    
    complete_t3 = (
      t3_cup_observed &
        t3_whiirs_observed
    )
  )


# Linked to dataset

analysis_data <- analysis_data %>%
  left_join(
    t3_missingness,
    by = "record_id"
  )

# analyses
main_model <- whirs ~
  trimester * Cannabis +
  agec +
  income_needs_demo_average_c +
  COTany +
  trimester * agec +
  trimester * income_needs_demo_average_c +
  trimester * COTany +
  Cannabis * agec +
  Cannabis * income_needs_demo_average_c +
  Cannabis * COTany +
  (1 | record_id)



main_model_wiirs <- lmer(
  formula = main_model,
  data = analysis_data,
  REML = TRUE,
  na.action = na.omit
)



anova_primary <- anova(
  main_model_wiirs,
  type = 2,
  ddf = "Satterthwaite"
)


#creation of complete third trimester dataset- removing all particpants who are missing either T3 CUP satus or T3 insommnia

analysis_data_complete_t3 <- analysis_data %>%
  filter(complete_t3) %>%
  droplevels()


cat("set")

print(
  analysis_data_complete_t3 %>%
    summarise(
      n_participants = n_distinct(record_id),
      n_longitudinal_rows = n()
    )
)



# sensitivity analyses
modelwhirs_complete_t3 <- lmer(
  formula = main_model,
  data = analysis_data_complete_t3,
  REML = TRUE,
  na.action = na.omit
)


anova_complete_t3 <- anova(
  modelwhirs_complete_t3,
  type = 2,
  ddf = "Satterthwaite"
)



# sensitivty analyses with missing T3 CUP Status data

modelwhirs_missing_cup <- lmer(
  whirs ~
    trimester * Cannabis +
    missing_t3_cup +
    agec +
    income_needs_demo_average_c +
    COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    Cannabis * agec +
    Cannabis * income_needs_demo_average_c +
    Cannabis * COTany +
    (1 | record_id),
  data = analysis_data,
  REML = TRUE,
  na.action = na.omit
)


anova_missing_cup <- anova(
  modelwhirs_missing_cup,
  type = 2,
  ddf = "Satterthwaite"
)



# sensitivty analysis missing T3 Whiirs
modelwhirs_missing_whiirs <- lmer(
  whirs ~
    trimester * Cannabis +
    missing_t3_whiirs +
    agec +
    income_needs_demo_average_c +
    COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    Cannabis * agec +
    Cannabis * income_needs_demo_average_c +
    Cannabis * COTany +
    (1 | record_id),
  data = analysis_data,
  REML = TRUE,
  na.action = na.omit
)


anova_missing_whiirs <- anova(
  modelwhirs_missing_whiirs,
  type = 2,
  ddf = "Satterthwaite"
)


#adjusting for T3 CUP Status and WHIIRS
modelwhirs_missing_both <- lmer(
  whirs ~
    trimester * Cannabis +
    missing_t3_cup +
    missing_t3_whiirs +
    agec +
    income_needs_demo_average_c +
    COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    Cannabis * agec +
    Cannabis * income_needs_demo_average_c +
    Cannabis * COTany +
    (1 | record_id),
  data = analysis_data,
  REML = TRUE,
  na.action = na.omit
)



anova_missing_both <- anova(
  modelwhirs_missing_both,
  type = 2,
  ddf = "Satterthwaite"
)


#POST HOC COMPARISONS FOR ALL
run_posthoc_tests <- function(
    model,
    adjustment = "none"
) {
  
  # within cup status group
  emm_trimester_within_cup <- emmeans(
    model,
    ~ trimester | Cannabis
  )
  
  trimester_within_cup <- pairs(
    emm_trimester_within_cup,
    adjust = adjustment
  )
  
  
  # within trimester 
  
  emm_cup_within_trimester <- emmeans(
    model,
    ~ Cannabis | trimester
  )
  
  cup_within_trimester <- pairs(
    emm_cup_within_trimester,
    adjust = adjustment
  )
  
  
#trimester by cup status 
  
  emm_cells <- emmeans(
    model,
    ~ trimester * Cannabis
  )
  

posthoc_primary <- run_posthoc_tests(
  main_model_wiirs,
  adjustment = "none"
)

posthoc_complete_t3 <- run_posthoc_tests(
  modelwhirs_complete_t3,
  adjustment = "none"
)

posthoc_missing_cup <- run_posthoc_tests(
  modelwhirs_missing_cup,
  adjustment = "none"
)

posthoc_missing_whiirs <- run_posthoc_tests(
  modelwhirs_missing_whiirs,
  adjustment = "none"
)

posthoc_missing_both <- run_posthoc_tests(
  modelwhirs_missing_both,
  adjustment = "none"
)


extract_interaction_test <- function(
    model,
    model_name
) {
  
  test_table <- as.data.frame(
    anova(
      model,
      type = 2,
      ddf = "Satterthwaite"
    )
  ) %>%
    rownames_to_column("term")
  
  interaction_row <- test_table %>%
    filter(
      term %in% c(
        "trimester:Cannabis",
        "Cannabis:trimester"
      )
    )
  
  if (nrow(interaction_row) == 0) {
    warning(
      paste(
        "The trimester-by-Cannabis interaction was not found for",
        model_name
      )
    )
    
    return(
      tibble(
        model = model_name,
        numerator_df = NA_real_,
        denominator_df = NA_real_,
        F_value = NA_real_,
        p_value = NA_real_
      )
    )
  }
  
  tibble(
    model = model_name,
    numerator_df = interaction_row$NumDF,
    denominator_df = interaction_row$DenDF,
    F_value = interaction_row$`F value`,
    p_value = interaction_row$`Pr(>F)`
  )
}


# comparisons across models 
interaction_sensitivity_summary <- bind_rows(
  extract_interaction_test(
    main_model_wiirs,
    "Primary model"
  ),
  
  extract_interaction_test(
    modelwhirs_complete_t3,
    "Complete T3 data"
  ),
  
  extract_interaction_test(
    modelwhirs_missing_cup,
    "Adjusted for missing T3 CUP"
  ),
  
  extract_interaction_test(
    modelwhirs_missing_whiirs,
    "Adjusted for missing T3 WHIIRS"
  ),
  
  extract_interaction_test(
    modelwhirs_missing_both,
    "Adjusted for both missingness indicators"
  )
)




extract_model_sample <- function(
    model,
    model_name
) {
  
  model_data <- model.frame(model)
  
  tibble(
    model = model_name,
    n_observations = nrow(model_data),
    n_participants = n_distinct(
      model_data$record_id
    )
  )
}


model_sample_summary <- bind_rows(
  extract_model_sample(
    main_model_wiirs,
    "primary"
  ),
  
  extract_model_sample(
    modelwhirs_complete_t3,
    "compmlete T3"
  ),
  
  extract_model_sample(
    modelwhirs_missing_cup,
    "adjust M T3 CUP"
  ),
  
  extract_model_sample(
    modelwhirs_missing_whiirs,
    "adjust M T3 status"
  ),
  
  extract_model_sample(
    modelwhirs_missing_both,
    "Adjusted for both missingness indicators"
  )
)

