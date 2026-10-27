
#code for analyses:  Associations Between Cannabis Use and Sleep Outcomes in a Sample of Pregnant Women 

# Variable key:
  #cuddel6= data listed where in each woman has three rows (1 per trimester)
  #trimester= T1,T2,T3
  #Cannabis= CUP Stats= positive for participants self-reporting cannabis use or having urine positivity during study visits (1/0)
  #mjheavy= frequency of cannabis use (prior to recoding)
  #whymj= motivation for use during the first trimester (assumed to not change across trimester) either for sleep or other reason
  #COTany= Cotitine Urine test (1/0) per trimester
  #whirs= insomnia severity symptoms
  #essv2= daytime sleepiness
  #irlsv2= restless legs symptom score
  #sleepqual= 1 item PSQI (quality of sleep in last 30 days)

#1) load packages and data
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyr)
library(psych)
library(car)
library(dplyr)
library(ggplot2)


cuddel6 <- read.csv("~/Downloads/XXXX.csv")


options(contrasts = c("contr.poly", "contr.poly"))

cuddel6$trimester <- factor(cuddel6$trimester)
cuddel6$Cannabis  <- factor(cuddel6$Cannabis)
cuddel6$mjheavy   <- factor(cuddel6$mjheavy)
cuddel6$whymj     <- factor(cuddel6$whymj)
cuddel6$COTany    <- factor(cuddel6$COTany)

cuddel6 <- cuddel6 %>%
  
  mutate(
    
    trimester = factor(trimester, levels = c("trimester1", "trimester2", "trimester3"))
    
  )

cuddel6$mjheavy_clean <- cuddel6$mjheavy

#cuddel6$mjheavy_clean[
#  cuddel6$uds_marijuana == 1 &
#   cuddel6$mjheavy == "never"
#] <- NA

cuddel6 <- cuddel6 %>%
  mutate(
    mjheavy_3level = case_when(
      mjheavy == "never" ~ "never",
      
      mjheavy %in% c(
        "monthly",
        "2to4timespermo"
      ) ~ "low",
      
      mjheavy %in% c(
        "2to3timesperwk",
        "4ormoreperwk"
      ) ~ "high",
      
      TRUE ~ NA_character_
    ),
    
    mjheavy_3level = factor(
      mjheavy_3level,
      levels = c(
        "never",
        "low",
        "high"
      ),
      labels = c(
        "Never",
        "Low",
        "High"
      )
    )
  )


#1.5) center age and average income to needs ratio

cuddel6 <- cuddel6 %>%
  mutate(
    income_needs_demo_average_c =
      income_needs_demo - mean(income_needs_demo, na.rm = TRUE),
    
    agec =
      maternal_age_birth - mean(maternal_age_birth, na.rm = TRUE)
  )


# only uncomment when looking at sensitvity analysis for cannabis col where all 1

#cuddel6 <- cuddel6 %>%
#  group_by(record_id) %>%
# mutate(
#  Cannabis_ever = factor(
#   as.integer(any(as.character(Cannabis) == "1", na.rm = TRUE)),
#    levels = c(0, 1)
#  )
# ) %>%
# ungroup()

#2) promary analyses: Insommnia symptoms 
modelwhirs1 <- lmer(
  whirs ~ trimester * Cannabis +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    Cannabis * agec +
    Cannabis * income_needs_demo_average_c +
    Cannabis * COTany +
    (1 | record_id),
  data = cuddel6
)

summary(modelwhirs1)
anova_modelwhirs1 <- anova(modelwhirs1, type = 2)

#2) 2.5 pairwise contrasts
emmeans(modelwhirs1, pairwise ~ trimester, adjust = "none")
emmeans(modelwhirs1, pairwise ~ trimester | Cannabis, adjust = "none")
emmeans(modelwhirs1, pairwise ~ Cannabis | trimester, adjust = "none")


#3) primary analyses: daytime sleepiness 
modeless1 <- lmer(
  essv2 ~ trimester * Cannabis +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    Cannabis * agec +
    Cannabis * income_needs_demo_average_c +
    Cannabis * COTany +
    (1 | record_id),
  data = cuddel6
)

summary(modeless1)
anova(modeless1, type = 2)

#3.5) pairwise contrasts
emmeans(modeless1, pairwise ~ trimester, adjust = "none")
emmeans(modeless1, pairwise ~ trimester | Cannabis, adjust = "none")
emmeans(modeless1, pairwise ~ Cannabis | trimester, adjust = "none")

#4) primary anlayses: restless legs
modelirls1 <- lmer(
  irlsv2 ~ trimester * Cannabis +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    Cannabis * agec +
    Cannabis * income_needs_demo_average_c +
    Cannabis * COTany +
    (1 | record_id),
  data = cuddel6
)

summary(modelirls1)
anova(modelirls1, type = 2)

#5) primary analyses sleep quality
modelsleepqual1 <- lmer(
  sleepqual ~ trimester * Cannabis +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    Cannabis * agec +
    Cannabis * income_needs_demo_average_c +
    Cannabis * COTany +
    (1 | record_id),
  data = cuddel6
)

summary(modelsleepqual1)
anova(modelsleepqual1, type = 2)
#5.5) primary analyses: sleep quality

#6) prairwise contrasts
emmeans(modelsleepqual1, pairwise ~ trimester, adjust = "none")
#emmeans(modelsleepqual1, pairwise ~ trimester | Cannabis, adjust = "none")
#emmeans(modelsleepqual1, pairwise ~ Cannabis | trimester, adjust = "none")


emm_trimester_sleepqual <- emmeans(
  modelsleepqual1,
  ~ trimester
)



#7) cannabis frequency: insommnia symptoms 
modelwhirs2 <- lmer(
  whirs ~ trimester * mjheavy_3level +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    mjheavy_3level * agec +
    mjheavy_3level * income_needs_demo_average_c +
    mjheavy_3level * COTany +
    (1 | record_id),
  data = subset(
    cuddel6,
    !(uds_marijuana == 1 & cannabis_self_report_factor == 0)
  )
)

summary(modelwhirs2)
anova(modelwhirs2, type = 2)

anova_modelwhirs2 <- anova(modelwhirs2, type = 2)



#8) cannabis frequency: daytime sleepiness
modeless2 <- lmer(
  essv2 ~ trimester * mjheavy_3level +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    mjheavy_3level * agec +
    mjheavy_3level * income_needs_demo_average_c +
    mjheavy_3level * COTany +
    (1 | record_id),
  data = subset(
    cuddel6,
    !(uds_marijuana == 1 & cannabis_self_report_factor == 0)
  )
)

summary(modeless2)

anova_modeless2<- anova(modeless2, type = 2)

#9) cannabis frequency: restless legs symptoms
modelirls2 <- lmer(
  irlsv2 ~ trimester * mjheavy_3level +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    mjheavy_3level * agec +
    mjheavy_3level * income_needs_demo_average_c +
    mjheavy_3level * COTany +
    (1 | record_id),
  data = subset(
    cuddel6,
    !(uds_marijuana == 1 & cannabis_self_report_factor == 0)
  )
)

summary(modelirls2)
anova(modelirls2, type = 2)

#10) cannabis frequency: restless legs symptoms 
anova_modelirls2<- anova(modelirls2, type = 2)



#11) cannabis frequency: sleep quality 
modelsleepqual2 <- lmer(
  sleepqual ~ trimester * mjheavy_3level +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    mjheavy_3level * agec +
    mjheavy_3level * income_needs_demo_average_c +
    mjheavy_3level * COTany +
    (1 | record_id),
  data = subset(
    cuddel6,
    !(uds_marijuana == 1 & cannabis_self_report_factor == 0)
  )
)

summary(modelsleepqual2)
anova(modelsleepqual2, type = 2)


#12) cannabis motivation: insommnia symptoms 
modelwhirs3 <- lmer(
  whirs ~ trimester * whymj +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    whymj * agec +
    whymj * income_needs_demo_average_c +
    whymj * COTany +
    (1 | record_id),
  data = subset(cuddel6, Cannabis == 1)
)

summary(modelwhirs3)
anova(modelwhirs3, type = 2)

emmeans(modelwhirs3, pairwise ~ whymj, adjust = "none")


#13) cannabis motivation: daytime sleepiness
modeless3 <- lmer(
  essv2 ~ trimester * whymj +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    whymj * agec +
    whymj * income_needs_demo_average_c +
    whymj * COTany +
    (1 | record_id),
  data = subset(cuddel6, Cannabis == 1)
)

summary(modeless3)
anova(modeless3, type = 2)

#13.5 pairwise contrasts
emmeans(modeless3, pairwise ~ whymj, adjust = "none")

#14) cannabis motivation: restless legs symptoms
modelirls3 <- lmer(
  irlsv2 ~ trimester * whymj +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    whymj * agec +
    whymj * income_needs_demo_average_c +
    whymj * COTany +
    (1 | record_id),
  data = subset(cuddel6, Cannabis == 1)
)

summary(modelirls3)
anova(modelirls3, type = 2)

emmeans(modelirls3, pairwise ~ whymj, adjust = "none")


#15 cannabis motivation: sleep quality 
modelsleepqual3 <- lmer(
  sleepqual ~ trimester * whymj +
    agec + income_needs_demo_average_c + COTany +
    trimester * agec +
    trimester * income_needs_demo_average_c +
    trimester * COTany +
    whymj * agec +
    whymj * income_needs_demo_average_c +
    whymj * COTany +
    (1 | record_id),
  data = subset(cuddel6, Cannabis == 1)
)

summary(modelsleepqual3)
anova(modelsleepqual3, type = 2)

emmeans(modelsleepqual3, pairwise ~ whymj, adjust = "none")


# 16) FDR TESTING CODE+ step by step table creation 

pull_P_values <- function(model, term) {
  
  a <- as.data.frame(anova(model, type = 2))
  
  p_col <- grep("^Pr\\(", colnames(a), value = TRUE)
  
  if (length(p_col) == 0) {
    stop("No p")
  }
  
  if (!term %in% rownames(a)) {
    stop(
      sprintf(
        "NF",
        term,
        paste(rownames(a), collapse = ", ")
      )
    )
  }
  
  as.numeric(a[term, p_col[1]])
}


# For each relevant term in each group of models- FDR-correct across outcomes.Rows = outcomes Columns = model terms.

make_p_val_rowsfor_table <- function(models, terms) {
  
  raw_p <- sapply(terms, function(term) {
    sapply(models, pull_P_values, term = term)
  })
  
  raw_p <- as.data.frame(raw_p)
  
  rownames(raw_p) <- names(models)
  colnames(raw_p) <- terms
  
  raw_p
}


make_fdr_table <- function(p_val_rowsfor_table) {
  
  p_value_fdr_corrected <- apply(p_val_rowsfor_table, 2, p.adjust, method = "fdr")
  p_value_fdr_corrected <- as.data.frame(p_value_fdr_corrected)
  
  rownames(p_value_fdr_corrected) <- rownames(p_val_rowsfor_table)
  colnames(p_value_fdr_corrected) <- colnames(p_val_rowsfor_table)
  
  p_value_fdr_corrected
}


pull_fdr <- function(fdr_table, outcome) {
  
  setNames(
    as.numeric(fdr_table[outcome, ]),
    colnames(fdr_table)
  )
}


#primary analyses FDR TABLE

model1_models <- list(
  IRLS      = modelirls1,
  WHIRS     = modelwhirs1,
  ESS       = modeless1,
  SLEEPQUAL = modelsleepqual1
)

model1_terms <- c(
  "trimester",
  "Cannabis",
  "agec",
  "income_needs_demo_average_c",
  "COTany",
  "trimester:Cannabis",
  "trimester:agec",
  "trimester:income_needs_demo_average_c",
  "trimester:COTany",
  "Cannabis:agec",
  "Cannabis:income_needs_demo_average_c",
  "Cannabis:COTany"
)

model1_raw_p <- make_p_val_rowsfor_table(
  models = model1_models,
  terms = model1_terms
)

model1_fdr <- make_fdr_table(model1_raw_p)

IRLSpfdr      <- pull_fdr(model1_fdr, "IRLS")
whirspfdr     <- pull_fdr(model1_fdr, "WHIRS")
esspfdr       <- pull_fdr(model1_fdr, "ESS")
sleepqualpfdr <- pull_fdr(model1_fdr, "SLEEPQUAL")


IRLSpfdr
whirspfdr
esspfdr
sleepqualpfdr


#CANNABIS FREQUENCY MODELS 

model2_models <- list(
  IRLS      = modelirls2,
  WHIRS     = modelwhirs2,
  ESS       = modeless2,
  SLEEPQUAL = modelsleepqual2
)

model2_terms <- c(
  "trimester",
  "mjheavy_3level",
  "agec",
  "income_needs_demo_average_c",
  "COTany",
  "trimester:mjheavy_3level",
  "trimester:agec",
  "trimester:income_needs_demo_average_c",
  "trimester:COTany",
  "mjheavy_3level:agec",
  "mjheavy_3level:income_needs_demo_average_c",
  "mjheavy_3level:COTany"
)
model2_raw_p <- make_p_val_rowsfor_table(
  models = model2_models,
  terms = model2_terms
)

model2_fdr <- make_fdr_table(model2_raw_p)

IRLSpfdrv2      <- pull_fdr(model2_fdr, "IRLS")
whirspfdrv2     <- pull_fdr(model2_fdr, "WHIRS")
esspfdrv2       <- pull_fdr(model2_fdr, "ESS")
sleepqualpfdrv2 <- pull_fdr(model2_fdr, "SLEEPQUAL")


IRLSpfdrv2
whirspfdrv2
esspfdrv2
sleepqualpfdrv2


#CANNABIS MOTIVATIONS 

model3_models <- list(
  IRLS      = modelirls3,
  WHIRS     = modelwhirs3,
  ESS       = modeless3,
  SLEEPQUAL = modelsleepqual3
)

model3_terms <- c(
  "trimester",
  "whymj",
  "agec",
  "income_needs_demo_average_c",
  "COTany",
  "trimester:whymj",
  "trimester:agec",
  "trimester:income_needs_demo_average_c",
  "trimester:COTany",
  "whymj:agec",
  "whymj:income_needs_demo_average_c",
  "whymj:COTany"
)

model3_raw_p <- make_p_val_rowsfor_table(
  models = model3_models,
  terms = model3_terms
)

model3_fdr <- make_fdr_table(model3_raw_p)

IRLSpfdrv3        <- pull_fdr(model3_fdr, "IRLS")
whirspfdrv3     <- pull_fdr(model3_fdr, "WHIRS")
esspfdrv3          <- pull_fdr(model3_fdr, "ESS")
sleepqualpfdrv3 <- pull_fdr(model3_fdr, "SLEEPQUAL")


IRLSpfdrv3
whirspfdrv3
esspfdrv3
sleepqualpfdrv3


