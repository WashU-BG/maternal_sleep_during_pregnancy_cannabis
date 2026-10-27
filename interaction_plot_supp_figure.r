

library(lme4)
library(emmeans)
library(dplyr)
library(ggplot2)
#RLS



## 1. Fit RLS model
modelirls1 <- lmer(
  irlsv2 ~ trimester*Cannabis +
    agec + income_needs_demo_average_c + COTany +
    trimester*agec +
    trimester*income_needs_demo_average_c +
    trimester*COTany +
    Cannabis*agec +
    Cannabis*income_needs_demo_average_c +
    Cannabis*COTany +
    (1 | record_id),
  data = cuddel6
)

## 2. Estimated marginal means
emm_rls <- emmeans(modelirls1, ~ trimester * Cannabis)
emm_df <- as.data.frame(emm_rls)

emm_df <- emm_df %>%
  mutate(
    trimester_f = factor(
      trimester,
      levels = c("trimester1", "trimester2", "trimester3"),
      labels = c("1", "2", "3")
    ),
    Cannabis_lab = factor(
      Cannabis,
      levels = c(0, 1),
      labels = c("No cannabis", "Cannabis")
    )
  )


n_df <- cuddel6 %>%
  filter(
    !is.na(irlsv2),
    !is.na(Cannabis),
    !is.na(agec),
    !is.na(income_needs_demo_average_c),
    !is.na(COTany)
  ) %>%
  mutate(
    trimester_f = factor(
      trimester,
      levels = c("trimester1", "trimester2", "trimester3"),
      labels = c("1", "2", "3")
    ),
    Cannabis_lab = factor(
      Cannabis,
      levels = c(0, 1),
      labels = c("No cannabis", "Cannabis")
    )
  ) %>%
  group_by(trimester_f, Cannabis_lab) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    y_n = case_when(
      Cannabis_lab == "No cannabis" ~ 0.18,
      Cannabis_lab == "Cannabis" ~ 0.08
    )
  )

## figure designs
pd <- position_dodge(width = 0.45)

cb_palette <- c(
  "No cannabis" = "#0072B2",
  "Cannabis" = "#D55E00"
)

shape_vals <- c(
  "No cannabis" = 16,
  "Cannabis" = 17
)

p_rls_emm <- ggplot(
  emm_df,
  aes(
    x = trimester_f,
    y = emmean,
    color = Cannabis_lab,
    shape = Cannabis_lab
  )
) +
  geom_point(
    position = pd,
    size = 3.2
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = pd,
    width = 0.12,
    linewidth = 0.7
  ) +
  geom_text(
    data = n_df,
    aes(
      x = trimester_f,
      y = y_n,
      label = paste0("n = ", n),
      group = Cannabis_lab
    ),
    position = pd,
    color = "black",
    size = 3.0,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cb_palette) +
  scale_shape_manual(values = shape_vals) +
  scale_y_continuous(
    name = "Restless Legs Symptoms",
    limits = c(0, 1.2),
    breaks = seq(0, 1.2, by = 0.2),
    expand = c(0, 0)
  ) +
  labs(
    x = "Trimester",
    color = "CUP Status",
    shape = "CUP Status"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 15,
      l = 15
    )
  )

p_rls_emm

## 6. Save high-resolution TIFF
ggsave(
  filename = "~/Downloads/Figure_RLS_EMMeans.tiff",
  plot = p_rls_emm,
  width = 6.5,
  height = 4.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)


#WHIRS

library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(ggplot2)

modelwhirs1 <- lmer(
  whirs ~ trimester*Cannabis +
    agec + income_needs_demo_average_c + COTany +
    trimester*agec +
    trimester*income_needs_demo_average_c +
    trimester*COTany +
    Cannabis*agec +
    Cannabis*income_needs_demo_average_c +
    Cannabis*COTany +
    (1 | record_id),
  data = cuddel6
)

emm_whirs <- emmeans(modelwhirs1, ~ trimester * Cannabis)
emm_df <- as.data.frame(emm_whirs)

emm_df <- emm_df %>%
  mutate(
    trimester_f = factor(
      trimester,
      levels = c("trimester1", "trimester2", "trimester3"),
      labels = c("1", "2", "3")
    ),
    Cannabis_lab = factor(
      Cannabis,
      levels = c(0, 1),
      labels = c("No cannabis", "Cannabis")
    )
  )


n_df <- cuddel6 %>%
  filter(
    !is.na(whirs),
    !is.na(Cannabis),
    !is.na(agec),
    !is.na(income_needs_demo_average_c),
    !is.na(COTany)
  ) %>%
  mutate(
    trimester_f = factor(
      trimester,
      levels = c("trimester1", "trimester2", "trimester3"),
      labels = c("1", "2", "3")
    ),
    Cannabis_lab = factor(
      Cannabis,
      levels = c(0, 1),
      labels = c("No cannabis", "Cannabis")
    )
  ) %>%
  group_by(trimester_f, Cannabis_lab) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    y_n = case_when(
      Cannabis_lab == "No cannabis" ~ 1.5,
      Cannabis_lab == "Cannabis" ~ 1.1
    )
  )

pd <- position_dodge(width = 0.45)

cb_palette <- c(
  "No cannabis" = "#0072B2",
  "Cannabis" = "#D55E00"
)

shape_vals <- c(
  "No cannabis" = 16,
  "Cannabis" = 17
)

## 5. Figure
p_whirs_emm <- ggplot(
  emm_df,
  aes(
    x = trimester_f,
    y = emmean,
    color = Cannabis_lab,
    shape = Cannabis_lab
  )
) +
  geom_point(
    position = pd,
    size = 3.2
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = pd,
    width = 0.12,
    linewidth = 0.7
  ) +
  geom_text(
    data = n_df,
    aes(
      x = trimester_f,
      y = y_n,
      label = paste0("n = ", n),
      group = Cannabis_lab
    ),
    position = pd,
    color = "black",
    size = 3.0,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cb_palette) +
  scale_shape_manual(values = shape_vals) +
  scale_y_continuous(
    name = "Insomnia Symptoms",
    limits = c(0, 12),
    breaks = seq(0, 12, by = 2),
    expand = c(0, 0)
  ) +
  labs(
    x = "Trimester",
    color = "CUP Status",
    shape = "CUP Status"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 15,
      l = 15
    )
  )

p_whirs_emm

ggsave(
  filename = "~/Downloads/Figure_WHIRS_EMMeans.tiff",
  plot = p_whirs_emm,
  width = 6.5,
  height = 4.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)
