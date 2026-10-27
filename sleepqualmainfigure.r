library(emmeans)
library(dplyr)
library(ggplot2)

# estimated marginal means by trimester
emm_sleepqual <- emmeans(
  modelsleepqual1,
  ~ trimester
)

# C=convert estimated marginal means to correct data typeframe
emm_df <- as.data.frame(emm_sleepqual) %>%
  mutate(
    trimester_lab = factor(
      trimester,
      levels = c(
        "trimester1",
        "trimester2",
        "trimester3"
      ),
      labels = c(
        "Trimester 1",
        "Trimester 2",
        "Trimester 3"
      )
    )
  )

# extract the model data
model_df <- as.data.frame(
  model.frame(modelsleepqual1)
)

#  find sample size for each trimester
n_df <- model_df %>%
  group_by(trimester) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    trimester_lab = factor(
      trimester,
      levels = c(
        "trimester1",
        "trimester2",
        "trimester3"
      ),
      labels = c(
        "Trimester 1",
        "Trimester 2",
        "Trimester 3"
      )
    )
  )

# y axis ranges
y_min <- min(
  emm_df$lower.CL,
  na.rm = TRUE
)

y_max <- max(
  emm_df$upper.CL,
  na.rm = TRUE
)

y_range <- y_max - y_min

# style choices
n_df <- n_df %>%
  mutate(
    y_n = y_min - y_range * 0.45
  )

plot_y_min <- min(
  n_df$y_n,
  na.rm = TRUE
) - y_range * 0.20

plot_y_max <- y_max + y_range * 0.30

#gererate figure
p_sleepqual_trimester <- ggplot(
  emm_df,
  aes(
    x = trimester_lab,
    y = emmean,
    group = 1
  )
) +
  geom_line(
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(
    size = 3,
    color = "black"
  ) +
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    width = 0.10,
    linewidth = 0.55,
    color = "black"
  ) +
  geom_text(
    data = n_df,
    aes(
      x = trimester_lab,
      y = y_n,
      label = paste0("n=", n)
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  scale_y_continuous(
    name = paste0(
      "Estimated Mean Sleep Quality Score\n",
      "(Higher = worse sleep quality)"
    ),
    limits = c(
      plot_y_min,
      plot_y_max
    ),
    expand = c(0, 0)
  ) +
  labs(
    x = "Trimester"
  ) +
  theme_classic(
    base_size = 12
  ) +
  theme(
    legend.position = "none",
    
    axis.title.x = element_text(
      face = "bold"
    ),
    
    axis.title.y = element_text(
      size = 12,
      face = "plain",
      lineheight = 0.85
    ),
    
    axis.text.x = element_text(
      color = "black",
      size = 11
    ),
    
    axis.text.y = element_text(
      color = "black"
    ),
    
    plot.title = element_blank()
  )

p_sleepqual_trimester

# Save the figure
ggsave(
  filename = "~/Downloads/Figure_SleepQual_trimester.tiff",
  plot = p_sleepqual_trimester,
  width = 5.5,
  height = 4.8,
  units = "in",
  dpi = 600,
  compression = "lzw"
)