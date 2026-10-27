library(ggplot2)
library(patchwork)

whymj_means <- read.csv(
  "downloads/motivation_means_nov2025.csv"
)

whymj_means$whymjF <- factor(
  whymj_means$whymj,
  levels = c("other", "sleep")
)

IRLSwhy <- ggplot(
  data = whymj_means[whymj_means$output == "IRLS", ],
  aes(
    x = whymjF,
    y = mean,
    group = whymjF,
    color = whymjF
  )
) +
  geom_errorbar(
    aes(
      ymin = L_CI,
      ymax = U_CI
    ),
    width = 0.2,
    position = position_dodge(width = 0.25)
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.25)
  ) +
  scale_color_manual(
    values = c(
      "other" = "#009e73",
      "sleep" = "#56B4E9"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "other" = "Other",
      "sleep" = "Sleep"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0.5
    ),
    axis.title.x = element_text(
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 12
    ),
    axis.text.y = element_text(
      size = 12
    )
  ) +
  labs(
    x = "Motivation for marijuana use",
    y = "Restless Legs Symptoms"
  )

ESSwhy <- ggplot(
  data = whymj_means[whymj_means$output == "ESS", ],
  aes(
    x = whymjF,
    y = mean,
    group = whymjF,
    color = whymjF
  )
) +
  geom_errorbar(
    aes(
      ymin = L_CI,
      ymax = U_CI
    ),
    width = 0.2,
    position = position_dodge(width = 0.25)
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.25)
  ) +
  scale_color_manual(
    values = c(
      "other" = "#009e73",
      "sleep" = "#56B4E9"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "other" = "Other",
      "sleep" = "Sleep"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0.5
    ),
    axis.title.x = element_text(
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 12
    ),
    axis.text.y = element_text(
      size = 12
    )
  ) +
  labs(
    x = "Motivation for marijuana use",
    y = "Daytime Sleepiness"
  )

WHIIRSwhy <- ggplot(
  data = whymj_means[whymj_means$output == "WHIIRS", ],
  aes(
    x = whymjF,
    y = mean,
    group = whymjF,
    color = whymjF
  )
) +
  geom_errorbar(
    aes(
      ymin = L_CI,
      ymax = U_CI
    ),
    width = 0.2,
    position = position_dodge(width = 0.25)
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.25)
  ) +
  scale_color_manual(
    values = c(
      "other" = "#009e73",
      "sleep" = "#56B4E9"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "other" = "Other",
      "sleep" = "Sleep"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0.5
    ),
    axis.title.x = element_text(
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 12
    ),
    axis.text.y = element_text(
      size = 12
    )
  ) +
  labs(
    x = "Motivation for marijuana use",
    y = "Insomnia Symptoms"
  )

SLEEPQUALwhy <- ggplot(
  data = whymj_means[whymj_means$output == "SLEEPQUAL", ],
  aes(
    x = whymjF,
    y = mean,
    group = whymjF,
    color = whymjF
  )
) +
  geom_errorbar(
    aes(
      ymin = L_CI,
      ymax = U_CI
    ),
    width = 0.2,
    position = position_dodge(width = 0.25)
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.25)
  ) +
  scale_color_manual(
    values = c(
      "other" = "#009e73",
      "sleep" = "#56B4E9"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "other" = "Other",
      "sleep" = "Sleep"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0.5
    ),
    axis.title.x = element_text(
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      size = 12,
      face = "plain",
      lineheight = 0.75,
      margin = margin(r = 2)
    ),
    axis.text.x = element_text(
      size = 12
    ),
    axis.text.y = element_text(
      size = 12
    )
  ) +
  labs(
    x = "Motivation for marijuana use",
    y = expression(
      atop(
        bold("Sleep Quality"),
        plain("(Higher = worse sleep quality)")
      )
    )
  )

combined_plot <- (
  WHIIRSwhy +
    free(
      SLEEPQUALwhy,
      type = "space",
      side = "l"
    ) +
    IRLSwhy +
    ESSwhy
) +
  plot_layout(
    nrow = 2,
    ncol = 2
  )

combined_plot

ggsave(
  filename = "downloads/_motivation_.tiff",
  plot = combined_plot,
  dpi = 600,
  width = 10,
  height = 8,
  units = "in",
  compression = "lzw"
)