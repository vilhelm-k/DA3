required_packages <- c(
    "rio", "ggplot2", "vtable",
    "corrplot", "regclass", "olsrr", "mediation", "multilevel", "stargazer", "ggplot2"
)

for (i in 1:length(required_packages)) {
    if (!required_packages[i] %in% installed.packages()) {
        install.packages(required_packages[i])
    }
}
df <- rio::import("BE603_gr_10.csv")
df$fundraising_ratio <- df$collected_funds / df$goal

# Hypothesis 3: early success is key
model_1 <- lm(collected_funds ~ goal, data = df)
print(summary(model_1))

h3_plot <- ggplot(df) +
    aes(y = collected_funds, x = goal) +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", formula = "y ~ x", se = TRUE) +
    labs(
        y = "Collected funds",
        x = "Goal",
        title = "Correlation between goal and collected funds"
    ) +
    theme_classic()

ggsave("H3_fundraisingratio_reachin30.png", width = 8, height = 6)
