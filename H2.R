required_packages <- c(
    "rio", "ggplot2", "vtable",
    "corrplot", "regclass", "olsrr", "mediation", "multilevel", "stargazer", "ggplot2"
)

for (i in 1:length(required_packages)) {
    if (!required_packages[i] %in% installed.packages()) {
        install.packages(required_packages[i])
    }
}

# Load required libraries
library(ggplot2)
library(rio)

df <- rio::import("BE603_gr_10.csv")
df$reach30in2 <- as.numeric(df$reach30in2)

# Hypothesis 2:
model_1 <- lm(collected_funds ~ social * negemo, data = df)
print(summary(model_1))


# Convert social to factor for discrete grouping
df$social_factor <- as.factor(df$social)

# Create and save the plot
h2_plot <- ggplot(df) +
    aes(y = collected_funds, x = negemo, color = social_factor) +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", formula = "y ~ x", se = TRUE) +
    labs(
        y = "Fundraising Ratio (Collected/Goal)",
        x = "Negative emotion",
        color = "Social (0/1)"
    ) +
    scale_color_discrete(labels = c("No Social", "Social")) +
    theme_classic()

# Save the plot
ggsave("h2_interaction_plot.png", h2_plot, width = 10, height = 6, dpi = 300)
