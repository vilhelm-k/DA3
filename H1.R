required_packages <- c(
  "rio", "ggplot2", "vtable", "plot3D",
  "corrplot", "regclass", "olsrr", "mediation", "multilevel", "stargazer"
)

for (i in 1:length(required_packages)) {
  if (!required_packages[i] %in% installed.packages()) {
    install.packages(required_packages[i])
  }
}
df <- rio::import("BE603_gr_10.csv")

# Hypothesis 1: community effect. mediating.
# Engaging with the community generates more comments (interaction), which in turn increases the collected funds.

model_T <- lm(collected_funds ~ updates_count, data = df)
print("------------------MODEL T------------------")
print(summary(model_T))

model_M <- lm(comments_count ~ updates_count, data = df)
print("------------------MODEL M------------------")
print(summary(model_M))

model_Y <- lm(collected_funds ~ comments_count + updates_count, data = df)
print("------------------MODEL Y------------------")
print(summary(model_Y))

# Bootstrap
model_bootstrap <- mediation::mediate(
  model.m = model_M,
  model.y = model_Y,
  treat = "updates_count",
  mediator = "comments_count",
  boot = TRUE,
  sims = 500
)
print("------------------BOOTSTRAP------------------")
print(summary(model_bootstrap))
plot(model_bootstrap)

# Save the mediation analysis plot
png("H1_mediation_analysis_plot.png")
plot(model_bootstrap)
dev.off()



M_plot <- ggplot(df) +
  aes(y = comments_count, x = updates_count) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl") +
  labs(
    y = "Comments Count",
    x = "Updates Count"
  ) +
  theme_classic()

# Save the plot
ggsave("H1_mediating_plot.png", M_plot, width = 10, height = 6, dpi = 300)

T_plot <- ggplot(df) +
  aes(y = collected_funds, x = updates_count) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", se = TRUE) +
  labs(
    y = "Collected funds",
    x = "Updates Count"
  ) +
  theme_classic()

# Create 3D scatter plot with regression plane
library(plot3D) # Add this line to load the package
x <- df$updates_count
y <- df$comments_count
z <- df$collected_funds

# Compute the linear regression
fit <- lm(z ~ x + y)

# Create grid for regression plane
grid.lines <- 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy),
  nrow = grid.lines, ncol = grid.lines
)

# Create fitted points for droplines
fitpoints <- predict(fit)

# Create and save 3D plot
png("h1_3d_plot.png", width = 800, height = 800)
scatter3D(x, y, z,
  pch = 19, cex = 1, colvar = NULL, col = "red",
  theta = 20, phi = 10, bty = "b",
  xlab = "Updates Count", ylab = "Comments Count",
  zlab = "Collected funds",
  surf = list(
    x = x.pred, y = y.pred, z = z.pred,
    facets = TRUE, fit = fitpoints,
    col = ramp.col(
      col = c("dodgerblue3", "seagreen2"),
      n = 300, alpha = 0.9
    ),
    border = "black"
  ),
  main = "Updates, Comments, and Collected funds"
)
dev.off()
