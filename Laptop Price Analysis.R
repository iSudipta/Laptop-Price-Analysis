library(dplyr)
library(ggstatsplot)
library(ggplot2)
library(reshape2)
library(GGally)

laptops <- read.csv("C:/Users/Sudipta/Documents/laptopprice.csv")
head(laptops)
str(laptops)

hist(
  laptops$Price_Euro,
  breaks = 100,
  col = "steelblue",
  border = "black",
  main = "Histogram of Laptop Prices",
  xlab = "Price (Euros)",
  ylab = "Frequency"
)

price_data <- laptops$Price_Euro

hist(price_data, 
     breaks = 100,
     probability = TRUE,
     col = "lightblue", 
     border = "black", 
     main = "Histogram of Laptop Prices with Gaussian Curve",
     xlab = "Price (Euro)")

curve(dnorm(x, mean = mean(price_data, na.rm = TRUE), sd = sd(price_data, na.rm = TRUE)), 
      col = "orange", 
      lwd = 2, 
      add = TRUE)

abline(v = mean(price_data, na.rm = TRUE), 
       col = "blue", 
       lwd = 2, 
       lty = 2)

legend("topright", 
       legend = c("Density Curve", "Mean"),
       col = c("red", "blue"), 
       lty = c(1, 2), 
       lwd = c(2, 2))

ggplot(laptops, aes(x = Inches, y = Price_Euro)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 1) +
  labs(
    title = "Relationship Between Screen Size and Price",
    x = "Screen Size (Inches)",
    y = "Price (Euros)"
  ) +
  theme_minimal()


ggplot(laptops) +
  geom_line(aes(x = Price_Euro, y = RAM_GB, color = "RAM (GB)"), linewidth = 1) +
  geom_line(aes(x = Price_Euro, y = Weight_kg, color = "Weight (kg)"), linewidth = 1) +
  geom_line(aes(x = Price_Euro, y = Inches, color = "Inches"), linewidth = 1) +
  scale_color_manual(
    name = "Legend",
    values = c("RAM (GB)" = "blue", "Weight (kg)" = "green", "Inches" = "red")
  ) +
  labs(
    title = "Relationship of RAM, Weight, and Screen Size with Price",
    x = "Price (Euros)",
    y = "Values"
  ) +
  theme_minimal()

ggbetweenstats(
  data = laptops,
  x = TypeName,
  y = Price_Euro,
  plot.type = "violin",
  title = "Distribution of Price by Laptop Type",
  xlab = "Laptop Type",
  ylab = "Price (Euro)",
  messages = FALSE,
  pairwise.comparisons = FALSE
)

ggplot(laptops, aes(x = TypeName, y = Price_Euro, fill = TypeName)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  labs(
    title = "Distribution of Price by Laptop Type",
    x = "Laptop Type",
    y = "Price (Euro)"
  ) +
  theme_minimal()


laptops$Group <- rep(c("Price", "RAM", "Weight", "Inches"), length.out = nrow(laptops))
ggpairs(
  laptops[, c("Price_Euro", "RAM_GB", "Weight_kg", "Inches")],
  title = "Scatterplot Matrix of Price, RAM, Weight, and Screen Size",
  columnLabels = c("Price (Euro)", "RAM (GB)", "Weight (kg)", "Screen Size (Inches)"),
  aes(color = laptops$Group)
)

numerical_cols <- laptops[, c("Price_Euro", "Inches", "Weight_kg", "RAM_GB")]
colors <- c("#3366CC", "#FF5733", "#33FF57", "#FF33FF")
pairs(numerical_cols,
      main = "Scatter Matrix of Laptop Characteristics",
      pch = 16,
      col = colors,
      cex = 0.6,
      labels = c("Price (€)", "Screen Size", "Weight (kg)", "RAM (GB)"))

ggplot(laptops, aes(x=RAM_GB, y=Price_Euro, alpha=Inches)) + 
  geom_point(size=4, color="darkred") +
  ggtitle("Scatterplot Of ScreenSize with price and ram") +
  theme_minimal()

