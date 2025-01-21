# Load necessary library
library(ggplot2)

# Read the CSV file (adjust the path accordingly)
data <- read.csv("C:\\Users\\test\\Desktop\\2MD\\SystemAdministrators.csv")

# Create a scatter plot
plot <- ggplot(data, aes(x = Experience, y = Training, color = Completed.task)) +
  geom_point(size = 3, alpha = 0.8) +  # Adjust size and transparency of points
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +  # Custom colors
  labs(
    title = "Experience vs. Training of System Administrators",
    x = "Years of Experience",
    y = "Number of Training Sessions",
    color = "Task Completion"
  ) +
  theme_minimal() +  # Use a clean theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

dev.off()  # Clears any previous graphics devices

# Display the plot
print(plot)

ggsave("C:\\Users\\test\\Desktop\\2MD\\scatterplot.png", plot, width = 8, height = 6)