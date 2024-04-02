song_data = read.csv("song.csv")
correlation <- cor(song_data$Months, song_data$Songs)
correlation

library(ggplot2)

# Create a scatter plot with a regression line
ggplot(song_data, aes(x = Months, y = Songs)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Songs vs Months with Regression Line",
       x = "Months",
       y = "Songs")