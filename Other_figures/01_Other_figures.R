###################################################
######### Other figures for the thesis ############
###################################################

# Load packages #
install.packages("gplots")  # Install the package "gplots"

# Set path to save the plots
file_path <- "C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Other_figures/figures"

####### Example skewness of data ########

library(ggplot2)

# Create data for the curves
x_values <- seq(0, 1, length.out = 1000)
data <- data.frame(
  x = rep(x_values, 3),
  y = c(dbeta(x_values, 2, 5)/12, dbeta(x_values, 5, 2)/12, dbeta(x_values, 5, 5)/12),
  color = rep(c("left-skewed", "right-skewed", "normal distribution"), each = 1000)
)

# Create plot for skewness
ggplot(data, aes(x = x, y = y, color = color)) +
  geom_line(size = 1.25) +
  labs(title = "Types of data distributions", y = "frequency", x = "count value") +
  scale_color_manual(name = "", values = c("left-skewed" = "blue", 
                                           "normal distribution" = "red", 
                                           "right-skewed" = "green")) +  theme_classic() +
  theme(
    legend.text = element_text(size=13, face ="plain"),
    axis.title = element_text(size =13, face= "plain"),
    title = element_text(size =13, face= "bold"),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.position = c(1.42, 1.1), legend.justification = c(1, 1),
    plot.margin = margin(t = 0, r = 5.5, b = 0, l = 0, unit = "cm"), 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # box around the graphic
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave(filename = file.path(file_path, "skewness.png"), width = 7.2, height = 3.2, dpi = 300, units = "in")


########## Example for a dendrogram ############

library(gplots)

# Create data for the dendrogram
data_matrix <- matrix(runif(31), nrow = 9)

# Calculate distance matrix
dist_matrix <- as.dist(1 - cor(t(data_matrix)))

# Create dendrogram
dendrogram <- as.dendrogram(hclust(dist_matrix))

par(mar = c(5, 4.5, 3, 0))  # margins for the bottom, left, up, right of the plot
png(filename = file.path(file_path, "example_dendrogram.png"), width = 430, height = 320, units = "px")

# Plot dendrogram
plot(dendrogram, main = "Example of a dendrogram", ylab = "distance", xlab = "objects", 
     cex.axis =1.1, cex = 1.1, cex.lab= 1.3, cex.main = 1.3)

# Save dendrogram
dev.off()  

### Example for a Scree plot ###

 # Load ggplot2
library(ggplot2)

# Create data for the scree plot
eigenvalues <- c(43, 15, 7, 6, 5.5, 5.3)

data <- data.frame(
  Components = c(1:length(eigenvalues)),
  Eigenvalues = eigenvalues)

# Create scree plot
scree_plot <- ggplot(data, aes(x = Components, y = Eigenvalues)) +
  geom_line(color = "black", size = 1) +
  labs(title = "Example for a scree plot", x = "cluster number", y = "distance") +
  theme_classic() +
  theme(
    axis.title = element_text(size =13, face= "plain"),
    axis.text.y  = element_text(size= 11, face= "plain"),
    axis.text.x =element_text(size=13, face= "plain"), 
    title = element_text(size =13, face= "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(min(data$Components), max(data$Components), by = 1))+
  geom_rect(aes(xmin = 2.7, xmax = 3.3, ymin = 3, ymax = 12), fill = NA, color = "red", size = 1)+
  geom_text(aes(x = 3.5, y = 16, label = "elbow"), color = "red", size = 5)

# Save scree plot
ggsave(filename = file.path(file_path, "example_scree_plot.png"), width = 6, height = 4, dpi = 300, units = "in")

