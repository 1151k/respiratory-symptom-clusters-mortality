# produce line plot (for average Silhouette score, clustering objective function, and compactness)



# Load external packages
packages_all = c("dplyr", "ggplot2")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
   install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# constants
output_name <- 'silhouette-score' # 'silhouette-score', 'objective-function', 'compactness' (the name of the output file [without extension])
y_text <- 'Average silhouette score' # 'Average silhouette score', 'Average objective function value', 'Average within-cluster distance' (the text to be displayed to the left of the y-axis)
color <- '#996bde' # #996bde, #30cca5, #1a1794 (the color of the line)
csv <- read.csv(paste0('output/csv/', output_name, '.csv'), header = FALSE)
# loop csv to get the average value for each "column", and store it in a c() with the name y
y <- c()
for (i in 1:ncol(csv)) {
    y <- c(y, mean(csv[,i]))
}
x <- seq(2, length(y)+1, 1)
output_folder <- "output/"
plot_data <- data.frame(
    x <- x,
    y <- y
)



# plot and save
plot <- ggplot(
    plot_data,
    aes(x)
) + labs(x = 'Number of clusters, k') + labs(y = y_text) + geom_line(aes(y = y), color = 'blue')
plot
ggsave(paste0(output_folder, 'svg/', output_name, '.svg'), plot, dpi = 300)