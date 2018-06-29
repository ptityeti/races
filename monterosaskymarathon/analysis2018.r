# make some analysis on the Monte Rosa Skymarathon results
library(ggplot2)

# read file as saved in script getresults.r
monterosa.results <- read.table('results_monterosa_2018.txt', sep = "\t", quote = '', stringsAsFactors = FALSE, header = TRUE)


# distribution over countries
ggplot(monterosa.results, aes(x = factor(country, levels = rev(levels(as.factor(country)))), fill = !is.na(cp4.time.numeric))) +
  geom_bar() +
  coord_flip() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(labels = c("Non Finishers", "Finishers"), values = c("lightgrey", "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL, title = "Participants by country")

