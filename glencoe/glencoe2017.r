library(ggplot2)
library(reshape2)

# set working directory to folder containing results first
setwd('~/Documents/raceanalysis/races/glencoe')

# function to convert hh:mm:ss to minutes
hhmmss2min <- function(hhmmss)
{as.numeric(strsplit(hhmmss, ":")[[1]][1]) *60 + as.numeric(strsplit(hhmmss, ":")[[1]][2]) + as.numeric(strsplit(hhmmss, ":")[[1]][3]) / 60}

# read results file 2015
results2015 <- read.table(file = "results/glencoe_results2015.txt", sep = "\t", stringsAsFactors = F, header = T, quote = "", comment.char = "")
# add finish position
results2015$Position <- as.numeric(gsub('(st|nd|rd|th|=)', '', results2015$Pos))
# clean up names
results2015$Name <- gsub(' $', '', results2015$Name)
results2015$Name <- gsub('(Twitter|Instagram|Facebook|Strava)', '', results2015$Name)
# finish time in minutes
results2015$Finish <- sapply(results2015$Time, hhmmss2min)
# cp times in minutes
for(i in 1:15)
{results2015[, paste0("CP", i)] <- sapply(results2015[, i+6], hhmmss2min)}

# read results file 2016
results2016 <- read.table(file = "results/glencoe_results2016.txt", sep = "\t", stringsAsFactors = F, header = T, quote = "", comment.char = "")
# add finish position
results2016$Position <- as.numeric(gsub('(st|nd|rd|th|=)', '', results2016$Pos))
# clean up names
results2016$Name <- gsub(' $', '', results2016$Name)
results2016$Name <- gsub('(Twitter|Instagram|Facebook|Strava|Attackpoint|Power Of Ten)', '', results2016$Name)
# finish time in minutes
results2016$Finish <- sapply(results2016$Time.Taken, hhmmss2min)
results2016[results2016$Last.Location != "Finish - Kinlochleven ", "Finish"] <- NA
# cp times in minutes
for(i in 1:15)
{results2016[, paste0("CPTime", i)] <- sapply(results2016[, paste0("CP", i)], hhmmss2min)}

# read results file 2017
results2017 <- read.table(file = "results/glencoe_results2017.txt", sep = "\t", stringsAsFactors = F, header = T, quote = "", comment.char = "")
# add finish position
results2017$Position <- as.numeric(gsub('(st|nd|rd|th|=)', '', results2017$Pos))
# clean up names
results2017$Name <- gsub(' $', '', results2017$Name)
results2017$Name <- gsub('(Twitter|Instagram|Facebook|Strava|Attackpoint|Power Of Ten)', '', results2017$Name)
# finish time in minutes
results2017$Finish <- sapply(results2017$Time.Taken, hhmmss2min)
# cp times in minutes
for(i in 1:15)
{results2017[, paste0("CPTime", i)] <- sapply(results2017[, i + 7], hhmmss2min)}
results2017$CPTime0 <- results2017$Start
results2017$CPTime16 <- results2017$Finish
results2017$Start <- 0

# define different distances (estimated from race profile)
distances <- data.frame(
  checkpoint = c(paste0("CPTime", seq(1,15)), "Finish", "Start"),
  #  dist = c(10, 13, 16, 18, 21, 25, 27, 28.5, 30, 31, 34, 35, 41, 44, 50, 55, 0)
  dist = c(9.5, 12.5, 14.9, 17, 19.8, 22.7, 25, 26.3, 27.1, 28, 31.7, 33.5, 36.8, 39.9, 43, 48.7, 0),
  checkpoint2 = c(paste0("Position", seq(1,15)), "Position16", 0)
)


# plot the times
plotdata <- melt(data = results2017[, c("Name", "Start", paste0("CPTime", seq(1:15)), "Finish")], id = "Name")
plotdata <- subset(plotdata, !is.na(value))
plotdata <- merge(x = plotdata, y = distances, by.x = "variable", by.y = "checkpoint", all = TRUE)
plotdata$Name <- as.factor(plotdata$Name)
ggplot(data = plotdata, aes(x = dist, y = value/60, group = Name)) + 
  geom_line(color = "blue", alpha = 0.2) +
  geom_line(data = subset(plotdata, Name == "Wouter Hamelinck"), aes(x = dist, y = value/60), color = "red") +
  geom_segment(data = data.frame(dist = c(9.5, 19.8, 31.7, 48.7), time = c(2.5, 6, 8, 14), Name = rep("", 4)), aes(x = dist, y = time, xend = dist, yend = Inf), color = "white", alpha = 1) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    axis.title.x = element_text(color = "white"),  
    axis.title.y = element_text(color = "white"),  
    panel.grid.major = element_line(color = rgb(0.3, 0.3, 0.3)),  
    panel.grid.minor = element_line(color = rgb(0.2, 0.2, 0.2)),
    legend.position = "none"
  ) +
#  scale_color_manual(values = sapply(levels(plotdata$Name), FUN = function(x){if(x == "Wouter Hamelinck"){"red"}else{"blue"}})) +
#  scale_alpha_manual(values = sapply(levels(plotdata$Name), FUN = function(x){if(x == "Wouter Hamelinck"){1.0}else{0.1}})) +
  labs(x = "Distance (km)", y = "Running time (hours)")
ggsave('timeplot2017.png', width = 8, height = 6, dpi = 100, units = "in")

# calculate race positions
# remove Bjorn Verduijn and Peter Toaig because they have a lot of missing values
positions <- data.frame(Name = subset(results2017, !is.na(Finish) & !(Name %in% c("Bjorn Verduijn", "Peter Toaig")))$Name)
for(i in 1:16)
{positions[, paste0("Position", i)] <- rank(subset(results2017, !is.na(Finish) & !(Name %in% c("Bjorn Verduijn", "Peter Toaig")))[, paste0("CPTime", i)])}
plotdatapos <- melt(data = positions, id = "Name")
# remove missing value at CP1 of Harry Kingston
plotdatapos <- subset(plotdatapos, Name != "Harry Kingston" | variable != "Position1")
plotdatapos <- merge(x = plotdatapos, y = distances, by.x = "variable", by.y = "checkpoint2", all = TRUE)
ggplot(data = plotdatapos, aes(x = dist, y = value, group = Name)) + 
  geom_line(color = "blue", alpha = 0.4) +
  geom_line(data = subset(plotdatapos, Name == "Wouter Hamelinck"), aes(x = dist, y = value), color = "red") +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    axis.title.x = element_text(color = "white"),  
    axis.title.y = element_text(color = "white"),  
    panel.grid.major = element_line(color = rgb(0.3, 0.3, 0.3)),  
    panel.grid.minor = element_line(color = rgb(0.2, 0.2, 0.2)),
    legend.position = "none"
  ) +
  #  scale_color_manual(values = sapply(levels(plotdata$Name), FUN = function(x){if(x == "Wouter Hamelinck"){"red"}else{"blue"}})) +
  #  scale_alpha_manual(values = sapply(levels(plotdata$Name), FUN = function(x){if(x == "Wouter Hamelinck"){1.0}else{0.1}})) +
  labs(x = "Distance (km)", y = "Position among finishers")
ggsave('positionplot2017.png', width = 8, height = 6, dpi = 100, units = "in")


#calculate times in leg
timeslist <- melt(results2017[, c("Name", paste0("CPTime", seq(0,16)))], id = "Name")
timeslist$variable <- as.numeric(as.character(substr(timeslist$variable, 7, 9)))
legtimes <- subset(merge(timeslist, timeslist, by = "Name"), variable.x < variable.y)
names(legtimes) <- c("Name", "From", "TimeStart", "To", "TimeEnd")
legtimes$Time <- legtimes$TimeEnd - legtimes$TimeStart
fastest.times <- aggregate(Time ~ From + To, legtimes, FUN = min)
fastest.times <- merge(fastest.times, legtimes)
fastest.men <- aggregate(Name ~ To + From, fastest.times[order(fastest.times$Name), ], FUN = function(x){paste(x, collapse = ", ")})
fastest.men$Name <- gsub(" Burgada", "", fastest.men$Name)
cplist <- c("Start", "Altnafeadh", "Stob Dearg", "Buachaille Etive More", "Lairig Gartain River", "Lairig Eilde Junction", "Lairig Eilde Cairn", "Stob Coire Sgreamhach", "Bidean Nam Bian", "Stob Coire nan Lochan", "Bidean Nam Bian", "Loch Achtrician", "Sgorr nam Fiannaidh", "Am Bodach", "Sron a' Choire Odhair-bhig", "WHW", "Finish")
ggplot(data = fastest.men, aes(y = as.factor(-From), x = as.factor(To), fill = as.factor(Name))) +
  geom_tile() +
  scale_x_discrete(labels = cplist[-1]) +
  scale_y_discrete(labels = rev(cplist[-17])) + 
  scale_fill_manual(values = c("#8dd3c7", "#bbcccc", "#ccbbcc", "#ffffb3", "#aeaaea", "#fb8072", "#80b1d3", "#fdb462", "#ccccbb", "#b3de69", "#fccde5")) +
  theme(
    legend.position = "bottom", 
    axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
    ) +
  labs(x = "To", y = "From", title = "Fastest runners for each part of the 2017 Glen Coe")
ggsave('fastestplot2017.png', width = 8, height = 6, dpi = 100, units = "in")
