library(ggplot2)
library(reshape2)

# set working directory to folder of this file first
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
# calculate leg times
results2015$Leg1 <- results2015$CP1
for(i in 2:15)
{results2015[, paste0("Leg", i)] <- results2015[, paste0("CP", i)] - results2015[, paste0("CP", i-1)]}
results2015$Leg16 <- results2015$Finish - results2015$CP15
# calculate leg percentages
for(i in 1:16)
{results2015[, paste0("LegPerc", i)] <- results2015[, paste0("Leg", i)] / results2015[, "Finish"]}

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
# calculate leg times
# ignore CP7 because it was not present in 2015
# hence, CP6->CP8 is Leg7, CP8->CP9 is Leg 8 etc
results2016$Leg1 <- results2016$CPTime1
for(i in 2:6)
{results2016[, paste0("Leg", i)] <- results2016[, paste0("CPTime", i)] - results2016[, paste0("CPTime", i-1)]}
results2016$Leg7 <- results2016$CPTime8 - results2016$CPTime6
for(i in 8:14)
{results2016[, paste0("Leg", i)] <- results2016[, paste0("CPTime", i+1)] - results2016[, paste0("CPTime", i)]}
results2016$Leg15 <- results2016$Finish - results2016$CPTime15
# calculate leg percentages
for(i in 1:15)
{results2016[, paste0("LegPerc", i)] <- results2016[, paste0("Leg", i)] / results2016[, "Finish"]}


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
# calculate leg times
# ignore CP7 because it was not present in 2015
# hence, CP6->CP8 is Leg7, CP8->CP9 is Leg 8 etc
results2017$Leg1 <- results2017$CPTime1
for(i in 2:6)
{results2017[, paste0("Leg", i)] <- results2017[, paste0("CPTime", i)] - results2017[, paste0("CPTime", i-1)]}
results2017$Leg7 <- results2017$CPTime8 - results2017$CPTime6
for(i in 8:14)
{results2017[, paste0("Leg", i)] <- results2017[, paste0("CPTime", i+1)] - results2017[, paste0("CPTime", i)]}
results2017$Leg15 <- results2017$Finish - results2017$CPTime15
# calculate leg percentages
for(i in 1:15)
{results2017[, paste0("LegPerc", i)] <- results2017[, paste0("Leg", i)] / results2017[, "Finish"]}


# merge the results for comparison
results.both <- merge(
  results2015[, c("Name", "Finish", paste0("CP", 1:15), paste0("Leg", 1:16))], 
  results2016[, c("Name", "Finish", paste0("CPTime", 1:15), paste0("Leg", 1:15))], 
  by = "Name")

# counts of finishers
# finishers in 2015
nrow(subset(results2015, !is.na(Finish)))
# finishers in 2016
nrow(subset(results2016, !is.na(Finish)))
# finishers in both 2015 and 2016
nrow(subset(results.both, !is.na(Finish.x) & !is.na(Finish.y)))
# twice participants, DNF in 2015
nrow(subset(results.both, is.na(Finish.x)))
# twice participants, DNF in 2016
nrow(subset(results.both, is.na(Finish.y)))
# twice participants, twice DNF
nrow(subset(results.both, is.na(Finish.x) & is.na(Finish.y)))

# compare times for double finishers
p <- ggplot(subset(results.both, !is.na(Finish.x) & !is.na(Finish.y)), aes(x = Finish.x, y = Finish.y)) +
  geom_abline(slope = 1, color = "red") +
  geom_point() +
  labs(x = "2015 Finish Time", y = "2016 Finish Time", title = "Comparison of Finish Times in 2015 and 2016")
ggsave('plots/finishevolution.png', p)
t.test(results.both$Finish.y / results.both$Finish.x)

# compare times over CP1-CP13/14
p <- ggplot(subset(results.both, !is.na(Finish.x) & !is.na(Finish.y)), aes(x = CP13 - CP1, y = CPTime14 - CPTime1)) + 
  geom_abline(slope = 1, color = "red") +
  geom_point() +
  labs(x = "2015 Time", y = "2016 Time", title = "Comparison of times between Altnafeach and Sron a' Choire Odhair-bhig")
ggsave('plots/commonevolution.png', p)
t.test((results.both$CPTime14 - results.both$CPTime1) / (results.both$CP13 - results.both$CP1) - 1)
faster <- mean((results.both$CPTime14 - results.both$CPTime1) / (results.both$CP13 - results.both$CP1), na.rm = T)

# compare times over CP2-CP13/14
# no guarantee that CP1 was both times at the same point
p <- ggplot(subset(results.both, !is.na(Finish.x) & !is.na(Finish.y)), aes(x = CP13 - CP2, y = CPTime14 - CPTime2)) + 
  geom_abline(slope = 1, color = "red") +
  geom_point() +
  labs(x = "2015 Time", y = "2016 Time", title = "Comparison of times between Stob Dearg and Sron a' Choire Odhair-bhig")
ggsave('plots/commonevolution_aftercurvedridge.png', p)
t.test((results.both$CPTime14 - results.both$CPTime2) / (results.both$CP13 - results.both$CP2) - 1)
faster <- mean((results.both$CPTime14 - results.both$CPTime2) / (results.both$CP13 - results.both$CP2), na.rm = T)

# compare times over Curved Ridge
p <- ggplot(subset(results.both, !is.na(Finish.x) & !is.na(Finish.y)), aes(x = CP2 - CP1, y = CPTime2 - CPTime1)) + 
  geom_abline(slope = 1, color = "red") +
  geom_point() +
  labs(x = "2015 Time", y = "2016 Time", title = "Comparison of times for Curved Ridge")
ggsave('plots/curved ridgeevolution.png', p)
t.test((results.both$CPTime2 - results.both$CPTime1) / (results.both$CP2 - results.both$CP1) - 1)
faster <- mean((results.both$CPTime14 - results.both$CPTime1) / (results.both$CP14 - results.both$CP1), na.rm = T)

# compare times over all common legs
plotdata <- data.frame(lower = numeric(0), mean = numeric(0), upper = numeric(0))
for(i in 2:13)
{
  testresult <- t.test(results.both[, paste0("Leg", i, ".y")] / results.both[, paste0("Leg", i, ".x")], na.rm = T)
  plotdata[nrow(plotdata) + 1, ] <- c(testresult[["conf.int"]][1], testresult[["estimate"]], testresult[["conf.int"]][2])
}  

# boxplots
plotdata <- melt(results2015, id.vars = "Name", measure.vars = paste0("LegPerc", 1:16))
ggplot(plotdata, aes(x = variable, y = value)) +
  geom_boxplot()

plotdata <- melt(results2016, id.vars = "Name", measure.vars = paste0("LegPerc", 1:16))
plotdata$variable <- sprintf("%02d", as.numeric(gsub("LegPerc", "", plotdata$variable)))
ggplot(plotdata, aes(x = variable, y = value)) +
  geom_boxplot() +
  geom_point(data = subset(plotdata, Name == "Wouter Hamelinck"), color = "red") +
  labs(title = "Boxplots", x = "Number of Leg", y = "Percentage of race time")


# triple finishers
results.triple <- merge(merge(results2015, results2016, by = "Name"), results2017, by = "Name")
results.triple$Finish + results.triple$Finish.x + results.triple$Finish.y
