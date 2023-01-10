library(ggplot2)
library(stringr)

######## New figures for density ########

# Path = "/Users/adrianbach/Desktop/PhD/GitKraken/Chapter2model/localSA/"
# Path = "/home/adrian/Documents/GitKraken/ne"
Path = "C:/Users/adb3/Desktop/PhD/GitKraken/newLocalSA/"

#### average offspring nb ####
folderPath = paste(Path, "folder-avgOff/allStatsAndPlots/localSAfiles", sep = "")
filePath = paste(folderPath, "stats-folder-avgOff.csv", sep = "/")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

x = data$prey2avgOffs
y1 = data$prey1densBeforeMean
y2 = data$prey1densAfterMean
y3 = data$prey2densBeforeMean
y4 = data$prey2densAfterMean
y5 = data$predatorDensMean
y1min = data$prey1densBeforeMin
y2min = data$prey1densAfterMin
y3min = data$prey2densBeforeMin
y4min = data$prey2densAfterMin
y5min = data$predatorDensMin
y1max = data$prey1densBeforeMax
y2max = data$prey1densAfterMax
y3max = data$prey2densBeforeMax
y4max = data$prey2densAfterMax
y5max = data$predatorDensMax
y1c = "pink"
y2c = "red"
y3c = "cyan"
y4c = "blue"
y5c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  # geom_hline(yintercept = -1, color = "darkred", linetype = "dashed") +
  # geom_hline(yintercept = y3[1], alpha = 0.5, color = "darkblue", linetype = "dashed") +
  # geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.2)) +
  geom_rect(aes(xmin = 0.75, xmax = 1.35, ymin = 0, ymax = 1.05*max(max(y2max), max(y4max))), alpha=0.5, fill = "lightgrey") +
  geom_hline(yintercept = y1[1], alpha = 0.5, color = "black", linetype = "dashed") +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = -0.20)) +
  # geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = -0.05)) +
  geom_line(aes(y = y4), color = y4c, alpha = 0.2, position = position_nudge(x = 0.00)) +
  geom_line(aes(y = y5), color = y5c, alpha = 0.2, position = position_nudge(x = 0.20)) +
  # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(data$prey2PopulationSizeMean)), alpha=0.5, fill = "lightgrey") +
  # geom_pointrange(aes(y = y1, ymin = y1min, ymax = y1max), shape = 21, fill = "white", size = 0.5, col = y1c, position = position_nudge(x = -0.2)) +
  geom_pointrange(aes(y = y2, ymin = y2min, ymax = y2max), shape = 21, fill = "white", size = 0.4, col = y2c, position = position_nudge(x = -0.20)) +
  # geom_pointrange(aes(y = y3, ymin = y3min, ymax = y3max), shape = 22, fill = "white", size = 0.4, col = y3c, position = position_nudge(x = -0.05)) +
  geom_pointrange(aes(y = y4, ymin = y4min, ymax = y4max), shape = 22, fill = "white", size = 0.4, col = y4c, position = position_nudge(x = 0.00)) +
  geom_pointrange(aes(y = y5, ymin = y5min, ymax = y5max), shape = 24, fill = "white", size = 0.4, col = y5c, position = position_nudge(x = 0.20)) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  # geom_point(aes(y = y1), size = 2.5, shape = 24, fill = "white", color = y1c, position = position_nudge(x = -0.1)) +
  # geom_point(aes(y = y2), size = 2.5, shape = 25, fill = "white", color = y2c, position = position_nudge(x = 0.1)) # +
  # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.15)) +
  labs(x = "Prey 2 average birth rate", y = "Average density") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank()) # + 
# theme(axis.line = element_line(color = "black")) # +
# scale_colour_manual(name='Populations',
#                     breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                     values=c(y2c, y4c, y5c))
fig
# scale_colour_manual(name='Populations',
#                     breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                     values=c(y2c, y4c, y5c))

# save plot in this folder
ggsave(filename = "newSA-prey2avgBR-density.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

#### Catch proba ####
folderPath = paste(Path, "folder-ctchPr/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-ctchPr.csv", sep = "")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

x = data$prey2catchProb
y1 = data$prey1densBeforeMean
y2 = data$prey1densAfterMean
y3 = data$prey2densBeforeMean
y4 = data$prey2densAfterMean
y5 = data$predatorDensMean
y1min = data$prey1densBeforeMin
y2min = data$prey1densAfterMin
y3min = data$prey2densBeforeMin
y4min = data$prey2densAfterMin
y5min = data$predatorDensMin
y1max = data$prey1densBeforeMax
y2max = data$prey1densAfterMax
y3max = data$prey2densBeforeMax
y4max = data$prey2densAfterMax
y5max = data$predatorDensMax
y1c = "pink"
y2c = "red"
y3c = "cyan"
y4c = "blue"
y5c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  # geom_hline(yintercept = -1, color = "darkred", linetype = "dashed") +
  # geom_hline(yintercept = y3[1], alpha = 0.5, color = "darkblue", linetype = "dashed") +
  # geom_vline(xintercept = 1, alpha = 0.5, color = "black", linetype = "dashed") +
  geom_rect(aes(xmin = 0.08, xmax = 0.12, ymin = 0, ymax = 1.05*max(max(y2max), max(y4max))), alpha=0.5, fill = "lightgrey") +
  geom_hline(yintercept = y1[1], alpha = 0.5, color = "black", linetype = "dashed") +
  # geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.2)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = -0.01)) +
  # geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = -0.05)) +
  geom_line(aes(y = y4), color = y4c, alpha = 0.2, position = position_nudge(x = 0.00)) +
  geom_line(aes(y = y5), color = y5c, alpha = 0.2, position = position_nudge(x = 0.01)) +
  # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(data$prey2PopulationSizeMean)), alpha=0.5, fill = "lightgrey") +
  # geom_pointrange(aes(y = y1, ymin = y1min, ymax = y1max), shape = 21, fill = "white", size = 0.5, col = y1c, position = position_nudge(x = -0.2)) +
  geom_pointrange(aes(y = y2, ymin = y2min, ymax = y2max), shape = 21, fill = "white", size = 0.4, col = y2c, position = position_nudge(x = -0.01)) +
  # geom_pointrange(aes(y = y3, ymin = y3min, ymax = y3max), shape = 22, fill = "white", size = 0.4, col = y3c, position = position_nudge(x = -0.05)) +
  geom_pointrange(aes(y = y4, ymin = y4min, ymax = y4max), shape = 22, fill = "white", size = 0.4, col = y4c, position = position_nudge(x = 0.00)) +
  geom_pointrange(aes(y = y5, ymin = y5min, ymax = y5max), shape = 24, fill = "white", size = 0.4, col = y5c, position = position_nudge(x = 0.01)) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, 2fill = y3c) +
  # geom_point(aes(y = y1), size = 2.5, shape = 24, fill = "white", color = y1c, position = position_nudge(x = -0.1)) +
  # geom_point(aes(y = y2), size = 2.5, shape = 25, fill = "white", color = y2c, position = position_nudge(x = 0.1)) # +
  # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.15)) +
  labs(x = "Prey 2 catch probability", y = "Average density") +
  scale_x_continuous(breaks = x, labels = x)+ 
  theme(panel.grid.minor = element_blank())
fig

# save plot in this folder
ggsave(filename = "newSA-prey2catchProba-density.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

#### Conv rate ####
folderPath = paste(Path, "folder-cnvRte/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-cnvRte.csv", sep = "")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

x = data$prey2convRate
y1 = data$prey1densBeforeMean
y2 = data$prey1densAfterMean
y3 = data$prey2densBeforeMean
y4 = data$prey2densAfterMean
y5 = data$predatorDensMean
y1min = data$prey1densBeforeMin
y2min = data$prey1densAfterMin
y3min = data$prey2densBeforeMin
y4min = data$prey2densAfterMin
y5min = data$predatorDensMin
y1max = data$prey1densBeforeMax
y2max = data$prey1densAfterMax
y3max = data$prey2densBeforeMax
y4max = data$prey2densAfterMax
y5max = data$predatorDensMax
y1c = "pink"
y2c = "red"
y3c = "cyan"
y4c = "blue"
y5c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  # geom_hline(yintercept = -1, color = "darkred", linetype = "dashed") +
  # geom_hline(yintercept = y3[1], alpha = 0.5, color = "darkblue", linetype = "dashed") +
  # geom_vline(xintercept = 1, alpha = 0.5, color = "black", linetype = "dashed") +
  geom_rect(aes(xmin = 90, xmax = 110, ymin = 0, ymax = 1.05*max(max(y2max), max(y4max))), alpha=0.5, fill = "lightgrey") +
  geom_hline(yintercept = y1[1], alpha = 0.5, color = "black", linetype = "dashed") +
  # geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.2)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = -7.00)) +
  # geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = -0.05)) +
  geom_line(aes(y = y4), color = y4c, alpha = 0.2, position = position_nudge(x = 0.00)) +
  geom_line(aes(y = y5), color = y5c, alpha = 0.2, position = position_nudge(x = 7.00)) +
  # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(data$prey2PopulationSizeMean)), alpha=0.5, fill = "lightgrey") +
  # geom_pointrange(aes(y = y1, ymin = y1min, ymax = y1max), shape = 21, fill = "white", size = 0.5, col = y1c, position = position_nudge(x = -0.2)) +
  geom_pointrange(aes(y = y2, ymin = y2min, ymax = y2max), shape = 21, fill = "white", size = 0.4, col = y2c, position = position_nudge(x = -7.00)) +
  # geom_pointrange(aes(y = y3, ymin = y3min, ymax = y3max), shape = 22, fill = "white", size = 0.4, col = y3c, position = position_nudge(x = -0.05)) +
  geom_pointrange(aes(y = y4, ymin = y4min, ymax = y4max), shape = 22, fill = "white", size = 0.4, col = y4c, position = position_nudge(x = 0.00)) +
  geom_pointrange(aes(y = y5, ymin = y5min, ymax = y5max), shape = 24, fill = "white", size = 0.4, col = y5c, position = position_nudge(x = 7.00)) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  # geom_point(aes(y = y1), size = 2.5, shape = 24, fill = "white", color = y1c, position = position_nudge(x = -0.1)) +
  # geom_point(aes(y = y2), size = 2.5, shape = 25, fill = "white", color = y2c, position = position_nudge(x = 0.1)) # +
  # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.15)) +
  labs(x = "Prey 2 resources/catch", y = "Average density") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank())
fig

folderPath = paste(Path, "folder-localSA-ConvRate-over1/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-localSA-ConvRate-over1.csv", sep = "")

data <- read.csv(filePath)
data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

y2Null = data$prey1densAfterMean[dim(data)[1]]
y4Null = data$prey2densAfterMean[dim(data)[1]]
y5Null = data$predatorDensMean[dim(data)[1]]
y2minNull = data$prey1densAfterMin[dim(data)[1]]
y4minNull = data$prey2densAfterMin[dim(data)[1]]
y5minNull = data$predatorDensMin[dim(data)[1]]
y2maxNull = data$prey1densAfterMax[dim(data)[1]]
y4maxNull = data$prey2densAfterMax[dim(data)[1]]
y5maxNull = data$predatorDensMax[dim(data)[1]]

data <- data[-dim(data)[1],]

x = data$convRateRatio
y1 = data$prey1densBeforeMean
y2 = data$prey1densAfterMean
y3 = data$prey2densBeforeMean
y4 = data$prey2densAfterMean
y5 = data$predatorDensMean
y1min = data$prey1densBeforeMin
y2min = data$prey1densAfterMin
y3min = data$prey2densBeforeMin
y4min = data$prey2densAfterMin
y5min = data$predatorDensMin
y1max = data$prey1densBeforeMax
y2max = data$prey1densAfterMax
y3max = data$prey2densBeforeMax
y4max = data$prey2densAfterMax
y5max = data$predatorDensMax
y1c = "pink"
y2c = "red"
y3c = "cyan"
y4c = "blue"
y5c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  # geom_hline(yintercept = -1, color = "darkred", linetype = "dashed") +
  # geom_hline(yintercept = y3[1], alpha = 0.5, color = "darkblue", linetype = "dashed") +
  # geom_vline(xintercept = 1, alpha = 0.5, color = "black", linetype = "dashed") +
  # geom_rect(aes(xmin = 0.96, xmax = 1.04, ymin = 0, ymax = 1.05*max(max(y2max), max(y4max))), alpha=0.5, fill = "lightgrey") +
  # geom_rect(aes(xmin = 0.8, xmax = 5.2, ymin = y1min, ymax = 1.05*max(max(y2max), max(y4max))), alpha=0.5, fill = "lightgrey") +
  geom_hline(yintercept = y2Null, alpha = 0.5, color = y2c, linetype = "dashed") +
  geom_hline(yintercept = y4Null, alpha = 0.5, color = y4c, linetype = "dashed") +
  geom_hline(yintercept = y5Null, alpha = 0.5, color = y5c, linetype = "dashed") +
  # geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.2)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = -0.075)) +
  # geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = -0.05)) +
  geom_line(aes(y = y4), color = y4c, alpha = 0.2, position = position_nudge(x = 0.00)) +
  geom_line(aes(y = y5), color = y5c, alpha = 0.2, position = position_nudge(x = 0.075)) +
  # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(data$prey2PopulationSizeMean)), alpha=0.5, fill = "lightgrey") +
  # geom_pointrange(aes(y = y1, ymin = y1min, ymax = y1max), shape = 21, fill = "white", size = 0.5, col = y1c, position = position_nudge(x = -0.2)) +
  geom_pointrange(aes(y = y2, ymin = y2min, ymax = y2max), shape = 21, fill = "white", size = 0.4, col = y2c, position = position_nudge(x = -0.075)) +
  # geom_pointrange(aes(y = y3, ymin = y3min, ymax = y3max), shape = 22, fill = "white", size = 0.4, col = y3c, position = position_nudge(x = -0.05)) +
  geom_pointrange(aes(y = y4, ymin = y4min, ymax = y4max), shape = 22, fill = "white", size = 0.4, col = y4c, position = position_nudge(x = 0.00)) +
  geom_pointrange(aes(y = y5, ymin = y5min, ymax = y5max), shape = 24, fill = "white", size = 0.4, col = y5c, position = position_nudge(x = 0.075)) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  # geom_point(aes(y = y1), size = 2.5, shape = 24, fill = "white", color = y1c, position = position_nudge(x = -0.1)) +
  # geom_point(aes(y = y2), size = 2.5, shape = 25, fill = "white", color = y2c, position = position_nudge(x = 0.1)) # +
  # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.15)) +
  labs(x = "Prey 2 to prey 1 resources/catch ratio", y = "Average density") +
  scale_x_continuous(breaks = x, labels = x)
fig

# save plot in this folder
ggsave(filename = "newSA-prey2ConvRate-density.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

#### resources available ####
folderPath = paste(Path, "folder-resAva/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-resAva.csv", sep = "")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

# data <- data[-1,]

x = data$prey2resAva
y1 = data$prey1densBeforeMean
y2 = data$prey1densAfterMean
y3 = data$prey2densBeforeMean
y4 = data$prey2densAfterMean
y5 = data$predatorDensMean
y1min = data$prey1densBeforeMin
y2min = data$prey1densAfterMin
y3min = data$prey2densBeforeMin
y4min = data$prey2densAfterMin
y5min = data$predatorDensMin
y1max = data$prey1densBeforeMax
y2max = data$prey1densAfterMax
y3max = data$prey2densBeforeMax
y4max = data$prey2densAfterMax
y5max = data$predatorDensMax
y1c = "pink"
y2c = "red"
y3c = "cyan"
y4c = "blue"
y5c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  # geom_hline(yintercept = -1, color = "darkred", linetype = "dashed") +
  # geom_hline(yintercept = y3[1], alpha = 0.5, color = "darkblue", linetype = "dashed") +
  # geom_vline(xintercept = 1, alpha = 0.5, color = "black", linetype = "dashed") +
  geom_rect(aes(xmin = 90, xmax = 110, ymin = 0, ymax = 1.05*max(max(y2max), max(y4max), max(y1))), alpha=0.5, fill = "lightgrey") +
  geom_hline(yintercept = y1[length(y1)], alpha = 0.5, color = "black", linetype = "dashed") +
  # geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.2)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = -7)) +
  # geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = -0.05)) +
  geom_line(aes(y = y4), color = y4c, alpha = 0.2, position = position_nudge(x = 0.00)) +
  geom_line(aes(y = y5), color = y5c, alpha = 0.2, position = position_nudge(x = 7.00)) +
  # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(data$prey2PopulationSizeMean)), alpha=0.5, fill = "lightgrey") +
  # geom_pointrange(aes(y = y1, ymin = y1min, ymax = y1max), shape = 21, fill = "white", size = 0.5, col = y1c, position = position_nudge(x = -0.2)) +
  geom_pointrange(aes(y = y2, ymin = y2min, ymax = y2max), shape = 21, fill = "white", size = 0.4, col = y2c, position = position_nudge(x = -7.00)) +
  # geom_pointrange(aes(y = y3, ymin = y3min, ymax = y3max), shape = 22, fill = "white", size = 0.4, col = y3c, position = position_nudge(x = -0.05)) +
  geom_pointrange(aes(y = y4, ymin = y4min, ymax = y4max), shape = 22, fill = "white", size = 0.4, col = y4c, position = position_nudge(x = 0.00)) +
  geom_pointrange(aes(y = y5, ymin = y5min, ymax = y5max), shape = 24, fill = "white", size = 0.4, col = y5c, position = position_nudge(x = 7.00)) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  # geom_point(aes(y = y1), size = 2.5, shape = 24, fill = "white", color = y1c, position = position_nudge(x = -0.1)) +
  # geom_point(aes(y = y2), size = 2.5, shape = 25, fill = "white", color = y2c, position = position_nudge(x = 0.1)) # +
  # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.15)) +
  labs(x = "Prey 2 max available resources/cell", y = "Average density") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank())
fig

# save plot in this folder
ggsave(filename = "newSA-prey2maxRes-density.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

#### max cons ####
folderPath = paste(Path, "folder-maxCon/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-maxCon.csv", sep = "")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

x = data$prey2maxCons
y1 = data$prey1densBeforeMean
y2 = data$prey1densAfterMean
y3 = data$prey2densBeforeMean
y4 = data$prey2densAfterMean
y5 = data$predatorDensMean
y1min = data$prey1densBeforeMin
y2min = data$prey1densAfterMin
y3min = data$prey2densBeforeMin
y4min = data$prey2densAfterMin
y5min = data$predatorDensMin
y1max = data$prey1densBeforeMax
y2max = data$prey1densAfterMax
y3max = data$prey2densBeforeMax
y4max = data$prey2densAfterMax
y5max = data$predatorDensMax
y1c = "pink"
y2c = "red"
y3c = "cyan"
y4c = "blue"
y5c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  # geom_hline(yintercept = -1, color = "darkred", linetype = "dashed") +
  # geom_hline(yintercept = y3[1], alpha = 0.5, color = "darkblue", linetype = "dashed") +
  # geom_vline(xintercept = 1, alpha = 0.5, color = "black", linetype = "dashed") +
  geom_rect(aes(xmin = 8, xmax = 12, ymin = 0, ymax = 1.05*max(max(y2max), max(y4max))), alpha=0.5, fill = "lightgrey") +
  geom_hline(yintercept = y1[1], alpha = 0.5, color = "black", linetype = "dashed") +
  # geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.2)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = -1.00)) +
  # geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = -0.05)) +
  geom_line(aes(y = y4), color = y4c, alpha = 0.2, position = position_nudge(x = 0.00)) +
  geom_line(aes(y = y5), color = y5c, alpha = 0.2, position = position_nudge(x = 1.00)) +
  # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(data$prey2PopulationSizeMean)), alpha=0.5, fill = "lightgrey") +
  # geom_pointrange(aes(y = y1, ymin = y1min, ymax = y1max), shape = 21, fill = "white", size = 0.5, col = y1c, position = position_nudge(x = -0.2)) +
  geom_pointrange(aes(y = y2, ymin = y2min, ymax = y2max), shape = 21, fill = "white", size = 0.4, col = y2c, position = position_nudge(x = -1.00)) +
  # geom_pointrange(aes(y = y3, ymin = y3min, ymax = y3max), shape = 22, fill = "white", size = 0.4, col = y3c, position = position_nudge(x = -0.05)) +
  geom_pointrange(aes(y = y4, ymin = y4min, ymax = y4max), shape = 22, fill = "white", size = 0.4, col = y4c, position = position_nudge(x = 0.00)) +
  geom_pointrange(aes(y = y5, ymin = y5min, ymax = y5max), shape = 24, fill = "white", size = 0.4, col = y5c, position = position_nudge(x = 1.00)) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  # geom_point(aes(y = y1), size = 2.5, shape = 24, fill = "white", color = y1c, position = position_nudge(x = -0.1)) +
  # geom_point(aes(y = y2), size = 2.5, shape = 25, fill = "white", color = y2c, position = position_nudge(x = 0.1)) # +
  # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.15)) +
  labs(x = "Prey 2 max consumption", y = "Average density") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank())
fig

# save plot in this folder
ggsave(filename = "newSA-prey2maxCons-density.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

######## New figures for extinction frequency ########

# Path = "/Users/adrianbach/Desktop/PhD/GitKraken/Chapter2model/localSA/"
# Path = "/home/adrian/Documents/GitKraken/ne"
Path = "C:/Users/adb3/Desktop/PhD/GitKraken/newLocalSA/"

#### average offspring nb ####
folderPath = paste(Path, "folder-avgOff/allStatsAndPlots/localSAfiles", sep = "")
filePath = paste(folderPath, "stats-folder-avgOff.csv", sep = "/")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

x = data$prey2avgOffs
y1 = data$prey1extFreq
y2 = data$prey2extFreq
y3 = data$pred1extFreq
# y1min = data$prey1PopulationSizeICinf
# y2min = data$prey2PopulationSizeICinf
# y3min = data$predator1PopulationSizeICinf
# y1max = data$prey1PopulationSizeICsup
# y2max = data$prey2PopulationSizeICsup
# y3max = data$predator1PopulationSizeICsup
y1c = "red"
y2c = "blue"
y3c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  ylim(0, 1) +
  # geom_rect(aes(xmin = 0.85, xmax = 1.15, ymin = 0, ymax = 1.05), alpha=0.5, fill = "lightgrey") +
  # geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
  # geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.10)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = 0)) +
  geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = 0.10)) +
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  geom_point(aes(y = y1), size = 3, shape = 21, fill = "white", color = y1c, position = position_nudge(x = -0.10)) +
  geom_point(aes(y = y2), size = 3, shape = 22, fill = "white", color = y2c) +
  geom_point(aes(y = y3), size = 3, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.10)) +
  labs(x = "Prey2 birth rate", y = "Extinction frequency") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank()) # +
# scale_colour_manual(name='Populations',
#                     breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                     values=c(y1c, y2c, y3c))
fig

# save plot in this folder
ggsave(filename = "newSA-prey2avgBR-extFreq.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

#### Catch proba ####
folderPath = paste(Path, "folder-ctchPr/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-ctchPr.csv", sep = "")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

x = data$prey2catchProb
y1 = data$prey1extFreq
y2 = data$prey2extFreq
y3 = data$pred1extFreq
# y1min = data$prey1PopulationSizeICinf
# y2min = data$prey2PopulationSizeICinf
# y3min = data$predator1PopulationSizeICinf
# y1max = data$prey1PopulationSizeICsup
# y2max = data$prey2PopulationSizeICsup
# y3max = data$predator1PopulationSizeICsup
y1c = "red"
y2c = "blue"
y3c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  ylim(0, 1) +
  # geom_rect(aes(xmin = 0.85, xmax = 1.15, ymin = 0, ymax = 1.05), alpha=0.5, fill = "lightgrey") +
  # geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
  # geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.01)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = 0)) +
  geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = 0.01)) +
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  geom_point(aes(y = y1), size = 3, shape = 21, fill = "white", color = y1c, position = position_nudge(x = -0.01)) +
  geom_point(aes(y = y2), size = 3, shape = 22, fill = "white", color = y2c) +
  geom_point(aes(y = y3), size = 3, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.01)) +
  labs(x = "Prey2 catch probability", y = "Extinction frequency") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank()) # +
# scale_colour_manual(name='Populations',
#                     breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                     values=c(y1c, y2c, y3c))
fig

# save plot in this folder
ggsave(filename = "newSA-prey2catchProba-extFreq.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

#### Conv rate ####
folderPath = paste(Path, "folder-cnvRte/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-cnvRte.csv", sep = "")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

x = data$prey2convRate
y1 = data$prey1extFreq
y2 = data$prey2extFreq
y3 = data$pred1extFreq
# y1min = data$prey1PopulationSizeICinf
# y2min = data$prey2PopulationSizeICinf
# y3min = data$predator1PopulationSizeICinf
# y1max = data$prey1PopulationSizeICsup
# y2max = data$prey2PopulationSizeICsup
# y3max = data$predator1PopulationSizeICsup
y1c = "red"
y2c = "blue"
y3c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  ylim(0, 1) +
  # geom_rect(aes(xmin = 0.85, xmax = 1.15, ymin = 0, ymax = 1.05), alpha=0.5, fill = "lightgrey") +
  # geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
  # geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -10)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = 0)) +
  geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = 10)) +
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  geom_point(aes(y = y1), size = 3, shape = 21, fill = "white", color = y1c, position = position_nudge(x = -10)) +
  geom_point(aes(y = y2), size = 3, shape = 22, fill = "white", color = y2c) +
  geom_point(aes(y = y3), size = 3, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 10)) +
  labs(x = "Prey2 resources/catch", y = "Extinction frequency") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank()) # +
# scale_colour_manual(name='Populations',
#                     breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                     values=c(y1c, y2c, y3c))
fig

# save plot in this folder
ggsave(filename = "newSA-prey2ConvRate-extFreq.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

#### resources available ####
folderPath = paste(Path, "folder-resAva/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-resAva.csv", sep = "")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

# data <- data[-1,]

x = data$prey2resAva
y1 = data$prey1extFreq
y2 = data$prey2extFreq
y3 = data$pred1extFreq
# y1min = data$prey1PopulationSizeICinf
# y2min = data$prey2PopulationSizeICinf
# y3min = data$predator1PopulationSizeICinf
# y1max = data$prey1PopulationSizeICsup
# y2max = data$prey2PopulationSizeICsup
# y3max = data$predator1PopulationSizeICsup
y1c = "red"
y2c = "blue"
y3c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  ylim(0, 1) +
  # geom_rect(aes(xmin = 0.85, xmax = 1.15, ymin = 0, ymax = 1.05), alpha=0.5, fill = "lightgrey") +
  # geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
  # geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -10)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = 0)) +
  geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = 10)) +
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  geom_point(aes(y = y1), size = 3, shape = 21, fill = "white", color = y1c, position = position_nudge(x = -10)) +
  geom_point(aes(y = y2), size = 3, shape = 22, fill = "white", color = y2c) +
  geom_point(aes(y = y3), size = 3, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 10)) +
  labs(x = "Prey2 max resources available", y = "Extinction frequency") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank()) # +
# scale_colour_manual(name='Populations',
#                     breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                     values=c(y1c, y2c, y3c))
fig

# save plot in this folder
ggsave(filename = "newSA-prey2maxRes-extFreq.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

#### max cons ####
folderPath = paste(Path, "folder-maxCon/allStatsAndPlots/localSAfiles/", sep = "")
filePath = paste(folderPath, "stats-folder-maxCon.csv", sep = "")

data <- read.csv(filePath)
# data <- subset(data, data$predSpecific == 0 & data$predOportunistic == 0)

x = data$prey2maxCons
y1 = data$prey1extFreq
y2 = data$prey2extFreq
y3 = data$pred1extFreq
# y1min = data$prey1PopulationSizeICinf
# y2min = data$prey2PopulationSizeICinf
# y3min = data$predator1PopulationSizeICinf
# y1max = data$prey1PopulationSizeICsup
# y2max = data$prey2PopulationSizeICsup
# y3max = data$predator1PopulationSizeICsup
y1c = "red"
y2c = "blue"
y3c = "orange"
# tIntro = 210

fig <- ggplot(data, aes(x)) + 
  ylim(0, 1) +
  # geom_rect(aes(xmin = 0.85, xmax = 1.15, ymin = 0, ymax = 1.05), alpha=0.5, fill = "lightgrey") +
  # geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
  # geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
  # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -1)) +
  geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = 0)) +
  geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = 1)) +
  # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
  geom_point(aes(y = y1), size = 3, shape = 21, fill = "white", color = y1c, position = position_nudge(x = -1)) +
  geom_point(aes(y = y2), size = 3, shape = 22, fill = "white", color = y2c) +
  geom_point(aes(y = y3), size = 3, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 1)) +
  labs(x = "Prey2 maximum consumption", y = "Extinction frequency") +
  scale_x_continuous(breaks = x, labels = x) + 
  theme(panel.grid.minor = element_blank()) # +
# scale_colour_manual(name='Populations',
#                     breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                     values=c(y1c, y2c, y3c))
fig

# save plot in this folder
ggsave(filename = "newSA-prey2maxCons-extFreq.pdf", path = folderPath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

######## function ########

plotDens <- function(dataSet, xVal, recWdt = c(0,0), posNud, xLabel, display = c(TRUE, FALSE), saveName = "densFig.pdf", savePath, save = c(TRUE, FALSE)) {
  
  x = xVal
  y1 = dataSet$prey1densBeforeMean
  y2 = dataSet$prey1densAfterMean
  y3 = dataSet$prey2densBeforeMean
  y4 = dataSet$prey2densAfterMean
  y5 = dataSet$predatorDensMean
  y1min = dataSet$prey1densBeforeMin
  y2min = dataSet$prey1densAfterMin
  y3min = dataSet$prey2densBeforeMin
  y4min = dataSet$prey2densAfterMin
  y5min = dataSet$predatorDensMin
  y1max = dataSet$prey1densBeforeMax
  y2max = dataSet$prey1densAfterMax
  y3max = dataSet$prey2densBeforeMax
  y4max = dataSet$prey2densAfterMax
  y5max = dataSet$predatorDensMax
  y1c = "pink"
  y2c = "red"
  y3c = "cyan"
  y4c = "blue"
  y5c = "orange"
  # tIntro = 210
  
  fig <- ggplot(data, aes(x)) + 
    # geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") +
    # geom_hline(yintercept = -1, color = "darkred", linetype = "dashed") +
    # geom_hline(yintercept = y3[1], alpha = 0.5, color = "darkblue", linetype = "dashed") +
    # geom_vline(xintercept = 1, alpha = 0.5, color = "black", linetype = "dashed") +
    geom_rect(aes(xmin = recWdt[1], xmax = recWdt[2], ymin = 0, ymax = 1.05*max(max(y2max), max(y4max), max(y1))), alpha=0.5, fill = "lightgrey") +
    geom_hline(yintercept = y1[length(y1)], alpha = 0.5, color = "black", linetype = "dashed") +
    # geom_line(aes(y = y1), color = y1c, alpha = 0.2, position = position_nudge(x = -0.2)) +
    geom_line(aes(y = y2), color = y2c, alpha = 0.2, position = position_nudge(x = -posNud)) +
    # geom_line(aes(y = y3), color = y3c, alpha = 0.2, position = position_nudge(x = -0.05)) +
    geom_line(aes(y = y4), color = y4c, alpha = 0.2, position = position_nudge(x = 0.00)) +
    geom_line(aes(y = y5), color = y5c, alpha = 0.2, position = position_nudge(x = posNud)) +
    # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(data$prey2PopulationSizeMean)), alpha=0.5, fill = "lightgrey") +
    # geom_pointrange(aes(y = y1, ymin = y1min, ymax = y1max), shape = 21, fill = "white", size = 0.5, col = y1c, position = position_nudge(x = -0.2)) +
    geom_pointrange(aes(y = y2, ymin = y2min, ymax = y2max), shape = 21, fill = "white", size = 0.4, col = y2c, position = position_nudge(x = -posNud)) +
    # geom_pointrange(aes(y = y3, ymin = y3min, ymax = y3max), shape = 22, fill = "white", size = 0.4, col = y3c, position = position_nudge(x = -0.05)) +
    geom_pointrange(aes(y = y4, ymin = y4min, ymax = y4max), shape = 22, fill = "white", size = 0.4, col = y4c, position = position_nudge(x = 0.00)) +
    geom_pointrange(aes(y = y5, ymin = y5min, ymax = y5max), shape = 24, fill = "white", size = 0.4, col = y5c, position = position_nudge(x = posNud)) +
    # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
    # geom_point(aes(y = y1), size = 2.5, shape = 24, fill = "white", color = y1c, position = position_nudge(x = -0.1)) +
    # geom_point(aes(y = y2), size = 2.5, shape = 25, fill = "white", color = y2c, position = position_nudge(x = 0.1)) # +
    # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c, position = position_nudge(x = 0.15)) +
    labs(x = xLabel, y = "Average density") +
    scale_x_continuous(breaks = x, labels = x) + 
    theme(panel.grid.minor = element_blank())
  
  # save plot in this folder
  if (save == TRUE) {
    ggsave(filename = saveName, path = savePath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
  }
  
  if (display == TRUE) {return(fig)}
}

######## excluding extinction ########

# Path = "/Users/adrianbach/Desktop/PhD/GitKraken/Chapter2model/localSA/"
# Path = "/home/adrian/Documents/GitKraken/ne"
Path = "C:/Users/adb3/Desktop/PhD/GitKraken/newLocalSA/"

# available resources
# folderPath = paste(Path, "folder-resAva/allStatsAndPlots/localSAfiles/", sep = "")
# filePath = paste(folderPath, "stats-folder-resAva.csv", sep = "")
# 
# data <- read.csv(filePath)
# 
# plotDens(dataSet = data, xVal = data$prey2resAva, recWdt = c(90, 110), posNud = 7, xLabel = "prey 2 maximum resources available", display = TRUE, saveName = "plotFunTest.pdf", savePath = folderPath, save = TRUE)

folderPath = paste(Path, "folder-resAva/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-stats-folder-resAva.csv", sep = "")

data <- read.csv(filePath)

plotDens(dataSet = data, xVal = data$prey2resAva, recWdt = c(93, 107), posNud = 5, xLabel = "prey 2 maximum resources available", display = TRUE, saveName = "newSA-prey2maxRes-density-woExt.pdf", savePath = folderPath, save = TRUE)

# avgoff
folderPath = paste(Path, "folder-avgOff/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-stats-folder-avgOff.csv", sep = "")

data <- read.csv(filePath)

plotDens(dataSet = data, xVal = data$prey2avgOffs, recWdt = c(0.75, 1.35), posNud = 0.20, xLabel = "prey 2 birth rate", display = TRUE, saveName = "newSA-prey2avgOff-density-woExt.pdf", savePath = folderPath, save = TRUE)

# cnvRte
folderPath = paste(Path, "folder-cnvRte/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-stats-folder-cnvRte.csv", sep = "")

data <- read.csv(filePath)

plotDens(dataSet = data, xVal = data$prey2convRate, recWdt = c(93, 107), posNud = 5, xLabel = "prey 2 resources/catch", display = TRUE, saveName = "newSA-prey2cnvRte-density-woExt.pdf", savePath = folderPath, save = TRUE)

# ctchPr
folderPath = paste(Path, "folder-ctchPr/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-stats-folder-ctchPr.csv", sep = "")

data <- read.csv(filePath)

plotDens(dataSet = data, xVal = data$prey2catchProb, recWdt = c(0.09, 0.11), posNud = 0.005, xLabel = "prey 2 catch probability", display = TRUE, saveName = "newSA-prey2ctchPr-density-woExt.pdf", savePath = folderPath, save = TRUE)

# maxCon
folderPath = paste(Path, "folder-maxCon/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-stats-folder-maxCon.csv", sep = "")

data <- read.csv(filePath)

plotDens(dataSet = data, xVal = data$prey2maxCons, recWdt = c(9, 11), posNud = 0.5, xLabel = "prey 2 maximum consumption", display = TRUE, saveName = "newSA-prey2maxCon-density-woExt.pdf", savePath = folderPath, save = TRUE)

######## SA dev plots ########

plotSADensDev <- function(dataSet, xVal, xLabel, wd, display = c(TRUE, FALSE), saveName = "densFig.pdf", savePath, save = c(TRUE, FALSE)) {
  
  x = xVal
  # baseIndex = which(xVal == xBase)
  # dataSet$densityDevMean[baseIndex] = 0
  # dataSet$densDevICinf[baseIndex] = 0
  # dataSet$densDevICsup[baseIndex] = 0
  
  fig <- ggplot(dataSet, aes(x=x, y=densityDevMean, colour="red")) + 
    geom_hline(yintercept = 0, alpha = 0.5, color = "black", linetype = "dashed") +
    geom_errorbar(aes(ymin=densDevICinf, ymax=densDevICsup), width=wd, col="darkred") +
    geom_line(alpha = 0.2) +
    geom_point(shape = 21, size = 2.5, fill = "white") +
    labs(x = xLabel, y = "Deviation in prey 1 equilibrium density") +
    scale_x_continuous(breaks = x, labels = x) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none")
  
  # save plot in this folder
  if (save == TRUE) {
    ggsave(filename = saveName, path = savePath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
  }
  
  if (display == TRUE) {return(fig)}
}

plotSAamplDev <- function(dataSet, xVal, xLabel, wd, display = c(TRUE, FALSE), saveName = "densFig.pdf", savePath, save = c(TRUE, FALSE)) {
  
  x = xVal
  # baseIndex = which(xVal == xBase)
  # dataSet$amplitudeDevMean[baseIndex] = 0
  # dataSet$ampldevICinf[baseIndex] = 0
  # dataSet$ampldevICsup[baseIndex] = 0
  
  fig <- ggplot(dataSet, aes(x=x, y=amplitudeDevMean, colour="red")) + 
    geom_hline(yintercept = 0, alpha = 0.5, color = "black", linetype = "dashed") +
    geom_errorbar(aes(ymin=ampldevICinf, ymax=ampldevICsup), width=wd, col="darkred") +
    geom_line(alpha = 0.2) +
    geom_point(shape = 21, size = 2.5, fill = "white") +
    labs(x = xLabel, y = "Deviation in prey 1 density oscillations range") +
    scale_x_continuous(breaks = x, labels = x) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none")
  
  # save plot in this folder
  if (save == TRUE) {
    ggsave(filename = saveName, path = savePath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
  }
  
  if (display == TRUE) {return(fig)}
}

# Path = "/home/adrian/Documents/GitKraken/Chapter2model/localSA/"
Path = "C:/Users/adb3/Desktop/PhD/GitKraken/newLocalSA/"

# # get base mean values
# ds <- read.csv(file = paste(Path, "/folder-avgOff/allStatsAndPlots/localSAfiles-woExt/woExt-stats-folder-avgOff.csv", sep = ""))
# BaseMeanDens = ds$prey1densAfterMean[which(ds$prey2avgOffs == 1)]
# BaseMeanAmplitude = ds$prey1densAfterMax[which(ds$prey2avgOffs == 1)]-ds$prey1densAfterMin[which(ds$prey2avgOffs == 1)]

# avgOff
folderPath = paste(Path, "folder-avgOff/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-SAdevStats-folder-avgOff.csv", sep = "")

data <- read.csv(filePath)

plotSADensDev(dataSet = data, xVal = data$prey2avgOffs, xLabel = "prey 2 birth rate", wd = 0.2, display = TRUE, saveName = "newSA-densityDev-prey2avgOff-woExt.pdf", savePath = folderPath, save = TRUE)
plotSAamplDev(dataSet = data, xVal = data$prey2avgOffs, xLabel = "prey 2 birth rate", wd = 0.2, display = TRUE, saveName = "newSA-amplitudeDev-prey2avgOff-woExt.pdf", savePath = folderPath, save = TRUE)

# cnvRte
folderPath = paste(Path, "folder-cnvRte/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-SAdevStats-folder-cnvRte.csv", sep = "")

data <- read.csv(filePath)

plotSADensDev(dataSet = data, xVal = data$prey2convRate, xLabel = "prey 2 resources per catch", wd = 2.5, display = TRUE, saveName = "newSA-densityDev-prey2cnvRte-woExt.pdf", savePath = folderPath, save = TRUE)
plotSAamplDev(dataSet = data, xVal = data$prey2convRate, xLabel = "prey 2 resources per catch", wd = 2.5, display = TRUE, saveName = "newSA-amplitudeDev-prey2avgOff-woExt.pdf", savePath = folderPath, save = TRUE)

# ctchPr
folderPath = paste(Path, "folder-ctchPr/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-SAdevStats-folder-ctchPr.csv", sep = "")

data <- read.csv(filePath)

plotSADensDev(dataSet = data, xVal = data$prey2catchProb, xLabel = "prey 2 catchProbability", wd = 0.005, display = TRUE, saveName = "newSA-densityDev-prey2ctchPr-woExt.pdf", savePath = folderPath, save = TRUE)
plotSAamplDev(dataSet = data, xVal = data$prey2catchProb, xLabel = "prey 2 catchProbability", wd = 0.005, display = TRUE, saveName = "newSA-amplitudeDev-prey2ctchPr-woExt.pdf", savePath = folderPath, save = TRUE)

# maxCon
folderPath = paste(Path, "folder-maxCon/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-SAdevStats-folder-maxCon.csv", sep = "")

data <- read.csv(filePath)

plotSADensDev(dataSet = data, xVal = data$prey2maxCons, xLabel = "prey 2 max consumption", wd = 0.5, display = TRUE, saveName = "newSA-densityDev-prey2maxCon-woExt.pdf", savePath = folderPath, save = TRUE)
plotSAamplDev(dataSet = data, xVal = data$prey2maxCons, xLabel = "prey 2 max consumption", wd = 0.5, display = TRUE, saveName = "newSA-amplitudeDev-prey2maxCon-woExt.pdf", savePath = folderPath, save = TRUE)

# resAva
folderPath = paste(Path, "folder-resAva/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-SAdevStats-folder-resAva.csv", sep = "")

data <- read.csv(filePath)

plotSADensDev(dataSet = data, xVal = data$prey2resAva, xLabel = "prey 2 max resources available", wd = 2, display = TRUE, saveName = "newSA-densityDev-prey2resAva-woExt.pdf", savePath = folderPath, save = TRUE)
plotSAamplDev(dataSet = data, xVal = data$prey2resAva, xLabel = "prey 2 max resources available", wd = 2, display = TRUE, saveName = "newSA-amplitudeDev-prey2resAva-woExt.pdf", savePath = folderPath, save = TRUE)
