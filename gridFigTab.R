library(ggplot2)
library(stringr)

folder="C:/Users/adb3/Desktop/PhD/GitKraken/NewWorkingDirectory/nullCalibNarrow/allStatsAndPlots/ResultsFiles"

# get content
simFol = list.files(folder)

# only csv files
results <- grep(pattern = c(".csv"), x = simFol, value = T)

# only two preys files
results <- grep(pattern = c("py1Init100"), x = results, value = T)

paramNames = c("prdSurv", "prdCtPr")

tab <- NULL

for (i in 1:length(results)) {
  
  # get param values and nb of rep
  strg = results[i]
  
  strg = sub(x = strg, pattern = "*.csv", replacement = "")   # cut ".csv" out
  strg = unlist(strsplit(strg, split = "-")) # split according to "-"
  strg = strg[-c(1: 3)] # take out non param elements
  
  p1 = as.numeric(sub(x = strg[which(str_detect(string = strg, pattern = paramNames[1]))], pattern = paste(paramNames[1], "*", sep = ""), replacement = ""))
  p2 = as.numeric(sub(x = strg[which(str_detect(string = strg, pattern = paramNames[2]))], pattern = paste(paramNames[2], "*", sep = ""), replacement = ""))
  
  name = paste(folder, results[i], sep="/")
  
  res = read.csv(name)
  
  nbRep = length(res$repNb)
  
  p1vec = rep(p1, nbRep)
  p2vec = rep(p2, nbRep)
  
  temp <- cbind(p1vec, p2vec, res)
  
  tab <- rbind(tab, temp)
}

write.csv(tab, file = paste(folder, "/nullCalibNarrow-crossedTable-Results.csv", sep = ""), row.names = FALSE)

x = tab$timeStep
y1 = tab$prey1PopulationSizeMean
y2 = tab$prey2PopulationSizeMean
y3 = tab$predator1PopulationSizeMean
y1min = tab$prey1PopulationSizeICinf
y2min = tab$prey2PopulationSizeICinf
y3min = tab$predator1PopulationSizeICinf
y1max = tab$prey1PopulationSizeICsup
y2max = tab$prey2PopulationSizeICinf
y3max = tab$predator1PopulationSizeICsup
y1c = "red"
y2c = "blue"
y3c = "orange"
tIntro = 200

fig <- ggplot(tab, aes(x)) + 
  geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(y1max)), alpha=0.5, fill = "lightgrey") +
  geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
  geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
  geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
  geom_line(aes(y = y1), color = y1c, size = 0.5) +
  geom_line(aes(y = y2), color = y2c, size = 0.5) +
  geom_line(aes(y = y3), color = y3c)

ggp <- fig + facet_grid(tab$p1vec~tab$p2vec) +
  labs(x = "Time steps", y = "Mean population size")

# save plot in this folder
ggsave(filename = "nullCalibNarrow-density.pdf", path = folder, plot = ggp, width = 6.22, height = 5.73, limitsize = TRUE)
