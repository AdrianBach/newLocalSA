folderPath = paste(Path, "folder-avgOff/allStatsAndPlots/localSAfiles-woExt/", sep = "")
filePath = paste(folderPath, "woExt-allSAdevStats-allPopfolder-avgOff.csv", sep = "")

data <- read.csv(filePath)

dataSet = data
xVal = dataSet$prey2avgOffs
xLabel = "Prey 2 fertility"
wd = 0.1
posNud = 0.1
ptSize = 2.5

plotSADensDevAllPop <- function(dataSet, xVal, xLabel, wd, posNud, display = c(TRUE, FALSE), saveName = "densDevFig.pdf", savePath, save = c(TRUE, FALSE)) {
  
  x = xVal
  y1 = dataSet$prey1densityDevMean
  y2 = dataSet$prey2densityDevMean
  y3 = dataSet$pred1densityDevMean
  y1min = dataSet$prey1densDevICinf
  y2min = dataSet$prey2densDevICinf
  y3min = dataSet$pred1densDevICinf
  y1max = dataSet$prey1densDevICsup
  y2max = dataSet$prey2densDevICsup
  y3max = dataSet$pred1densDevICsup
  y1c = "red"
  y2c = "blue"
  y3c = "orange"
  
  fig <- ggplot(dataSet, aes(x=x)) + 
    geom_hline(yintercept = 0, alpha = 0.5, color = "black", linetype = "dashed") +
    geom_line(aes(y = y1), color = y1c, size=1, alpha = 0.2, position = position_nudge(x = -posNud)) +
    geom_line(aes(y = y2), color = y2c, size=1, alpha = 0.2, position = position_nudge(x = 0)) +
    geom_line(aes(y = y3), color = y3c, size=1, alpha = 0.2, position = position_nudge(x = posNud)) +
    geom_errorbar(aes(ymin=y1min, ymax=y1max), width=wd, col=y1c, position = position_nudge(x = -posNud)) +
    geom_errorbar(aes(ymin=y2min, ymax=y2max), width=wd, col=y2c, position = position_nudge(x = 0)) +
    geom_errorbar(aes(ymin=y3min, ymax=y3max), width=wd, col=y3c, position = position_nudge(x = posNud)) +
    geom_point(aes(y=y1), shape=21, size=2.5, fill="white", col=y1c, position = position_nudge(x = -posNud)) +
    geom_point(aes(y=y2), shape=22, size=2.5, fill="white", col=y2c, position = position_nudge(x = 0)) +
    geom_point(aes(y=y3), shape=24, size=2.5, fill="white", col=y3c, position = position_nudge(x = posNud)) +
    labs(x = xLabel, y = "Deviation in final density") +
    scale_x_continuous(breaks = x, labels = x) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none")
  
  # save plot in this folder
  if (save == TRUE) {
    ggsave(filename = saveName, path = savePath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
  }
  
  if (display == TRUE) {return(fig)}
}

plotSACtRtDevAllPop <- function(dataSet, xVal, xLabel, wd, posNud, display = c(TRUE, FALSE), saveName = "densDevFig.pdf", savePath, save = c(TRUE, FALSE)) {
  
  x = xVal
  y1 = dataSet$prey1catchRtDevMean
  y2 = dataSet$prey2catchRtDevMean
  # y3 = dataSet$pred1c
  y1min = dataSet$prey1ctRtDevICinf
  y2min = dataSet$prey2ctRtDevICinf
  # y3min = dataSet$pred1densDevICinf
  y1max = dataSet$prey1ctRtDevICsup
  y2max = dataSet$prey2ctRtDevICsup
  # y3max = dataSet$pred1densDevICsup
  y1c = "red"
  y2c = "blue"
  # y3c = "orange"
  
  fig <- ggplot(dataSet, aes(x=x)) + 
    geom_hline(yintercept = 0, alpha = 0.5, color = "black", linetype = "dashed") +
    geom_line(aes(y = y1), color = y1c, size=1, alpha = 0.2, position = position_nudge(x = -posNud)) +
    geom_line(aes(y = y2), color = y2c, size=1, alpha = 0.2, position = position_nudge(x = posNud)) +
    # geom_line(aes(y = y3), color = y3c, size=1, alpha = 0.2, position = position_nudge(x = posNud)) +
    geom_errorbar(aes(ymin=y1min, ymax=y1max), width=wd, col=y1c, position = position_nudge(x = -posNud)) +
    geom_errorbar(aes(ymin=y2min, ymax=y2max), width=wd, col=y2c, position = position_nudge(x = posNud)) +
    # geom_errorbar(aes(ymin=y3min, ymax=y3max), width=wd, col=y3c, position = position_nudge(x = posNud)) +
    geom_point(aes(y=y1), shape=21, size=2.5, fill="white", col=y1c, position = position_nudge(x = -posNud)) +
    geom_point(aes(y=y2), shape=22, size=2.5, fill="white", col=y2c, position = position_nudge(x = posNud)) +
    # geom_point(aes(y=y3), shape=24, size=2.5, fill="white", col=y3c, position = position_nudge(x = posNud)) +
    labs(x = xLabel, y = "Deviation in final density") +
    scale_x_continuous(breaks = x, labels = x) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none")
  
  # save plot in this folder
  if (save == TRUE) {
    ggsave(filename = saveName, path = savePath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
  }
  
  if (display == TRUE) {return(fig)}
}

plotSAamplDevAllPop <- function(dataSet, xVal, xLabel, wd, posNud, ptSize, display = c(TRUE, FALSE), saveName = "densDevFig.pdf", savePath, save = c(TRUE, FALSE)) {
  
  x = xVal
  y1 = dataSet$prey1amplituDevMean
  y2 = dataSet$prey2amplituDevMean
  y3 = dataSet$pred1amplituDevMean
  y1min = dataSet$prey1ampldevICinf
  y2min = dataSet$prey2ampldevICinf
  y3min = dataSet$pred1ampldevICinf
  y1max = dataSet$prey1ampldevICsup
  y2max = dataSet$prey2ampldevICsup
  y3max = dataSet$pred1ampldevICsup
  y1c = "red"
  y2c = "blue"
  y3c = "orange"
  
  fig <- ggplot(dataSet, aes(x=x)) + 
    geom_hline(yintercept = 0, alpha = 0.5, color = "black", linetype = "dashed") +
    geom_line(aes(y = y1), color = y1c, size=1, alpha = 0.2, position = position_nudge(x = -posNud)) +
    geom_line(aes(y = y2), color = y2c, size=1, alpha = 0.2, position = position_nudge(x = 0)) +
    geom_line(aes(y = y3), color = y3c, size=1, alpha = 0.2, position = position_nudge(x = posNud)) +
    geom_errorbar(aes(ymin=y1min, ymax=y1max), width=wd, col=y1c, position = position_nudge(x = -posNud)) +
    geom_errorbar(aes(ymin=y2min, ymax=y2max), width=wd, col=y2c, position = position_nudge(x = 0)) +
    geom_errorbar(aes(ymin=y3min, ymax=y3max), width=wd, col=y3c, position = position_nudge(x = posNud)) +
    geom_point(aes(y=y1), shape=21, size=ptSize, fill="white", col=y1c, position = position_nudge(x = -posNud)) +
    geom_point(aes(y=y2), shape=22, size=ptSize, fill="white", col=y2c, position = position_nudge(x = 0)) +
    geom_point(aes(y=y3), shape=24, size=ptSize, fill="white", col=y3c, position = position_nudge(x = posNud)) +
    labs(x = xLabel, y = "Deviation in final density") +
    scale_x_continuous(breaks = x, labels = x) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none")
  
  # save plot in this folder
  if (save == TRUE) {
    ggsave(filename = saveName, path = savePath, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
  }
  
  if (display == TRUE) {return(fig)}
}

SAallResults <- function(path, keyword = c("Results", "Snapshot"), pattern) { # , baseMeanDens, baseAmplitude
  
  # get the directory content
  # content <- list.files(paste("~/", path, sep = ""))
  content <- list.files(path)
  
  # order alphabetically
  content <- content[order(content)]
  
  # only the folders
  content <- grep(pattern = c("folder"), x = content, value = T)
  
  # create headers list 
  headers <- c("prey2avgOffs", "prey2convRate", "prey2catchProb", "prey2maxCons", "prey2resAva", 
               "replicatesNb",
               # "prey1densBeforeMean", "prey1densBeforeMax", "prey1densBeforeMin",
               # "prey2densBeforeMean", "prey2densBeforeMax", "prey2densBeforeMin",
               # "prey1growthBeforeMean", "prey1growthBeforeMax", "prey1growthBeforeMin",
               # "prey2growthBeforeMean", "prey2growthBeforeMax", "prey2growthBeforeMin",
               # "prey1densAfterMean", "prey1densAfterMax", "prey1densAfterMin",
               # "prey2densAfterMean", "prey2densAfterMax", "prey2densAfterMin",
               # "predatorDensMean", "predatorDensMax", "predatorDensMin",
               # "prey1growthAfterMean", "prey1growthAfterMax", "prey1growthAfterMin",
               # "prey2growthAfterMean", "prey2growthAfterMax", "prey2growthAfterMin",
               # "predatorGrowthMean", "predatorGrowthMax", "predatorGrowthMin",
               # "prey1catchesMean", "prey1catchesMax", "prey1catchesMin",
               # "prey2catchesMean", "prey2catchesMax", "prey2catchesMin",
               "prey1densityDevMean", "prey1densDevICinf", "prey1densDevICsup", "prey1densDevMin", "prey1densDevMax",
               "prey2densityDevMean", "prey2densDevICinf", "prey2densDevICsup", "prey2densDevMin", "prey2densDevMax",
               "pred1densityDevMean", "pred1densDevICinf", "pred1densDevICsup", "pred1densDevMin", "pred1densDevMax",
               "prey1amplituDevMean", "prey1ampldevICinf", "prey1ampldevICsup",
               "prey2amplituDevMean", "prey2ampldevICinf", "prey2ampldevICsup",
               "pred1amplituDevMean", "pred1ampldevICinf", "pred1ampldevICsup",
               "prey1catchRtDevMean", "prey1ctRtDevICinf", "prey1ctRtDevICsup", "prey1catchRtMin", "prey1densDevMax",
               "prey2catchRtDevMean", "prey2ctRtDevICinf", "prey2ctRtDevICsup", "prey2catchRtMin", "prey2densDevMax")
  
  # set before after intervals
  # tIntro = 210
  tEnd = 1000
  # before <- c(tIntro-100, tIntro)
  after <- c(tEnd-250, tEnd)
  
  # loop over the folders
  for (i in 1:length(content)) {
    
    # path to folder
    # folder = paste("~/", path, content[i], sep = "")
    folder = paste(path, content[i], sep = "")
    print(paste("in localSA folder ", i, ": ", folder))# get the value of XparamName
    
    tab <- data.frame(matrix(ncol = length(headers), nrow = 0))
    
    # # name columns
    # colnames(tab) <- headers
    
    # get content
    globalFol = list.files(folder)
    
    # select only folders
    globalFol <- grep(pattern = c(pattern), x = globalFol, value = T)
    
    ## compute baseMeanDens and Ampl
    # find baseline param
    simNames = c("avgOff", "cnvRte", "ctchPr", "maxCon", "resAva")
    varNames = c("py2offs", "py2cvRt", "py2ctPr", "py2cons", "py2res")
    baseValues = c(1, 100, 0.1, 10, 100)
    
    # in which case are we
    baseIndex = which(str_detect(string = content[i], pattern = simNames))
    baseNam = varNames[baseIndex]
    baseVal = baseValues[baseIndex]
    
    # open base folder
    baseFol <- grep(pattern = c(paste(baseNam, baseVal, sep = "")), x = globalFol, value = T)[1]
    
    baseFolPath <- paste(folder, baseFol, sep = "/")
    
    # get results files
    statsFol <- list.files(baseFolPath)
    
    # find stats folder and store path
    statsFol <- grep(pattern = c("stats"), x = statsFol, value = T)
    print(paste("in", paste(baseFolPath, statsFol, sep = "/")))
    
    results <- list.files(paste(baseFolPath, statsFol, sep = "/"))
    
    # only csv files
    results <- grep(pattern = c("merged"), x = results, value = T)
    
    # only the results files
    results <- grep(pattern = c(keyword), x = results, value = T)
    
    # read the merged results file
    res <- read.csv(paste(baseFolPath, statsFol, results, sep = "/"))
    
    replicates <- levels(as.factor(res$replicate))
    densDevPrey1 <- NULL
    amplDevPrey1 <- NULL
    densDevPrey2 <- NULL
    amplDevPrey2 <- NULL
    densDevPred1 <- NULL
    amplDevPred1 <- NULL
    ctRtDevPrey1 <- NULL
    ctRtDevPrey2 <- NULL
    
    for (n in 1:length(replicates)) {
      # subset replicate
      sub <- subset(res, res$replicate == replicates[n])
      
      if (sub$prey1PopulationSize[dim(sub)[1]] == 0 | sub$prey2PopulationSize[dim(sub)[1]] == 0 | sub$predator1PopulationSize[dim(sub)[1]] == 0) {
        # print(paste("replicate", n, "ended in extinction"))
        
      } else {
        
        # calculus
        # subset after
        subAfter <- subset(sub, subset = sub$timeStep >= after[1] & sub$timeStep <= after[2])
        
        # compute dev from base line
        densDevPrey1 <- c(densDevPrey1, mean(subAfter$prey1PopulationSize))
        
        # compute amplitude dev
        amplDevPrey1 <- c(amplDevPrey1, max(subAfter$prey1PopulationSize) - min(subAfter$prey1PopulationSize)) #  - 1)
        
        # compute dev from base line
        densDevPrey2 <- c(densDevPrey2, mean(subAfter$prey2PopulationSize))
        
        # compute amplitude dev
        amplDevPrey2 <- c(amplDevPrey2, max(subAfter$prey2PopulationSize) - min(subAfter$prey2PopulationSize)) #  - 1)
        
        # compute dev from base line
        densDevPred1 <- c(densDevPred1, mean(subAfter$predator1PopulationSize))
        
        # compute amplitude dev
        amplDevPred1 <- c(amplDevPred1, max(subAfter$predator1PopulationSize) - min(subAfter$predator1PopulationSize)) #  - 1)
        
        # compute catch rate dev from base line
        ctRtDevPrey1 <- c(ctRtDevPrey1, mean(subAfter$prey1catches/subAfter$prey1PopulationSize))
        
        # compute catch rate dev
        ctRtDevPrey2 <- c(ctRtDevPrey2, mean(subAfter$prey2catches/subAfter$prey2PopulationSize)) #  - 1)
      }
    }
    
    # perform stats
    baseMeanDensPrey1 = mean(densDevPrey1)
    baseAmplitudPrey1 = mean(amplDevPrey1)
    baseMeanDensPrey2 = mean(densDevPrey2)
    baseAmplitudPrey2 = mean(amplDevPrey2)
    baseMeanDensPred1 = mean(densDevPred1)
    baseAmplitudPred1 = mean(amplDevPred1)
    baseMeanCtRtPrey1 = mean(ctRtDevPrey1)
    baseMeanCtRtPrey2 = mean(ctRtDevPrey2)
    
    # loop over the sim folders
    for (k in 1:length(globalFol)) {
      
      # get results folder
      resFol <- paste(folder, globalFol[k], sep = "/")
      # print(paste("in", resFol))
      # 
      # # find stats folder and store path
      # 
      # get results files
      statsFol <- list.files(resFol)
      
      # find stats folder and store path
      statsFol <- grep(pattern = c("stats"), x = statsFol, value = T)
      print(paste("in", paste(resFol, statsFol, sep = "/")))
      
      results <- list.files(paste(resFol, statsFol, sep = "/"))
      
      # only csv files
      results <- grep(pattern = c("merged"), x = results, value = T)
      
      # only the results files
      results <- grep(pattern = c(keyword), x = results, value = T)
      
      # print(paste("in simFolder", globalFol[k]))
      
      # initiate new line
      newLine <- NULL
      
      # get param values and nb of rep
      # get param values and nb of rep
      strg = globalFol[k]
      
      # strg = sub(x = strg, pattern = "*.csv", replacement = "")   # cut ".csv" out
      strg = unlist(strsplit(strg, split = "-")) # split according to "-"
      strg = strg[-1] # take out non param elements
      # strg = strg[-c(1, 2, 3, 4)] # take out non param elements
      
      # varNames = c("py2offs", "py2cvRt", "py2ctPr", "py2cons", "py2res")
      # baseValues = c(1, 100, 0.1, 10, 100)
      
      # get param values 
      for (m in 1:length(varNames)) {
        newLine <- c(newLine, ifelse(str_detect(string = strg, pattern = varNames[m]), as.numeric(sub(x = strg, pattern = paste(varNames[m], "*", sep = ""), replacement = "")), baseValues[m]))
      }
      
      
      for (j in 1:length(results)) {
        
        # # initiate extinctions counters
        # pry1ext = 0
        # pry2ext = 0
        # predExt = 0
        # 
        # # loop over the results files
        # for (m in 1:length(results)) {
        #   
        #   # print(paste("reading file ", results[m]))
        #   # read results
        #   res <- read.csv(paste(resFol, results[m], sep = "/"))
        #   
        #   # count extinctions
        #   if (res[dim(res)[1], 4] == 0) {pry1ext = pry1ext+1}
        #   if (res[dim(res)[1], 5] == 0) {pry2ext = pry2ext+1}
        #   if (res[dim(res)[1], 8] == 0) {predExt = predExt+1}
        #   
        # } # end loop over files (replicates)
        # 
        # # update new line
        # newLine <- c(newLine, pry1ext/length(results), pry2ext/length(results), predExt/length(results))
        
        # # browse stats folder and read stats
        # stats <- list.files(paste(resFol, statsFol, sep = "/"))
        # stats <- grep(pattern = c("stats"), x = stats, value = T)
        # stats <- grep(pattern = c(".csv"), x = stats, value = T)
        
        res <- read.csv(paste(resFol, statsFol, results[j], sep = "/"))
        
        # # create temporary table
        # temp <- data.frame(matrix(ncol = length(newHeaders), nrow = 0))
        
        # subset merged results and move to temp only if no extinction
        replicates <- levels(as.factor(res$replicate))
        repNb = 0
        densDevPrey1 <- NULL
        amplDevPrey1 <- NULL
        densDevPrey2 <- NULL
        amplDevPrey2 <- NULL
        densDevPred1 <- NULL
        amplDevPred1 <- NULL
        ctRtDevPrey1 <- NULL
        ctRtDevPrey2 <- NULL
        
        for (n in 1:length(replicates)) {
          # subset replicate
          sub <- subset(res, res$replicate == replicates[n])
          
          if (sub$prey1PopulationSize[dim(sub)[1]] == 0 | sub$prey2PopulationSize[dim(sub)[1]] == 0 | sub$predator1PopulationSize[dim(sub)[1]] == 0) {
            print(paste("replicate", n, "ended in extinction"))
            
          } else {
            # increase repNb 
            repNb <- repNb + 1
            
            # calculus
            # subset after
            subAfter <- subset(sub, subset = sub$timeStep >= after[1] & sub$timeStep <= after[2])
            
            # compute dev from base line
            densDevPrey1 <- c(densDevPrey1, (mean(subAfter$prey1PopulationSize) - baseMeanDensPrey1)  / baseMeanDensPrey1)
            
            # compute amplitude dev
            amplDevPrey1 <- c(amplDevPrey1, ((max(subAfter$prey1PopulationSize) - min(subAfter$prey1PopulationSize)) - baseAmplitudPrey1) / baseAmplitudPrey1) #  - 1)
            
            # compute dev from base line
            densDevPrey2 <- c(densDevPrey2, (mean(subAfter$prey2PopulationSize) - baseMeanDensPrey2)  / baseMeanDensPrey2)
            
            # compute amplitude dev
            amplDevPrey2 <- c(amplDevPrey2, ((max(subAfter$prey2PopulationSize) - min(subAfter$prey2PopulationSize)) - baseAmplitudPrey2) / baseAmplitudPrey2) #  - 1)
            
            # compute dev from base line
            densDevPred1 <- c(densDevPred1, (mean(subAfter$predator1PopulationSize) - baseMeanDensPred1)  / baseMeanDensPred1)
            
            # compute amplitude dev
            amplDevPred1 <- c(amplDevPred1, ((max(subAfter$predator1PopulationSize) - min(subAfter$predator1PopulationSize)) - baseAmplitudPred1) / baseAmplitudPred1) #  - 1)
            
            # compute catch rate dev from base line
            ctRtDevPrey1 <- c(ctRtDevPrey1, (mean(subAfter$prey1catches/subAfter$prey1PopulationSize) - baseMeanCtRtPrey2) / baseMeanCtRtPrey2)
            
            # compute catch rate dev
            ctRtDevPrey2 <- c(ctRtDevPrey2, (mean(subAfter$prey2catches/subAfter$prey2PopulationSize) - baseMeanCtRtPrey2) / baseMeanCtRtPrey2) #  - 1)
          }
        }
        
        # go to next pqrqm set if only extinctions
        if (repNb == 0) {next}
        
        # add nb of replicates
        newLine <- c(newLine, repNb)
        
        # # subset before pred introduction
        # sub <- subset(res, subset = res$timeStep >= before[1] & res$timeStep < before[2])
        # # get before measures
        # newLine <- c(newLine, 
        #              mean(sub$prey1PopulationSizeMean), max(sub$prey1PopulationSizeMean), min(sub$prey1PopulationSizeMean),
        #              mean(sub$prey2PopulationSizeMean), max(sub$prey2PopulationSizeMean), min(sub$prey2PopulationSizeMean),
        #              mean(sub$prey1growthRateMean/100), max(sub$prey1growthRateMean/100), min(sub$prey1growthRateMean/100),
        #              mean(sub$prey2growthRateMean/100), max(sub$prey2growthRateMean/100), min(sub$prey2growthRateMean/100))
        # 
        # # subset after
        # sub <- subset(res, subset = res$timeStep >= after[1] & res$timeStep <= after[2])
        # # get before measures
        # newLine <- c(newLine, 
        #              mean(sub$prey1PopulationSizeMean), max(sub$prey1PopulationSizeMean), min(sub$prey1PopulationSizeMean),
        #              mean(sub$prey2PopulationSizeMean), max(sub$prey2PopulationSizeMean), min(sub$prey2PopulationSizeMean),
        #              mean(sub$predator1PopulationSizeMean), max(sub$predator1PopulationSizeMean), min(sub$predator1PopulationSizeMean),
        #              mean(sub$prey1growthRateMean/100), max(sub$prey1growthRateMean/100), min(sub$prey1growthRateMean/100),
        #              mean(sub$prey2growthRateMean/100), max(sub$prey2growthRateMean/100), min(sub$prey2growthRateMean/100),
        #              mean(sub$predatorGrowthRateMean/100), max(sub$predatorGrowthRateMean/100), min(sub$predatorGrowthRateMean/100),
        #              mean(sub$prey1catchesMean), max(sub$prey1catchesMean), min(sub$prey1catchesMean),
        #              mean(sub$prey2catchesMean), max(sub$prey2catchesMean), min(sub$prey2catchesMean))
        
        # perform stats
        newLine <- c(newLine,
                     mean(densDevPrey1), boot_sd_ci(densDevPrey1)[-1], min(densDevPrey1), max(densDevPrey1), 
                     mean(densDevPrey2), boot_sd_ci(densDevPrey2)[-1], min(densDevPrey2), max(densDevPrey2), 
                     mean(densDevPred1), boot_sd_ci(densDevPred1)[-1], min(densDevPred1), max(densDevPred1),
                     mean(amplDevPrey1), boot_sd_ci(amplDevPrey1)[-1],
                     mean(amplDevPrey2), boot_sd_ci(amplDevPrey2)[-1],
                     mean(amplDevPred1), boot_sd_ci(amplDevPred1)[-1],
                     mean(ctRtDevPrey1), boot_sd_ci(ctRtDevPrey1)[-1], min(ctRtDevPrey1), max(ctRtDevPrey1),
                     mean(ctRtDevPrey1), boot_sd_ci(ctRtDevPrey1)[-1], min(ctRtDevPrey1), max(ctRtDevPrey1))
        
        # rbind the line to tab
        tab <- rbind(tab, as.numeric(newLine))
        
      } # end loop over results folders
      
    } # end loop over globalFol
    
    # name columns
    colnames(tab) <- headers
    
    # create local SA results folder in allStatsAndPlots and save table
    newFolderDir <- paste(folder, "allStatsAndPlots", "localSAfiles-woExt", sep = "/")
    # dir.create(path = newFolderDir)
    write.csv(tab, file = paste(newFolderDir, "/woExt-allSAdevStats-allPop", content[i], ".csv", sep = ""), row.names = FALSE)
    
  } # end loop over folders
  
}

folder = "C:/Users/adb3/Desktop/PhD/GitKraken/newLocalSA/folder-avgOff/"
path = "C:/Users/adb3/Desktop/PhD/GitKraken/newLocalSA/"
keyword = "Results"
pattern = "newSA-p"
exclude.ext = T

statsResults <- function(path, keyword = c("Results", "Snapshot"), pattern) {
  
  # get the directory content
  # content <- list.files(paste("~/", path, sep = ""))
  content <- list.files(path)
  
  # order alphabetically
  content <- content[order(content)]
  
  # only the folders
  content <- grep(pattern = c("folder"), x = content, value = T)
  
  # loop over the local SA folders
  for (i in 1:length(content)) {
    
    # path to folder
    folder = paste(path, content[i], sep = "")
    print(paste("in localSA folder ", folder))
    
    # create a global stats folder
    dir.create(path = paste(folder, "allStatsAndPlots", sep = "/"))
    
    # create a Results folder
    dir.create(path = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
    
    # get content
    simFol = list.files(folder)
    
    # select only folders
    simFol <- grep(pattern = c(pattern), x = simFol, value = T)
    
    # loop over the sim folders
    for (j in 1:length(simFol)) {
      
      print(paste("in sim folder ", simFol[j]))
      
      # # rename the file
      # cux <- list.files(paste(path, content[i], simFol[j], sep = "/"))
      # cux <- grep(pattern = c("stats"), x = cux, value = T)
      # file.rename(paste(path, content[i], simFol[j], cux, sep = "/"), paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = ""))
      
      # path to folder
      # statsFol = paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = "")
      statsFol = paste(folder, simFol[j], "/stats", sep = "/")
      # statsFol = paste(path, simFol[j], "/stats-", simFol[j], sep = "")
      
      # file.rename(paste(statsFol, results, sep = "/"), paste(statsFol, "/merged", keyword, "-", simFol[j], ".csv", sep = ""))
      
      # get files
      results <- list.files(statsFol)
      
      # only csv files
      results <- grep(pattern = c("merged"), x = results, value = T)
      
      # only the results files
      results <- grep(pattern = c(keyword), x = results, value = T)
      
      # get name
      name <- paste(statsFol, "/", results, sep = "")
      
      # read table
      stats <- read.csv(name)
      
      ## create table
      
      # new headers
      headers <- colnames(stats)
      newHeaders <- c(headers[2], "repNb")
      for (k in 3:length(headers)) {
        newHeaders <- c(newHeaders, paste(headers[k], "Mean", sep = ""), paste(headers[k], "ICinf", sep = ""), paste(headers[k], "ICsup", sep = ""))
      }
      
      # create an empty table 
      tab <- data.frame(matrix(ncol = length(newHeaders), nrow = 0))
      
      # for loop making subset for each time step
      ts <- levels(as.factor(stats$timeStep))
      for (k in 1:length(ts)) {
        
        # subset per ts number
        sub <- subset(stats, stats$timeStep == as.numeric(ts[k]))
        
        # initiate new line with time step
        newLine <- c(as.numeric(ts[k]), dim(sub)[1])
        
        # apply stats to each column of measures
        for (k in 3:(dim(sub)[2])) { # first line is rep, second is ts
          newLine <- c(newLine, mean(sub[,k]), boot_sd_ci(sub[,k])[2], boot_sd_ci(sub[,k])[3])
        }
        
        # rbind the line to tab
        tab <- rbind(tab, newLine)
      } # end loop over time steps
      
      # column names
      colnames(tab) <- newHeaders
      
      # write in the corresponding folder
      write.csv(tab, file = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), row.names = FALSE)
      
      # and copy in the global stats folder
      file.copy(from = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
      
      # plot and save figure
      data <- read.csv(paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""))
      
      x = data$timeStep
      y1 = data$prey1growthRateMean/100
      y2 = data$prey2growthRateMean/100
      y3 = data$predatorGrowthRateMean/100
      y1min = data$prey1growthRateICinf/100
      y2min = data$prey2growthRateICinf/100
      y3min = data$predatorGrowthRateICinf/100
      y1max = data$prey1growthRateICsup/100
      y2max = data$prey2growthRateICsup/100
      y3max = data$predatorGrowthRateICsup/100
      y1c = "red"
      y2c = "blue"
      y3c = "orange"
      tIntro = 210
      
      fig <- ggplot(data, aes(x)) + 
        geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 1.05*min(c(min(y1min), min(y2min), min(y3min))), ymax = 1.05*max(c(max(y1max), max(y2max), max(y3max)))), alpha=0.5, fill = "lightgrey") +
        geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
        geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
        geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
        geom_line(aes(y = y1), color = y1c) +
        geom_line(aes(y = y2), color = y2c) +
        geom_line(aes(y = y3), color = y3c) +
        # geom_point(aes(y = y1), size = 2.5, shape = 21, fill = "white", color = y1c) +
        # geom_point(aes(y = y2), size = 2.5, shape = 22, fill = "white", color = y2c) +
        # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c) +
        labs(x = "Time steps", y = "Realised growth rate") +
        scale_colour_manual(name='Populations',
                            breaks=c('Prey 1', 'Prey 2', 'Predator'),
                            values=c(y1c, y2c, y3c))
      
      # save plot in this folder
      ggsave(filename = paste("stats", keyword, "-", simFol[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
      
      # and copy in the global stats folder
      file.copy(from = paste(statsFol, "/stats", keyword, "-growthRate", simFol[j], ".pdf", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
      
    } # end loop over sim folders
    
  } # end loop over local SA folders
  
} # end of function

statsResultsNoExt <- function(path, keyword = c("Results", "Snapshot"), pattern, exclude.ext = FALSE) {
  
  # get the directory content
  # content <- list.files(paste("~/", path, sep = ""))
  content <- list.files(path)
  
  # order alphabetically
  content <- content[order(content)]
  
  # only the folders
  content <- grep(pattern = c("folder"), x = content, value = T)
  
  # loop over the local SA folders
  for (i in 1:length(content)) {
    
    # path to folder
    folder = paste(path, content[i], sep = "")
    print(paste("in localSA folder ", folder))
    
    # # create a global stats folder
    # dir.create(path = paste(folder, "allStatsAndPlots", sep = "/"))
    # 
    # # create a Results folder
    # ifelse(exclude.ext == FALSE, 
    #        dir.create(path = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = "")),
    #        dir.create(path = paste(folder, "/allStatsAndPlots/", keyword, "Files", "-woExt", sep = "")))
    # 
    # get content
    simFol = list.files(folder)
    
    # select only folders
    simFol <- grep(pattern = c("allStatsAndPlots"), x = simFol, value = T)
    
    simFol = paste(folder, simFol, "ResultsFiles-woExt", sep = "/")
    
    # get files
    results <- list.files(simFol)
    
    # # only csv files
    # results <- grep(pattern = c("merged"), x = results, value = T)
    
    # only the results files
    results <- grep(pattern = c(".csv"), x = results, value = T)
    
    # loop over the sim folders
    for (j in 1:length(results)) {  
      
      print(paste("in sim results ", results[j]))
      
      # # rename the file
      # cux <- list.files(paste(path, content[i], simFol[j], sep = "/"))
      # cux <- grep(pattern = c("stats"), x = cux, value = T)
      # file.rename(paste(path, content[i], simFol[j], cux, sep = "/"), paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = ""))
      
      # path to folder
      # statsFol = paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = "")
      # statsFol = paste(path, simFol[j], "/stats-", simFol[j], sep = "")
      # statsFol = paste(folder, simFol, "ResultsFiles-woExt", sep = "/")
      
      # file.rename(paste(statsFol, results, sep = "/"), paste(statsFol, "/merged", keyword, "-", simFol[j], ".csv", sep = ""))
      
      # # get files
      # results <- list.files(statsFol)
      # 
      # # # only csv files
      # # results <- grep(pattern = c("merged"), x = results, value = T)
      # 
      # # only the results files
      # results <- grep(pattern = c(".csv"), x = results, value = T)
      
      # # rm false file and rename true file
      # if (length(results) > 1) {
      #   if  (grep(pattern = c("rep"), x = results[2]) == 1) {
      #     file.remove(paste(statsFol, grep(pattern = c("rep"), x = results, value = T), sep = "/"))
      #     file.rename(paste(statsFol, grep(pattern = c("rep"), x = results, value = T, invert = TRUE), sep = "/"), paste(statsFol, grep(pattern = c("rep"), x = results, value = T, invert = TRUE), sep = "/"))
      #   } else {
      #     file.remove(paste(statsFol, grep(pattern = c("NA"), x = results, value = T), sep = "/"))
      #   }
      #   
      #   # get files
      #   results <- list.files(statsFol)
      #   
      #   # only csv files
      #   results <- grep(pattern = c("merged"), x = results, value = T)
      #   
      #   # only the results files
      #   results <- grep(pattern = c(keyword), x = results, value = T)
      # }
      
      # get name
      name <- paste(simFol, "/", results[j], sep = "")
      
      # read table
      stats <- read.csv(name)
      
      # ## create table
      # 
      # # new headers
      # headers <- colnames(stats)
      # newHeaders <- c(headers[2], "repNb")
      # for (k in 3:length(headers)) {
      #   newHeaders <- c(newHeaders, paste(headers[k], "Mean", sep = ""), paste(headers[k], "ICinf", sep = ""), paste(headers[k], "ICsup", sep = ""))
      # }
      # 
      # # create an empty table 
      # tab <- data.frame(matrix(ncol = length(newHeaders), nrow = 0))
      # 
      # if (exclude.ext == TRUE) {
      #   # create temporary table
      #   temp <- data.frame(matrix(ncol = length(newHeaders), nrow = 0))
      #   
      #   # subset merged results and move to temp only if no extinction
      #   repNb <- levels(as.factor(stats$replicate))
      #   
      #   for (m in 1:length(repNb)) {
      #     sub <- subset(stats, stats$replicate == repNb[m])
      #     
      #     if (sub$prey1PopulationSize[dim(sub)[1]] == 0 | sub$prey2PopulationSize[dim(sub)[1]] == 0 | sub$predator1PopulationSize[dim(sub)[1]] == 0) {
      #       print(paste("replicate", m, "ended in extinction"))
      #     } else {
      #       paste("cux")
      #       temp <- rbind(temp, sub)
      #     }
      #   }
      # } # end of if loop
      # 
      # # if temp is still empty skip to the next j loop
      # if (exclude.ext == TRUE & dim(temp)[1] == 0) {
      #   print("extinctions only -- move to next sim folder")
      #   
      # } else {
      #   
      #   if (exclude.ext == TRUE) {
      #     # otherwise replace stats by temp
      #     stats <- temp
      #   }
      #   
      #   # for loop making subset for each time step
      #   ts <- levels(as.factor(stats$timeStep))
      #   for (k in 1:length(ts)) {
      #     
      #     # subset per ts number
      #     sub <- subset(stats, stats$timeStep == as.numeric(ts[k]))
      #     
      #     # initiate new line with time step
      #     newLine <- c(as.numeric(ts[k]), dim(sub)[1])
      #     
      #     # apply stats to each column of measures
      #     for (k in 3:(dim(sub)[2])) { # first line is rep, second is ts
      #       newLine <- c(newLine, mean(sub[,k]), boot_sd_ci(sub[,k])[2], boot_sd_ci(sub[,k])[3])
      #     }
      #     
      #     # rbind the line to tab
      #     tab <- rbind(tab, newLine)
      #   } # end loop over time steps
      #   
      #   # column names
      #   colnames(tab) <- newHeaders
      #   
      #   if (exclude.ext == FALSE) {
      #     # write in the corresponding folder
      #     write.csv(tab, file = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), row.names = FALSE) 
      #     
      #     # and copy in the global stats folder
      #     file.copy(from = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
      #   } else {
      #     # write directly in the global folder
      #     write.csv(tab, file = paste(folder, "/allStatsAndPlots/", keyword, "Files-woExt/woExt-", simFol[j], ".csv", sep = ""), row.names = FALSE)
      #   } 
        
        # # plot and save figure
        # data <- tab
        # 
        # x = data$timeStep
        # y1 = data$prey1PopulationSizeMean
        # y2 = data$prey2PopulationSizeMean
        # y3 = data$predator1PopulationSizeMean
        # y1min = data$prey1PopulationSizeICinf
        # y2min = data$prey2PopulationSizeICinf
        # y3min = data$predator1PopulationSizeICinf
        # y1max = data$prey1PopulationSizeICsup
        # y2max = data$prey2PopulationSizeICsup
        # y3max = data$predator1PopulationSizeICsup
        # y1c = "red"
        # y2c = "blue"
        # y3c = "orange"
        # tIntro = 210
        # 
        # fig <- ggplot(data, aes(x)) + 
        #   geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(c(max(y1max), max(y2max), max(y3max)))), alpha=0.5, fill = "lightgrey") +
        #   geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
        #   geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
        #   geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
        #   geom_line(aes(y = y1), color = y1c) +
        #   geom_line(aes(y = y2), color = y2c) +
        #   geom_line(aes(y = y3), color = y3c) +
        #   # geom_point(aes(y = y1), size = 2.5, shape = 21, fill = "white", color = y1c) +
        #   # geom_point(aes(y = y2), size = 2.5, shape = 22, fill = "white", color = y2c) +
        #   # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c) +
        #   labs(x = "Time steps", y = "Population size") +
        #   scale_colour_manual(name='Populations',
        #                       breaks=c('Prey 1', 'Prey 2', 'Predator'),
        #                       values=c(y1c, y2c, y3c))
        # 
        # if (exclude.ext == FALSE) {
        #   # save plot in this folder
        #   ggsave(filename = paste("stats", keyword, "-", simFol[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
        #   
        #   # and copy in the global stats folder
        #   file.copy(from = paste(statsFol, "/stats", keyword, "-", simFol[j], ".pdf", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
        # } else {
        #   # save plot in global folder
        #   ggsave(filename = paste("woExt-stats", keyword, "-", simFol[j], ".pdf", sep = ""), path = paste(folder, "/allStatsAndPlots/", keyword, "Files-woExt", sep = ""), plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
        # }
        
        # plot and save growth rate figure
        data <- stats
        
        x = data$timeStep
        y1 = data$prey1growthRateMean/100
        y2 = data$prey2growthRateMean/100
        y3 = data$predatorGrowthRateMean/100
        y1min = data$prey1growthRateICinf/100
        y2min = data$prey2growthRateICinf/100
        y3min = data$predatorGrowthRateICinf/100
        y1max = data$prey1growthRateICsup/100
        y2max = data$prey2growthRateICsup/100
        y3max = data$predatorGrowthRateICsup/100
        y1c = "red"
        y2c = "blue"
        y3c = "orange"
        tIntro = 210
        
        fig <- ggplot(data, aes(x)) + 
          geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 1.05*min(c(min(y1min), min(y2min), min(y3min))), ymax = 1.05*max(c(max(y1max), max(y2max), max(y3max)))), alpha=0.5, fill = "lightgrey") +
          geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
          geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
          geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
          geom_line(aes(y = y1), color = y1c) +
          geom_line(aes(y = y2), color = y2c) +
          geom_line(aes(y = y3), color = y3c) +
          # geom_point(aes(y = y1), size = 2.5, shape = 21, fill = "white", color = y1c) +
          # geom_point(aes(y = y2), size = 2.5, shape = 22, fill = "white", color = y2c) +
          # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c) +
          labs(x = "Time steps", y = "Realised growth rate") +
          scale_colour_manual(name='Populations',
                              breaks=c('Prey 1', 'Prey 2', 'Predator'),
                              values=c(y1c, y2c, y3c))
        
        if (exclude.ext == FALSE) {
          # save plot in this folder
          ggsave(filename = paste("stats", keyword, "-growthRate-", results[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
          
          # and copy in the global stats folder
          file.copy(from = paste(statsFol, "/stats", keyword, "-", results[j], ".pdf", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
        } else {
          # save plot in global folder
          ggsave(filename = paste("woExt-stats", keyword, "-growthRate-", sub(x = results[j], pattern = paste(".csv", sep = ""), replacement = ""), ".pdf", sep = ""), path = paste(folder, "/allStatsAndPlots/", keyword, "Files-woExt", sep = ""), plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
        }
        
      # } # end of else loop
      
    } # end loop over sim folders
    
  } # end loop over local SA folders
  
} # end of function

