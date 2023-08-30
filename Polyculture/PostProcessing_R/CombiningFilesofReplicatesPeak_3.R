# Code to: 
#   1. Add indication of replica number. 
#   2. combine output files in the correct way. 
#   3. Outputs file with all effects of all combinations (in right way so we can calculate effects afterwards?)
# NOTE: in this file we can consider plant-level outputs, such as average leaf area per plant, average accumulated PAR per plant and average (total) biomass per plant.
#       
# Initialize
#install.packages('matrixStats')

plant_Rep_1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/plant_Rep_1.txt'
plant_Rep_2 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/plant_Rep_2.txt'
plant_Rep_3 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/plant_Rep_3.txt'
parameterbounds <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Input/ParametersBean.txt'
simulationpoints <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Input/SimulationPoints.txt'
outputMeasuresAccPAR <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_AccPAR.txt'
outputMeasuresNrLeaves <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_NrLeavesPeak.txt'
# outputMeasuresAccPAR <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_AccPARPeak.txt'
outputEffectsAccPAR <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_AccPAR.txt'
outputEffectsNrLeaves <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_NrLeaves.txt'
# outputEffectsAccPAR <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_AccPARPeak.txt'

nrOfParameters <- 6
nrOfSimDays <- 100 # or 100-1? I think 100 is fine, but should check.
nrOfTrajectories <- 20 
nrOfSimsPerRep <- nrOfTrajectories*(nrOfParameters+1)
nrOfPlants <- 100

# Read in each of the 3 output data files for each full replicate; make 3 new data files with only harvest day outputs, adding which replicate they came from
df_plant_Rep_1 <- read.table(plant_Rep_1, header = TRUE)
df_plant_Rep_2 <- read.table(plant_Rep_2, header = TRUE)
df_plant_Rep_3 <- read.table(plant_Rep_3, header = TRUE)#read.table(plant_Rep_3)

# Find day where AccPAR reaches maximum for each simulation; save those days and line numbers in vectors
maxAccPARRep1 <- sapply(1:nrOfSimsPerRep, function(x) max(df_plant_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 5]) ) #as.matrix to make sure it's in global environment
maxAccPARRep2 <- sapply(1:nrOfSimsPerRep, function(x) max(df_plant_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 5]) )
maxAccPARRep3 <- sapply(1:nrOfSimsPerRep, function(x) max(df_plant_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 5]) )
maxAccPARvalues <- cbind(maxAccPARRep1,maxAccPARRep2,maxAccPARRep3)
maxAccPARindexRep1 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_plant_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 5]))) #Unlist to list->matrix
maxAccPARindexRep2 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_plant_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 5])))
maxAccPARindexRep3 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_plant_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 5])))
maxAccPARindices <- cbind(maxAccPARindexRep1,maxAccPARindexRep2,maxAccPARindexRep3)
# Find day where NrLeaves reaches maximum for each simulation; save those days and line numbers in vectors
maxNrLeavesRep1 <- sapply(1:nrOfSimsPerRep, function(x) max(df_plant_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 6]) ) #as.matrix to make sure it's in global environment
maxNrLeavesRep2 <- sapply(1:nrOfSimsPerRep, function(x) max(df_plant_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 6]) )
maxNrLeavesRep3 <- sapply(1:nrOfSimsPerRep, function(x) max(df_plant_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 6]) )
maxNrLeavesvalues <- cbind(maxNrLeavesRep1,maxNrLeavesRep2,maxNrLeavesRep3)
maxNrLeavesindexRep1 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_plant_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 6]))) #Unlist to list->matrix
maxNrLeavesindexRep2 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_plant_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 6])))
maxNrLeavesindexRep3 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_plant_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 6])))
maxNrLeavesindices <- cbind(maxNrLeavesindexRep1,maxNrLeavesindexRep2,maxNrLeavesindexRep3)

Rep_1_harvest_nrOfSimDays <- as.matrix(df_plant_Rep_1[df_plant_Rep_1[ ,1] == "nrOfSimDays", ])
Rep_1_harvest_nrOfSimDays[,5] <- df_plant_Rep_1[maxAccPARindexRep1, 5]  # change AccPAR outputs to the max values
Rep_1_harvest_nrOfSimDays[,6] <- df_plant_Rep_1[maxNrLeavesindexRep1, 6]  # change NrLeaves outputs to the max values
Rep_1_harvest_nrOfSimDays <- cbind(Rep_1_harvest_nrOfSimDays, rep = rep(1,nrow(Rep_1_harvest_nrOfSimDays))) #add rep number
colnames(Rep_1_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"AccPAR",	"NrLeaves",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"plantRFR", "Rep")
#
Rep_2_harvest_nrOfSimDays <- as.matrix(df_plant_Rep_2[df_plant_Rep_2[ ,1] == "nrOfSimDays", ])
Rep_2_harvest_nrOfSimDays[,5] <- df_plant_Rep_2[maxAccPARindexRep2, 5]
Rep_2_harvest_nrOfSimDays[,6] <- df_plant_Rep_2[maxNrLeavesindexRep2, 6]  
Rep_2_harvest_nrOfSimDays <- cbind(Rep_2_harvest_nrOfSimDays, rep = rep(2,nrow(Rep_2_harvest_nrOfSimDays)))
colnames(Rep_2_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"AccPAR",	"NrLeaves",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"plantRFR", "Rep")
#
Rep_3_harvest_nrOfSimDays <- as.matrix(df_plant_Rep_3[df_plant_Rep_3[ ,1] == "nrOfSimDays", ])
Rep_3_harvest_nrOfSimDays[,5] <- df_plant_Rep_3[maxAccPARindexRep3, 5]
Rep_3_harvest_nrOfSimDays[,6] <- df_plant_Rep_3[maxNrLeavesindexRep3, 6]  
Rep_3_harvest_nrOfSimDays <- cbind(Rep_3_harvest_nrOfSimDays, rep = rep(3,nrow(Rep_3_harvest_nrOfSimDays)))
colnames(Rep_3_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"AccPAR",	"NrLeaves",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"plantRFR", "Rep")


# Combine outputs of different replicates into effects (columns for different combinations?); add mean effect too?
# per output?
allplantRFRoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 11], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 11], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 11] )), 2, as.numeric)
allNrLeavesoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 6], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 6], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 6] )), 2, as.numeric)
allAccPARoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 5], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 5], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 5] )), 2, as.numeric)
library(matrixStats)
allplantRFRoutputs <- cbind(allplantRFRoutputs, rowMeans(allplantRFRoutputs)) # First average outputs (mean at each sim. point), then calculate effects (as an alternative)
allNrLeavesoutputs <- cbind(allNrLeavesoutputs, rowMeans(allNrLeavesoutputs))
allAccPARoutputs <- cbind(allAccPARoutputs, rowMeans(allAccPARoutputs))
dfParameterbounds <- read.table(parameterbounds, comment.char = "", header = TRUE) #read in delta-values and min_i, max_i
dfSimulationpoints <- read.table(simulationpoints) #read in step-sizes

# Calculate relative standard deviation of outputs for the 3 replicates at each simulation point and make some histograms
RSDplantRFR <- as.matrix(apply(allplantRFRoutputs[,0:2], 1, function(x) 100*sd(x)/mean(x)))
RSDNrLeaves <- as.matrix(apply(allNrLeavesoutputs[,0:2], 1, function(x) 100*sd(x)/mean(x)))
RSDAccPAR <- as.matrix(apply(allAccPARoutputs[,0:2], 1, function(x) 100*sd(x)/mean(x)))
c1 <- rgb(90,120,255,max = 255, alpha = 255, names = "lt.orange") #blue
c2 <- rgb(192,192,192, max = 255, alpha = 255, names = "lt.grey") 
c3 <- rgb(255,0,0, max = 255, alpha = 255, names = "lt.gold") #red
hgA <- hist(RSDplantRFR, 
            xlab="RSD",
            xlim=c(0,10),
            breaks = 20,
            plot = FALSE)
hgB <- hist(RSDNrLeaves, 
            plot = FALSE)
hgC <- hist(RSDAccPAR, 
            plot = FALSE)
opar <- par(lwd=2)
plot(hgB, border = c1, xlim=c(0,10),xlab="RSD",ylim=c(0,1200), main=NULL) # Plot 1st histogram using a transparent color
plot(hgA, border = "black", add = TRUE) # Add 2nd histogram using different color
plot(hgC, border = "red", add = TRUE)
legend("topright", c("Peak NrLeaves", "Yield", "Peak AccPAR"), col=c(c1, "black", "red"), lwd=2)
par(opar)

library(gtools)
combinations <- rbind(expand.grid(rep(list(1:3),2)),c(4,4)) # 4,4 is the row for first average outputs then calculate effects
plantRFRAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allplantRFRoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allplantRFRoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
NrLeavesAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allNrLeavesoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allNrLeavesoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
AccPARAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allAccPARoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allAccPARoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
plantRFREffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allplantRFRoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allplantRFRoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
NrLeavesEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allNrLeavesoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allNrLeavesoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
AccPAREffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allAccPARoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allAccPARoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))

### Remove outliers (i.e. those with very small (relative) step size delta)
################### calculate deltas 
deltatest <- matrix(0,nrOfTrajectories,nrOfParameters)
simpointsSortedBase <- matrix(0,nrOfTrajectories,nrOfParameters)
simpointsSortedPert <- matrix(0,nrOfTrajectories,nrOfParameters)
for(ii in 1:nrOfParameters){
  if(ii==nrOfParameters){
    temptest <- abs(dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 1,ii]-dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 0,ii])
    temptest2 <- dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 1,ii]
    temptest3 <- dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 0,ii]
  } else {
    temptest <- abs(dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 1,ii]-dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == (ii+1),ii])
    temptest2 <- dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 1,ii]
    temptest3 <- dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == (ii+1),ii]
  }
  deltatest[,ii] <- temptest
  simpointsSortedBase[,ii] <- temptest2
  simpointsSortedPert[,ii] <- temptest3
}
# Option 1: those step sizes <2% of 25%-quantile for that input (so out of nrOfTrajectories step sizes)
#deltatestquantiles <- apply(deltatest,2,function(x) quantile(x, probs=c(.25, .75), na.rm = FALSE))
#low <- deltatestquantiles[1,]*0.02 # lower range for outliers as % of 25%-quantile
# Option 2: assuming coordinates from QR sequence are iid Unif, step sizes |delta| follow triangle distribution. Requiring CDF(|delta|)>0.005 gives outliers are those for which |delta|<1-\sqrt{(nrOfTrajectories-1)8}/20 \approx 0.0025.
low <- rep(1,nrOfParameters)*(1-sqrt(398)/20)
for(i in 1:nrOfParameters){
  low[i] <- low[i]*(dfParameterbounds[i,3]-dfParameterbounds[i,2])
}

#Outliers
nroutliers <- matrix(0,nrOfParameters,2) #Number of outliers per parameter
nroutliers[,1] <- 1:nrOfParameters
indices <- c(0,0) #These effects should not be taken into account.
for(i in 1:nrOfParameters){
  nroutliers[i,2] <- sum(deltatest[,i]<low[i]) # | deltatest[,i]>up[i]
  indices <- rbind(indices, cbind( rep(i,nroutliers[i,2]),  which(deltatest[,i]<low[i]) ))
}
indices <- indices[-1,] 
colnames(indices)<-c("Parameter", "Trajectory") 
#print some stuff
#indices
#nroutliers
# deltatestminpercent <- apply(deltatest, 2, function(x) min(x)*100/mean(x))
# deltatestmaxpercent <- apply(deltatest, 2, function(x) max(x)*100/mean(x))
# #print(deltatest)
# print(deltatestminpercent)
#print(deltatestmaxpercent)

#Option 1: set rows in e.g. YieldEffects to NA if outliers, then means/medians with na.rm=TRUE.
#Option 2: Leave e.g. YieldEffects as is, and take means only over appropriate rows in calc of e.g. plantRFRAbsMeanEffects etc.
### END TEST 

# add means and medians of absolute effects over all combinations (columns);
# calculating S_i over mean/median column instead of taking average/median of S_i's of all combinations seems better.
library(matrixStats)
plantRFRAbsEffects <- cbind(plantRFRAbsEffects[,-ncol(plantRFRAbsEffects)], rowMeans(plantRFRAbsEffects[,-ncol(plantRFRAbsEffects)]), rowMedians(plantRFRAbsEffects[,-ncol(plantRFRAbsEffects)]),plantRFRAbsEffects[,ncol(plantRFRAbsEffects)]) # don't include first average outputs column in mean/median calcs, but it is added as the last column seperately
NrLeavesAbsEffects <- cbind(NrLeavesAbsEffects[,-ncol(NrLeavesAbsEffects)], rowMeans(NrLeavesAbsEffects[,-ncol(NrLeavesAbsEffects)]), rowMedians(NrLeavesAbsEffects[,-ncol(NrLeavesAbsEffects)]),NrLeavesAbsEffects[,ncol(NrLeavesAbsEffects)])
AccPARAbsEffects <- cbind(AccPARAbsEffects[,-ncol(AccPARAbsEffects)], rowMeans(AccPARAbsEffects[,-ncol(AccPARAbsEffects)]), rowMedians(AccPARAbsEffects[,-ncol(AccPARAbsEffects)]),AccPARAbsEffects[,ncol(AccPARAbsEffects)])
plantRFREffects <- cbind(plantRFREffects[,-ncol(plantRFREffects)], rowMeans(plantRFREffects[,-ncol(plantRFREffects)]), rowMedians(plantRFREffects[,-ncol(plantRFREffects)]),plantRFREffects[,ncol(plantRFREffects)])
NrLeavesEffects <- cbind(NrLeavesEffects[,-ncol(NrLeavesEffects)], rowMeans(NrLeavesEffects[,-ncol(NrLeavesEffects)]), rowMedians(NrLeavesEffects[,-ncol(NrLeavesEffects)]),NrLeavesEffects[,ncol(NrLeavesEffects)])
AccPAREffects <- cbind(AccPAREffects[,-ncol(AccPAREffects)], rowMeans(AccPAREffects[,-ncol(AccPAREffects)]), rowMedians(AccPAREffects[,-ncol(AccPAREffects)]),AccPAREffects[,ncol(AccPAREffects)])

# Also make table (for first average outputs) with absolute scaled effects per input (easier for writing to file)
plantRFRAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
NrLeavesPeakAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
AccPARPeakAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
plantRFRAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
NrLeavesPeakAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
AccPARPeakAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
for(i in 1:nrOfParameters){
  plantRFRAbsEffectsNiceReadScaled[,i] <- plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(plantRFRAbsEffects)]
  NrLeavesPeakAbsEffectsNiceReadScaled[,i] <- NrLeavesAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(NrLeavesAbsEffects)]
  AccPARPeakAbsEffectsNiceReadScaled[,i] <- AccPARAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(AccPARAbsEffects)]
  plantRFRAbsEffectsNiceReadUnscaled[,i] <- plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(plantRFRAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2]) 
  NrLeavesPeakAbsEffectsNiceReadUnscaled[,i] <- NrLeavesAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(NrLeavesAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2]) 
  AccPARPeakAbsEffectsNiceReadUnscaled[,i] <- AccPARAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(AccPARAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2])
}

##########
#### Run from here for write to file
##########

# Write absolute scaled effects to file (only for average outputs first, then calculate effects; 3 tables of nrOfTrajectories*nrOfParameters)
# TO DO are these scaled or unscaled? In principle scaled, but can unscale them.
line <- sprintf("Absolute (scaled and unscaled) effects; first averaged outputs of 3 replicates.")
write(line,file=outputEffectsYield,append=FALSE)
write(line,file=outputEffectsBiom,append=FALSE)
write(line,file=outputEffectsAccPAR,append=FALSE)
line <- sprintf("\n Unscaled absolute effects:")
write(line,file=outputEffectsYield,append=TRUE)
write(line,file=outputEffectsBiom,append=TRUE)
write(line,file=outputEffectsAccPAR,append=TRUE)
cat("Yield:", file=outputEffectsYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(plantRFRAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Biom peak:", file=outputEffectsBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(NrLeavesPeakAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("AccPAR peak:", file=outputEffectsAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(plantRFRAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsAccPAR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
line <- sprintf("\n Scaled (in input direction) absolute effects:")
write(line,file=outputEffectsYield,append=TRUE)
write(line,file=outputEffectsBiom,append=TRUE)
write(line,file=outputEffectsAccPAR,append=TRUE)
cat("Yield:", file=outputEffectsYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(plantRFRAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Biom peak:", file=outputEffectsBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(NrLeavesPeakAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("AccPAR peak:", file=outputEffectsAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(plantRFRAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsAccPAR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 



# Scaled but dimensional measures mu* and chi
plantRFRAbsMeanEffects <- matrix(0,nrOfParameters,ncol(plantRFRAbsEffects))
plantRFRAbsMedianEffects <- matrix(0,nrOfParameters,ncol(plantRFRAbsEffects))
plantRFRSigma <- matrix(0,nrOfParameters,ncol(plantRFRAbsEffects))
plantRFRMeanEffects <- matrix(0,nrOfParameters,ncol(plantRFRAbsEffects))
NrLeavesAbsMeanEffects <- matrix(0,nrOfParameters,ncol(NrLeavesAbsEffects))
NrLeavesAbsMedianEffects <- matrix(0,nrOfParameters,ncol(NrLeavesAbsEffects))
NrLeavesSigma <- matrix(0,nrOfParameters,ncol(NrLeavesAbsEffects))
NrLeavesMeanEffects <- matrix(0,nrOfParameters,ncol(NrLeavesAbsEffects))
AccPARAbsMeanEffects <- matrix(0,nrOfParameters,ncol(AccPARAbsEffects))  #mean(plantRFRAbsEffects[which(plantRFReffects[,j]%%nrOfParameters==i & !(plantRFReffects[,j] %in% indices[indices[,1]==j,2])])
AccPARAbsMedianEffects <- matrix(0,nrOfParameters,ncol(AccPARAbsEffects))
AccPARSigma <- matrix(0,nrOfParameters,ncol(AccPARAbsEffects))
AccPARMeanEffects <- matrix(0,nrOfParameters,ncol(AccPARAbsEffects))

for(j in 1:ncol(plantRFRAbsEffects)){
  for(i in 1:nrOfParameters){
    if(length(indices[indices[,1]==i,2])!=0){
      #if(j==1){ print(length(plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]))}
      plantRFRAbsMeanEffects[i,j] <- mean(plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])       #TEST 13/01/22 remove outliers  #mean(plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      plantRFRAbsMedianEffects[i,j] <- median(plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      NrLeavesAbsMeanEffects[i,j] <- mean(NrLeavesAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
      NrLeavesAbsMedianEffects[i,j] <- median(NrLeavesAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      AccPARAbsMeanEffects[i,j] <- mean(AccPARAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
      AccPARAbsMedianEffects[i,j] <- median(AccPARAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      
      plantRFRSigma[i,j] <- sd(plantRFREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]) #sigma over non-absolute (scaled) effects
      NrLeavesSigma[i,j] <- sd(NrLeavesEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]) # filter outliers
      AccPARSigma[i,j] <- sd(AccPAREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      
      plantRFRMeanEffects[i,j] <- mean(plantRFREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      NrLeavesMeanEffects[i,j] <- mean(NrLeavesEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      AccPARMeanEffects[i,j] <- mean(AccPAREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
    } else {
      #if(j==1){ print(length(plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j]))}
      plantRFRAbsMeanEffects[i,j] <- mean(plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j]) #scaled dimensionless
      plantRFRAbsMedianEffects[i,j] <- median(plantRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      NrLeavesAbsMeanEffects[i,j] <- mean(NrLeavesAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      NrLeavesAbsMedianEffects[i,j] <- median(NrLeavesAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      AccPARAbsMeanEffects[i,j] <- mean(AccPARAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      AccPARAbsMedianEffects[i,j] <- median(AccPARAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      
      plantRFRSigma[i,j] <- sd(plantRFREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      NrLeavesSigma[i,j] <- sd(NrLeavesEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      AccPARSigma[i,j] <- sd(AccPAREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      
      plantRFRMeanEffects[i,j] <- mean(plantRFREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      NrLeavesMeanEffects[i,j] <- mean(NrLeavesEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      AccPARMeanEffects[i,j] <- mean(AccPAREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
    }
  }
}
plantRFRSens <- cbind( plantRFRAbsMeanEffects, plantRFRAbsMedianEffects ) #mu*, chi
NrLeavesSens <- cbind( NrLeavesAbsMeanEffects, NrLeavesAbsMedianEffects ) #mu*, chi
AccPARSens <- cbind( AccPARAbsMeanEffects, AccPARAbsMedianEffects )  #mu*, chi
plantRFRMeanEffects <- abs(plantRFRMeanEffects)
NrLeavesMeanEffects <- abs(NrLeavesMeanEffects)
AccPARMeanEffects <- abs(AccPARMeanEffects)

#Write original sensitivity measures to file?
#  Yes, but only scaled mu*, chi and sigma. mu is obsolete; unscaled measures are obsolete.
line <- sprintf("***Non-normalized (and dimensional but scaled in input-direction) sensitivity indices mu*, chi and sigma per parameter (rows) and combination (columns):\n Combi 1-11 are mu*; 12-22 are Chi. Sigma is separate table. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n Mu* & chi:")
write(line,file=outputMeasuresYield,append=FALSE)
write(line,file=outputMeasuresBiom,append=FALSE)
write(line,file=outputMeasuresAccPAR,append=FALSE)
cat("Combination:", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(plantRFRSens), file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(plantRFRSens,digits=14, format="e"), file=outputMeasuresYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(NrLeavesSens), file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(NrLeavesSens,digits=14, format="e"), file=outputMeasuresBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(AccPARSens), file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(AccPARSens,digits=14, format="e"), file=outputMeasuresAccPAR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
line <- sprintf("\n Sigma:")
write(line,file=outputMeasuresYield,append=TRUE)
write(line,file=outputMeasuresBiom,append=TRUE)
write(line,file=outputMeasuresAccPAR,append=TRUE)
cat("Combination:", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(plantRFRSigma), file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(plantRFRSigma,digits=14, format="e"), file=outputMeasuresYield,  sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(NrLeavesSigma), file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(NrLeavesSigma,digits=14, format="e"), file=outputMeasuresBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(AccPARSigma), file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(AccPARSigma,digits=14, format="e"), file=outputMeasuresAccPAR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

#normalize sensitivity indices
plantRFRSens <- apply(plantRFRSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
NrLeavesSens <- apply(NrLeavesSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
AccPARSens <- apply(AccPARSens, 2, function(x) x/sum(x)) #S_mu*, S_chi

#Write normalized sensitivities to file:
line <- sprintf("\n ***Normalized sensitivity indices per parameter (rows):\n combinations (columns) 1-11 are based on S_mu, (columns) 12-22 on S_chi. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n")
write(line,file=outputMeasuresYield,append=TRUE)
write(line,file=outputMeasuresBiom,append=TRUE)
write(line,file=outputMeasuresAccPAR,append=TRUE)
cat("Combination:", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(plantRFRSens), file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(plantRFRSens,digits=14, format="e"), file=outputMeasuresYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(NrLeavesSens), file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(NrLeavesSens,digits=14, format="e"), file=outputMeasuresBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(AccPARSens), file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(AccPARSens,digits=14, format="e"), file=outputMeasuresAccPAR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

#NrLeavesSens <- cbind( NrLeavesAbsMeanEffects/sum(NrLeavesAbsMeanEffects), NrLeavesAbsMedianEffects/sum(NrLeavesAbsMedianEffects) ) #S_mu*, S_chi
#AccPARSens <- cbind( AccPARAbsMeanEffects/sum(AccPARAbsMeanEffects), AccPARAbsMedianEffects/sum(AccPARAbsMedianEffects) ) #S_mu*, S_chi
# apply(plantRFRSens, 2, function(x) which(x==max(x, na.rm = TRUE)))
# apply(NrLeavesSens, 2, function(x) which(x==max(x, na.rm = TRUE)))
# apply(AccPARSens, 2, function(x) which(x==max(x, na.rm = TRUE)))
sortedYieldSens <- apply(plantRFRSens, 2, sort)
sortedBiomSens <- apply(NrLeavesSens, 2, sort)
sortedAccPARSens <- apply(AccPARSens, 2, sort)
orderYield <- apply(plantRFRSens, 2, function(x) order(x)) #ranking of parameters (least to most important) per column. See also Excel for example
orderBiom <- apply(NrLeavesSens, 2, function(x) order(x))
orderAccPAR <- apply(AccPARSens, 2, function(x) order(x))
# colnames(orderYield) <- c("Comb. 1",2:ncol(orderYield))
# colnames(orderBiom) <- c(1:ncol(orderBiom))
# colnames(orderAccPAR) <- c(1:ncol(orderAccPAR))

#write ordered parameters to file:
line <- sprintf("\n\n***Ordered parameters (least to most important): combinations (columns) 1-11 are based on S_mu, (columns) 12-22 on S_chi.\n Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n")
write(line,file=outputMeasuresYield,append=TRUE)
write(line,file=outputMeasuresBiom,append=TRUE)
write(line,file=outputMeasuresAccPAR,append=TRUE)
cat("Combination:", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(orderYield),"\r", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderYield, file=outputMeasuresYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(orderBiom),"\r", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderBiom, file=outputMeasuresBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(orderAccPAR),"\r", file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderAccPAR, file=outputMeasuresAccPAR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

# Calculate (un)important groups based on h-levels
line <- sprintf("\n\n ***(Un)important parameters: combinations 1-11 are based on S_mu, 12-22 on S_chi. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n\n")
write(line,file=outputMeasuresYield,append=TRUE)
write(line,file=outputMeasuresBiom,append=TRUE)
write(line,file=outputMeasuresAccPAR,append=TRUE)

for(h in c(0.05,0.1,0.15,0.2,0.25,0.3)){ #0.05,0.1,0.15,0.2,0.25,
  line <- sprintf(" ---Combinations for h-level %.2f---",h)
  write(line,file=outputMeasuresYield,append=TRUE)
  write(line,file=outputMeasuresBiom,append=TRUE)
  write(line,file=outputMeasuresAccPAR,append=TRUE)
  for(j in 1:ncol(orderYield)){
    tempa <- orderYield[1:max(which(cumsum(sortedYieldSens[,j])<h)),j] #unimportant 
    tempb <- sortedYieldSens[1:max(which(cumsum(sortedYieldSens[,j])<h)),j]
    tempc <- orderYield[which(as.vector(sortedYieldSens[,j])>(mean(tempb)+3*sd(tempb))),j] #important
    line <- sprintf("Unimportant parameters for output Yield, combination %i:",j)
    write(line,file=outputMeasuresYield,append=TRUE)
    cat(tempa, file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    line <- sprintf("Important parameters for output Yield, combination %i:",j)
    write(line,file=outputMeasuresYield,append=TRUE)
    cat(tempc, "\n", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    #
    tempa <- orderBiom[1:max(which(cumsum(sortedBiomSens[,j])<h)),j] #unimportant 
    tempb <- sortedBiomSens[1:max(which(cumsum(sortedBiomSens[,j])<h)),j]
    tempc <- orderBiom[which(as.vector(sortedBiomSens[,j])>(mean(tempb)+3*sd(tempb))),j] #important
    line <- sprintf("Unimportant parameters for output Biom, combination %i:",j)
    write(line,file=outputMeasuresBiom,append=TRUE)
    cat(tempa, file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    line <- sprintf("Important parameters for output Biom, combination %i:",j)
    write(line,file=outputMeasuresBiom,append=TRUE)
    cat(tempc, "\n", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    #
    tempa <- orderAccPAR[1:max(which(cumsum(sortedAccPARSens[,j])<h)),j] #unimportant 
    tempb <- sortedAccPARSens[1:max(which(cumsum(sortedAccPARSens[,j])<h)),j]
    tempc <- orderAccPAR[which(as.vector(sortedAccPARSens[,j])>(mean(tempb)+3*sd(tempb))),j] #important
    line <- sprintf("Unimportant parameters for output AccPAR, combination %i:",j)
    write(line,file=outputMeasuresAccPAR,append=TRUE)
    cat(tempa, file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    line <- sprintf("Important parameters for output AccPAR, combination %i:",j)
    write(line,file=outputMeasuresAccPAR,append=TRUE)
    cat(tempc, "\n", file=outputMeasuresAccPAR, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
  }
}
