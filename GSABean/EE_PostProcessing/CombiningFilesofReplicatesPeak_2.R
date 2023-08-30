# Code to: 
#   1. Add indication of replica number. 
#   2. combine output files in the correct way. 
#   3. Outputs file with all effects of all combinations (in right way so we can calculate effects afterwards?)
# NOTE: this is a duplicate of the other file, simmply to consider three other outputs (easier than adding a lot of new code to the other file)
# So here consider fAbs, assCO2 and fieldRFR

# Initialize
#install.packages('matrixStats')

field_Rep_1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_1.txt'
field_Rep_2 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_2.txt'
field_Rep_3 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_3.txt'
parameterbounds <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Input/ParametersBean_EE.txt'
simulationpoints <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Input/Bean_EE_Trajectories.txt'
outputMeasuresFieldRFR <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_fieldRFR.txt'
outputMeasuresAssCOtwo <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_assCOtwoPeak.txt'
outputMeasuresfAbs <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_fAbsPeak.txt'
outputEffectsFieldRFR <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_fieldRFR.txt'
outputEffectsAssCOtwo <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_assCOtwoPeak.txt'
outputEffectsfAbs <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_fAbsPeak.txt'

nrOfParameters <- 31
nrOfSimDays <- 60 # or 100-1? I think 100 is fine, but should check.
nrOfTrajectories <- 20 
nrOfSimsPerRep <- nrOfTrajectories*(nrOfParameters+1)

# Read in each of the 3 output data files for each full replicate; make 3 new data files with only harvest day outputs, adding which replicate they came from
df_field_Rep_1 <- read.table(field_Rep_1, header = TRUE)
df_field_Rep_2 <- read.table(field_Rep_2, header = TRUE)
df_field_Rep_3 <- read.table(field_Rep_3, header = TRUE)#read.table(field_Rep_3)

# Find day where fAbs reaches maximum for each simulation; save those days and line numbers in vectors
maxfAbsRep1 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 5]) ) #as.matrix to make sure it's in global environment
maxfAbsRep2 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 5]) )
maxfAbsRep3 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 5]) )
maxfAbsvalues <- cbind(maxfAbsRep1,maxfAbsRep2,maxfAbsRep3)
maxfAbsindexRep1 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 5]))) #Unlist to list->matrix
maxfAbsindexRep2 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 5])))
maxfAbsindexRep3 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 5])))
maxfAbsindices <- cbind(maxfAbsindexRep1,maxfAbsindexRep2,maxfAbsindexRep3)
# Find day where assCOtwo reaches maximum for each simulation; save those days and line numbers in vectors
maxassCOtwoRep1 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 6]) ) #as.matrix to make sure it's in global environment
maxassCOtwoRep2 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 6]) )
maxassCOtwoRep3 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 6]) )
maxassCOtwovalues <- cbind(maxassCOtwoRep1,maxassCOtwoRep2,maxassCOtwoRep3)
maxassCOtwoindexRep1 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 6]))) #Unlist to list->matrix
maxassCOtwoindexRep2 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 6])))
maxassCOtwoindexRep3 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 6])))
maxassCOtwoindices <- cbind(maxassCOtwoindexRep1,maxassCOtwoindexRep2,maxassCOtwoindexRep3)

Rep_1_harvest_nrOfSimDays <- as.matrix(df_field_Rep_1[df_field_Rep_1[ ,1] == nrOfSimDays, ])
Rep_1_harvest_nrOfSimDays[,5] <- df_field_Rep_1[maxfAbsindexRep1, 5]  # change fAbs outputs to the max values
Rep_1_harvest_nrOfSimDays[,6] <- df_field_Rep_1[maxassCOtwoindexRep1, 6]  # change assCOtwo outputs to the max values
Rep_1_harvest_nrOfSimDays <- cbind(Rep_1_harvest_nrOfSimDays, rep = rep(1,nrow(Rep_1_harvest_nrOfSimDays))) #add rep number
colnames(Rep_1_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_2_harvest_nrOfSimDays <- as.matrix(df_field_Rep_2[df_field_Rep_2[ ,1] == nrOfSimDays, ])
Rep_2_harvest_nrOfSimDays[,5] <- df_field_Rep_2[maxfAbsindexRep2, 5]
Rep_2_harvest_nrOfSimDays[,6] <- df_field_Rep_2[maxassCOtwoindexRep2, 6]  
Rep_2_harvest_nrOfSimDays <- cbind(Rep_2_harvest_nrOfSimDays, rep = rep(2,nrow(Rep_2_harvest_nrOfSimDays)))
colnames(Rep_2_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_3_harvest_nrOfSimDays <- as.matrix(df_field_Rep_3[df_field_Rep_3[ ,1] == nrOfSimDays, ])
Rep_3_harvest_nrOfSimDays[,5] <- df_field_Rep_3[maxfAbsindexRep3, 5]
Rep_3_harvest_nrOfSimDays[,6] <- df_field_Rep_3[maxassCOtwoindexRep3, 6]  
Rep_3_harvest_nrOfSimDays <- cbind(Rep_3_harvest_nrOfSimDays, rep = rep(3,nrow(Rep_3_harvest_nrOfSimDays)))
colnames(Rep_3_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")


# Combine outputs of different replicates into effects (columns for different combinations?); add mean effect too?
# per output?
allfieldRFRoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 11], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 11], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 11] )), 2, as.numeric)
allassCOtwooutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 6], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 6], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 6] )), 2, as.numeric)
allfAbsoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 5], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 5], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 5] )), 2, as.numeric)
library(matrixStats)
allfieldRFRoutputs <- cbind(allfieldRFRoutputs, rowMeans(allfieldRFRoutputs)) # First average outputs (mean at each sim. point), then calculate effects (as an alternative)
allassCOtwooutputs <- cbind(allassCOtwooutputs, rowMeans(allassCOtwooutputs))
allfAbsoutputs <- cbind(allfAbsoutputs, rowMeans(allfAbsoutputs))
dfParameterbounds <- read.table(parameterbounds, comment.char = "", header = TRUE) #read in delta-values and min_i, max_i
dfSimulationpoints <- read.table(simulationpoints) #read in step-sizes

# Calculate relative standard deviation of outputs for the 3 replicates at each simulation point and make some histograms
RSDfieldRFR <- as.matrix(apply(allfieldRFRoutputs[,1:3], 1, function(x) 100*sd(x)/mean(x)))
RSDassCOtwo <- as.matrix(apply(allassCOtwooutputs[,1:3], 1, function(x) 100*sd(x)/mean(x)))
RSDfAbs <- as.matrix(apply(allfAbsoutputs[,1:3], 1, function(x) 100*sd(x)/mean(x)))
c1 <- rgb(90,120,255,max = 255, alpha = 255, names = "lt.orange") #blue
c2 <- rgb(192,192,192, max = 255, alpha = 255, names = "lt.grey") 
c3 <- rgb(255,0,0, max = 255, alpha = 255, names = "lt.gold") #red
hgA <- hist(RSDfieldRFR, 
            breaks = 10,
            plot = FALSE)
hgB <- hist(RSDassCOtwo, 
            breaks = 10,
            plot = FALSE)
hgC <- hist(RSDfAbs, 
            breaks = 10,
            plot = FALSE)
opar <- par(lwd=2)
plot(hgB, border = c1, xlim=c(0,5),xlab="RSD",ylim=c(0,300), main=NULL) # Plot 1st histogram using a transparent color
plot(hgA, border = "black", add = TRUE) # Add 2nd histogram using different color
plot(hgC, border = "red", add = TRUE)
legend("topright", c("Peak assCO2", "FieldRFR", "Peak fAbs"), col=c(c1, "black", "red"), lwd=2)
par(opar)

library(gtools)
combinations <- rbind(expand.grid(rep(list(1:3),2)),c(4,4)) # 4,4 is the row for first average outputs then calculate effects
fieldRFRAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allfieldRFRoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allfieldRFRoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
assCOtwoAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allassCOtwooutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allassCOtwooutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
fAbsAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allfAbsoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allfAbsoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
fieldRFREffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allfieldRFRoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allfieldRFRoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
assCOtwoEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allassCOtwooutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allassCOtwooutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
fAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allfAbsoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allfAbsoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))

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
indices <- matrix(0,1,2)#c(0,0) #These effects should not be taken into account.
for(i in 1:nrOfParameters){
  nroutliers[i,2] <- sum(deltatest[,i]<low[i]) # | deltatest[,i]>up[i]
  indices <- rbind(indices, cbind( rep(i,nroutliers[i,2]),  which(deltatest[,i]<low[i]) ))
}
indices <- indices[-1,]
if(sum(nroutliers[,2]==1)) {indices <- t(indices)} #apparently R makes matrix with 1 row into a COLUMN vector...
colnames(indices)<-c("Parameter", "Trajectory") 
#print some stuff
#indices
#nroutliers
# deltatestminpercent <- apply(deltatest, 2, function(x) min(x)*100/mean(x))
# deltatestmaxpercent <- apply(deltatest, 2, function(x) max(x)*100/mean(x))
# #print(deltatest)
# print(deltatestminpercent)
#print(deltatestmaxpercent)

#Option 1: set rows in e.g. FieldRFREffects to NA if outliers, then means/medians with na.rm=TRUE.
#Option 2: Leave e.g. FieldRFREffects as is, and take means only over appropriate rows in calc of e.g. fieldRFRAbsMeanEffects etc.
### END TEST 

# add means and medians of absolute effects over all combinations (columns);
# calculating S_i over mean/median column instead of taking average/median of S_i's of all combinations seems better.
library(matrixStats)
fieldRFRAbsEffects <- cbind(fieldRFRAbsEffects[,-ncol(fieldRFRAbsEffects)], rowMeans(fieldRFRAbsEffects[,-ncol(fieldRFRAbsEffects)]), rowMedians(fieldRFRAbsEffects[,-ncol(fieldRFRAbsEffects)]),fieldRFRAbsEffects[,ncol(fieldRFRAbsEffects)]) # don't include first average outputs column in mean/median calcs, but it is added as the last column seperately
assCOtwoAbsEffects <- cbind(assCOtwoAbsEffects[,-ncol(assCOtwoAbsEffects)], rowMeans(assCOtwoAbsEffects[,-ncol(assCOtwoAbsEffects)]), rowMedians(assCOtwoAbsEffects[,-ncol(assCOtwoAbsEffects)]),assCOtwoAbsEffects[,ncol(assCOtwoAbsEffects)])
fAbsAbsEffects <- cbind(fAbsAbsEffects[,-ncol(fAbsAbsEffects)], rowMeans(fAbsAbsEffects[,-ncol(fAbsAbsEffects)]), rowMedians(fAbsAbsEffects[,-ncol(fAbsAbsEffects)]),fAbsAbsEffects[,ncol(fAbsAbsEffects)])
fieldRFREffects <- cbind(fieldRFREffects[,-ncol(fieldRFREffects)], rowMeans(fieldRFREffects[,-ncol(fieldRFREffects)]), rowMedians(fieldRFREffects[,-ncol(fieldRFREffects)]),fieldRFREffects[,ncol(fieldRFREffects)])
assCOtwoEffects <- cbind(assCOtwoEffects[,-ncol(assCOtwoEffects)], rowMeans(assCOtwoEffects[,-ncol(assCOtwoEffects)]), rowMedians(assCOtwoEffects[,-ncol(assCOtwoEffects)]),assCOtwoEffects[,ncol(assCOtwoEffects)])
fAbsEffects <- cbind(fAbsEffects[,-ncol(fAbsEffects)], rowMeans(fAbsEffects[,-ncol(fAbsEffects)]), rowMedians(fAbsEffects[,-ncol(fAbsEffects)]),fAbsEffects[,ncol(fAbsEffects)])

# Also make table (for first average outputs) with absolute scaled effects per input (easier for writing to file)
fieldRFRAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
assCOtwoPeakAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
fAbsPeakAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
fieldRFRAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
assCOtwoPeakAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
fAbsPeakAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
for(i in 1:nrOfParameters){
  fieldRFRAbsEffectsNiceReadScaled[,i] <- fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(fieldRFRAbsEffects)]
  assCOtwoPeakAbsEffectsNiceReadScaled[,i] <- assCOtwoAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(assCOtwoAbsEffects)]
  fAbsPeakAbsEffectsNiceReadScaled[,i] <- fAbsAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(fAbsAbsEffects)]
  fieldRFRAbsEffectsNiceReadUnscaled[,i] <- fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(fieldRFRAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2]) 
  assCOtwoPeakAbsEffectsNiceReadUnscaled[,i] <- assCOtwoAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(assCOtwoAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2]) 
  fAbsPeakAbsEffectsNiceReadUnscaled[,i] <- fAbsAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(fAbsAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2])
}

##########
#### Run from here for write to file
##########

# Write absolute scaled effects to file (only for average outputs first, then calculate effects; 3 tables of nrOfTrajectories*nrOfParameters)
# TO DO are these scaled or unscaled? In principle scaled, but can unscale them.
line <- sprintf("Absolute (scaled and unscaled) effects; first averaged outputs of 3 replicates.")
write(line,file=outputEffectsFieldRFR,append=FALSE)
write(line,file=outputEffectsAssCOtwo,append=FALSE)
write(line,file=outputEffectsfAbs,append=FALSE)
line <- sprintf("\n Unscaled absolute effects:")
write(line,file=outputEffectsFieldRFR,append=TRUE)
write(line,file=outputEffectsAssCOtwo,append=TRUE)
write(line,file=outputEffectsfAbs,append=TRUE)
cat("FieldRFR:", file=outputEffectsFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fieldRFRAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsFieldRFR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("AssCOtwo peak:", file=outputEffectsAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(assCOtwoPeakAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsAssCOtwo, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("fAbs peak:", file=outputEffectsfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fieldRFRAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsfAbs, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
line <- sprintf("\n Scaled (in input direction) absolute effects:")
write(line,file=outputEffectsFieldRFR,append=TRUE)
write(line,file=outputEffectsAssCOtwo,append=TRUE)
write(line,file=outputEffectsfAbs,append=TRUE)
cat("FieldRFR:", file=outputEffectsFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fieldRFRAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsFieldRFR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("AssCOtwo peak:", file=outputEffectsAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(assCOtwoPeakAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsAssCOtwo, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("fAbs peak:", file=outputEffectsfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:nrOfParameters, file=outputEffectsfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fieldRFRAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsfAbs, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 



# Scaled but dimensional measures mu* and chi
fieldRFRAbsMeanEffects <- matrix(0,nrOfParameters,ncol(fieldRFRAbsEffects))
fieldRFRAbsMedianEffects <- matrix(0,nrOfParameters,ncol(fieldRFRAbsEffects))
fieldRFRSigma <- matrix(0,nrOfParameters,ncol(fieldRFRAbsEffects))
fieldRFRMeanEffects <- matrix(0,nrOfParameters,ncol(fieldRFRAbsEffects))
assCOtwoAbsMeanEffects <- matrix(0,nrOfParameters,ncol(assCOtwoAbsEffects))
assCOtwoAbsMedianEffects <- matrix(0,nrOfParameters,ncol(assCOtwoAbsEffects))
assCOtwoSigma <- matrix(0,nrOfParameters,ncol(assCOtwoAbsEffects))
assCOtwoMeanEffects <- matrix(0,nrOfParameters,ncol(assCOtwoAbsEffects))
fAbsAbsMeanEffects <- matrix(0,nrOfParameters,ncol(fAbsAbsEffects))  #mean(fieldRFRAbsEffects[which(fieldRFReffects[,j]%%nrOfParameters==i & !(fieldRFReffects[,j] %in% indices[indices[,1]==j,2])])
fAbsAbsMedianEffects <- matrix(0,nrOfParameters,ncol(fAbsAbsEffects))
fAbsSigma <- matrix(0,nrOfParameters,ncol(fAbsAbsEffects))
fAbsMeanEffects <- matrix(0,nrOfParameters,ncol(fAbsAbsEffects))

for(j in 1:ncol(fieldRFRAbsEffects)){
  for(i in 1:nrOfParameters){
    if(length(indices[indices[,1]==i,2])!=0){
      #if(j==1){ print(length(fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]))}
      fieldRFRAbsMeanEffects[i,j] <- mean(fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])       #TEST 13/01/22 remove outliers  #mean(fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      fieldRFRAbsMedianEffects[i,j] <- median(fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      assCOtwoAbsMeanEffects[i,j] <- mean(assCOtwoAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
      assCOtwoAbsMedianEffects[i,j] <- median(assCOtwoAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      fAbsAbsMeanEffects[i,j] <- mean(fAbsAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
      fAbsAbsMedianEffects[i,j] <- median(fAbsAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      
      fieldRFRSigma[i,j] <- sd(fieldRFREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]) #sigma over non-absolute (scaled) effects
      assCOtwoSigma[i,j] <- sd(assCOtwoEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]) # filter outliers
      fAbsSigma[i,j] <- sd(fAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      
      fieldRFRMeanEffects[i,j] <- mean(fieldRFREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      assCOtwoMeanEffects[i,j] <- mean(assCOtwoEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      fAbsMeanEffects[i,j] <- mean(fAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
    } else {
      #if(j==1){ print(length(fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j]))}
      fieldRFRAbsMeanEffects[i,j] <- mean(fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j]) #scaled dimensionless
      fieldRFRAbsMedianEffects[i,j] <- median(fieldRFRAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      assCOtwoAbsMeanEffects[i,j] <- mean(assCOtwoAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      assCOtwoAbsMedianEffects[i,j] <- median(assCOtwoAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      fAbsAbsMeanEffects[i,j] <- mean(fAbsAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      fAbsAbsMedianEffects[i,j] <- median(fAbsAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      
      fieldRFRSigma[i,j] <- sd(fieldRFREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      assCOtwoSigma[i,j] <- sd(assCOtwoEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      fAbsSigma[i,j] <- sd(fAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      
      fieldRFRMeanEffects[i,j] <- mean(fieldRFREffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      assCOtwoMeanEffects[i,j] <- mean(assCOtwoEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      fAbsMeanEffects[i,j] <- mean(fAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
    }
  }
}
fieldRFRSens <- cbind( fieldRFRAbsMeanEffects, fieldRFRAbsMedianEffects ) #mu*, chi
assCOtwoSens <- cbind( assCOtwoAbsMeanEffects, assCOtwoAbsMedianEffects ) #mu*, chi
fAbsSens <- cbind( fAbsAbsMeanEffects, fAbsAbsMedianEffects )  #mu*, chi
fieldRFRMeanEffects <- abs(fieldRFRMeanEffects)
assCOtwoMeanEffects <- abs(assCOtwoMeanEffects)
fAbsMeanEffects <- abs(fAbsMeanEffects)

#Write original sensitivity measures to file?
#  Yes, but only scaled mu*, chi and sigma. mu is obsolete; unscaled measures are obsolete.
line <- sprintf("***Non-normalized (and dimensional but scaled in input-direction) sensitivity indices mu*, chi and sigma per parameter (rows) and combination (columns):\n Combi 1-11 are mu*; 12-22 are Chi. Sigma is separate table. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n Mu* & chi:")
write(line,file=outputMeasuresFieldRFR,append=FALSE)
write(line,file=outputMeasuresAssCOtwo,append=FALSE)
write(line,file=outputMeasuresfAbs,append=FALSE)
cat("Combination:", file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"), file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fieldRFRSens[,c(12,24)],digits=14, format="e"), file=outputMeasuresFieldRFR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"), file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(assCOtwoSens[,c(12,24)],digits=14, format="e"), file=outputMeasuresAssCOtwo, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"), file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fAbsSens[,c(12,24)],digits=14, format="e"), file=outputMeasuresfAbs, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
line <- sprintf("\n Sigma:")
write(line,file=outputMeasuresFieldRFR,append=TRUE)
write(line,file=outputMeasuresAssCOtwo,append=TRUE)
write(line,file=outputMeasuresfAbs,append=TRUE)
cat("Combination:", file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(ncol(fieldRFRSigma), file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fieldRFRSigma[,c(12)],digits=14, format="e"), file=outputMeasuresFieldRFR,  sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(ncol(assCOtwoSigma), file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(assCOtwoSigma[,c(12)],digits=14, format="e"), file=outputMeasuresAssCOtwo, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(ncol(fAbsSigma), file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fAbsSigma[,c(12)],digits=14, format="e"), file=outputMeasuresfAbs, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

#normalize sensitivity indices
fieldRFRSens <- apply(fieldRFRSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
assCOtwoSens <- apply(assCOtwoSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
fAbsSens <- apply(fAbsSens, 2, function(x) x/sum(x)) #S_mu*, S_chi

#Write normalized sensitivities to file:
line <- sprintf("\n ***Normalized sensitivity indices per parameter (rows):\n combinations (columns) 1-11 are based on S_mu, (columns) 12-22 on S_chi. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n")
write(line,file=outputMeasuresFieldRFR,append=TRUE)
write(line,file=outputMeasuresAssCOtwo,append=TRUE)
write(line,file=outputMeasuresfAbs,append=TRUE)
cat("Combination:", file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"), file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fieldRFRSens[,c(12,24)],digits=14, format="e"), file=outputMeasuresFieldRFR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"), file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(assCOtwoSens[,c(12,24)],digits=14, format="e"), file=outputMeasuresAssCOtwo, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"), file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(fAbsSens[,c(12,24)],digits=14, format="e"), file=outputMeasuresfAbs, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

#assCOtwoSens <- cbind( assCOtwoAbsMeanEffects/sum(assCOtwoAbsMeanEffects), assCOtwoAbsMedianEffects/sum(assCOtwoAbsMedianEffects) ) #S_mu*, S_chi
#fAbsSens <- cbind( fAbsAbsMeanEffects/sum(fAbsAbsMeanEffects), fAbsAbsMedianEffects/sum(fAbsAbsMedianEffects) ) #S_mu*, S_chi
# apply(fieldRFRSens, 2, function(x) which(x==max(x, na.rm = TRUE)))
# apply(assCOtwoSens, 2, function(x) which(x==max(x, na.rm = TRUE)))
# apply(fAbsSens, 2, function(x) which(x==max(x, na.rm = TRUE)))
sortedFieldRFRSens <- apply(fieldRFRSens, 2, sort)
sortedAssCOtwoSens <- apply(assCOtwoSens, 2, sort)
sortedfAbsSens <- apply(fAbsSens, 2, sort)
orderFieldRFR <- apply(fieldRFRSens, 2, function(x) order(x)) #ranking of parameters (least to most important) per column. See also Excel for example
orderAssCOtwo <- apply(assCOtwoSens, 2, function(x) order(x))
orderfAbs <- apply(fAbsSens, 2, function(x) order(x))
# colnames(orderFieldRFR) <- c("Comb. 1",2:ncol(orderFieldRFR))
# colnames(orderAssCOtwo) <- c(1:ncol(orderAssCOtwo))
# colnames(orderfAbs) <- c(1:ncol(orderfAbs))

#write ordered parameters to file:
line <- sprintf("\n\n***Ordered parameters (least to most important): combinations (columns) 1-11 are based on S_mu, (columns) 12-22 on S_chi.\n Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n")
write(line,file=outputMeasuresFieldRFR,append=TRUE)
write(line,file=outputMeasuresAssCOtwo,append=TRUE)
write(line,file=outputMeasuresfAbs,append=TRUE)
cat("Combination:", file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"),"\r", file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderFieldRFR[,c(12,24)], file=outputMeasuresFieldRFR, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"),"\r", file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderAssCOtwo[,c(12,24)], file=outputMeasuresAssCOtwo, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(c("mu*", "chi"),"\r", file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderfAbs[,c(12,24)], file=outputMeasuresfAbs, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

# Calculate (un)important groups based on h-levels
line <- sprintf("\n\n ***(Un)important parameters: combinations 1-11 are based on S_mu, 12-22 on S_chi. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n\n")
write(line,file=outputMeasuresFieldRFR,append=TRUE)
write(line,file=outputMeasuresAssCOtwo,append=TRUE)
write(line,file=outputMeasuresfAbs,append=TRUE)

for(h in c(0.2,0.3,0.4)){ #0.05,0.1,0.15,0.2,0.25,
  line <- sprintf(" ---Combinations for h-level %.2f---",h)
  write(line,file=outputMeasuresFieldRFR,append=TRUE)
  write(line,file=outputMeasuresAssCOtwo,append=TRUE)
  write(line,file=outputMeasuresfAbs,append=TRUE)
  for(j in c(12,24)){
    tempa <- orderFieldRFR[1:max(which(cumsum(sortedFieldRFRSens[,j])<h)),j] #unimportant 
    tempb <- sortedFieldRFRSens[1:max(which(cumsum(sortedFieldRFRSens[,j])<h)),j]
    tempc <- orderFieldRFR[which(as.vector(sortedFieldRFRSens[,j])>(3/2*mean(tempb)+3*sd(tempb))),j] #important
    line <- sprintf("Unimportant parameters for output FieldRFR, combination %i:",j)
    write(line,file=outputMeasuresFieldRFR,append=TRUE)
    cat(tempa, file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    line <- sprintf("Important parameters for output FieldRFR, combination %i:",j)
    write(line,file=outputMeasuresFieldRFR,append=TRUE)
    cat(tempc, "\n", file=outputMeasuresFieldRFR, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    #
    tempa <- orderAssCOtwo[1:max(which(cumsum(sortedAssCOtwoSens[,j])<h)),j] #unimportant 
    tempb <- sortedAssCOtwoSens[1:max(which(cumsum(sortedAssCOtwoSens[,j])<h)),j]
    tempc <- orderAssCOtwo[which(as.vector(sortedAssCOtwoSens[,j])>(3/2*mean(tempb)+3*sd(tempb))),j] #important
    line <- sprintf("Unimportant parameters for output AssCOtwo, combination %i:",j)
    write(line,file=outputMeasuresAssCOtwo,append=TRUE)
    cat(tempa, file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    line <- sprintf("Important parameters for output AssCOtwo, combination %i:",j)
    write(line,file=outputMeasuresAssCOtwo,append=TRUE)
    cat(tempc, "\n", file=outputMeasuresAssCOtwo, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    #
    tempa <- orderfAbs[1:max(which(cumsum(sortedfAbsSens[,j])<h)),j] #unimportant 
    tempb <- sortedfAbsSens[1:max(which(cumsum(sortedfAbsSens[,j])<h)),j]
    tempc <- orderfAbs[which(as.vector(sortedfAbsSens[,j])>(3/2*mean(tempb)+3*sd(tempb))),j] #important
    line <- sprintf("Unimportant parameters for output fAbs, combination %i:",j)
    write(line,file=outputMeasuresfAbs,append=TRUE)
    cat(tempa, file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    line <- sprintf("Important parameters for output fAbs, combination %i:",j)
    write(line,file=outputMeasuresfAbs,append=TRUE)
    cat(tempc, "\n", file=outputMeasuresfAbs, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
  }
}
