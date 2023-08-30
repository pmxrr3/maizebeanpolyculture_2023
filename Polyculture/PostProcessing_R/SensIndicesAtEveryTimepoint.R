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
outputMeasuresFieldRFR <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_fieldRFR_day30.txt'
outputMeasuresAssCOtwo <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_assCOtwoPeak_day30.txt'
outputMeasuresfAbs <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_fAbsPeak_day30.txt'
outputEffectsFieldRFR <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_fieldRFR_day30.txt'
outputEffectsAssCOtwo <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_assCOtwoPeak_day30.txt'
outputEffectsfAbs <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_fAbsPeak_day30.txt'

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

for(simdaycounter in 1:60){
Rep_1_harvest_nrOfSimDays <- as.matrix(df_field_Rep_1[df_field_Rep_1[ ,1] == simdaycounter, ])
#Rep_1_harvest_nrOfSimDays[,5] <- df_field_Rep_1[maxfAbsindexRep1, 5]  # change fAbs outputs to the max values
#Rep_1_harvest_nrOfSimDays[,6] <- df_field_Rep_1[maxassCOtwoindexRep1, 6]  # change assCOtwo outputs to the max values
Rep_1_harvest_nrOfSimDays <- cbind(Rep_1_harvest_nrOfSimDays, rep = rep(1,nrow(Rep_1_harvest_nrOfSimDays))) #add rep number
colnames(Rep_1_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_2_harvest_nrOfSimDays <- as.matrix(df_field_Rep_2[df_field_Rep_2[ ,1] == simdaycounter, ])
#Rep_2_harvest_nrOfSimDays[,5] <- df_field_Rep_2[maxfAbsindexRep2, 5]
#Rep_2_harvest_nrOfSimDays[,6] <- df_field_Rep_2[maxassCOtwoindexRep2, 6]  
Rep_2_harvest_nrOfSimDays <- cbind(Rep_2_harvest_nrOfSimDays, rep = rep(2,nrow(Rep_2_harvest_nrOfSimDays)))
colnames(Rep_2_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_3_harvest_nrOfSimDays <- as.matrix(df_field_Rep_3[df_field_Rep_3[ ,1] == simdaycounter, ])
#Rep_3_harvest_nrOfSimDays[,5] <- df_field_Rep_3[maxfAbsindexRep3, 5]
#Rep_3_harvest_nrOfSimDays[,6] <- df_field_Rep_3[maxassCOtwoindexRep3, 6]  
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

library(gtools)
combinations <- rbind(expand.grid(rep(list(1:3),2)),c(4,4)) # 4,4 is the row for first average outputs then calculate effects
fieldRFRAbsEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
fieldRFRAbsEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allfieldRFRoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allfieldRFRoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
#
assCOtwoAbsEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
assCOtwoAbsEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allassCOtwooutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allassCOtwooutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
#
fAbsAbsEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
fAbsAbsEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allfAbsoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allfAbsoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
#
fieldRFREffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
fieldRFREffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allfieldRFRoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allfieldRFRoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
#
assCOtwoEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
assCOtwoEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allassCOtwooutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allassCOtwooutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
#
fAbsEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
fAbsEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allfAbsoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allfAbsoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))

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

for(j in ncol(fieldRFRAbsEffects)){
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

#normalize sensitivity indices
fieldRFRSens <- apply(fieldRFRSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
assCOtwoSens <- apply(assCOtwoSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
fAbsSens <- apply(fAbsSens, 2, function(x) x/sum(x)) #S_mu*, S_chi

if(simdaycounter == 1) {
  fieldRFRSensMatrix <- fieldRFRSens[,24]
  assCOtwoSensMatrix <- assCOtwoSens[,24]
  fAbsSensMatrix <- fAbsSens[,24]
} else {
  fieldRFRSensMatrix <- cbind(fieldRFRSensMatrix,fieldRFRSens[,24])
  assCOtwoSensMatrix <- cbind(assCOtwoSensMatrix,assCOtwoSens[,24])
  fAbsSensMatrix <- cbind(fAbsSensMatrix,fAbsSens[,24])
}
print(simdaycounter)
} #end big for loop

#Make plots
#How to colour the 'highest' lines? Define threshold, and colour all lines that somewhere cross that threshold
fieldRFRSensMatrix <- t(fieldRFRSensMatrix)
#fieldRFRSensMatrix <- fieldRFRSensMatrix[,order(fieldRFRSensMatrix[nrow(fieldRFRSensMatrix),])]
assCOtwoSensMatrix <- t(assCOtwoSensMatrix)
#assCOtwoSensMatrix <- assCOtwoSensMatrix[,order(assCOtwoSensMatrix[nrow(assCOtwoSensMatrix),])]
fAbsSensMatrix <- t(fAbsSensMatrix)
#fAbsSensMatrix <- fAbsSensMatrix[,order(fAbsSensMatrix[nrow(fAbsSensMatrix),])]
#
colnames(fieldRFRSensMatrix) <- 1:31
colnames(assCOtwoSensMatrix) <- 1:31
colnames(fAbsSensMatrix) <- 1:31
rownames(fieldRFRSensMatrix) <- 1:60
rownames(assCOtwoSensMatrix) <- 1:60
rownames(fAbsSensMatrix) <- 1:60

indicesfieldRFR <- unique(which(fieldRFRSensMatrix[10:60,] >= .1, arr.ind = TRUE)[,2])
indicesassCO2 <- unique(which(assCOtwoSensMatrix[10:60,] >= .1, arr.ind = TRUE)[,2])
indicesfAbs <- unique(which(fAbsSensMatrix[10:60,] >= .1, arr.ind = TRUE)[,2])
uniqueindices <- sort(unique(c(indicesfieldRFR,indicesassCO2,indicesfAbs)))
#install.packages("randomcoloR")                               # Install & load randomcoloR package
library("randomcoloR")
#colourarray <- distinctColorPalette(max(c(length(indicesassCO2),length(indicesfAbs),length(indicesfieldRFR))))
colourarray <- distinctColorPalette(length(uniqueindices))
#
par(xpd=TRUE) #square
if(1 %in% indicesfieldRFR){
  plot(xlim = c(1,60), ylim=c(0,0.3), c(fieldRFRSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col=colourarray[which(uniqueindices %in% 1)], lwd="2") 
} else {
  plot(xlim = c(1,60), ylim=c(0,0.3), c(fieldRFRSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col="gray")
}
for(j in 2:(31)){
  if(j %in% indicesfieldRFR){
    lines(1:60,c(fieldRFRSensMatrix[1:60,j]), col=colourarray[which(uniqueindices %in% j)], lwd="2") 
  } else {
    lines(1:60,c(fieldRFRSensMatrix[1:60,j]), col="gray")
  }
}
title("Field red/far-red ratio")
legend("topleft", legend=indicesfieldRFR, fill = colourarray[which(uniqueindices %in% indicesfieldRFR)], col = colourarray[which(uniqueindices %in% indicesfieldRFR)], title="Param.")
#
par(xpd=TRUE) #square
if(1 %in% indicesassCO2){
  plot(xlim = c(1,60), ylim=c(0,0.3), c(assCOtwoSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col=colourarray[which(uniqueindices %in% 1)], lwd="2") 
} else {
  plot(xlim = c(1,60), ylim=c(0,0.3), c(assCOtwoSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col="gray")
}
for(j in 2:(31)){
  if(j %in% indicesassCO2){
    lines(1:60,c(assCOtwoSensMatrix[1:60,j]), col=colourarray[which(uniqueindices %in% j)], lwd="2") 
  } else {
    lines(1:60,c(assCOtwoSensMatrix[1:60,j]), col="gray")
  }
}
title("Assimilated CO2")
legend("topleft", legend=indicesassCO2, fill = colourarray[which(uniqueindices %in% indicesassCO2)], col = colourarray[which(uniqueindices %in% indicesassCO2)], title="Param.")
#
par(xpd=TRUE) #square
if(1 %in% indicesfAbs){
  plot(xlim = c(1,60), ylim=c(0,0.3), c(fAbsSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col=colourarray[which(uniqueindices %in% 1)], lwd="2") 
} else {
  plot(xlim = c(1,60), ylim=c(0,0.3), c(fAbsSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col="gray")
}
for(j in 2:(31)){
  if(j %in% indicesfAbs){
    lines(1:60,c(fAbsSensMatrix[1:60,j]), col=colourarray[which(uniqueindices %in% j)], lwd="2") 
  } else {
    lines(1:60,c(fAbsSensMatrix[1:60,j]), col="gray")
  }
}
title("Fraction of radiation absorbed")
legend("topleft", legend=indicesfAbs, fill = colourarray[which(uniqueindices %in% indicesfAbs)], col = colourarray[which(uniqueindices %in% indicesfAbs)], title="Param.")
#
