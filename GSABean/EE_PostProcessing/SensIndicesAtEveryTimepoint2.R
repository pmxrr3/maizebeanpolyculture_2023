# Code to: 
#   1. Add indication of replica number. 
#   2. combine output files in the correct way. 
#   3. Outputs file with all effects of all combinations (in right way so we can calculate effects afterwards?)
# NOTE: this is a duplicate of the other file, simmply to consider three other outputs (easier than adding a lot of new code to the other file)
# So here consider LAI, assCO2 and Biom

# Initialize
#install.packages('matrixStats')

field_Rep_1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_1.txt'
field_Rep_2 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_2.txt'
field_Rep_3 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_3.txt'
parameterbounds <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Input/ParametersBean_EE.txt'
simulationpoints <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Input/Bean_EE_Trajectories.txt'
# outputMeasuresBiom <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_Biom_day30.txt'
# outputMeasuresYield <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_YieldPeak_day30.txt'
# outputMeasuresfAbs <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_fAbsPeak_day30.txt'
# outputEffectsBiom <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_Biom_day30.txt'
# outputEffectsYield <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_YieldPeak_day30.txt'
# outputEffectsfAbs <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_fAbsPeak_day30.txt'

nrOfParameters <- 31
nrOfSimDays <- 60 # or 100-1? I think 100 is fine, but should check.
nrOfTrajectories <- 20 
nrOfSimsPerRep <- nrOfTrajectories*(nrOfParameters+1)

# Read in each of the 3 output data files for each full replicate; make 3 new data files with only harvest day outputs, adding which replicate they came from
df_field_Rep_1 <- read.table(field_Rep_1, header = TRUE)
df_field_Rep_2 <- read.table(field_Rep_2, header = TRUE)
df_field_Rep_3 <- read.table(field_Rep_3, header = TRUE)#read.table(field_Rep_3)

# Find day where fAbs reaches maximum for each simulation; save those days and line numbers in vectors
maxLAIRep1 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 3]) ) #as.matrix to make sure it's in global environment
maxLAIRep2 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 3]) )
maxLAIRep3 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 3]) )
maxLAIvalues <- cbind(maxLAIRep1,maxLAIRep2,maxLAIRep3)
maxLAIindexRep1 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 3]))) #Unlist to list->matrix
maxLAIindexRep2 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 3])))
maxLAIindexRep3 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 3])))
maxLAIindices <- cbind(maxLAIindexRep1,maxLAIindexRep2,maxLAIindexRep3)
# Find day where Yield reaches maximum for each simulation; save those days and line numbers in vectors
maxYieldRep1 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 7]) ) #as.matrix to make sure it's in global environment
maxYieldRep2 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 7]) )
maxYieldRep3 <- sapply(1:nrOfSimsPerRep, function(x) max(df_field_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 7]) )
maxYieldvalues <- cbind(maxYieldRep1,maxYieldRep2,maxYieldRep3)
maxYieldindexRep1 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_1[(x-1)*nrOfSimDays+1:nrOfSimDays, 7]))) #Unlist to list->matrix
maxYieldindexRep2 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_2[(x-1)*nrOfSimDays+1:nrOfSimDays, 7])))
maxYieldindexRep3 <- unlist(sapply(1:nrOfSimsPerRep, function(x) (x-1)*nrOfSimDays+which.max(df_field_Rep_3[(x-1)*nrOfSimDays+1:nrOfSimDays, 7])))
maxYieldindices <- cbind(maxYieldindexRep1,maxYieldindexRep2,maxYieldindexRep3)

for(simdaycounter in 1:60){
  Rep_1_harvest_nrOfSimDays <- as.matrix(df_field_Rep_1[df_field_Rep_1[ ,1] == simdaycounter, ])
  #Rep_1_harvest_nrOfSimDays[,5] <- df_field_Rep_1[maxLAIindexRep1, 5]  # change LAI outputs to the max values
  #Rep_1_harvest_nrOfSimDays[,6] <- df_field_Rep_1[maxYieldindexRep1, 6]  # change Yield outputs to the max values
  Rep_1_harvest_nrOfSimDays <- cbind(Rep_1_harvest_nrOfSimDays, rep = rep(1,nrow(Rep_1_harvest_nrOfSimDays))) #add rep number
  colnames(Rep_1_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"Biom", "Rep")
  #
  Rep_2_harvest_nrOfSimDays <- as.matrix(df_field_Rep_2[df_field_Rep_2[ ,1] == simdaycounter, ])
  #Rep_2_harvest_nrOfSimDays[,5] <- df_field_Rep_2[maxfAbsindexRep2, 5]
  #Rep_2_harvest_nrOfSimDays[,6] <- df_field_Rep_2[maxYieldindexRep2, 6]  
  Rep_2_harvest_nrOfSimDays <- cbind(Rep_2_harvest_nrOfSimDays, rep = rep(2,nrow(Rep_2_harvest_nrOfSimDays)))
  colnames(Rep_2_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"Biom", "Rep")
  #
  Rep_3_harvest_nrOfSimDays <- as.matrix(df_field_Rep_3[df_field_Rep_3[ ,1] == simdaycounter, ])
  #Rep_3_harvest_nrOfSimDays[,5] <- df_field_Rep_3[maxfAbsindexRep3, 5]
  #Rep_3_harvest_nrOfSimDays[,6] <- df_field_Rep_3[maxYieldindexRep3, 6]  
  Rep_3_harvest_nrOfSimDays <- cbind(Rep_3_harvest_nrOfSimDays, rep = rep(3,nrow(Rep_3_harvest_nrOfSimDays)))
  colnames(Rep_3_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"Biom", "Rep")
  
  
  # Combine outputs of different replicates into effects (columns for different combinations?); add mean effect too?
  # per output?
  allBiomoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 7], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 7], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 7] )), 2, as.numeric)
  allYieldoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 8], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 8], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 8] )), 2, as.numeric)
  allLAIoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 3], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 3], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 3] )), 2, as.numeric)
  library(matrixStats)
  allBiomoutputs <- cbind(allBiomoutputs, rowMeans(allBiomoutputs)) # First average outputs (mean at each sim. point), then calculate effects (as an alternative)
  allYieldoutputs <- cbind(allYieldoutputs, rowMeans(allYieldoutputs))
  allLAIoutputs <- cbind(allLAIoutputs, rowMeans(allLAIoutputs))
  dfParameterbounds <- read.table(parameterbounds, comment.char = "", header = TRUE) #read in delta-values and min_i, max_i
  dfSimulationpoints <- read.table(simulationpoints) #read in step-sizes
  
  library(gtools)
  combinations <- rbind(expand.grid(rep(list(1:3),2)),c(4,4)) # 4,4 is the row for first average outputs then calculate effects
  BiomAbsEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
  BiomAbsEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allBiomoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allBiomoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
  #
  YieldAbsEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
  YieldAbsEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allYieldoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allYieldoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
  #
  LAIAbsEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
  LAIAbsEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allLAIoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allLAIoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
  #
  BiomEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
  BiomEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allBiomoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allBiomoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
  #
  YieldEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
  YieldEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allYieldoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allYieldoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
  #
  LAIEffects <- matrix(1 , nrOfParameters*nrOfTrajectories, 10 )
  LAIEffects[,10] <-  c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allLAIoutputs[j*(nrOfParameters+1)+1,combinations[nrow(combinations),1]]-allLAIoutputs[j*(nrOfParameters+1)+(z+1),combinations[nrow(combinations),2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])))))
  
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
  BiomAbsEffects <- cbind(BiomAbsEffects[,-ncol(BiomAbsEffects)], rowMeans(BiomAbsEffects[,-ncol(BiomAbsEffects)]), rowMedians(BiomAbsEffects[,-ncol(BiomAbsEffects)]),BiomAbsEffects[,ncol(BiomAbsEffects)]) # don't include first average outputs column in mean/median calcs, but it is added as the last column seperately
  YieldAbsEffects <- cbind(YieldAbsEffects[,-ncol(YieldAbsEffects)], rowMeans(YieldAbsEffects[,-ncol(YieldAbsEffects)]), rowMedians(YieldAbsEffects[,-ncol(YieldAbsEffects)]),YieldAbsEffects[,ncol(YieldAbsEffects)])
  LAIAbsEffects <- cbind(LAIAbsEffects[,-ncol(LAIAbsEffects)], rowMeans(LAIAbsEffects[,-ncol(LAIAbsEffects)]), rowMedians(LAIAbsEffects[,-ncol(LAIAbsEffects)]),LAIAbsEffects[,ncol(LAIAbsEffects)])
  BiomEffects <- cbind(BiomEffects[,-ncol(BiomEffects)], rowMeans(BiomEffects[,-ncol(BiomEffects)]), rowMedians(BiomEffects[,-ncol(BiomEffects)]),BiomEffects[,ncol(BiomEffects)])
  YieldEffects <- cbind(YieldEffects[,-ncol(YieldEffects)], rowMeans(YieldEffects[,-ncol(YieldEffects)]), rowMedians(YieldEffects[,-ncol(YieldEffects)]),YieldEffects[,ncol(YieldEffects)])
  LAIEffects <- cbind(LAIEffects[,-ncol(LAIEffects)], rowMeans(LAIEffects[,-ncol(LAIEffects)]), rowMedians(LAIEffects[,-ncol(LAIEffects)]),LAIEffects[,ncol(LAIEffects)])
  
  # Also make table (for first average outputs) with absolute scaled effects per input (easier for writing to file)
  BiomAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
  YieldPeakAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
  LAIPeakAbsEffectsNiceReadScaled <- matrix(0,nrOfTrajectories,nrOfParameters)
  BiomAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
  YieldPeakAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
  LAIPeakAbsEffectsNiceReadUnscaled <- matrix(0,nrOfTrajectories,nrOfParameters)
  for(i in 1:nrOfParameters){
    BiomAbsEffectsNiceReadScaled[,i] <- BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(BiomAbsEffects)]
    YieldPeakAbsEffectsNiceReadScaled[,i] <- YieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(YieldAbsEffects)]
    LAIPeakAbsEffectsNiceReadScaled[,i] <- LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(LAIAbsEffects)]
    BiomAbsEffectsNiceReadUnscaled[,i] <- BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(BiomAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2]) 
    YieldPeakAbsEffectsNiceReadUnscaled[,i] <- YieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(YieldAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2]) 
    LAIPeakAbsEffectsNiceReadUnscaled[,i] <- LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,ncol(LAIAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2])
  }
  
  # Scaled but dimensional measures mu* and chi
  BiomAbsMeanEffects <- matrix(0,nrOfParameters,ncol(BiomAbsEffects))
  BiomAbsMedianEffects <- matrix(0,nrOfParameters,ncol(BiomAbsEffects))
  BiomSigma <- matrix(0,nrOfParameters,ncol(BiomAbsEffects))
  BiomMeanEffects <- matrix(0,nrOfParameters,ncol(BiomAbsEffects))
  YieldAbsMeanEffects <- matrix(0,nrOfParameters,ncol(YieldAbsEffects))
  YieldAbsMedianEffects <- matrix(0,nrOfParameters,ncol(YieldAbsEffects))
  YieldSigma <- matrix(0,nrOfParameters,ncol(YieldAbsEffects))
  YieldMeanEffects <- matrix(0,nrOfParameters,ncol(YieldAbsEffects))
  LAIAbsMeanEffects <- matrix(0,nrOfParameters,ncol(LAIAbsEffects))  #mean(BiomAbsEffects[which(Biomeffects[,j]%%nrOfParameters==i & !(Biomeffects[,j] %in% indices[indices[,1]==j,2])])
  LAIAbsMedianEffects <- matrix(0,nrOfParameters,ncol(LAIAbsEffects))
  LAISigma <- matrix(0,nrOfParameters,ncol(LAIAbsEffects))
  LAIMeanEffects <- matrix(0,nrOfParameters,ncol(LAIAbsEffects))
  
  for(j in ncol(BiomAbsEffects)){
    for(i in 1:nrOfParameters){
      if(length(indices[indices[,1]==i,2])!=0){
        #if(j==1){ print(length(BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]))}
        BiomAbsMeanEffects[i,j] <- mean(BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])       #TEST 13/01/22 remove outliers  #mean(BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
        BiomAbsMedianEffects[i,j] <- median(BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
        YieldAbsMeanEffects[i,j] <- mean(YieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
        YieldAbsMedianEffects[i,j] <- median(YieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
        LAIAbsMeanEffects[i,j] <- mean(LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
        LAIAbsMedianEffects[i,j] <- median(LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
        
        BiomSigma[i,j] <- sd(BiomEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]) #sigma over non-absolute (scaled) effects
        YieldSigma[i,j] <- sd(YieldEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]) # filter outliers
        LAISigma[i,j] <- sd(LAIEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
        
        BiomMeanEffects[i,j] <- mean(BiomEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
        YieldMeanEffects[i,j] <- mean(YieldEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
        LAIMeanEffects[i,j] <- mean(LAIEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      } else {
        #if(j==1){ print(length(BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j]))}
        BiomAbsMeanEffects[i,j] <- mean(BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j]) #scaled dimensionless
        BiomAbsMedianEffects[i,j] <- median(BiomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
        YieldAbsMeanEffects[i,j] <- mean(YieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
        YieldAbsMedianEffects[i,j] <- median(YieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
        LAIAbsMeanEffects[i,j] <- mean(LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
        LAIAbsMedianEffects[i,j] <- median(LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
        
        BiomSigma[i,j] <- sd(BiomEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
        YieldSigma[i,j] <- sd(YieldEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
        LAISigma[i,j] <- sd(LAIEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
        
        BiomMeanEffects[i,j] <- mean(BiomEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
        YieldMeanEffects[i,j] <- mean(YieldEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
        LAIMeanEffects[i,j] <- mean(LAIEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      }
    }
  }
  BiomSens <- cbind( BiomAbsMeanEffects, BiomAbsMedianEffects ) #mu*, chi
  YieldSens <- cbind( YieldAbsMeanEffects, YieldAbsMedianEffects ) #mu*, chi
  LAISens <- cbind( LAIAbsMeanEffects, LAIAbsMedianEffects )  #mu*, chi
  BiomMeanEffects <- abs(BiomMeanEffects)
  YieldMeanEffects <- abs(YieldMeanEffects)
  LAIMeanEffects <- abs(LAIMeanEffects)
  
  #normalize sensitivity indices
  BiomSens <- apply(BiomSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
  YieldSens <- apply(YieldSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
  LAISens <- apply(LAISens, 2, function(x) x/sum(x)) #S_mu*, S_chi
  
  if(simdaycounter == 1) {
    BiomSensMatrix <- BiomSens[,24]
    YieldSensMatrix <- YieldSens[,24]
    LAISensMatrix <- LAISens[,24]
  } else {
    BiomSensMatrix <- cbind(BiomSensMatrix,BiomSens[,24])
    YieldSensMatrix <- cbind(YieldSensMatrix,YieldSens[,24])
    LAISensMatrix <- cbind(LAISensMatrix,LAISens[,24])
  }
  print(simdaycounter)
} #end big for loop

#Make plots
#How to colour the 'highest' lines? Define threshold, and colour all lines that somewhere cross that threshold
BiomSensMatrix <- t(BiomSensMatrix)
#BiomSensMatrix <- BiomSensMatrix[,order(BiomSensMatrix[nrow(BiomSensMatrix),])]
YieldSensMatrix <- t(YieldSensMatrix)
#YieldSensMatrix <- YieldSensMatrix[,order(YieldSensMatrix[nrow(YieldSensMatrix),])]
LAISensMatrix <- t(LAISensMatrix)
#LAISensMatrix <- LAISensMatrix[,order(LAISensMatrix[nrow(LAISensMatrix),])]
#
colnames(BiomSensMatrix) <- 1:31
colnames(YieldSensMatrix) <- 1:31
colnames(LAISensMatrix) <- 1:31
rownames(BiomSensMatrix) <- 1:60
rownames(YieldSensMatrix) <- 1:60
rownames(LAISensMatrix) <- 1:60

indicesBiom <- unique(which(BiomSensMatrix[10:60,] >= .1, arr.ind = TRUE)[,2])
indicesYield <- unique(which(YieldSensMatrix[10:60,] >= .1, arr.ind = TRUE)[,2])
indicesLAI <- unique(which(LAISensMatrix[10:60,] >= .1, arr.ind = TRUE)[,2])
uniqueindices <- sort(unique(c(indicesBiom,indicesYield,indicesLAI)))
#install.packages("randomcoloR")                               # Install & load randomcoloR package
library("randomcoloR")
#colourarray <- distinctColorPalette(max(c(length(indicesYield),length(indicesLAI),length(indicesBiom))))
colourarray <- distinctColorPalette(length(uniqueindices))
#
par(xpd=FALSE) #square
if(1 %in% indicesBiom){
  plot(xlim = c(1,60), ylim=c(0,0.3), c(BiomSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col=colourarray[which(uniqueindices %in% 1)], lwd="2") 
} else {
  plot(xlim = c(1,60), ylim=c(0,0.3), c(BiomSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col="gray")
}
for(j in 2:(31)){
  if(j %in% indicesBiom){
    lines(1:60,c(BiomSensMatrix[1:60,j]), col=colourarray[which(uniqueindices %in% j)], lwd="2") 
  } else {
    lines(1:60,c(BiomSensMatrix[1:60,j]), col="gray")
  }
}
title("Aboveground biomass")
legend("topleft", legend=indicesBiom, fill = colourarray[which(uniqueindices %in% indicesBiom)], col = colourarray[which(uniqueindices %in% indicesBiom)], title="Param.")
#
par(xpd=TRUE) #square
if(1 %in% indicesYield){
  plot(xlim = c(1,60), ylim=c(0,0.4), c(YieldSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col=colourarray[which(uniqueindices %in% 1)], lwd="2") 
} else {
  plot(xlim = c(1,60), ylim=c(0,0.4), c(YieldSensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col="gray")
}
for(j in 2:(31)){
  if(j %in% indicesYield){
    lines(1:60,c(YieldSensMatrix[1:60,j]), col=colourarray[which(uniqueindices %in% j)], lwd="2") 
  } else {
    lines(1:60,c(YieldSensMatrix[1:60,j]), col="gray")
  }
}
title("Yield")
legend("topleft", legend=indicesYield, fill = colourarray[which(uniqueindices %in% indicesYield)], col = colourarray[which(uniqueindices %in% indicesYield)], title="Param.")
#
par(xpd=TRUE) #square
if(1 %in% indicesLAI){
  plot(xlim = c(1,60), ylim=c(0,0.3), c(LAISensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col=colourarray[which(uniqueindices %in% 1)], lwd="2") 
} else {
  plot(xlim = c(1,60), ylim=c(0,0.3), c(LAISensMatrix[1:60,1]), xlab="Simulation day", ylab="S_chi", type="l", col="gray")
}
for(j in 2:(31)){
  if(j %in% indicesLAI){
    lines(1:60,c(LAISensMatrix[1:60,j]), col=colourarray[which(uniqueindices %in% j)], lwd="2") 
  } else {
    lines(1:60,c(LAISensMatrix[1:60,j]), col="gray")
  }
}
title("Leaf area index")
legend("topleft", legend=indicesLAI, fill = colourarray[which(uniqueindices %in% indicesLAI)], col = colourarray[which(uniqueindices %in% indicesLAI)], title="Param.")
#
