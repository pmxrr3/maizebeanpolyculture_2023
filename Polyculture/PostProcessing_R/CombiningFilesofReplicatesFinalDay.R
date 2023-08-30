# Code to: 
#   1. Add indication of replica number. 
#   2. combine output files in the correct way. 
#   3. Outputs file with all effects of all combinations (in right way so we can calculate effects afterwards?)

# Initialize
#install.packages('matrixStats')
library(matrixStats)

field_Rep_1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_1.txt'
field_Rep_2 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_2.txt'
field_Rep_3 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_3.txt'
parameterbounds <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Input/ParametersBean.txt'
simulationpoints <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Input/SimulationPoints.txt'
outputMeasuresYield <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_yield.txt'
outputMeasuresBiom <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_biomPeak.txt'
outputMeasuresLAI <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Measures_Rep_1to3_LAIPeak.txt'
outputEffectsYield <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_yield.txt'
outputEffectsBiom <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_biomPeak.txt'
outputEffectsLAI <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/Effects_Rep_1to3_LAIPeak.txt'

nrOfParameters <- 6
nrOfSimDays <- 100 # or 100-1? I think 100 is fine, but should check.
nrOfTrajectories <- 20 
nrOfSimsPerRep <- nrOfTrajectories*(nrOfParameters+1)

# Read in each of the 3 output data files for each full replicate; make 3 new data files with only harvest day outputs, adding which replicate they came from
df_field_Rep_1 <- read.table(field_Rep_1, header = TRUE)
df_field_Rep_2 <- read.table(field_Rep_2, header = TRUE)
df_field_Rep_3 <- read.table(field_Rep_3, header = TRUE)#read.table(field_Rep_3)

Rep_1_harvest_nrOfSimDays <- as.matrix(df_field_Rep_1[df_field_Rep_1[ ,1] == "nrOfSimDays", ])
Rep_1_harvest_nrOfSimDays <- cbind(Rep_1_harvest_nrOfSimDays, rep = rep(1,nrow(Rep_1_harvest_nrOfSimDays)))
colnames(Rep_1_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
Rep_2_harvest_nrOfSimDays <- as.matrix(df_field_Rep_2[df_field_Rep_2[ ,1] == "nrOfSimDays", ])
Rep_2_harvest_nrOfSimDays <- cbind(Rep_2_harvest_nrOfSimDays, rep = rep(2,nrow(Rep_2_harvest_nrOfSimDays)))
colnames(Rep_2_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
Rep_3_harvest_nrOfSimDays <- as.matrix(df_field_Rep_3[df_field_Rep_3[ ,1] == "nrOfSimDays", ])
Rep_3_harvest_nrOfSimDays <- cbind(Rep_3_harvest_nrOfSimDays, rep = rep(3,nrow(Rep_3_harvest_nrOfSimDays)))
colnames(Rep_3_harvest_nrOfSimDays) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")


# Combine outputs of different replicates into effects (columns for different combinations?); add mean effect too?
# per output?
allyieldoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 8], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 8], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 8] )), 2, as.numeric)
allbiomoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 7], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 7], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 7] )), 2, as.numeric)
allLAIoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_nrOfSimDays[, 3], "Rep 2" =  Rep_2_harvest_nrOfSimDays[, 3], "Rep 3" = Rep_3_harvest_nrOfSimDays[, 3] )), 2, as.numeric)
library(matrixStats)
allyieldoutputs <- cbind(allyieldoutputs, rowMeans(allyieldoutputs)) # First average outputs (mean at each sim. point), then calculate effects (as an alternative)
allbiomoutputs <- cbind(allbiomoutputs, rowMeans(allbiomoutputs))
allLAIoutputs <- cbind(allLAIoutputs, rowMeans(allLAIoutputs))
dfParameterbounds <- read.table(parameterbounds, comment.char = "", header = TRUE) #read in delta-values and min_i, max_i
dfSimulationpoints <- read.table(simulationpoints) #read in step-sizes

# Calculate relative standard deviation for replicates at each simulation point and make some histograms
RSDyield <- as.matrix(apply(allyieldoutputs[,0:2], 1, function(x) sd(x)/mean(x)))
RSDbiom <- as.matrix(apply(allbiomoutputs[,0:2], 1, function(x) sd(x)/mean(x)))
RSDLAI <- as.matrix(apply(allLAIoutputs[,0:2], 1, function(x) sd(x)/mean(x)))
hist(RSDyield, main="Relative standard deviation yield outputs day nrOfSimDays",
     xlab="RSD",
     xlim=c(0,0.1),
     breaks = 20,
     col="darkmagenta")
hist(RSDbiom, main="Relative standard deviation Biom outputs day nrOfSimDays",
     xlab="RSD",
     xlim=c(0,0.1),
     breaks = 20,
     col="darkmagenta")
hist(RSDLAI, main="Relative standard deviation LAI outputs day nrOfSimDays",
     xlab="RSD",
     xlim=c(0,0.1),
     breaks = 20,
     col="darkmagenta")


# calculate effects
# library(gtools)
# combinations <- expand.grid(rep(list(1:3),2)) 
# yieldAbsEffects <- rep(0,nrOfTrajectories*nrOfParameters)
# biomAbsEffects <- rep(0,nrOfTrajectories*nrOfParameters)
# LAIAbsEffects <- rep(0,nrOfTrajectories*nrOfParameters)
# yieldEffects <- rep(0,nrOfTrajectories*nrOfParameters)
# biomEffects <- rep(0,nrOfTrajectories*nrOfParameters)
# LAIEffects <- rep(0,nrOfTrajectories*nrOfParameters)
# temp <- rep(0,nrOfTrajectories*nrOfParameters)
# tempp <- temp
# temppp <- temp 
# temp4 <- temp 
# temp5 <- temp 
# temp6 <- temp 
# for(i in 1:nrow(combinations)){
#   for(j in 0:(nrOfTrajectories-1)){
#     for(z in 1:nrOfParameters){
#       if(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])>0){
#         # scaled absolute effects
#         temp[j*nrOfParameters+z] <-  abs(allyieldoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allyieldoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         temp[j*nrOfParameters+z] <- temp[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) #scale effects and incorporate step size
#         tempp[j*nrOfParameters+z] <-  abs(allbiomoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allbiomoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         tempp[j*nrOfParameters+z] <- tempp[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) 
#         temppp[j*nrOfParameters+z] <-  abs(allLAIoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allLAIoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         temppp[j*nrOfParameters+z] <- temppp[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) 
#         
#         # scaled effects
#         temp4[j*nrOfParameters+z] <-  allyieldoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allyieldoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]]
#         temp4[j*nrOfParameters+z] <- temp4[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) #scale effects and incorporate step size
#         temp5[j*nrOfParameters+z] <-  allbiomoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allbiomoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]]
#         temp5[j*nrOfParameters+z] <- temp5[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) 
#         temp6[j*nrOfParameters+z] <-  allLAIoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allLAIoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]]
#         temp6[j*nrOfParameters+z] <- temp6[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) 
#       } else {
#         # scaled absolute effects
#         temp[j*nrOfParameters+z] <-  abs(allyieldoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allyieldoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         temp[j*nrOfParameters+z] <- temp[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) #scale effects and incorporate step size
#         tempp[j*nrOfParameters+z] <-  abs(allbiomoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allbiomoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         tempp[j*nrOfParameters+z] <- tempp[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) 
#         temppp[j*nrOfParameters+z] <-  abs(allLAIoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allLAIoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         temppp[j*nrOfParameters+z] <- temppp[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) 
#         
#         # scaled effects
#         temp4[j*nrOfParameters+z] <-  -(allyieldoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allyieldoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         temp4[j*nrOfParameters+z] <- temp4[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) #scale effects and incorporate step size
#         temp5[j*nrOfParameters+z] <-  -(allbiomoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allbiomoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         temp5[j*nrOfParameters+z] <- temp5[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) 
#         temp6[j*nrOfParameters+z] <-  -(allLAIoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allLAIoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])
#         temp6[j*nrOfParameters+z] <- temp6[j*nrOfParameters+z]*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),])) 
#       }
#     }
#   }
#   yieldAbsEffects <- cbind(yieldAbsEffects,  temp)
#   biomAbsEffects <- cbind(biomAbsEffects,  tempp)
#   LAIAbsEffects <- cbind(LAIAbsEffects,  temppp)
#   yieldEffects <- cbind(yieldEffects,  temp4)
#   biomEffects <- cbind(biomEffects,  temp5)
#   LAIEffects <- cbind(LAIEffects,  temp6)
# }
# yieldAbsEffects <- yieldAbsEffects[,-1]   #remove first column
# biomAbsEffects <- biomAbsEffects[,-1]   #remove first column
# LAIAbsEffects <- LAIAbsEffects[,-1]   #remove first column
# yieldEffects <- yieldEffects[,-1]   #remove first column
# biomEffects <- biomEffects[,-1]   #remove first column
# LAIEffects <- LAIEffects[,-1]   #remove first column
combinations <- rbind(expand.grid(rep(list(1:3),2)),c(4,4)) # 4,4 is the row for first average outputs then calculate effects
yieldAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allyieldoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allyieldoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
biomAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allbiomoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allbiomoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
LAIAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) abs(allLAIoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allLAIoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
yieldEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allyieldoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allyieldoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
biomEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allbiomoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allbiomoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
LAIEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:(nrOfTrajectories-1), function(j) sapply(1:nrOfParameters, function(z) sign(max(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))*(allLAIoutputs[j*(nrOfParameters+1)+1,combinations[i,1]]-allLAIoutputs[j*(nrOfParameters+1)+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*(nrOfParameters+1)+1,]-dfSimulationpoints[j*(nrOfParameters+1)+(z+1),]))))))
  
### Test 13/01/22 Remove outliers (i.e. those with very small (relative) step size delta)
################### calculate deltas 
  deltatest <- matrix(0,nrOfTrajectories,nrOfParameters)
  for(ii in 1:nrOfParameters){
    if(ii==nrOfParameters){
      temptest <- abs(dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 1,ii]-dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 0,ii])
    } else {
      temptest <- abs(dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == 1,ii]-dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% (nrOfParameters+1) == (ii+1),ii])
    }
    deltatest[,ii] <- temptest
  }
  #Find quartiles + IQR to find outliers 
  deltatestquantiles <- apply(deltatest,2,function(x) quantile(x, probs=c(.25, .75), na.rm = FALSE))
  #deltatestIQR <- apply(deltatest,2,function(x) IQR(x, na.rm = FALSE))
  #up <-  deltatestquantiles[2,]+1.5*deltatestIQR # Upper Range  #n/a
  # low<- deltatestquantiles[1,]-1.5*deltatestIQR # Lower Range� #Doesn't work because non-negative values only.
  low <- deltatestquantiles[1,]*0.02 # lower range for outliers as % of 25%-quantile
  
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
  #Option 2: Leave e.g. YieldEffects as is, and take means only over appropriate rows in calc of e.g. yieldAbsMeanEffects etc.
### END TEST 

# add means and medians of absolute effects over all combinations (columns);
# calculating S_i over mean/median column instead of taking average/median of S_i's of all combinations seems better.
  library(matrixStats)
  yieldAbsEffects <- cbind(yieldAbsEffects[,-ncol(yieldAbsEffects)], rowMeans(yieldAbsEffects[,-ncol(yieldAbsEffects)]), rowMedians(yieldAbsEffects[,-ncol(yieldAbsEffects)]),yieldAbsEffects[,ncol(yieldAbsEffects)]) # don't include first average outputs column in mean/median calcs, but it is added as the last column seperately
  biomAbsEffects <- cbind(biomAbsEffects[,-ncol(biomAbsEffects)], rowMeans(biomAbsEffects[,-ncol(biomAbsEffects)]), rowMedians(biomAbsEffects[,-ncol(biomAbsEffects)]),biomAbsEffects[,ncol(biomAbsEffects)])
  LAIAbsEffects <- cbind(LAIAbsEffects[,-ncol(LAIAbsEffects)], rowMeans(LAIAbsEffects[,-ncol(LAIAbsEffects)]), rowMedians(LAIAbsEffects[,-ncol(LAIAbsEffects)]),LAIAbsEffects[,ncol(LAIAbsEffects)])
  yieldEffects <- cbind(yieldEffects[,-ncol(yieldEffects)], rowMeans(yieldEffects[,-ncol(yieldEffects)]), rowMedians(yieldEffects[,-ncol(yieldEffects)]),yieldEffects[,ncol(yieldEffects)])
  biomEffects <- cbind(biomEffects[,-ncol(biomEffects)], rowMeans(biomEffects[,-ncol(biomEffects)]), rowMedians(biomEffects[,-ncol(biomEffects)]),biomEffects[,ncol(biomEffects)])
  LAIEffects <- cbind(LAIEffects[,-ncol(LAIEffects)], rowMeans(LAIEffects[,-ncol(LAIEffects)]), rowMedians(LAIEffects[,-ncol(LAIEffects)]),LAIEffects[,ncol(LAIEffects)])
#Write absolute scaled effects to file:
# TO DO? Can do, but to different file.

##########
#### Run from here for write to file
##########

# Scaled but dimensional measures mu* and chi
yieldAbsMeanEffects <- matrix(0,nrOfParameters,ncol(yieldAbsEffects))
yieldAbsMedianEffects <- matrix(0,nrOfParameters,ncol(yieldAbsEffects))
yieldSigma <- matrix(0,nrOfParameters,ncol(yieldAbsEffects))
yieldMeanEffects <- matrix(0,nrOfParameters,ncol(yieldAbsEffects))
biomAbsMeanEffects <- matrix(0,nrOfParameters,ncol(biomAbsEffects))
biomAbsMedianEffects <- matrix(0,nrOfParameters,ncol(biomAbsEffects))
biomSigma <- matrix(0,nrOfParameters,ncol(biomAbsEffects))
biomMeanEffects <- matrix(0,nrOfParameters,ncol(biomAbsEffects))
LAIAbsMeanEffects <- matrix(0,nrOfParameters,ncol(LAIAbsEffects))  #mean(yieldAbsEffects[which(yieldeffects[,j]%%nrOfParameters==i & !(yieldeffects[,j] %in% indices[indices[,1]==j,2])])
LAIAbsMedianEffects <- matrix(0,nrOfParameters,ncol(LAIAbsEffects))
LAISigma <- matrix(0,nrOfParameters,ncol(LAIAbsEffects))
LAIMeanEffects <- matrix(0,nrOfParameters,ncol(LAIAbsEffects))

for(j in 1:ncol(yieldAbsEffects)){
  for(i in 1:nrOfParameters){
    if(length(indices[indices[,1]==i,2])!=0){
      #if(j==1){ print(length(yieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]))}
      yieldAbsMeanEffects[i,j] <- mean(yieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])       #TEST 13/01/22 remove outliers  #mean(yieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      yieldAbsMedianEffects[i,j] <- median(yieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      biomAbsMeanEffects[i,j] <- mean(biomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
      biomAbsMedianEffects[i,j] <- median(biomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      LAIAbsMeanEffects[i,j] <- mean(LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
      LAIAbsMedianEffects[i,j] <- median(LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      
      yieldSigma[i,j] <- sd(yieldEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]) #sigma over non-absolute (scaled) effects
      biomSigma[i,j] <- sd(biomEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])]) # filter outliers
      LAISigma[i,j] <- sd(LAIEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      
      yieldMeanEffects[i,j] <- mean(yieldEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      biomMeanEffects[i,j] <- mean(biomEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
      LAIMeanEffects[i,j] <- mean(LAIEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j][-(indices[indices[,1]==i,2])])
    } else {
      #if(j==1){ print(length(yieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j]))}
      yieldAbsMeanEffects[i,j] <- mean(yieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j]) #scaled dimensionless
      yieldAbsMedianEffects[i,j] <- median(yieldAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      biomAbsMeanEffects[i,j] <- mean(biomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      biomAbsMedianEffects[i,j] <- median(biomAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      LAIAbsMeanEffects[i,j] <- mean(LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])  #scaled dimensionless
      LAIAbsMedianEffects[i,j] <- median(LAIAbsEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      
      yieldSigma[i,j] <- sd(yieldEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      biomSigma[i,j] <- sd(biomEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      LAISigma[i,j] <- sd(LAIEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      
      yieldMeanEffects[i,j] <- mean(yieldEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      biomMeanEffects[i,j] <- mean(biomEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
      LAIMeanEffects[i,j] <- mean(LAIEffects[nrOfParameters*(0:(nrOfTrajectories-1))+i,j])
    }
  }
}
yieldSens <- cbind( yieldAbsMeanEffects, yieldAbsMedianEffects ) #mu*, chi
biomSens <- cbind( biomAbsMeanEffects, biomAbsMedianEffects ) #mu*, chi
LAISens <- cbind( LAIAbsMeanEffects, LAIAbsMedianEffects )  #mu*, chi
yieldMeanEffects <- abs(yieldMeanEffects)
biomMeanEffects <- abs(biomMeanEffects)
LAIMeanEffects <- abs(LAIMeanEffects)

#Write original sensitivity measures to file?
#  Yes, but only scaled mu*, chi and sigma. mu is obsolete; unscaled measures are obsolete.
line <- sprintf("***Non-normalized (and dimensional) sensitivity indices mu*, chi and sigma per parameter (rows) and combination (columns):\n Combi 1-11 are mu*; 12-22 are Chi. Sigma is separate table. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n Mu* & chi:")
write(line,file=outputMeasuresYield,append=FALSE)
write(line,file=outputMeasuresBiom,append=FALSE)
write(line,file=outputMeasuresLAI,append=FALSE)
cat("Combination:", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(yieldSens), file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(yieldSens,digits=14, format="e"), file=outputMeasuresYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(biomSens), file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(biomSens,digits=14, format="e"), file=outputMeasuresBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(LAISens), file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(LAISens,digits=14, format="e"), file=outputMeasuresLAI, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
line <- sprintf("\n Sigma:")
write(line,file=outputMeasuresYield,append=TRUE)
write(line,file=outputMeasuresBiom,append=TRUE)
write(line,file=outputMeasuresLAI,append=TRUE)
cat("Combination:", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(yieldSigma), file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(yieldSigma,digits=14, format="e"), file=outputMeasuresYield,  sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(biomSigma), file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(biomSigma,digits=14, format="e"), file=outputMeasuresBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(LAISigma), file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(LAISigma,digits=14, format="e"), file=outputMeasuresLAI, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

#normalize sensitivity indices
yieldSens <- apply(yieldSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
biomSens <- apply(biomSens, 2, function(x) x/sum(x)) #S_mu*, S_chi
LAISens <- apply(LAISens, 2, function(x) x/sum(x)) #S_mu*, S_chi

#Write normalized sensitivities to file:
line <- sprintf("\n ***Normalized sensitivity indices per parameter (rows):\n combinations (columns) 1-11 are based on S_mu, (columns) 12-22 on S_chi. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n")
write(line,file=outputMeasuresYield,append=TRUE)
write(line,file=outputMeasuresBiom,append=TRUE)
write(line,file=outputMeasuresLAI,append=TRUE)
cat("Combination:", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(yieldSens), file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(yieldSens,digits=14, format="e"), file=outputMeasuresYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(biomSens), file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(biomSens,digits=14, format="e"), file=outputMeasuresBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(LAISens), file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(LAISens,digits=14, format="e"), file=outputMeasuresLAI, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

#biomSens <- cbind( biomAbsMeanEffects/sum(biomAbsMeanEffects), biomAbsMedianEffects/sum(biomAbsMedianEffects) ) #S_mu*, S_chi
#LAISens <- cbind( LAIAbsMeanEffects/sum(LAIAbsMeanEffects), LAIAbsMedianEffects/sum(LAIAbsMedianEffects) ) #S_mu*, S_chi
# apply(yieldSens, 2, function(x) which(x==max(x, na.rm = TRUE)))
# apply(biomSens, 2, function(x) which(x==max(x, na.rm = TRUE)))
# apply(LAISens, 2, function(x) which(x==max(x, na.rm = TRUE)))
sortedYieldSens <- apply(yieldSens, 2, sort)
sortedBiomSens <- apply(biomSens, 2, sort)
sortedLAISens <- apply(LAISens, 2, sort)
orderYield <- apply(yieldSens, 2, function(x) order(x)) #ranking of parameters (least to most important) per column. See also Excel for example
orderBiom <- apply(biomSens, 2, function(x) order(x))
orderLAI <- apply(LAISens, 2, function(x) order(x))
# colnames(orderYield) <- c("Comb. 1",2:ncol(orderYield))
# colnames(orderBiom) <- c(1:ncol(orderBiom))
# colnames(orderLAI) <- c(1:ncol(orderLAI))

#write ordered parameters to file:
line <- sprintf("\n\n***Ordered parameters (least to most important): combinations (columns) 1-11 are based on S_mu, (columns) 12-22 on S_chi.\n Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n")
write(line,file=outputMeasuresYield,append=TRUE)
write(line,file=outputMeasuresBiom,append=TRUE)
write(line,file=outputMeasuresLAI,append=TRUE)
cat("Combination:", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(orderYield),"\r", file=outputMeasuresYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderYield, file=outputMeasuresYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Combination:", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(orderBiom),"\r", file=outputMeasuresBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderBiom, file=outputMeasuresBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
cat("Combination:", file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:ncol(orderLAI),"\r", file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "\t")
write.table(orderLAI, file=outputMeasuresLAI, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)

# Calculate (un)important groups based on h-levels
line <- sprintf("\n\n ***(Un)important parameters: combinations 1-11 are based on S_mu, 12-22 on S_chi. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n\n")
write(line,file=outputMeasuresYield,append=TRUE)
write(line,file=outputMeasuresBiom,append=TRUE)
write(line,file=outputMeasuresLAI,append=TRUE)

for(h in c(0.05,0.1,0.15,0.2,0.25,0.3)){ #0.05,0.1,0.15,0.2,0.25,
  line <- sprintf(" ---Combinations for h-level %.2f---",h)
  write(line,file=outputMeasuresYield,append=TRUE)
  write(line,file=outputMeasuresBiom,append=TRUE)
  write(line,file=outputMeasuresLAI,append=TRUE)
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
    tempa <- orderLAI[1:max(which(cumsum(sortedLAISens[,j])<h)),j] #unimportant 
    tempb <- sortedLAISens[1:max(which(cumsum(sortedLAISens[,j])<h)),j]
    tempc <- orderLAI[which(as.vector(sortedLAISens[,j])>(mean(tempb)+3*sd(tempb))),j] #important
    line <- sprintf("Unimportant parameters for output LAI, combination %i:",j)
    write(line,file=outputMeasuresLAI,append=TRUE)
    cat(tempa, file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
    line <- sprintf("Important parameters for output LAI, combination %i:",j)
    write(line,file=outputMeasuresLAI,append=TRUE)
    cat(tempc, "\n", file=outputMeasuresLAI, fill = TRUE, labels = NULL, append = TRUE, sep = ", ")
  }
}
