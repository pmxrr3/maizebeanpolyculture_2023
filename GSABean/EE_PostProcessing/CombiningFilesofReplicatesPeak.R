# Code to: 
#   1. Add indication of replica number. 
#   2. combine output files in the correct way. 
#   3. Outputs file with all effects of all combinations (in right way so we can calculate effects afterwards?)

# Initialize
#install.packages('matrixStats')

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

# Read in each of the 3 output data files for each full replicate; make 3 new data files with only harvest day outputs, adding which replicate they came from
df_field_Rep_1 <- read.table(field_Rep_1, header = TRUE)
df_field_Rep_2 <- read.table(field_Rep_2, header = TRUE)
df_field_Rep_3 <- read.table(field_Rep_3, header = TRUE)#read.table(field_Rep_3)

# Find day where LAI reaches maximum for each simulation; save those days and line numbers in vectors
maxLAIRep1 <- sapply(1:2120, function(x) max(df_field_Rep_1[(x-1)*159+1:159, 3]) ) #as.matrix to make sure it's in global environment
maxLAIRep2 <- sapply(1:2120, function(x) max(df_field_Rep_2[(x-1)*159+1:159, 3]) )
maxLAIRep3 <- sapply(1:2120, function(x) max(df_field_Rep_3[(x-1)*159+1:159, 3]) )
maxLAIvalues <- cbind(maxLAIRep1,maxLAIRep2,maxLAIRep3)
maxLAIindexRep1 <- unlist(sapply(1:2120, function(x) (x-1)*159+which.max(df_field_Rep_1[(x-1)*159+1:159, 3]))) #Unlist to list->matrix
maxLAIindexRep2 <- unlist(sapply(1:2120, function(x) (x-1)*159+which.max(df_field_Rep_2[(x-1)*159+1:159, 3])))
maxLAIindexRep3 <- unlist(sapply(1:2120, function(x) (x-1)*159+which.max(df_field_Rep_3[(x-1)*159+1:159, 3])))
maxLAIindices <- cbind(maxLAIindexRep1,maxLAIindexRep2,maxLAIindexRep3)
# Find day where biom reaches maximum for each simulation; save those days and line numbers in vectors
maxbiomRep1 <- sapply(1:2120, function(x) max(df_field_Rep_1[(x-1)*159+1:159, 7]) ) #as.matrix to make sure it's in global environment
maxbiomRep2 <- sapply(1:2120, function(x) max(df_field_Rep_2[(x-1)*159+1:159, 7]) )
maxbiomRep3 <- sapply(1:2120, function(x) max(df_field_Rep_3[(x-1)*159+1:159, 7]) )
maxbiomvalues <- cbind(maxbiomRep1,maxbiomRep2,maxbiomRep3)
maxbiomindexRep1 <- unlist(sapply(1:2120, function(x) (x-1)*159+which.max(df_field_Rep_1[(x-1)*159+1:159, 7]))) #Unlist to list->matrix
maxbiomindexRep2 <- unlist(sapply(1:2120, function(x) (x-1)*159+which.max(df_field_Rep_2[(x-1)*159+1:159, 7])))
maxbiomindexRep3 <- unlist(sapply(1:2120, function(x) (x-1)*159+which.max(df_field_Rep_3[(x-1)*159+1:159, 7])))
maxbiomindices <- cbind(maxbiomindexRep1,maxbiomindexRep2,maxbiomindexRep3)

Rep_1_harvest_159 <- as.matrix(df_field_Rep_1[df_field_Rep_1[ ,1] == "159", ])
Rep_1_harvest_159[,3] <- df_field_Rep_1[maxLAIindexRep1, 3]  # change LAI outputs to the max values
Rep_1_harvest_159[,7] <- df_field_Rep_1[maxbiomindexRep1, 7]  # change biom outputs to the max values
Rep_1_harvest_159 <- cbind(Rep_1_harvest_159, rep = rep(1,nrow(Rep_1_harvest_159))) #add rep number
colnames(Rep_1_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_2_harvest_159 <- as.matrix(df_field_Rep_2[df_field_Rep_2[ ,1] == "159", ])
Rep_2_harvest_159[,3] <- df_field_Rep_2[maxLAIindexRep2, 3]
Rep_2_harvest_159[,7] <- df_field_Rep_2[maxbiomindexRep2, 7]  
Rep_2_harvest_159 <- cbind(Rep_2_harvest_159, rep = rep(2,nrow(Rep_2_harvest_159)))
colnames(Rep_2_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_3_harvest_159 <- as.matrix(df_field_Rep_3[df_field_Rep_3[ ,1] == "159", ])
Rep_3_harvest_159[,3] <- df_field_Rep_3[maxLAIindexRep3, 3]
Rep_3_harvest_159[,7] <- df_field_Rep_3[maxbiomindexRep3, 7]  
Rep_3_harvest_159 <- cbind(Rep_3_harvest_159, rep = rep(3,nrow(Rep_3_harvest_159)))
colnames(Rep_3_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")


# Combine outputs of different replicates into effects (columns for different combinations?); add mean effect too?
# per output?
allyieldoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_159[, 8], "Rep 2" =  Rep_2_harvest_159[, 8], "Rep 3" = Rep_3_harvest_159[, 8] )), 2, as.numeric)
allbiomoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_159[, 7], "Rep 2" =  Rep_2_harvest_159[, 7], "Rep 3" = Rep_3_harvest_159[, 7] )), 2, as.numeric)
allLAIoutputs <- apply(as.matrix(data.frame("Rep 1" = Rep_1_harvest_159[, 3], "Rep 2" =  Rep_2_harvest_159[, 3], "Rep 3" = Rep_3_harvest_159[, 3] )), 2, as.numeric)
library(matrixStats)
allyieldoutputs <- cbind(allyieldoutputs, rowMeans(allyieldoutputs)) # First average outputs (mean at each sim. point), then calculate effects (as an alternative)
allbiomoutputs <- cbind(allbiomoutputs, rowMeans(allbiomoutputs))
allLAIoutputs <- cbind(allLAIoutputs, rowMeans(allLAIoutputs))
dfParameterbounds <- read.table(parameterbounds, comment.char = "", header = TRUE) #read in delta-values and min_i, max_i
dfSimulationpoints <- read.table(simulationpoints) #read in step-sizes

# Calculate relative standard deviation of outputs for the 3 replicates at each simulation point and make some histograms
RSDyield <- as.matrix(apply(allyieldoutputs[,0:2], 1, function(x) 100*sd(x)/mean(x)))
RSDbiom <- as.matrix(apply(allbiomoutputs[,0:2], 1, function(x) 100*sd(x)/mean(x)))
RSDLAI <- as.matrix(apply(allLAIoutputs[,0:2], 1, function(x) 100*sd(x)/mean(x)))
c1 <- rgb(90,120,255,max = 255, alpha = 255, names = "lt.orange") #blue
c2 <- rgb(192,192,192, max = 255, alpha = 255, names = "lt.grey") 
c3 <- rgb(255,0,0, max = 255, alpha = 255, names = "lt.gold") #red
hgA <- hist(RSDyield, 
            xlab="RSD",
            xlim=c(0,10),
            breaks = 20,
            plot = FALSE)
hgB <- hist(RSDbiom, 
            plot = FALSE)
hgC <- hist(RSDLAI, 
            plot = FALSE)
opar <- par(lwd=2)
plot(hgB, border = c1, xlim=c(0,10),xlab="RSD",ylim=c(0,1200), main=NULL) # Plot 1st histogram using a transparent color
plot(hgA, border = "black", add = TRUE) # Add 2nd histogram using different color
plot(hgC, border = "red", add = TRUE)
legend("topright", c("Peak biomass", "Yield", "Peak LAI"), col=c(c1, "black", "red"), lwd=2)
par(opar)

library(gtools)
combinations <- rbind(expand.grid(rep(list(1:3),2)),c(4,4)) # 4,4 is the row for first average outputs then calculate effects
yieldAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:39, function(j) sapply(1:52, function(z) abs(allyieldoutputs[j*53+1,combinations[i,1]]-allyieldoutputs[j*53+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))))))
biomAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:39, function(j) sapply(1:52, function(z) abs(allbiomoutputs[j*53+1,combinations[i,1]]-allbiomoutputs[j*53+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))))))
LAIAbsEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:39, function(j) sapply(1:52, function(z) abs(allLAIoutputs[j*53+1,combinations[i,1]]-allLAIoutputs[j*53+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))))))
yieldEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:39, function(j) sapply(1:52, function(z) sign(max(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))*(allyieldoutputs[j*53+1,combinations[i,1]]-allyieldoutputs[j*53+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))))))
biomEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:39, function(j) sapply(1:52, function(z) sign(max(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))*(allbiomoutputs[j*53+1,combinations[i,1]]-allbiomoutputs[j*53+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))))))
LAIEffects <- sapply(1:nrow(combinations), function(i) c(sapply(0:39, function(j) sapply(1:52, function(z) sign(max(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))*(allLAIoutputs[j*53+1,combinations[i,1]]-allLAIoutputs[j*53+(z+1),combinations[i,2]])*(dfParameterbounds[z,3]-dfParameterbounds[z,2])/ max(abs(dfSimulationpoints[j*53+1,]-dfSimulationpoints[j*53+(z+1),]))))))

### Remove outliers (i.e. those with very small (relative) step size delta)
################### calculate deltas 
deltatest <- matrix(0,40,52)
simpointsSortedBase <- matrix(0,40,52)
simpointsSortedPert <- matrix(0,40,52)
for(ii in 1:52){
  if(ii==52){
    temptest <- abs(dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% 53 == 1,ii]-dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% 53 == 0,ii])
    temptest2 <- dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% 53 == 1,ii]
    temptest3 <- dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% 53 == 0,ii]
  } else {
    temptest <- abs(dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% 53 == 1,ii]-dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% 53 == (ii+1),ii])
    temptest2 <- dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% 53 == 1,ii]
    temptest3 <- dfSimulationpoints[seq_len(nrow(dfSimulationpoints)) %% 53 == (ii+1),ii]
  }
  deltatest[,ii] <- temptest
  simpointsSortedBase[,ii] <- temptest2
  simpointsSortedPert[,ii] <- temptest3
}
# Option 1: those step sizes <2% of 25%-quantile for that input (so out of 40 step sizes)
  #deltatestquantiles <- apply(deltatest,2,function(x) quantile(x, probs=c(.25, .75), na.rm = FALSE))
  #low <- deltatestquantiles[1,]*0.02 # lower range for outliers as % of 25%-quantile
# Option 2: assuming coordinates from QR sequence are iid Unif, step sizes |delta| follow triangle distribution. Requiring CDF(|delta|)>0.005 gives outliers are those for which |delta|<1-\sqrt{398}/20 \approx 0.0025.
  low <- rep(1,52)*(1-sqrt(398)/20)
  for(i in 1:52){
    low[i] <- low[i]*(dfParameterbounds[i,3]-dfParameterbounds[i,2])
  }
  
#Outliers
nroutliers <- matrix(0,52,2) #Number of outliers per parameter
nroutliers[,1] <- 1:52
indices <- c(0,0) #These effects should not be taken into account.
for(i in 1:52){
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

# Also make table (for first average outputs) with absolute scaled effects per input (easier for writing to file)
yieldAbsEffectsNiceReadScaled <- matrix(0,40,52)
biomPeakAbsEffectsNiceReadScaled <- matrix(0,40,52)
LAIPeakAbsEffectsNiceReadScaled <- matrix(0,40,52)
yieldAbsEffectsNiceReadUnscaled <- matrix(0,40,52)
biomPeakAbsEffectsNiceReadUnscaled <- matrix(0,40,52)
LAIPeakAbsEffectsNiceReadUnscaled <- matrix(0,40,52)
for(i in 1:52){
  yieldAbsEffectsNiceReadScaled[,i] <- yieldAbsEffects[52*(0:39)+i,ncol(yieldAbsEffects)]
  biomPeakAbsEffectsNiceReadScaled[,i] <- biomAbsEffects[52*(0:39)+i,ncol(biomAbsEffects)]
  LAIPeakAbsEffectsNiceReadScaled[,i] <- LAIAbsEffects[52*(0:39)+i,ncol(LAIAbsEffects)]
  yieldAbsEffectsNiceReadUnscaled[,i] <- yieldAbsEffects[52*(0:39)+i,ncol(yieldAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2]) 
  biomPeakAbsEffectsNiceReadUnscaled[,i] <- biomAbsEffects[52*(0:39)+i,ncol(biomAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2]) 
  LAIPeakAbsEffectsNiceReadUnscaled[,i] <- LAIAbsEffects[52*(0:39)+i,ncol(LAIAbsEffects)]/(dfParameterbounds[i,3]-dfParameterbounds[i,2])
}

##########
#### Run from here for write to file
##########

# Write absolute scaled effects to file (only for average outputs first, then calculate effects; 3 tables of 40*52)
# TO DO are these scaled or unscaled? In principle scaled, but can unscale them.
line <- sprintf("Absolute (scaled and unscaled) effects; first averaged outputs of 3 replicates.")
write(line,file=outputEffectsYield,append=FALSE)
write(line,file=outputEffectsBiom,append=FALSE)
write(line,file=outputEffectsLAI,append=FALSE)
line <- sprintf("\n Unscaled absolute effects:")
write(line,file=outputEffectsYield,append=TRUE)
write(line,file=outputEffectsBiom,append=TRUE)
write(line,file=outputEffectsLAI,append=TRUE)
cat("Yield:", file=outputEffectsYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:52, file=outputEffectsYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(yieldAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Biom peak:", file=outputEffectsBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:52, file=outputEffectsBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(biomPeakAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("LAI peak:", file=outputEffectsLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:52, file=outputEffectsLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(yieldAbsEffectsNiceReadUnscaled,digits=10, format="e"), file=outputEffectsLAI, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
line <- sprintf("\n Scaled (in input direction) absolute effects:")
write(line,file=outputEffectsYield,append=TRUE)
write(line,file=outputEffectsBiom,append=TRUE)
write(line,file=outputEffectsLAI,append=TRUE)
cat("Yield:", file=outputEffectsYield, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:52, file=outputEffectsYield, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(yieldAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsYield, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("Biom peak:", file=outputEffectsBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:52, file=outputEffectsBiom, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(biomPeakAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsBiom, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 
cat("LAI peak:", file=outputEffectsLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "")
cat(1:52, file=outputEffectsLAI, fill = TRUE, labels = NULL, append = TRUE, sep = "\t\t\t\t\t")
write.table(format(yieldAbsEffectsNiceReadScaled,digits=10, format="e"), file=outputEffectsLAI, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE) 



# Scaled but dimensional measures mu* and chi
yieldAbsMeanEffects <- matrix(0,52,ncol(yieldAbsEffects))
yieldAbsMedianEffects <- matrix(0,52,ncol(yieldAbsEffects))
yieldSigma <- matrix(0,52,ncol(yieldAbsEffects))
yieldMeanEffects <- matrix(0,52,ncol(yieldAbsEffects))
biomAbsMeanEffects <- matrix(0,52,ncol(biomAbsEffects))
biomAbsMedianEffects <- matrix(0,52,ncol(biomAbsEffects))
biomSigma <- matrix(0,52,ncol(biomAbsEffects))
biomMeanEffects <- matrix(0,52,ncol(biomAbsEffects))
LAIAbsMeanEffects <- matrix(0,52,ncol(LAIAbsEffects))  #mean(yieldAbsEffects[which(yieldeffects[,j]%%52==i & !(yieldeffects[,j] %in% indices[indices[,1]==j,2])])
LAIAbsMedianEffects <- matrix(0,52,ncol(LAIAbsEffects))
LAISigma <- matrix(0,52,ncol(LAIAbsEffects))
LAIMeanEffects <- matrix(0,52,ncol(LAIAbsEffects))

for(j in 1:ncol(yieldAbsEffects)){
  for(i in 1:52){
    if(length(indices[indices[,1]==i,2])!=0){
      #if(j==1){ print(length(yieldAbsEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])]))}
      yieldAbsMeanEffects[i,j] <- mean(yieldAbsEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])       #TEST 13/01/22 remove outliers  #mean(yieldAbsEffects[52*(0:39)+i,j])  #scaled dimensionless
      yieldAbsMedianEffects[i,j] <- median(yieldAbsEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])
      biomAbsMeanEffects[i,j] <- mean(biomAbsEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
      biomAbsMedianEffects[i,j] <- median(biomAbsEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])
      LAIAbsMeanEffects[i,j] <- mean(LAIAbsEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])  #scaled dimensionless
      LAIAbsMedianEffects[i,j] <- median(LAIAbsEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])
      
      yieldSigma[i,j] <- sd(yieldEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])]) #sigma over non-absolute (scaled) effects
      biomSigma[i,j] <- sd(biomEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])]) # filter outliers
      LAISigma[i,j] <- sd(LAIEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])
      
      yieldMeanEffects[i,j] <- mean(yieldEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])
      biomMeanEffects[i,j] <- mean(biomEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])
      LAIMeanEffects[i,j] <- mean(LAIEffects[52*(0:39)+i,j][-(indices[indices[,1]==i,2])])
    } else {
      #if(j==1){ print(length(yieldAbsEffects[52*(0:39)+i,j]))}
      yieldAbsMeanEffects[i,j] <- mean(yieldAbsEffects[52*(0:39)+i,j]) #scaled dimensionless
      yieldAbsMedianEffects[i,j] <- median(yieldAbsEffects[52*(0:39)+i,j])
      biomAbsMeanEffects[i,j] <- mean(biomAbsEffects[52*(0:39)+i,j])  #scaled dimensionless
      biomAbsMedianEffects[i,j] <- median(biomAbsEffects[52*(0:39)+i,j])
      LAIAbsMeanEffects[i,j] <- mean(LAIAbsEffects[52*(0:39)+i,j])  #scaled dimensionless
      LAIAbsMedianEffects[i,j] <- median(LAIAbsEffects[52*(0:39)+i,j])
      
      yieldSigma[i,j] <- sd(yieldEffects[52*(0:39)+i,j])
      biomSigma[i,j] <- sd(biomEffects[52*(0:39)+i,j])
      LAISigma[i,j] <- sd(LAIEffects[52*(0:39)+i,j])
      
      yieldMeanEffects[i,j] <- mean(yieldEffects[52*(0:39)+i,j])
      biomMeanEffects[i,j] <- mean(biomEffects[52*(0:39)+i,j])
      LAIMeanEffects[i,j] <- mean(LAIEffects[52*(0:39)+i,j])
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
line <- sprintf("***Non-normalized (and dimensional but scaled in input-direction) sensitivity indices mu*, chi and sigma per parameter (rows) and combination (columns):\n Combi 1-11 are mu*; 12-22 are Chi. Sigma is separate table. Combi 10 and 21 are means (per row) of 9 combinations, 11 and 22 are medians (per row) of 9 combinations.\n Mu* & chi:")
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
