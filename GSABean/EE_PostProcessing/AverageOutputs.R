# Create file with average outputs over 3 replicates.

field_Rep_1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_1.txt'
field_Rep_2 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_2.txt'
field_Rep_3 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/field_Rep_3.txt'
plant_Rep_1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/plant_Rep_1.txt'
plant_Rep_2 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/plant_Rep_2.txt'
plant_Rep_3 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/GSA/Output/plant_Rep_3.txt'

df_field_Rep_1 <- read.table(field_Rep_1, header = TRUE)
df_field_Rep_2 <- read.table(field_Rep_2, header = TRUE)
df_field_Rep_3 <- read.table(field_Rep_3, header = TRUE)
# df_plant_Rep_1 <- read.table(plant_Rep_1, header = TRUE)
# df_plant_Rep_2 <- read.table(plant_Rep_2, header = TRUE)
# df_plant_Rep_3 <- read.table(plant_Rep_3, header = TRUE)

nrOfParameters <- 31
nrOfSimDays <- 60 # or 100-1? I think 100 is fine, but should check.
nrOfTrajectories <- 20 
nrOfSimsPerRep <- nrOfTrajectories*(nrOfParameters+1)

#Average field outputs
AllOutputsAveragedOverReplicates <- sapply(1:nrow(df_field_Rep_1), function(i) sapply(3:11, function(j) mean(c(df_field_Rep_1[i, j],df_field_Rep_2[i, j],df_field_Rep_3[i, j]),na.rm = TRUE) ))
AllOutputsAveragedOverReplicates <- cbind(df_field_Rep_1[,1:2],t(AllOutputsAveragedOverReplicates))
colnames(AllOutputsAveragedOverReplicates) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
AllOutputsAverageAverage <- sapply(3:11, function(i) sapply(1:nrOfSimDays, function(j) mean( AllOutputsAveragedOverReplicates[j + 0:(nrOfSimsPerRep-1)*nrOfSimDays,i] ) ))
AllOutputsAverageAverage <- cbind(AllOutputsAveragedOverReplicates[1:nrOfSimDays,1:2]  , AllOutputsAverageAverage)
colnames(AllOutputsAverageAverage) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
#Average plant outputs


#Plots of field-level outputs
###
# for-loop to plot mean +- std for each of the 100 days? for a given (or multiple) output
#ylablist <- c('Germination delay bean', 'Internode length bean', 'Helix inclination', 'Branching probability', 'Time to flower')
#opar <- par(lwd=2)
ylimitvector <- c(9,250,.8,2800000,2200,1000,.8,90,1.2)
xlimitvector <- c(1,1,1,1,1,35,35,1,1)
for(i in 3:11) {
  par(pty="s") #square
  plot(xlim = c(xlimitvector[i-2],nrOfSimDays), ylim=c(0,ylimitvector[i-2]), c(AllOutputsAveragedOverReplicates[1:nrOfSimDays,i]), xlab="Simulation day", ylab=colnames(AllOutputsAveragedOverReplicates)[i], type="l")
  for(j in 2:nrOfSimsPerRep){
  lines(1:nrOfSimDays,c(AllOutputsAveragedOverReplicates[j*nrOfSimDays+1:nrOfSimDays,i]))
  # lines(1:100,c(Mean_beandata_poly_1[i*100+1:100,3]), col="red")
  }
  lines(1:nrOfSimDays,c(AllOutputsAverageAverage[1:nrOfSimDays,i]), col="red")
  title(colnames(AllOutputsAveragedOverReplicates)[i])
}
#legend("topleft", c("Maize", "Bean"), col=c("black", "red"), lwd=2)
#par(opar)

