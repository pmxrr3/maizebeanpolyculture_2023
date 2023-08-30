#install.packages("export")
#library(export)
# Create file with average outputs over 3 replicates.

maizeSingleSimtxt <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Polyculture OAT sims data/Output/field_Maize_Mono.txt'
field_OAT_Core_2_v1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Polyculture OAT sims data/Output/field_OAT_Core_7_v4.txt'


maizeSingleSim <- read.table(maizeSingleSimtxt, header = TRUE, fill = TRUE)
maizeSingleSim <- maizeSingleSim[,1:11]
df_field_OAT_Core_2_v1 <- read.table(field_OAT_Core_2_v1, header = TRUE, fill = TRUE)
df_field_OAT_Core_2_v1 <- df_field_OAT_Core_2_v1[,1:11]
maizeSingleSim[,6] <- maizeSingleSim[,6]/1000000 # assCO2 divided by million for better axis values (now in mol/m2)
df_field_OAT_Core_2_v1[,6] <- df_field_OAT_Core_2_v1[,6]/1000000
maizeSingleSim[,7] <- maizeSingleSim[,7]/1000 # biomass divided by 1000 for better axis values (now in kg/m2)
df_field_OAT_Core_2_v1[,7] <- df_field_OAT_Core_2_v1[,7]/1000

nrOfParameters <- 7
nrOfSimDays <- 200#130 # or 100-1? I think 100 is fine, but should check.
nrOfTrajectories <- 11 
nrOfSimsPerRep <- 11 #nrOfTrajectories*(nrOfParameters+1)

#Separate maize and bean values
maizeSingleSim <- maizeSingleSim[maizeSingleSim[,2]==9,]
df_field_OAT_Core_2_v1_maize <- df_field_OAT_Core_2_v1[df_field_OAT_Core_2_v1[,2]==9,]
df_field_OAT_Core_2_v1_bean <- df_field_OAT_Core_2_v1[df_field_OAT_Core_2_v1[,2]==16,]
colnames(df_field_OAT_Core_2_v1_maize) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
colnames(df_field_OAT_Core_2_v1_bean) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")


ylimitvector <- c(5,250,.7,1.3,1.2,1000,.8,90,1.2)
xlimitvector <- c(1,1,1,1,1,1,1,1,1)
for(i in c(6)) { #c(3,5,6,7,8)
  par(pty="s") #square
  plot(xlim = c(xlimitvector[i-2],nrOfSimDays), ylim=c(0,ylimitvector[i-2]), c(df_field_OAT_Core_2_v1_maize[1:nrOfSimDays,i]), xlab="", ylab="", type="l")
  # xlab="Simulation day", ylab=colnames(df_field_OAT_Core_2_v1_maize)[i],
  lines(1:nrOfSimDays,c(df_field_OAT_Core_2_v1_bean[1:nrOfSimDays,i]), col="red")
  for(j in 2:nrOfSimsPerRep){
    if(j == 6) {
      lines(1:nrOfSimDays,c(df_field_OAT_Core_2_v1_maize[j*nrOfSimDays+1:nrOfSimDays,i]), lty=2)
      lines(1:nrOfSimDays,c(df_field_OAT_Core_2_v1_bean[j*nrOfSimDays+1:nrOfSimDays,i]), col="red", lty=2)
    } else {
      lines(1:nrOfSimDays,c(df_field_OAT_Core_2_v1_maize[j*nrOfSimDays+1:nrOfSimDays,i]))
      lines(1:nrOfSimDays,c(df_field_OAT_Core_2_v1_bean[j*nrOfSimDays+1:nrOfSimDays,i]), col="red")
    }
  }
  abline(v = 100, col="black", lty=2)
  abline(v = 200, col="red", lty=2)
  lines(1:nrOfSimDays,c(maizeSingleSim[1:nrOfSimDays,i]), col="green2", lty=3)
 # title(colnames(df_field_OAT_Core_2_v1_maize)[i])
}
#graph2ppt(file="test.pptx", width=4, height=4)


