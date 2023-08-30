field_poly_1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/field_polyculture_1 - Copy.txt'

df_field_poly_1 <- read.table(field_poly_1, header = TRUE)
df_field_poly_1 <- rbind(df_field_poly_1[601:1200,],df_field_poly_1[1:600,],df_field_poly_1[1201:nrow(df_field_poly_1),]) # make sure baseline is on top.
rownames(df_field_poly_1) = seq(length=nrow(df_field_poly_1))
Maizedata_poly_1 <- as.matrix(df_field_poly_1[df_field_poly_1[ ,2] == "9", ]) # select all maize data 
colnames(Maizedata_poly_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
rownames(Maizedata_poly_1) = seq(length=nrow(Maizedata_poly_1))
Beandata_poly_1 <- as.matrix(df_field_poly_1[df_field_poly_1[ ,2] == "16", ]) # select all bean data 
colnames(Beandata_poly_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
rownames(Beandata_poly_1) = seq(length=nrow(Beandata_poly_1))

nrSims <- 33
# calculate mean and standard deviation for each 3 reps per scenario 
Mean_Maizedata_poly_1 <- sapply(3:ncol(Maizedata_poly_1), function (j) sapply(1:11, function(i) sapply(1:100, function(x) mean(c(Maizedata_poly_1[(i-1)*300+x, j],Maizedata_poly_1[(i-1)*300+x+100, j],Maizedata_poly_1[(i-1)*300+x+200, j])))))
Mean_Maizedata_poly_1 <- cbind(Maizedata_poly_1[1:nrow(Mean_Maizedata_poly_1),1:2], Mean_Maizedata_poly_1)
colnames(Mean_Maizedata_poly_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
#
Mean_beandata_poly_1 <- sapply(3:ncol(Beandata_poly_1), function (j) sapply(1:11, function(i) sapply(1:100, function(x) mean(c(Beandata_poly_1[(i-1)*300+x, j],Beandata_poly_1[(i-1)*300+x+100, j],Beandata_poly_1[(i-1)*300+x+200, j])))))
Mean_beandata_poly_1 <- cbind(Beandata_poly_1[1:nrow(Mean_beandata_poly_1),1:2], Mean_beandata_poly_1)
colnames(Mean_beandata_poly_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
#
STD_Maizedata_poly_1 <- sapply(3:ncol(Maizedata_poly_1), function (j) sapply(1:11, function(i) sapply(1:100, function(x) sd(c(Maizedata_poly_1[(i-1)*300+x, j],Maizedata_poly_1[(i-1)*300+x+100, j],Maizedata_poly_1[(i-1)*300+x+200, j])))))
STD_Maizedata_poly_1 <- cbind(Maizedata_poly_1[1:nrow(STD_Maizedata_poly_1),1:2], STD_Maizedata_poly_1)
colnames(STD_Maizedata_poly_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
#
STD_Beandata_poly_1 <- sapply(3:ncol(Beandata_poly_1), function (j) sapply(1:11, function(i) sapply(1:100, function(x) sd(c(Beandata_poly_1[(i-1)*300+x, j],Beandata_poly_1[(i-1)*300+x+100, j],Beandata_poly_1[(i-1)*300+x+200, j])))))
STD_Beandata_poly_1 <- cbind(Beandata_poly_1[1:nrow(STD_Beandata_poly_1),1:2], STD_Beandata_poly_1)
colnames(STD_Beandata_poly_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")

###
# for-loop to plot mean +- std for each of the 100 days? for a given (or multiple) output
#opar <- par(lwd=2)
plot(1:100,c(Mean_Maizedata_poly_1[1:100,3]), xlab="Simulation day", ylab="Mean LAI", type="l")
lines(1:100,c(Mean_beandata_poly_1[1:100,3]), col="red")
for(i in 2:10) {
  lines(1:100,c(Mean_Maizedata_poly_1[i*100+1:100,3]))
  lines(1:100,c(Mean_beandata_poly_1[i*100+1:100,3]), col="red")
}
legend("topleft", c("Maize", "Bean"), col=c("black", "red"), lwd=2)
#par(opar)

###
library(dplyr)
Titlelist <- c('Germination delay bean', 'Internode length bean', 'Helix inclination', 'Branching probability', 'Time to flower')
columnnumber <- 3
ymax <- 2 #1500
yLabel <- 'LAI' #'Biomass (g/m^2)' #Biomass (g/m^2)
for(counter in 1:length(Titlelist)) {
  legendlabels <- case_when(
    counter==1 ~ c("Maize 25d", "Maize 35d","Maize 45d", "Bean 25d", "Bean 35d", "Bean 45d"),
    counter==2 ~ c("Maize 0.07m", "Maize 0.1m","Maize 0.15m", "Bean 0.07m", "Bean 0.1m", "Bean 0.15m"),
    counter==3 ~ c("Maize 45deg", "Maize 60deg","Maize 75deg", "Bean 45deg", "Bean 60deg", "Bean 75deg"),
    counter==4 ~ c("Maize 0.2", "Maize 0.1","Maize 0.05", "Bean 0.2", "Bean 0.1", "Bean 0.05"),
    counter==5 ~ c("Maize 25d", "Maize 35d","Maize 45d", "Bean 25d", "Bean 35d", "Bean 45d"),
    TRUE ~ c("Maize 0.25m", "Maize 0.5m","Maize 1.0m", "Bean 0.25m", "Bean 0.5m", "Bean 1.0m"))
  ###
  plot(1:100,c(Mean_Maizedata_poly_1[1:100,columnnumber]), xlab="Simulation day", ylab=yLabel, ylim=c(0,ymax), type="l",lwd=2)
  lines(1:100,c(Mean_Maizedata_poly_1[counter*200+1:100,columnnumber]), lty="dashed",lwd=2)
  lines(1:100,c(Mean_Maizedata_poly_1[counter*200-100+1:100,columnnumber]), lty="dotted",lwd=2)
  lines(1:100,c(Mean_beandata_poly_1[1:100,columnnumber]), col="red", lty="solid",lwd=2)
  lines(1:100,c(Mean_beandata_poly_1[counter*200+1:100,columnnumber]), col="red", lty="dashed",lwd=2)
  lines(1:100,c(Mean_beandata_poly_1[counter*200-100+1:100,columnnumber]), col="red", lty="dotted",lwd=2)
  legend("topleft", legend=legendlabels, col=c("black","black","black","red","red","red"), lty=c("dotted", "solid", "dashed","dotted", "solid", "dashed"))
  title(Titlelist[[counter]])
  
  ###
  plot(1:100,c(Mean_beandata_poly_1[1:100,columnnumber]), xlab="Simulation day", ylab=yLabel, ylim=c(0,ymax), type="l",lwd=2, col="red")
  lines(1:100,c(Mean_beandata_poly_1[counter*200+1:100,columnnumber]), col="red", lty="dashed",lwd=2)
  lines(1:100,c(Mean_beandata_poly_1[counter*200-100+1:100,columnnumber]), col="red", lty="dotted",lwd=2)
  legend("topleft", legend=legendlabels[4:6], col=c("red","red","red"), lty=c("dotted", "solid", "dashed"))
  title(Titlelist[[counter]])
}

###
# attempt at grid of plots (combi's of maize and corresponding bean)
library(ggplot2)
library(gridExtra)
library(scales)
#install.packages("ggpubr")
#library(ggpubr)
p <- vector(mode = "list", length = 5)
Titlelist <- c('Germination delay', 'Internode length', 'Helix inclination', 'Branching rate', 'Time to flower')
columnnumber <- 10
yLabel <- 'Leaf area' #Biomass (g/m^2)
for(i in 1:length(p)) {
  df <- data.frame(x=c(1:100), y=c(Mean_Maizedata_poly_1[1:100,columnnumber])) # baseline maize
  df2 <- data.frame(x=c(1:100), y=c(Mean_beandata_poly_1[1:100,columnnumber])) # baseline bean
  df3 <- data.frame(x=c(1:100), y=c(Mean_Maizedata_poly_1[i*200+1:100,columnnumber])) # higher maize
  df4 <- data.frame(x=c(1:100), y=c(Mean_beandata_poly_1[i*200+1:100,columnnumber])) # higher bean
  df5 <- data.frame(x=c(1:100), y=c(Mean_Maizedata_poly_1[i*200-100+1:100,columnnumber])) # lower maize
  df6 <- data.frame(x=c(1:100), y=c(Mean_beandata_poly_1[i*200-100+1:100,columnnumber])) # lower bean
  
  p[[i]] <- ggplot(df, aes(x,y)) + geom_line(size=1) + geom_line(data=df2, aes(x,y), color="red", size=1) + geom_line(data=df3, aes(x,y), color="black", size=1, linetype = "dashed") + geom_line(data=df4, aes(x,y), color="red", size=1, linetype = "dashed") + geom_line(data=df5, aes(x,y), color="black", size=1, linetype = "dotted") + geom_line(data=df6, aes(x,y), color="red", size=1, linetype = "dotted")  + labs(y = yLabel, x="Simulation day", title=Titlelist[[i]])
    #+ scale_y_continuous(limits=c(0, 1200), breaks=c(0, 300, 600, 900, 1200)) 
  #+ scale_y_continuous(trans='log2', breaks = trans_breaks("log2", function(x) 2^x), labels = trans_format("log2", math_format(2^.x)))
}
nCol <- floor(sqrt(length(p)))
do.call("grid.arrange", c(p, ncol=nCol))
# alternative that might work better to try and include a legend:
#ggarrange(p[[1]], p[[2]],p[[3]],p[[4]],p[[5]], ncol=2, nrow=3, common.legend = TRUE, legend="bottom")

