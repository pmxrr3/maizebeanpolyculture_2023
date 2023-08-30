field_1 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/field_mono_nrSubInternodes_1(2).txt'
field_2 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/field_mono_nrSubInternodes_2(2).txt'
field_4 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/field_mono_nrSubInternodes_4(2).txt'
field_8 <- 'C:/Users/Rik/OneDrive - The University of Nottingham/FSPM/Climbing bean data for experiments/field_mono_nrSubInternodes_8(2).txt'

df_field_1 <- read.table(field_1, header = TRUE)
df_field_2 <- read.table(field_2, header = TRUE)
df_field_4 <- read.table(field_4, header = TRUE)
df_field_8 <- read.table(field_8, header = TRUE)
#df_field_poly_1 <- rbind(df_field_poly_1[601:1200,],df_field_poly_1[1:600,],df_field_poly_1[1201:nrow(df_field_poly_1),]) # make sure baseline is on top.
rownames(df_field_1) = seq(length=nrow(df_field_1))
rownames(df_field_2) = seq(length=nrow(df_field_2))
rownames(df_field_4) = seq(length=nrow(df_field_4))
rownames(df_field_8) = seq(length=nrow(df_field_8))
# Maizedata_poly_1 <- as.matrix(df_field_poly_1[df_field_poly_1[ ,2] == "9", ]) # select all maize data 
# colnames(Maizedata_poly_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
# rownames(Maizedata_poly_1) = seq(length=nrow(Maizedata_poly_1))
Beandata_field_1 <- as.matrix(df_field_1[df_field_1[ ,2] == "16", ]) # select all bean data 
colnames(Beandata_field_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
rownames(Beandata_field_1) = seq(length=nrow(Beandata_field_1))
Beandata_field_2 <- as.matrix(df_field_2[df_field_2[ ,2] == "16", ]) # select all bean data 
colnames(Beandata_field_2) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
rownames(Beandata_field_2) = seq(length=nrow(Beandata_field_2))
Beandata_field_4 <- as.matrix(df_field_4[df_field_4[ ,2] == "16", ]) # select all bean data 
colnames(Beandata_field_4) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
rownames(Beandata_field_4) = seq(length=nrow(Beandata_field_4))
Beandata_field_8 <- as.matrix(df_field_8[df_field_8[ ,2] == "16", ]) # select all bean data 
colnames(Beandata_field_8) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
rownames(Beandata_field_8) = seq(length=nrow(Beandata_field_8))

nrSims <- 39
# calculate mean and standard deviation for each 3 reps per scenario 
Mean_beandata_field_1 <- sapply(3:ncol(Beandata_field_1), function (j) sapply(1:1, function(i) sapply(1:80, function(x) mean(c(Beandata_field_1[(i-1)*240+x, j],Beandata_field_1[(i-1)*240+x+80, j],Beandata_field_1[(i-1)*240+x+160, j])))))
Mean_beandata_field_1 <- cbind(Beandata_field_1[1:nrow(Mean_beandata_field_1),1:2], Mean_beandata_field_1)
colnames(Mean_beandata_field_1) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
#
Mean_beandata_field_2 <- sapply(3:ncol(Beandata_field_2), function (j) sapply(1:1, function(i) sapply(1:80, function(x) mean(c(Beandata_field_2[(i-1)*240+x, j],Beandata_field_2[(i-1)*240+x+80, j],Beandata_field_2[(i-1)*240+x+160, j])))))
Mean_beandata_field_2 <- cbind(Beandata_field_2[1:nrow(Mean_beandata_field_2),1:2], Mean_beandata_field_2)
colnames(Mean_beandata_field_2) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
#
Mean_beandata_field_4 <- sapply(3:ncol(Beandata_field_4), function (j) sapply(1:1, function(i) sapply(1:80, function(x) mean(c(Beandata_field_4[(i-1)*240+x, j],Beandata_field_4[(i-1)*240+x+80, j],Beandata_field_4[(i-1)*240+x+160, j])))))
Mean_beandata_field_4 <- cbind(Beandata_field_4[1:nrow(Mean_beandata_field_4),1:2], Mean_beandata_field_4)
colnames(Mean_beandata_field_4) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
#
Mean_beandata_field_8 <- sapply(3:ncol(Beandata_field_8), function (j) sapply(1:1, function(i) sapply(1:80, function(x) mean(c(Beandata_field_8[(i-1)*240+x, j],Beandata_field_8[(i-1)*240+x+80, j],Beandata_field_8[(i-1)*240+x+160, j])))))
Mean_beandata_field_8 <- cbind(Beandata_field_8[1:nrow(Mean_beandata_field_8),1:2], Mean_beandata_field_8)
colnames(Mean_beandata_field_8) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
#

###
# # for-loop to plot mean +- std for each of the 100 days? for a given (or multiple) output
# #opar <- par(lwd=2)
# plot(1:100,c(Mean_Maizedata_poly_1[1:100,3]), xlab="Simulation day", ylab="Mean LAI", type="l", ylim=c(0,5))
# lines(1:100,c(Mean_beandata_poly_1[1:100,3]), col="red")
# for(i in 2:10) {
#   lines(1:100,c(Mean_Maizedata_poly_1[i*100+1:100,3]))
#   lines(1:100,c(Mean_beandata_poly_1[i*100+1:100,3]), col="red")
# }
# legend("topleft", c("Maize", "Bean"), col=c("black", "red"), lwd=2)
# #par(opar)

###
library(dplyr)
Titlelist <- c('LAI', 'nrShoots', 'fAbs', 'assCO2', 'biomAbove', 'yield', 'leafArea', 'fieldRFR')
legendlabels <- c("1", "2","4", "8")
# ymax <- 5 #1500
yLabel <- 'Output' #'Biomass (g/m^2)' #Biomass (g/m^2)
for(counter in 1:length(Titlelist)) {
  columnnumber <- case_when(
    counter==1 ~ 3,
    counter==2 ~ 4,
    counter==3 ~ 5,
    counter==4 ~ 6,
    counter==5 ~ 7,
    counter==6 ~ 8,
    counter==7 ~ 10,
    TRUE ~ 11
    )
  ymax <- case_when(
    counter==1 ~ 7,
    counter==2 ~ 80,
    counter==3 ~ 0.9,
    counter==4 ~ 1800000,
    counter==5 ~ 1200,
    counter==6 ~ 600,
    counter==7 ~ 80,
    TRUE ~ 1
  )
  ###
  plot(1:80,c(Mean_beandata_field_1[1:80,columnnumber]), xlab="Simulation day", ylab=yLabel, ylim=c(0,ymax), type="l",lwd=2)
  lines(1:80,c(Mean_beandata_field_2[1:80,columnnumber]), lty="dashed",lwd=2)
  lines(1:80,c(Mean_beandata_field_4[1:80,columnnumber]), col="red", lty="solid",lwd=2)
  lines(1:80,c(Mean_beandata_field_8[1:80,columnnumber]), col="red", lty="dashed",lwd=2)
  legend("topleft", legend=legendlabels, col=c("black","black","red","red"), lty=c("solid", "dashed", "solid", "dashed"))
  title(Titlelist[[counter]])
  
  ###
  # plot(1:100,c(Mean_beandata_poly_1[1:100,columnnumber]), xlab="Simulation day", ylab=yLabel, ylim=c(0,ymax), type="l",lwd=2, col="red")
  # lines(1:100,c(Mean_beandata_poly_1[counter*200+1:100,columnnumber]), col="red", lty="dashed",lwd=2)
  # lines(1:100,c(Mean_beandata_poly_1[counter*200-100+1:100,columnnumber]), col="red", lty="dotted",lwd=2)
  # legend("topleft", legend=legendlabels[4:6], col=c("red","red","red"), lty=c("dotted", "solid", "dashed"))
  # title(Titlelist[[counter]])
}



###
# attempt at grid of plots (combi's of maize and corresponding bean)
library(ggplot2)
library(gridExtra)
library(scales)
#install.packages("ggpubr")
#library(ggpubr)
p <- vector(mode = "list", length = 6)
Titlelist <- c('Row distance', 'Internode length', 'Helix inclination', 'Branching rate', 'Time to flower', 'Plant distance')
columnnumber <- 7
yLabel <- 'Biomass (g/m^2)' #Biomass (g/m^2)
for(i in 1:length(p)) {
  # df <- data.frame(x=c(1:100), y=c(Mean_Maizedata_poly_1[1:100,columnnumber])) # baseline maize
  df2 <- data.frame(x=c(1:100), y=c(Mean_beandata_poly_1[1:100,columnnumber])) # baseline bean
  #  df3 <- data.frame(x=c(1:100), y=c(Mean_Maizedata_poly_1[i*200+1:100,columnnumber])) # higher maize
  df4 <- data.frame(x=c(1:100), y=c(Mean_beandata_poly_1[i*200+1:100,columnnumber])) # higher bean
  #  df5 <- data.frame(x=c(1:100), y=c(Mean_Maizedata_poly_1[i*200-100+1:100,columnnumber])) # lower maize
  df6 <- data.frame(x=c(1:100), y=c(Mean_beandata_poly_1[i*200-100+1:100,columnnumber])) # lower bean
  
  # p[[i]] <- ggplot(df, aes(x,y)) + geom_line(size=1) + geom_line(data=df2, aes(x,y), color="red", size=1) + geom_line(data=df3, aes(x,y), color="black", size=1, linetype = "dashed") + geom_line(data=df4, aes(x,y), color="red", size=1, linetype = "dashed") + geom_line(data=df5, aes(x,y), color="black", size=1, linetype = "dotted") + geom_line(data=df6, aes(x,y), color="red", size=1, linetype = "dotted")  + labs(y = yLabel, x="Simulation day", title=Titlelist[[i]])
  p[[i]] <- ggplot(df2, aes(x,y)) + geom_line(size=1, color="red", group=Titlelist[[i]]) + geom_line(data=df4, aes(x,y), color="red", size=1, linetype = "dashed")  + geom_line(data=df6, aes(x,y), color="red", size=1, linetype = "dotted")  + labs(y = yLabel, x="Simulation day", title=Titlelist[[i]])
  #+ scale_y_continuous(limits=c(0, 1200), breaks=c(0, 300, 600, 900, 1200)) 
  #+ scale_y_continuous(trans='log2', breaks = trans_breaks("log2", function(x) 2^x), labels = trans_format("log2", math_format(2^.x)))
}
nCol <- floor(sqrt(length(p)))
#do.call("grid.arrange", c(p, ncol=nCol))
p[[1]]
# alternative that might work better to try and include a legend:
#ggarrange(p[[1]], p[[2]],p[[3]],p[[4]],p[[5]], ncol=2, nrow=3, common.legend = TRUE, legend="bottom")

