###Calculate photovoltaic values from SENEC CSV files

rm(list=ls()) #clean memory
gc()          #collect garbage

##Dieses Script erwartet, dass alle CSV Dateien eines Jahres in einem Verzeichnis liegen
##Die Verzeichnisse mit den einzelnen Jahren liegen in einem Verzeichnis das hier spezifiziert wird:

setwd("D:/private/PV-Anlage")

#Eingabe des Jahres, f?r das die Auswertung gemacht werden soll
year <- 2025   # set year to compute
pathtofiles <- paste(year,'/*.csv', sep="")

#read all CSV files in folder and put them in one dataframe

files <- (Sys.glob(pathtofiles)) #read all filenames

for (file in files){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("strom")){
    strom <- read.csv(file, header=TRUE, sep=";", dec = ",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("strom")){
    temp_strom <-read.csv(file, header=TRUE, sep=";", dec = ",")
    strom<-rbind(strom, temp_strom)
    rm(temp_strom)
  }
  
}

#convert kW to kWh

Bezug <- sum(strom[2]/12)
Einspeisung <- sum(strom[3]/12)
Hausverbrauch <- sum(strom[4]/12)
Erzeugung <- sum(strom[7]/12)
Eigenverbrauch <- Erzeugung-Einspeisung

#plot simple graph
Messung = c("Erzeugung","Hausverbrauch","Eigenverbrauch","Bezug","Einspeisung")
Werte = c(Erzeugung,Hausverbrauch,Eigenverbrauch,Bezug,Einspeisung)

colors = c("coral1", "palegreen2", "lightblue1", "sandybrown", "khaki") 
yaxis <- paste('PV-Werte',year,'[kWh]')

bp_simple <- barplot(Werte, names.arg = Messung, ylab=yaxis, col = colors)
text(bp_simple,0,round(Werte,1),cex=1,pos=3)

#save graph as PNG

filename <- paste(year,'_simple.png', sep="")
dev.copy(png,filename,width = 1600, height = 800)
dev.off()

#plot stacked graph
sgraph <- matrix(c(Eigenverbrauch,Eigenverbrauch,Bezug,Einspeisung),ncol=2, byrow=TRUE)
colnames(sgraph) <- c("Verbrauch","Erzeugung")
rownames(sgraph) <- c("Eigenanteil","Netzanteil")
sgraph <- as.table(sgraph)

barplot(as.matrix(sgraph), ylab=yaxis, col = colors, legend.text = TRUE,
#       ylim = c(0, (1.3*Hausverbrauch)),
        args.legend = list(x = "topright", bty = "n"))

#save graph as PNG
filename <- paste(year,'_stacked.png', sep="")
dev.copy(png,filename,width = 1200)
dev.off()
