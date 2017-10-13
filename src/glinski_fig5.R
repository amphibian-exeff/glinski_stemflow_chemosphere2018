#import tifton data
tifton <- read.table(paste(stemflow.csv.in,"tifton2015.csv",sep=""), header = TRUE, sep = ",")

summary(tifton)
colnames(tifton)

which(tifton$Type == "TP")
which(tifton$Type == "SF")
which(tifton$Type == "TF")
unique(tifton$Date)
unique(tifton$Compound)

which_metolochlor <- which(tifton$Compound == "Metolachlor")
which_tebuconazole <- which(tifton$Compound == "Tebuconazole")

which_sf <- which(tifton$Type == "SF")
which_tf <- which(tifton$Type == "TF")
