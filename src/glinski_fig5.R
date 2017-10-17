library(ggplot2)

#import tifton data
tifton <- read.table(paste(stemflow.csv.in,"tifton2015.csv",sep=""), header = TRUE, sep = ",")
dim(tifton)
summary(tifton)
colnames(tifton)

# get metolachlor and tebuconzole data
tifton_compounds <- subset(tifton, tifton$Compound == "Metolachlor" | tifton$Compound == "Tebuconazole")
dim(tifton_compounds)

# drop non-stemflow data
tifton_compounds_plot <- subset(tifton_compounds, Type != "TP")
dim(tifton_compounds_plot)



tifton_compounds_plot$Date
tifton_compounds_plot$Conc

# set nas to zero for the plot
tifton_compounds_plot$Conc[which(is.na(tifton_compounds_plot$Conc))] <- 0

# change to factors, drop extra levels
tifton_compounds_plot$Type <- factor(tifton_compounds_plot$Type)
tifton_compounds_plot$Date <- factor()
tifton_compounds_plot$Site <- factor(tifton_compounds_plot$Site)
# View(tifton_compounds_plot)

#sort the dates
tifton_compounds_plot$Date <- as.Date(tifton_compounds_plot$Date, "%m/%d/%Y")
tifton_compounds_plot$Date <- factor(tifton_compounds_plot$Date)
sorted_tifton_compounds_plot <- tifton_compounds_plot[order(tifton_compounds_plot$Date),]

#plot and save to file
p <- ggplot(data=tifton_compounds_plot, aes(Date, Conc)) +
  theme_bw() + 
  geom_point(aes(color=Type, shape=Site)) + 
  facet_wrap(~Compound, scales = "free", nrow = 2) +
  labs(x = "Sample Date", y=expression(paste("Concentration (",mu,"g/L)",sep=""))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

jpeg(paste(stemflow.graphics,"glinski_fig5.jpg", sep=""),width = 5, height = 6, units = "in",res=300)
  p
dev.off()



