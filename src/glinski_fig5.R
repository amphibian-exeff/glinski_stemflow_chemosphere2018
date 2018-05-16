library(ggplot2)
library(ggpubr)

#import tifton rainfall data
tifton_rainfall <- read.table(paste(stemflow.csv.in,"rainwater_amount_updated.csv",sep=""), header = TRUE, sep = ",")
dim(tifton_rainfall)
summary(tifton_rainfall)
colnames(tifton_rainfall)
tifton_rainfall$date <- as.Date(tifton_rainfall$date,format="%m/%d/%Y")
typeof(as.Date(tifton_rainfall$date,format="%m/%d/%Y"))

#barchart of rainfall data
p1 <- ggplot(tifton_rainfall, aes(date, rain_inches)) +
  theme_bw() +
  geom_bar(stat="identity", na.rm = TRUE) +
  ylab("Precipitation (in)") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())
p1


#############
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
typeof(tifton_compounds_plot$Date)
# tifton_compounds_plot$Date <- factor(tifton_compounds_plot$Date)
# typeof(tifton_compounds_plot$Date)
sorted_tifton_compounds_plot <- tifton_compounds_plot[order(tifton_compounds_plot$Date),]

#plot and save to file
p2 <- ggplot(data=tifton_compounds_plot, aes(Date, Conc)) +
  theme_bw() + 
  geom_point(aes(color=Type, shape=Site)) + 
  facet_wrap(~Compound, scales = "free_y", nrow = 2) +
  labs(x = "Date", y=expression(paste("Concentration (",mu,"g/L)",sep=""))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="bottom")
p2

p3 <- ggarrange(p1, p2, heights = c(1, 2),
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
p3

jpeg(paste(stemflow.graphics,"glinski_fig5.jpg", sep=""),width = 5, height = 8, units = "in",res=600)
  p3
dev.off()



