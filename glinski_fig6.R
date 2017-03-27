#import tifton data
tifton <- read.table(paste(stemflow.csv.in,"tifton2015.csv",sep=""), header = TRUE, sep = ",")

#merge on site, date, compound
tifton_merge <- merge(tifton_sf,tifton_tf,by=c("Compound","Site","Date"))
#change NAs to zeroes for comparisons and the Wilcox test
tifton_merge$Conc.x[is.na(tifton_merge$Conc.x)] <- 0
tifton_merge$Conc.y[is.na(tifton_merge$Conc.y)] <- 0

tifton_wilcox <- wilcox.test(tifton_merge$Conc.y, tifton_merge$Conc.x, 
                             alternative = 'two.sided', paired = TRUE, exact = FALSE)
x_max <- max(tifton_merge$Conc.x)
y_max <- max(tifton_merge$Conc.y)
max_conc <- max(x_max,y_max)
max_conc_vector <- seq(0,max_conc,20)
print(tifton_wilcox)
#View(tifton_merge)
jpeg(paste(stemflow.graphics,"glinski_fig6.jpg", sep=""),width = 6, height = 6, units = "in",res=300)
  plot(tifton_merge$Conc.x, tifton_merge$Conc.y, 
       xlim=c(0,4),ylim=c(0,4),
       #expression(paste("Throughfall Concentration ( ",mu,"g/L)",sep=""))
       xlab=expression(paste("Stemflow Concentration (", mu,"g/L)",sep="")),
       ylab=expression(paste("Throughfall Concentration (", mu,"g/L)",sep="")),
       main=paste('All Tifton Data (WRS p-value = ',round(tifton_wilcox$p.value,4),')'),
       sub = comparisons)
  abline(0,1,col='red')
dev.off()

#break down by day
unique_dates <- unique(tifton_merge$Date)


#break down by site


#rainwater
View(rainwater)
#tifton[which(tifton$Compound==chem),]
tifton_sf <- tifton[which(tifton$Type=='SF'),]
tifton_tf <- tifton[which(tifton$Type=='TF'),]
tifton_tp <- tifton[which(tifton$Type=='TP'),]
unique(tifton_sf$Date)
unique(tifton_tf$Date)
unique(tifton_tp$Date)
#merge on site, date, compound
tifton_merge <- merge(tifton_sf,tifton_tf,by=c("Compound","Site","Date"))
tifton_merge <- merge(tifton_merge, rainwater, by="Date")
pond_merge <- merge(tifton_merge, tifton_tp, by=c("Compound","Site","Date"))
View(pond_merge)
#View(tifton_merge)
summary(tifton_merge)
plot(tifton_merge$amount_inches, tifton_merge$Conc.x)

#pond plots
summary(pond_merge)
plot(pond_merge$Conc.x,pond_merge$Conc)
plot(pond_merge$Conc.y,pond_merge$Conc)
plot(pond_merge$amount_inches,pond_merge$Conc)
