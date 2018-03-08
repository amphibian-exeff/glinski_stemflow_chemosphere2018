library(ggplot2)

#import 4l data
ponds_4l <- read.table(paste(stemflow.csv.in,"Ponds_4L.csv",sep=""), header = TRUE, sep = ",", stringsAsFactors=FALSE)
dim(ponds_4l)
colnames(ponds_4l)
summary(ponds_4l)
levels(ponds_4l$Month)
#View(ponds_4l)
n <- length(ponds_4l$Month)

#build metolachlor data.frame with factors and conce
ponds_dates <- as.factor(c(ponds_4l$Month, ponds_4l$Month))
#levels(ponds_dates) <- as.vector(unique(ponds_dates))
ponds_dates <- factor(ponds_dates, levels = unique(ponds_dates))
ponds_sites <- c(ponds_4l$Site, ponds_4l$Site)
ponds_concs <- c(ponds_4l$Metolachlor, ponds_4l$Other.Pesticides)
ponds_factor <- as.factor(c(rep('Metolachlor',n),rep('Other',n)))

ponds_concs_nas <- which(is.na(ponds_concs))
ponds_concs[ponds_concs_nas] <- 0
ponds_4l_df <- data.frame(ponds_dates, ponds_sites, ponds_concs, ponds_factor)
summary(ponds_4l_df)
# View(ponds_4l_df)

ponds_stacked <- ggplot(data=ponds_4l_df, aes(x=factor(ponds_dates), y=ponds_concs, fill=ponds_factor)) +
  geom_bar(stat="identity") +
  facet_grid(~ponds_sites) +
  theme_bw() + 
  labs(x = "Sample Date", y=expression(paste("Concentration (",mu,"g/L)",sep=""))) +
  guides(fill=guide_legend(title="Chemical")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4))
ponds_stacked

stacked_barplot <- paste(stemflow.graphics,"glinski_fig4.jpg",sep="")
jpeg(stacked_barplot, width = 7, height = 4, units = "in",res=600)
  ponds_stacked
dev.off()
