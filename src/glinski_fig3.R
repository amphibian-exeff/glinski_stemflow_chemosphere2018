#install of ggpubr currently a disaster
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library(plyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(RColorBrewer)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  stemflow.root <- "d:/git/glinski_stemflow/"
}
print(paste("Root directory location: ", stemflow.root, sep=""))

stemflow.csv.in <- paste(stemflow.root, "csv_in/", sep="")
stemflow.csv.out <- paste(stemflow.root, "csv_out/", sep="")
stemflow.graphics <- paste(stemflow.root, "graphics/", sep="")
stemflow.tables <- paste(stemflow.root, "tables/", sep="")

#maybe ok even if remote drive returns false
print(paste("check to see if R can access files OK: ", file.exists(stemflow.csv.in), sep = ""))

#import tifton data
tifton <- read.table(paste(stemflow.csv.in,"tifton2015.csv",sep=""), header = TRUE, sep = ",")

summary(tifton)
colnames(tifton)

which(tifton$Type == "TP")
unique(tifton$Date)

#pull out metolachlor and tebuconazole for surface water
flag_tp_metolachlor <- which(tifton$Compound=="Metolachlor" & tifton$Type == "TP")
flag_tp_tebuconazole <- which(tifton$Compound=="Tebuconazole" & tifton$Type == "TP")

#build metolachlor data.frame
metolachlor_dates <- tifton[flag_tp_metolachlor,]$Date
metolachlor_sites <- tifton[flag_tp_metolachlor,]$Site
metolachlor_concs <- tifton[flag_tp_metolachlor,]$Conc
metolachlor_nds <- which(is.na(metolachlor_concs))
metolachlor_concs[is.na(metolachlor_concs)] <- 0
metolachlor_df <- data.frame(metolachlor_dates, metolachlor_sites, metolachlor_concs)
metolachlor_df$metolachlor_dates <- factor(metolachlor_df$metolachlor_dates, 
                                           levels = unique(metolachlor_df$metolachlor_dates))
metolachlor_df_nds <- metolachlor_df[-metolachlor_nds,]
sorted_metolachlor_df_nds <- metolachlor_df_nds[order(metolachlor_df_nds$metolachlor_dates),]
dim(sorted_metolachlor_df_nds)
sorted_metolachlor_df <- metolachlor_df[order(metolachlor_df$metolachlor_dates),]
dim(sorted_metolachlor_df)
#sort data.frame on date in chron order
#unique(metolachlor_dates)

#sites as factor
summary(metolachlor_df)
metolachlor_df$metolachlor_sites <- factor(metolachlor_df$metolachlor_sites)

# colfunc <- colorRampPalette(c("black", "gray89"))
# colfunc <- colorRampPalette(c("blue4", "darkolivegreen1"))
# fill_palette <- colfunc(10)
fill_palette <- brewer.pal(10,"Spectral")


metolachlor_stacked <- ggplot(data=metolachlor_df, aes(x=metolachlor_dates, y=metolachlor_concs, fill=metolachlor_sites)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=fill_palette) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
metolachlor_stacked

metolachlor_boxplot <- ggplot(data=metolachlor_df, aes(x=metolachlor_dates, y=metolachlor_concs, fill=metolachlor_sites)) +
  geom_boxplot(fill='cornflowerblue') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
metolachlor_boxplot

metolachlor_boxplot_nds <- ggplot(data=metolachlor_df_nds, aes(x=metolachlor_dates, y=metolachlor_concs, fill=metolachlor_sites)) +
  geom_boxplot(fill='cornflowerblue') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#build tebuconazole data.frame
tebuconazole_dates <- tifton[flag_tp_tebuconazole,]$Date
tebuconazole_sites <- tifton[flag_tp_tebuconazole,]$Site
tebuconazole_concs <- tifton[flag_tp_tebuconazole,]$Conc
tebuconazole_nds <- which(is.na(tebuconazole_concs))
tebuconazole_concs[is.na(tebuconazole_concs)] <- 0
tebuconazole_df <- data.frame(tebuconazole_dates, tebuconazole_sites, tebuconazole_concs)
tebuconazole_df$tebuconazole_dates <- factor(tebuconazole_df$tebuconazole_dates, 
                                           levels = unique(tebuconazole_df$tebuconazole_dates))
tebuconazole_df_nds <- tebuconazole_df[-tebuconazole_nds,]

#sites as factor
summary(tebuconazole_df)
tebuconazole_df$tebuconazole_sites <- factor(tebuconazole_df$tebuconazole_sites)

sorted_tebuconazole_df_nds <- tebuconazole_df_nds[order(tebuconazole_df_nds$tebuconazole_dates),]
dim(sorted_tebuconazole_df_nds)
sorted_tebuconazole_df <- tebuconazole_df[order(tebuconazole_df$tebuconazole_dates),]
dim(sorted_tebuconazole_df)



#sort data.frame on date in chron order
unique(tebuconazole_dates)  
tebuconazole_stacked <- ggplot(data=tebuconazole_df, aes(x=tebuconazole_dates, y=tebuconazole_concs, fill=tebuconazole_sites)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=fill_palette) +
  theme_bw() + 
  labs(x = "Sample Date", y=expression(paste("Concentration (",mu,"g/L)",sep=""))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
tebuconazole_stacked

#    geom_bar(stat="identity", position="stack") +
#  scale_fill_grey(start=0.2,end=0.8) +
#  scale_fill_brewer(palette = "Set3") + 
#  theme_bw() + 
#  labs(x = "Sample Date", y=expression(paste("Concentration (",mu,"g/L)",sep=""))) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tebuconazole_boxplot <- ggplot(data=tebuconazole_df, aes(x=tebuconazole_dates, y=tebuconazole_concs, fill=tebuconazole_sites)) +
  geom_boxplot(fill='cornflowerblue') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tebuconazole_boxplot_nds <- ggplot(data=tebuconazole_df_nds, aes(x=tebuconazole_dates, y=tebuconazole_concs, fill=tebuconazole_sites)) +
  geom_boxplot(fill='cornflowerblue') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

compare_boxplot <- paste(stemflow.graphics,"met_teb_boxplot.png",sep="")
png(compare_boxplot, width = 4, height = 6, units = "in",res=300)
multiplot(metolachlor_boxplot, tebuconazole_boxplot, cols=1)
dev.off()
#arrangeGrob(metolachlor_boxplot, tebuconazole_boxplot, ncol = 1)
#ggarrange(metolachlor_boxplot, tebuconazole_boxplot,labels=c("A","B"),ncol=1,nrows=2)
#ggsave(compare_boxplot,device="png",width=6,height=4)

compare_boxplot_nds <- paste(stemflow.graphics,"met_teb_boxplot_nds.png",sep="")
png(compare_boxplot_nds, width = 4, height = 6, units = "in",res=300)
multiplot(metolachlor_boxplot_nds, tebuconazole_boxplot_nds, cols=1)
dev.off()
#arrangeGrob(metolachlor_boxplot, tebuconazole_boxplot, ncol = 1)
#ggarrange(metolachlor_boxplot, tebuconazole_boxplot,labels=c("A","B"),ncol=1,nrows=2)
#ggsave(compare_boxplot_nds,device="png",width=6,height=4)

compare_stacked <- paste(stemflow.graphics,"glinski_fig3_met_teb_stacked_old.png",sep="")
png(compare_stacked, width = 6, height = 6, units = "in",res=300)
  multiplot(metolachlor_stacked, tebuconazole_stacked, cols=1)
dev.off()
#ggarrange(metolachlor_stacked, tebuconazole_stacked,labels=c("A","B"),ncol=1,nrows=2)
#ggsave(compare_stacked,device="png",width=6,height=4)

#combine for final figure
#metolachlor
colnames(metolachlor_df) <- c("dates", "Sites", "concs")
n <- length(metolachlor_df$dates)
compound <- rep("Metolachlor", n)
metolachlor_df2 <- cbind(metolachlor_df, compound)
#tebuconazole
colnames(tebuconazole_df) <- c("dates", "Sites", "concs")
n <- length(tebuconazole_df$dates)
compound <- rep("Tebuconazole", n)
tebuconazole_df2 <- cbind(tebuconazole_df, compound)

#combine
combined_df <- rbind(metolachlor_df2, tebuconazole_df2)

#final figure 3
combined_stacked <- ggplot(data=combined_df, aes(x=dates, y=concs, fill=Sites)) +
  geom_bar(stat="identity") +
  facet_wrap(~ compound, scales = "free") + # , nrow = 2
  scale_fill_manual(values=fill_palette) +
  theme_bw() + 
  labs(x = "Sample Date", y=expression(paste("Concentration (",mu,"g/L)",sep=""))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
combined_stacked

#write to jpeg
jpeg(paste(stemflow.graphics,"glinski_fig3.jpg", sep=""),width = 5, height = 6, units = "in",res=600)
  combined_stacked
dev.off()
