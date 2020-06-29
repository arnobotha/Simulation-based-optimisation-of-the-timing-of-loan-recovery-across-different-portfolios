# ================= Manicured plots for article
# This script creates the detailed and manicured graph used for the research article

require(ggplot2)
require(ggthemes)
require(scales)
require(extrafont)
require(RColorBrewer)
require(data.table)
require(tidyr)

# ======= Import Data
load("2-Data-RandomDefaults-TruncationPoints.RData")


# ================= Create Main Plot

# Losses across Thresholds by Delinquency measure
# ---- Data Preparation for Plots
col.headings <- sapply(1:it.max,function(i)paste("k=", formatC(it.vec[i],width=2,flag="0"),sep=''))
colnames(mat.TotLoss.CD) <- col.headings
colnames(mat.d.CD) <- col.headings

plot.data.CD <- data.table( gather(as.data.frame(mat.TotLoss.CD),"Portfolio","Loss"),
                            Threshold=gather(as.data.frame(mat.d.CD),"Portfolio","Threshold")$Threshold)

# - dependent on simulation parameters
port.sum <- n * principal
plot.data.CD[, Loss := Loss / port.sum]

#plot.data <- subset(plot.data.CD, Threshold < 40)
plot.data <- plot.data.CD
plot.data2 <- plot.data #for plotting points
plot.data3 <- data.frame(Portfolio=NA,Loss=NA,Threshold=NA)

#manually prepare dataset for plotting points in order to mitigate overplotting
for (k in 1:it.max) {
  #k<-1
  
  #decrease number of points after eaching w=k
  treat.data <- subset(plot.data2, Portfolio == paste("k=",formatC(it.vec[k],width=2,flag="0"),sep='') & Threshold > it.vec[k]) 
  plot.data2 <- subset(plot.data2, !(Portfolio == paste("k=",formatC(it.vec[k],width=2,flag="0"),sep='') & Threshold > it.vec[k]))
  sel.rows <- seq(1,nrow(treat.data),by=2)
  plot.data2 <- rbind(plot.data2, treat.data[sel.rows,])
  
  #remove points before w=k
  plot.data2 <- subset(plot.data2, !(Portfolio == paste("k=",formatC(it.vec[k],width=2,flag="0"),sep='') & (Threshold < it.vec[k] & Threshold > 0)))
  
  plot.data3 <- rbind(plot.data3,
                      data.frame(Portfolio=paste("k=", formatC(it.vec[k],width=2,flag="0"),sep=''),
                                 Loss=min(mat.TotLoss.CD[,k])/port.sum,
                                 Threshold=mat.d.CD[Position(function(x) x==min(mat.TotLoss.CD[,k]),mat.TotLoss.CD[,k]),k]
                      )
  )
}

label.vec <- 1:it.max

plot.data3 <- na.omit(plot.data3)

chosenFont <- "Calibri"
chosenFont <- "Times New Roman"


# -- full plot
plot.full <- 
  ggplot(plot.data, aes(x=Threshold, y=Loss)) + 
  geom_point(aes(x=Threshold,y=Loss, color=Portfolio, shape=Portfolio), data=plot.data2, size=1.75) +
  geom_line(aes(x=Threshold,y=Loss, color=Portfolio), size=0.5) +
  labs(y="Loss (%)",x=bquote(Thresholds~italic(d)~on~italic(g)[1])) + theme_minimal() + 
  theme(text=element_text(family=chosenFont, size=13),
        legend.position="bottom") + 
  scale_shape_manual(values=1:it.max, name=bquote(Truncation~italic(k)), labels = label.vec) +
  scale_color_manual(name=bquote(Truncation~italic(k)), labels=label.vec,
                     values = rev(colorRampPalette(brewer.pal(10, "Paired"))(it.max)))  +
  scale_y_continuous(breaks= pretty_breaks(), labels=percent) 
plot.full


# -- zoomed plot
# zoom bounding box
xlim <- c(0,12); ylim <- c(0.23, 0.4)


plot.zoom <- 
  plot.full + coord_cartesian(xlim=xlim, ylim=ylim) + 
  geom_segment(aes(x=Threshold,xend=Threshold, y=0, yend=Loss, color=Portfolio), linetype="dashed", data=plot.data3) + 
  geom_segment(aes(x=0,xend=12, y=Loss, yend=Loss, color=Portfolio), linetype="dashed", data=plot.data3) + 
  geom_point(aes(x=Threshold,y=Loss, color=Portfolio, shape=Portfolio), data=plot.data2, size=2.5) +
  geom_point(data=plot.data3, aes(x=Threshold, y=Loss, color=Portfolio), size=7, colour="black", shape=1) + 
 # scale_x_continuous(plot.data2=pretty_breaks()) + 
  theme(legend.position="none", 
        #axis.text.x = element_text(margin=unit(c(-6,0,1,0),"mm"),size=9),
        #axis.text.y=element_text(margin=unit(c(0,-10,0,1),"mm"),size=9),axis.ticks=element_blank(),
        axis.text.x = element_text(margin=unit(c(0,0,0,0),"mm"),size=9),
        axis.text.y=element_text(margin=unit(c(0,0,0,0),"mm"),size=9),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(color='black', fill="white"),
        plot.margin = unit(c(0,0,0,0),"mm"))
plot.zoom


# -- merge plots
g <- plot.full + annotation_custom(grob = ggplotGrob(plot.zoom), xmin = 0, xmax=35,
                                   ymin = 0.45, ymax = 0.85)
g

dpi <- 200 #130 dpi used here for clarity in floating subplots in latex
ggsave(g, file="LossRateThresh_randomdefaults_g1_k-g1-trunc.png", width=1200/dpi, height=1000/dpi,dpi=dpi)


