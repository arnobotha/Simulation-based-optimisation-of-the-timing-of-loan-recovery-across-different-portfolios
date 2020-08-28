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
load("6b-Data-MarkovianDefaults-TransitionRates.RData")


# ================= Create Main Plot

# Losses across Thresholds by Delinquency measure
# ---- Data Preparation for Plots
prefixed <- ""
k.truncate <- 1 #natural truncation occurs at w=1 due to chain never leaving D-state when P_DD=1
k.truncate2 <- k #natural truncation occurs at w=1 due to chain never leaving D-state when P_DD=1
col.headings <- sapply(1:it.max,function(i)paste(prefixed, it.vec[i],sep=''))
colnames(mat.TotLoss.CD) <- col.headings
colnames(mat.d.CD) <- col.headings

plot.data.CD <- data.table( gather(as.data.frame(mat.TotLoss.CD),"Portfolio","Loss"),
                            Threshold=gather(as.data.frame(mat.d.CD),"Portfolio","Threshold")$Threshold)
# - dependent on simulation parameters
port.sum <- n * principal
plot.data.CD[, Loss := Loss / port.sum]


plot.data <- subset(plot.data.CD, Threshold < 40)
plot.data2 <- plot.data #for plotting points
plot.data3 <- data.frame(Portfolio=NA,Loss=NA,Threshold=NA, Global=NA) #for minima at w=1
plot.data4 <- data.frame(Portfolio=NA,Loss=NA,Threshold=NA, Global=NA, Global.ExclTruncK=NA) #for maxima
plot.data5 <- data.frame(Portfolio=NA,Loss=NA,Threshold=NA, Global=NA) #for minima at w=k

#decrease overall number of points
treat.data <- subset(plot.data2, Threshold > 2 & Threshold < k.truncate2) 
plot.data2 <- subset(plot.data2, !(Threshold > 2 & Threshold < k.truncate2))
sel.rows <- seq(1,nrow(treat.data),by=2)
plot.data2 <- rbind(plot.data2, treat.data[sel.rows,])

treat.data <- subset(plot.data2, Threshold > k.truncate2) 
plot.data2 <- subset(plot.data2, !(Threshold > k.truncate2))
sel.rows <- seq(1,nrow(treat.data),by=2)
plot.data2 <- rbind(plot.data2, treat.data[sel.rows,])

#manually prepare dataset for plotting points in order to mitigate overplotting
for (i in 1:it.max) {
  # -- Minima
  #find minimum in losses (first position and last position)
  minimumThres <- mat.d.CD[Position(function(x) x==min(mat.TotLoss.CD[,i]),mat.TotLoss.CD[,i]),i]
  lastMinimum <- mat.d.CD[Position(function(x) x==min(mat.TotLoss.CD[,i]),mat.TotLoss.CD[,i], right=T),i]
  #if two positions equal, then it is a global minimum, if not, then only local
  if (minimumThres == lastMinimum) isGlobal <- T else isGlobal <- F
  
  #minima at w=1
  plot.data3 <- rbind(plot.data3,
                      data.frame(Portfolio=paste(prefixed, it.vec[i],sep=''),
                                 Loss=min(mat.TotLoss.CD[,i]) / port.sum,
                                 Threshold=minimumThres,
                                 Global=isGlobal
                      )
  )
  if (minimumThres == k.truncate2) {
    plot.data5 <- rbind(plot.data5,
                        data.frame(Portfolio=paste(prefixed, it.vec[i],sep=''),
                                   Loss=min(mat.TotLoss.CD[,i]) / port.sum,
                                   Threshold=minimumThres,
                                   Global=isGlobal
                        )
    )
  }
  
  # -- Maxima
  #find minimum in losses (first position and last position)
  maxThres <- mat.d.CD[Position(function(x) x==max(mat.TotLoss.CD[,i]),mat.TotLoss.CD[,i]),i]
  lastMax <- mat.d.CD[Position(function(x) x==max(mat.TotLoss.CD[,i]),mat.TotLoss.CD[,i], right=T),i]
  #if two positions equal, then it is a global minimum, if not, then only local
  if (maxThres == lastMax) isGlobalMax <- T else isGlobalMax <- F
  
  #check global maxima again, but for 0 < w < k range instead.
  if (isGlobalMax) {
    if (maxThres > 0 && maxThres < k.truncate2) {
      isGlobalMaxExclTrunc <- T
    } else {
      isGlobalMaxExclTrunc <- F  
    }
  } else {
    isGlobalMaxExclTrunc <- F
  }
  
  plot.data4 <- rbind(plot.data4,
                      data.frame(Portfolio=paste(prefixed, it.vec[i],sep=''),
                                 Loss=max(mat.TotLoss.CD[,i]) / port.sum,
                                 Threshold=maxThres,
                                 Global=isGlobalMax,
                                 Global.ExclTruncK=isGlobalMaxExclTrunc
                      )
  )
}

plot.data3 <- na.omit(plot.data3)
plot.data4 <- na.omit(plot.data4)
plot.data5 <- na.omit(plot.data5)

#find region of rates for which w=1 yields minima in losses
#this is simply for plotting reasons
attach(plot.data3)
plot.data3 <- plot.data3[order(Portfolio),]
detach(plot.data3)
y.end <- plot.data3[Position(function(x) x==k.truncate, plot.data3$Threshold),]$Loss
y.start <- plot.data3[Position(function(x) x==k.truncate, subset(plot.data3,Global==T)$Threshold, right=T),]$Loss

#find region of rates for which w=k yields minima in losses
attach(plot.data5)
plot.data5 <- plot.data5[order(Portfolio),]
detach(plot.data5)
y.end.trunc <- plot.data5[Position(function(x) x==k.truncate2, plot.data5$Threshold),]$Loss
y.start.trunc <- plot.data5[Position(function(x) x==k.truncate2, subset(plot.data5,Global==T)$Threshold, right=T),]$Loss

#find region of rates for which 0 < w < k for which global maxima in losses exist
attach(plot.data4)
plot.data4 <- plot.data4[order(Portfolio),]
plot.data4.rev <- plot.data4[order(-rank(Portfolio)),]
detach(plot.data4)
y.endMax <- subset(plot.data4,Global.ExclTruncK==T)[1,]$Loss
x.endMax <- subset(plot.data4,Global.ExclTruncK==T)[1,]$Threshold
y.startMax <- subset(plot.data4.rev,Global.ExclTruncK==T)[1,]$Loss
x.startMax <- subset(plot.data4.rev,Global.ExclTruncK==T)[1,]$Threshold

lgnd.label <- expression(Rate~italic(P)[PP])
label.vec <- formatC(it.vec)

chosenFont <- "Calibri"
chosenFont <- "Times New Roman"


# -- full plot
plot.full <- 
  ggplot(plot.data, aes(x=Threshold, y=Loss)) + 
  geom_point(aes(x=Threshold,y=Loss, color=Portfolio, shape=Portfolio), data=plot.data2, size=1.75) +
  geom_line(aes(x=Threshold,y=Loss, color=Portfolio), size=0.5) +
  labs(y="Loss (%)",x=bquote(Default~thresholds~italic(d)~on~italic(g)[1])) + theme_minimal() + 
  theme(text=element_text(family=chosenFont, size=13),
        legend.position="bottom") + 
  scale_shape_manual(values=1:it.max, name=lgnd.label) +
  scale_color_manual(name=lgnd.label,
                     values = rev(colorRampPalette(brewer.pal(10, "Paired"))(it.max)))  +
  scale_y_continuous(breaks= pretty_breaks(), labels=percent)
plot.full


# -- zoomed plot
# zoom bounding box
xlim <- c(0,2); ylim <- c(0.3975, 0.3998)

plot.zoom <- 
  plot.full + coord_cartesian(xlim=xlim, ylim=ylim) + 
  geom_segment(aes(x=0,xend=Threshold, y=Loss, yend=Loss, color=Portfolio), linetype="dashed", data=plot.data3) + 
  geom_point(aes(x=Threshold,y=Loss, color=Portfolio, shape=Portfolio), data=plot.data2, size=2.5) +
  geom_point(aes(x=Threshold,y=Loss), data=plot.data3, size=7, colour="black", shape=1) +
  #geom_segment(aes(x=k.truncate, xend=k.truncate, y=y.start, yend=y.end), size=0.5, linetype="solid", color="gray50",alpha=1,
   #            arrow=arrow(angle=20,ends = "both", type="closed", length = unit(2, "mm"))) + 
  scale_x_continuous(breaks=pretty_breaks(n=3)) + 
  annotate("rect",xmin=k.truncate-0.25, xmax=k.truncate+0.25, ymin=y.start*0.999, ymax=y.end*1.001, fill="white", alpha=0.5, color="gray25") + 
  theme(legend.position="none", axis.text.x = element_text(margin=unit(c(0,0,0,0),"mm"),size=9),
        axis.text.y=element_text(margin=unit(c(0,0,0,0),"mm"),size=9),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(color='black', fill="white"), 
        plot.background=element_rect(color="white"),
        plot.margin = unit(c(0,0,0,0),"mm"))
plot.zoom


# annotate main plot further
plot.full2 <- 
  plot.full + geom_vline(xintercept=k.truncate2, size=0.5, color="gray50") + 
  geom_segment(aes(x=0,xend=40, y=Loss, yend=Loss, color=Portfolio), linetype="dashed", 
               data=subset(plot.data4, Global.ExclTruncK==T)) + 
  geom_point(aes(x=Threshold,y=Loss), data=subset(plot.data4,Global.ExclTruncK==T), size=7, colour="black", shape=1) + 
  geom_point(aes(x=Threshold,y=Loss, color=Portfolio, shape=Portfolio), data=subset(plot.data4,Global.ExclTruncK==T), size=1.75) + 
  geom_point(aes(x=Threshold,y=Loss), data=plot.data5, size=7, colour="gray75", shape=1) + 
  geom_segment(aes(x=0,xend=40, y=Loss, yend=Loss, color=Portfolio), linetype="dashed", data=plot.data5) +
  annotate("rect",xmin=k.truncate2-2.5, xmax=k.truncate2+2.5, ymin=y.start.trunc*0.975, ymax=y.end.trunc*1.025, fill="white", alpha=0.5, color="gray25")
plot.full2


# -- merge plots
g <- plot.full2 + annotation_custom(grob = ggplotGrob(plot.zoom), xmin = 0, xmax=18,
                                   ymin = 0.415, ymax = 0.5)
g

dpi <- 200
ggsave(g, file="LossRateThresh_Markoviandefaults_g1_DD40_PP_25g1-trunc.png", width=1200/dpi, height=1000/dpi,dpi=dpi)


