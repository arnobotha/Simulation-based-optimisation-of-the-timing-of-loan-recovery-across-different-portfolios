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
load("4-Data-RandomDefaults-ArrearsRate.RData")


# ================= Create Main Plot

# Losses across Thresholds by Delinquency measure
# ---- Data Preparation for Plots
prefixed <- ""
k.truncate <- k
col.headings <- sapply(1:it.max,function(i)paste(prefixed, it.vec[i],sep=''))
colnames(mat.TotLoss.CD) <- col.headings
colnames(mat.d.CD) <- col.headings

plot.data.CD <- data.table( gather(as.data.frame(mat.TotLoss.CD),"Portfolio","Loss"),
                            Threshold=gather(as.data.frame(mat.d.CD),"Portfolio","Threshold")$Threshold)
# - dependent on simulation parameters
port.sum <- n * principal
plot.data.CD[, Loss := Loss / port.sum]


plot.data <- subset(plot.data.CD, Threshold < 41)
plot.data2 <- plot.data #for plotting points
plot.data3 <- data.frame(Portfolio=NA,Loss=NA,Threshold=NA)

#manually prepare dataset for plotting points in order to mitigate overplotting
for (i in 1:it.max) {
  
  #decrease number of points after eaching w=k
  treat.data <- subset(plot.data2, Portfolio == paste(prefixed, it.vec[i],sep='') & Threshold > k.truncate) 
  plot.data2 <- subset(plot.data2, !(Portfolio == paste(prefixed, it.vec[i],sep='') & Threshold > k.truncate))
  sel.rows <- seq(1,nrow(treat.data),by=2)
  plot.data2 <- rbind(plot.data2, treat.data[sel.rows,])
  
  plot.data3 <- rbind(plot.data3,
                      data.frame(Portfolio=paste(prefixed, it.vec[i],sep=''),
                                 Loss=min(mat.TotLoss.CD[,i]) / port.sum,
                                 Threshold=mat.d.CD[Position(function(x) x==min(mat.TotLoss.CD[,i]),mat.TotLoss.CD[,i]),i]
                      )
  )
}

plot.data3 <- na.omit(plot.data3)


#find region of portfolios (ito probability (b)) for which w=k yields minima in losses
#this is simply for plotting reasons
attach(plot.data3)
plot.data3 <- plot.data3[order(Portfolio),]
detach(plot.data3)
y.start <- plot.data3[Position(function(x) x==k.truncate, plot.data3$Threshold),]$Loss
y.end <- plot.data3[Position(function(x) x==k.truncate, plot.data3$Threshold, right=T),]$Loss

lgnd.label <- expression(Arrears~Loss~Rate~italic(r)[A])
label.vec <- percent(it.vec)

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
  scale_shape_manual(values=1:it.max, name=lgnd.label, guide = guide_legend(nrow=2), labels=label.vec) +
  scale_color_manual(name=lgnd.label, labels=label.vec,
                     values = rev(colorRampPalette(brewer.pal(10, "Paired"))(it.max)))  +
  scale_y_continuous(breaks= pretty_breaks(), labels=percent) 
plot.full


# -- zoomed plot
# zoom bounding box
xlim <- c(2,8); ylim <- c(0.285, 0.355)

plot.zoom <- 
  plot.full + coord_cartesian(xlim=xlim, ylim=ylim) + 
  geom_segment(aes(x=0,xend=Threshold, y=Loss, yend=Loss, color=Portfolio), linetype="dashed", data=plot.data3) + 
  geom_point(aes(x=Threshold,y=Loss, color=Portfolio, shape=Portfolio), data=plot.data2, size=2.5) +
  #geom_segment(aes(x=k.truncate, xend=k.truncate, y=y.start, yend=y.end), size=1, linetype="solid", color="gray50",alpha=1,
   #            arrow=arrow(angle=20,ends = "both", type="closed", length = unit(2.5, "mm"))) + 
  geom_point(data=plot.data3, aes(x=Threshold, y=Loss, color=Portfolio), size=7, colour="black", shape=1) + 
  scale_x_continuous(breaks=pretty_breaks()) + 
  annotate("rect",xmin=k.truncate-0.5, xmax=k.truncate+0.5, ymin=y.start*0.98, ymax=y.end*1.02, fill="white", alpha=0.5, color="gray25") + 
  theme(legend.position="none", axis.text.x = element_text(margin=unit(c(0,0,0,0),"mm"),size=9),
        axis.text.y=element_text(margin=unit(c(0,0,0,0),"mm"),size=9),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(color='black', fill="white"), 
        plot.background=element_rect(color="white"),
        plot.margin = unit(c(0,0,0,0),"mm"))
plot.zoom


# -- merge plots
g <- plot.full + annotation_custom(grob = ggplotGrob(plot.zoom), xmin = 20, xmax=40,
                                   ymin = 0.26, ymax = 0.7)
g


dpi <- 200 
ggsave(g, file="LossRateThresh_randomdefaults_g1_ra_6g1-trunc.png", width=1200/dpi, height=1000/dpi,dpi=dpi)

