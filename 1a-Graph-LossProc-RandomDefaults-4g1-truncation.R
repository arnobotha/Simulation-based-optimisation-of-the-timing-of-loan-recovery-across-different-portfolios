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
load("1a-Data-RandomDefaults-4g1.RData")


# ================= Create Main Plot

# Losses across Thresholds by Delinquency measure
plot.data <- data.table(rbind( data.frame(Measure="g1: CD", Threshold=vec.d.CD, Loss=vec.TotLoss.CD),
                    data.frame(Measure="g2: MD", Threshold=vec.d.MD, Loss=vec.TotLoss.MD),
                    data.frame(Measure="g3: DoD", Threshold=vec.d.DoD, Loss=vec.TotLoss.DoD) ))

# - dependent on simulation parameters
port.sum <- n * principal
plot.data[, Loss := Loss/ port.sum]

#plot.data <- subset(plot.data, Threshold < 40)
plot.data2 <- plot.data #for plotting points

#manually prepare dataset for plotting points in order to mitigate overplotting
#CD
treat.data <- subset(plot.data2, Measure == "g1: CD" & Threshold > 3 )
plot.data2 <- subset(plot.data2, !(Measure == "g1: CD" & Threshold > 0))
sel.rows <- seq(1,nrow(treat.data),by=2)
plot.data2 <- rbind(plot.data2, treat.data[sel.rows,])
#MD
treat.data <- subset(plot.data2, Measure == "g2: MD" & Threshold > 2)
plot.data2 <- subset(plot.data2, !(Measure == "g2: MD" & Threshold > 2))
sel.rows <- seq(1,nrow(treat.data),by=2)
plot.data2 <- rbind(plot.data2, treat.data[sel.rows,])
#DoD
treat.data <- subset(plot.data2, Measure == "g3: DoD" & Threshold > 2.5)
plot.data2 <- subset(plot.data2, !(Measure == "g3: DoD" & (Threshold > 2.5 | Threshold < 2.4)))
sel.rows <- seq(1,nrow(treat.data),by=2)
plot.data2 <- rbind(plot.data2, treat.data[sel.rows,])


label.vec <-list(bquote({italic(g)[1]}),bquote({italic(g)[2]}),bquote({italic(g)[3]}))

chosenFont <- "Calibri"
chosenFont <- "Times New Roman"

# -- full plot
plot.full <- ggplot(plot.data, aes(x=Threshold, y=Loss)) + 
  geom_point(aes(x=Threshold,y=Loss, color=Measure, shape=Measure), data=plot.data2, size=1.75) +
  geom_line(aes(x=Threshold, y=Loss, color=Measure), size = 0.5) + 
  #geom_smooth(aes(x=Threshold, y=Loss, color=Measure), method = "glm", formula = y~splines::bs(x,7), size = 1) + 
  labs(y="Loss (%)",x=bquote(Thresholds~italic(d))) + theme_minimal() + 
  theme(text=element_text(family=chosenFont, size=12),
        plot.title = element_text(size  = 14,face="bold",family="Calibri Light"),
        legend.position="bottom") + 
  scale_color_economist(name="Delinquency Measure",guide = guide_legend(ncol=3), labels=label.vec) +
  scale_shape_manual(values=c(1,16,8), 
                     name="Delinquency Measure",guide = guide_legend(ncol=3), labels=label.vec) +
  scale_y_continuous(breaks= pretty_breaks(), labels=percent)
plot.full


#annotations: minima
plot.data.min <- data.table(rbind( data.frame(Measure="g1: CD", Threshold=vec.d.CD[Position(function(x) x==min(vec.TotLoss.CD), vec.TotLoss.CD)], Loss=min(vec.TotLoss.CD)),
                        data.frame(Measure="g2: MD", Threshold=vec.d.MD[Position(function(x) x==min(vec.TotLoss.MD), vec.TotLoss.MD)], Loss=min(vec.TotLoss.MD)),
                        data.frame(Measure="g3: DoD", Threshold=vec.d.DoD[Position(function(x) x==min(vec.TotLoss.DoD), vec.TotLoss.DoD)], Loss=min(vec.TotLoss.DoD)) ))
plot.data.min[, Loss := Loss / port.sum]



# -- zoomed plot
# zoom bounding box
xlim <- c(1,5); ylim <- c(0.335,0.405)

plot.zoom <- plot.full + coord_cartesian(xlim=xlim, ylim=ylim) + geom_point(aes(color=Measure,shape=Measure), size=2) + 
  theme(legend.position="none", axis.line=element_blank(),axis.text.x = element_text(margin=unit(c(0,0,1,0),"mm"),size=9),
        axis.text.y=element_text(margin=unit(c(0,0,0,0),"mm"),size=9),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(color='black', fill="white"),
        plot.margin = unit(c(0,0,0,0),"mm")) +
  geom_segment(aes(x=Threshold,xend=Threshold, y=0, yend=Loss, color=Measure), linetype="dashed", data=plot.data.min) + 
  geom_segment(aes(x=0,xend=Threshold, y=Loss, yend=Loss, color=Measure), linetype="dashed", data=plot.data.min) + 
  geom_point(data=plot.data.min, aes(x=Threshold, y=Loss, color=Measure), size=7, colour="black", shape=1)
plot.zoom

# -- merge plots
g <- plot.full + annotation_custom(grob = ggplotGrob(plot.zoom), xmin = 25, xmax=60,
                                   ymin = 0.325, ymax = 0.5)
g

dpi <- 300
ggsave(g, file="LossRateThresh_randomdefaults_4g1-trunc.png", width=1200/dpi, height=1000/dpi,dpi=dpi)
