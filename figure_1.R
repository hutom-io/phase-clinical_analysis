library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)


df_fin <-get(load('df_100_220530.RData'))




dhms <- function(t){
  paste(paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
              ,formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
              ,formatC(t %% 60, width = 2, format = "d", flag = "0")
              ,sep = ":"
  )
  )
}



#phase_duration_idx <- c(paste0('phase_s_',3:20),'OP_time')
phase_duration_idx <- c(paste0('phase_s_',3:20))
phase_count_idx <- c(paste0('phase_c_',3:20))
phase_color = c('#810000','#a50005','#cb0009','#f01929','#ff5543','#ff7a4d','#ff9043',
                '#ffc600' ,'#ffd600','#ffe161','#ffed79','#ffe901','#efd900','#d1bb00',
                '#aaef69', '#63eb40',  '#00e127','#00d100','#00af00','#009401','#007e01')

phase_color_new <- c()

for(i in 1:length(phase_color)){
  phase_color_new[i] <- phase_color[22-i]
}


phase_color_ana <- phase_color_new[3:20]
df_phase <- df_fin[phase_duration_idx]
phase_vis <- df_phase %>% gather(variable,value)
phase_vis <- phase_vis[phase_vis$value !=0,]
phase_vis$variable <- factor(phase_vis$variable,levels =phase_duration_idx)
phase_n <- summary(phase_vis$variable)
save(phase_n,file = 'phase_n.Rdata')
time_breaks <- seq(0,max(phase_vis$value),600)
time_breaks <- append(time_breaks,time_breaks[length(time_breaks)]+600)
time_breaks <- time_breaks/60

time_label <- c()
for(i in 1:length(time_breaks)){
  time_label[i] <-hms(time_breaks[i])
}


p1 <- ggplot(data= phase_vis,mapping = aes(x= variable,y= value/60,fill = variable))+ 
  stat_boxplot(geom= 'errorbar', width = 0.2) +
  geom_boxplot() +
  xlab('')+
  ylab('duration (minutes)')+
  scale_fill_manual(values = phase_color_ana)+
  scale_x_discrete(labels = paste0(c(paste0('phase ',3:20),'operation time')))+
  scale_y_continuous(limits = c(0,40), expand = c(0,0), 
                     breaks = c(seq(0,40,10)), labels = c(seq(0,40,10))) + 
  #jtools::theme_apa() + 
  theme_bw() + 
  theme(axis.line.y = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.grid.major.y = element_line(colour=),
        plot.title = element_text(hjust = 0.5,size = 40),
        axis.text.x = element_text(angle = 70,vjust = 0.5),legend.position="none", axis.text.y = element_text(hjust=1),text=element_text(
          family="Times-Roman",size = 20),plot.margin = unit(c(-0.1, 0.5, 0, 0.5), "cm"))
  
p2 <- ggplot(data= phase_vis,mapping = aes(x= variable,y= value/60,fill = variable))+ 
  geom_point() +
  xlab('')+
  ylab('')+
  scale_fill_manual(values = phase_color_ana)+
  scale_x_discrete(labels = paste0(c(paste0('phase ',3:20),'operation time')))+
  theme_bw()+
  scale_y_continuous(limits = c(38.75,90), expand = c(0,0), 
                     breaks = c(40,90), labels = c('',90)) + 
  theme_bw()+
  theme(axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
    legend.position="none", axis.text.y = element_text(hjust=1),text=element_text(
      family="Times-Roman",size = 20),plot.margin = unit(c(0.5, 0.5, 0, 0.5), "cm"))
  
 
gA <- ggplotGrob(p1)
gB <- ggplotGrob(p2)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
p_list <- list(gB,gA)

grid.arrange(grobs = p_list, ncol=1, heights=c(0.18,0.82))
#grid.draw(p)
#cairo_ps("./figure_3.eps",width=10, height=16)
#dev.off()

library(extrafont)
font_import()

ggsave(file = 'figure1.eps', arrangeGrob(grobs = p_list, ncol = 1,heights=c(0.18,0.82)),height = 10,width = 16) 

ggsave(file = 'figure1.eps', height = 10,width = 16) 
## convert png to eps file in web



#quartz.save( type = "eps", device = dev.cur(), dpi = 300, )