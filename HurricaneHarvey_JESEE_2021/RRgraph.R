#install.packages('tidyverse')
#install.packages('readxl')
library(ggplot2)
library(egg)
library(readxl)
library(hablar)
library(dplyr)
library(stringr)
library(scales)
library(plyr)

AXIS_Y_SIZE<-14
LEGEND_SIZE<-14
Y_TITLE_SIZE<-14
LINE_WIDTH<-.5 #erro bar line width
GRID_WIDTH<-.9
POINT_WIDTH<-1.8 #error bar middle point width
DASH_WIDTH<-.2
DOT_SIZE<-.5
ERROR_BAR_TOP<-.2
LABEL_SIZE<-13
WORD_WRAP<-15
PLOT_MARGIN<-unit(c(.1,.2,-.2,.2), "cm")
LEGEND_MAR<-margin(-0.7,0,.4,0,"cm")
STRIP_LINES=0.2

merge_charts<-function (flood_df,filename,format='.pdf'){
  loutcomes<-list()
  loutcomes[[1]]<-c("CO Poisoning","Drowning","Hypothermia","Dehydration","Intestinal Infectious Diseases","Insect Bite","Heat Related Illness")
  loutcomes[[2]]<-c("All","Pregnancy Complications","ARI","Chest Pain/Palpitation","Asthma","Mortality")
  #loutcomes[[3]]<-c("ARI","Chest Pain/Palpitation","Asthma")
  
  #flood_df<-flood_df %>% retype()
  flood_df<-flood_df[order(flood_df$Outcome),]
  
  
  outcomes<-loutcomes[[1]]
  flood_df_sub<-subset(flood_df,Outcome %in% outcomes)
  p1<-make_chart(flood_df_sub)
  
  outcomes<-loutcomes[[2]]
  flood_df_sub<-subset(flood_df,Outcome %in% outcomes)
  p2<-make_chart(flood_df_sub,legend='bottom')
  
  #save the plot
  p<-ggarrange(p1,p2)
  ggsave(paste0(filename,format),plot=p,width = unit("8.5",'cm'),height=unit('8','cm'))
}


make_chart<-function (flood_df_sub,legend='none'){
  flood_df_sub$xindex<-rep(c(1,2,3),length(flood_df_sub$Outcome)/3)
  flood_df_sub<-flood_df_sub[order(flood_df_sub$Period),]
  flood_df_sub$Outcome = str_wrap(flood_df_sub$Outcome,width = WORD_WRAP)
  par(mfrow=c(2,1))
  p1 <- ggplot(flood_df_sub, aes(y = RR, x = xindex ,shape=factor(Period),color=factor(Period))) +   facet_wrap(~Outcome,nrow=1)+
      geom_errorbar(aes(ymax = conf25, ymin = conf95), size = LINE_WIDTH, width = 
                      ERROR_BAR_TOP) +
      geom_point(size = POINT_WIDTH) +
      scale_shape_manual(values=c(15,17,16))+
      scale_color_manual(values=c( "#009E73","#0072B2", "#D55E00"))+
      geom_hline(aes(yintercept = 1), size = DASH_WIDTH, linetype = "dashed")+
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", function(x) round(10^x,1)))+ 
      theme_bw()+
      theme(panel.grid.minor = element_blank(),panel.grid.major.x = element_blank(),
            legend.title = element_blank(),
            panel.grid.major.y = element_line(size = GRID_WIDTH),
            legend.position =legend,legend.margin=LEGEND_MAR,legend.text=element_text(size=LEGEND_SIZE),
            axis.text.x=element_blank(),axis.ticks.length.x=unit(0, "cm"),
            axis.text.y = element_text(size = AXIS_Y_SIZE,angle = 90),axis.title = element_text(size = Y_TITLE_SIZE),
            plot.margin = PLOT_MARGIN, strip.text = element_text(size=LABEL_SIZE), 
            panel.spacing.x = unit(0,'cm'),panel.border = element_rect(size=STRIP_LINES,linetype = 'solid'),
            axis.line = element_line(linetype = 'solid',size=LINE_WIDTH),strip.background = element_rect(colour="black", fill="gray95",size = LINE_WIDTH),)+
      ylab("Rate ratio") + xlab("")
  return(p1)
}
# --------------------------------------------------- flooded vs non flooded ----

all_df<-read_excel("Docum_for_paper.xlsx",sheet='dicho_comparision_RR',range='A17:J30')
colnames(all_df)<-c('Outcome',rep(c('RR','conf25','conf95'),7))
flood_df<-all_df[,1:4]
flood_df<-rbind(flood_df,all_df[,c(1,5,6,7)])
flood_df<-rbind(flood_df,all_df[,c(1,8,9,10)])
flood_df$Period<-rep(c('Flood Period','Post Flood 1', 'Post Flood 2'),each=length(unique(flood_df$Outcome)))
flood_df<-flood_df %>% retype()
merge_charts(flood_df,'Figx')

# ---------------------------------------------------  plot_cat_floodPeriod----

all_df<-read_excel("Docum_for_paper.xlsx",sheet='categorical_comp_RR',range='A3:J16')
colnames(all_df)<-c('Outcome',rep(c('RR','conf25','conf95'),7))
flood_df<-all_df[,1:4]
flood_df<-rbind(flood_df,all_df[,c(1,5,6,7)])
flood_df<-rbind(flood_df,all_df[,c(1,8,9,10)])
flood_df$Period<- rep(c('Low flood','Moderate flood', 'High flood'),each=length(unique(flood_df$Outcome)))
flood_df<-flood_df %>% retype()
flood_df$Period<- factor(flood_df$Period,levels = c('Low flood','Moderate flood', 'High flood'))
merge_charts(flood_df,'Fig4')

# ---------------------------------------------------  plot_cat_PostPeriod1 ----

all_df<-read_excel("Docum_for_paper.xlsx",sheet='categorical_comp_RR',range='A20:J33')
colnames(all_df)<-c('Outcome',rep(c('RR','conf25','conf95'),7))
flood_df<-all_df[,1:4]
flood_df<-rbind(flood_df,all_df[,c(1,5,6,7)])
flood_df<-rbind(flood_df,all_df[,c(1,8,9,10)])
flood_df$Period<- rep(c('Low flood','Moderate flood', 'High flood'),each=length(unique(flood_df$Outcome)))
flood_df<-flood_df %>% retype()
flood_df$Period<- factor(flood_df$Period,levels = c('Low flood','Moderate flood', 'High flood'))

merge_charts(flood_df,'Fig5')

# ---------------------------------------------------  plot_cat_PostPeriod2 ----

all_df<-read_excel("Docum_for_paper.xlsx",sheet='categorical_comp_RR',range='A37:J50')
colnames(all_df)<-c('Outcome',rep(c('RR','conf25','conf95'),7))
flood_df<-all_df[,1:4]
flood_df<-rbind(flood_df,all_df[,c(1,5,6,7)])
flood_df<-rbind(flood_df,all_df[,c(1,8,9,10)])
flood_df$Period<- rep(c('Low flood','Moderate flood', 'High flood'),each=length(unique(flood_df$Outcome)))
flood_df<-flood_df %>% retype()
flood_df$Period<- factor(flood_df$Period,levels = c('Low flood','Moderate flood', 'High flood'))

merge_charts(flood_df,'SupFig1')


# ------------------------REvision graphs --------------------------------------------------------------------------
all_df<-read_excel('merged_flood_cat.xlsx')
all_df$outcome<-mapvalues(all_df$outcome,from = c("ALL","DEATH","CO_Exposure", "Bite-Insect", "Dehydration", "Intestinal_infectious_diseases", "Pregnancy_complic","Asthma",'Chest_pain','Heat_Related_But_Not_dehydration'),
                          to= c("All","Mortality","CO Poisoning","Insect Bite", "Dehydration", "Intestinal Infectious Diseases","Pregnancy Complications","Asthma",'Chest Pain','Heat Related Illness'))

all_df<-all_df[grep('^Time_',all_df$covar),]
all_df$Period<-gsub('Time_','',all_df$covar)
all_df$Outcome<-all_df$outcome
merge_charts(all_df,'trial_graph2')
