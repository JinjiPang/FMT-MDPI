

phylum<-read.csv("fq1tc/data/fq1tcphylum.csv",header = T)


phylum<-phylum%>%select(-c("Sample","Treatment_Isolation","Age"))

#convert data frame from a "wide" format to a "long" format
pcm = melt(phylum, id = c("Group","DPT"))


pcm$DPT <- factor(pcm$DPT,levels=unique(pcm$DPT))
pcm$Group <- factor(pcm$Group,levels=unique(pcm$Group))

str(pcm)











dfnew<-df[,c(1, 7,9)]

#convert data frame from a "wide" format to a "long" format
pcm1 = melt(df, id = c("Sample"))

colours = c( "#CCCCFF",  "#FFFF00", "#679289",
             "#FFCCCC")

str(dfnew)
dfnew$Sample <- factor(dfnew$Sample,levels=unique(dfnew$Sample))


xx = ggplot(dfnew, aes(x = Sample, y = Phylum)) +
  geom_point(aes(size = percentage*100, fill = Phylum), alpha = 0.75, shape = 21) +
  scale_size_continuous(limits = c(0.0001, 100), range = c(1,17), breaks = c(1,5,10,90)) +
  labs( x= "", y = "", size = "Relative Abundance (%)", fill = "")  +
  theme(legend.key=element_blank(),
        axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 90, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(colour = "black", face = "bold", size = 11),
        legend.text = element_text(size = 10, face ="bold", colour ="black"),
        legend.title = element_text(size = 12, face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.position = "right") +
  scale_fill_manual(values = colours, guide = FALSE) +
  scale_y_discrete(limits = rev(levels(dfnew$Phylum)))

xx





