##July 17th, 2023, modify figures for publication

fmt2alpha<- read.csv("../data/fmt2-alpha.csv",header = T)



fmt2alpha$Group<-factor(fmt2alpha$Group, levels=c("Control","FMT"))



data_long<-gather(fmt2alpha,key= "cata", value="alpha", `observed_features`:`faith_pd`,factor_key = T)


levels(data_long$cata)

levels(data_long$cata) <- c("Observed features","Shannon","Pielou's evenness","Faith's pd")

str(fmt2alpha)

fmt2alpha$DPM<-factor(fmt2alpha$DPM,levels = c("DPM-0","DPM-5","DPM-10","DPM-15"))


fmt2alpha%>%
  group_by(Group)%>%
  summarise(Mean_evenness = mean(pielou_evenness),
            Mean_shannon = mean(shannon_entropy),
            Mean_otu = mean(observed_features),
            Mean = mean(faith_pd))%>%
  kbl(caption = "A", digits = 2)%>%
  kable_classic(full_width = F, html_font = "Cambria")


fmt2alpha%>%
  group_by(Group,DPM)%>%
  summarise(Evenness = mean(pielou_evenness),
            Shannon = mean(shannon_entropy),
            Observed_features = mean(observed_features),
            Faith_pd= mean(faith_pd))%>%
  kbl(caption = "A", digits = 2)%>%
  kable_classic(full_width = F, html_font = "Cambria")







alpha=ggboxplot(data_long,x="Group",y="alpha",fill="Group",palette = c("#FF9999","#66B2FF"),
                ylab = "Alpha diversity indexes",xlab=" ", legend.title= expression(italic("Group")))+
  stat_compare_means(aes(group=Group), label = "p.signif",label.x.npc="center")+
  stat_ellipse(geom = "polygon",
               alpha = 0.001)+
  scale_x_discrete(labels=c("FMT" = "FMT", "Control" = "Control"))+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        legend.text = element_text(size = 12, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank())+
  facet_wrap(~cata,scale="free")


alpha



alpha_date=ggboxplot(data_long,x="DPM",y="alpha",fill="Group",palette = c("#FF9999","#66B2FF"),
                     ylab = "Alpha diversity indexes",xlab=" ", legend.title= expression(italic("Group")))+
  stat_compare_means(aes(group=Group), label = "p.signif",label.x.npc="center")+
  stat_ellipse(geom = "polygon",
               alpha = 0.001)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        legend.text = element_text(size = 12, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank())+
  facet_wrap(~cata,scale="free")


alpha_date

