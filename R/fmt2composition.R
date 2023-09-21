##Composition for FMT2
library(RColorBrewer)

##fmt2 composition at phylum level
##08022023-modified

fmt2comp<-read.csv("../data/fmt2-composition-phylum.csv",header = T)
fmt2compg<-read.csv("../data/fmt2-composition-genus.csv",header = T)

str(fmt2comp)


fmt2comp$Group <- factor(fmt2comp$Group, levels = c('Control','FMT'),ordered = TRUE)
fmt2compg$Group <- factor(fmt2compg$Group, levels = c('Control','FMT'),ordered = TRUE)

#######
#farm phylum relative abundance 03-30-2023
dfphylum<-fmt2comp%>%filter(Level =="Phylum")

dfphylum$Name

dfphylum$Name <- factor(dfphylum$Name,levels = c( "Thermoplasmatota",
                                                  "Actinobacteriota",
                                                  "Verrucomicrobiota",
                                                  "Desulfobacterota",
                                                  "Cyanobacteria",
                                                  "Proteobacteria",
                                                  "Campylobacterota",
                                                  "Bacteroidota",
                                                  "Firmicutes"))

p2<-dfphylum%>%
  ggplot(aes(fill=Name, y=RA, x=Group)) +
  geom_bar(stat = "identity",width = 0.6)+
  scale_fill_brewer(palette = "Paired",
                    name="Phylum",
                    labels = c(
                      expression(italic("Thermoplasmatota")),
                      expression(italic("Actinobacteriota")),
                      expression(italic("Verrucomicrobiota" )),
                      expression(italic("Desulfobacterota")),
                      expression(italic("Cyanobacteria")),
                      expression(italic("Proteobacteria")),
                      expression(italic("Campylobacterota")),
                      expression(italic("Bacteroidota")),
                      expression(italic("Firmicutes"))))+
  ylab("Relative abundance")+theme_bw()+
  xlab(" ")+theme(axis.title.x = element_blank(),legend.text.align = 0)


p2

##Figure 9 for FMT paper, Aug-03-2023
ggarrange(p1,p2,labels = c("A", "B"),
          ncol = 2, nrow = 1)



#########Figure 10 for FMT paper, Aug-04-2023
########fmt2 genus composition stacked bar-plot
dfgenus<-fmt2compg%>%filter(Level =="Genus")

dfgenus$Name

dfgenus$Name <- factor(dfgenus$Name,levels = c(
  "Other",
  "Clostridia_UCG.014",
  "Ruminococcaceae(f)",
  "Oscillospiraceae_UCG.005",
  "Bacteroides",
  "Lactobacillus",
  "Gastranaerophilales",
  "Helicobacter",
  "Faecalibacterium",
  "Ruminococcus_torques_group",
  "Lachnospiraceae(f)",
  "Clostridia_vadinBB60_group"
))


str(dfgenus)


fmt2genus<-dfgenus%>%
  ggplot(aes(x=Group, y=RA, fill=Name))+
  geom_bar(stat = "identity", position = "fill")+
  scale_fill_manual(
    values =c("#FFCCCC","#CC99FF", "#4C9900","#3399FF","#FF3333",
              "#A0A0A0","#CC6600", "#FFFF00","#CCE5FF",
               "#EE82EE","#3CB371", "#FFD700"),

    labels = c(expression(italic("Other")),
               expression(italic("Clostridia_UCG.014")),
               expression(italic("Ruminococcaceae(f)")),
               expression(italic("Oscillospiraceae_UCG.005")),
               expression(italic("Bacteroides")),
               expression(italic("Lactobacillus")),
               expression(italic("Gastranaerophilales")),
               expression(italic("Helicobacter")),
               expression(italic("Faecalibacterium")),
               expression(italic("Ruminococcus_torques_group")),
               expression(italic("Lachnospiraceae(f)")),
               expression(italic("Clostridia_vadinBB60_group"))))+
  ylab("Relative abundance")+theme_bw()+
  xlab(" ")+theme(axis.title.x = element_blank(),legend.text.align = 0)

fmt2genus
