library(ANCOMBC)
library(qiime2R)
library(tidyverse)
library(ggplot2)
library(ggpubr)



SVs<-read_qza("../qiime2/filtered-sequences/feature-frequency-filtered-table.qza")

metadata<-read_tsv("../qiime2/fmt2metadata.tsv")

taxonomy<-read_qza("../qiime2/taxonomy/taxonomy.qza")

taxonomy<-parse_taxonomy(taxonomy$data)

physeq<-qza_to_phyloseq(
  features="../qiime2/filtered-sequences/feature-frequency-filtered-table.qza",
  tree="../qiime2/readin/rooted-tree.qza",
  taxonomy="../qiime2/taxonomy/taxonomy.qza",
  metadata = "../qiime2/fmt2metadata.tsv"
)
physeq

sample_data(physeq)[, 'Group'] <- factor(get_variable(sample_data(physeq), 'Group'), levels = c('Control', 'FMT'))
get_variable(sample_data(physeq), 'Group')



##Alpha Diversity
##observed features
otu<-read_qza("../qiime2/core-metrics-results/observed_features_vector.qza")
otu<-otu$data %>% rownames_to_column("Sample-ID")

metadata<-
  metadata %>%
  left_join(otu)


##evenness
evenness<-read_qza("../qiime2/core-metrics-results/evenness_vector.qza")
evenness<-evenness$data %>% rownames_to_column("Sample-ID")

metadata<-
  metadata %>%
  left_join(evenness)

##shannon
shannon<-read_qza("../qiime2/core-metrics-results/shannon_vector.qza")
shannon<-shannon$data %>% rownames_to_column("Sample-ID")


gplots::venn(list(metadata=metadata$`Sample-ID`, shannon=shannon$`Sample-ID`))

metadata<-
  metadata %>%
  left_join(shannon)

##faithpd
faithpd<-read_qza("../qiime2/core-metrics-results/faith_pd_vector.qza")
faithpd<-faithpd$data %>% rownames_to_column("Sample-ID")

metadata<-
  metadata %>%
  left_join(faithpd)

str(metadata)

metadata$DPM<-factor(metadata$DPM,levels = c("DPM-0","DPM-5","DPM-10","DPM-15"))
metadata$Group<-factor(metadata$Group,levels = c("Control","FMT"))


observedotu1=ggboxplot(metadata,x="Group",y="observed_features",fill="Group",
                       palette = c("#FFCC99","#66B2FF"),
                       ylab = "Observed features",xlab=" ")+
  labs(title = "FMT Trial-2 Alpha Diversity Observed Features")+
  stat_compare_means(aes(group=Group), label = "p.signif",label.x = 1.5, label.y = 550)

observedotu1
##

pieloue1=ggboxplot(metadata,x="Group",y="pielou_evenness",fill="Group",palette = c("#FFCC99","#66B2FF"),
                   ylab = "pielou-evenness",xlab=" ")+
  labs(title = "FMT Trial-2 Alpha Diversity Pielou-evenness")+
  stat_compare_means(aes(group=Group), label = "p.signif",label.x = 1.5)


pieloue1

##
faithpd1=ggboxplot(metadata,x="Group",y="faith_pd",fill="Group",palette = c("#FFCC99","#66B2FF"),
                   ylab = "faith-pd",xlab=" ")+
  labs(title = "FMT Trial-2 Alpha Diversity Faith-pd")+
  stat_compare_means(aes(group=Group), label = "p.signif",label.x = 1.5)

faithpd1
##

shannon1=ggboxplot(metadata,x="Group",y="shannon_entropy",fill="Group",palette = c("#FFCC99","#66B2FF"),
                   ylab = "shannon",xlab=" ")+
  labs(title = "FMT Trial-2 Alpha Diversity Shannon_entropy")+
  stat_compare_means(aes(group=Group), label = "p.signif",label.x = 1.5)

shannon1


## group and treatment alpha

observedotu=ggboxplot(metadata,x="DPM",y="observed_features",fill="Group",palette = c("#FFCC99","#66B2FF"),
                      ylab = "Observed features",xlab="Day Post Mingle")+
  labs(title = "FMT Trial-2 Alpha Diversity Observed Features")+
  stat_compare_means(aes(group=Group), label = "p.signif")

observedotu

pieloue=ggboxplot(metadata,x="DPM",y="pielou_evenness",fill="Group",palette = c("#FFCC99","#66B2FF"),
                  ylab = "pielou-evenness",xlab="Day Post Mingle")+
  labs(title = "FMT Trial-2 Alpha Diversity Pielou-evenness")+
  stat_compare_means(aes(group=Group), label = "p.signif")

pieloue


faithpd=ggboxplot(metadata,x="DPM",y="faith_pd",fill="Group",palette = c("#FFCC99","#66B2FF"),
                  ylab = "faith-pd",xlab="Day Post Mingle")+
  labs(title = "FMT Trial-2 Alpha Diversity Faith-pd")+
  stat_compare_means(aes(group=Group), label = "p.signif")

faithpd

shannon=ggboxplot(metadata,x="DPM",y="shannon_entropy",fill="Group",palette = c("#FFCC99","#66B2FF"),
                  ylab = "shannon_entropy",xlab="Day Post Mingle")+
  labs(title = "FMT Trial-2 Alpha Diversity Shannon_entropy")+
  stat_compare_means(aes(group=Group), label = "p.signif")

shannon

##
metadata %>%
  ggplot(aes(x=DPM, y=shannon_entropy, color=Group)) +
  stat_summary(geom="errorbar", fun.data=mean_se, width=0) +
  stat_summary(geom="line", fun.data=mean_se) +
  stat_summary(geom="point", fun.data=mean_se) +
  xlab("Day Post Mingle") +
  ylab("Shannon Diversity") +
  theme_q2r() + # try other themes like theme_bw() or theme_classic()
  scale_color_viridis_d(name="Group")


metadata %>%
  ggplot(aes(x=Group, y=shannon_entropy, fill=Group)) +
  stat_summary(geom="bar", fun.data=mean_se, color="black") + #here black is the outline for the bars
  geom_jitter(shape=21, width=0.2, height=0) +
  coord_cartesian(ylim=c(2,7)) + # adjust y-axis
  facet_grid(~DPM) + # create a panel for each body site
  xlab("Group") +
  ylab("Shannon Diversity") +
  theme_q2r() +
  scale_fill_manual(values=c("cornflowerblue","indianred")) + #specify custom colors
  theme(legend.position="none")



## beta diversity

##Plotting PCoA
uwunifrac<-read_qza("../qiime2/core-metrics-results/unweighted_unifrac_pcoa_results.qza")

wunifrac<-read_qza("../qiime2/core-metrics-results/weighted_unifrac_pcoa_results.qza")

bray<-read_qza("../qiime2/core-metrics-results/bray_curtis_pcoa_results.qza")


head(uwunifrac)

colnames(metadata)[1] <-'SampleID'

uwunifrac$data$Vectors %>%
  select(`SampleID`, PC1, PC2) %>%
  left_join(metadata)%>%
  ggplot(aes(x=PC1, y=PC2, color=DPM, shape=Group, size=shannon_entropy)) +
  geom_point(alpha=0.5) +labs(x="PC1(45.78%)", y= "PC2(10.61%)")+
  theme_q2r() +
  scale_shape_manual(values=c(16,9), name="Group") +
  scale_size_continuous(name="Shannon Diversity") +
  scale_color_discrete(name="DPM")


p1<-uwunifrac$data$Vectors %>%
  select(`SampleID`, PC1, PC2) %>%
  left_join(metadata)%>%
  ggplot(aes(x=PC1, y=PC2, color=DPM,shape=Group)) +
  geom_point(alpha=0.8,size=3) +labs(x="PC1(45.78%)", y= "PC2(10.61%)")+
  theme_q2r() +
  scale_shape_manual(values=c(16,9), name="Group")+
  scale_color_brewer(palette="Set1")+
  # stat_ellipse(geom = "polygon",
  #              alpha = 0.01)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        legend.text = element_text(size = 12, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank())


p1

fmt2p1<-p1+guides(color = guide_legend(order=2),
         shape = guide_legend(order=1))



p2<-uwunifrac$data$Vectors %>%
  select(`SampleID`, PC1, PC2) %>%
  left_join(metadata)%>%
  ggplot(aes(x=PC1, y=PC2, color=Group, shape=Group)) +
  geom_point(alpha=0.8,size=3) +labs(x="PC1(45.78%)", y= "PC2(10.61%)")+
  scale_shape_manual(values=c(16,9), name="Group")+
  scale_color_brewer(palette="Set1")+
  stat_ellipse(geom = "polygon",
               alpha = 0.01)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        legend.text = element_text(size = 12, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank())



##08022023 redo figure beta diversity PCoA

ggarrange(p2,fmt2p1,
          labels = c("A", "B"),
          ncol =2, nrow =1)


##weighted unifrac reviewer1's
##09-20-2023


p1<-wunifrac$data$Vectors %>%
  select(`SampleID`, PC1, PC2) %>%
  left_join(metadata)%>%
  ggplot(aes(x=PC1, y=PC2, color=DPM,shape=Group)) +
  geom_point(alpha=0.8,size=3) +labs(x="PC1(30.06%)", y= "PC2(14.19%)")+
  theme_q2r() +
  scale_shape_manual(values=c(16,9), name="Group")+
  scale_color_brewer(palette="Set1")+
  # stat_ellipse(geom = "polygon",
  #              alpha = 0.01)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        legend.text = element_text(size = 12, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank())


p1

fmt2p1<-p1+guides(color = guide_legend(order=2),
                  shape = guide_legend(order=1))



p2<-wunifrac$data$Vectors %>%
  select(`SampleID`, PC1, PC2) %>%
  left_join(metadata)%>%
  ggplot(aes(x=PC1, y=PC2, color=Group, shape=Group)) +
  geom_point(alpha=0.8,size=3) +labs(x="PC1(30.06%)", y= "PC2(14.19%)")+
  scale_shape_manual(values=c(16,9), name="Group")+
  scale_color_brewer(palette="Set1")+
  stat_ellipse(geom = "polygon",
               alpha = 0.01)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        legend.text = element_text(size = 12, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank())



ggarrange(p2,fmt2p1,
          labels = c("A", "B"),
          ncol =2, nrow =1)



##Plotting a Heatmap

SVs<-SVs$data


taxasums<-summarize_taxa(SVs, taxonomy)$Genus

taxa_heatmap(taxasums, metadata, "Group")



##Making a taxonomic barplot

taxa_barplot(taxasums, metadata, "Treatment_Isolation")



## ANCOM-BC


library(ANCOMBC)
library(microbiome)
library(DT)
library(writexl)


## ANCOM-BC at phylum level

phylum_data = aggregate_taxa(physeq, "Phylum")


out = ancombc(phyloseq = phylum_data, formula = "Group",
              p_adj_method = "holm", prv_cut = 0.10, lib_cut = 1000,
              group = "Group", struc_zero = TRUE, neg_lb = TRUE, tol = 1e-5,
              max_iter = 100, conserve = TRUE, alpha = 0.05, global = TRUE)


res = out$res

knitr::kable(res$diff_abn)


tab_lfc = res$lfc
col_name = c("Treatment-Control")
colnames(tab_lfc) = col_name
tab_lfc %>% datatable(caption = "LFC from the Primary Result") %>%
  formatRound(col_name, digits = 2)



tab_se = res$se
colnames(tab_se) = col_name
tab_se %>% datatable(caption = "SEs from the Primary Result") %>%
  formatRound(col_name, digits = 2)

tab_w = res$W
colnames(tab_w) = col_name
tab_w %>% datatable(caption = "Test Statistics from the Primary Result") %>%
  formatRound(col_name, digits = 2)

tab_p = res$p_val
colnames(tab_p) = col_name
tab_p %>% datatable(caption = "P-values from the Primary Result") %>%
  formatRound(col_name, digits = 2)

tab_q = res$q_val
colnames(tab_q) = col_name
tab_q %>% datatable(caption = "Adjusted p-values from the Primary Result") %>%
  formatRound(col_name, digits = 2)

tab_diff = res$diff_abn
colnames(tab_diff) = col_name
tab_diff %>%
  datatable(caption = "Differentially Abundant Phylum
            from the Primary Result")

## save the numerical data

write_xlsx(res, "../data/fmt2raphylumancomnew.xlsx")


#### ANCOM-BC at genus level

genus_data = aggregate_taxa(physeq, "Genus")

out = ancombc(phyloseq = genus_data, formula = "Group",
              p_adj_method = "holm", zero_cut = 0.90, lib_cut = 1000,
              group = "Group", struc_zero = TRUE, neg_lb = TRUE, tol = 1e-5,
              max_iter = 100, conserve = TRUE, alpha = 0.05, global = TRUE)


res = out$res

knitr::kable(res$diff_abn)

tab_coef = res$beta
col_name = c("Group")
colnames(tab_coef) = col_name
tab_coef %>% datatable(caption = "Coefficients from the Primary Result") %>%
  formatRound(col_name, digits = 2)



tab_se = res$se
colnames(tab_se) = col_name
tab_se %>% datatable(caption = "SEs from the Primary Result") %>%
  formatRound(col_name, digits = 2)



tab_w = res$W
colnames(tab_w) = col_name
tab_w %>% datatable(caption = "Test Statistics from the Primary Result") %>%
  formatRound(col_name, digits = 2)



tab_p = res$p_val
colnames(tab_p) = col_name
tab_p %>% datatable(caption = "P-values from the Primary Result") %>%
  formatRound(col_name, digits = 2)


tab_q = res$q
colnames(tab_q) = col_name
tab_q %>% datatable(caption = "Adjusted p-values from the Primary Result") %>%
  formatRound(col_name, digits = 2)


tab_diff = res$diff_abn
colnames(tab_diff) = col_name

tab_diff %>%
  datatable(caption = "Differentially Abundant Taxa
            from the Primary Result")


write_xlsx(res, "../data/fmt2ragenusancom.xlsx")





