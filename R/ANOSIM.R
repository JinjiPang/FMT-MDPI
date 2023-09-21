##
nmddf<-read.csv("../data/fmt2-nmd.csv",header = T)

com=nmddf[, 5:ncol(nmddf)]

env=nmddf[, 1:4]

m_com = as.matrix(com)
nmds <- metaMDS(com, distance = "bray", k = 3)


ano = anosim(m_com, nmddf$Group, distance = "bray", permutations = 999)


summary(ano)

plot(ano, ylab=" ",xlab=" ")
p3 <- recordPlot()
plot.new() ## clean up device
p3 # redraw


######3

ano1 = anosim(m_com, nmddf$DPM, distance = "bray", permutations = 999)

summary(ano1)

plot(ano1, ylab=" ",xlab=" ")
