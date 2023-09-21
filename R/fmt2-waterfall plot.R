

getwd()


water<-read.csv("../data/fmt2-ra-waterpy.csv",header = T)


water$LFC.Treatment.Control.

water%>%filter(Test=="TRUE")%>%ggplot(
  aes(x=Phylum, y=LFC.Treatment.Control.)
)+geom_bar(stat = "identity")
