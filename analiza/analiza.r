# 4. faza: Analiza podatkov
library("data.table")   
library(cowplot)


graf1 <- ggplot(data = SkupnaTabela) +
  geom_bar(aes(x=reorder(Country, desc(Dohodek.2019)), y=Dohodek.2019), stat = "Identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=8)) +
  xlab("Country") + ylab("Dohodek") + ggtitle("Višina dohodka po državah") +
  geom_hline(aes(yintercept = mean(Dohodek.2019)), size=1.5)

print(graf1)

obdrzi_stolpce <- c("Country","Kriminal.2013", "Kriminal.2014", "Kriminal.2015", "Kriminal.2016", "Kriminal.2017", "Kriminal.2018", "Kriminal.2019")
Leto <- c(2013,2014,2015,2016,2017,2018,2019)

Slovenija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Slovenia")
Slovenija <- Slovenija[, -1]
Slovenija <- transpose(Slovenija)
colnames(Slovenija)[1] <- "Slovenia"
Slovenija <- cbind(Leto, Slovenija)



Avstrija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Austria")
Avstrija <- Avstrija[, -1]
Avstrija <- transpose(Avstrija)
colnames(Avstrija)[1] <- "Austria"
Avstrija <- cbind(Leto, Avstrija)

Italija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Italy")
Italija <- Italija[, -1]
Italija <- transpose(Italija)
colnames(Italija)[1] <- "Italy"
Italija <- cbind(Leto, Italija)

Nemcija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Germany")
Nemcija <- Nemcija[, -1]
Nemcija <- transpose(Nemcija)
colnames(Nemcija)[1] <- "Germany"
Nemcija <- cbind(Leto, Nemcija)

Francija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "France")
Francija <- Francija[, -1]
Francija <- transpose(Francija)
colnames(Francija)[1] <- "France"
Francija <- cbind(Leto, Francija)

Islandija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Iceland")
Islandija <- Islandija[, -1]
Islandija <- transpose(Islandija)
colnames(Islandija)[1] <- "Iceland"
Islandija <- cbind(Leto, Islandija)

Svedska <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Sweden")
Svedska <- Svedska[, -1]
Svedska <- transpose(Svedska)
colnames(Svedska)[1] <- "Sweden"
Svedska <- cbind(Leto, Svedska)

Grcija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Greece")
Grcija <- Grcija[, -1]
Grcija <- transpose(Grcija)
colnames(Grcija)[1] <- "Greece"
Grcija <- cbind(Leto, Grcija)

graf2 <- ggplot()+
  geom_line(data = Slovenija, aes(x=Leto, y=Slovenia, colour = "Slovenia"))+
  geom_line(data = Avstrija, aes(x=Leto, y=Austria, colour = "Austria"))+
  geom_line(data = Italija, aes(x=Leto, y=Italy, colour = "Italy"))+
  geom_line(data = Nemcija, aes(x=Leto, y=Germany, colour = "Germany"), size = 1.5)+
  geom_line(data = Francija, aes(x=Leto, y=France, colour = "France"))+
  geom_line(data = Islandija, aes(x=Leto, y=Iceland, colour = "Iceland"))+
  geom_line(data = Svedska, aes(x=Leto, y=Sweden, colour = "Sweden"), size = 1.5)+
  geom_line(data = Grcija, aes(x=Leto, y=Greece, colour = "Greece"))+
  scale_colour_manual("",
                      breaks = c("Slovenia", "Austria", "Italy", "Germany", "France", "Iceland", "Sweden", "Greece"),
                      values = c("Slovenia" = "blue", "Austria" = "red", "Italy" = "brown", "Germany" = "black", "France" = "green", "Iceland" = "gray", "Sweden" = "yellow", "Greece" = "orange"))+
  xlab("Leto") + ylab("Indeks kriminala") 

print(graf2)


Slovenija2 <- Dohodek %>% filter(Country == "Slovenia")
Slovenija2 <- Slovenija2[,-1]
Slovenija2 <- transpose(Slovenija2)
colnames(Slovenija2)[1] <- "Slovenia_Dohodek"
Slovenija2 <- cbind(Leto, Slovenija2)

Slovenija3 <- Population %>% filter(Country == "Slovenia")
Slovenija3 <- Slovenija3[,-1]
Slovenija3 <- transpose(Slovenija3)
colnames(Slovenija3)[1] <- "Slovenia_Population"
Slovenija3 <- cbind(Leto, Slovenija3)

Slovenija4 <- Izobrazba %>% filter(Country == "Slovenia")
Slovenija4 <- Slovenija4[,-1]
Slovenija4 <- transpose(Slovenija4)
colnames(Slovenija4)[1] <- "Slovenia_Izobrazba"
Slovenija4 <- cbind(Leto, Slovenija4)

Svica2 <- Dohodek %>% filter(Country == "Switzerland")
Svica2 <- Svica2[,-1]
Svica2 <- transpose(Svica2)
colnames(Svica2)[1] <- "Switzerland_Dohodek"
Svica2 <- cbind(Leto, Svica2)

Svica3 <- Population %>% filter(Country == "Switzerland")
Svica3 <- Svica3[,-1]
Svica3 <- transpose(Svica3)
colnames(Svica3)[1] <- "Switzerland_Population"
Svica3 <- cbind(Leto, Svica3)

Svica4 <- Izobrazba %>% filter(Country == "Switzerland")
Svica4 <- Svica4[,-1]
Svica4 <- transpose(Svica4)
colnames(Svica4)[1] <- "Switzerland_Izobrazba"
Svica4 <- cbind(Leto, Svica4)


graf3 <- ggplot(data = Slovenija2, aes(x=Leto, y=Slovenia_Dohodek,colour = "Slovenia"), stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+ theme(legend.position = "none") +
  ylab("Dohodek") 

graf4 <- ggplot(data = Slovenija3, aes(x=Leto, y=Slovenia_Population, colour = "Slovenia"), stat = "Identity", show.legend = F) +
  geom_line() + geom_point() + theme(legend.position = "none") +
  ylab("Populacija") 

graf5 <- ggplot(data = Slovenija4, aes(x=Leto, y=Slovenia_Izobrazba, colour = "Slovenia"), stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+ theme(legend.position = "none") +
  ylab("Izobrazba") 


graf6 <- ggplot(data = Svica2, aes(x=Leto, y=Switzerland_Dohodek), colour = "black", stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+
  ylab("Dohodek") 

graf7 <- ggplot(data = Svica3, aes(x=Leto, y=Switzerland_Population), stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+
  ylab("Populacija") 

graf8 <- ggplot(data = Svica4, aes(x=Leto, y=Switzerland_Izobrazba), stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+
  ylab("Izobrazba") 

skupek1 <- plot_grid(graf3, graf4, graf5, graf6, graf7, graf8)
print(skupek1)


graf_DV_2013 <- ggplot(Tabela2013, aes(x=Dohodek.2013, y = Varnost.2013))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2013") + xlab("Visina place") + ylab("Indeks varnosti")

graf_DV_2014 <- ggplot(Tabela2014, aes(x=Dohodek.2014, y = Varnost.2014))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2014") + xlab("Visina pla?e") + ylab("Indeks varnosti")

graf_DV_2015 <- ggplot(Tabela2015, aes(x=Dohodek.2015, y = Varnost.2015))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2015") + xlab("Visina place") + ylab("Indeks varnosti")

graf_DV_2016 <- ggplot(Tabela2016, aes(x=Dohodek.2016, y = Varnost.2016))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2016") + xlab("Višina plače") + ylab("Indeks varnosti")

graf_DV_2017 <- ggplot(Tabela2017, aes(x=Dohodek.2017, y = Varnost.2017))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2017") + xlab("Višina plače") + ylab("Indeks varnosti")

graf_DV_2018 <- ggplot(Tabela2018, aes(x=Dohodek.2018, y = Varnost.2018))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2018") + xlab("Višina plače") + ylab("Indeks varnosti")

graf_DV_2019 <- ggplot(Tabela2019, aes(x=Dohodek.2019, y = Varnost.2019))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2019") + xlab("Višina plače") + ylab("Indeks varnosti")

skupek2 <- plot_grid(graf_DV_2013,graf_DV_2015,graf_DV_2017, graf_DV_2019)
print(skupek2)




graf_PK_2013 <- ggplot(Tabela2013, aes(x=Population.2013, y = Kriminal.2013))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2013") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

graf_PK_2014 <- ggplot(Tabela2014, aes(x=Population.2014, y = Kriminal.2014))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2014") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

graf_PK_2015 <- ggplot(Tabela2015, aes(x=Population.2015, y = Kriminal.2015))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2015") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

graf_PK_2016 <- ggplot(Tabela2016, aes(x=Population.2016, y = Kriminal.2016))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2016") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

graf_PK_2017 <- ggplot(Tabela2017, aes(x=Population.2017, y = Kriminal.2017))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2017") + xlab("Število prebivalceve") + ylab("Indeks kriminala")

graf_PK_2018 <- ggplot(Tabela2018, aes(x=Population.2018, y = Kriminal.2018))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2018") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

graf_PK_2019 <- ggplot(Tabela2019, aes(x=Population.2019, y = Kriminal.2019))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2019") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

skupek3 <- plot_grid(graf_PK_2013,graf_PK_2015,graf_PK_2017, graf_PK_2019)
print(skupek3)








