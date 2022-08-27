# 3. faza: Vizualizacija podatkov

library("data.table")   
library(cowplot)

# dohodek za leto 2019 za različne države

tabela1 <- skupna %>% filter(Leto == 2019 & tip == "dohodek")

graf1 <- ggplot(data = tabela1) +
  geom_bar(aes(x=reorder(Drzava, desc(vrednost)), y=vrednost), stat = "Identity", show.legend = F, colour = "black", fill = "orange") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=8)) +
  xlab("Država") + ylab("Povprečni letni dohodek ($)") + ggtitle("Višina povprečnega letnega dohodka ($) po državah") +
  geom_hline(aes(yintercept = mean(vrednost)), size=1.5, colour = "black")

#print(graf1)

mean(tabela1$vrednost)
#-------------------------------------------------------------------------------

# 7 izbranih držav, čez leta kako kriminal narašča
drzave2 <- c("Slovenia", "Austria", "Italy", "Germany", "France", "Iceland", "Sweden", "Greece")
#tabela2 <- Varnost %>% filter(Drzava %in% drzave2) %>% filter(indeks == "Kriminal")
tabela2 <- skupna %>% filter(Drzava %in% drzave2) %>% filter(tip == "kriminal")


graf2 <- ggplot(tabela2, aes(x = Leto, y = vrednost, color = Drzava)) + geom_line()+
  xlab("Leto") + ylab("Indeks kriminala") 

#print(graf2)

#-------------------------------------------------------------------------------
#primerjam Slovenijo in Švico (dohodek izobrazba, populacija)

graf_primerjava <- function(drzava, tip_podatka, naslov){
  tabela <- skupna %>% filter(Drzava == drzava)
  graf <- ggplot(data = tabela %>% filter (tip == tip_podatka), aes(x=Leto, y=vrednost), show.legend = F) +
    geom_line(colour = "orange") + geom_point()+ theme(legend.position = "none") +
    ylab(naslov)
  graf
}


graf3 <- graf_primerjava("Slovenia", "dohodek", "Povprečni letni dohodek ($)")
graf4 <- graf_primerjava("Slovenia", "populacija", "Število prebivalcev")
graf5 <- graf_primerjava("Slovenia", "izobrazba", "Stopnja izobrazbe")
graf6 <- graf_primerjava("Switzerland", "dohodek", "Povprečni letni dohodek ($)")
graf7 <- graf_primerjava("Switzerland", "populacija", "Število prebivalcev")
graf8 <- graf_primerjava("Switzerland", "izobrazba", "Stopnja izobrazbe")


skupek1 <- plot_grid(graf3, graf4, graf5, graf6, graf7, graf8)
#print(skupek1)

#-------------------------------------------------------------------------------

# dohodek, varnost za leto 2013
tabela5 <- skupna %>% filter(Leto  == 2013 & tip %in% c("varnost", "dohodek"))
tabela5 <- tabela5 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DV_2013 <- ggplot(tabela5, aes(x=dohodek, y = varnost))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2013") + xlab("Povprečni letni dohodek ($)") + ylab("Indeks varnosti")

#dohodek, varnost za leto 2015
tabela6 <- skupna %>% filter(Leto  == 2015 & tip %in% c("varnost", "dohodek"))
tabela6 <- tabela6 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DV_2015 <- ggplot(tabela6, aes(x=dohodek, y = varnost))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2015") + xlab("Povprečni letni dohodek ($)") + ylab("Indeks varnosti")

#dohodek, varnost za leto 2017
tabela7 <- skupna %>% filter(Leto  == 2017 & tip %in% c("varnost", "dohodek"))
tabela7 <- tabela7 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DV_2017 <- ggplot(tabela7, aes(x=dohodek, y = varnost))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2017") + xlab("Povprečni letni dohodek ($)") + ylab("Indeks varnosti")

#dohodek, varnost za leto 2019
tabela8 <- skupna %>% filter(Leto  == 2019 & tip %in% c("varnost", "dohodek"))
tabela8 <- tabela8 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DV_2019 <- ggplot(tabela5, aes(x=dohodek, y = varnost))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2019") + xlab("Povprečni letni dohodek ($)") + ylab("Indeks varnosti")

#združim v en graf
skupek2 <- plot_grid(gr_DV_2013, gr_DV_2015, gr_DV_2017, gr_DV_2019)
#print(skupek2)

#-------------------------------------------------------------------------------


# populacija, kriminal za leto 2013
tabela9 <- skupna %>% filter(Leto  == 2013 & tip %in% c("kriminal", "populacija"))
tabela9 <- tabela9 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_PK_2013 <- ggplot(tabela9, aes(x=populacija, y = kriminal))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2013") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

gr_PK_2013

# populacija, kriminal za leto 2015
tabela10 <- skupna %>% filter(Leto  == 2015 & tip %in% c("kriminal", "populacija"))
tabela10 <- tabela10 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_PK_2015 <- ggplot(tabela10, aes(x=populacija, y = kriminal))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2015") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

gr_PK_2015

# populacija, kriminal za leto 2017
tabela11 <- skupna %>% filter(Leto  == 2017 & tip %in% c("kriminal", "populacija"))
tabela11 <- tabela11 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_PK_2017 <- ggplot(tabela11, aes(x=populacija, y = kriminal))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2017") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

gr_PK_2017

# populacija, kriminal za leto 2019
tabela12 <- skupna %>% filter(Leto  == 2019 & tip %in% c("kriminal", "populacija"))
tabela12 <- tabela12 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_PK_2019 <- ggplot(tabela12, aes(x=populacija, y = kriminal))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2019") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

gr_PK_2019


#združim 4 grafe v en graf
skupek3 <- plot_grid(gr_PK_2013, gr_PK_2015, gr_PK_2017, gr_PK_2019)
#print(skupek3)


#-------------------------------------------------------------------------------

# dohodek, populacija za leto 2013
tabela13 <- skupna %>% filter(Leto  == 2013 & tip %in% c("dohodek", "populacija"))
tabela13 <- tabela13 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DP_2013 <- ggplot(tabela13, aes(x=dohodek , y = populacija))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2013") + xlab("Povprečni letni dohodek ($)") + ylab("Število prebivalcev")

gr_DP_2013

# dohodek, populacija za leto 2015
tabela14 <- skupna %>% filter(Leto  == 2015 & tip %in% c("dohodek", "populacija"))
tabela14 <- tabela14 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DP_2015 <- ggplot(tabela14, aes(x=dohodek , y = populacija))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2015") + xlab("Povprečni letni dohodek ($)") + ylab("Število prebivalcev")

gr_DP_2015

# dohodek, populacija za leto 2017
tabela15 <- skupna %>% filter(Leto  == 2017 & tip %in% c("dohodek", "populacija"))
tabela15 <- tabela15 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DP_2017 <- ggplot(tabela13, aes(x=dohodek , y = populacija))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2017") + xlab("Povprečni letni dohodek ($)") + ylab("Število prebivalcev")

gr_DP_2017

# dohodek, populacija za leto 2019
tabela16 <- skupna %>% filter(Leto  == 2013 & tip %in% c("dohodek", "populacija"))
tabela16 <- tabela16 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DP_2019 <- ggplot(tabela16, aes(x=dohodek , y = populacija))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2019") + xlab("Povprečni letni dohodek ($)") + ylab("Število prebivalcev")

gr_DP_2019


#zdužim 4 grafe v enega
skupek4 <- plot_grid(gr_DP_2013, gr_DP_2015, gr_DP_2017, gr_DP_2019)
#print(skupek4)

#-------------------------------------------------------------------------------

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/110m_cultural.zip",
                             "ne_110m_admin_0_countries", encoding="UTF-8") %>% fortify

zemljevid[zemljevid$ADMIN == "Czechia",]$ADMIN <- "Czech Republic"
zemljevid$ADMIN <- as.character(zemljevid$ADMIN)

Dohodek_2019 <- skupna %>% filter(Leto == 2019 & tip == "dohodek")

zdruzitev <- left_join(zemljevid, Dohodek_2019, by=c("ADMIN"="Drzava"))

slikazemljevid <- ggplot(zdruzitev) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = vrednost )) + xlab("") + ylab("") + ggtitle("Povprečni letni dohodek na prebivalca ($) 2019") + coord_cartesian(xlim=c(-30, 30), ylim=c(30, 70))
slikazemljevid <- slikazemljevid + guides(fill=guide_legend(title="Povprečni letni dohodek ($)"))
slikazemljevid
 

