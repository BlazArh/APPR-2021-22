# 4. faza: Analiza podatkov
library("data.table")   
library(cowplot)

# dohodek za leto 2019 za različne države

tabela1 <- skupna %>% filter(Leto == 2019 & tip == "dohodek")

graf1 <- ggplot(data = tabela1) +
  geom_bar(aes(x=reorder(Drzava, desc(vrednost)), y=vrednost), stat = "Identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=8)) +
  xlab("Country") + ylab("Dohodek") + ggtitle("Višina dohodka po drzavah") +
  geom_hline(aes(yintercept = mean(vrednost)), size=1.5)

print(graf1)
# spremeni barvo!




# 7 izbranih držav, čez leta kako kriminal narašča
drzave2 <- c("Slovenia", "Austria", "Italy", "Germany", "France", "Iceland", "Sweden", "Greece")
#tabela2 <- Varnost %>% filter(Drzava %in% drzave2) %>% filter(indeks == "Kriminal")
tabela2 <- skupna %>% filter(Drzava %in% drzave2) %>% filter(tip == "kriminal")

graf_primerjava <- function(drzava, tip_podatka, naslov){
  tabela <- skupna %>% filter(Drzava == drzava)
  graf <- ggplot(data = tabela %>% filter (tip == tip_podatka), aes(x=Leto, y=vrednost), show.legend = F) +
  geom_line() + geom_point()+ theme(legend.position = "none") +
  ylab(naslov)
  graf
}



graf3 <- graf_primerjava("Slovenia", "dohodek", "Dohodek")

graf4 <- graf_primerjava("Slovenia", "populacija", "Število prebivalcev")

graf5 <- graf_primerjava("Slovenia", "izobrazba", "Indeks izobrazbe")

graf6 <- graf_primerjava("Switzerland", "dohodek", "Dohodek")

graf7 <- graf_primerjava("Switzerland", "populacija", "Število prebivalcev")

graf8 <- graf_primerjava("Switzerland", "izobrazba", "Indeks izobrazbe")

skupek1 <- plot_grid(graf3, graf4, graf5, graf6, graf7, graf8)

print(skupek1)


# dohodek, varnost za leto 2013
tabela5 <- skupna %>% filter(Leto  == 2013 & tip %in% c("varnost", "dohodek"))
tabela5 <- tabela5 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DV_2013 <- ggplot(tabela5, aes(x=dohodek, y = varnost))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2013") + xlab("Visina place") + ylab("Indeks varnosti")

tabela6 <- skupna %>% filter(Leto  == 2015 & tip %in% c("varnost", "dohodek"))
tabela6 <- tabela6 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DV_2015 <- ggplot(tabela6, aes(x=dohodek, y = varnost))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2015") + xlab("Visina place") + ylab("Indeks varnosti")


tabela7 <- skupna %>% filter(Leto  == 2017 & tip %in% c("varnost", "dohodek"))
tabela7 <- tabela7 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DV_2017 <- ggplot(tabela7, aes(x=dohodek, y = varnost))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2017") + xlab("Visina place") + ylab("Indeks varnosti")

tabela8 <- skupna %>% filter(Leto  == 2019 & tip %in% c("varnost", "dohodek"))
tabela8 <- tabela8 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_DV_2019 <- ggplot(tabela5, aes(x=dohodek, y = varnost))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2019") + xlab("Visina place") + ylab("Indeks varnosti")

skupek2 <- plot_grid(gr_DV_2013, gr_DV_2015, gr_DV_2017, gr_DV_2019)
print(skupek2)




# populacija, kriminal za leto 2013
tabela9 <- skupna %>% filter(Leto  == 2013 & tip %in% c("kriminal", "populacija"))
tabela9 <- tabela9 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_PK_2013 <- ggplot(tabela9, aes(x=populacija, y = kriminal))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2013") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

gr_PK_2013


tabela10 <- skupna %>% filter(Leto  == 2015 & tip %in% c("kriminal", "populacija"))
tabela10 <- tabela10 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_PK_2015 <- ggplot(tabela10, aes(x=populacija, y = kriminal))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2015") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

gr_PK_2015


tabela11 <- skupna %>% filter(Leto  == 2017 & tip %in% c("kriminal", "populacija"))
tabela11 <- tabela11 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_PK_2017 <- ggplot(tabela11, aes(x=populacija, y = kriminal))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2017") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

gr_PK_2017

tabela12 <- skupna %>% filter(Leto  == 2019 & tip %in% c("kriminal", "populacija"))
tabela12 <- tabela12 %>% pivot_wider( names_from = tip, values_from = vrednost)

gr_PK_2019 <- ggplot(tabela12, aes(x=populacija, y = kriminal))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2019") + xlab("Število prebivalcev") + ylab("Indeks kriminala")

gr_PK_2019

skupek3 <- plot_grid(gr_PK_2013, gr_PK_2015, gr_PK_2017, gr_PK_2019)
print(skupek3)














