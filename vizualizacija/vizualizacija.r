# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/110m_cultural.zip",
                             "ne_110m_admin_0_countries", encoding="UTF-8") %>% fortify

zemljevid$ADMIN <- as.character(zemljevid$ADMIN)

Dohodek_2019 <- skupna %>% filter(Leto == 2019 & tip == "dohodek")

zdruzitev <- left_join(zemljevid, Dohodek_2019, by=c("ADMIN"="Drzava"))

slikazemljevid <- ggplot(zdruzitev) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = vrednost )) + xlab("") + ylab("") + ggtitle("Dohodek prebivalstva 2019") + coord_cartesian(xlim=c(-30, 30), ylim=c(30, 70))
slikazemljevid <- slikazemljevid + guides(fill=guide_legend(title="Dohodek (USD)"))
slikazemljevid
 

