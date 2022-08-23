# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/110m_cultural.zip",
                             "ne_110m_admin_0_countries", encoding="UTF-8") %>% fortify

zemljevid$ADMIN <- as.character(zemljevid$ADMIN)

Dohodek_2019 <- SkupnaTabela[, c("Country", "Dohodek.2019")]

zdruzitev <- left_join(zemljevid, Dohodek_2019, by=c("ADMIN"="Country"))

slikazemljevid <- ggplot(zdruzitev) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Dohodek.2019 )) + xlab("") + ylab("") + ggtitle("Dohodek prebivalstva 2019") + coord_cartesian(xlim=c(-30, 30), ylim=c(30, 70))
slikazemljevid <- slikazemljevid + guides(fill=guide_legend(title="Dohodek (USD)"))
slikazemljevid


