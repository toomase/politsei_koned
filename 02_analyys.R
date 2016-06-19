# Politseile laekunud teadete visualiseerimine

library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(viridis)
library(splitstackshape)
library(gridExtra)
library(tidyr)
library(ggbeeswarm)
library(plotly)
library(ggthemes)
library(extrafont)
library(htmlwidgets)

# 01_lae_andmed.R scripti tulemuse laadimine
load("data/politsei_postitused.rData")

# Töötle teadete andmeid
politsei_postitused <- politsei_postitused_raw %>%
    tbl_df() %>%
    select(-from_id, -from_name, -link) %>%
    # ainult teated facebooki feed-st
    filter(type == "status") %>%
    mutate(tunnus = str_extract(message, "^[^:]+"),  # teatest tunnus
           tund = hour(aeg),
           tund = ifelse(tund == 0, 24, tund),
           aasta = year(aeg),
           #suuremate tunnuste grupeerimine
           tunnus_toodeldud = ifelse(str_detect(tunnus, "LIIKLUS"), "LIIKLUS", 
                                     tunnus),
           tunnus_toodeldud = ifelse(str_detect(tunnus_toodeldud, "JOOBES"), 
                                                "JOOBES INIMENE", tunnus_toodeldud),
           tunnus_toodeldud = ifelse(str_detect(tunnus_toodeldud, "VÄGIVALD"), 
                                     "VÄGIVALD", tunnus_toodeldud),
           tunnus_toodeldud = ifelse(tunnus_toodeldud %in% c("AVALIK KORD", "LÄRM", 
                                                             "LÕHKUMINE"), "AVALIK KORD", 
                                     tunnus_toodeldud),
           tunnus_toodeldud = ifelse(!tunnus_toodeldud %in% c("VARGUS", "INFO", "LIIKLUS",
                                                              "JOOBES INIMENE", "VÄGIVALD",
                                                              "AVALIK KORD"), "MUU", 
                                     tunnus_toodeldud),
           tunnus = tunnus_toodeldud) 

# Kõik tunnid ja tunnused tabelisse
vorm <- expand.grid(unique(politsei_postitused$tund), unique(politsei_postitused$tunnus))
names(vorm) <- c("tund", "tunnus")

# postituste arv tunnuste ja tundide lõikes
postituste_arv <- politsei_postitused %>%
    filter(aasta == 2016) %>%
    group_by(tund, tunnus) %>%
    tally() %>%
    ungroup() %>%
    right_join(vorm) %>%
    mutate(n = ifelse(is.na(n), 0, n),
           am_pm = ifelse(tund > 12, "PM", "AM"))
    
# fuktsioon AM kella graafiku tegemiseks
am <- function(x){
    postituste_arv %>%
        filter(am_pm == "AM", tunnus == x) %>%
        ggplot(aes(x = factor(tund), y = n, group = am_pm)) +
        geom_bar(stat = "identity", position = "dodge", fill = "#460E5B") +
        # ümmargune graafik
        coord_polar(theta = "x", start = 0.26) +
        xlab("") +
        ylab("") +
        # y-telja max AM ja PM graafikul samaks
        ylim(0, postituste_arv %>%
                 filter(tunnus == x) %>%
                 summarise(maksimum = max(n)) %>%
                 .$maksimum) +
        labs(subtitle = "AM - kell 00:00 - 12:00") +
        theme(axis.ticks = element_blank(), 
              axis.text.y = element_blank(), 
              panel.background = element_blank(), 
              panel.grid.major.x = element_line(colour = "grey", lineend = 5),
              axis.text.x = element_text(size = 15, hjust = 300), 
              legend.title = element_blank(),
              legend.position = "none")
}

# fuktsioon PM kella graafiku tegemiseks
pm <- function(x){
    postituste_arv %>%
        filter(am_pm == "PM", tunnus == x) %>%
        ggplot(aes(x = factor(tund), y = n, group = am_pm)) +
        geom_bar(stat = "identity", position = "dodge", fill = "#F2E51D") +
        coord_polar(theta = "x", start = 0.26) +
        xlab("") +
        ylab("") +
        ylim(0, postituste_arv %>%
                 filter(tunnus == x) %>%
                 summarise(maksimum = max(n)) %>%
                 .$maksimum) +
        scale_fill_viridis() +
        labs(subtitle = "PM - kell 12:00 - 00:00") +
        theme(axis.ticks = element_blank(), 
              axis.text.y = element_blank(), 
              panel.background = element_blank(), 
              panel.grid.major.x = element_line(colour = "grey", lineend = 5),
              axis.text.x = element_text(size = 15, hjust = 300), 
              legend.title = element_blank(),
              legend.position = "none")
}

# AM ja PM graafikud kõrvuti
grid.arrange(am(x = "JOOBES INIMENE"), pm(x = "JOOBES INIMENE"), ncol = 2, 
             top = "Vägivallaga seotud teated")

# Beesworm graafik
p <- politsei_postitused %>%    
    filter(tunnus %in% c("VÄGIVALD", "LIIKLUS", "JOOBES INIMENE", "AVALIK KORD",
                         "VARGUS"), aasta == 2016) %>%
    mutate(message = str_wrap(message, width = 50),
           message = str_replace_all(message, "\\n", "<br>")) %>%
    # text määrab tooltip väärtuse
    ggplot(aes(x = factor(tund), y = tunnus_toodeldud, group = tunnus_toodeldud, 
               text = message, color = tunnus_toodeldud)) +
    geom_beeswarm(aes(size = likes_count), cex = 5) +  # cex määrab punktide kauguse
    labs(title = "Politseile laekunud teated tundide lõikes") +
    # kasuta minimalistliku teemat ja eraldi fonti
    theme_bw(base_family = "Lucida Sans") +
    xlab("tund") +
    scale_color_brewer(palette = "Set1") + 
    theme(panel.background = element_blank(), 
          panel.grid.major.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          legend.position = "none")

# Plotly interaktiivne graafik
politsei_teated_beesworm <- ggplotly(p, tooltip = c("text"))

# salvesta graafik, et see iframe abil blogisse lisada
saveWidget(as.widget(politsei_teated_beesworm), file="politsei_teated.html", 
           selfcontained = TRUE)

# gif graafikus värvid öötunnid tumedad ja päeval heledad
fill <- data_frame(c(1:13, 12:2), c(1:24))
names(fill) <- c("fill", "tund")

# gif graafik teadete arvuga kokku tundide lõikes
animeeritud_graafik <- postituste_arv %>%
    # pealkirjas kasutatav kellaaeg
    mutate(tund_frame = str_c(ifelse(tund < 10, "0", ""),
                              tund, ".00 - ", 
                              ifelse((tund + 1) == 25, "01",
                                     ifelse((tund + 1) < 10, str_c("0", tund + 1), 
                                            tund + 1)),
                              ".00", sep = ""),
           tund_am_pm = ifelse(am_pm == "PM", tund - 12, tund)) %>%
    group_by(tund, tund_am_pm, tund_frame, am_pm) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    left_join(fill) %>%
    ggplot(aes(x = factor(tund_am_pm), y = n, group = am_pm, fill = fill, 
               frame = tund_frame)) +
    geom_bar(aes(cumulative = TRUE), stat = "identity", position = "dodge") +
    coord_polar(theta = "x", start = 0.26) +
    xlab("") +
    ylab("") +
    scale_fill_viridis() +
    labs(title = "Teated kell") +
    theme(axis.ticks = element_blank(), 
          axis.text.y = element_blank(), 
          panel.background = element_blank(), 
          panel.grid.major.x = element_line(colour = "grey", lineend = 5),
          axis.text.x = element_text(size = 15, hjust = 300), 
          legend.title = element_blank(),
          legend.position = "none")

# salvesta graafik GIF-na
gg_animate(animeeritud_graafik, filename = "output/koned.gif", interval = .5)
