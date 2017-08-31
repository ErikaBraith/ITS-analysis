its<- read.csv("drugdeaths-age-race-sex.csv")

# keeping unreliable estimates
its$crude1<- gsub("\\(Unreliable)", "", its$cruderate)

# dropping unreliable estimates

its$crude2<- gsub("\\D+\\(Unreliable)", NA, its$cruderate)

# Dropping unreliable estimaetes
ggplot(subset(its, racebw == "black"), aes(x = year, y = crude1)) + 
        geom_point(alpha = .4) + 
        #geom_smooth(method = 'loess') + 
        facet_wrap(~gar) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000", 
             title = "Crude rate of drug deaths in the United States, 1968-1998") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) 

ggsave(crude_race_sex, filename = "crude_race_sex.png", width = 12, height = 8)

deaths_race_sex<-ggplot(dat = its, aes(x = year, y = deaths)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~ga) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "Number drug deaths per 100,000", 
             title = "Number of drug deaths in the United States, 1968-1998") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) + 
        scale_y_continuous(breaks = pretty(its$deaths, n = 5))
deaths_race_sex

ggsave(deaths_race_sex, filename = "deaths_race_sex.png", width = 12, height = 8)

