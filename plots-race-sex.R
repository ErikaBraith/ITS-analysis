

# Making the figures for age/race stratified estimates 
# These aren't age standardized... 
pacman::p_load(tidyverse, grid, gridExtra)

its<- read.csv("drugdeaths-race-sex.csv")

crude_race_sex<-ggplot(dat = its, aes(x = year, y = cruderate)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~ga) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000", 
             title = "Crude rate of drug deaths in the United States, 1968-1998") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) 

ggsave(crude_race_sex, filename = "crude_race_sex.png", width = 12, height = 8)

adj_race_sex<-ggplot(dat = its, aes(x = year, y = ageadjustedrate)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~ga) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "Age adjusted rate of drug deaths per 100,000", 
             title = "Age adjusted rate of drug deaths in the United States, 1968-1998") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) + 
        scale_y_continuous(breaks = pretty(its$ageadjustedrate, n = 5))
adj_race_sex

ggsave(adj_race_sex, filename = "adj_race_sex.png", width = 12, height = 8)

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






