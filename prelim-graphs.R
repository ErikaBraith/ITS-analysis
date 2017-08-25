
pacman::p_load(tidyverse, grid, gridExtra)

its<- read.csv("drugdeaths.csv")


# Raw rates

# excluding unreliable estimates
ggplot(dat = its, aes(x = year, y = crude1)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~gender) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) 

# including unreliable estimates
ggplot(dat = its, aes(x = year, y = crude)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~gender) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) 


# excluding unreliable estimates
ggplot(dat = its, aes(x = year, y = crude1)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~racebw) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) 
# including unreliable estimates
ggplot(dat = its, aes(x = year, y = crude)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~racebw) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) 

# excluding unreliable estimates
ggplot(dat = its, aes(x = year, y = crude1)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~agegr) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000") + 
        scale_x_continuous(breaks = pretty(its$year, n = 10), 
                           expand = c(0,0.6)) 



its3<-its2 %>%
        gather(key = typerate, value = rate , cruderate, ageadjustedrate)

# reshape 


its4<-its %>%
        gather(key = quality, value = rate, crude, crude1)

#Crude (unreliable versus reliable)
agecat.b<-ggplot(subset(its4, racebw == "black"), aes(x = year, y = rate, group = quality, fill = quality)) + 
        #geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~agegr) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000", 
             title = "Crude rates by age, among blacks") + 
        scale_x_continuous(breaks = pretty(its$year, n = 10), 
                           expand = c(0,0.6)) + 
        scale_fill_discrete(name="Reliability",
                            breaks=c("crude", "crude1"),
                            labels=c("Includes unreliable", "Excludes unreliable"))


agecat.b
ggsave(agecat.b, filename = 'agecat-b.png', height=8, width=12)

agecat.w<-ggplot(subset(its4, racebw == "white"), aes(x = year, y = rate, group = quality, fill = quality)) + 
        #geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~agegr) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000", 
             title = "Crude rates by age, among whites") + 
        scale_x_continuous(breaks = pretty(its$year, n = 10), 
                           expand = c(0,0.6)) + 
        scale_fill_discrete(name="Reliability",
                            breaks=c("crude", "crude1"),
                            labels=c("Includes unreliable", "Excludes unreliable"))


agecat.w
ggsave(agecat.w, filename = 'agecat-w.png', height=8, width=12)



agecat.b<-ggplot(subset(its, racebw == "black"), aes(x = year, y = crude1)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_wrap(~agegr) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000", 
             title = "Crude rates by age, amoung blacks") + 
        scale_x_continuous(breaks = pretty(its$year, n = 10), 
                           expand = c(0,0.6)) 

agecat.b
ggsave(agecat, filename = 'agecat-b.png', height=4, width=6)



# Just plot race & gender

ggplot(dat = its, aes(x = year, y = crude1)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_grid(gender~racebw) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) 

ggplot(dat = its, aes(x = year, y = crude)) + 
        geom_point(alpha = .4) + 
        geom_smooth(method = 'loess') + 
        facet_grid(gender~racebw) + 
        geom_vline(xintercept = 1986) + 
        labs(y = "crude rate of drug deaths per 100,000") + 
        scale_x_continuous(breaks = pretty(its$year, n = 12), 
                           expand = c(0,0.6)) 


## Look at the age adjusted rates for the race-sex categories 

its2<-read.csv("drugdeaths-race-sex.csv")



# plotting age adjusted and crude together 

its3<-its2 %>%
        gather(key = typerate, value = rate , cruderate, ageadjustedrate)

genderrace<-ggplot(dat = its3, aes(x = year, y = rate, group = typerate, colour = typerate )) +
        geom_point() + 
        facet_grid(gender~racebw) + 
        geom_smooth(method = 'loess') + 
        geom_vline(xintercept = 1986) + 
        scale_x_continuous(breaks = pretty(its2$year, n = 12), 
                           expand= c(0, 0.6)) + 
        labs(y = "rate per 100,000", 
             title = "Crude versus age adjusted rate of drug deaths, by race and gender") + 
        theme_bw()

ggsave(genderrace, filename = "genderrace.png", height = 8, width = 12)




