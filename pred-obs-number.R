
pacman::p_load(tidyverse, grid, gridExtra)

its<- read.csv("its-grdata.csv")
its2 <-read.csv("raw-its.csv")

# Raw rates
# see below for data reshaping - don't know why I made it so messy. 
rate<-ggplot(dat = its2, aes(x = year, y = rate1)) + 
        geom_line() + 
        theme_bw() + 
        geom_vline(aes(xintercept = 1986), linetype = 4, colour = "red") + 
        annotate("text", x = 1989, y = 6, label = "1986 Anti-Drug Abuse Act") + 
        labs(x = "Year", 
             y = "Rate of drug related deaths, per 100,000")

ggsave(rate, filename = 'rate.png', height=4, width=6)


its.plot<-ggplot(dat = its, aes(x = year, y = bothsexes)) + 
        geom_point() + 
        geom_smooth(dat = its, aes(x= year, y = pnums, colour = "red"), method = 'loess') + 
        geom_smooth(dat = its, aes(x = year, y = bothsexes, colour = "blue"), method = 'loess') + 
        geom_vline(aes(xintercept = 1986), linetype = 4, colour = "red") + 
        theme_bw() + 
        scale_x_continuous(breaks = pretty(its$year, n = 10), 
                           expand = c(0,0.17)) +
        theme(legend.position = "top") +
        guides(color=guide_legend(override.aes=list(fill=NA))) +
        labs(x = "Year", 
             y = "Number of drug deaths", 
             #title = "Observed versus expected number of drug deaths", 
             color = " ") +
        scale_color_manual(labels = c("Observed (smoothed)", "Predicted"), 
                           values = c("red", "darkblue")) + 
        annotate("text", x = 1988, y = 24000, label = "1986 Anti-Drug Abuse Act")


its.plot

ggsave(its.plot, filename = 'its-obspred.png', height=4, width=6)

# reshape for ease of plottin g

its2 <- its %>%
        gather(key = type, value = deaths, pnums, bothsexes)

ggplot(dat = its2, aes(x = year, y = deaths, group = type,  colour = type)) + 
        geom_smooth(method = 'loess') + 
        #geom_point(dat = subset(its, type == "pnums"), aes(x = year, y = pnums)) + 
        geom_point(aes(x = year, y = pnums), subset(its, type == "pnums"))
                   
data = subset(r2, antigen=='DR1')

# Plot observed minus expected 

diff<-ggplot(dat = its, aes(x = year, y = omenums)) + 
        geom_bar(stat = "identity") + 
        theme_bw() +
        labs(x = "year", 
             y = "Observed - predicted number of deaths") + 
             #title = "Annual difference in observed minus expected drug-related deaths") 
        geom_vline(xintercept = 1986, linetype = 4, colour = "red") + 
        annotate("text", x = 1989, y = -7000, label = "1986 Anti-Drug Abuse Act")

diff

ggsave(diff, filename = 'its-diff.png', height=4, width=6)
