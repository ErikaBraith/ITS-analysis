
pacman::p_load(tidyverse, grid, gridExtra)

its<- read.csv("its-grdata.csv")

its.plot<-ggplot(dat = its, aes(x = year, y = bothsexes)) + 
        geom_point() + 
        geom_smooth(dat = its, aes(x= year, y = pnums, colour = "blue"), method = 'loess') + 
        geom_smooth(dat = its, aes(x = year, y = bothsexes, colour = "red"), method = 'loess') + 
        geom_vline(aes(xintercept = 1986), linetype = 4, colour = "red") + 
        theme_bw() + 
        scale_x_continuous(breaks = pretty(its$year, n = 10), 
                           expand = c(0,0.17)) +
        theme(legend.position = "top") +
        labs(x = "Year", 
             y = "Number of drug deaths", 
             title = "Observed versus expected number of drug deaths", 
             color = " ") +
        scale_color_manual(labels = c("Observed (smoothed)", "Predicted"), 
                           values = c("darkblue", "red")) + 
        annotate("text", x = 1988, y = 24000, label = "1986 Anti-Drug Abuse Act")
        
        
its.plot


# Plot observed minus expected 

ggplot(dat = its, aes(x = year, y = omenums)) + 
        geom_bar(stat = "identity") + 
        theme_bw() +
        labs(x = "year", 
             y = "Observed - predicted number of deaths", 
             title = "Annual difference in observed minus predicted drug-related deaths") + 
        geom_vline(xintercept = 1986, linetype = 4, colour = "red") + 
        annotate("text", x = 1988, y = -7000, label = "1986 Anti-Drug Abuse Act")
