library(dplyr)
library(knitr)

# simple lap speed calculator
runs <- read.csv2("running.csv", stringsAsFactors = FALSE)

totals <- 
        group_by(runs, run, date) %>%
        summarize_each(funs(sum)) %>%
        ungroup() %>% mutate(section = "Total") %>%
        mutate(minutes = minutes + as.integer(seconds/60),
               seconds = seconds - as.integer(seconds/60) * 60)

data <- rbind(runs, totals) %>% arrange(run, section) %>%
        mutate(time  = minutes + seconds / 60,
               speed = dist / time * 60) %>%
        mutate(time  = round(time,2),
               speed = round(speed,2))

kable(data %>% filter(section == "Total"))
kable(data %>% filter(run > (max(run)-5)))
