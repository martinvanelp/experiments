#
# Simple running analyzer
#

library(dplyr)
library(knitr)

runs <- read.csv2(".\\experiments\\running.csv", stringsAsFactors = FALSE)

totals <- 
        group_by(runs, run, date) %>%
        summarize_each(funs(sum)) %>%
        ungroup() %>% mutate(section = "Total") %>%
        mutate(minutes = minutes + as.integer(seconds/60),
               seconds = seconds - as.integer(seconds/60) * 60)

data <- rbind(runs, totals) %>% arrange(run, section) %>%
        mutate(time  = (minutes + seconds / 60) %>% round(2),
               speed = (dist / time * 60) %>% round(2))

kable(data %>% filter(section == "Total"))
kable(data %>% filter(run > (max(run)-5)))