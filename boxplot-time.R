dt.rotterdamPas <-readRDS("./providedData/dt.rotterdamPas.RData")

dt.time.distribution <- dt.rotterdamPas[, c("age_category", "time")]
dt.time.distribution <- dt.time.distribution[!dt.time.distribution$age_category == "fout", ]

library(ggplot2)
library("hms")

dt.time.distribution$time <- as.hms(dt.time.distribution$time)

ggplot(dt.time.distribution, aes(x = age_category, y = time)) +
  geom_boxplot(outlier.shape = NA, aes(fill = age_category, group = age_category)) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Time at which passholders use their pass, per age category",
       fill = "Age Category", 
       y = "Time") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# ggsave("boxplot-time.png")

# save script as pdf
knitr::stitch('boxplot-time.R')