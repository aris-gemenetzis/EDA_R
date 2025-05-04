library(tidyverse)
# Use EconomistData dataset (source: Economist magazine)
# Indices: 
# HDI = Human Development Index (http://hdr.undp.org/en/content/human-development-index-hdi)
# CPI = Corruption Perceptions Index (https://www.transparency.org/)
dat <- read_csv("data/EconomistData.csv")
# dat <- read_csv("EconomistData.csv")
glimpse(dat)

# HDI Index values: 0 to 1, with 1 being optimal
# CPI Index values: 0 to 10, with 10 being optimal

# View Regions
unique(dat$Region)

# Region Acronyms: 
# MENA = Middle East and North Africa
# SSA = Sub-Saharan Africa
# East EU Cemt Asia = Eastern Europe and Central Asian countries with a CEMT permit
# (CEMT = Conférence Européenne des Ministres des Transports)

# View "EU W. Europe" Region countries
dat %>% filter(Region=="EU W. Europe") %>% view()

# Using sqldf library for pure SQL queries instead of
# dplyr's select, filter, arrange, mutate, summarize
library(sqldf)

(dat2 <- sqldf("
  select Region, avg(HDI) as avgHDI, avg(CPI) as avgCPI
  from dat
  group by Region")
)

# Compare Regions based on HDI, CPI indices
ggplot(dat2, aes(x = avgHDI, y = avgCPI, color=Region)) +
  geom_point()

# Same operation using dplyr
(dat3 <- dat %>% 
    group_by(Region) %>% summarise(avgHDI = mean(HDI), avgCPI = mean(CPI)))

ggplot(dat3, aes(x = avgHDI, y = avgCPI, color=Region)) +
  geom_point()

# Following plot operations based on ggplot2 cheat sheet
# (https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)

# CPI distribution plot
ggplot(data = dat) + 
geom_density(mapping = aes(x = CPI))

# Plot numebr of countries per Region
(dat4 <- sqldf("
  select Region, count(Country) as Countries
  from dat
  group by Region")
)
ggplot(data = dat4) + 
  geom_bar(mapping = aes(x = Region, y = Countries, fill=Region), stat = "identity") + guides(fill=FALSE)

# Plot % of countries per Region
(dat5 <- dat4 %>% 
  mutate(Percentage = Countries / sum(Countries) * 100))
ggplot(data = dat5) + 
  geom_bar(mapping = aes(x = Region, y = Percentage, fill=Region), stat = "identity") + guides(fill=FALSE)

# Plot average HDI per Region
ggplot(data = dat3) + 
  geom_bar(mapping = aes(x = Region, y = avgHDI, fill=Region), stat = "identity") + guides(fill=FALSE)

# Correlaction plot for HDI and CPI for all countries
ggplot(data = dat) + 
  geom_point(mapping = aes(x = HDI, y = CPI, colour = Region))

# Facet plot for HDI and CPI correlation for all countries of each Region
ggplot(data = dat) + 
  geom_point(mapping = aes(x = HDI, y = CPI, colour = Region)) + guides(colour=FALSE) + 
  facet_wrap(~ Region, nrow = 2) 
