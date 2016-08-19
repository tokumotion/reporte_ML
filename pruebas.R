# Código que llama la data

library(RAdwords); library(dplyr); library(tidyr); library(scales); library(ggthemes)
library(ggplot2); library(magrittr); library(lubridate); library(stringr)

# load Google credentials
load('~/Documents/ML_Adwords')
VillaClub <- "828-183-0329"

metricas <- metrics("CAMPAIGN_PERFORMANCE_REPORT")
report <- statement(metricas[c(99, 89, 46, 54, 60)], 
                    report = "CAMPAIGN_PERFORMANCE_REPORT",
                    start = "20160101", end = "20160818")
data <- getData(clientCustomerId = VillaClub, google_auth = Adwords,
                statement = report)

data %<>% 
  group_by(Month) %>% 
  summarise_all(sum) %>% 
  mutate(CTR = Clicks/Impressions, CTO = Conversions/Clicks, CPC = Clicks/Cost,
         CPO = Conversions/Cost, CPM = Cost/Impressions*1000) %>% 
  gather(var, val, 2:10)

data$Month <- ymd(data$Month)

ggplot(data = data[which(data$var == 'CTR'),], aes(x = Month, y = val)) + 
  geom_line(stat = 'identity') + ylab('CTR (%)') + xlab('Mes') +
  ggtitle('CTR - 01/01/15 al 17/08/15 vs \n01/01/16 al 17/08/16') +
  theme(plot.title = element_text(vjust = 3, size = 15)) +
  scale_y_continuous(labels = percent) + 
  scale_x_date(date_labels = "%B", date_breaks = '1 month', 
               date_minor_breaks = '1 month') + 
  theme_minimal()
  