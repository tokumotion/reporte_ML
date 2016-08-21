# Código que llama la data

library(RAdwords); library(dplyr); library(tidyr); library(scales); library(ggthemes)
library(ggplot2); library(magrittr); library(lubridate); library(stringr); 
library(htmlTable)

# load Google credentials
load('~/Documents/ML_Adwords')
VillaClub <- "828-183-0329"

metricas <- metrics("CAMPAIGN_PERFORMANCE_REPORT")
report.16 <- statement(metricas[c(99, 131, 89, 46, 54, 60)], 
                       report = "CAMPAIGN_PERFORMANCE_REPORT",
                       start = "20160101", end = "20160818")
data <- getData(clientCustomerId = VillaClub, google_auth = Adwords,
                statement = report.16)

report.15 <- statement(metricas[c(99, 131, 89, 46, 54, 60)], 
                       report = "CAMPAIGN_PERFORMANCE_REPORT",
                       start = "20150101", end = "20150818")
data15 <- getData(clientCustomerId = VillaClub, google_auth = Adwords,
                  statement = report.15)

data.f <- rbind(data, data15)

data.f %<>% 
  group_by(Year, Month) %>% 
  summarise_all(sum) %>% 
  mutate(CTR = Clicks/Impressions, CTO = Conversions/Clicks, CPC = Cost/Clicks,
         CPO = Cost/Conversions, CPM = Cost/Impressions*1000) %>% 
  gather(var, val, 3:11)

data.f$M <- month(data.f$Month)

data$Month <- ymd(data$Month)
data15$Month <- ymd(data15$Month)

ggplot(data = data[which(data$var == 'CTR'),], aes(x = Month, y = val)) + 
  geom_line(stat = 'identity') + ylab('CTR (%)') + xlab('Mes') +
  ggtitle('CTR - 01/01/15 al 17/08/15 vs \n01/01/16 al 17/08/16') +
  theme(plot.title = element_text(vjust = 3, size = 15)) +
  scale_y_continuous(labels = percent) + 
  scale_x_date(date_labels = "%B", date_breaks = '1 month', 
               date_minor_breaks = '1 month') + 
  theme_minimal()

# Hacer las tablas

a <- data.f %>% 
  spread(var, val) %>% 
  select(-Month)
a <- merge(a[which(a$Year == 2015),], a[which(a$Year == 2016),], by = 'M')
a <- a[,c(11,21,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10,20)]

a[,c(15:18)] <- lapply(a[,c(15:18)], percent)
a[,c(7:14)] <- lapply(a[,c(7:14)], dollar)
a[,c(1:6)] <- lapply(a[,c(1:6)], comma)

htmlTable(x = a, 
          header = paste(rep(c('2015', '2016'), 9)),
          rnames = paste(c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 
                           'Junio', 'Julio', 'Agosto')),
          cgroup = c('Impresiones', 'Clicks', 'Conversiones', 'Inversión (USD)',
                     'CPC (USD)', 'CPM (USD)', 'CPO (USD)', 'CTO (%)', 'CTR (%)'),
          n.cgroup = c(2,2,2,2,2,2,2,2,2),
          caption = 'Villaclub - Tabla comparativa de KPIs (2015/2016)')