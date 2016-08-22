# Código que llama la data

library(RAdwords); library(dplyr); library(tidyr); library(scales); library(stringr)
library(ggplot2); library(magrittr); library(lubridate); library(htmlTable)

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

funnel <- data.f %>% 
  spread(var, val) %>% 
  select(-c(2, 7:11)) %>% 
  select(2, 1, 6, 3, 4, 5) %>% 
  lapply(. %>% as.numeric) %>% 
  as.data.frame() 
  
nam <- names(funnel)
nam %>% 
  .[3:length(.)] %>% 
  str_c('.$`',.,'') %>% 
  str_c(.,'.x`','')

fun_nel <- merge(funnel[which(funnel$Year == 2015),],
                funnel[which(funnel$Year == 2016),], by = 'M') %>% 
  .[,c(3, 8, 4, 9, 5, 10, 6, 11)] %>% 
  rowwise %>% 
  do(data.frame(., 
                var_imp = (.$`Impressions.y` - .$`Impressions.x`)/.$`Impressions.x`)) %>% 
  mutate(var_imp = percent(var_imp))

funnel[,c(7:8)] <- lapply(funnel[,c(7:8)], dollar)
funnel[,c(1:6)] <- lapply(funnel[,c(1:6)], comma)

htmlTable(x = funnel, 
          header = paste(rep(c('2015', '2016'), 4)),
          rnames = paste(c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 
                           'Junio', 'Julio', 'Agosto')),
          cgroup = c('Impresiones', 'Clicks', 'Conversiones', 'Inversión (USD)'),
          n.cgroup = c(2,2,2,2),
          caption = 'Villaclub - Funnel de conversión (2015/2016)',
          ctable = TRUE, css.cell = 'padding-left: .5em; padding-right: .5em',
          col.rgroup = c('none', '#F7F7F7'),
          css.cgroup = 'padding-left: .7em; padding-right: .7em')

KPI <- data.f %>% 
  spread(var, val) %>% 
  .[,c(3, 1, 7:11)] %>% 
  select(1, 2, 4, 3, 5, 7, 6) %>% 
  lapply(. %>%  as.numeric) %>% 
  as.data.frame() 

KPI <- merge(KPI[which(KPI$Year == 2015),], 
             KPI[which(KPI$Year == 2016),], by = 'M') %>% 
  .[,c(3, 9, 4, 10, 5, 11, 6, 12, 7, 13)]

KPI[,c(1:6)] <- lapply(KPI[,c(1:6)], dollar)
KPI[,c(7:10)] <- lapply(KPI[,c(7:10)], percent)

htmlTable(x = KPI, 
          header = paste(rep(c('2015', '2016'), 5)),
          rnames = paste(c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 
                           'Junio', 'Julio', 'Agosto')),
          cgroup = c('CPM (USD)', 'CPC (USD)', 'CPO (USD)', 'CTR (%)', 'CTO (%)'),
          n.cgroup = c(2,2,2,2, 2),
          caption = 'Villaclub - Tabla de KPIs (2015/2016)',
          ctable = TRUE, css.cell = 'padding-left: .5em; padding-right: .5em',
          col.rgroup = c('none', '#F7F7F7'),
          css.cgroup = 'padding-left: .7em; padding-right: .7em')