library(dplyr)
library(lubridate)
#library(devtools)
#install_github("EricGoldsmith/AppleHealthAnalysis")
library(AppleHealthAnalysis)
library(ggplot2)
library(scales)

# library(XML)
# 
# #load apple health export.xml file
# xml <- xmlParse("data/apple_health_export/export.xml")
# 
# #transform xml file to data frame - select the Record rows from the xml file
# healthData <- XML:::xmlAttrsToDataFrame(xml["//Record"])


healthData <- ah_import_xml("data/apple_health_export/export.xml") %>%
  mutate(endDate = with_tz(endDate, tzone = "US/Eastern"))

maxDate <- max(healthData$endDate) %>% date()

#ah_shiny(healthData)

dailySteps <- healthData %>%
  filter(type == "StepCount") %>%
  transmute(date = date(endDate), sourceName, steps = value) %>%
  group_by(date, sourceName) %>%
  summarize(steps = sum(steps)) %>%
  ungroup()

dailyStepsGraph <- dailySteps %>% 
  filter(date >= maxDate - months(2))
ggplot(dailyStepsGraph, aes(x = date, y = steps, group = sourceName, color = sourceName)) +
  geom_line() +
  scale_x_date(breaks = pretty_breaks(n = 10), date_labels = "%b %e, '%y ") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Daily steps")

ggsave(sprintf("figs/dailySteps_%s.png", maxDate), width = 12, height = 8, dpi = 96)


dailyDistance <- healthData %>%
  filter(type == "DistanceWalkingRunning") %>%
  transmute(date = date(endDate), sourceName, distance = value) %>%
  group_by(date, sourceName) %>%
  summarize(distance = sum(distance)) %>%
  ungroup()

dailyDistanceGraph <- dailyDistance %>% 
  filter(date >= maxDate - months(2))
ggplot(dailyDistanceGraph, aes(x = date, y = distance, group = sourceName, color = sourceName)) +
  geom_line() +
  scale_x_date(breaks = pretty_breaks(n = 10), date_labels = "%b %e, '%y ") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Daily distance",
       y = "miles")

yearlyDistanceSummary <- dailyDistance %>%
  filter(sourceName == "Erics AppleWatch") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(distance = sum(distance))

ggplot(yearlyDistanceSummary, aes(x = year, y = distance)) +
  geom_col() +
  labs(title = "Yearly distance",
       y = "miles")


bloodPressure <- healthData %>% 
  filter(sourceName %in% c("Health", "OmronWellness", "OMRON connect"), 
         type %in% c("BloodPressureSystolic", "BloodPressureDiastolic", "HeartRate")) %>%
  select(datetime = endDate, type, value) %>%
  tidyr::spread(key = type, value = value) %>%
  select(datetime, systolic = BloodPressureSystolic, diastolic = BloodPressureDiastolic, pulse = HeartRate)

write.csv(bloodPressure, sprintf("output/bloodPressure_%s.csv", maxDate), row.names = FALSE)

colorSystolic <- rgb(0.7, 0.2, 0.1)
colorDiastolic <- rgb(0.2, 0.7, 0.1)
colorPulse <- "blue"

# Lollipop/dumbbell chart
bloodPressureGraph <- bloodPressure %>% 
  filter(datetime >= maxDate - months(3))
ggplot(bloodPressureGraph, aes(x = datetime)) +
  geom_segment(aes(xend = datetime, y = diastolic, yend = systolic), color="grey") +
  geom_point(aes(y = systolic), color = colorSystolic, size = 3, alpha = 0.5) +
  geom_point(aes(y = diastolic), color = colorDiastolic, size = 3, alpha = 0.5) +
  geom_point(aes(y = pulse), color = colorPulse, size = 3, alpha = 0.5) +
  # For some reason, Loess smoothing is no longer working as expected.
  # Switch to simpler linear model until I have time to investigate.
  geom_smooth(aes(y = systolic), method = "lm", color = colorSystolic, size = 0.6, alpha = 0.2) +
  geom_smooth(aes(y = diastolic), method = "lm", color = colorDiastolic, size = 0.6, alpha = 0.2) +
  geom_smooth(aes(y = pulse), method = "lm", color = colorPulse, size = 0.6, alpha = 0.2) +
  geom_text(data = bloodPressureGraph %>% filter(!is.na(systolic)) %>% filter(datetime == min(datetime)),
            aes(x = datetime, y = systolic, label = "systolic"),
            color = colorSystolic, size = 4, vjust = -1) +
  geom_text(data = bloodPressureGraph %>% filter(!is.na(diastolic)) %>% filter(datetime == min(datetime)), 
            aes(x = datetime, y = diastolic, label = "diastolic"), 
            color = colorDiastolic, size = 4, vjust = -1) +
  geom_text(data = bloodPressureGraph %>% filter(!is.na(pulse)) %>% filter(datetime == min(datetime)),
            aes(x = datetime, y = pulse, label = "pulse"),
            color = colorPulse, size = 4, vjust = -1) +
  scale_x_datetime(breaks = pretty_breaks(n = 10), date_labels = "%b %e, '%y ") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  labs(title = "Blood pressure and pulse", x = "", y = "") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(sprintf("figs/bloodPressure_%s.png", maxDate), width = 12, height = 8, dpi = 96)
  