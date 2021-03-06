library(xml2)
library(purrr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(stringr)


cleanSourceName <- function(sourceName) {
  # Clean up source names (some seem to contain special characters)
  return(str_replace_all(sourceName, "[^A-Za-z0-9'’ ]", " "))
}


getHealthData <- function(xmlData) {
  
  results <- xml_find_all(xmlData, "//Record") %>%
    map(xml_attrs) %>%
    map_df(as.list) %>%
    select(-device, -creationDate) %>%
    filter(!str_detect(type, "HKCategoryTypeIdentifier.*")) %>%
    mutate(
      type = str_replace_all(type, "HKQuantityTypeIdentifier", ""),
      sourceName = cleanSourceName(sourceName),
      startDate = ymd_hms(startDate),
      endDate = ymd_hms(endDate),
      value = as.numeric(value)
    )
  
  return(results)
}


getWorkoutData <- function(xmlData) {
  
  results <- xml_find_all(xmlData, "//Workout") %>%
    map(xml_attrs) %>%
    map_df(as.list) %>%
    select(-device, -creationDate) %>%
    rename(activityType = workoutActivityType) %>%
    mutate(
      activityType = str_replace_all(activityType, "HKWorkoutActivityType", ""),
      sourceName = cleanSourceName(sourceName),
      duration = as.numeric(duration),
      totalDistance = as.numeric(totalDistance),
      totalEnergyBurned = as.numeric(totalEnergyBurned),
      startDate = ymd_hms(startDate),
      endDate = ymd_hms(endDate)
    )
  
  return(results)
}


#
# Main
#

filename <- "data/apple_health_export/export.xml"
xmlData <- read_xml(filename)

healthData <- getHealthData(xmlData) %>%
  mutate(startDate = with_tz(startDate, tzone = "US/Eastern"),
         endDate = with_tz(endDate, tzone = "US/Eastern"))

maxHealthDate <- max(healthData$endDate) %>% date()

dailySteps <- healthData %>%
  filter(type == "StepCount") %>%
  transmute(date = date(endDate), sourceName, steps = value) %>%
  group_by(date, sourceName) %>%
  summarize(steps = sum(steps)) %>%
  ungroup()

dailyStepsGraph <- dailySteps %>% 
  filter(date >= maxHealthDate - months(2))

ggplot(dailyStepsGraph, aes(x = date, y = steps, group = sourceName, color = sourceName)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(breaks = pretty_breaks(n = 10), date_labels = "%b %e, '%y ") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Daily steps")

ggsave(sprintf("figs/dailySteps_%s.png", maxHealthDate), width = 12, height = 8, dpi = 96)


dailyDistance <- healthData %>%
  filter(type == "DistanceWalkingRunning") %>%
  transmute(date = date(endDate), sourceName, distance = value) %>%
  group_by(date, sourceName) %>%
  summarize(distance = sum(distance)) %>%
  ungroup()

dailyDistanceGraph <- dailyDistance %>% 
  filter(date >= maxHealthDate - months(2))

ggplot(dailyDistanceGraph, aes(x = date, y = distance, group = sourceName, color = sourceName)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(breaks = pretty_breaks(n = 10), date_labels = "%b %e, '%y ") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Daily distance",
       y = "miles")

ggsave(sprintf("figs/dailyDistance_%s.png", maxHealthDate), width = 12, height = 8, dpi = 96)


yearlyDistanceSummary <- dailyDistance %>%
  mutate(year = year(date)) %>%
  group_by(year, sourceName) %>%
  summarize(distance = sum(distance)) %>%
  ungroup()

ggplot(yearlyDistanceSummary, aes(x = as.factor(year), y = distance, fill = sourceName, group = sourceName)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  geom_text(aes(label = round(distance), vjust = "inward"), position = position_dodge2(width = 0.9)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .01))) +
  labs(title = "Yearly distance",
       x = "year",
       y = "miles")

ggsave(sprintf("figs/yearlyDistance_%s.png", maxHealthDate), width = 12, height = 8, dpi = 96)


bloodPressure <- healthData %>% 
  filter(sourceName %in% c("Health", "OmronWellness", "OMRON connect"), 
         type %in% c("BloodPressureSystolic", "BloodPressureDiastolic", "HeartRate")) %>%
  select(datetime = endDate, type, value) %>%
  # It seems that for some reason, blood pressure data seems duplicated
  distinct() %>%
  tidyr::spread(key = type, value = value) %>%
  select(datetime, systolic = BloodPressureSystolic, diastolic = BloodPressureDiastolic, pulse = HeartRate)

write.csv(bloodPressure, sprintf("output/bloodPressure_%s.csv", maxHealthDate), row.names = FALSE)

colorSystolic <- rgb(0.7, 0.2, 0.1)
colorDiastolic <- rgb(0.2, 0.7, 0.1)
colorPulse <- "blue"

# Lollipop/dumbbell chart
bloodPressureGraph <- bloodPressure %>% 
  filter(datetime >= maxHealthDate - months(3))

ggplot(bloodPressureGraph, aes(x = datetime)) +
  geom_segment(aes(xend = datetime, y = diastolic, yend = systolic), color="grey") +
  geom_point(aes(y = systolic), color = colorSystolic, size = 3, alpha = 0.5) +
  geom_point(aes(y = diastolic), color = colorDiastolic, size = 3, alpha = 0.5) +
  geom_point(aes(y = pulse), color = colorPulse, size = 3, alpha = 0.5) +
  geom_smooth(aes(y = systolic), color = colorSystolic, size = 0.6, alpha = 0.2) +
  geom_smooth(aes(y = diastolic), color = colorDiastolic, size = 0.6, alpha = 0.2) +
  geom_smooth(aes(y = pulse), color = colorPulse, size = 0.6, alpha = 0.2) +
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

ggsave(sprintf("figs/bloodPressure_%s.png", maxHealthDate), width = 12, height = 8, dpi = 96)


workoutData <- getWorkoutData(xmlData) %>%
  mutate(startDate = with_tz(startDate, tzone = "US/Eastern"),
         endDate = with_tz(endDate, tzone = "US/Eastern"))

maxWorkoutDate <- max(workoutData$endDate) %>% date()

monthlyWorkoutSummary <- workoutData %>%
  mutate(month = floor_date(endDate, unit = "month")) %>%
  group_by(month, activityType) %>%
  summarize(distance = sum(totalDistance)) %>%
  ungroup()

ggplot(monthlyWorkoutSummary, aes(x = month, y = distance, fill = activityType)) +
  geom_col() +
  geom_text(aes(label = round(distance)), position = position_stack(vjust = .5), size = 3) +
  scale_x_datetime(breaks = pretty_breaks(n = 10), date_labels = "%b %e, '%y ") +
  scale_y_continuous(breaks = pretty_breaks(n = 10), expand = expand_scale(mult = c(0, .01))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Monthly workout distance",
       y = "miles")

ggsave(sprintf("figs/monthlyWorkout_%s.png", maxWorkoutDate), width = 12, height = 8, dpi = 96)
