####### Aggregated on discharge date ###########
library(tidyverse)
library(janitor)
library(finalfit)
library(zoo)
library(slider)
library(patchwork)
library(scales)

theme_set(theme_bw())

### Globals
sdec_frailty_esc <- ymd("20250801")
sdec_esc_coch <- ymd("20250301")
sdec_corridor_esc <- ymd("20250901")
mh_max <- ymd("20250601")

imds_data <- readRDS("data\\imds_data.rds")

imds_data2 <- imds_data |>
  clean_names() |>
  rename(admission_datetime = admission_date,
         discharge_datetime = discharge_date) |>
  mutate(admission_date = date(admission_datetime),
         discharge_date = date(discharge_datetime),
         eph_date = pmin(date(first_eph), discharge_date, na.rm = TRUE),
         nctr_date = pmin(date(first_nctr_date), discharge_date, na.rm = TRUE),
         eph_nctr_date = pmin(eph_date, nctr_date, discharge_date, na.rm = TRUE),
         los_hrs = admission_datetime%--%discharge_datetime / hours(1),
         mh_days = admission_date%--%discharge_date / days(1),
         los_no_eph = admission_date%--%eph_date / days(1),
         los_ctr = admission_date%--%nctr_date / days(1),
         los_nctr = nctr_date%--%discharge_date / days(1),
         los_nctr2 = if_else(is.na(los_nctr), 0, los_nctr),
         los_no_eph_nctr = admission_date%--%eph_nctr_date / days(1),
         discharge_month = floor_date(discharge_date, "month"),
         admission_month = floor_date(admission_date, "month"),
         not_discharged = if_else(is.na(discharge_datetime), 1, 0)) |>
  filter(
    admission_method == "Emergency",
    (mh_days > 1 | is.na(mh_days)),
    !patient_classification %in% c('Regular day', 'Regular night'),
    discharge_date >= ymd("20210801"),
    discharge_date < floor_date(today(), "month"),
    #mh_days > 7
    )

plot_data <- imds_data2 |>
  group_by(discharge_month) |>
  summarise(n = n(),
            incomplete = sum(not_discharged),
            mean_los = mean(mh_days, na.rm = T),
            mean_no_eph = mean(los_no_eph, na.rm = T),
            mean_ctr = mean(los_ctr, na.rm = T),
            mean_nctr = mean(los_nctr, na.rm = T),
            mean_no_eph_nctr = mean(los_no_eph_nctr, na.rm = T)
  ) |>
  ungroup() |>
  mutate(rolling_6mn_avg = zoo::rollmean(mean_los, k = 6, fill = NA, align = "right"),
         rolling_adm_avg = zoo::rollmean(n, k = 6, fill = NA, align = "right"),
         rolling_no_eph = zoo::rollmean(mean_no_eph, k = 6, fill = NA, align = "right"),
         rolling_ctr = zoo::rollmean(mean_ctr, k = 6, fill = NA, align = "right"),
         rolling_nctr = zoo::rollmean(mean_nctr, k = 6, fill = NA, align = "right"),
         rolling_no_eph_nctr = zoo::rollmean(mean_no_eph_nctr, k = 6, fill = NA, align = "right")
  )

## Recreate MH plot - trimmed + trend
los_model <- lm(mean_los ~ discharge_month, data = plot_data)
yearly_increase <- coef(los_model)["discharge_month"] * 365 #convert coef from daily to monthly
latest <- plot_data |> filter(discharge_month == max_month) |> pull(rolling_6mn_avg)

los_plot <- plot_data |>
  ggplot(aes(x = discharge_month)) +
  #geom_vline(xintercept = sdec_esc_coch, linetype = "dashed") +
  #geom_vline(xintercept = sdec_frailty_esc, linetype = "dashed") +
  #geom_vline(xintercept = sdec_corridor_esc, linetype = "dashed") +
  
  # Add annotations for each vline
  #annotate("text", x = sdec_esc_coch, y = 13, label = "SDEC Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  #annotate("text", x = sdec_frailty_esc, y = 13, label = "Frailty Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  #annotate("text", x = sdec_corridor_esc, y = 13, label = "Corridor Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  
  geom_rect(xmin = mh_max, xmax = max(plot_data$discharge_month), ymin = -Inf, ymax = Inf,
            fill = "gold", alpha = 0.01, inherit.aes = FALSE) +
  annotate("label",
           x = ymd("20250801"),
           y = 14,
           label = "New") +
  annotate("label",
           x = ymd("20230801"),
           y = 14,
           label = "On Model Hospital") +
  
  geom_line(aes(y = rolling_6mn_avg, color = "Rolling 6-month average")) +
  geom_smooth(aes(y = mean_los), method = "lm") +
  geom_line(aes(y = mean_los, color = "Actual average"),
            alpha = 0.5) +
  geom_point(aes(y = mean_los, color = "Actual average"),
             alpha = 0.5) +
  geom_point(aes(y = rolling_6mn_avg, color = "Rolling 6-month average")) +
  scale_x_date(
    date_breaks = "3 months",      
    date_labels = "%b %Y"          
  ) +
  scale_color_manual(
    name = NULL, # No title for the color legend
    values = c(
      "Rolling 6-month average" = "black",
      "Actual average" = "grey50")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(x = NULL,
       y = "Mean LOS (Days)",
       title = "Mean Emergency Inpatient LOS (Rolling 6 months)",
       subtitle = paste("Latest Mean: ", round(latest, 1),
                        "Trend :", round(yearly_increase, 1), "days per year"))

los_plot

## Plot CTR vs NCTR Los
nctr_data <- plot_data |> filter(discharge_month >= ymd("20230401") )

los_model <- lm(mean_ctr ~ discharge_month, data = nctr_data)
yearly_increase <- coef(los_model)["discharge_month"] * 365 #convert coef from daily to monthly
latest <- nctr_data |> filter(discharge_month == max_month) |> pull(rolling_ctr)

ctr_plot <- nctr_data |>
  ggplot(aes(x = discharge_month)) +
  #geom_vline(xintercept = sdec_esc_coch, linetype = "dashed") +
  #geom_vline(xintercept = sdec_frailty_esc, linetype = "dashed") +
  #geom_vline(xintercept = sdec_corridor_esc, linetype = "dashed") +
  
  # Add annotations for each vline
  #annotate("text", x = sdec_esc_coch, y = 13, label = "SDEC Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  #annotate("text", x = sdec_frailty_esc, y = 13, label = "Frailty Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  #annotate("text", x = sdec_corridor_esc, y = 13, label = "Corridor Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  
  geom_line(aes(y = rolling_ctr, color = "Rolling 6-month average")) +
  geom_smooth(aes(y = mean_ctr), method = "lm") +
  geom_line(aes(y = mean_ctr, color = "Actual average"),
            alpha = 0.5) +
  geom_point(aes(y = mean_ctr, color = "Actual average"),
             alpha = 0.5) +
  geom_point(aes(y = rolling_ctr, color = "Rolling 6-month average")) +
  scale_x_date(
    date_breaks = "3 months",      
    date_labels = "%b %Y"          
  ) +
  scale_color_manual(
    name = NULL, # No title for the color legend
    values = c(
      "Rolling 6-month average" = "black",
      "Actual average" = "grey50")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(x = NULL,
       y = "Mean LOS (Days)",
       title = "Mean CTR LOS (Rolling 6 months)")

ctr_plot

los_model <- lm(mean_nctr ~ discharge_month, data = nctr_data)
yearly_increase <- coef(los_model)["discharge_month"] * 365 #convert coef from daily to monthly
latest <- nctr_data |> filter(discharge_month == max_month) |> pull(rolling_nctr)

nctr_plot <- nctr_data |>
  ggplot(aes(x = discharge_month)) +
  #geom_vline(xintercept = sdec_esc_coch, linetype = "dashed") +
  #geom_vline(xintercept = sdec_frailty_esc, linetype = "dashed") +
  #geom_vline(xintercept = sdec_corridor_esc, linetype = "dashed") +
  
  # Add annotations for each vline
  #annotate("text", x = sdec_esc_coch, y = 13, label = "SDEC Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  #annotate("text", x = sdec_frailty_esc, y = 13, label = "Frailty Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  #annotate("text", x = sdec_corridor_esc, y = 13, label = "Corridor Esc", vjust = 0, hjust = -0.1, angle=90, color = "black") +
  
  geom_line(aes(y = rolling_nctr, color = "Rolling 6-month average")) +
  geom_smooth(aes(y = mean_nctr), method = "lm") +
  geom_line(aes(y = mean_nctr, color = "Actual average"),
            alpha = 0.5) +
  geom_point(aes(y = mean_nctr, color = "Actual average"),
             alpha = 0.5) +
  geom_point(aes(y = rolling_nctr, color = "Rolling 6-month average")) +
  scale_x_date(
    date_breaks = "3 months",      
    date_labels = "%b %Y"          
  ) +
  scale_color_manual(
    name = NULL, # No title for the color legend
    values = c(
      "Rolling 6-month average" = "black",
      "Actual average" = "grey50")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(x = NULL,
       y = "Mean LOS (Days)",
       title = "Mean NCTR LOS (Rolling 6 months)") +
  theme(
    axis.text.x = element_blank(),
    legend.position = "none"
  )

nctr_plot

(nctr_plot / ctr_plot)

## Plot activity over the same period
## Plot admission activity
activity_plot <- plot_data |>
  ggplot(aes(x = discharge_month)) +
  geom_line(aes(y = rolling_adm_avg, color = "Rolling 6-month average")) +
  geom_smooth(aes(y = n), method = "lm") +
  geom_line(aes(y = n, color = "Actual average"),
            alpha = 0.5) +
  geom_point(aes(y = n, color = "Actual average"),
             alpha = 0.5) +
  geom_point(aes(y = rolling_adm_avg, color = "Rolling 6-month average")) +
  scale_x_date(
    date_breaks = "3 months",      
    date_labels = "%b %Y"          
  ) +
  scale_color_manual(
    name = NULL, # No title for the color legend
    values = c(
      "Rolling 6-month average" = "black",
      "Actual average" = "grey50")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(x = NULL,
       y = "Numbers of Discharges",
       title = "Mean Emergency Inpatient Activity (Rolling 6 months)")

los_plot / activity_plot

## Is the reason for decline more short stays after opening esc beds
## Compare April, May, June with Aug, Sept, Oct

period_compare <- imds_data2 |>
  filter(between(discharge_month, ymd("20250401"), ymd("20250601")) |
           between(discharge_month, ymd("20250801"), ymd("20251001")) ) |>
  mutate(period = if_else(between(discharge_month, ymd("20250401"), ymd("20250601")),
                          "April-June",
                          "Aug-Oct"))

period_compare |>
  filter(mh_days < 200) |>
  ggplot(aes(x=mh_days)) +
  geom_histogram() +
  facet_wrap(~period, ncol = 1)

period_compare |>
  filter(mh_days < 200) |>
  ggplot(aes(sample=mh_days, color = period)) +
  geom_qq(alpha = 0.5)


# 1. Separate the data for each period into vectors
days1 <- period_compare |> 
  filter(period == "April-June", mh_days < 200) |> 
  pull(mh_days)

days2 <- period_compare |> 
  filter(period == "Aug-Oct", mh_days < 200) |> 
  pull(mh_days)

# 2. Create the Q-Q plot
# qqplot handles different sample sizes automatically
qqplot(days1, days2, 
       xlab = "Quantiles for April-June", 
       ylab = "Quantiles for Aug-Oct",
       main = "Q-Q Plot of mh_days: April-June vs. Aug-Oct")

# 3. Add the y=x reference line
# This line represents the case where both samples have identical distributions
abline(a = 0, b = 1, col = "red", lwd = 2)


