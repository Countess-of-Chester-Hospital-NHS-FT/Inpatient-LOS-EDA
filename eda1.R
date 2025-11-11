library(tidyverse)
library(janitor)
library(finalfit)
library(zoo)
library(slider)
library(patchwork)
library(scales)

theme_set(theme_bw())

### data import
db <- DBI::dbConnect(odbc::odbc(), "coch_p2")
imds_data <-  DBI::dbGetQuery(db, "
  with last_epi as (
  select
	  ENCNTR_ID
	  ,luspec.Specialty as [DischargeSpecialty]
  from CernerStaging.[BI].[InpatientMDS] imds
  left join [InformationDB].[dbo].[LuSpecialtyDivision] luspec on luspec.TreatCode = imds.SpecialtyFunctionCode
  where LastEpisodeOfSpellIndicator = 1
  )

  , nctr_first as (
  select 
	  ENCNTR_ID
	  ,interval_start_dt
  from InformationSandpitDB.dbo.NCTR_Intervals
  where asc_interval_seq = 1
  )

  , eph_first as (
  select
	  ENCNTR_ID
	  ,WardStartDateTime
	  ,row_number() over (partition by ENCNTR_ID order by WardStartDateTime) [RN]
  from CernerStaging.BI.IP_Ward_Stays
  where LocationName like '%EPH%'
  )
  
  select
	imds.ENCNTR_ID
	,ENCNTR_SLICE_ID
	,LocalPatientIdentifier
	,AdmissionDate
	,DischargeDate
	,datediff(hour, AdmissionDate, DischargeDate) / 24.0 [LOS_Days]
		,CASE WHEN AdmissionMethodCode like '1%' THEN 'Elective'
			WHEN AdmissionMethodCode like '2%' THEN 'Emergency'
			WHEN AdmissionMethodCode like '3%' THEN 'Maternity'
			WHEN AdmissionMethodCode like '8%' THEN 'Other'
			ELSE AdmissionMethodCode
		END AS [AdmissionMethod]
	,CASE WHEN PatientClassification = '1' THEN 'Ordinary'
		WHEN PatientClassification = '2' THEN 'Day case'
		WHEN PatientClassification = '3' THEN 'Regular day'
		WHEN PatientClassification = '4' THEN 'Regular night'
		WHEN PatientClassification = '5' THEN 'Delivery facilities only'
		ELSE PatientClassification
		END AS [PatientClassification]
	,CASE WHEN IntendedManagementCode = '1' THEN 'Overnight'
		WHEN IntendedManagementCode = '2' THEN 'Day case'
		WHEN IntendedManagementCode = '3' THEN 'Sequence (day and night)'
		WHEN IntendedManagementCode = '4' THEN 'Sequence (day)'
		WHEN IntendedManagementCode = '5' THEN 'Sequence (night only)'
		WHEN IntendedManagementCode = '8' THEN 'NA'
		WHEN IntendedManagementCode = '9' THEN 'Unknown'
		ELSE IntendedManagementCode
		END AS [IntendedManagementCode]
	,luspec.Specialty as [AdmissionSpecialty]
	,last_epi.DischargeSpecialty
	,PrimaryDiagnosis
	,icd10.[DESCRIPTION] as [PrimaryDiagnosisDesc]
	,PrimaryProcedure
	,opcs.OperationCategory as [PrimaryProcedureDesc]
	,nctr_first.interval_start_dt [FirstNCTRDate]
	,eph_first.WardStartDateTime [FirstEPH]
from CernerStaging.[BI].[InpatientMDS] imds
left join [InformationDB].[dbo].[LuSpecialtyDivision] luspec on luspec.TreatCode = imds.SpecialtyFunctionCode
left join LoadRef.TRUD.OPCS4_10 opcs on opcs.OperationCode = imds.PrimaryProcedure COLLATE DATABASE_DEFAULT
left join LoadRef.TRUD.ICD10 icd10 on icd10.ALT_CODE = imds.PrimaryDiagnosis COLLATE DATABASE_DEFAULT
left join last_epi on imds.ENCNTR_ID = last_epi.ENCNTR_ID
left join nctr_first on nctr_first.ENCNTR_ID = imds.ENCNTR_ID
left join eph_first on eph_first.ENCNTR_ID = imds.ENCNTR_ID and eph_first.RN = 1
where 1=1
	and EpisodeNumber = 1
order by AdmissionDate desc                        
                              ")

saveRDS(imds_data, "data\\imds_data.rds")
#imds_data <- readRDS("data\\imds_data.rds") for offline

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
         los_no_nctr = admission_date%--%nctr_date / days(1),
         los_no_eph_nctr = admission_date%--%eph_nctr_date / days(1),
         discharge_month = floor_date(discharge_date, "month"),
         admission_month = floor_date(admission_date, "month"),
         not_discharged = if_else(is.na(discharge_datetime), 1, 0)) |>
  filter(
         admission_method == "Emergency",
         (mh_days > 1 | is.na(mh_days)),
         !patient_classification %in% c('Regular day', 'Regular night'),
         admission_date >= ymd("20210801"),
         admission_date < floor_date(today(), "month"))

# test <- imds_data2 |>
#   filter(los_hrs < 24,
#          mh_days == 0)

imds_summary <- imds_data2 |>
  group_by(admission_month) |>
  summarise(n = n(),
            incomplete = sum(not_discharged),
            mean_los = mean(mh_days, na.rm = T)) |>
  ungroup() |>
  mutate(rolling_6mn_avg = zoo::rollmean(mean_los, k = 6, fill = NA, align = "right"),
         rolling_adm_avg = zoo::rollmean(n, k = 6, fill = NA, align = "right"))

# This is used to scale the secondary variable to the primary one
transformation_ratio <- max(imds_summary$rolling_6mn_avg, na.rm = T) / max(imds_summary$incomplete, na.rm = T)

## Recreate MH plot - visualise missing data
imds_summary |>
  ggplot(aes(x = admission_month)) +
  geom_col(aes(y = incomplete * transformation_ratio),
           alpha = 0.5) +
  geom_line(aes(y = rolling_6mn_avg, color = "Rolling 6-month average")) +
  geom_line(aes(y = mean_los, color = "Actual average"),
            alpha = 0.5) +
  geom_point(aes(y = mean_los, color = "Actual average"),
            alpha = 0.5) +
  geom_point(aes(y = rolling_6mn_avg, color = "Rolling 6-month average")) +
  scale_y_continuous(
    name = "Mean LOS (Days)",
    sec.axis = sec_axis(~ . / transformation_ratio, name = "Incomplete stays")
  ) +
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
       title = "Mean Emergency Inpatient LOS (Rolling 6 months)",
       subtitle = "Bars show extent of incomplete stays")

## Recreate MH plot - trimmed + trend
max_month <- imds_summary |> filter(incomplete < 2) |> pull(admission_month) |> max()

plot_data <- imds_summary |> filter(admission_month <= max_month)

los_model <- lm(mean_los ~ admission_month, data = plot_data)
yearly_increase <- coef(los_model)["admission_month"] * 365 #convert coef from daily to monthly

plot_data |>
  ggplot(aes(x = admission_month)) +
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
       subtitle = paste("Trend indicates increase of ", round(yearly_increase, 1), "days per year"))

## Plot admission activity
plot_data |>
  ggplot(aes(x = admission_month)) +
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
       y = "Mean LOS (Days)",
       title = "Mean Emergency Inpatient Activity (Rolling 6 months)")
       #subtitle = paste("Trend indicates increase of ", round(yearly_increase, 1), "days per year"))


### Specialties and length of stay
specialty_summary <- imds_data2 |>
  filter(admission_month <= max_month) |>
  group_by(discharge_specialty) |>
  summarise(n = n(),
            mean_los = mean(mh_days, na.rm = T),
            mean_no_nctr = mean(los_no_nctr, na.rm = T)) |>
  mutate(mean_ctr = mean_los - mean_no_nctr)


specialty_activity <- specialty_summary |>
  mutate(discharge_specialty = fct_lump_n(discharge_specialty, n = 18, w = n, other_level = "Other")) |>
  mutate(discharge_specialty = fct_explicit_na(discharge_specialty, na_level = "Other")) |>
  mutate(discharge_specialty = fct_reorder(discharge_specialty, n, .desc = F)) |> 
  filter(!discharge_specialty == "Other") |>
  ggplot(aes(x = discharge_specialty, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)

specialty_los <- specialty_summary |>
  mutate(discharge_specialty = fct_lump_n(discharge_specialty, n = 18, w = n, other_level = "Other")) |>
  mutate(discharge_specialty = fct_explicit_na(discharge_specialty, na_level = "Other")) |>
  mutate(discharge_specialty = fct_reorder(discharge_specialty, n, .desc = F)) |>
  filter(!discharge_specialty == "Other") |>
  ggplot(aes(x = discharge_specialty, y = mean_los)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL,
       y = "Mean LoS (Days)")

specialty_activity + specialty_los
