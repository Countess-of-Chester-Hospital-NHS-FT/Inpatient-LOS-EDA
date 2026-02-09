## Calc news score function
calculate_news_score <- function(data) {
  data %>%
    mutate(
      # 1. Convert character columns to numeric
      resp_rate_num = as.numeric(respiratory_rate),
      spo2_num = as.numeric(sp_o2),
      sbp_num = as.numeric(systolic_blood_pressure),
      pulse_num = as.numeric(heart_rate_monitored),
      temp_num = as.numeric(temperature),
      
      # 2. Calculate individual component scores
      
      # Respiratory Rate
      score_rr = case_when(
        resp_rate_num <= 8 ~ 3,
        resp_rate_num >= 9 & resp_rate_num <= 11 ~ 1,
        resp_rate_num >= 12 & resp_rate_num <= 20 ~ 0,
        resp_rate_num >= 21 & resp_rate_num <= 24 ~ 2,
        resp_rate_num >= 25 ~ 3,
        TRUE ~ NA_real_
      ),
      
      # SpO2 (Using Standard Scale 1)
      score_spo2 = case_when(
        spo2_num <= 91 ~ 3,
        spo2_num >= 92 & spo2_num <= 93 ~ 2,
        spo2_num >= 94 & spo2_num <= 95 ~ 1,
        spo2_num >= 96 ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Oxygen Therapy
      # Assign 0 if "Room air", 2 if anything else (indicating supplemental O2)
      score_o2 = if_else(
        str_detect(oxygen_therapy, regex("Room air", ignore_case = TRUE)), 
        0, 
        2
      ),
      
      # Systolic Blood Pressure
      score_sbp = case_when(
        sbp_num <= 90 ~ 3,
        sbp_num >= 91 & sbp_num <= 100 ~ 2,
        sbp_num >= 101 & sbp_num <= 110 ~ 1,
        sbp_num >= 111 & sbp_num <= 219 ~ 0,
        sbp_num >= 220 ~ 3,
        TRUE ~ NA_real_
      ),
      
      # Heart Rate (Pulse)
      score_pulse = case_when(
        pulse_num <= 40 ~ 3,
        pulse_num >= 41 & pulse_num <= 50 ~ 1,
        pulse_num >= 51 & pulse_num <= 90 ~ 0,
        pulse_num >= 91 & pulse_num <= 110 ~ 1,
        pulse_num >= 111 & pulse_num <= 130 ~ 2,
        pulse_num >= 131 ~ 3,
        TRUE ~ NA_real_
      ),
      
      # Level of Consciousness (ACVPU)
      # 0 if Alert, 3 for anything else (Confusion, Voice, Pain, Unresponsive)
      score_acvpu = if_else(
        str_detect(acvpu_conscious_level, regex("Alert", ignore_case = TRUE)), 
        0, 
        3
      ),
      
      # Temperature
      score_temp = case_when(
        temp_num <= 35.0 ~ 3,
        temp_num >= 35.1 & temp_num <= 36.0 ~ 1,
        temp_num >= 36.1 & temp_num <= 38.0 ~ 0,
        temp_num >= 38.1 & temp_num <= 39.0 ~ 1,
        temp_num >= 39.1 ~ 2,
        TRUE ~ NA_real_
      ),
      
      # 3. Sum components for Total NEWS Score
      news_score = score_rr + score_spo2 + score_o2 + score_sbp + score_pulse + score_acvpu + score_temp
    )
}

interpret_news_score <- function(data) {
  data %>%
    mutate(
      # Check if ANY individual parameter scored a 3 (Red Score)
      # We check all the score_ columns created in the previous step
      has_red_score = if_else(
        score_rr == 3 | score_spo2 == 3 | score_sbp == 3 | 
          score_pulse == 3 | score_acvpu == 3 | score_temp == 3,
        TRUE, 
        FALSE
      ),
      
      # Determine Clinical Risk
      clinical_risk = case_when(
        # High Risk: Aggregate 7+
        news_score >= 7 ~ "High",
        
        # Medium Risk: Aggregate 5-6
        news_score >= 5 & news_score <= 6 ~ "Medium",
        
        # Low-Medium Risk: Aggregate 0-4 BUT has a Red Score (3) in one parameter
        news_score >= 0 & news_score <= 4 & has_red_score ~ "Low-medium",
        
        # Low Risk: Aggregate 0-4 AND no Red Scores
        news_score >= 0 & news_score <= 4 & !has_red_score ~ "Low",
        
        TRUE ~ "Unknown" # Handle NAs if data is missing
      )
    )
}