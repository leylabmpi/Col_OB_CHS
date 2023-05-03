# Framingham Risk Score Calculator.
# It calculates the Framingham Risk Score, a sex-specific algorithm used 
# to estimate the 10-year cardiovascular risk of an individual.
# 
# Adapted from Andrey Kuznetsov's Shiny app. See:
# https://github.com/ankuznetsov/Framingham_risk_score
# https://ankuznetsov.shinyapps.io/Framingham_risk_score/
#-------------------------------------------------------------------------------------------------


# Functions ----
# Libraries
require(data.table)

# Men ----
points_men <- function(age, totchol, smoker, HDLChol, treated, SBP)
{
  # In case data are missing
  features = list(age, totchol, smoker, HDLChol, treated, SBP)
  if(any(is.na(features))){
    return(NA)
  } else {
    
    points = 0
    
    # Age points
    if(between(age, 18, 34)) points = points - 9
    else if(between(age, 35, 39)) points = points - 4
    else if(between(age, 45, 49)) points = points + 3
    else if(between(age, 50, 54)) points = points + 6
    else if(between(age, 55, 59)) points = points + 8
    else if(between(age, 60, 64)) points = points + 10
    else if(between(age, 65, 69)) points = points + 11
    else if(between(age, 70, 74)) points = points + 12
    else if(between(age, 75, 79)) points = points + 13
    
    # Total cholesterol points
    if(between(age, 18, 34) | between(age, 35, 39))
    {
      # Age 20–39 years
      if(between(totchol, 160, 199)) points = points + 4
      else if(between(totchol, 200, 239)) points = points + 7
      else if(between(totchol, 240, 279)) points = points + 9
      else if(totchol > 280) points = points + 11
    } else if(between(age, 40, 44) | between(age, 45, 49))
    {
      # Age 40–49 years
      if(between(totchol, 160, 199)) points = points + 3
      else if(between(totchol, 200, 239)) points = points + 5
      else if(between(totchol, 240, 279)) points = points + 6
      else if(totchol > 280) points = points + 8
    } else if(between(age, 50, 54) | between(age, 55, 59))
    {
      # Age 50–59 years
      if(between(totchol, 160, 199)) points = points + 2
      else if(between(totchol, 200, 239)) points = points + 3
      else if(between(totchol, 240, 279)) points = points + 4
      else if(totchol > 280) points = points + 5
    } else if(between(age, 60, 64) | between(age, 65, 69))
    {
      # Age 60–69 years
      if(between(totchol, 160, 199)) points = points + 1
      else if(between(totchol, 200, 239)) points = points + 1
      else if(between(totchol, 240, 279)) points = points + 2
      else if(totchol > 280) points = points + 3
    } else if(between(age, 70, 74) | between(age, 75, 79))
    {
      # Age 70–79 years
      if(between(totchol, 240, 279)) points = points + 1
      else if(totchol > 280) points = points + 1
    } 
    
    # Cigarette smoker points
    if(smoker == TRUE)
    {
      if(between(age, 18, 34) | between(age, 35, 39)) points = points + 8
      else if(between(age, 40, 44) | between(age, 45, 49)) points = points + 5
      else if(between(age, 50, 54) | between(age, 55, 59)) points = points + 3
      else if(between(age, 60, 64) | between(age, 65, 69)) points = points + 1
      else if(between(age, 70, 74) | between(age, 75, 79)) points = points + 1
    }
    
    # HDL cholesterol points
    if(HDLChol >= 60) points = points - 1
    else if(between(HDLChol, 40, 49)) points = points + 1
    else if(HDLChol < 40) points = points + 2
    
    # Systolic blood pressure points
    if(treated == TRUE)
    {
      if(between(SBP, 120, 129)) points = points + 1
      else if(between(SBP, 130, 139)) points = points + 2
      else if(between(SBP, 140, 159)) points = points + 2
      else if(SBP > 160) points = points + 3
    } else if(treated == FALSE)
    {
      if(between(SBP, 130, 139)) points = points + 1
      else if(between(SBP, 140, 159)) points = points + 1
      else if(SBP > 160) points = points + 2
    }
  return(points)  
  }

}

# Women ----
points_women <- function(age, totchol, smoker, HDLChol, treated, SBP)
{
  # In case data are missing
  features = list(age, totchol, smoker, HDLChol, treated, SBP)
  if(any(is.na(features))){
    return(NA)
  } else {
    points = 0
    
    # Age points
    if(between(age, 18, 34)) points = points - 7
    else if(between(age, 35, 39)) points = points - 3
    else if(between(age, 45, 49)) points = points + 3
    else if(between(age, 50, 54)) points = points + 6
    else if(between(age, 55, 59)) points = points + 8
    else if(between(age, 60, 64)) points = points + 10
    else if(between(age, 65, 69)) points = points + 12
    else if(between(age, 70, 74)) points = points + 14
    else if(between(age, 75, 79)) points = points + 16
    
    # Total cholesterol points
    if(between(age, 18, 34) | between(age, 35, 39))
    {
      # Age 20–39 years
      if(between(totchol, 160, 199)) points = points + 4
      else if(between(totchol, 200, 239)) points = points + 8
      else if(between(totchol, 240, 279)) points = points + 11
      else if(totchol > 280) points = points + 13
    } else if(between(age, 40, 44) | between(age, 45, 49))
    {
      # Age 40–49 years
      if(between(totchol, 160, 199)) points = points + 3
      else if(between(totchol, 200, 239)) points = points + 6
      else if(between(totchol, 240, 279)) points = points + 8
      else if(totchol > 280) points = points + 10
    } else if(between(age, 50, 54) | between(age, 55, 59))
    {
      # Age 50–59 years
      if(between(totchol, 160, 199)) points = points + 2
      else if(between(totchol, 200, 239)) points = points + 4
      else if(between(totchol, 240, 279)) points = points + 5
      else if(totchol > 280) points = points + 7
    } else if(between(age, 60, 64) | between(age, 65, 69))
    {
      # Age 60–69 years
      if(between(totchol, 160, 199)) points = points + 1
      else if(between(totchol, 200, 239)) points = points + 2
      else if(between(totchol, 240, 279)) points = points + 3
      else if(totchol > 280) points = points + 4
    } else if(between(age, 70, 74) | between(age, 75, 79))
    {
      # Age 70–79 years
      if(between(totchol, 160, 199)) points = points + 1
      else if(between(totchol, 200, 239)) points = points + 1
      else if(between(totchol, 240, 279)) points = points + 2
      else if(totchol > 280) points = points + 2
    } 
    
    # Cigarette smoker points
    if(smoker == TRUE)
    {
      if(between(age, 18, 34) | between(age, 35, 39)) points = points + 9
      else if(between(age, 40, 44) | between(age, 45, 49)) points = points + 7
      else if(between(age, 50, 54) | between(age, 55, 59)) points = points + 4
      else if(between(age, 60, 64) | between(age, 65, 69)) points = points + 2
      else if(between(age, 70, 74) | between(age, 75, 79)) points = points + 1
    }
    
    # HDL cholesterol points
    if(HDLChol >= 60) points = points - 1
    else if(between(HDLChol, 40, 49)) points = points + 1
    else if(HDLChol < 40) points = points + 2
    
    # Systolic blood pressure points
    if(treated == TRUE)
    {
      if(between(SBP, 120, 129)) points = points + 3
      else if(between(SBP, 130, 139)) points = points + 4
      else if(between(SBP, 140, 159)) points = points + 5
      else if(SBP >= 160) points = points + 6
    } else if(treated == FALSE)
    {
      if(between(SBP, 120, 129)) points = points + 1
      else if(between(SBP, 130, 139)) points = points + 2
      else if(between(SBP, 140, 159)) points = points + 3
      else if(SBP >= 160) points = points + 4
    }
    
    return(points)
  }
}


# Framingham risk
F_risk_women = function(risk){
  # In case data are missing
  if(is.na(risk)){
    return(NA)
  } else {
    F_risk = 0
    if(risk < 9) F_risk = 1
    else if(between(risk,9, 12)) F_risk = 1
    else if(between(risk, 13, 14)) F_risk = 2
    else if(risk == 15) F_risk = 3
    else if(risk == 16) F_risk = 4
    else if(risk == 17) F_risk = 5
    else if(risk == 18) F_risk = 6
    else if(risk == 19) F_risk = 8
    else if(risk == 20) F_risk = 11
    else if(risk == 21) F_risk = 14
    else if(risk == 22) F_risk = 17
    else if(risk == 23) F_risk = 22
    else if(risk == 24) F_risk = 27
    else if(risk >= 25) F_risk = 30
    return(F_risk)
  }
}

F_risk_men = function(risk){
  # In case data are missing
  if(is.na(risk)){
    return(NA)
  } else {
    F_risk = 0
    if(risk == 0) F_risk = 1
    else if(between(risk, 1, 4)) F_risk = 1
    else if(between(risk, 5, 6)) F_risk = 2
    else if(risk == 7) F_risk = 3
    else if(risk == 8) F_risk = 4
    else if(risk == 9) F_risk = 5
    else if(risk == 10) F_risk = 6
    else if(risk == 11) F_risk = 8
    else if(risk == 12) F_risk = 10
    else if(risk == 13) F_risk = 12
    else if(risk == 14) F_risk = 16
    else if(risk == 15) F_risk = 20
    else if(risk == 16) F_risk = 25
    else if(risk >= 17) F_risk = 30
    return(F_risk)
  }
}
