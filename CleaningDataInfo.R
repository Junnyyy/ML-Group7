#Read in the dataset by replacing '[file path]' with the path that leads to the healthcare-dataset-stroke-data.csv local file:
#library(readr)
#stroke <- read_csv("[file path]/healthcare-dataset-stroke-data.csv", col_types = cols(gender = col_factor(levels = c("Male", "Female", "Other")), hypertension = col_factor(levels = c("0", "1")), heart_disease = col_factor(levels = c("0", "1")), ever_married = col_factor(levels = c("No", "Yes")), work_type = col_factor(levels = c("children", "Govt_job", "Never_worked", "Private", "Self-employed")), Residence_type = col_factor(levels = c("Rural", "Urban")), bmi = col_number(), smoking_status = col_factor(levels = c("formerly smoked", "never smoked", "smokes")), stroke = col_factor(levels = c("0", "1"))))

#Notable fixes:
#The column labeled smoking_status has been reformatted to reflect that it is a factor variable with 3 levels: "formerly smoked", "never smoked", "smokes". The other option, "Unknown", was previously used to encapsulate NA values.
#The bmi column was also fixed by transforming it from character to numeric. This changes the non-numeric "N/A" cells to reflect NA values in the now numeric column.


#Next, omit the NA values:
#stroke = na.omit(stroke)

#Now any rows that contain NA values, which are only located in the bmi or smoking_status columns, are easily removed from the stroke dataset.
