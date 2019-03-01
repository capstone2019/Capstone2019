# Get data of chronic diseases of interest into separate dataframes for further analysis

diabetes <- med_revise %>% 
  filter(grepl('Diabetes', Primary.ICD.Rollup)) %>%
  mutate(label = 'Diabetes')

hypertension <- med_revise %>% 
  filter(grepl('I10', Primary.ICD.Diagnosis.Code)) %>%
  mutate(label = 'Hypertension')

obesity <- med_revise %>% 
  filter(grepl('E66', Primary.ICD.Diagnosis.Code)) %>%
  mutate(label = 'Obesity')

cancer <- med_revise %>% 
  filter(grepl('Cancer', Primary.ICD.Rollup, ignore.case = TRUE)) %>%
  mutate(label = 'Cancer')

musculoskeletal <- med_revise %>% 
  filter(grepl('musculoskeletal', Primary.ICD.Rollup)) %>%
  mutate(label = 'Musculoskeletal')

cardiovascular <- med_revise %>% 
  filter(grepl('heart', Primary.ICD.Rollup)) %>%
  mutate(label = 'Cardiovascular')

asthma <- med_revise %>% 
  filter(grepl('Asthma', Primary.ICD.Rollup)) %>%
  mutate(label = 'Asthma')

copd <- med_revise %>% 
  filter(grepl('Chronic obstructive pulmonary disease', Primary.ICD.Rollup)) %>%
  mutate(label = 'COPD')

cholesterol <- med_revise %>% 
  filter(grepl('E78', Primary.ICD.Diagnosis.Code)) %>%
  mutate(label = 'Cholesterol')

# Primary.ICD.Rollup unknown
sleep_disorders <- med_revise %>% 
  filter(grepl('G47', Primary.ICD.Diagnosis.Code)) %>%
  mutate(label = 'Sleep Disorders')



# All joined together
all <- rbind(diabetes, hypertension, obesity, cancer, musculoskeletal, cardiovascular,
             asthma, copd, cholesterol, sleep_disorders)

