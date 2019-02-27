
diabetes <- med_revise %>% 
  filter(grepl('Diabetes', Primary.ICD.Rollup)) %>% 
  group_by(Member.ID.Encrypted, Primary.ICD.Rollup) %>% 
  summarise(count = n())