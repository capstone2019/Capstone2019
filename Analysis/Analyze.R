library("dplyr")
library('PerformanceAnalytics') #find correlation chart.Correlation()
library("ggplot2")

#Read in the data()
med <- read.csv('data/claims_med_interns_2016-2018.csv')
pharm <- read.csv('data/claims_pharm_capstone_2016-2018.csv')

#med data analysis
print(colnames(med))
med_revise <- med %>% select("Claim.ID", "Member.Gender", "Age", "Zip..5.digit.", 
                             "Primary.ICD.Diagnosis.Code", "Primary.ICD.Rollup",
                             "Revenue.Code", "DRG.Code", "CCHG.Grouping", "CCHG.Label",
                             "Incurred.Year")

#Chronic disease by gender
med_gender <- med_revise %>% 
  group_by(CCHG.Label, Member.Gender) %>% 
  distinct(Claim.ID) %>% 
  summarize(n=n()) %>% 
  arrange(CCHG.Label)

#Chronic disease by age
med_age <- med_revise %>% 
  group_by(CCHG.Label, Age) %>% 
  distinct(Claim.ID) %>% 
  summarize(n=n()) %>% 
  arrange(CCHG.Label)

#Chronic disease by zipcode
med_zip <- med_revise %>% 
  group_by(CCHG.Label, Zip..5.digit.) %>% 
  distinct(Claim.ID) %>% 
  summarize(n=n()) %>% 
  arrange(CCHG.Label)

#plot of 2016 Chronic disease by gender and age
med_analysis_2016 <- med_revise %>% 
  filter(Incurred.Year == 2016) %>% 
  group_by(CCHG.Label, Age, Member.Gender) %>% 
  distinct(Claim.ID) %>% 
  summarize(n=n()) %>% 
  arrange(CCHG.Label) %>% 
  ggplot() +
  geom_point(mapping = aes(x = Age, y = n, color = Member.Gender)) +
  facet_wrap(~CCHG.Label)

#plot of 2017 Chronic disease by gender and age
med_analysis_2017 <- med_revise %>% 
  filter(Incurred.Year == 2017) %>% 
  group_by(CCHG.Label, Age, Member.Gender) %>% 
  distinct(Claim.ID) %>% 
  summarize(n=n()) %>% 
  arrange(CCHG.Label) %>% 
  ggplot() +
  geom_point(mapping = aes(x = Age, y = n, color = Member.Gender)) +
  facet_wrap(~CCHG.Label)

#plot of 2018 Chronic disease by gender and age
med_analysis_2018 <- med_revise %>% 
  filter(Incurred.Year == 2018) %>% 
  group_by(CCHG.Label, Age, Member.Gender) %>% 
  distinct(Claim.ID) %>% 
  summarize(n=n()) %>% 
  arrange(CCHG.Label) %>% 
  ggplot() +
  geom_point(mapping = aes(x = Age, y = n, color = Member.Gender)) +
  facet_wrap(~CCHG.Label)

#pharm data analysis

#pharm in 2016
pharm_2016 <- pharm %>% 
  filter(Incurred.Year == 2016) %>% 
  group_by(Drug.Name) %>% 
  distinct(Claim.ID) %>% 
  summarize(n_2016=n()) %>% 
  arrange(-n_2016)
pharm_2016_top_ten <- head(pharm_2016, 10)

#pharm in 2017
pharm_2017 <- pharm %>% 
  filter(Incurred.Year == 2017) %>% 
  group_by(Drug.Name) %>% 
  distinct(Claim.ID) %>% 
  summarize(n_2017=n()) %>% 
  arrange(-n_2017) 
pharm_2017_top_ten <- head(pharm_2017, 10)

#top pharm in 2018
pharm_2018 <- pharm %>% 
  filter(Incurred.Year == 2018) %>% 
  group_by(Drug.Name) %>% 
  distinct(Claim.ID) %>% 
  summarize(n_2018=n()) %>% 
  arrange(-n_2018) 
pharm_2018_top_ten <- head(pharm_2018, 10)

pharm_combo <- full_join(pharm_2016, pharm_2017)
pharm_combo <- full_join(pharm_combo, pharm_2018) %>% 
  arrange(-n_2018, -n_2017, -n_2016) %>% 
  head(5)

flip <- data.frame(t(pharm_combo))
name <- flip[1,]
flip <- flip[2:4,]
colnames(flip) <- c('LEVOTHYROXINE SODIUM', 'LISINOPRIL', 'ATORVASTATIN CALCIUM',
                    'OMEPRAZOLE', 'HYDROCODONE/ACETAMINOPHEN')
flip['year'] <- c('2016', '2017', '2018')

ggplot(flip, aes(x = year), group = 1) + 
  geom_line(aes(y = flip$`LEVOTHYROXINE SODIUM`, group = 1), colour="blue", linetype = "dashed") + 
  geom_line(aes(y = flip$LISINOPRIL, group = 1), colour = "grey", linetype = "dashed") + 
  geom_line(aes(y = flip$`ATORVASTATIN CALCIUM`, group = 1), colour = "black", linetype = "dashed") + 
  geom_line(aes(y = flip$OMEPRAZOLE, group = 1), colour = "red", linetype = "dashed") + 
  geom_line(aes(y = flip$`HYDROCODONE/ACETAMINOPHEN`, group = 1), colour = "green", linetype = "dashed") + 
  ylab("Number of people") + 
  xlab("Year") + 
  ggtitle("Change of Top Ten Drug from 2016 -2018")
