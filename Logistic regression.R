library(dplyr)
library(ggplot2)

appli_clean <- read.csv("Applicants_clean.csv", header = TRUE)

# data cleaning
appli_clean  <- appli_clean [-c(6,7)]

# convert to dummy/binary
appli_clean  <- mutate(appli_clean, Resident=ifelse(Residency.Status=="I am a U.S. Citizen or U.S. Permanent Resident", 1, 0))
appli_clean <- mutate(appli_clean, International=ifelse(Residency.Status=="I am an International applicant", 1, 0))

appli_clean <- mutate(appli_clean, Program=ifelse(Programs=="Full-Time MBA"|Programs=="Executive MBA"|
                                                    Programs=="Fully Employed MBA"|Programs=="JD/MBA"|Programs=="MD/MBA", 1, 0))

appli_clean <- mutate(appli_clean, MBA=ifelse(Programs=="Full-Time MBA"|Programs=="Executive MBA"|
                                                Programs=="Fully Employed MBA"|Programs=="JD/MBA"|Programs=="MD/MBA", 1, 0))

appli_clean <- mutate(appli_clean, special_1year=ifelse(Programs=="Full-Time MBA"|Programs=="Executive MBA"|
                                                          Programs=="Fully Employed MBA"|Programs=="JD/MBA"|Programs=="MD/MBA", 0, 1))

# logistic regression for program
gml_resident <- glm(Program~Resident,family = binomial(), data = appli_clean)
summary(gml_resident)
exp(coef(gml_resident))

gml_intl <- glm(Program~International,family = binomial(), data = appli_clean)
summary(gml_intl)
exp(coef(gml_intl))

######

all_inquiry <- read.csv("All Inquiry Form Conversions.csv", header = TRUE)

# data cleaning
all_inquiry  <- all_inquiry [-c(5,7,8)]

# filter
all_inquiry <- all_inquiry %>% filter(Student.Stage != "Archive")

# convert
all_inquiry  <- mutate(all_inquiry, Program=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                                     Program=="Fully Employed MBA"|Program=="JD/MBA"|Program=="MD/MBA", 1, 0))
all_inquiry  <- mutate(all_inquiry, Applicant=ifelse(Student.Stage=="Applicant", 1, 0))
all_inquiry  <- mutate(all_inquiry, Inquiry=ifelse(Student.Stage=="Inquiry", 1, 0))

# logistic regression for inquiry
inquiry_applicant <- glm(Program~Applicant,family = binomial(), data = all_inquiry)
summary(inquiry_applicant)
exp(coef(inquiry_applicant))

inquiry_inquiry <- glm(Program~Inquiry,family = binomial(), data = all_inquiry)
summary(inquiry_inquiry)
exp(coef(inquiry_inquiry))

######

# data
fb <- read.csv("Media Type Conversion - Facebook.csv", header = TRUE)
google <- read.csv("Media Type COnversion - Google.csv", header = TRUE)
linkedin <- read.csv("Media Type COnversion - LinkedIn.csv", header = TRUE)

# clean
fb <- fb %>% filter(Student.Stage != "Archive")
fb <- fb %>% filter(Program != "Master of Science in Biotechnology Management") %>% 
  filter(Program != "Master of Science in Engineering Management")

google <- google %>% filter(Student.Stage != "Archive")
google <- google %>% filter(Program != "Master of Science in Engineering Management") %>%
  filter(Program != "PH.D. in Management")

linkedin <- linkedin %>% filter(Student.Stage != "Archive")
linkedin <- linkedin %>% filter(Program != "Master of Science in Biotechnology Management")

# convert
fb <- mutate(fb, Inquiry=ifelse(Student.Stage=="Inquiry", 0, 1))
google <- mutate(google, Inquiry=ifelse(Student.Stage=="Inquiry", 0, 1))
linkedin <- mutate(linkedin, Inquiry=ifelse(Student.Stage=="Inquiry", 0, 1))

fb <- mutate(fb, MBA=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                              Program=="Fully Employed MBA"|Program=="JD/MBA"|
                              Program=="MD/MBA", 1, 0))

fb <- mutate(fb, year_1=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                 Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                 Program=="MD/MBA", 0, 1))

google <- mutate(google, MBA=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                      Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                      Program=="MD/MBA", 1, 0))

google <- mutate(google, year_1=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                         Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                         Program=="MD/MBA", 0, 1))

linkedin <- mutate(linkedin, MBA=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                          Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                          Program=="MD/MBA", 1, 0))

linkedin <- mutate(linkedin, year_1=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                             Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                             Program=="MD/MBA", 0, 1))

# logistic regression for facebook
fb_gml_mba<- glm(Inquiry~MBA,family = binomial(), data = fb)
summary(fb_gml_mba)
exp(coef(fb_gml_mba))

fb_gml_year_1 <- glm(Inquiry~year_1,family = binomial(), data = fb)
summary(fb_gml_year_1)
exp(coef(fb_gml_year_1))

# logistic regression -google
google_gml_mba <- glm(Inquiry~MBA,family = binomial(), data = google)
summary(google_gml_mba)
exp(coef(google_gml_mba))

google_gml_year_1 <- glm(Inquiry~year_1,family = binomial(), data = google)
summary(google_gml_year_1)
exp(coef(google_gml_year_1))

# logistic regression - linkedin
linkedin_gml_mba <- glm(Inquiry~MBA,family = binomial(), data = linkedin)
summary(linkedin_gml_mba )
exp(coef(linkedin_gml_mba ))

linkedin_gml_year_1<- glm(Inquiry~year_1,family = binomial(), data = linkedin)
summary(linkedin_gml_year_1)
exp(coef(linkedin_gml_year_1))

######
 