setwd("C:/rramach/ChainChecker/Nosocomial piece")
vhf_metadata <- read.csv("vhf-columns.csv")

nscm_data <- data.frame(matrix(ncol=51,nrow=50))
colnames(nscm_data) <- vhf_metadata$column_name
# library(devtools)
# install_github("trinker/wakefield") #Tyler Rinker
library(wakefield)

set.seed(12321)

#library(synthpop)
library(lubridate)
#http://apps.who.int/gho/data/view.ebola-sitrep.ebola-summary-latest-age-sex
nscm_data$Age <- age(50, x=c(0:14, 15:44, 45:110), prob=c(rep((0.1814/15),15),rep((0.5635/30),30),rep((0.2174/66),66)))
nscm_data$Gender <- sex(50,x=c("Male","Female"),prob=c(0.4891406, 0.5108594))
nscm_data$AgeUnit <- "years"
sampsize <- dim(nscm_data)[1]
#Ebola epidemic in West Africa ranged from Jan 2014 to Jun 2018: https://www.independent.co.uk/news/world/africa/ebola-crisis-outbreak-timeline-disease-virus-africa-congo-facts-who-a8357676.html
#Coltart, C. E., Lindsey, B., Ghinai, I., Johnson, A. M., & Heymann, D. L. (2017). The Ebola outbreak, 2013-2016: old lessons for new epidemics. Philosophical Transactions of the Royal Society B: Biological Sciences, 372(1721), 20160297.
nscm_data$reported_onset_date <- sample(seq(as.Date('2013-12-01'), as.Date('2016-09-07'), by="day"),sampsize, replace=TRUE) #Date of Illness Onset

for(i in 1:sampsize)
{
  # Date of Death has to be after Onset Date
  #https://www.cdc.gov/vhf/ebola/outbreaks/drc/2018-may.html
  nscm_data$death_date[i] <- sample(seq.Date(from=as.Date(nscm_data$reported_onset_date[i], origin="1970-01-01"), to=as.Date("2018-07-24"),by=1),1,replace=TRUE)
  #Date of admission to current hospital has to be between Illness Onset and Death date
  nscm_data$DateHospitalCurrentAdmit[i] <- sample(seq.Date(from=as.Date('2013-12-01'), to=as.Date(nscm_data$death_date[i], origin = "1970-01-01"), by=1),1,replace=TRUE)
  #Was patient hospitalized or did he-she go to a clinic or visit anyone in the hospital before this illness? if yes, date start
  nscm_data$HospitalBeforeIllDateStart[i] <- sample(seq.Date(from=as.Date('2014-01-01',origin = "1970-01-01"), to=as.Date(nscm_data$reported_onset_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #If yes to above question, date end
  nscm_data$HospitalBeforeIllDateEnd[i] <- sample(seq.Date(from=as.Date(nscm_data$HospitalBeforeIllDateStart[i],origin = "1970-01-01"), to=as.Date(nscm_data$reported_onset_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #Date of hospitalization 1 start
  nscm_data$DateHospitalPastStart1[i] <- sample(seq.Date(from=as.Date(nscm_data$DateHospitalCurrentAdmit[i],origin = "1970-01-01"), to=as.Date(nscm_data$death_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #Date of hospitalization 1 end
  nscm_data$DateHospitalPastEnd1[i] <- sample(seq.Date(from=as.Date(nscm_data$DateHospitalPastStart1[i],origin = "1970-01-01"), to=as.Date(nscm_data$death_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #Date of hospitalization 2 start
  nscm_data$DateHospitalPastStart2[i] <- sample(seq.Date(from=as.Date(nscm_data$DateHospitalPastEnd1[i],origin = "1970-01-01"), to=as.Date(nscm_data$death_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #Date of hospitalization 2 end
  nscm_data$DateHospitalPastEnd2[i] <- sample(seq.Date(from=as.Date(nscm_data$DateHospitalPastStart2[i],origin = "1970-01-01"), to=as.Date(nscm_data$death_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #Date of visit to traditional healer (Did the patient consult a traditional healer before becoming ill; if yes, date estimated)
  nscm_data$TradHealerDate[i] <- sample(seq.Date(from=as.Date('2014-01-01'), to=as.Date(nscm_data$reported_onset_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #Date of Ebola vaccination
  nscm_data$ebolaDateVacc[i] <- sample(seq.Date(from=as.Date('2014-01-01'), to=as.Date(nscm_data$death_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #date of onset of source
  nscm_data$reported_onset_datevhf_source[i] <- sample(seq.Date(from=as.Date('2014-01-01'), to=as.Date(nscm_data$reported_onset_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #date of death of source
  nscm_data$death_date_source[i] <- sample(seq.Date(from=as.Date(nscm_data$reported_onset_datevhf_source[i],origin = "1970-01-01"), to=as.Date(nscm_data$death_date[i],origin = "1970-01-01"), by=1),1,replace=TRUE)
  #date of non-secure burial
  nscm_data$date_of_nonsecure_burial[i] <- sample(seq.Date(from=as.Date(nscm_data$death_date[i], origin="1970-01-01"), to=as.Date(today()), by=1),1,replace=TRUE) 
}

nscm_data$reported_onset_date <- (as.Date(nscm_data$reported_onset_date, origin="1970-01-01"))
nscm_data$death_date <- (as.Date(nscm_data$death_date, origin="1970-01-01"))
nscm_data$DateHospitalCurrentAdmit <- (as.Date(nscm_data$DateHospitalCurrentAdmit, origin="1970-01-01"))
nscm_data$HospitalBeforeIllDateStart <- (as.Date(nscm_data$HospitalBeforeIllDateStart, origin="1970-01-01"))
nscm_data$HospitalBeforeIllDateEnd <- (as.Date(nscm_data$HospitalBeforeIllDateEnd, origin="1970-01-01"))
nscm_data$DateHospitalPastStart1 <- (as.Date(nscm_data$DateHospitalPastStart1, origin="1970-01-01"))
nscm_data$DateHospitalPastEnd1 <- (as.Date(nscm_data$DateHospitalPastEnd1, origin="1970-01-01"))
nscm_data$DateHospitalPastStart2 <- (as.Date(nscm_data$DateHospitalPastStart2, origin="1970-01-01"))
nscm_data$DateHospitalPastEnd2 <- (as.Date(nscm_data$DateHospitalPastEnd2, origin="1970-01-01"))
nscm_data$TradHealerDate <- (as.Date(nscm_data$TradHealerDate, origin="1970-01-01"))
nscm_data$ebolaDateVacc <- (as.Date(nscm_data$ebolaDateVacc, origin="1970-01-01"))
nscm_data$reported_onset_datevhf_source <- (as.Date(nscm_data$reported_onset_datevhf_source, origin="1970-01-01"))
nscm_data$death_date_source <- (as.Date(nscm_data$death_date_source, origin="1970-01-01"))
nscm_data$date_of_nonsecure_burial <- (as.Date(nscm_data$date_of_nonsecure_burial, origin="1970-01-01"))

nscm_data$HCW <- rbinom(sampsize,1,prob=0.5)
nscm_data$TraditionalHealer <- rbinom(sampsize,1,prob=0.5)
nscm_data$Pregnant <- rbinom(sampsize,1,prob=0.05)
nscm_data$Lactating <- rbinom(sampsize,1,prob=0.01)
nscm_data$vaccAgainstEbola <- rbinom(sampsize,1,prob=0.1)
nscm_data$participated_nonsecure_burial <- rbinom(sampsize,1,prob=0.1)
nscm_data$infectionNosocomial <- rbinom(sampsize,1,prob=0.5)
nscm_data$infection_community <-rbinom(sampsize,1,prob=0.5)

prob_choices <- c("None","Low","Medium","High")
nscm_data$probability_of_source_of_infection <- sample(x=prob_choices,size=sampsize,replace=TRUE)
nscm_data$probability_virus_transmission_due_to_burial <- sample(x=prob_choices,size=sampsize,replace=TRUE)
nscm_data$probability_virus_transmission_due_to_nosocomial <- sample(x=prob_choices,size=sampsize,replace=TRUE)
nscm_data$probability_virus_transmission_due_to_community <- sample(x=prob_choices,size=sampsize,replace=TRUE)

cas_source_choices <- c("Family","Friend","Co-worker","Neighbor","Healthcare Worker","Religious Figure","Transportation Personnel","Traditional Healer")
nscm_data$lien_cas_source <- sample(x=cas_source_choices,size=sampsize,replace=TRUE)

library(stringi)
nscm_data$id <- stri_rand_strings(sampsize,10)
nscm_data$caseId_source <- (sample(100000:999999,sampsize,replace=TRUE))

library(data.table)
library(randomNames)
nscm_data$Surname <- randomNames(sampsize, nscm_data$Gender, ethnicity=3, which.names="last")
nscm_data$OtherNames <- randomNames(sampsize, nscm_data$Gender, ethnicity=3, which.names="first")

nscm_data$surname_source <- randomNames(sampsize, nscm_data$Gender, ethnicity=3, which.names="last")
nscm_data$OtherNames_source <- randomNames(sampsize, nscm_data$Gender, ethnicity=3, which.names="first")
nscm_data$TradHealerName <- randomNames(sampsize, nscm_data$Gender, ethnicity=3, which.names="both")

setwd("C:/rramach/ChainChecker")
library(xlsx)
#https://www.fantasynamegenerators.com/hospital-names.php
hosp_names <- read.xlsx(file="Randomly_generated_hospital_names.xlsx",1)
hosp_names <- t(hosp_names)
nscm_data$HospitalCurrent <- sample(hosp_names,size=sampsize,replace=TRUE)
nscm_data$HospitalBeforeIllName <- sample(hosp_names,size=sampsize,replace=TRUE)
nscm_data$HospitalPast1 <- sample(hosp_names,size=sampsize,replace=TRUE)
nscm_data$HospitalPast2 <- sample(hosp_names,size=sampsize,replace=TRUE)

#Use numbers for location-based fields (e.g.zone, parish, and village) for both patient and source since we don't have access to the drop-down menu choices
nscm_data$SCRes <- sample((1:sampsize),size=sampsize,replace=TRUE)
nscm_data$ParishRes <- sample((1:sampsize),size=sampsize,replace=TRUE)
nscm_data$VillageRes <- sample((1:sampsize),size=sampsize,replace=TRUE)
nscm_data$SCOnset <- sample((1:sampsize),size=sampsize,replace=TRUE)
nscm_data$VillageOnset <- sample((1:sampsize),size=sampsize,replace=TRUE)

nscm_data$SCRes_source <- sample((1:sampsize),size=sampsize,replace=TRUE)
nscm_data$Parish_source <- sample((1:sampsize),size=sampsize,replace=TRUE)
nscm_data$VillageRes_source <- sample((1:sampsize),size=sampsize,replace=TRUE)
nscm_data$SCOnset_source <- sample((1:sampsize),size=sampsize,replace=TRUE)
nscm_data$VillageOnset_source <- sample((1:sampsize),size=sampsize,replace=TRUE)

