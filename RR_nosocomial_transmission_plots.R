library(ggplot2)
library(reshape2)
library(dplyr)
library(vistime)

set.seed(12321)
nscm_data$incubation_period <- sample(c(2:21),sampsize, replace=TRUE) #incubation period for Ebola: 2 to 21 days; https://www.who.int/csr/disease/ebola/faq-ebola/en/
nscm_data$death_avail <- rbinom(sampsize,1,prob=0.5) #average Ebola Virus disease (EVD) case fatality is around 50% (Case fatility rates have varied from 25-90% in past outbreaks): https://www.afro.who.int/health-topics/ebola-virus-disease
nscm_data$bleeding_at_reported_onset <- rbinom(sampsize,1,prob=0.5) #McElroy, A. (2015). Understanding bleeding in Ebola virus disease. Clinical advances in hematology & oncology: H&O, 13(1), 29.
nscm_data$diarrhea_at_reported_onset <- rbinom(sampsize,1,prob=0.5) #Schieffelin, J. S., Shaffer, J. G., Goba, A., Gbakie, M., Gire, S. K., Colubri, A., ... & Fullah, M. (2014). Clinical illness and outcomes in patients with Ebola in Sierra Leone. New england journal of medicine, 371(22), 2092-2100.

nscm_data$days_onset_to_death <- sample(c(6:16), sampsize, replace=TRUE) #Ruzek, edited by Singh S. Viral Hemorrhagic Fevers, 2014 (book)
nscm_data$days_onset_to_bleeding <- sample(c(5:7), sampsize, replace=TRUE) #http://www.ebolavirusnet.com/signs-a-symptoms.html
nscm_data$days_onset_to_diarrhea <- sample(c(5:7), sampsize, replace=TRUE) #inferred from above link

#Hospitalization time for EVD survivors (group 1) and patients who died (group 2) in Sierra Leone
# Ji, Y. J., Duan, X. Z., Gao, X. D., Li, L., Li, C., Ji, D., Li, W. G., Wang, L. F., Meng, Y. H., Yang, X., Ling, 
# B. F., Song, X. A., Gu, M. L., Jiang, T., Koroma, S. M., Bangalie, J., . Duan, H. J. (2016). 
# Clinical presentations and outcomes of patients with Ebola virus disease in Freetown, Sierra Leone. Infectious diseases of poverty, 5(1), 101. doi:10.1186/s40249-016-0195-9
mn1 <- 20.38
sd1 <- 7.58
N1 <- 146

mn2 <- 5.56
N2 <- 139
sd2 <- 6.11

mn12 <- ((N1*mn1) + (N2*mn2))/(N1+N2)
sd12 <- sqrt((
  ((N1-1)*(sd1^2)) + ((N2-1)*(sd2^2)) + (((N1*N2)/(N1+N2))*((mn1^2) + (mn2^2) - (2*mn1*mn2)))
  )/(N1+N2-1))  
  
hosp_LOS <- rnorm(sampsize,mean=mn1,sd=sd1)
hosp_LOS[hosp_LOS<0] <- 0 #lower limit of LOS has to be zero (non-negative)

nscm_data$DateDisch <- (as.Date(nscm_data$reported_onset_date)+ round(hosp_LOS,0)) #Date of Discharge for patients that did not die

#based on calculator_functions.R in ChainChecker
nscm_data$DateExposure <-  (as.Date(nscm_data$reported_onset_date)- nscm_data$incubation_period) 
nscm_data$EndDate <- (as.Date(nscm_data$death_date)) #end date is death date or discharge date for those patients that survived
nscm_data$EndDate[nscm_data$death_avail==0] <- (as.Date(nscm_data$DateDisch[nscm_data$death_avail==0]))
#true date of onset  
library(dplyr)

nscm_data$true_reported_onset_date <- ifelse(nscm_data$death_avail==1,(as.Date(nscm_data$reported_onset_date)- nscm_data$days_onset_to_death),
                                          ifelse(nscm_data$bleeding_at_reported_onset==1,((as.Date(nscm_data$reported_onset_date)- nscm_data$days_onset_to_bleeding)),
                                              ifelse(nscm_data$diarrhea_at_reported_onset==1,((as.Date(nscm_data$reported_onset_date)- nscm_data$days_onset_to_diarrhea)),
                                                (as.Date(nscm_data$reported_onset_date)))))
nscm_data$true_reported_onset_date <- as.Date(nscm_data$true_reported_onset_date, origin="1970-01-01")

setwd("C:/rramach/ChainChecker/Nosocomial piece")
write.csv(nscm_data,file="Synthetic_ChainChecker_data_for_RShiny.csv")

#group dataset by hospital
hosp_gantt <- as.data.frame(nscm_data %>% group_by(HospitalCurrent) %>% select(HospitalCurrent,DateHospitalCurrentAdmit, EndDate, true_reported_onset_date, infectionNosocomial) %>% arrange(HospitalCurrent))
hosp_gantt$infectionNosocomial[hosp_gantt$infectionNosocomial==0] <- "Not Nosocomial"
hosp_gantt$infectionNosocomial[hosp_gantt$infectionNosocomial==1] <- "Nosocomial"
names(hosp_gantt) <- c('Hospital','Start_date','End_date','true_reported_onset_date','Nosocomial_status')
hosp_gantt$Start_date <- as.Date(hosp_gantt$Start_date, format="%d/%m/%Y")
hosp_gantt$End_date <- as.Date(hosp_gantt$End_date, format="%d/%m/%Y")
hosp_gantt$earlyOnset <-  (as.Date(hosp_gantt$true_reported_onset_date, format="%d/%m/%Y") < hosp_gantt$Start_date) #(as.Date(hosp_gantt$DateOnset, format="%d/%m/%Y") >= as.Date(hosp_gantt$Admission_date)-hosp_gantt$incubation_period)    

#group dataset by patient
pat_gantt <- as.data.frame(nscm_data %>% group_by(Surname,OtherNames) %>% select(HospitalCurrent,DateHospitalCurrentAdmit, DateDeath, HospitalBeforeIllName,HospitalBeforeIllDateStart, HospitalBeforeIllDateEnd, HospitalPast1, DateHospitalPastStart1, DateHospitalPastEnd1, HospitalPast2, DateHospitalPastStart2, DateHospitalPastEnd2, true_reported_onset_date, infectionNosocomial, Surname, OtherNames)) %>% arrange(Surname)
pat_gantt$infectionNosocomial[pat_gantt$infectionNosocomial==0] <- "Not Nosocomial"
pat_gantt$infectionNosocomial[pat_gantt$infectionNosocomial==1] <- "Nosocomial"
pat_gantt$FullName <- paste0(pat_gantt$Surname,",",pat_gantt$OtherNames)
pat_gantt$true_reported_onset_date <- format.Date(pat_gantt$true_reported_onset_date, format="%d/%m/%Y")
pat_gantt$earlyOnset <-  (pat_gantt$true_reported_onset_date < pat_gantt$DateHospitalCurrentAdmit) #(as.Date(hosp_gantt$DateOnset, format="%d/%m/%Y") >= as.Date(hosp_gantt$Admission_date)-hosp_gantt$incubation_period)    

library(plyr)
pat_gantt <- rename(pat_gantt,c("HospitalBeforeIllDateStart"="DateHospitalBeforeIllStart", "HospitalBeforeIllDateEnd"="DateHospitalBeforeIllEnd", "infectionNosocomial"="Nosocomial_status"))
pat_gantt <- subset(pat_gantt, select=-c(Surname,OtherNames))
library(data.table)
colsA=c("HospitalCurrent","HospitalBeforeIllName","HospitalPast1","HospitalPast2")
colsB=c("DateHospitalCurrentAdmit","DateHospitalBeforeIllStart","DateHospitalPastStart1","DateHospitalPastStart2")
colsC=c("DateDeath","DateHospitalBeforeIllEnd","DateHospitalPastEnd1","DateHospitalPastEnd2")
#melt data to get information by hospital (current, before ill, past1, past2)
pat_melted <- data.table::melt(setDT(pat_gantt), id=c("FullName","Nosocomial_status","earlyOnset"), measure=list(colsA, colsB, colsC), value.name=c("Hospital","Start_Date","End_Date"))
pat_melted <- pat_melted %>% group_by(FullName) %>% arrange(FullName)
pat_melted$Start_Date <- as.Date(pat_melted$Start_Date, format="%d/%m/%Y")
pat_melted$End_Date <- as.Date(pat_melted$End_Date, format="%d/%m/%Y")

#plot GANTT charts at both the hospital and patient levels
#GANTT charts for hospitals
setwd("C:/rramach/ChainChecker/Nosocomial piece/html_output")
hosp_gantt$Start_date <- as.Date(hosp_gantt$Start_date)
hosp_gantt$End_date <- as.Date(hosp_gantt$End_date)
names(hosp_gantt)[2] <- "Hospitalization_Dates"
tooltip <- paste0(hosp_gantt$Hospitalization_Dates, " to ", hosp_gantt$End_date)
hosp_gantt$time_nosocomial <- paste0(tooltip,",",hosp_gantt$Nosocomial_status)
hosp_timeline <-
  ggplot(hosp_gantt, aes(x=Hospitalization_Dates,xend=End_date,y=Hospital,yend=Hospital,color=earlyOnset, label=time_nosocomial)) +
  labs(title="Hospital admissions during Ebola outbreak") +
  scale_color_manual(values=c("TRUE" = "red", "FALSE" = "green")) +
  geom_segment(size=4) + 
  theme(axis.title.y=element_blank(),legend.position="none")

#title("Hospital admissions during Ebola outbreak")
#hosp_timeline$tooltip <- hosp_timeline$data$tooltip

library(plotly)
GANTT_hosp <- ggplotly(hosp_timeline, tooltip=c("time_nosocomial"))
htmlwidgets::saveWidget(as_widget(GANTT_hosp), "GANTT_hospital.html")

#GANTT charts for patients
patient_timelines <- list()
tooltip <- paste0(pat_melted$Start_Date, " to ",pat_melted$End_Date)

#pdf("Patient_GANTT_charts_labels.pdf")
pat_melted$Start_Date <- as.Date(pat_melted$Start_Date)
pat_melted$End_Date <- as.Date(pat_melted$End_Date)
names(pat_melted)[6] <- "Hospitalization_Dates"
tooltip <- paste0(pat_melted$Hospitalization_Dates, " to ", pat_melted$End_Date)
pat_melted$time_nosocomial <- paste0(tooltip,",",pat_melted$Nosocomial_status)
for(i in 1:dim(nscm_data)[1])
{
  pat_melt_sub <- pat_melted[c((4*(i-1)+1):(4*i)),]
  ptime <- ggplot(pat_melt_sub, aes(x=Hospitalization_Dates,xend=End_Date,y=paste0(Hospital,",",variable,sep=""),yend=paste0(Hospital,",",variable,sep=""),color=earlyOnset, label=time_nosocomial)) +
    labs(title="Patient admissions (Current Hospital, Hospital before Ill, Hospital Past 1, Hospital Past 2) during Ebola outbreak") +
    scale_color_manual(values=c("TRUE" = "red", "FALSE" = "green")) +
    geom_segment(size=4) + 
    theme(axis.title.y=element_blank(),legend.position="none")
  patient_timelines[[i]] <- ggplotly(ptime, tooltip=c("time_nosocomial"))
  htmlwidgets::saveWidget(as_widget(patient_timelines[[i]]), paste0("GANTT_patient",i,".html",sep=""))
  
  #print(patient_timelines[[i]]) 
}
#dev.off()
#Order of hospitals in pat_melted: Current hospital, Hospital Before Ill, Hospital Past 1, Hospital Past 2