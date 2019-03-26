library(ggplot2)
library(reshape2)
library(dplyr)
library(vistime)
library(plyr)
library(data.table)
library(plotly)

get_nosocomial_plots_by_case_id <- function(input, nscm_data){
  nscm_data$incubation_period <- input$noso_min_incubation_all #incubation period for Ebola: 2 to 21 days; https://www.who.int/csr/disease/ebola/faq-ebola/en/
  nscm_data$death_avail <- input$noso_death_avail #average Ebola Virus disease (EVD) case fatality is around 50% (Case fatility rates have varied from 25-90% in past outbreaks): https://www.afro.who.int/health-topics/ebola-virus-disease
  nscm_data$bleeding_at_onset <- input$noso_bleeding_at_onset #McElroy, A. (2015). Understanding bleeding in Ebola virus disease. Clinical advances in hematology & oncology: H&O, 13(1), 29.
  nscm_data$diarrhea_at_onset <- input$noso_diarrhea_at_onset #Schieffelin, J. S., Shaffer, J. G., Goba, A., Gbakie, M., Gire, S. K., Colubri, A., ... & Fullah, M. (2014). Clinical illness and outcomes in patients with Ebola in Sierra Leone. New england journal of medicine, 371(22), 2092-2100.

  nscm_data$days_onset_to_death <- input$noso_days_onset_to_death_all #Ruzek, edited by Singh S. Viral Hemorrhagic Fevers, 2014 (book)
  nscm_data$days_onset_to_bleeding <- input$noso_days_onset_to_bleeding_all #http://www.ebolavirusnet.com/signs-a-symptoms.html
  nscm_data$days_onset_to_diarrhea <- input$noso_days_onset_to_diarrhea_all #inferred from above link

  #Hospitalization time for EVD survivors (group 1) and patients who died (group 2) in Sierra Leone
  # Ji, Y. J., Duan, X. Z., Gao, X. D., Li, L., Li, C., Ji, D., Li, W. G., Wang, L. F., Meng, Y. H., Yang, X., Ling, 
  # B. F., Song, X. A., Gu, M. L., Jiang, T., Koroma, S. M., Bangalie, J., . Duan, H. J. (2016). 
  # Clinical presentations and outcomes of patients with Ebola virus disease in Freetown, Sierra Leone. Infectious diseases of poverty, 5(1), 101. doi:10.1186/s40249-016-0195-9

  nscm_data$DateDisch <- as.Date(Sys.Date(), format="%d/%m/%Y") #Date of Discharge for patients that did not die

  #based on calculator_functions.R in ChainChecker
  nscm_data$DateExposure <-  as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$incubation_period
  nscm_data$EndDate <- nscm_data$DateDeath #end date is death date or discharge date for those patients that survived
  nscm_data$EndDate[!is.na(as.Date(nscm_data$death_date, format="%d/%m/%Y"))] = format.Date(nscm_data$DateDisch[!is.na(as.Date(nscm_data$death_date, format="%d/%m/%Y"))],  format="%d/%m/%Y")
  #true date of onset  
  nscm_data$true_DateOnset <- ifelse(nscm_data$death_avail==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_death),
                                    ifelse(nscm_data$bleeding_at_onset==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_bleeding),
                                        ifelse(nscm_data$diarrhea_at_onset==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_diarrhea),
                                              as.Date(nscm_data$onset_date, format="%d/%m/%Y"))))
  nscm_data$true_DateOnset <- as.Date(nscm_data$true_DateOnset, origin="1970-01-01")

  #group dataset by patient
  pat_gantt <- as.data.frame(nscm_data %>% group_by(Surname,OtherNames) %>% select(id, HospitalCurrent,DateHospitalCurrentAdmit, DateDeath, HospitalBeforeIllName,HospitalBeforeIllDateStart, HospitalBeforeIllDateEnd, HospitalPast1, DateHospitalPastStart1, DateHospitalPastEnd1, HospitalPast2, DateHospitalPastStart2, DateHospitalPastEnd2, true_DateOnset, infectionNosocomial, Surname, OtherNames)) %>% arrange(Surname)
  pat_gantt$CaseID <- paste0(pat_gantt$id)
  pat_gantt$true_DateOnset <- format.Date(pat_gantt$true_DateOnset, format="%d/%m/%Y")
  pat_gantt$earlyOnset <-  (as.Date(pat_gantt$DateHospitalCurrentAdmit) < pat_gantt$true_DateOnset) #(as.Date(hosp_gantt$DateOnset, format="%d/%m/%Y") >= as.Date(hosp_gantt$Admission_date)-hosp_gantt$incubation_period)    
  pat_gantt$infectionNosocomial <- ifelse(pat_gantt$earlyOnset == TRUE, "Nosocomial", "Not Nosocomial")

  #pat_gantt <- rename(pat_gantt,c("HospitalBeforeIllDateStart"="DateHospitalBeforeIllStart", "HospitalBeforeIllDateEnd"="DateHospitalBeforeIllEnd", "infectionNosocomial"="Nosocomial_status"))
  pat_gantt <- subset(pat_gantt, select=-c(Surname,OtherNames))

  colsA=c("HospitalCurrent","HospitalBeforeIllName","HospitalPast1","HospitalPast2")
  colsB=c("DateHospitalCurrentAdmit","HospitalBeforeIllDateStart","DateHospitalPastStart1","DateHospitalPastStart2")
  colsC=c("DateDeath","HospitalBeforeIllDateEnd","DateHospitalPastEnd1","DateHospitalPastEnd2")
  #melt data to get information by hospital (current, before ill, past1, past2)
  pat_melted <- data.table::melt(setDT(pat_gantt), c("CaseID","infectionNosocomial","earlyOnset", "true_DateOnset"), measure=list(colsA, colsB, colsC), value.name=c("Hospital","Start_Date","End_Date"))
  pat_melted <- pat_melted %>% group_by(CaseID) %>% arrange(CaseID)

  pat_melted$Start_Date <- as.Date(pat_melted$Start_Date, format="%d/%m/%Y")
  pat_melted$End_Date <- as.Date(pat_melted$End_Date, format="%d/%m/%Y")

  patient_timelines <- list()
  tooltip <- paste0(pat_melted$Start_Date, " to ", pat_melted$End_Date)

  names(pat_melted)[7] <- "Hospitalization_Dates"
  tooltip <- paste0(pat_melted$Hospitalization_Dates, " to ", pat_melted$End_Date, ". Date Onset: ", pat_melted$true_DateOnset)
  pat_melted$time_nosocomial <- paste0(tooltip,",",pat_melted$infectionNosocomial)

  patient_timeline = NULL
  for(i in 1:dim(nscm_data)[1])
  {
    pat_melt_sub <- pat_melted[c((4*(i-1)+1):(4*i)),]
     ptime <- ggplot(pat_melt_sub, aes(x=Hospitalization_Dates,xend=End_Date,y=paste0(Hospital,",",variable,sep=""),yend=paste0(Hospital,",",variable,sep=""),color=earlyOnset, label=time_nosocomial)) +
      labs(title=paste0("Patient admissions (Current Hospital, Hospital before Ill, Hospital Past 1, Hospital Past 2) for ", input$noso_case_id)) +
      scale_color_manual(values=c("TRUE" = "green", "FALSE" = "red")) +
      geom_segment(size=4) + 
      theme(axis.title.y=element_blank(),legend.position="none")
    
    if(pat_melt_sub[1,]$CaseID == input$noso_case_id){
      patient_timeline = ggplotly(ptime, tooltip=c("time_nosocomial"))
    }
  }
  
  if(is.na(input$noso_case_id) || input$noso_case_id == "" || nrow(pat_melt_sub) == 0){
    stop(safeError("Please select a Case ID"))
    
  }else{
    patient_timeline
  }

#dev.off()
#Order of hospitals in pat_melted: Current hospital, Hospital Before Ill, Hospital Past 1, Hospital Past 2
}

get_nosocomial_plots_by_hospital_patients <- function(input, nscm_data){
  nscm_data$incubation_period <- input$noso_min_incubation_all #incubation period for Ebola: 2 to 21 days; https://www.who.int/csr/disease/ebola/faq-ebola/en/
  nscm_data$death_avail <- input$noso_death_avail #average Ebola Virus disease (EVD) case fatality is around 50% (Case fatility rates have varied from 25-90% in past outbreaks): https://www.afro.who.int/health-topics/ebola-virus-disease
  nscm_data$bleeding_at_onset <- input$noso_bleeding_at_onset #McElroy, A. (2015). Understanding bleeding in Ebola virus disease. Clinical advances in hematology & oncology: H&O, 13(1), 29.
  nscm_data$diarrhea_at_onset <- input$noso_diarrhea_at_onset #Schieffelin, J. S., Shaffer, J. G., Goba, A., Gbakie, M., Gire, S. K., Colubri, A., ... & Fullah, M. (2014). Clinical illness and outcomes in patients with Ebola in Sierra Leone. New england journal of medicine, 371(22), 2092-2100.

  nscm_data$days_onset_to_death <- input$noso_days_onset_to_death_all #Ruzek, edited by Singh S. Viral Hemorrhagic Fevers, 2014 (book)
  nscm_data$days_onset_to_bleeding <- input$noso_days_onset_to_bleeding_all #http://www.ebolavirusnet.com/signs-a-symptoms.html
  nscm_data$days_onset_to_diarrhea <- input$noso_days_onset_to_diarrhea_all #inferred from above link

  #Hospitalization time for EVD survivors (group 1) and patients who died (group 2) in Sierra Leone
  # Ji, Y. J., Duan, X. Z., Gao, X. D., Li, L., Li, C., Ji, D., Li, W. G., Wang, L. F., Meng, Y. H., Yang, X., Ling, 
  # B. F., Song, X. A., Gu, M. L., Jiang, T., Koroma, S. M., Bangalie, J., . Duan, H. J. (2016). 
  # Clinical presentations and outcomes of patients with Ebola virus disease in Freetown, Sierra Leone. Infectious diseases of poverty, 5(1), 101. doi:10.1186/s40249-016-0195-9

  nscm_data$DateDisch <- as.Date(Sys.Date(), format="%d/%m/%Y") #Date of Discharge for patients that did not die

  #based on calculator_functions.R in ChainChecker
  nscm_data$DateExposure <-  as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$incubation_period
  nscm_data$EndDate <- nscm_data$DateDeath #end date is death date or discharge date for those patients that survived
  nscm_data$EndDate[!is.na(as.Date(nscm_data$death_date, format="%d/%m/%Y"))] = format.Date(nscm_data$DateDisch[!is.na(as.Date(nscm_data$death_date, format="%d/%m/%Y"))],  format="%d/%m/%Y")
  #true date of onset  
  nscm_data$true_DateOnset <- ifelse(nscm_data$death_avail==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_death),
                                    ifelse(nscm_data$bleeding_at_onset==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_bleeding),
                                        ifelse(nscm_data$diarrhea_at_onset==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_diarrhea),
                                              as.Date(nscm_data$onset_date, format="%d/%m/%Y"))))
  nscm_data$true_DateOnset <- as.Date(nscm_data$true_DateOnset, origin="1970-01-01")

  #group dataset by patient
  pat_gantt <- as.data.frame(nscm_data %>% group_by(Surname,OtherNames) %>% select(id, HospitalCurrent,DateHospitalCurrentAdmit, DateDeath, HospitalBeforeIllName,HospitalBeforeIllDateStart, HospitalBeforeIllDateEnd, HospitalPast1, DateHospitalPastStart1, DateHospitalPastEnd1, HospitalPast2, DateHospitalPastStart2, DateHospitalPastEnd2, true_DateOnset, infectionNosocomial, Surname, OtherNames)) %>% arrange(Surname)
  pat_gantt$CaseID <- paste0(pat_gantt$id)
  pat_gantt$true_DateOnset <- format.Date(pat_gantt$true_DateOnset, format="%d/%m/%Y")
  pat_gantt$earlyOnset <-  (as.Date(pat_gantt$DateHospitalCurrentAdmit) < pat_gantt$true_DateOnset) #(as.Date(hosp_gantt$DateOnset, format="%d/%m/%Y") >= as.Date(hosp_gantt$Admission_date)-hosp_gantt$incubation_period)    
  pat_gantt$infectionNosocomial <- ifelse(pat_gantt$earlyOnset == TRUE, "Nosocomial", "Not Nosocomial")

  #pat_gantt <- rename(pat_gantt,c("HospitalBeforeIllDateStart"="DateHospitalBeforeIllStart", "HospitalBeforeIllDateEnd"="DateHospitalBeforeIllEnd", "infectionNosocomial"="Nosocomial_status"))
  pat_gantt <- subset(pat_gantt, select=-c(Surname,OtherNames))

  colsA=c("HospitalCurrent","HospitalBeforeIllName","HospitalPast1","HospitalPast2")
  colsB=c("DateHospitalCurrentAdmit","HospitalBeforeIllDateStart","DateHospitalPastStart1","DateHospitalPastStart2")
  colsC=c("DateDeath","HospitalBeforeIllDateEnd","DateHospitalPastEnd1","DateHospitalPastEnd2")
  #melt data to get information by hospital (current, before ill, past1, past2)
  pat_melted <- data.table::melt(setDT(pat_gantt), c("CaseID","infectionNosocomial","earlyOnset", "true_DateOnset"), measure=list(colsA, colsB, colsC), value.name=c("Hospital","Start_Date","End_Date"))
  pat_melted <- pat_melted %>% group_by(CaseID) %>% arrange(CaseID)

  pat_melted$Start_Date <- as.Date(pat_melted$Start_Date, format="%d/%m/%Y")
  pat_melted$End_Date <- as.Date(pat_melted$End_Date, format="%d/%m/%Y")

  patient_timelines <- list()
  tooltip <- paste0(pat_melted$Start_Date, " to ", pat_melted$End_Date)

  pat_melted$Start_Date <- as.Date(pat_melted$Start_Date)
  pat_melted$End_Date <- as.Date(pat_melted$End_Date)
  names(pat_melted)[7] <- "Hospitalization_Dates"
  tooltip <- paste0(pat_melted$Hospitalization_Dates, " to ", pat_melted$End_Date, ". Date Onset: ", pat_melted$true_DateOnset)
  pat_melted$time_nosocomial <- paste0(tooltip,",",pat_melted$infectionNosocomial)

  patient_timeline = NULL

  pat_melt_sub <- pat_melted[pat_melted$Hospital == input$noso_hospital_id,]

  if(nrow(pat_melt_sub) == 0){
    stop(safeError("Please select a Hospital with data"))
  }

    ptime <- ggplot(pat_melt_sub, aes(x=Hospitalization_Dates,xend=End_Date,y=paste0(CaseID,sep=""),yend=paste0(CaseID,sep=""),color=earlyOnset, label=time_nosocomial)) +
    labs(title=paste0("Admissions for ",input$noso_hospital_id)) +
    scale_color_manual(values=c("TRUE" = "green", "FALSE" = "red")) +
    geom_segment(size=4) + 
    theme(axis.title.y=element_blank(),legend.position="none")
  ptime = ggplotly(ptime, tooltip=c("time_nosocomial"))
  ptime

#dev.off()
#Order of hospitals in pat_melted: Current hospital, Hospital Before Ill, Hospital Past 1, Hospital Past 2
}

get_nosocomial_plots_by_hospital <- function(input, nscm_data){

  if(is.na(input$noso_hospital_id) || input$noso_hospital_id == ""){
    stop(safeError("Please select a Hospital"))
    
  }
  nscm_data$incubation_period <- input$noso_min_incubation_all #incubation period for Ebola: 2 to 21 days; https://www.who.int/csr/disease/ebola/faq-ebola/en/
  nscm_data$death_avail <- input$noso_death_avail #average Ebola Virus disease (EVD) case fatality is around 50% (Case fatility rates have varied from 25-90% in past outbreaks): https://www.afro.who.int/health-topics/ebola-virus-disease
  nscm_data$bleeding_at_onset <- input$noso_bleeding_at_onset #McElroy, A. (2015). Understanding bleeding in Ebola virus disease. Clinical advances in hematology & oncology: H&O, 13(1), 29.
  nscm_data$diarrhea_at_onset <- input$noso_diarrhea_at_onset #Schieffelin, J. S., Shaffer, J. G., Goba, A., Gbakie, M., Gire, S. K., Colubri, A., ... & Fullah, M. (2014). Clinical illness and outcomes in patients with Ebola in Sierra Leone. New england journal of medicine, 371(22), 2092-2100.

  nscm_data$days_onset_to_death <- input$noso_days_onset_to_death_all #Ruzek, edited by Singh S. Viral Hemorrhagic Fevers, 2014 (book)
  nscm_data$days_onset_to_bleeding <- input$noso_days_onset_to_bleeding_all #http://www.ebolavirusnet.com/signs-a-symptoms.html
  nscm_data$days_onset_to_diarrhea <- input$noso_days_onset_to_diarrhea_all #inferred from above link

  #Hospitalization time for EVD survivors (group 1) and patients who died (group 2) in Sierra Leone
  # Ji, Y. J., Duan, X. Z., Gao, X. D., Li, L., Li, C., Ji, D., Li, W. G., Wang, L. F., Meng, Y. H., Yang, X., Ling, 
  # B. F., Song, X. A., Gu, M. L., Jiang, T., Koroma, S. M., Bangalie, J., . Duan, H. J. (2016). 
  # Clinical presentations and outcomes of patients with Ebola virus disease in Freetown, Sierra Leone. Infectious diseases of poverty, 5(1), 101. doi:10.1186/s40249-016-0195-9

  nscm_data$DateDisch <- as.Date(Sys.Date(), format="%d/%m/%Y") #Date of Discharge for patients that did not die

  #based on calculator_functions.R in ChainChecker
  nscm_data$DateExposure <-  as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$incubation_period
  nscm_data$EndDate <- nscm_data$DateDeath #end date is death date or discharge date for those patients that survived
  nscm_data$EndDate[is.na(nscm_data$EndDate)] = format.Date(nscm_data$DateDisch[is.na(nscm_data$EndDate)],  format="%d/%m/%Y")
  #true date of onset  
  nscm_data$true_DateOnset <- ifelse(nscm_data$death_avail==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_death),
                                    ifelse(nscm_data$bleeding_at_onset==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_bleeding),
                                        ifelse(nscm_data$diarrhea_at_onset==1,(as.Date(nscm_data$DateDeath, format="%d/%m/%Y") - nscm_data$days_onset_to_diarrhea),
                                              as.Date(nscm_data$onset_date, format="%d/%m/%Y"))))
  nscm_data$true_DateOnset <- as.Date(nscm_data$true_DateOnset, origin="1970-01-01")

  #group dataset by hospital
  hosp_gantt <- as.data.frame(nscm_data %>% group_by(HospitalCurrent) %>% select(HospitalCurrent,DateHospitalCurrentAdmit, EndDate, true_DateOnset, infectionNosocomial) %>% arrange(HospitalCurrent))
  names(hosp_gantt) <- c('Hospital','Start_date','End_date','true_DateOnset','Nosocomial_status')
  hosp_gantt$Start_date <- as.Date(hosp_gantt$Start_date, format="%d/%m/%Y")
  hosp_gantt$End_date <- as.Date(hosp_gantt$End_date, format="%d/%m/%Y")
  hosp_gantt$earlyOnset <-  (as.Date(hosp_gantt$Start_date) < as.Date(hosp_gantt$true_DateOnset, format="%d/%m/%Y")) #(as.Date(hosp_gantt$DateOnset, format="%d/%m/%Y") >= as.Date(hosp_gantt$Admission_date)-hosp_gantt$incubation_period)    
  hosp_gantt$infectionNosocomial[hosp_gantt$earlyOnset == TRUE] <- "Nosocomial"
  hosp_gantt$infectionNosocomial[hosp_gantt$earlyOnset != TRUE] <- "Not Nosocomial"

  hosp_gantt$Start_date <- as.Date(hosp_gantt$Start_date)
  hosp_gantt$End_date <- as.Date(hosp_gantt$End_date)

  hosp_gantt = hosp_gantt[hosp_gantt$Hospital == input$noso_hospital_id, ]

  names(hosp_gantt)[2] <- "Hospitalization_Dates"
  tooltip <- paste0(hosp_gantt$Hospitalization_Dates, " to ", hosp_gantt$End_date, ". Date Onset: ", hosp_gantt$true_DateOnset)
  hosp_gantt$time_nosocomial <- paste0(tooltip,",",hosp_gantt$infectionNosocomial)
  hosp_timeline <-
    ggplot(hosp_gantt, aes(x=Hospitalization_Dates,xend=End_date,y=Hospital,yend=Hospital,color=earlyOnset, label=time_nosocomial)) +
    labs(title="Hospital admissions during Ebola outbreak") +
    scale_color_manual(values=c("TRUE" = "green", "FALSE" = "red")) +
    geom_segment(size=4) + 
    theme(axis.title.y=element_blank(),legend.position="none")

#title("Hospital admissions during Ebola outbreak")
#hosp_timeline$tooltip <- hosp_timeline$data$tooltip
  GANTT_hosp <- ggplotly(hosp_timeline, tooltip=c("time_nosocomial"))

  GANTT_hosp

#dev.off()
#Order of hospitals in pat_melted: Current hospital, Hospital Before Ill, Hospital Past 1, Hospital Past 2
}