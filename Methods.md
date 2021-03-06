---
title: "Methods"
output: 
  html_document:
    df_print:
      "kable"
---




This app was developed to produce estimates of exposure dates for Ebola cases. It can also produce estimates of the date that symptoms developed if we have information on symptoms or date of death.

Authors: 

* Mary Choi developed the calculator logic.

* Katy Gaythorpe developed the app. Feedback and requests for the app should be sent to k.gaythorpe AT imperial.ac.uk


# How to use

The app is broken down into a few pages depending on the number of individuals you wish to look at.

### Timeline

If you are interested in checking one individual, use this page to put in estimates of the date of death (if you have it) or the date of reported onset. You can also fill in the durations although these have default values in established ranges (see section on defaults). When you have filled out the information on the left, the plot and text will update on the right. The plot shows the dates of interest: exposure, onset and death. The text gives thes dates in words. The logic behind the calculation is shown in **calculator logic**.

### Upload

If you wish to check the dates or visualise a number of individuals, you can upload data here. The two buttons at the top give you the option to download templates for the data which you can fill in and then upload. The linelist .csv details characteristics of each person. The contacts .csv details the connections between each linked pair of people; there is also the option to state types of connection. Both templates are a minimal set, meaning you can add more columns to both. However, these templates must retain the same basic column headers and should link to each other, for example if individual EG1 appears in the contacts list, they should have an entry in the linelist with the same identifier.

### Exposure windows for uploaded linelist

This shows the same information as is shown in **timeline** but for the uploaded linelist data. This again depends on a list of specified parameters such as the minimum duration of the incubation period. The plot can be turned into a .png and downloaded (hover over the top right corner of the plot). Additionally, the new linelist, with the estimated onset and exposure window can be downloaded.

### Transmission tree for uploaded linelist and contacts

In this page we visualise two transmission trees. The first is from the data as entered; the second is from the data as adjusted through the calculator. When visualising the tree, it is possible to colour the different nodes by chracteristics from the linelist. Additionally, it is possible to colour the links by characteristics from the contact list. 

As default, the link colour is defined by whether it is **INCONSISTENT**. If highlighted, this means that either the onset date of the infector is after the maximum exposure date of the infectee OR the death date of the infector is before the minimum exposure date of the infectee. The exposure dates in this case are calculated in the same way as shown in the **Exposure windows** tab. The contact list with the **INCONSISTENT** status of each link can be downloaded as a .csv file. The onset date is either as reported (default) or estimated (as shown in the exposure windows), this will affect which links are considered inconsistent. For example, if we think person EG1 may have infected EG2, we can plot their exposure windows as shown below:

![plot of chunk inconsitent_eg](figure/inconsitent_eg-1.png)

In the above example, if we use the onset dates as reported, the link will be flagged as inconsistent. However, if we use the estimated onset dates, the link will not be flagged as EG1 is infectious in the exposure window of EG2.

The transmission tree plot can be turned into a .png and downloaded (hover over the top right corner of the plot). Additionally, the tree may be downloaded as HTML which will retain the options to zoom and hover over for more information.

The tree is visualised using the `epicontacts` package and the chainchecker function `vis_epicontacts_ggplot`.

# Calculator logic

The calculator uses the following steps to estimate onset and exposure dates.

1. If date of death is available. **Estimated onset = date of death - time from onset to death.** Then go to 5. 
    + If date of death is unavailable, go to 2.
2. If individual was bleeding at reported onset. **Estimated onset = reported onset - bleeding correction factor.** Then go to 5. 
    + If they were not bleeding go to 3.
3. If individual had diarrhea at reported onset. **Estimated onset = reported onset - diarrhea correction factor.** Then go to 5. 
    + If they did not have diarrhea go to 4.
4. **Estimated onset  = reported onset**.
5. **Earliest exposure date = estimated onset - maximum incubation period**.
6. **Latest exposure date = estimated onset - minimum incubation period**.

# Defaults

There are a few default values set for the periods of interest. These are taken from the literature and we detail the ranges below.

### Incubation

We take the default maximum incubation period to be 21 days and the minimum to be 4 days.
This has been estimated in numerous studies; selected reading includes, **Eichner, Dowell & Firese, 2011**, **Bull. WHO, 1978**, **Bwaka et al. 1999** and **Ebola virus disease, WHO, 2018**.


### Developing symptoms

There are two correction factors, one for bleeding and one for diarrhea with defaults 6 and 4 days respectively. These denote the average time to develop each symptom after disease onset. There are multiple studies detailing the development of Ebola symptoms; **Valasquez et al. 2015** produced a systematic review on the time from infection to disease.

### Time from onset to death

If an individual does not recover, we set the default time from symptom onset to death at 9 days. This value usually falls within the range of 8 to 10 days with further discussion found in **Valasquez et al. 2015**.

# Definitions

^Bleeding is defined as **bleeding from the nose, bleeding from the mouth/gums, blood in their vomit, blood in their stool and /or bleeding from puncture sites** -at time of reported onset-.
