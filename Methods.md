Introduction
============

This app was developed to produce estimates of exposure dates for Ebola
cases. It can also produce estimates of the date that symptoms developed
if we have information on symptoms or date of death.

Authors:

-   Mary Choi developed the calculator logic.

-   Katy Gaythorpe developed the app.

How to use
==========

The app is broken down into a few pages depending on the number of
individuals you wish to look at.

### Timeline

If you are interested in checking one individual, use this page to put
in estimates of the date of death (if you have it) or the date of
reported onset. You can also fill in the durations although these have
default values in established ranges (see section on defaults). When you
have filled out the information on the left, the plot and text will
update on the right. The plot shows the dates of interest: exposure,
onset and death. The text gives thes dates in words. The logic behind
the calculation is shown in `calculator logic`.

### Upload

If you wish to check the dates or visualise a number of individuals, you
can upload data here. The two buttons at the top give you the option to
download templates for the data which you can fill in then upload. The
linelist .csv details characteristics of each person. The contacts .csv
details the connections between each linked pair of people; there is
also the option to state types of connection. Both templates are a
minimal set, meaning you can add more columns to both. However, these
templates must retain the same basic column headers and should link to
each other, for example if individual EG1 appears in the contacts list,
they should have an entry in the linelist with the same identifier.

### Exposure windows for uploaded linelist

This shows the same information as is shown in `timeline` but for the
uploaded linelist data. This again depends on a list of specified
parameters such as the minimum duration of the incubation period. The
plot can be turned into a .png and downloaded. Additionally, the new
linelist, with the estimated onset and exposure window can be
downloaded.

### Transmission tree for uploaded linelist and contacts

In this page we visualise two transmission trees. The first is from the
data as entered; the second is from the data as adjusted through the
calculator. When visualising the tree, it is possible to colour the
different nodes by chracteristics from the linelist. Additionally, it is
possible to colour the vertical lines by characteristics from the
contact list.

The horiontal lines are highlighted <span style="color:red">red</span>
if the onset times of the two linked individuals are in the wrong order.

The horiontal lines are highlighted <span
style="color:orange">orange</span> if the onset times of the two linked
individuals are too close together, as determined by the minimum serial
interval.

Calculator logic
================

The calculator uses the following steps to estimate onset and exposure
dates.

1.  If date of death is available.
    `Estimated onset = date of death - time from onset to death.` Then
    go to 5.
    -   If date of death is unavailable, go to 2.
2.  If individual was bleeding at reported onset.
    `Estimated onset = reported onset - bleeding correction factor.`
    Then go to 5.
    -   If they were not bleeding go to 3.
3.  If individual had diarrhea at reported onset.
    `Estimated onset = reported onset - diarrhea correction factor.`
    Then go to 5.
    -   If they did not have diarrhea go to 4.
4.  `Estimated onset  = reported onset`.
5.  `Earliest exposure date = estimated onset - maximum incubation period`.
6.  `Latest exposure date = estimated onset - minimum incubation period`.

Definitions
===========

\*Bleeding is defined as
`bleeding from the nose, bleeding from the mouth/gums, blood in their vomit, blood in their stool and /or bleeding from puncture sites`.
