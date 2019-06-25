About
=====

This app was developed to produce estimates of exposure dates for Ebola
cases. It can also produce estimates of the date that symptoms developed
if we have information on symptoms or date of death.

Authors:

-   Mary Choi developed the calculator logic.

-   Katy Gaythorpe developed the app.

-   Aaron Morris developed the cluster analysis.

-   Miles Stewart developed the VHF app version.

Feedback and requests for the app should be sent to k.gaythorpe AT
imperial.ac.uk

Calculator logic
================

The calculator uses the following steps to estimate onset and exposure
dates.

1.  If date of death is available. **Estimated onset = date of death -
    time from onset to death.** Then go to 5.
    -   If date of death is unavailable, go to 2.
2.  If individual was bleeding at reported onset. **Estimated onset =
    reported onset - bleeding correction factor.** Then go to 5.
    -   If they were not bleeding go to 3.
3.  If individual had diarrhea at reported onset. **Estimated onset =
    reported onset - diarrhea correction factor.** Then go to 5.
    -   If they did not have diarrhea go to 4.
4.  **Estimated onset = reported onset**.
5.  **Earliest exposure date = estimated onset - maximum incubation
    period**.
6.  **Latest exposure date = estimated onset - minimum incubation
    period**.

Defaults
========

There are a few default values set for the periods of interest. These
are taken from the literature and we detail the ranges below.

### Incubation

We take the default maximum incubation period to be 21 days and the
minimum to be 4 days. This has been estimated in numerous studies;
selected reading includes, **Eichner, Dowell & Firese, 2011**, **Bull.
WHO, 1978**, **Bwaka et al. 1999** and **Ebola virus disease, WHO,
2018**.

### Developing symptoms

There are two correction factors, one for bleeding^& and one for
diarrhea with defaults 6 and 4 days respectively. These denote the
average time to develop each symptom after disease onset. There are
multiple studies detailing the development of Ebola symptoms;
**Valasquez et al. 2015** produced a systematic review on the time from
infection to disease.

### Time from onset to death

If an individual does not recover, we set the default time from symptom
onset to death at 9 days. This value usually falls within the range of 8
to 10 days with further discussion found in **Valasquez et al. 2015**.

Definitions
===========

^& Bleeding is defined as **bleeding from the nose, bleeding from the
mouth/gums, blood in their vomit, blood in their stool and /or bleeding
from puncture sites** -at time of reported onset-.
