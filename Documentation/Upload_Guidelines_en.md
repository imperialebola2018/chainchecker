Upload guidelines
-----------------

Data in the form of a linelist and contacts list can be uploaded on this
page. Templates can be downloaded containing a simple example which can
be run through the app. Guidelines for adding your own data to these
templates are shown below.

Linelist
--------

The linelist details the information on each individual. This is matched
to the contacts sheet by **id**. The order of the columns does not
matter. **When you have filled in the new data, save as a .csv ready to
upload.**

### Mandatory fields

#### id

This is the **unique** identifier of each individual. It should match
exactly with their entry (if they have one) in the contacts list. For
example, the identifier could have the form “EG1”.

#### reported\_onset\_date

This is the reported date of symptom onset. It should always be entered
in the form **dd/mm/yy**, mm/dd/yy is also accepted.

#### death\_date

This is the reported date of death. It should always be entered in the
form **dd/mm/yy**, mm/dd/yy is also accepted.

### Optional fields

#### bleeding\_at\_reported\_onset

Whether a person was bleeding^ at onset. This should take the value
**TRUE** or **FALSE**; empty cells will be interpreted as **FALSE**.

#### diarrhea\_at\_reported\_onset

Whether a person had diarrhea at onset. This should take the value
**TRUE** or **FALSE**; empty cells will be interpreted as **FALSE**.

#### Other

Any number of extra columns may be added, such as Age or Sex. If dates,
they should have the form **dd/mm/yy**, mm/dd/yy is also accepted.

Contacts
--------

This details the links between people. One line corresponds to one
connection. **from** and **to** should only take **id** that are present
in the linelist and the **id** should be in exactly the same format. The
order of the columns does not matter. **When you have filled in the new
data, save as a .csv ready to upload.**.

### Mandatory fields

#### from

The source of infection. This should take the unique **id** of the
individual.

#### to

The individual who is infected by the source. This should take the
unique **id** of the individual. The app will accept multiple sources of
infection for an individual, just add another row for the new
transmission link.

### Optional fields

#### contact\_of\_typex

Any number of extra columns may be added to describe the **type** of
link, for example **in\_funeral** would indicate the transmission
occurred at a funeral. All additional columns must have entries
**TRUE**, **FALSE** or be left empty; empty cells are interpreted as
**FALSE**. A more specific example is as follows.

<table style="width:65%;">
<colgroup>
<col style="width: 9%" />
<col style="width: 8%" />
<col style="width: 18%" />
<col style="width: 29%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: center;">from</th>
<th style="text-align: center;">to</th>
<th style="text-align: center;">in_funeral</th>
<th style="text-align: center;">in_health_facility</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: center;">EG1</td>
<td style="text-align: center;">EG2</td>
<td style="text-align: center;">TRUE</td>
<td style="text-align: center;">FALSE</td>
</tr>
<tr class="even">
<td style="text-align: center;">EG1</td>
<td style="text-align: center;">EG4</td>
<td style="text-align: center;">FALSE</td>
<td style="text-align: center;">FALSE</td>
</tr>
<tr class="odd">
<td style="text-align: center;">EG2</td>
<td style="text-align: center;">EG5</td>
<td style="text-align: center;">FALSE</td>
<td style="text-align: center;">TRUE</td>
</tr>
<tr class="even">
<td style="text-align: center;">EG3</td>
<td style="text-align: center;">EG6</td>
<td style="text-align: center;">FALSE</td>
<td style="text-align: center;">FALSE</td>
</tr>
</tbody>
</table>

In the above, transmission from **EG1** to **EG2** occurred at a funeral
and transmission from **EG2** to **EG5** occurred in a health facility.
