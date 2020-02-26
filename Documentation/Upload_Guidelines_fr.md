Directives de téléchargement
----------------------------

Les données sous forme de linelist et de liste de contacts peuvent être
téléchargées sur cette page. Des modèles peuvent être téléchargés
contenant un exemple simple qui peut être exécuté via l’application. Les
directives pour ajouter vos propres données à ces modèles sont indiquées
ci-dessous.

Linelist
--------

La linelist détaille les informations sur chaque individu. Ceci est
associé à la feuille de contacts par **id**. L’ordre des colonnes n’a
pas d’importance. **Lorsque vous avez rempli les nouvelles données,
enregistrez-les au format .csv prêt à être téléchargé.**

### Champs obligatoires

#### id

Il s’agit de l’identifiant **unique** de chaque individu. Il doit
correspondre exactement à leur entrée (s’ils en ont une) dans la liste
des contacts. Par exemple, l’identifiant pourrait avoir la forme “EG1”.

#### reported\_onset\_date

Il s’agit de la date signalée de l’apparition des symptômes. Elle doit
toujours être entrée sous la forme **jj / mm / aa**, mm / jj / aa est
également accepté.

#### death\_date

Il s’agit de la date de décès signalée. Elle doit toujours être entré
sous la forme **jj / mm / aa**, mm / jj / aa est également accepté.

### Champs facultatifs

#### bleeding\_at\_reported\_onset

Si une personne saignait au début. Cela devrait prendre la valeur
**TRUE** ou **FALSE**; les cellules vides seront interprétées comme
**FALSE**.

#### diarrhea\_at\_reported\_onset

Si une personne a eu une diarrhée au début. Cela devrait prendre la
valeur **TRUE** ou **FALSE**; les cellules vides seront interprétées
comme **FALSE**.

#### Autre

N’importe quel nombre de colonnes supplémentaires peuvent être ajoutées,
telles que l’âge ou le sexe. Les dates doivent avoir la forme **jj / mm
/ aa**, mm / jj / aa est également acceptée.

Contacts
--------

Cela détaille les liens entre les personnes. Une ligne correspond à une
connexion. **from** et **to** ne doivent predre que la valeur de la
colonne **id** presente dans la linelist et **id** doit garder la meme
format. L’ordre des colonnes n’a pas d’importance. **Lorsque vous avez
rempli les nouvelles données, enregistrez-les au format .csv prêt à
télécharger.**.

### Champs obligatoires

#### from

La source de l’infection. Cela devrait prendre l’unique **id** de
l’individu.

#### to

L’individu infecté par la source. Cela devrait prendre l’unique **id**
de l’individu. L’application accepte plusieurs sources d’infection pour
un individu, il suffit d’ajouter une autre ligne pour le nouveau lien de
transmission.

### Champs facultatifs

#### contact\_of\_typex

N’importe quel nombre de colonnes supplémentaires peut être ajouté pour
décrire le **type** de lien, par exemple **in\_funeral** indiquerait que
la transmission s’est produite lors d’un enterrement. Toutes les
colonnes supplémentaires doivent avoir des entrées **TRUE**, **FALSE**
ou être laissées vides; les cellules vides sont interprétées comme
**FALSE**. Un exemple plus spécifique est le suivant.

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

Dans cet example, la transmission de **EG1** à **EG2** s’est produite
lors d’un enterrement et la transmission de **EG2** à **EG5** s’est
produite dans un établissement de santé.
