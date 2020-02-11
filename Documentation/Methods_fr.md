À propos de
===========

Cette application a été développée pour produire des estimations des
dates d’exposition pour les cas d’Ebola. Il peut également produire des
estimations de la date à laquelle les symptômes se sont développés si
nous avons des informations sur les symptômes ou la date de décès.

Auteurs:

-   Mary Choi a développé la logique de la calculatrice.

-   Katy Gaythorpe a développé l’application.

-   Aaron Morris a développé l’analyse de cluster.

-   Miles Stewart a développé la version de l’application VHF.

Les commentaires et les demandes pour l’application doivent être envoyés
à k.gaythorpe AT imperial.ac.uk ou aaron.morris10 AT imperial.ac.uk

Contrôleur de chaîne portable / de bureau à utiliser avec la base de données VHF
--------------------------------------------------------------------------------

Est disponible depuis le référentiel GitHub sur
<a href="https://github.com/imperialebola2018/chainchecker/releases" class="uri">https://github.com/imperialebola2018/chainchecker/releases</a>

Logique de la calculatrice
==========================

La calculatrice utilise les étapes suivantes pour estimer les dates de
début et d’exposition.

1.  Si la date du décès est connue. \*\* Début estimé = date de décès -
    temps écoulé entre le début et la mort. \*\* Passez ensuite à 5.
        + Si la date du décès n’est pas disponible, passez à 2.
2.  Si l’individu saignait au début signalé. \*\* Début estimé = début
    signalé - facteur de correction des saignements. \*\* Passez ensuite
    à 5.     + S’ils ne saignaient pas, passez à 3.
3.  Si la personne a présenté une diarrhée au début de la grossesse.
    \*\* Début estimé = début signalé - facteur de correction de la
    diarrhée. \*\* Passez ensuite à 5.     + S’ils n’ont pas eu de
    diarrhée, passez à 4.
4.  \*\* Début estimé = début signalé \*\*.
5.  \*\* Date d’exposition la plus précoce = début estimé - période
    d’incubation maximale \*\*.
6.  \*\* Dernière date d’exposition = début estimé - période
    d’incubation minimale \*\*.

Incohérences
============

Les liens étiquetés \*\* INCONSISTENT \*\* sont des liens de contact qui
ont été signalés pour une vérification supplémentaire par l’utilisateur
car la fenêtre d’exposition de la personne infectée ne chevauche pas la
période infectieuse de l’infecteur. Cela peut être dû au fait que la
date de début d’infection est postérieure à celle de l’infecté ou
peut-être que l’infecteur est décédé avant que l’infecté ne soit exposé
(bien que cela puisse bien sûr être une transmission funéraire).

Valeurs par défaut
==================

Il existe quelques valeurs par défaut définies pour les périodes
d’intérêt. Ceux-ci sont tirés de la littérature et nous détaillons les
gammes ci-dessous.

### Incubation

Nous considérons que la période d’incubation maximale par défaut est de
21 jours et le minimum de 4 jours. Cela a été estimé dans de nombreuses
études; la lecture sélectionnée comprend, \*\* Eichner, Dowell & Firese,
2011 **, ** Bull. WHO, 1978 **, ** Bwaka et al. 1999 \*\* et **Ebola
virus disease, WHO, 2018**.

### Développer des symptômes

Il existe deux facteurs de correction, un pour les saignements ^ et un
pour la diarrhée avec des défauts de 6 et 4 jours respectivement.
Celles-ci indiquent le temps moyen pour développer chaque symptôme après
le début de la maladie. Il existe plusieurs études détaillant le
développement des symptômes d’Ebola; \*\* Valasquez et al. 2015 \*\* a
produit une revue systématique du temps écoulé entre l’infection et la
maladie.

### Temps entre le début et la mort

Si une personne ne se rétablit pas, nous fixons le délai par défaut
entre l’apparition des symptômes et la mort à 9 jours. Cette valeur se
situe généralement dans l’intervalle de 8 à 10 jours, une discussion
plus approfondie se trouvant dans \*\* Valasquez et al. 2015 \*\*.

Définitions
===========

^ & Les saignements sont définis comme \*\* des saignements de nez, des
saignements de la bouche / des gencives, du sang dans leurs
vomissements, du sang dans leurs selles et / ou des saignements des
sites de ponction \*\* -au moment du début signalé-.
