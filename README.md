[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) [![R-CMD-check](https://github.com/CWFC-CCFB/QcPSPIntervals/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CWFC-CCFB/QcPSPIntervals/actions/workflows/R-CMD-check.yaml)

The QcPSPIntervals package
=======================

## Introduction

The QcPSPIntervals package contains a clean version of Quebec PSP data.

It provides additional data.frame objects in which the tree measurements are
reported by nonoverlapping intervals. These kind of data can be useful for 
growth model fitting.

The original data was produced and made freely available online
at https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui by Direction des inventaires forestiers
of Ministère des Ressources naturelles et des Forêts du Québec. 

## License

This package is licensed under the LGPL-3. 

## How to use it

The package can be installed using the remotes package:

~~~R
library(remotes)
remotes::install_github("CWFC-CCFB/QcPSPIntervals")
~~~

To get access to the tables of the database:

~~~R
QcPSPIntervals::restoreQcPSPIntervalsData()
~~~

