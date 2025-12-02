# Chinook Salmon Escapement in the Chena and Salcha Rivers and Coho Salmon Escapement in the Delta Clearwater River

Salmon enumeration projects in the Tanana River drainage will be conducted by the Alaska Department of Fish and
Game (ADF&G) on the Chena, Salcha, and Delta Clearwater rivers. The primary purpose of these projects is to
estimate spawning escapement abundance and determine whether or not the established escapement goals for these 3
rivers are met. Chinook salmon *Oncorhynchus tshawytscha* escapement for the Chena and Salcha rivers will be
estimated using tower-counting and sonar techniques and coho salmon *O. kisutch* escapement in the Delta
Clearwater River will be estimated by a visual boat survey at peak escapement.

## Operational Plan

A recent operational plan (2019-2023) is available [here](https://www.adfg.alaska.gov/FedAidPDFs/ROP.SF.3F.2019.03.pdf).
Field and analysis methods have not undergone any meaningful changes since this 
document, as of 2025.

## Overview

All code and data archived here pertain to estimating the escapement of Chinook 
and chum salmon in the Chena and Salcha Rivers, which are tributaries to the Tanana
River.  The two species ran concurrently, with the Chinook run arriving and peaking
a few days or weeks earlier than the chum run, but with considerable overlap in 
timing.  The runs for each species were consistently similar in timing between 
the two rivers.  All field activity was timed in order to capture the entirety of
the Chinook run in each season, and the majority of the chum run.

Daily passage has been estimated (2014-2025) using a suite of sequence of
enumeration methods, depending on data availability and data quality.  These were:

* **Expanded visual (tower) counts**: During periods of adequate water clarity,
technicians counted fish of both species from counting towers during 20-minute 
intervals in each hour.  Counts were then mathematically expanded using formulae
for point estimates and standard errors for each day.

* **Sonar apportionment**: Sonars were also operational at the same sites as the 
counting towers, in order to provide an alternate means of enumeration when water
clarity was inadequate for visual counts.  The lengths of all salmon in the sonar
record were digitally measured, and a Bayesian mixture model was employed to 
estimate the species of each sonar target (i.e. Chinook or chum salmon).  In addition
to the sonar data, the mixture model relied on two additional inputs: the distribution
of lengths associated with each species / sex / river (in terms of mean, SE, and SD),
and the respective timing of the runs for each species.

* **Running average interpolation**: Small data gaps were filled using simple running
average interpolation methods.

* **Hierarchical run-timing model**: Larger data gaps were filled using a hierarchical
Bayesian model that fit the functional form of the run-timing curve for all years, 
with curve hyperparameters estimated hierarchically.

## Folder Contents

Folders are included for analysis years **2024** and **2025**.  Each year contains
the following subfolders (or similar):

### R

* **1_ChenaSalchaYEAR.R**: This script reads and processes the sonar data, and estimates
values for the length and differential run-timing priors to be incorporated in the
sonar mixture model.  Respective sets of priors (length and run-timing) are each
estimated using hierarchical Bayesian models, which are each run in this script.
All results of this script are saved as .Rdata files, which are loaded as appropriate
in subsequent scripts.

* **2_CSmixmodelYEAR.R**: This script defines and runs the sonar mixture model, and
saves the simplified result as a .Rdata file.

* **3_CSvisualdataYEAR.R**: This script reads the hourly visual (tower) 
counts, and compares the totals to sonar.  Much of the script is devoted to a 
comparison of the time periods with complete visual counts and sonar records
("apples to apples"" comparison), which also compares both species combined (visual)
to the total number of sonar targets.  Theoretically, these should match!

* **4_CSestimatesYEAR.R**: Finally, this script puts it all together.  It re-reads
the visual counts and computes the visual expansion formulae for both visual and 
sonar estimates as necessary, checks the visual expansion results against the spreadsheet,
then conducts the running average and hierarchical run-timing interpolations as
needed.  It then compiles the set of "final" daily estimates and writes these to
external .csv files.

### raw_data

A master spreadsheet (.xlsx) for each river (Chena and Salcha) is typically compiled
by the project biologist.  This contains all hourly visual counts and expansion 
formulae, as well as historical runs, etc.

These live on local computers and are .gitignore'd, and all data sheets that are
relevant to R code are extracted as .csv files.

### flat_data

* **AllCarcassData_uptoYEAR.csv**: All carcass samples for both rivers, which is
used for length priors in the sonar mixture model.  This is manually appended every year.

* **RIVER_historic_YEAR.csv**: This should be the NON-EXPANDED daily counts for 
both species, which is used in the differential run-timing model that creates
inputs for the sonar mixture model.  This is manually appended every year.

* **RIVERSPECIES_expansionsheet.csv**: This is a copy of the visual expansion sheet
from the master .xlsx file for each river / species, and is imported as a check of
the visual expansion results.

* **RIVERSPECIES_historic_runtiming.csv**: This is a table of EXPANDED daily estimates
for all years, and is used as an input to the hierarchical run-timing model.
This is manually appended every year.

* **RIVERSPECIES_vis20min.csv**: This is a table of raw 20-minute visual counts, 
and is redundant with the first columns of RIVERSPECIES_expansionsheet.csv.  Oops.

* **RIVERClarity_vis20min.csv**: This is a table of the associated water clarity
values, and is structured the same as RIVERSPECIES_vis20min.csv.

* **RELEVANT SONAR FILES** are likely to have different names each year, but will
be obvious.  These have been batch-processed to compile all the relevant .txt files
into one (or multiple) master .csv file(s).

### Rdata

* **CSpriorsYEAR.Rdata**: This file contains the length and differential run-timing
priors that are used in the sonar mixture model.

* **hier_runtimingYEAR.Rdata**: This file contains output from the hierarchical 
run-timing model used for final interpolations.

* **mixmodelYEAR.Rdata**: This file contains output from the sonar mixture model.

* **sonardataYEAR.Rdata**: This file contains processed sonar data, which is used
as an input for the sonar mixture model.

* **vis_YEAR.Rdata**: This file contains processed visual count data, which is used 
for final estimates.

### output

* **ChenaSalcha_SummaryTotals_YEAR.csv**: A summary table of all daily estimates & SE's,
as well as what percentage of the total run (in terms of numbers of fish, not days) 
estimated to have been enumerated by each method.

* **ChenaSalcha_RIVERSPECIES_YEAR.csv**: A summary table of daily estimates & SE's
and enumeration method used, as well as estimates from all methods.

* **ChenaSalcha_RIVERSPECIES_vcov_YEAR.csv**: A variance-covariance matrix of
daily estimates.  Since a nontrivial amount of covariance is present between daily 
estimates from the sonar mixture model and particularly the hierarchical run-timing
models, the variances of the full-season totals will not be the simple sum of the
daily variances.