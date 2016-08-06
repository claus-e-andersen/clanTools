clanTools
=========

This R package contains tools that are useful for general data analysis and reporting of results.
Installation instructions are given at the end of the page.

clan.install:
List of recommended R packages to install.

clanTools:
Lists all functions available in this package and version information.

coefficients.ca & lm.extract:
This function helps get easy access to the uncertainties associated with the fit.

create.latex.table which:
This function facilitates the generation of data to be used in the tabular environment of the 
latex (i.e. the type-setting system called Latex). It is also used in the txtplot function (see the
clanLattice package).

create.date.string:
Create a nice date string (like: Aug. 10, 2016).

first.element, last.element, most.common.element: 
Find the first, last or the most common element(s) in a single vector

replacechar & substitute.char: 
Replace a character in a string with a new character

leading.zeros: 
Add zeros when numbers occur in filenames and such

leading.zeros.to.fit:
Add zeros when numbers occur in filenames and such

extract.first.number: 
Extract the first numbers from each individual string element in a vector.  

extract.given.number:
Extract the N'th numbers from each individual string element in a vector.  

round.resolution:
Round off to given resolution. 

round.ca:
Round off to given number of decimals 

trim.whitespace:
Remove white space (blanks etc.) from string.

leading.blanks:
Add leading blanks. This is to align data in Latex tables.

wrline:
Write a double line. This function can be used to separate report output.

dayno.clock:
Convert from wall-clock time to fraction of day.

dayno.calc:
Calculate the dayno relative to some reference.
Dates and wall-clock times are used as input. The output is in number of days.

dayno.clock.reversed:
Convert from  fraction of day to wall-clock time.

thermistor.degC & thermistor.ohm:
Conversion for NTC (e.g. 2 kohm) thermistors (Risoe parameterization)

workflow.ca:
Template for graphical output using pdf, ps or png format.
This function includes a recommended wrapper for priducing plots
on screen or to files.


  
To install this package do the following:

(1) First get the devtools package, if you do not already have it:

install.packages("devtools")

library(devtools)



(2) Then get the clanTools package from github:

install_github("clanTools","claus-e-andersen")

library(clanTools)


(3) To get a list of functions in the library, just call:

help(package=clanTools)

or

library(help=clanTools)

