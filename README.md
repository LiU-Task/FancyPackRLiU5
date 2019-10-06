# FancyPackRLiU5

Package on ThenMap API 

## Description
*ICU* is a RC class which reads map infomation from ThenMap API according to user's requirement and show the ma of specified entity, based on which a simple Shiny application was made. To avoid calling many libraries, a totally hand-written function and process is presented in the package.

However, there is some inherent limitations of this package. For example, some countries' map would be shown in a "strange" shape, such as China with id of 507 and Sweden with 455 (from data modules) are not included/found in the geo modules etc. But the R code should be proven OK, after many rounds of hand tests.  

## Fields
*data*, dataset to choose and use

*date*, a specified date when the entity's map would be drawn

*guo*, a list of entities id, name, geography coordinates

## Methods
*visa()* is designed to show the map of specified entity at special date

*rawData()* is designed to read and re-structure raw data from ThenMap API, and prepare the data form for *visa()* method




Pass status of Travis building:


name<-"Jun Li"
liuid<-"junli559"