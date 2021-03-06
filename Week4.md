Kruger National Park: Safari Overview
========================================================
author: Marius Peché
date: 12 August 2017
autosize: true


Introduction
========================================================

Welcome to the final project for the __Data Products__ course of the  __Data Science Specialization__.

The course is presented by the __John Hobkins University__ on _Coursera.org_

This project continues on the previous two assignments, displaying overviews of data on the sightings of species gathered during a five day safari in the _Kruger National Park_, South Africa. The data contains the following of each sighting:

 * Species Name and Class
 * Time stamp
 * Location information




Tab 1: Checklist
========================================================

__Checklist__ contains a table identifying the species from the selected class sighted during either the requested day or the entire safari.

 - Selecting a specific Order
    + Only species with that classification will be listed.
    + List of Orders is updated according to the selected Class.
    + Orders not sighted during a requested day will not be offered.
    
    
 - Selecting the "Entire Safari" checkbox
    + Table will display the entire safari, indicating on which day(s) a species was sighted.
    + Using the library _shinyjs_, the date input widget will be rendered inactive.
   
   
Tab 1: Entire safari checklist (code)
========================================================

As an example, suppose the user opted to display the entire safari's sightings of "Large Birds of Prey"


```r
library(dplyr); library(tidyr)
tmp <- knp %>%  filter(Class=="Birds") %>%
  filter(Order=="Large Birds of Prey") %>%
  select(date,Name) %>% unique()
tmp$mark = "*"
tmp <- spread(tmp,date,mark)
tmp[][is.na(tmp)] <- " "
head(tmp, n=2)
```

```
                Name 2015-04-26 2015-04-27 2015-04-28 2015-04-29
1 African Fish Eagle          *          *          *          *
2           Bateleur                     *          *          *
  2015-04-30
1          *
2          *
```

Tab 2 : Most Common Species
========================================================

__Most Common Species__ displays a list of the species of the selected classes that has been sighted most often during the safari, as well as the number of sightings that has been recorded. Species are ordered from most sighted to least sighted.

The number of species to be listed may be selected from a range between 5 to 20 species.

Multiple classes may be selected to be compared against each other.

Note that these values are only sighting events, and not the number of individuals viewed.
