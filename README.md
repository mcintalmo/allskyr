allskyr
=======

Author: Alex McIntosh
---------------------
Based on programs from the College of Saint Benedict  
and Saint John's University
---------------------------

Welcome to the allskyr project. This project includes R functions and an 
accompanying Shiny app for determining the radiant of meteor showers detected
by [Rob Weryk's ASGARD](http://meteor.uwo.ca/~weryk/asgard/) software. In 
particular, this project looks at data collected by the Saint John's University 
(in Collegeville, MN) allsky camera.

---
**Running the GUI:**

  The easiest way to access the GUI alone is to run the following command 
  within R:

        runGitHub("allskyr", "Mcintalmo")

---
GUI Options:
  
    **Shower Name:** Name of the meteor shower you would like to see.  
    *Default:* Quadrantids
    
    **Year:** Year of the meteor shower you would like to see.  
    *Default: 2009*
    
    **Remove Outliers:** Check this if you would like to remove outlying events 
    from the plot. WANRING: Can take up to 30 seconds to generate a large
    meteor shower.  
    *Default: Unchecked*
    
  **IQR:** Strictness of outlier detection. Smaller numbers remove more events.  
    *Default: 1.5*
    
  **Number of Bins:** Number of bins to be used for determining the shower 
    radiant.  
    *Default: 25*
    
  **Generate Plot:** Generate the plot given the input provided.  
  
New Class:
  **Event** - Holds information pulled from event files.  

Included Functions:

  BASH: 
    txt_rsync.sh - Syncs SJU events to the current working directory.
  
  R:
    **ensure.package** - Ripped straight from Introduction to Data Science by 
      Jeffrey M. Stanton
    
    **update.events**
    
    **load.events**
    
    **update.showers**
    
    **load.showers**
    
    **norm**
    
    **normalize**
    
    **cross.product**
    
    **equatorial.to.cartesian**
    
    **radiant**
    
    **numeric.date**
    
    **find.events**
    
    **find.shower**
    
    **shower.radiant**
    
    **iqr.threshold**
    
    **mode2d**
    
    **center.on.point**
    
    **outlier.trim**
    
    **event.name**
    
    **event.file**
    
    **event.version**
    
    **event.num_fr**
    
    **event.date**
    
    **event.unix**
    
    **event.ntp**
    
    **event.seq0**
    
    **event.mul**
    
    **event.site**
    
    **event.latlon**
    
    **event.text**
    
    **event.plate**
    
    **event.geom**
    
    **event.reject**
    
    **event.fr**
    
    **event.time**
    
    **event.sum**
    
    **event.seq**
    
    **event.cx**
    
    **event.cy**
    
    **event.th**
    
    **event.phi**
    
    **event.lsp**
    
    **event.mag**
    
    **event.flag**
    
    **event.alt**
    
    **event.az**
    
    **event.julian.date**
    
    **event.ra**
    
    **event.dec**
    
    **event.ha**
    
    **event.equatorial**
    
    **event.start.equatorial**
    
    **event.end.equatorial**
    
    **event.year**
    
    **event.month**
    
    **event.day**
    
    **event.hour**
    
    **event.minute**
    
    **event.second**
    
    **event.calendar.date**
    
    **event.clock.time**
    
    **event.get**

    

      
    
  
