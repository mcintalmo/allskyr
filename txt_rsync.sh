#!/bin/bash

source=ammcintosh@nx.csbsju.edu:/net/plasma/allsky/events
destination=/home/alex/CSBSJU
options=-azP #-a includes all files, -z compresses the files, 
             #-P shows progress and allows pausing
exclude=--include '*/' --exclude '*' --prune-empty-dirs #scan subdirectories, but
                                                        #  ignore files not
                                                        #  explicitly included.
include=--include '*.txt' #include all text files
echo source:$source
echo destination:$destination
echo options:$options
echo exclude:$exclude
echo include:$include
echo rsync $options $exclude $include $source $destination
rsync $options $exclude $include $source $destination