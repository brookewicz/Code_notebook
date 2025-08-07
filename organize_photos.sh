#!/bin/bash

cd ~/
cd Google\ Drive/My\ Drive/SCTLD_tracking/Belize/Colony\ Photos 

# for loop - loop through each T folder 

for folder in T*/; do
    cd "$folder" 

# loop thru species and 
# make species folders 

    for species in SSID PAST MCAV ORBI DLAB PSTR; do
        mkdir -p "$species"
    done

# move photos to that folder 

    for species in SSID PAST MCAV ORBI DLAB; do 
        mv -n *"$species"* "./$species/"
        # -n avoids accidental overwriting 
    done
    cd ..

done