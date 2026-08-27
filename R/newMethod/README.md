# Usage

## step 1 - data download and preparation

Run 00_run.R up to "step 01"

## step 2 - extract training data

Run 00_run.R up to "step 02" which creates a "DT.all.parquet" file containing a large matrix used for model training

## step 3 - training and testing XGBoost ML 

Run 00_run.R up to "step 03" which checks that previous steps are done and uses the "DT.all.parquet" file containing a large matrix for model training. Uses k-fold cross-validation avoiding spatial autocorrelation through dividing in stripes according to latitude of 0.1° latitude width.

## step 4 - prediction 

Run 00_run.R up to "step 04" which checks that previous steps are done and creates a final map with predicted highest-probability Scott&Burgan fuel model class and the probability layer called "confidence layer" respectively in folders "CEFuelMapPre" and "CEFuelMapPreConfidence".

Also the second-most-probably class is mapped to another layer in a folders called respectively "CEFuelMapPre2" and "CEFuelMapPreConfidence2". This helps in understanding how reliable is the class assigned is by using the difference between highest-probability and second-higher-probability.

## step 5 - consensus via disambiguation

Run 00_run.R up to "step 05" or if you have alread have the following:

- a folder with CLC+ layer tiles
- a folder with CLC+ Confidence Layer tiles
- a folder with a Scott&Burgan classes layer tiles
- a folder with a Scott&Burgan Confidence layer tiles

then you can just download the following files:

- 00_globals.R
- 00_functions.R
- 05_step_validation_and_consensus.R

and run the last R file. It will check and apply consensus rules.

# References

> Kurtchartt et al. 202X - A novel hybrid approach to fuel mapping in Central Europe based on remote sensing foundation model embeddings.

# Acknowledgements 

This work is supported by the Wildfire CE Interreg Project, grant number CE0200934.

![](images/clipboard-259128068.png)
