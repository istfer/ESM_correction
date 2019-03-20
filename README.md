# ESM_correction
PalEON tree-ring fading record correction using Empirical Succession Mapping (ESM)

1. First step is to extract ESM phase space data from FIA database : fia_extract.R 
2. You can plot backward ESM vectors : plotting_ESM4PalEON.R
3. Fit allometries and prepare the AB lookup table : fit_allometries.R
4. Correct PalEON tree-ring only model using the ESM phase space: esm_fading_record_correction.R

allometry_check.R function sort of validates that the way I use the PEcAn.allometry package to calculate ab agrees with Paleon model estimates.
