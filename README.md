# Glucose-Lowering Medication Use for Diabetes: National Evidence from 62 Low- and Middle-Income Countries

This repository contains replication code for Teufel, Felix et al. "Glucose-Lowering Medication Use for Diabetes: National Evidence from 62 Low- and Middle-Income Countries", which is currently under review at Nature Communications. 

## System requirements

This code has been written in Stata SE version 15.1 for macOS Ventura 13.0.1 and not tested in other software versions or operating systems. The code does not require any non-standard hardware.

## Content

The following do-files can be found in the 'code' folder of this repository:
- HPACC_dia_meds_prep.do: Cleaning the data and generating variables
- HPACC_dia_meds_subset.do: Sub-setting the data and re-scaling survey weights
- HPACC_dia_meds_analysis.do: Creating main tables and figures
- HPACC_dia_meds_appendix.do: Creating supplementary tables and figures
  
## Installation, demo and instructions

No software beyond Stata needs to be installed for running the code. The 2 analytic do-files ("HPACC_dia_meds_analysis.do" and "HPACC_dia_meds_appendix.do") can be run on a small simulated dataset that is provided in the 'data' folder of this repository. The generated output will show similar graphs as in the paper, though estimates will likely differ in the simulated dataset. The expected run time of the two do-files on a desktop computer with 16GB RAM is around XX-XX minutes.

## Contact

I am Felix Teufel, PhD Student at Emory University. You can reach me at:
felixteufel@emory.edu

## License

This project is licensed under the MIT License.


