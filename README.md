<img width="468" height="630" alt="image" src="https://github.com/user-attachments/assets/b8655d60-06ad-4777-bff1-962e24c3edc0" />Project name: Cost-effective threshold price for alternative infant and neonatal rotavirus vaccines: a dual-country evaluation 
Authors: Xiao Li,  Ernest O. Asare, Catherine G. C. Wenger, Joke Bilcke, Philippe Beutels, Virginia E. Pitzer

The objective of this rotavirus cost-effectiveness modelling project is to identify the conditions under which switching to RV3-BB is optimal in Malawi and Ghana. We built a decision tree on the validated, country-calibrated transmission models to conduct a full incremental cost-effectiveness analysis comparing all relevant vaccination strategies in both countries.

This software contains the model implementation and input to conduct a health economic analysis and is distributed under the terms of the GNU GENERAL PUBLIC LICENSE Version 3 licence. If you use (parts of) this modelling project, please cite: Li X, Asare EO, Kwon J, Wenger CGC, Armah GE, Cunliffe NA, Jere KC, Bilcke J, Beutels P, Pitzer VE. Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation. medRxiv [Preprint]. 2026 May 15:2026.05.12.26353029. doi: 10.64898/2026.05.12.26353029. PMID: 42180371; PMCID: PMC13193085.

 # README: Cost-effective threshold price for alternative infant and neonatal rotavirus vaccines in Malawi and Ghana 


## Where to start?

We organised our R-project upon **main.R**, which is the workbench to coordinate the model input, the number of runs, random seeds,... and all output. Please make sure that your [**Working Directory**](https://stat.ethz.ch/R-manual/R-devel/library/base/html/getwd.html) is specified as the location of this R file, since all links in de code are relative to the location of this script.

The main script includes an option to run the model in a so-called "debug" mode. This boolean (`bool_debug_mode`) can be toggled on or off at the start of `main.R`. 

## Where are my results stored?

During the execution of the model (by using the main script), an **output** directory will be created. Each run will create a separate sub-directory to store all results.

## How to specify intervention options and countries?

The main script uses a csv file in the "config" directory, of which each row specifies one intervention program to be included in the cost-utility analysis for a specific country and scenario. Simulation results are grouped per "config_tag" to create figures and tables.

## Main directory

<p align="left">

| Directory or file name | Content                                                                                                                                                 |
|------------------|------------------------------------------------------|
| config                 | Directory with model configurations by country, scenario and rotavirus vaccines characteristics (coverage, price per dose, delivery cost) |
| functions              | Directory with R help functions                                                                                                                         |
| input                  | Directory with input data to run the analysis                                                                                                           |
| README.pdf             | Readme file with the project introduction and reference                                                                                                 |
| main.R             | Main script to run the cost-effectiveness analysis                                                                                                      |
| LICENCE.txt            | GNU GENERAL PUBLIC LICENSE Version 3                                                                                                                    |

</p>

All code is tested with R Version 4.3.1 on MacOS 13.7

\newpage


## Coding tips
- **Parallel worker messages**  
  Warning messages such as *"closing unused connection..."* or *"Error in serialize(data, node$config)"* are harmless.  
  They occur when parallel workers shut down after completing their tasks.  
  If these messages persist or interrupt execution, simply restart the parallel backend.

- **Reproducibility across systems**  
  Even with a fixed random seed, model results can vary between systems due to differences in the operating system, R version, or package versions.  
  For strict reproducibility, record and share your session information (use `sessionInfo()`).

- **Parallel execution with `smd_start_cluster()`**  
  The function `smd_start_cluster()` automatically detects the number of available CPU cores  
  (e.g., 28 on the cluster, typically 8 on a laptop).  
  As a result, iterations within a `foreach` loop are executed in **parallel**, not sequentially.  

  Compared to a standard `for` loop, `foreach` provides built-in functionality to automatically combine results,  
  which is controlled by the `.combine` argument (for example, `.combine = rbind`).



## Published work with this framework

- Li X, Asare EO, Kwon J, Wenger CGC, Armah GE, Cunliffe NA, Jere KC, Bilcke J, Beutels P, Pitzer VE. Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation. medRxiv [Preprint]. 2026 May 15:2026.05.12.26353029. doi: 10.64898/2026.05.12.26353029. PMID: 42180371; PMCID: PMC13193085.

## Other references for the dynamic models (not included in this project)
- Asare, E. O., Al-Mamun, M. A., Armah, G. E., Lopman, B. A. & Pitzer, V. E. Impact of dosing schedules on performance of rotavirus vaccines in Ghana. Sci Adv 10, eadn4176 (2024). https://doi.org/10.1126/sciadv.adn4176
- 	Asare, E. O., Lartey, B. L., Armah, G. E. & Pitzer, V. E. Quantifying the impact of switching from Rotarix to Rotavac rotavirus vaccine in Ghana. J Infect Dis (2026). https://doi.org/10.1093/infdis/jiag003
- 	Asare, E. O. et al. Modeling of rotavirus transmission dynamics and impact of vaccination in Ghana. Vaccine 38, 4820-4828 (2020). https://doi.org/10.1016/j.vaccine.2020.05.057
<img width="468" height="157" alt="image" src="https://github.com/user-attachments/assets/a05e3f5b-f06d-444c-aaa3-92a48b0c52c3" />
- Pitzer, V. E. et al. Evaluating strategies to improve rotavirus vaccine impact during the second year of life in Malawi. Sci Transl Med 11 (2019). https://doi.org/10.1126/scitranslmed.aav6419
-	Pitzer, V. E. et al. Impact of rotavirus vaccination in Malawi from 2012 to 2022 compared to model predictions. NPJ Vaccines 9, 227 (2024). <img width="468" height="86" alt="image" src="https://github.com/user-attachments/assets/fca7c66d-d303-438c-9265-4c1e0ed9558e" />


------------------------------------------------------------------------

Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP

Contact: Xiao Li <xiao.li@gmail.com>

Last update: 2026-07-23

