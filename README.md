[![DOI](https://zenodo.org/badge/259440541.svg)](https://zenodo.org/badge/latestdoi/259440541)
# Investigation and Visualization of the Time-Delays Inherent to the Estimation of the Time-Varying Reproduction Number R<sub>t</sub> as Published for Austria

R shiny app to visualize and investigate the different time delays in the estimates of the time-varying reproduction number, which is currently being published for Austria. 

The estimation of R<sub>t</sub> is based upon the 
[EpiEstim](https://github.com/mrc-ide/EpiEstim) R package, which uses the method described in 
[Cori, A., Ferguson, N. M., Fraser, C. & Cauchemez, S. A New Framework and Software to Estimate Time-Varying Reproduction Numbers During Epidemics. American Journal of Epidemiology 178, 1505–1512 (2013)](https://academic.oup.com/aje/article/178/9/1505/89262).

The time-delay estimation is based upon published estimates and our own work. For further details please refer to the current draft of our method paper:
[Valka, F. & Schuler, C. Estimation and Interactive Visualization of the Time-Varying Reproduction Number Rt and the Time-Delay from Infection to Estimation. (2020)](https://fvalka.github.io/r_estimate/r_estimate-methods.pdf).

## Example Output
![Example Output](https://covid19-r.com/social-media-preview.png)

Available on the web at: [covid19-r.com](https://covid19-r.com)

## Dataset Output
The Rt estimates are now also available as CSV files in the GitHub repo here in the folder:
[r_estimate/data/csv](r_estimate/data/csv)

for each time window, tau, using the naming schema `{state}/r-estimate-{tau}.csv` with AT being the countrywide estimate. 

## Build
The Shiny app can simply be built using the Dockerimage provided in this repo. 

```
docker build . -t r-estimate
```

## Run Shiny Server

Everything needed to run the shiny server and appis already prepared in the Dockerimage:

```
docker run -d -p 3838:3838 r-estimate
```

## Updating Estimates

Updates are run by running `estimates.R`. 

```
RScript estimates.R
```

## Combined Time Delays for Specific Assumed Underlying Distributions

Rds-files of the time-delay ECDFs for Austria can be found in the `r_estimate/data` folder. 

For obtaining your own estimates you can use the RScripts provided in 
`time_delay_ecdf`.

## Data Sources

### Case data for Austria
[Bundesministerium für Soziales, Gesundheit, Pflege und Konsumentenschutz](https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html)
aggregated by the Complexity Science Hub Vienna and published on 
[GitHub](https://github.com/osaukh/dashcoch-AT/blob/master/data_AT/covid19_cases_austria.csv).

The data as published at the 15:00 CET deadline is used in this dataset.

### AGES R<sub>t</sub> estimate
TU Graz Rt,τ estimate AGES - Österreichische Agentur für Gesundheit und Ernährungssicherheit GmbH
available online here: [Epidemiologische Parameter des COVID19 Ausbruchs, Update 10.07.2020, Österreich, 2020](https://www.ages.at/en/wissen-aktuell/publikationen/epidemiologische-parameter-des-covid19-ausbruchs-oesterreich-2020/)

### Public holidays 
Public holiday data from [nager-date](https://github.com/nager/Nager.Date) is used for a different fill 
colour on public holidays. 
