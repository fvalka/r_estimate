# Investigation and Visualization of the Time-Delays Inherent to the Estimation of the Time-Varying Reproduction Number R(t) as Published for Austria

R shiny app to visualize and investigate the different time delays in the estimates of the time-varying reproduction number, which is currently being published for Austria. 

Available on the web at: [covid19-r.com](https://covid19-r.com)

For further details on the methods used please refer to the current draf of 
the [method paper](https://fvalka.github.io/r_estimate/r_estimate-methods.pdf).

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