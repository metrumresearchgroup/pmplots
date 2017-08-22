##' ---
##' title: "Available functions"
##' author: ""
##' date: ""
##' output:
##'   github_document:
##'     toc: true
##' ---

#+ echo=FALSE
knitr::opts_chunk$set(comment='.', message=FALSE, warning=FALSE,
                      fig.path="img/everyfunction--",
                      fig.width=4, fig.height=4)

#+ message=FALSE
library(pmplots)
library(dplyr)

##' # Example data in the package
df <- superset2() %>% filter(EVID==0)

id <- distinct(df, ID, .keep_all=TRUE)

dayx <- defx(breaks = seq(0,168,24))
.what <- "NoDoz (ng/mL)"

##' # `col//title` specification
##'
##' This is a way to specify the column name for source
##' data along with the axis label
col_label("CL//Clearance (L)")


##' # Observed vs predicted
##' ## Observed versus population predicted (`dv_pred`)
dv_pred(df, what=.what)

##' ### Observed versus population predicted - log/log
dv_pred(df,loglog=TRUE, what=.what)

##' ## Observed versus individual predicted (`dv_ipred`)
dv_ipred(df, what=.what)

##' ### Observed versus individual predicted - log/log
dv_ipred(df, loglog=TRUE, what=.what)


##' # Residual plots
##'
##' ## Residuals
##'
##' ### Residuals versus time (`res_time`)
res_time(df)

##' ### Residuals versus time after dose (`res_tad`)
res_tad(df)

##' ### Residuals versus population predicted (`res_pred`)
res_pred(df)

##' ### RES versus continuous covariate (`res_cont`)
res_cont(df, x="WT//Weight (kg)")

##' ### RES by categorical covariate (`res_cat`)
res_cat(df, x="STUDYc//Study type")

##' ### RES QQ plot (`res_q`)
res_q(df)

##' ## Weighted residuals
##'
##' ### Weighted residuals versus time (`wres_time`)
wres_time(df)

##' ### Weighted residuals versus time after dose (`wres_tad`)
wres_tad(df)

##' ### Weighted esiduals versus population predicted (`wres_pred`)
wres_pred(df)

##' ### WRES versus continuous covariate (`wres_cont`)
wres_cont(df, x="WT//Weight (kg)")

##' ### WRES by categorical covariate (`wres_cat`)
wres_cat(df, x="STUDYc//Study type")

##' ### WRES QQ plot (`wres_q`)
wres_q(df)

##' ## Conditional weighted residuals (CWRES)
##'
##' ### CWRES versus time (`cwres_time`)
cwres_time(df)

##' ### CWRES versus time after dose (`cwres_tad`)
cwres_tad(df)

##' ### CWRES versus continuous covariate (`cwres_cont`)
cwres_cont(df, x="WT//Weight (kg)")

##' ### CWRES by categorical covariate (`cwres_cat`)
cwres_cat(df, x="STUDYc//Study type")

##' ### CWRES versus population predicted (`cwres_pred`)
cwres_pred(df)

##' ### CWRES QQ plot (`cwres_q`)
cwres_q(df)

##' # ETA plots
##' ## ETA versus continuous covariates (`eta_cont`)
##'
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- eta_cont(df, x="WT//Weight (kg)",y=etas)
#+ fig.width=6, fig.height=7
mrggdraw(p, ncol=2, arrange=TRUE, script="everyfunction.R")

##' ## ETA by categorical covariates (`eta_cat`)
p <- eta_cat(df, x="STUDYc//Study type", y=etas)
#+ fig.width=6, fig.height=7
mrggdraw(p, ncol=2, arrange=TRUE, script="everyfunction.R")

##' ## ETA histograms (`eta_hist`)
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- eta_hist(df,etas, bins=10)
#+ fig.width=6, fig.height=7
mrggdraw(p, ncol=2, arrange=TRUE, script="everyfunction.R")


##' # DV versus time (`dv_time`)

##' ## Basic plot
dv_time(df, what=.what)

##' ## Faceted
#+ fig.width=6, fig.height=7
dv_time(df, what="NoDoze (ng/mL)") +
  facet_wrap(~DOSE, scales="free_x")

##' ## Colored
#+ fig.width=6, fig.height=7
dv_time(df, what="NoDoze (ng/mL)", col="STUDYc") +
  facet_wrap(~DOSE, scales="free")

##' ## log-Scale
#+ fig.width=6, fig.height=6
dv_time(df, what="NoDoze (ng/mL)", log=TRUE) +
  facet_wrap(~STUDYc)

##' # Data summary
##'
##' ## Continuous variable by categorical variable (`cont_cat`)
cont_cat(df, x="STUDYc//Study name", y="WT//Weight (kg)")

##' ## Split and plot (`split_plot`)
p <- split_plot(df, sp="STUDYc", fun=dv_ipred)
#+ fig.width=6, fig.height=7
mrggdraw(p, arrange=TRUE, script="everyfunction.R")

##' # Plot output (`mrggsave`)
##'
##' ## Saving single plots
p1 <- dv_pred(df)
#+ eval=FALSE
mrggsave(p1, script="everyfunction.R", stem="figure1")

##' ## Save multiple plots to one file
p2 <- dv_ipred(df)
p3 <- cwres_pred(df)
p4 <- cwres_time(df)
#+ eval=FALSE
mrggsave(list(p1,p2,p3,p4), script="everyfunction.R", stem="figure1")

##' ## Arrange multiple plots on a single page
#+ fig.width=6, fig.height=7
mrggdraw(list(p1,p2,p3,p4), arrange=TRUE, script="everyfunction.R")

