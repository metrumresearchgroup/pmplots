##' ---
##' title: "Example"
##' author: ""
##' date: ""
##' output:
##'   github_document:
##'     toc: true
##' ---

#+ echo=FALSE
knitr::opts_chunk$set(comment='.', message=FALSE, warning=FALSE,
                      fig.path="img/test--",
                      fig.width=4, fig.height=4)

#+ message=FALSE
library(pmplots)
library(dplyr)

##' # Exammple data in the package
obs <- superset2() %>% filter(EVID==0)

id <- distinct(obs, ID, .keep_all=TRUE)

dayx <- defx(breaks = seq(0,168,24))

##' # Observed versus population predicted
dv_pred(obs)

##' # Observed versus population predicted by renal function
dv_pred(obs) + facet_wrap(~RF)

##' # Observed versus population predicted by study (y-axis varies by plot)
p <- split_plot(obs, sp="STUDYc", fun=dv_pred, what="NoDoze (ng/ml)")

#+ fig.width=6, fig.height=7
mrggdraw(p, ncol=2, script="test.R", arrange=TRUE)

##' # Residuals versus population predictions
res_pred(obs)

##' # Conditional weighted residuals versus population predictions
cwres_pred(obs)

##' Conditional weighted residuals versus population predictions with only
##' unusual points labeled with ID


##' # Residuals and conditional weighted residuals versus time
cwres_time(obs)
res_time(obs)

##' # Conditional weighted residuals versus time after dose
cwres_tad(obs)

##' # Boxplot of ETAs versus categorical covariates
etal <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- eta_cat(id,
             x="STUDYc//Study",
             y=etal)

#+ fig.width=6, fig.height=6
mrggdraw(p, ncol=2, arrange=TRUE,script="test.R")


##' # Scatterplots of ETAs versus continuous covariates
p <- eta_cont(id,
              x="WT//Weight (kg)",
              y=etal)
#+ fig.width=6, fig.height=6
mrggdraw(p, ncol=2, arrange=TRUE,script="test.R")


##' # Boxplot of conditional weighted residuals versus categorical covariates
cwres_cat(obs, x="STUDYc//Study")

##' # Scatterplots of conditional weighted residuals versus continuous covariates
cwres_cont(obs, "WT//Weight (kg)")

##' # Scatterplot matrix of ETAs
cols <- paste0("ETA", 1:3)
labs <- paste0("ETA-", c("CL", "V2", "KA"))
GGally::ggpairs(id[,cols],columnLabels=labs)

##' # Quantile-quantile plot of conditional weighted residuals
cwres_q(obs)



