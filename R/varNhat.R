# Calculate the variance contribution of the detection function
#  to abundance estimates
#' @importFrom dplyr reframe
varNhat <- function(data, model){

  # format the data
  # relies on dplyr's internal representation
  # faff here is to detect if there is an NA grouping and drop it
  # this has all the grouping data in it
  grps <- attr(data, "groups")
  grps$.rows <- NULL
  strat_vars <- colnames(grps)
  # are there any NAs?
  grps <- apply(grps, 1, function(x) any(is.na(x)))
  # get structure
  vardat_str <- attr(data, "groups")[!grps, , drop=FALSE]

  # get the covered area and survey area summaries per strata
  grp_dat <- data %>%
    select("Covered_area", "Area", "Sample.Label", !!strat_vars) %>%
    distinct() %>%
    reframe(Covered_area = sum(.data$Covered_area),
              Area         = .data$Area) %>%
    distinct()

  # join the per-stratum data onto the frame
  data$Covered_area <- NULL
  data$Area <- NULL
  data <- left_join(data, grp_dat, by=strat_vars)

  # function to calculate Nhat
  dhtd <- function(par, data, model){
    # set par
    model$par <- par

    ## calculate Nc
    data$p <- predict(model, newdata=as.data.frame(data), integrate=TRUE,
                      compute=TRUE)$fitted

    res <- data %>%
      mutate(Nc = (.data$size/.data$p)/.data$rate) %>%
      reframe(N = (.data$Area/.data$Covered_area) *
                     sum(.data$Nc, na.rm=TRUE)) %>%
      distinct()

    res$N
  }

  # get variance-covariance matrix for the detection function
  vcov <- solvecov(model$hessian)$inv

  # remove the rows where there were no observations
  data <- data[!is.na(data$object), ]

  # calculate variance
  dm <- DeltaMethod(model$par, dhtd, vcov, sqrt(.Machine$double.eps),
                    model=model, data=data)
  attr(dm, "vardat_str") <- vardat_str

  # fiddle with variance data.frame
  vardat_str$.rows <- NULL
  vardat_str$df_var <- diag(dm$variance)

  # detection function p uncertainty
  ddf_summary <- summary(model)
  vardat_str$p_var <- ddf_summary$average.p.se[1,1]^2
  vardat_str$p_average <- ddf_summary$average.p


  # return the data.frame with the delta method stuff as an attribute
  ret <- vardat_str
  attr(ret, "dm") <- dm

  return(ret)
}
