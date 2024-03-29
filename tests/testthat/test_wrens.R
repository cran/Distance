# wren tests
context("wrens")

test_that("wren 5 minute counts works",{

  ## standard five-minute counts
  data(wren_5min)

  cu_wren_5min <- 1/sqrt(10000)

  w1_df_unif <- ds(wren_5min, transect="point", truncation=110, key="unif",
                   convert_units=cu_wren_5min, order=c(1,2), er_var="P3",
                   optimizer = "R")


  # do the same thing with dht2
  w1_nhat <- dht2(w1_df_unif, flatfile=wren_5min, strat_formula=~Region.Label,
                  convert_units=cu_wren_5min, er_est="P3")

  d_stuff <- w1_df_unif$dht$individuals$D
  expect_equal(attr(w1_nhat,"density")$Density, d_stuff$Estimate, tol=1e-3)
  expect_equal(attr(w1_nhat,"density")$LCI, d_stuff$lcl, tol=1e-3)
  expect_equal(attr(w1_nhat,"density")$UCI, d_stuff$ucl, tol=1e-3)
  expect_equal(attr(w1_nhat,"density")$Density_se, d_stuff$se, tol=1e-3)
  expect_equal(attr(w1_nhat,"density")$Density_CV, d_stuff$cv, tol=1e-3)
})


test_that("wren snapshot works",{
  ## the ‘snapshot’ method
  data(wren_snapshot)
  cu_wren_snapshot <- 1/sqrt(10000)

  w2_df_hr <- ds(wren_snapshot, transect="point", truncation=110, key="hr",
                 adjustment=NULL, convert_units=cu_wren_snapshot, er_var="P3")

  w2_nhat <- dht2(w2_df_hr, flatfile=wren_snapshot, strat_formula=~Region.Label,
                  convert_units=cu_wren_snapshot, er_est="P3")

  d_stuff <- w2_df_hr$dht$individuals$D

  expect_equal(attr(w2_nhat,"density")$n, w2_df_hr$dht$individuals$summary$n)
  expect_equal(attr(w2_nhat,"density")$k, w2_df_hr$dht$individuals$summary$k[1])
  expect_equal(attr(w2_nhat,"density")$Effort, w2_df_hr$dht$individuals$summary$Effort)
  expect_equal(attr(w2_nhat,"density")$Covered_area,
               w2_df_hr$dht$individuals$summary$CoveredArea)
  expect_equal(attr(w2_nhat,"density")$Density, d_stuff$Estimate, tol=1e-3)
  expect_equal(attr(w2_nhat,"density")$LCI, d_stuff$lcl, tol=1e-3)
  expect_equal(attr(w2_nhat,"density")$UCI, d_stuff$ucl, tol=1e-3)
  expect_equal(attr(w2_nhat,"density")$Density_se, d_stuff$se, tol=1e-6)
  expect_equal(attr(w2_nhat,"density")$Density_CV, d_stuff$cv, tol=1e-3)
})



test_that("wren cue count works",{
  ## cue count method
  data(wren_cuecount)
  mult <- unique(wren_cuecount[, c("Cue.rate","Cue.rate.SE")])
  names(mult) <- c("rate", "SE")
  # search time is the effort
  wren_cuecount$Effort <- wren_cuecount$Search.time

  # for comparability with distance, we ignore replicate visits
  wren_cuecount$Sample.Label_old <- wren_cuecount$Sample.Label
  wren_cuecount$Sample.Label <- sub("-\\d+", "", wren_cuecount$Sample.Label)

  cu <- sqrt(0.0001)

  w3_df_hr <- ds(wren_cuecount, transect="point", truncation=92.5,
                 adjustment=NULL, key="hr", er_var="P3", convert_units=cu)

  w3_nhat <- dht2(w3_df_hr, flatfile=wren_cuecount, strat_formula=~Region.Label,
                  multipliers=list(creation=mult), convert_units=cu, er_est="P3")


  expect_equal(w3_nhat$Abundance_se, 8.0002, tol=1e-2)
  expect_equal(attr(w3_nhat,"density")$Density, 1.2123, tol=1e-1)
  expect_equal(attr(w3_nhat,"density")$Density_se, 0.24262, tol=1e-2)
  expect_equal(attr(w3_nhat,"density")$Density_CV, .2001, tol=1e-2)
  expect_equal(attr(w3_nhat,"density")$LCI, 0.82134, tol=1e-2)
  expect_equal(attr(w3_nhat,"density")$UCI, 1.7893, tol=1e-2)

})

#test_that("wren 4 works",{
#  ## line transect data
#  data(wren4)
#
#  cu <- (1/1000)*1/0.01
#  # cribbed the best model
#  w4_df_hn <- ds(wren4, transect="line", truncation=95, key="hn",
#                 adjustment=NULL, convert.units=cu)
#
#
#  result <- ddf(dsmodel = ~mcds(key = "hn", formula = ~1, adj.series="herm", adj.order=c(4,6)),
#                data = w4_df_hn$ddf$data, method = "ds",
#                meta.data = list(width = 95),
#                control=list(initial=list(scale=log(.3532E5), adjust=c(.1184E6, .1534E5)),
#                             nofit=TRUE))
#
#
#  w4_nhat <- dht2(result, flatfile=wren4, strat_formula=~Region.Label,
#                      convert_units=cu)
#  expect_equal(attr(w4_nhat,"density")$Density, 2.1344, tol=1e-1)
#  expect_equal(attr(w4_nhat,"density")$Density_se, 0.21949, tol=1e-2)
#  expect_equal(attr(w4_nhat,"density")$Density_CV, .1028, tol=1e-2)
#  expect_equal(attr(w4_nhat,"density")$LCI, 1.7382, tol=1e-2)
#  expect_equal(attr(w4_nhat,"density")$UCI, 2.6207, tol=1e-2)
#})
