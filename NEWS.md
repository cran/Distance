# Distance 2.0.1

* Fixes issue with print dht2 when multipliers are a data.frame (Issue #179)
* Fixes bug when including a uniform with no adjustment terms in the summarize_ds_models function (Issue #180)
* dht2 can now deal with strata where there are no observations (Issue #194)
* Users can alternatively pass a list of models to summarize_ds_models rather than passing them individually. (Issue #149)
* Truncation distances greater than the largest cutpoint value for binned data are no longer permitted as these cause fitting issues. (Issue #175)
* print.dht_result now displays estimates for groups as well as individuals by default when group size is present. (Issue #178)
* Issues a warning when truncation is provided as a character but without the explicit % sign. (Issue #166) 
* Fix sample ordering issue in dht2 for the S1, S2 and O1 encounter rate variance estimators. (Issue #174)

Enhancements

* Warnings and documentation clarification regarding ER variance estimation when there is only a single transect. (Issue #192 and mrds Issue #115)
* Code is more robust to the different ways of defining a binned analysis and documentation has been clarified. (Issue #144)

# Distance 2.0.0

* Requires mrds 3.0.0. mrds is called by ds for fitting detection functions. In mrds there has been a change of optimizer used for CDS detection functions - a constraint solver slsqp now used. This removes the need for external optimizer MCDS.exe in most cases. Other minor changes to optimization have been implemented to improve reliability (see NEWS file of mrds for more info).
* New argument mono_method added so that the previous constraint solver (solnp) can still be used. MCDS.exe is also still available if needed.

# Distance 1.0.9

* Changed the default encounter rate estimator for point transect surveys from P3 to P2. (Issue #138)
* Fixed bug which produced NA's when stratum names came after 'Total' in the alphabet. (Issue #158)
* Added additional documentation explaining the adjustment term options when covariates are in the model. (Issue #156)
* Fixed dht bootstrap to work when distbegin and distend are supplied but not distance. (Issue #147)
* Added a warning for the dht bootstrap when Sample.Label values are not unique across all strata. (Issue #157)
* Distance 1.0.9 depends on mrds >= 2.3.0 due to re-named documentation page links.

# Distance 1.0.8

* Support for using MCDS.exe from Distance for Windows to run analyses. You can now download MCDS.exe using mrds::download_MCDS_dot_exe run analyses using this engine, rather (or in tandem with) the usual R optimizers provided in mrds. ds will pick the best (by likelihood) and return that. See ?ds and ?"mcds-dot-exe" for more details.

# Distance 1.0.7

* dht2 now requires the object field in flatfile formatted data. The following vignette shows how to add an object field if your data does not have already have one: https://distancesampling.org/Distance/articles/web-only/CTDS/camera-distill.html
* Fix bugs when a uniform is fitted with no adjustments
* Fixed error in dht2 when binned data used distend / distbegin

# Distance 1.0.6

* Fix bug in auto binning data when using flatfile (#116)
* convert.units in bootdht() was not properly implemented in previous release, fixed (#122)
* fix bug in detection function variance estimation (#125)
* fix bug in bootstrap where columns needed to be character (thanks to Nick Wilkinson for finding this)
* fix bug in covered area calculation for dht2, this fixes incorrect density estimate under left truncation (#135)
* experimental support for multiple detection functions in dht2, joint work T.J. Clark-Wolf, funded by Environment Canada. Note that now the object field is required in data supplied to dht2.

# Distance 1.0.5

* To improve consistency in functions and arguments in the package, some functions and arguments have changed from . separation to _. An error is now thrown when the "old" arguments/functions using . are used. This error will be removed in Distance 1.0.6.
  * create.bins() -> create_bins()
  * bootdht():
    * convert.units -> convert_units
  * ds():
    * dht.group       -> dht_group
    * region.table    -> region_table
    * sample.table    -> sample_table
    * obs.table       -> obs_table
    * convert.units   -> convert_units
    * er.var          -> er_var
    * debug.level     -> debug_level
    * initial.values  -> initial_values
    * max.adjustments -> max_adjustments
* fix bootdht issue when cluster results were requests (#103)
* improve flatfile documentation (thanks to Maggie Blake for pointing this out)
* fixed bug in cutpoint calculations in create.bins (#108)
* order argument to ds() is now only used to specify order, to fix a given number of adjustments use the new argument nadj (see ?ds for more info)
* fix bug where polynomial adjustments started at the wrong order (2 rather than 4)

# Distance 1.0.4

* fix bootdht issue where the arguments for ds() were not found
* bootdht_Nhat_summarize now reports the stratum labels as well as their abundance estimates for ease of use
* add function QAIC to calculate QAIC for overdispersed data, such as that from camera trap distance sampling
* bootdht is now less verbose when cores>1
* bootdht now accepts multipliers
* bootdht multipliers can now be specified using the activity package, see ?make_activity_fun
* fix issue in Hermite adjustment order calculation when length(order)>1
* set.seed can now be used with bootdht in parallel to obtain reproducible bootstrap results

# Distance 1.0.3

* fix bug in dht2 where warnings were thrown if object column was not in the flatfile (https://github.com/DistanceDevelopment/Distance/issues/83)
* removed silent=TRUE in try() around model fitting to enable users to get error messages from mrds during fitting. Old behaviour can be recovered using quiet=TRUE argument to ds()
* better handling of when models fail to converge during AIC adjustment term selection
* documentation now in rmarkdown format
* fix issue #85 when species was used in the detection function and for post-stratification. Thanks to jason-airst for reporting the bug.
* fix dht2 bug where stratification="replicate" variance estimation was 0 due to order of operations
* fix dht2 bug where stratification="effort_sum" encounter rate variance estimation, due to incorrect grouping of transects into strata. Thanks to Samantha Ball and Jamie McKaughan for reporting this issue.
* bootdht can now run in parallel via the foreach/doParallel packages, see the cores argument.
* multiple multipliers can now be specified, for example to have different creation/decay rates for each stratum
* new argument er.method to ds(), allows further refinement of encounter rate variance calculation. Default 2 is as before, use er.method=1 to get results which match Distance for Windows.
* fix issues with Satterthwaite degrees of freedom calculations when geographical stratification was used with clustered observations
* Sample fraction may now be specified as a data.frame if fractions are different for each transect
* Fix various bugs in dht2 when stratification="replicate", thanks to Sam Ball and Jamie McKaughan for reporting issues and testing.

# Distance 1.0.2

* ds.gof is now deprecated for goodness-of-fit testing. gof_ds is now preferred.
* add_df_covar_line (actually located in mrds) can now plot probability density functioins for point transects
* bootdht can now use the progress package if installed to give an estimated time remaining for bootstraps (option progress_bar="progress"). Alternatively no progress bar can be shown with progress_bar="none".

# Distance 1.0.1

* fix bug in dht2 when object IDs were not specified in flatfile formatted data
* fix bugs in bootdht where the function crashed if all models failed to fit and when the hessian couldn't be computed
* better checking of data$observer, thanks to Martin Biuw for pointing this out
* fix bug in dht2 where the covered area was calculated incorrectly when left truncation was used for point transects
* add example data for camera trap distance sampling, see ?DuikerCameraTrap for more information
* Stratum area column (Area) is no longer required by ds(). If it is omitted density estimates are returned.
* Fix bug when dht2 is used with pre-binned data. Thanks to Delphine Ducros for reporting this bug.
* Fix to dht2 bugs when Innes et al estimator is used for encounter rate variance estimation
* fix bootdht issue where convert.units argument was not handled properly

# Distance 1.0.0

* call now saved in the model object as `$call`
* Added lots of example data sets
* new abundance estimation via dht2! Handles more complex situations.
* bootstrap variance estimation via bootdht
* for more examples see https://distancesampling.org/resources/vignettes.html

# Distance 0.9.8

* Includes reference and citation for paper on 'Distance Sampling in R'.
* AIC now works for multiple models at once (as it does for other model classes) thanks to Tiago Marques and Len Thomas for this suggestion.
* Added examples to create.bins, ds.gof, gof_ds, summarize_ds_models, logLik.dsmodel and AIC.dsmodel. Thanks to a reviewer of our Journal of Statistical Software paper.
* Parameters from previous fit are used as starting values for the next fit when AIC is used to select adjustments
* when distbegin and distend were specified in the data but distance wasn't, checkdata() threw an error. checkdata() now generates the distance column at the midpoint. Thanks to Tom for spotting this.
* new argument to ds(), max.adjustments gives the maximum number of adjustment terms to add to the model when doing AIC term selection. Thanks to Oscar Dewhurst for the suggestion.

# Distance 0.9.7

* summarize_ds_models now will only compare models that are allowed by AIC (all binning and truncation must be the same). Thanks to Carolin Tröger and Eric Rextad for highlighting this issue.
* If there are numerical issues that cause NAs in the Hessian, ds() will not try to run dht() to estimate abundance (as it will fail), instead throws a message and returns only the detection function. Thanks to Steve Ahlswede for bringing this to our attention.

# Distance 0.9.6

* Coefficients are called coefficients (not a mixture of coefficients and parameters) in summary() results
* Added gof_ds() for easy access to goodness of fit testing and q-q plotting
* Checking of truncation distance was checking via is.double rather than is.numeric. Thanks to Tiago Marques for spotting this!
* Functions AIC() and logLik() now exist for quick extraction of AIC and log-likelihood values. Thanks to Tiago Marques for this suggestion.
* Added amakihi (point transect) data
* add extra documentation for objects in obs.table, thanks to Olivier Devineau for spotting this

# Distance 0.9.5

* Truncation by percentage now works when there are missing distances (i.e. when we are using flatfile). Thanks to Len Thomas for pointing out this bug.

# Distance 0.9.4

* Object ID uniqueness stopped abundance estimation from working (since NA IDs were "not unique").
* Check that areas are consistently entered. This was problematic when areas were not entered identically for each region, but unique was used to extract the region table. Thanks to Katy Echave for finding this bug!
* Monotonicity constraints were not applied during automated model selection. Thanks to Tiago Marques for spotting this.
* AIC selection of adjustment terms goes up to 5 terms by default, as in DISTANCE. Thanks to Eric Rexstad for suggesting this.

# Distance 0.9.3

* Updated tests to work with new unique object ID code.
* Liberally sprinkled tests with suppressMessages()

# Distance 0.9.2

* Now warning when columns are correctly named but not in the correct case. Thanks to Richard Borthwick for reporting this bug.
* Now checks that object IDs are unique. Thanks to Ricardo Lima & Francisco Azevedo for highlighting this issue.

# Distance 0.9.1

* Models with both covariates and adjustment terms can actually be specified -- this was not fully implemented in previous version.
* ds() now tells the user the models which is returned (rather than previously fitted model)
* links to mrds documentation on optimisation issues

# Distance 0.9

* Flat file support example, see ?flatfile
* New data set: simulated minke whale data, see ?minke and ?flatfile for an example analysis
* Models with both covariates and adjustment terms can be specified.
* Default left truncation is now 0, default right truncation is now the largest observed distance or furthest bin end.

# Distance 0.8.1

* another fix to binning (redundant code/inconsistent definition between docs and code). (Thanks to Jason Roberts for finding this.)
* binning would fail if there were NA distances, which might occur when using the simplified data tables
* check implemented to ensure that samples have consistent (i.e. the same) effort (Eric Rexstad found this bug)
* clarification that stratification only occurs at the abundance/density estimation stage (dht), rather than at the detection function modelling stage (thanks to Filipe Dias for this suggestion)
* Setting order=0 is equivalent to adjustment=NULL to specify a detection function without adjustments. (Eric Rexstad brought this to my attention.)

# Distance 0.8.0

* new simplified table data format (see ?ds)
* bug in binning from cutpoints (thanks to Colin Beale for finding this)
* removed percentage truncation for binned data, as it doesn't really make sense

# Distance 0.7.4

* new initial values argument

# Distance 0.7.3

* remove annoying crash when mrds failed to fit a model
* NB the optimiser underlying mrds (optimx) has changed, update both of these packages to avoid issues.

# Distance 0.7.2

* message tells the user the model that was selected

# Distance 0.7.1

* debugging options
* bug fixes (see github for further details)
* automatic generation of adjustments did not generate any for poly/herm.

# Distance 0.7

* "width" is now default for scaling
