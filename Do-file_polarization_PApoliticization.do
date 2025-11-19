
//Importing dataset
use "V-Dem-CY-Full+Others-v15.dta", clear

//Aligning sustantive directions of the variables
gen inv_v2dlcommon= -1 * v2dlcommon
gen inv_v2dlconslt= -1 * v2dlconslt
gen inv_v2dlengage= -1 * v2dlengage
gen inv_v2dlcountr= -1 * v2dlcountr
gen inv_v2dlreason= -1 * v2dlreason
gen inv_v2smpolsoc= -1 * v2smpolsoc
gen inv_v2smpolhate= -1 * v2smpolhate

//Aligning sustantive directions of the variables, so that higher values correspond to higher politicization
gen inv_v2stcritrecadm= -1 * v2stcritrecadm 
gen inv_v2clrspct = -1 * v2clrspct

//Generating indexes
gen polarization_index = (inv_v2dlcommon + inv_v2dlconslt + inv_v2dlengage + inv_v2dlcountr + inv_v2dlreason + inv_v2smpolsoc + inv_v2smpolhate) / 7
gen PA_politicization_index = (inv_v2stcritrecadm + inv_v2clrspct) / 2

//Check correlation between polarization index and political_polarization
correlate polarization_index v2cacamps

//Logging GDP to use as control variable
gen log_gdppc = log(e_gdppc)

//Generating lagged polarization_index
gen lag_polarization_index = L2.polarization_index

//Setting panel data time-unit variables
tsset country_id year

//Running RE with both polarization options + controls
	*Pooling all regime types
xtreg PA_politicization_index lag_polarization_index log_gdppc e_peaveduc v2x_polyarchy, re cluster(country_id)
eststo RE1_regindif

xtreg PA_politicization_index L2.v2cacamps log_gdppc e_peaveduc v2x_polyarchy, re cluster(country_id)
eststo RE2_regindif

	**Dividing democracies/autocracies
xtreg PA_politicization_index lag_polarization_index c.lag_polarization_index##i.e_boix_regime log_gdppc e_peaveduc v2x_polyarchy, re cluster(country_id)
eststo RE1

xtreg PA_politicization_index L2.v2cacamps c.L2.v2cacamps##i.e_boix_regime log_gdppc e_peaveduc v2x_polyarchy, re cluster(country_id)
eststo RE2

//Running FE with both polarization options + controls
	*Pooling all regime types
xtreg PA_politicization_index lag_polarization_index log_gdppc e_peaveduc v2x_polyarchy, fe cluster(country_id)
eststo FE1_regindif

xtreg PA_politicization_index L2.v2cacamps log_gdppc e_peaveduc v2x_polyarchy, fe cluster(country_id)
eststo FE2_regindif

	**Dividing democracies/autocracies
xtreg PA_politicization_index lag_polarization_index c.lag_polarization_index##i.e_boix_regime log_gdppc e_peaveduc v2x_polyarchy, fe cluster(country_id)
eststo FE1

xtreg PA_politicization_index L2.v2cacamps c.L2.v2cacamps##i.e_boix_regime log_gdppc e_peaveduc v2x_polyarchy, fe cluster(country_id)
eststo FE2

xtreg PA_politicization_index lag_polarization_index c.lag_polarization_index##i.e_boix_regime log_gdppc e_peaveduc, fe cluster(country_id)
eststo FE3

xtreg PA_politicization_index L2.v2cacamps c.L2.v2cacamps##i.e_boix_regime log_gdppc e_peaveduc, fe cluster(country_id)
eststo FE4
		
//Running GMM wiht both polarization options + controls
	*Pooling all regime types
xtabond PA_politicization_index lag_polarization_index log_gdppc e_peaveduc v2x_polyarchy, robust
eststo GMM1_regindif

xtabond PA_politicization_index L2.v2cacamps log_gdppc e_peaveduc v2x_polyarchy, robust
eststo GMM2_regindif
	
	**Dividing democracies/autocracies
	//Define the interaction
gen interactv1 = lag_polarization_index * e_boix_regime
gen interactv2 = L2.v2cacamps * e_boix_regime

	//Running the models
xtabond2 PA_politicization_index lag_polarization_index interactv1 log_gdppc e_peaveduc v2x_polyarchy, gmm(lag_polarization_index interactv1, collapse) iv(log_gdppc e_peaveduc v2x_polyarchy) robust
eststo GMM1
scalar hansen_p = e(jstatp)
estadd scalar hansen_p = hansen_p
eststo GMM1
	
xtabond PA_politicization_index lag_polarization_index interactv1 log_gdppc e_peaveduc v2x_polyarchy, robust
estat sargan
scalar sargan_stat = r(chisq)
scalar sargan_pval = r(p)
estat abond
scalar ar1_p = r(p1)
scalar ar2_p = r(p2)
eststo GMM2

//Running Hausman test between RE and FE
hausman RE1 FE1
hausman RE2 FE2

//Printing the models
esttab FE1 FE2 using Polarization&PAPoliticizationv1.rtf, replace se r2 ar2 label scalars(rmse r2_w r2_o r2_b)
esttab FE1 FE2 GMM1 GMM2 using Polarization&PAPoliticizationv2.rtf, replace se r2 ar2 label scalars(rmse r2_w r2_o r2_b hansen_p sargan_stat sargan_pval ar1_p ar2_p)
esttab using RE1_regindif RE2_regindif FE1_regindif FE2_regindif GMM1_regindif GMM2_regindif using Polarization&PAPoliticization_regindif.rtf, replace se r2 ar2  label scalars(rmse r2_w r2_o r2_b)

//Testing Administrative Traditions
	//Creating the dataset
use "V-Dem-CY-Full+Others-v15.dta", clear
merge m:1 country_name using "admin_traditions.dta", keepusing(admin_tradition)
keep if _merge == 3
drop _merge
save "V-Dem_with_AdminTradition.dta", replace

	//Generating the dummy
encode admin_tradition, gen(admin_trad_num)

	//Running the RE models
xtreg PA_politicization_index c.lag_polarization_index##i.admin_trad_num log_gdppc e_peaveduc v2x_polyarchy,  re cluster(country_id)
eststo AdminRE1

xtreg PA_politicization_index c.L2.v2cacamps##i.admin_trad_num log_gdppc e_peaveduc v2x_polyarchy,  re cluster(country_id)
eststo AdminRE2

//Printing the models
esttab AdminRE1 AdminRE2 using FundamentalsofDem_Admin.rtf, replace se r2 ar2  label scalars(rmse r2_w r2_o r2_b)
