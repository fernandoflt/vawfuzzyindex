********************************************************************************

**# FUZZY-SET APPROACH TO MEASURE VIOLENCE AGAINST WOMEN (VAW)

/*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼
Do-file to compute the fuzzy multidimension index of 
violence against women proposed by Bettio, Ticci, and Betti (2020). 
Paper available at: https://doi.org/10.1007/s11205-019-02197-7

DO-FILE AUTHORS: FERNANDO FLORES TAVARES, GIANNI BETTI, FRANCESCA BETTIO, ELISA
TICCI.
CONTACT: fernando.tavares@unisi.it



PLEASE NOTE: 
1.The data used in this do file to exemplify the computation of the index 
are FRA survey microdata (entire sample for 28 European countries). Please refer 
to the questionnaire for the original variable labels 
(https://fra.europa.eu/sites/default/files/fra-violence-against-women-survey
-questionnaire_en.pdf)
2. Before running the do-file, please install the following Stata modules: 
ssc install asgen
ssc install egenmore
3. It is assumed that steps 1 and 2 of the step sequence proposed in Bettio, 
Ticci, and Betti (2020) have already been carried out. Hence this do file starts
from step 3
4. The sampling weights variable is labelled 'wteuover'
*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼*/

*Loading the data

cd "C:\...yourpath..."

use "fra.dta", clear

*Preparing the data under the assumption that a non-answer is equal to no violence

/*Note that:
Psychological abuses are categorical variables 
with the following categories:
1 = never;
2 = sometimes;
3 = Often;
4 = All the time.

Physical and sexual abuses are categorical variables
with the following categories:
1 = never;
2 = once;
3 = 2–5 times;
4 = 6 or more times.
*/

* Global macros
global vars1 e01a e01b e01c e01d e01e e01f e01g e01h ///
 e02a e02b e02c e02d e02e e02f e02g e02h e03ai 
global vars2 e03bi e03ci e03di e03ei e03fi e03gi e03hi e03ii e03ji
global vars3  e04ai e04bi e04ci e04di
 
* Recoding and generating new abuse items variables
foreach group in vars1 vars2 vars3 {
    local i = 1
    foreach x in $`group' {
        * Determining the prefix for the new variable based on the group
        local prefix = "var_psy_"
        if "`group'" == "vars2" local prefix = "var_phy_"
        if "`group'" == "vars3" local prefix = "var_sex_"
		* New variables recoded
        gen `prefix'`i' = `x'
        recode `prefix'`i' (.=0) (-99=0) (1=0) (2=1) (3=2) (4=3) (96=0) ///
		(97=0) (98=0) (99=0)

        local ++i
    }
}


*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼
**# Step 3. Victim status
*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼

* Global macros for psychological, physical, and sexual abuse variables
global psychological var_psy_1 var_psy_2 var_psy_3 var_psy_4 var_psy_5 ///
 var_psy_6 var_psy_7 var_psy_8 var_psy_9 var_psy_10 var_psy_11 var_psy_12 ///
 var_psy_13 var_psy_14 var_psy_15 var_psy_16 var_psy_17
global physical var_phy_1 var_phy_2 var_phy_3 var_phy_4 var_phy_5 ///
var_phy_6 var_phy_7 var_phy_8 var_phy_9
global sexual var_sex_1 var_sex_2 var_sex_3 var_sex_4

* Calculating frequency and membership function for each variable in the lists
foreach list in psychological physical sexual{

****************************************	
* Frequency calculations
****************************************

foreach x in $`list' {
    asgen d_`x'_1 = (`x' == 0), w(wteuover)
    asgen d_`x'_2 = (`x' == 1), w(wteuover)
    asgen d_`x'_3 = (`x' == 2), w(wteuover)
    asgen d_`x'_4 = (`x' == 3), w(wteuover)
}

****************************************
*Membership function in the interval [0,1] 
****************************************

foreach x in $`list'{
	gen mu_`x' = 0 if `x'== 0
	replace mu_`x' = ((d_`x'_1 + d_`x'_2)) if `x'== 1
	replace mu_`x' = ((d_`x'_1 + d_`x'_2 + d_`x'_3)) if `x'== 2
	replace mu_`x' = 1 if `x'== 3
	asgen  mu_mean_`x' =  mu_`x', w(wteuover) 
}

*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼
 **# STEP 4 Calculating severity weights
*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼

****************************************
* Prevalence component calculation (wa)
****************************************

*Option 1 (The log of the inverse of the mean value of the membership function):

foreach x in $`list'{
   gen wa_`x' = log(1 /mu_mean_`x') 
}

* Normalizing the prevalence component
if "`list'" == "sexual" {
    egen wa_mean_`list'= rowmean(wa_var_sex*)
} 
else if "`list'" == "physical" {
    egen wa_mean_`list'= rowmean(wa_var_phy*)
}
else {
    egen wa_mean_`list'= rowmean(wa_var_psy*)
}

foreach x in $`list'{
	gen wa_`x'_`list' = wa_`x'/wa_mean_`list'
	drop wa_`x' 
}
drop wa_mean_*

/*Option 2 (Coefficient of variation):
foreach x in  $`list'{
	*standard deviation
	gen sd_`x' =.
   summarize mu_`x' [w=wteuover], detail 
   replace sd_`x' = r(sd)
   *weight calculation
   gen wa2_`x' = sd_`x'/mu_mean_`x'
}

* Normalizing the prevalence component
  if "`list'" == "sexual" {
		egen wa2_mean_`list'= rowmean(wa2_var_sex*)
    } 
    else if "`list'" == "physical" {
		egen wa2_mean_`list'= rowmean(wa2_var_phy*)
    }
	else {
		egen wa2_mean_`list'= rowmean(wa2_var_psy*)
    }
	
foreach x in $`list'{
	gen wa2_`x'_`list' = wa2_`x'/wa2_mean_`list'
	drop wa2_`x' 
}
drop wa2_mean_*
*/

****************************************
* Correlation component calculation (wb)
****************************************

* Local macros
	if "`list'" == "sexual" {
		local refvars "mu_var_sex_1 mu_var_sex_2 mu_var_sex_3 mu_var_sex_4"
	} 
	else if "`list'" == "physical" {
		local refvars ///
"mu_var_phy_1 mu_var_phy_2 mu_var_phy_3 mu_var_phy_4 mu_var_phy_5 mu_var_phy_6 mu_var_phy_7 mu_var_phy_8 mu_var_phy_9"
}
else {
    local refvars ///
"mu_var_psy_1 mu_var_psy_2 mu_var_psy_3 mu_var_psy_4 mu_var_psy_5 mu_var_psy_6 mu_var_psy_7 mu_var_psy_8 mu_var_psy_9 mu_var_psy_10 mu_var_psy_11 mu_var_psy_12 mu_var_psy_13 mu_var_psy_14 mu_var_psy_15 mu_var_psy_16 mu_var_psy_17"
}

*Calculating correlations 
foreach x in $`list' {
    local totalCorrel = 0

    foreach refvar in `refvars' {
        corr `refvar' mu_`x'
        if !missing(r(rho)) {
            local totalCorrel = `totalCorrel' + r(rho)
        }
    }

    if `totalCorrel' != 0 {
        gen wb_`x' = 1 / `totalCorrel'
    } 
    else {
        gen wb_`x' = .
    }
}

* Normalizing the correlation components
if "`list'" == "sexual" {
    egen wb_mean_`list'= rowmean(wb_var_sex*)
} 
else if "`list'" == "physical" {
    egen wb_mean_`list'= rowmean(wb_var_phy*)
}
else {
    egen wb_mean_`list'= rowmean(wb_var_psy*)
}

foreach x in $`list'{
gen wb_`x'_`list' = wb_`x'/wb_mean_`list'
drop wb_`x' 
}
drop wb_mean_*

****************************************
* Final Weight calculation 
****************************************
* Multiplying the prevalence components and correlation components
foreach x in $`list'{
	gen w_`x' = wa_`x'_`list'*wb_`x'_`list'
}
}

*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼
 **# STEP 5 Type-specific index scores
*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼

****************************************
* Psychological
****************************************
* Local macros
local sum_product_psy = 0
local sum_weights_psy = 0
* Calculating the weighted sum
forval i = 1/17 { 
    local sum_product_psy "`sum_product_psy' + (w_var_psy_`i' * mu_mean_var_psy_`i')"
    local sum_weights_psy "`sum_weights_psy' + w_var_psy_`i'"
}
gen index_psy = (`sum_product_psy') / (`sum_weights_psy')

****************************************
* Physical
****************************************
* Local macros
local sum_product_phy = 0
local sum_weights_phy = 0
* Calculating the weighted sum
forval i = 1/9 {
    local sum_product_phy "`sum_product_phy' + (w_var_phy_`i' * mu_mean_var_phy_`i')"
    local sum_weights_phy "`sum_weights_phy' + w_var_phy_`i'"
}
gen index_phy = (`sum_product_phy') / (`sum_weights_phy')

****************************************
* Sexual
****************************************
* Local macros
local sum_product_sex = 0
local sum_weights_sex = 0
* Calculating the weighted sum
forval i = 1/4 {
    local sum_product_sex "`sum_product_sex' + (w_var_sex_`i' * mu_mean_var_sex_`i')"
    local sum_weights_sex "`sum_weights_sex' + w_var_sex_`i'"
}
gen index_sex = (`sum_product_sex') / (`sum_weights_sex')

*☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼
 **# STEP 6 - Aggregating type-specific index scores into an overall index score
 *☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼
 
*NOTE: THIS IS AN OPTIONAL STEP, SEE ARTICLE

****************************************
* Overall index
****************************************
* Calculating a simple unweighted mean
egen overall_index = rowmean(index_psy index_phy index_sex)   


********************************************************************************
