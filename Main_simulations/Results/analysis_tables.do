** Cleaning simulation datasets 
cd "G:\BACKUP\Chris\Project_3\Main_simulations\Results"

** Collate results for beta prior first 
foreach num of numlist 1/144{
    import delimited results_`num', clear
    save "Analysis\results_`num'.dta", replace
}


use "Analysis\results_1",clear

foreach num of numlist 2/144{
    append using "Analysis\results_`num'"
}

** Drop alphas and do this separately later 
drop if prior == "Alpha"


// Basic checks 
// Performance measures 
summ bias, d
assert bias != .

summ coverage, d
assert coverage != .

summ mse, d 
assert mse != .

summ relbias, d
assert relbias != .



** Get rid of N/As and convert to values 
replace bias_se = "" if bias_se == "NA"
replace relbias_se = "" if relbias_se == "NA"
replace coverage_se = "" if coverage_se == "NA"
replace mse_se = "" if mse_se == "NA"
replace postprob = "" if postprob == "NA"
replace postprob_se = "" if postprob_se == "NA"
replace postsup = "" if postsup == "NA"
replace postsup_se = "" if postsup_se == "NA"
replace stopping_se = "" if stopping_se == "NA"

destring bias_se, replace 
destring relbias_se, replace 
destring coverage_se, replace 
destring mse_se, replace 
destring postprob, replace 
destring postprob_se, replace 
destring postsup, replace 
destring postsup_se, replace 
destring stopping_se, replace 

replace postsup = postsup * 100
replace stopping = stopping * 100

// MCSE 
summ bias_se, d

summ coverage_se, d

summ mse_se, d 

summ relbias_se, d

summ postprob_se, d 

summ postsup_se, d 

summ stopping_se, d 


** Also check the other strings 
summ postprob, d 
summ postsup, d 

//Check other variables are in range 
tab model 
assert model != ""

tab category
assert category != .

replace effectsize = "Small [OR = 1.10]" if effectsize == "Weak [OR = 1.10]"
tab effectsize

tab controlprob //These should be equal numbers
assert controlprob != ""

tab sampsize 
assert inlist(sampsize,100,500)

tab design
assert design != ""

tab prior
assert prior != ""

summ stopping ,d 

//Row headers - don't need these 
drop v1 

sort effectsize category model controlprob design sampsize

order effectsize category model controlprob design prior sampsize

gen coveragepct = coverage*100
drop coverage 
rename coveragepct coverage 
order coverage, after(bias_se)

*Where there should not be duplicates 
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se 

replace relbias = relbias*100 
export excel using "Analysis\Graphs\dataforgraph_beta.xlsx", replace firstrow(var) keepcellfmt


** ALPHA
** Collate results for alpha prior 
foreach num of numlist 1/144{
    import delimited results_`num', clear
    save "Analysis\results_`num'.dta", replace
}


use "Analysis\results_1",clear

foreach num of numlist 2/144{
    append using "Analysis\results_`num'"
}

** Drop alphas and do this separately later 
drop if prior == "Beta"


// Basic checks 
// Performance measures 
summ bias, d
assert bias != .

summ coverage, d
assert coverage != .

summ mse, d 
assert mse != .

summ relbias, d
assert relbias != .



** Get rid of N/As and convert to values 
replace bias_se = "" if bias_se == "NA"
replace relbias_se = "" if relbias_se == "NA"
replace coverage_se = "" if coverage_se == "NA"
replace mse_se = "" if mse_se == "NA"
replace postprob = "" if postprob == "NA"
replace postprob_se = "" if postprob_se == "NA"
replace postsup = "" if postsup == "NA"
replace postsup_se = "" if postsup_se == "NA"
replace stopping_se = "" if stopping_se == "NA"

destring bias_se, replace 
destring relbias_se, replace 
destring coverage_se, replace 
destring mse_se, replace 
destring postprob, replace 
destring postprob_se, replace 
destring postsup, replace 
destring postsup_se, replace 
destring stopping_se, replace 

replace postsup = postsup * 100
replace stopping = stopping * 100

// MCSE 
summ bias_se, d

summ coverage_se, d

summ mse_se, d 

summ relbias_se, d

summ postprob_se, d 

summ postsup_se, d 

summ stopping_se, d 


** Also check the other strings 
summ postprob, d 
summ postsup, d 

//Check other variables are in range 
tab model 
assert model != ""

tab category
assert category != .

replace effectsize = "Small [OR = 1.10]" if effectsize == "Weak [OR = 1.10]"
tab effectsize

tab controlprob //These should be equal numbers
assert controlprob != ""

tab sampsize 
assert inlist(sampsize,100,500)

tab design
assert design != ""

tab prior
assert prior != ""

summ stopping ,d 

//Row headers - don't need these 
drop v1 

sort effectsize category model controlprob design sampsize

order effectsize category model controlprob design prior sampsize

gen coveragepct = coverage*100
drop coverage 
rename coveragepct coverage 
order coverage, after(bias_se)

*Where there should not be duplicates 
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se 

replace relbias = relbias*100 
export excel using "Analysis\Graphs\dataforgraph_alpha.xlsx", replace firstrow(var) keepcellfmt


** SENSITIVITY ANALYSIS 
foreach num of numlist 78/80{
    import delimited results_`num'_sens, clear
    save "Analysis\results_`num'_sens.dta", replace
}

foreach num of numlist 87/88{
    import delimited results_`num'_sens, clear
    save "Analysis\results_`num'_sens.dta", replace
}

foreach num of numlist 95/103{
    import delimited results_`num'_sens, clear
    save "Analysis\results_`num'_sens.dta", replace
}

foreach num of numlist 110/111{
    import delimited results_`num'_sens, clear
    save "Analysis\results_`num'_sens.dta", replace
}

foreach num of numlist 115/124{
    import delimited results_`num'_sens, clear
    save "Analysis\results_`num'_sens.dta", replace
}

foreach num of numlist 127/129{
    import delimited results_`num'_sens, clear
    save "Analysis\results_`num'_sens.dta", replace
}

foreach num of numlist 131/144{
    import delimited results_`num'_sens, clear
    save "Analysis\results_`num'_sens.dta", replace
}

use "Analysis\results_100_sens.dta", clear 

foreach var of varlist bias bias_se relbias relbias_se coverage coverage_se mse mse_se postprob postprob_se postsup postsup_se{
	replace `var' = "" if `var' == "NA" & v1 != 6
	destring `var', replace
}
save "Analysis\results_100_sens.dta", replace 

use "Analysis\results_103_sens.dta", clear 

foreach var of varlist bias bias_se relbias relbias_se coverage coverage_se mse mse_se postprob postprob_se postsup postsup_se{
	replace `var' = "" if `var' == "NA" & v1 != 6
	destring `var', replace
}
save "Analysis\results_103_sens.dta", replace 


use "Analysis\results_116_sens.dta", clear 

foreach var of varlist bias bias_se relbias relbias_se coverage coverage_se mse mse_se postprob postprob_se postsup postsup_se{
	replace `var' = "" if `var' == "NA" & v1 != 6
	destring `var', replace
}
save "Analysis\results_116_sens.dta", replace 


use "Analysis\results_134_sens.dta", clear 

foreach var of varlist bias bias_se relbias relbias_se coverage coverage_se mse mse_se postprob postprob_se postsup postsup_se stopping {
	replace `var' = "" if `var' == "NA" & v1 != 6
	destring `var', replace
}
save "Analysis\results_134_sens.dta", replace 

use "Analysis\results_141_sens.dta", clear 

foreach var of varlist bias bias_se relbias relbias_se coverage coverage_se mse mse_se postprob postprob_se postsup postsup_se stopping {
	replace `var' = "" if `var' == "NA" & v1 != 6
	destring `var', replace
}
save "Analysis\results_141_sens.dta", replace 


*100, 103 , 116, 134, 141


** Import data 
use "Analysis\results_78_sens",clear

foreach num of numlist 79/80{
    append using "Analysis\results_`num'_sens" 
}

foreach num of numlist 87/88{
    append using "Analysis\results_`num'_sens" 
}

foreach num of numlist 95/103{
    append using "Analysis\results_`num'_sens"
}

foreach num of numlist 110/111{
    append using "Analysis\results_`num'_sens"
}

foreach num of numlist 115/124{
    append using "Analysis\results_`num'_sens"
}


foreach num of numlist 127/129{
    append using "Analysis\results_`num'_sens" 
}

foreach num of numlist 131/144{
    append using "Analysis\results_`num'_sens"  
}



** Drop alphas and do this separately later 
drop if prior == "Beta"


// Basic checks 
// Performance measures 
summ bias, d
assert bias != .

summ coverage, d
assert coverage != .

summ mse, d 
assert mse != .

summ relbias, d
assert relbias != .



** Get rid of N/As and convert to values 
replace bias_se = "" if bias_se == "NA"
replace relbias_se = "" if relbias_se == "NA"
replace coverage_se = "" if coverage_se == "NA"
replace mse_se = "" if mse_se == "NA"
replace postprob = "" if postprob == "NA"
replace postprob_se = "" if postprob_se == "NA"
replace postsup = "" if postsup == "NA"
replace postsup_se = "" if postsup_se == "NA"
replace stopping_se = "" if stopping_se == "NA"

destring bias_se, replace 
destring relbias_se, replace 
destring coverage_se, replace 
destring mse_se, replace 
destring postprob, replace 
destring postprob_se, replace 
destring postsup, replace 
destring postsup_se, replace 
destring stopping_se, replace 

replace postsup = postsup * 100
replace stopping = stopping * 100

// MCSE 
summ bias_se, d

summ coverage_se, d

summ mse_se, d 

summ relbias_se, d

summ postprob_se, d 

summ postsup_se, d 

summ stopping_se, d 


** Also check the other strings 
summ postprob, d 
summ postsup, d 

//Check other variables are in range 
tab model 
assert model != ""

tab category
assert category != .

replace effectsize = "Small [OR = 1.10]" if effectsize == "Weak [OR = 1.10]"
tab effectsize

tab controlprob //These should be equal numbers
assert controlprob != ""

tab sampsize 
assert inlist(sampsize,100,500)

tab design
assert design != ""

tab prior
assert prior != ""

summ stopping ,d 

//Row headers - don't need these 
drop v1 

sort effectsize category model controlprob design sampsize

order effectsize category model controlprob design prior sampsize

gen coveragepct = coverage*100
drop coverage 
rename coveragepct coverage 
order coverage, after(bias_se)

*Where there should not be duplicates 
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se 

replace relbias = relbias*100 
export excel using "Analysis\Graphs\dataforgraph_alpha_sens.xlsx", replace firstrow(var) keepcellfmt


exit 



