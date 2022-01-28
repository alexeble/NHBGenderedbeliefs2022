/*

Date last modified: 2022-01-27

This code generates the tables and figures in 
"Gendered beliefs about math ability transmit across generations through children's peers"
by Alex Eble and Feng Hu
	   
*/

* Preamble

capture log close
set more off
clear *
set matsize 10000
	

* These will be made anonymous before posting

global basefolder "***/analysis"
global datafolder "$basefolder/input/"
global analysisfolder "$basefolder/temp/"
global outputfolder "$basefolder/output/"
global paperfolder "***"


* Setting global for estimation sample 

global cond1 (ple1503 == 1 & hra05 == 2 & (grade9 == 0 | (ple17 == 5 & grade9 == 1)))

global t1sample (ppb !=. & ppbxfemale !=. & mathbelief_gd !=. & fem_belief_gd !=. & mathbelief_sf !=. & female != . & age != . & edul_fa != . & edul_fax != . & edul_mo != . & edul_mox != . & agrhk != . & agrhkxfemale != . & nsibling != . & nsiblingxfemale != . & minority != . & minorityxfemale != . & hhinc_low != . & hhinc_lowxfem != . & female_math != . & fsft_math != . & awardpro_math != . & awardcity_math != . &  teacol_math != . & teachyr_math != . & qual1_math != . & age_math != . & math_grade6 != . & math_grade6x != .)

cd "$analysisfolder"

* Loading data
foreach v in peerpar peerpar_class {
	use "***/build/output/`v'", clear
	save "$datafolder/`v'", replace
	use "$datafolder/`v'", clear
	}

program loaddata
		set more off
		use "$datafolder/peerpar", clear
		keep if (ple1503 == 1 & hra05 == 2 & (grade9 == 0 | (ple17 == 5 & grade9 == 1))) /*& mathbelief_gd != . & math_grade6 != .*/
		cd "$analysisfolder"
	end

program standardize
		syntax, invar(varname) outvar(name)
		sum `invar'
		local mu = r(mean)
		local sd = r(sd)
		gen `outvar' = (`invar' - `mu')/`sd'
	end
	

									***************
									*** Results ***
									***************

*Master varlist ppb`b' ppb`b'xfemale mathbelief_gd fem_belief_gd female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

** Table 1

loaddata

local varlist female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

foreach b in a b g {
	
	* Regression
	reg mathbelief_sf ppb`b' ppb`b'xfemale mathbelief_gd fem_belief_gd `varlist' if $cond1 ,vce(cluster grad_schid) absorb(grad_schid)	

	* Saving bits of the matrix for reporting
	matrix t1ppb`b' = r(table)
	matrix list t1ppb`b'
	forvalues x = 1/5 {
	local t1ppb`b'var`x'_beta: di %6.3f t1ppb`b'[1,`x']
	local t1ppb`b'var`x'_lb: di %6.3f t1ppb`b'[5,`x']	
	local t1ppb`b'var`x'_ub: di %6.3f t1ppb`b'[6,`x']
	local t1ppb`b'var`x'_pv: di %6.3f t1ppb`b'[4,`x']
	local t1ppb`b'var`x'_lb = subinstr("`t1ppb`b'var`x'_lb'"," ","",.)
	local t1ppb`b'var`x'_ub = subinstr("`t1ppb`b'var`x'_ub'"," ","",.)
	local t1ppb`b'var`x'_pv = subinstr("`t1ppb`b'var`x'_pv'"," ","",.)
	
	}

	test ppb`b' + ppb`b'xfemale  = 0
	
	local rt1_`b': di %9.3fc e(r2)
	local Nt1_`b': di %9.0fc e(N)

}

putexcel set "$paperfolder/table1.csv", replace 
* Column 1 variable labels
putexcel A1=("Peer parent beliefs") A4=("PPB x female") A7=("Own parent beliefs (OPB)") 
putexcel A10=("OPB x female") A13=("Female") A16=("R-squared") A17=("Observations")

* Coefficients

foreach b in a b g {

	
	if "`b'" == "a" local c = "B"
	if "`b'" == "b" local c = "C"
	if "`b'" == "g" local c = "D"
	*if "`v'" == "g" local pref = "g"
	forvalues x = 1/5 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `c'`beta`x''=("`t1ppb`b'var`x'_beta'")
		putexcel `c'`ci`x''=("(`t1ppb`b'var`x'_lb'-`t1ppb`b'var`x'_ub')") 
		putexcel `c'`pv`x''=("[`t1ppb`b'var`x'_pv']") 

	}
	putexcel `c'16=("`rt1_`b''")
	putexcel `c'17=("`Nt1_`b''")
}

putexcel save
	

** NHB table 2 - math performance (need better word?)

loaddata

replace stdmat = stdmat/10

local varlist female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

foreach b in a b g {
	reg stdmat ppb`b' ppb`b'xfemale mathbelief_gd fem_belief_gd `varlist' if $cond1 ,vce(cluster grad_schid) absorb(grad_schid)	

	matrix t2ppb`b' = r(table)
	matrix list t2ppb`b'

	* Saving bits of the matrix for reporting
	matrix list t2ppb`b'
	forvalues x = 1/5 {
	local t2ppb`b'var`x'_beta: di %6.3f t2ppb`b'[1,`x']
	*display "Coefficient is `t2ppb`b'var`x'_beta'"
	local t2ppb`b'var`x'_lb: di %6.3f t2ppb`b'[5,`x']
	local t2ppb`b'var`x'_ub: di %6.3f t2ppb`b'[6,`x']
	local t2ppb`b'var`x'_pv: di %6.3f t2ppb`b'[4,`x']
	local t2ppb`b'var`x'_lb = subinstr("`t2ppb`b'var`x'_lb'"," ","",.)
	local t2ppb`b'var`x'_ub = subinstr("`t2ppb`b'var`x'_ub'"," ","",.)
	local t2ppb`b'var`x'_pv = subinstr("`t2ppb`b'var`x'_pv'"," ","",.)

	}

	local rt2_`b': di %9.3fc e(r2)
	local Nt2_`b': di %9.0fc e(N)

	test ppb`b' + ppb`b'xfemale = 0
		
	}

putexcel set "$paperfolder/table2.csv", replace 

* Column 1 variable labels
putexcel A1=("Peer parent beliefs") A4=("PPB x female") A7=("Own parent beliefs (OPB)") 
putexcel A10=("OPB x female") A13=("Female") A16=("R-squared") A17=("Observations")

* Coefficients

foreach b in a b g {

	
	if "`b'" == "a" local c = "B"
	if "`b'" == "b" local c = "C"
	if "`b'" == "g" local c = "D"
	*if "`v'" == "g" local pref = "g"
	forvalues x = 1/5 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `c'`beta`x''=("`t2ppb`b'var`x'_beta'")
		putexcel `c'`ci`x''=("(`t2ppb`b'var`x'_lb'-`t2ppb`b'var`x'_ub')") 
		putexcel `c'`pv`x''=("[`t2ppb`b'var`x'_pv']") 

	}
	putexcel `c'16=("`rt2_`b''")
	putexcel `c'17=("`Nt2_`b''")
}

putexcel save


** NHB Table 3 - Horse Race

loaddata

replace stdmat = stdmat/10

local varlist1 female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math teacol_math teachyr_math qual1_math age_math stcog i.math_grade6 i.math_grade6x i.edul_mox i.edul_fax ta4_mat
local varlist2 female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math teacol_math teachyr_math qual1_math age_math stcog i.math_grade6 i.math_grade6x i.edul_mox i.edul_fax ta4_mat av_eduyr_peerfa av_eduyr_peermo fem_av_eduyr_peerfa fem_av_eduyr_peermo 
local varlist3 female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math teacol_math teachyr_math qual1_math age_math stcog i.math_grade6 i.math_grade6x i.edul_mox i.edul_fax ta4_mat av_eduyr_peerfa av_eduyr_peermo fem_av_eduyr_peerfa fem_av_eduyr_peermo peerpar_hhinc_low peerpar_hhinc_lowxfemale
local varlist4 female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math teacol_math teachyr_math qual1_math age_math stcog i.math_grade6 i.math_grade6x i.edul_mox i.edul_fax ta4_mat av_eduyr_peerfa av_eduyr_peermo fem_av_eduyr_peerfa fem_av_eduyr_peermo peerpar_hhinc_low peerpar_hhinc_lowxfemale peerpar_agrhk peerpar_agrhkxfemale
local varlist5 female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math teacol_math teachyr_math qual1_math age_math stcog i.math_grade6 i.math_grade6x i.edul_mox i.edul_fax ta4_mat av_eduyr_peerfa av_eduyr_peermo fem_av_eduyr_peerfa fem_av_eduyr_peermo peerpar_hhinc_low peerpar_hhinc_lowxfemale peerpar_agrhk peerpar_agrhkxfemale propfemale propfemalexfemale
local varlist6 female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math teacol_math teachyr_math qual1_math age_math stcog i.math_grade6 i.math_grade6x i.edul_mox i.edul_fax ta4_mat av_eduyr_peerfa av_eduyr_peermo fem_av_eduyr_peerfa fem_av_eduyr_peermo peerpar_hhinc_low peerpar_hhinc_lowxfemale peerpar_agrhk peerpar_agrhkxfemale propfemale propfemalexfemale avg_cogabil avg_cogabilxfemale

foreach depvar in mathbelief_sf stdmat {
	
	if "`depvar'" == "mathbelief_sf" local dv = "b"
	if "`depvar'" == "stdmat" local dv = "s"
	
	forvalues v = 1/6 {

		if `v' == 1 local pref = "v1"
		if `v' == 2 local pref = "v2" /* Varlist 2 */
		if `v' == 3 local pref = "v3" /* Varlist 3 */ 
		if `v' == 4 local pref = "v4" /* Varlist 4 */
		if `v' == 5 local pref = "v5" /* Varlist 5 */
		if `v' == 6 local pref = "v6" /* Varlist 6 */
		
		display ""
		display ""
		display "Varlist is `v', pref is `pref', depvar is `dv'"
		display ""
		display ""
		
		*sum `depvar' if $cond1
		*local `dv'`pref'_mean = r(mean)
		
		*if "`dv'" == "b" local `dv'`pref'_mean: di %6.3f ``dv'`pref'_mean'
		*if "`dv'" == "s" local `dv'`pref'_mean: di %6.1f ``dv'`pref'_mean'
		
		reg `depvar' ppb ppbxfemale mathbelief_gd fem_belief_gd i.math_grade6x `varlist`v'' if $cond1 ,vce(cluster grad_schid) absorb(grad_schid)
		
		* Matrix stuff goes here
		matrix t3ppb`b' = r(table)
		matrix list t3ppb`b'

		* Saving bits of the matrix for reporting
		forvalues x = 1/2 {
		local t3ppb`dv'`v'var`x'_beta: di %6.3f t3ppb`b'[1,`x']
		local t3ppb`dv'`v'var`x'_lb: di %6.3f t3ppb`b'[5,`x']
		local t3ppb`dv'`v'var`x'_ub: di %6.3f t3ppb`b'[6,`x']
		local t3ppb`dv'`v'var`x'_pv: di %6.3f t3ppb`b'[4,`x']
		local t3ppb`dv'`v'var`x'_lb = subinstr("`t3ppb`dv'`v'var`x'_lb'"," ","",.)
		local t3ppb`dv'`v'var`x'_ub = subinstr("`t3ppb`dv'`v'var`x'_ub'"," ","",.)
		local t3ppb`dv'`v'var`x'_pv = subinstr("`t3ppb`dv'`v'var`x'_pv'"," ","",.)

		}

		local rt3_`dv'`v': di %9.3fc e(r2)
		local Nt3_`dv'`v': di %9.0fc e(N)
	}
}

display "LB is `t3ppbb2var2_lb', UB is `t3ppbb2var2_ub', PV is `t3ppbb2var2_pv'"
*"(`t3ppbb2var2_lb'-`t3ppbb2var2_ub')"

putexcel set "$paperfolder/table3.csv", replace 
* Column 1 variable labels

putexcel A1=("Peer parent beliefs") A4=("Peer parent beliefs x female") A7=("R-squared") A8=("Observations") 
putexcel A10=("Peer parent beliefs") A13=("Peer parent beliefs x female") A16=("R-squared") A17=("Observations") 

* Coefficients

* Looping across beliefs and test score dependent variables
foreach dv in b s { 
		
	* Looping across the six varlists
	forvalues v = 1/6 {

		if `v' == 1 local c = "B"
		if `v' == 2 local c = "C"
		if `v' == 3 local c = "D"
		if `v' == 4 local c = "E"
		if `v' == 5 local c = "F"
		if `v' == 6 local c = "G"
		
		* Putting in the regression coefficients
		forvalues x = 1/2 {
		
			if "`dv'" == "b" local rstart = 1
			if "`dv'" == "s" local rstart = 10

			* Getting the rows right
			local `dv'beta`x' = `rstart'+(`x'-1)*3
			local `dv'ci`x' = ``dv'beta`x''+1
			local `dv'pv`x' = ``dv'beta`x''+2
			
			* Putting the locals for beta, ci, p-value
			putexcel `c'``dv'beta`x''=("`t3ppb`dv'`v'var`x'_beta'")
			putexcel `c'``dv'ci`x''=("(`t3ppb`dv'`v'var`x'_lb'-`t3ppb`dv'`v'var`x'_ub')") 
			putexcel `c'``dv'pv`x''=("[`t3ppb`dv'`v'var`x'_pv']") 
		
			local `dv'_r_row = `rstart'+6
			local `dv'_N_row = `rstart'+7

			putexcel `c'``dv'_r_row'=("`rt3_`dv'`v''")
			putexcel `c'``dv'_N_row'=("`Nt3_`dv'`v''")

		}
	
	}
	
}

putexcel save


*
*** Figures
*

** Figure 1 - Nonparametrics in beliefs

loaddata

* Math beliefs

reg mathbelief_sf i.grad_schid, vce(cluster grad_schid) absorb(grad_schid)
predict resid_beliefs, residuals

graph drop _all
* Girls
graph twoway lpolyci resid_beliefs ppb if female == 1 &  abs(ppb) <= 2, ti("Y: Girl student beliefs    X: All peer parent beliefs")   ///
	yti("") ///
	xti("") name(beliefppba_g) bgcolor(white) ///
	/* Other graph formatting */ bgcolor(white) graphregion(color(white)) ysc(range(-.25 .28) noline) clwidth(thick) ylab(-.2 -.1 0 .1 .2 , angle(horizontal) notick) legend(off)
graph export "$paperfolder/np_beliefppba_g.png", as(png) replace
graph export "$paperfolder/Fig1PanelALeftside.eps", as(eps) replace

	
* Boys
graph twoway lpolyci resid_beliefs ppb if female == 0 &  abs(ppb) <= 2, ti("Y: Boy student beliefs    X: All peer parent beliefs")  ///
	yti("") ///
	xti("") name(beliefppba_b)   ///
	/* Other graph formatting */ bgcolor(white) graphregion(color(white)) ysc(range(-.25 .28) noline) clwidth(thick) ylab(-.2 -.1 0 .1 .2 , angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig1PanelARightside.eps", as(eps) replace
graph export "$paperfolder/np_beliefppba_b.png", as(png) replace

* Homophily

* Girls
graph twoway lpolyci resid_beliefs ppbb if female == 1 &  abs(ppbb) <= 2, ti("Y: Girl student beliefs    X: Boy peer parent beliefs")   ///
	yti("") ///
	xti("") name(beliefppbb_g) ///
	/* Other graph formatting */ bgcolor(white) graphregion(color(white)) ysc(range(-.25 .28)noline) clwidth(thick) ylab(-.2 -.1 0 .1 .2 , angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig1PanelCLeftside.eps", as(eps) replace
graph export "$paperfolder/np_beliefppbb_g.png", as(png) replace

graph twoway lpolyci resid_beliefs ppbg if female == 1 &  abs(ppbg) <= 2, name(beliefppbg_g) ti("Y: Girl student beliefs    X: Girl peer parent beliefs")  ///
	yti("") ///
	xti("") ///
	/* Other graph formatting */ bgcolor(white) graphregion(color(white)) ysc(range(-.25 .28) noline) ///
	clwidth(thick) ylab(-.2 -.1 0 .1 .2, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig1PanelBLeftside.eps", as(eps) replace
graph export "$paperfolder/np_beliefppbg_g.png", as(png) replace
	
* Boys
graph twoway lpolyci resid_beliefs ppbb if female == 0 &  abs(ppbb) <= 2, ti("Y: Boy student beliefs    X: Boy peer parent beliefs")  ///
	yti("") ///
	xti("") name(beliefppbb_b)  ///
	/* Other graph formatting */ bgcolor(white) graphregion(color(white)) ysc(range(-.25 .28) noline) ///
	clwidth(thick) ylab(-.2 -.1 0 .1 .2, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig1PanelBRightside.eps", as(eps) replace
graph export "$paperfolder/np_beliefppbb_b.png", as(png) replace

graph twoway lpolyci resid_beliefs ppbg if female == 0 &  abs(ppbg) <= 2, ti("Y: Boy student beliefs    X: Girl peer parent beliefs")  ///
	yti("") ///
	xti("") name(beliefppbg_b)  ///
	/* Other graph formatting */ bgcolor(white) graphregion(color(white)) ysc(range(-.25 .28) noline) ///
	clwidth(thick) ylab(-.2 -.1 0 .1 .2, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig1PanelCRightside.eps", as(eps) replace
graph export "$paperfolder/np_beliefppbg_b.png", as(png) replace

graph combine beliefppba_g beliefppbg_g beliefppbb_g  beliefppba_b beliefppbb_b beliefppbg_b , altshrink ///
	colfirst rows(3) col(2) graphregion(color(white)) ysize(5) name(F1Ysize5)

graph export "$paperfolder/Figure1.eps", as(eps) replace


*
*** Figure 2 - nonparametrics for test scores
*

loaddata

replace stdmat = stdmat/10

reg stdmat i.grad_schid,vce(cluster grad_schid) absorb(grad_schid)
predict resid_scores, residuals

graph drop _all

* Girls
graph twoway lpolyci resid_scores ppb if female == 1 &  abs(ppb) <= 2,   ///
	ti("Y: Girl student test scores    X: All peer parent beliefs") ///
	yti("") xti("") name(scoreppba_g) ///
	/* Other graph formatting */ graphregion(color(white))  ysc(range(-.41 .35) noline) ///
	clwidth(thick) ylab(-.4 -.3 -.2 -.1 0 .1 .2 .3, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig2PanelALeftside.eps", as(eps) replace
	graph export "$paperfolder/np_scoreppba_g.png", as(png) replace

* Boys
graph twoway lpolyci resid_scores ppb if female == 0 &  abs(ppb) <= 2,  ///
	ti("Y: Boy student test scores    X: All peer parent beliefs") ///
	yti("") xti("") name(scoreppba_b)  ///
	/* Other graph formatting */ graphregion(color(white)) ysc(range(-.41 .35) noline) ///
	clwidth(thick) ylab(-.4 -.3 -.2 -.1 0 .1 .2 .3, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig2PanelARightside.eps", as(eps) replace
graph export "$paperfolder/np_scoreppba_b.png", as(png) replace

* Homophily

* Girls
graph twoway lpolyci resid_scores ppbb if female == 1 &  abs(ppbb) <= 2,   ///
	ti("Y: Girl student test scores    X: Boy peer parent beliefs") ///
	yti("")	xti("") name(scoreppbb_g) ///
	/* Other graph formatting */ graphregion(color(white)) ysc(range(-.41 .35) noline) ///
	clwidth(thick) ylab(-.4 -.3 -.2 -.1 0 .1 .2 .3, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig2PanelCLeftside.eps", as(eps) replace
graph export "$paperfolder/np_scoreppbb_g.png", as(png) replace

graph twoway lpolyci resid_scores ppbg if female == 1 &  abs(ppbg) <= 2,  ///
	ti("Y: Girl student test scores    X: Girl peer parent beliefs") ///
	yti("") xti("") name(scoreppbg_g) ///
	/* Other graph formatting */ graphregion(color(white)) ysc(range(-.41 .35) noline) ///
	clwidth(thick) ylab(-.4 -.3 -.2 -.1 0 .1 .2 .3, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig2PanelBLeftside.eps", as(eps) replace
graph export "$paperfolder/np_scoreppbg_g.png", as(png) replace

* Boys
graph twoway lpolyci resid_scores ppbb if female == 0 &  abs(ppbb) <= 2,   ///
	ti("Y: Boy student test scores    X: Boy peer parent beliefs") ///
	yti("") xti("") name(scoreppbb_b)  ///
	/* Other graph formatting */ graphregion(color(white)) ysc(range(-.41 .35) noline) ///
	clwidth(thick) ylab(-.4 -.3 -.2 -.1 0 .1 .2 .3, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig2PanelBRightside.eps", as(eps) replace
graph export "$paperfolder/np_scoreppbb_b.png", as(png) replace
	
graph twoway lpolyci resid_scores ppbg if female == 0 &  abs(ppbg) <= 2,  ///
	ti("Y: Boy student test scores    X: Girl peer parent beliefs") ///
	yti("")	xti("") name(scoreppbg_b)  ///
	/* Other graph formatting */ graphregion(color(white)) ysc(range(-.41 .35) noline) ///
	clwidth(thick) ylab(-.4 -.3 -.2 -.1 0 .1 .2 .3, angle(horizontal) notick) legend(off)
graph export "$paperfolder/Fig2PanelCRightside.eps", as(eps) replace
graph export "$paperfolder/np_scoreppbg_b.png", as(png) replace

graph combine scoreppba_g scoreppbg_g scoreppbb_g  scoreppba_b scoreppbb_b scoreppbg_b , altshrink ///
	colfirst rows(3) col(2) graphregion(color(white)) ysize(5)

graph export "$paperfolder/Figure2.eps", as(eps) replace
	
	
*** Supplementary tables

** Supplementary table 1 - summary statistics
	
use "$datafolder/peerpar", clear

cd "$analysisfolder"

keep if $cond1 & $t1sample

label var stdmat "Math test score"
label var stdeng "English test score"
label var stdchn "Chinese test score"
label var age "Age"
label var minority "Ethnic minority"
label var agrhk "Holds agricultural hukou"
label var edul_fa2 "Middle school"
label var edul_fa3 "High school"
label var edul_fa4 "College"
label var edul_mo2 "Middle school"
label var edul_mo3 "High school"
label var edul_mo4 "College"
label var nsibling "Number of siblings"
label var hhinc_low "Low income household"

local varlist = "age agrhk minority nsibling hhinc_low  edul_mo2 edul_mo3 edul_mo4 edul_fa2 edul_fa3 edul_fa4 stdmat stdeng stdchn "

foreach var in `varlist' {
	local varname`var' : variable label `var' 
	sum `var'
	local mean_all_`var': di %9.2f r(mean)
	*if "`var'" == "stdmat" |  "`var'" == "stdchn" |  "`var'" == "stdeng" local mean_all_`var': di %9.1f r(mean)
	sum `var' if female == 0
	local mean_male_`var': di %9.2f r(mean)
	*if "`var'" == "stdmat" |  "`var'" == "stdchn" |  "`var'" == "stdeng" local mean_male_`var': di %9.1f r(mean)
	sum `var' if female == 1
	local mean_female_`var': di %9.2f r(mean)
	*if "`var'" == "stdmat" |  "`var'" == "stdchn" |  "`var'" == "stdeng" local mean_female_`var': di %9.1f r(mean)
	local diff_`var': di %9.2f `mean_female_`var'' -  `mean_male_`var''
	ttest `var', by(female)
	local pval_`var': di %9.3f r(p)

	}

count
local obs_all: di %9.0f r(N)
count if female == 0
local obs_male: di %9.0f r(N)
count if female == 1
local obs_female: di %9.0f r(N)

* Note: CIs copied and pasted manually from Stata output from ttest
		
texdoc init "$paperfolder/table_s1sumstats.csv", replace force
	    
tex 										,	(1)								,	(2)									,	(3)						,	(4) 				,	(5) 				,	(5)					
tex 										,	All								,	Girls								,	Boys					,	Differ-			 	,	95 %			 	,	P-value				
tex 										,	children						,	only								,	only					,	ence				,	CI				 	,					
tex , , , , , ,
tex Age										,	`mean_all_age'				 	,	`mean_female_age'					,	`mean_male_age'			,	`diff_age'			,,	`pval_age'			
tex , , , , , ,
tex Holds agricultural hukou				,	`mean_all_agrhk'			 	,	`mean_female_agrhk'				 	,	`mean_male_agrhk'		,	`diff_agrhk'		,,	`pval_agrhk'		
tex , , , , , ,
tex Number of siblings						,	`mean_all_nsibling'		 		,	`mean_female_nsibling'		 		,	`mean_male_nsibling'	,	`diff_nsibling'		,,	`pval_nsibling'		
tex , , , , , ,
tex Low income household					,	`mean_all_hhinc_low'			,	`mean_female_hhinc_low'				,	`mean_male_hhinc_low'	,	`diff_hhinc_low'	,,	`pval_hhinc_low'	
tex , , , , , ,
tex Ethnic minority							,	`mean_all_minority'		 		,	`mean_female_minority'		 		,	`mean_male_minority'	,	`diff_minority'		,,	`pval_minority'	
tex , , , , , ,
tex Mother's years of schooling , , , , , ,
tex Middle school			 				,	`mean_all_edul_mo2'		 		,	`mean_female_edul_mo2'		 		,	`mean_male_edul_mo2'	,	`diff_edul_mo2'		,,	`pval_edul_mo2'	
tex High school			 				,	`mean_all_edul_mo3'		 		,	`mean_female_edul_mo3'		 		,	`mean_male_edul_mo3'	,	`diff_edul_mo3'		,,	`pval_edul_mo3'	
tex College school			 				,	`mean_all_edul_mo4'		 		,	`mean_female_edul_mo4'		 		,	`mean_male_edul_mo4'	,	`diff_edul_mo4'		,,	`pval_edul_mo4'	
tex , , , , , ,
tex Father's years of schooling , , , , , ,
tex Middle school			 				,	`mean_all_edul_fa2'		 		,	`mean_female_edul_fa2'		 		,	`mean_male_edul_fa2'	,	`diff_edul_fa2'		,,	`pval_edul_fa2'	
tex High school			 				,	`mean_all_edul_fa3'		 		,	`mean_female_edul_fa3'		 		,	`mean_male_edul_fa3'	,	`diff_edul_fa3'		,,	`pval_edul_fa3'	
tex College school			 				,	`mean_all_edul_fa4'		 		,	`mean_female_edul_fa4'		 		,	`mean_male_edul_fa4'	,	`diff_edul_fa4'		,,	`pval_edul_fa4'	
tex , , , , , ,
tex	Math test score							,	`mean_all_stdmat'			 	,	`mean_female_stdmat'			 	,	`mean_male_stdmat'		,	`diff_stdmat'		,,	`pval_stdmat'	
tex , , , , , ,
tex	English test score						,	`mean_all_stdeng'			 	,	`mean_female_stdeng'			 	,	`mean_male_stdeng'		,	`diff_stdeng'		,,	`pval_stdeng'	
tex , , , , , ,
tex	Chinese test score						,	`mean_all_stdchn'			 	,	`mean_female_stdchn'			 	,	`mean_male_stdchn'		,	`diff_stdchn'		,,	`pval_stdchn'	
tex , , , , , ,
tex Number of observations					,	`obs_all'			 			,	`obs_female'			 			,	`obs_male'				,	---					,,	---				
tex , , , , , ,

texdoc close


** Supplementary table 2 - summary stats by parent beliefs

use "$datafolder/peerpar", clear

cd "$analysisfolder"

keep if $cond1 & $t1sample

* Labelling variables
label var female Female
label var age Age
label var minority "Ethnic minority"
label var eduyr_mo "Mother's years of schooling"
label var eduyr_fa "Father's years of schooling"
label var agrhk "Holds agricultural hukou"
label var nsibling "Number of siblings"
label var hhinc_low "Low income household"
label var edul_fa2 "Middle school degree"
label var edul_fa3 "High school or techinical degree" 
label var edul_fa4 "College or above"
label var edul_mo2 "Middle school degree"
label var edul_mo3 "High school or techinical degree" 
label var edul_mo4 "College or above"
label var parent_age "Parent's age"
label var female "Child is female"

local varlist = "female parent_age minority agrhk nsibling hhinc_low edul_mo2 edul_mo3 edul_mo4 edul_fa2 edul_fa3 edul_fa4"

foreach var in `varlist' {
		local varname`var' : variable label `var' 
		sum `var'
		local mean_all_`var': di %9.2f r(mean)
		local sd_all_`var': di %9.3f r(sd)
		sum `var' if mathbelief_gd_p == 0
		local mean_nb_`var': di %9.2f r(mean)
		local sd_nb_`var': di %9.3f r(sd)
		sum `var' if mathbelief_gd_p == 1
		local mean_yb_`var': di %9.2f r(mean)
		local sd_yb_`var': di %9.3f r(sd)
		local diff_`var': di %9.2f `mean_yb_`var'' - `mean_nb_`var'' 
		ttest `var', by(mathbelief_gd_p)
		local pval_`var': di %9.3f r(p)
		}
	
count
local obs_all: di %9.0f r(N)
count if mathbelief_gd == 0
local obs_nb: di %9.0f r(N)
count if mathbelief_gd == 1
local obs_yb: di %9.0f r(N)

* Note: CIs copied and pasted manually from Stata output from ttest

texdoc init "$paperfolder/table_s2opbsumstats.csv", replace force
tex		 					,	(1)								,	(2)								,	(3)									,	(4)					,	(5)					,	(6)				
tex 						,	Full							,	Believes						,	Does not							,	Differ-				,	95%					,	P-value			
tex 						,	sample							,	$ B_m $ > $ G_m $				,	believe								,	ence				,	CI					,					
tex , , , , , 

local varlist2 = "minority agrhk nsibling hhinc_low  female parent_age"

foreach var in `varlist2' {

	tex , , , , , 
	tex `varname`var''		,	`mean_all_`var''	 			,	`mean_yb_`var''					,	`mean_nb_`var''		,	`diff_`var''		,,	`pval_`var''	
	}
tex , , , , , 
tex Mother's highest credential , , , , , 
local varlist3 = "edul_mo2 edul_mo3 edul_mo4"
foreach var in `varlist3' {
	tex `varname`var''		,	`mean_all_`var''	 			,	`mean_yb_`var''					,	`mean_nb_`var''		,	`diff_`var''		,,	`pval_`var''	
	}
tex , , , , , 
tex Father's highest credential , , , , , 
local varlist4 = "edul_fa2 edul_fa3 edul_fa4"
foreach var in `varlist4' {
	tex `varname`var''		,	`mean_all_`var''	 			,	`mean_yb_`var''					,	`mean_nb_`var''		,	`diff_`var''		,,	`pval_`var''	
	}

tex , , , , , 
tex Number of observations	,	`obs_all'			 			,	`obs_yb'			 			,	`obs_nb'			, 	---					,,	---				
tex , , , , , 

texdoc close


** Supplementary table 3 - randomization / balance test

*use "$datafolder/peerpar", clear

loaddata

keep if $t1sample

tab grad_schid, gen(sxgids)
local varlist1 "female age minority agrhk nsibling hhinc_low edul_mo2 edul_mo3 edul_mo4 edul_fa2 edul_fa3 edul_fa4"
local varlist2 "female age minority agrhk nsibling hhinc_low edul_mo2 edul_mo3 edul_mo4 edul_fa2 edul_fa3 edul_fa4 i.grad_schid"

forvalues c = 1/2 {
	reg ppb `varlist`c'' if $cond1 , 
		
	matrix ts3c`c' = r(table)
	matrix list ts3c`c'

	* Saving bits of the matrix for reporting
	forvalues x = 1/12 {
		local ts3c`c'var`x'_beta: di %6.3f  ts3c`c'[1,`x']
		*display "Coefficient is ` ts3c`c'var`x'_beta'"
		local ts3c`c'var`x'_lb: di %6.3f  ts3c`c'[5,`x']
		local ts3c`c'var`x'_ub: di %6.3f  ts3c`c'[6,`x']
		local ts3c`c'var`x'_pv: di %6.3f  ts3c`c'[4,`x']
		local ts3c`c'var`x'_lb = subinstr("`ts3c`c'var`x'_lb'"," ","",.)
		local ts3c`c'var`x'_ub = subinstr("`ts3c`c'var`x'_ub'"," ","",.)
		local ts3c`c'var`x'_pv = subinstr("`ts3c`c'var`x'_pv'"," ","",.)

	}

	
	display "Beta on var12 is `ts3c2var12_beta'"
	
	local ts3c`c'r2: di %9.3fc e(r2)
	local ts3c`c'N: di %9.0fc e(N)

	test `varlist1'

	local ts3c`c'fstat: di %9.3f r(F)
	local ts3c`c'pvalfstat: di %9.3f r(p)

	}

putexcel set "$paperfolder/table_s3.csv", replace 

* Column 1 variable labels
putexcel A1=("Female") A4=("Age") A7=("Ethnic minority") A10=("Holds agricultural hukou")
putexcel A13=("Number of siblings") A16=("Household is poor")
putexcel A19=("Mother's highest credential") A20=("Middle school") A23=("High/technical school") A26=("College or above")
putexcel A29=("Father's highest credential") A30=("Middle school") A33=("High/technical school") A36=("College or above")
putexcel A39=("Number of observations")
putexcel A40=("Grade by school fixed effects") C40=("X")
putexcel A41=("Joint test F-statistic") A42=("[p-value]")

* Coefficients
forvalues c = 1/2 {

	
	if `c' == 1 local col = "B"
	if `c' == 2 local col = "C"

	forvalues x = 1/6 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `col'`beta`x''=("`ts3c`c'var`x'_beta'")
		putexcel `col'`ci`x''=("(`ts3c`c'var`x'_lb'-`ts3c`c'var`x'_ub')") 
		putexcel `col'`pv`x''=("[`ts3c`c'var`x'_pv']") 

	}
	
	* Mother's education, starting after skipping a row
	forvalues x = 7/9 { 
		
		* Getting the rows right
		local beta`x'=2+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `col'`beta`x''=("`ts3c`c'var`x'_beta'")
		putexcel `col'`ci`x''=("(`ts3c`c'var`x'_lb'-`ts3c`c'var`x'_ub')") 
		putexcel `col'`pv`x''=("[`ts3c`c'var`x'_pv']") 

	}
	
	* Father's education, starting after skipping a row
	forvalues x = 10/12 { 
		
		* Getting the rows right
		local beta`x'=3+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `col'`beta`x''=("`ts3c`c'var`x'_beta'")
		putexcel `col'`ci`x''=("(`ts3c`c'var`x'_lb'-`ts3c`c'var`x'_ub')") 
		putexcel `col'`pv`x''=("[`ts3c`c'var`x'_pv']") 

	}

	* Obs
	putexcel `col'39=("`ts3c`c'N'")
	
	* F-stat and pvalue
	putexcel `col'41=("`ts3c`c'fstat'")
	putexcel `col'42=("`ts3c`c'pvalfstat'")
	
	}
	
putexcel save


** Supplementary table 4 - how malleable are parent beliefs

loaddata

keep if $t1sample

* Generating easy grade variables
gen gradeseven = 1-grade9
gen gradenine = grade9

* Panel A - Main explanatory variable: gender gap in math

* Estimating the mean of the variable for the bottom of the table
sum  mathbelief_gd if $cond1
local ts4p1c1_mean = r(mean)
local ts4p1c1_mean: di %6.3f `ts4p1c1_mean'

local varlist female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low i.math_grade6 agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math

reghdfe mathbelief_gd peermathgap peermathgap_fem `varlist' if $cond1 , vce(cluster grad_schid) absorb(grad_schid)

local  ts4p1c1_N: di %6.0fc e(N)

matrix ts4p1c1 = r(table)
matrix list ts4p1c1

	forvalues x = 1/2 {
	local ts4p1c1var`x'_beta: di %6.3f ts4p1c1[1,`x']
	local ts4p1c1var`x'_lb: di %6.3f ts4p1c1[5,`x']
	local ts4p1c1var`x'_ub: di %6.3f ts4p1c1[6,`x']
	local ts4p1c1var`x'_pv: di %6.3f ts4p1c1[4,`x']
	local ts4p1c1var`x'_lb = subinstr("`ts4p1c1var`x'_lb'"," ","",.)
	local ts4p1c1var`x'_ub = subinstr("`ts4p1c1var`x'_ub'"," ","",.)
	local ts4p1c1var`x'_pv = subinstr("`ts4p1c1var`x'_pv'"," ","",.)

	}
	
foreach y in s n {
	
	if "`y'" == "s" local c = 2
	if "`y'" == "n" local c = 3
	
	* Estimating the mean of the variable for the bottom of the table
	sum  mathbelief_gd if $cond1 & grade`y' == 1
	local ts4p1c`c'_mean = r(mean)
	local ts4p1c`c'_mean: di %6.3f `ts4p1c`c'_mean'

	local varlist female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low i.math_grade6 agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math

	* Gender gap in math regressions
	reghdfe mathbelief_gd peermathgap peermathgap_fem `varlist' if $cond1 & grade`y' == 1, vce(cluster grad_schid) absorb(grad_schid)
		
	matrix ts4p1c`c' = r(table)
	matrix list ts4p1c`c'

	forvalues x = 1/2 {
		local ts4p1c`c'var`x'_beta: di %6.3f ts4p1c`c'[1,`x']
		local ts4p1c`c'var`x'_lb: di %6.3f ts4p1c`c'[5,`x']
		local ts4p1c`c'var`x'_ub: di %6.3f ts4p1c`c'[6,`x']
		local ts4p1c`c'var`x'_pv: di %6.3f ts4p1c`c'[4,`x']
		local ts4p1c`c'var`x'_lb = subinstr("`ts4p1c`c'var`x'_lb'"," ","",.)
		local ts4p1c`c'var`x'_ub = subinstr("`ts4p1c`c'var`x'_ub'"," ","",.)
		local ts4p1c`c'var`x'_pv = subinstr("`ts4p1c`c'var`x'_pv'"," ","",.)

		}
	
	local ts4p1c`c'_N: di %6.0fc e(N)
}

display "First beta, col 2, is `ts4p1c2var2_beta'"

putexcel set "$paperfolder/table_s4panelA.csv", replace 

* Column 1 variable labels
putexcel A1=("Gender gap in child's peers'") A2=("test scores")
putexcel A4=("Gender gap in child's peers'") A5=("test scores x child is female")
putexcel A7=("Mean in sample") A8=("Number of observations")

* Coefficients

forvalues c = 1/3 {

	
	if `c' == 1 local col = "B"
	if `c' == 2 local col = "C"
	if `c' == 3 local col = "D"

	forvalues x = 1/2 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `col'`beta`x''=("`ts4p1c`c'var`x'_beta'")
		putexcel `col'`ci`x''=("(`ts4p1c`c'var`x'_lb'-`ts4p1c`c'var`x'_ub')") 
		putexcel `col'`pv`x''=("[`ts4p1c`c'var`x'_pv']") 
	
	}
	putexcel `col'7=("`ts4p1c`c'_mean'")
	putexcel `col'8=("`ts4p1c`c'_N'")

	}
	
putexcel save


* Panel B - Main explanatory variable: top student is male

* Estimating the mean of the variable for the bottom of the table
sum  mathbelief_gd if $cond1

local ts4p2c1_mean = r(mean)
local ts4p2c1_mean: di %6.3f `ts4p2c1_mean'

local varlist female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low i.math_grade6 agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math

* Top student male
reghdfe mathbelief_gd t3sm t3smx `varlist' if $cond1 , vce(cluster grad_schid) absorb(grad_schid)
local N_ts: di %6.0f e(N)

local  ts4p2c1_N: di %6.0fc e(N)

matrix ts4p2c1 = r(table)
matrix list ts4p2c1

	forvalues x = 1/2 {
	local ts4p2c1var`x'_beta: di %6.3f ts4p2c1[1,`x']
	local ts4p2c1var`x'_lb: di %6.3f ts4p2c1[5,`x']
	local ts4p2c1var`x'_ub: di %6.3f ts4p2c1[6,`x']
	local ts4p2c1var`x'_pv: di %6.3f ts4p2c1[4,`x']
	local ts4p2c1var`x'_lb = subinstr("`ts4p2c1var`x'_lb'"," ","",.)
	local ts4p2c1var`x'_ub = subinstr("`ts4p2c1var`x'_ub'"," ","",.)
	local ts4p2c1var`x'_pv = subinstr("`ts4p2c1var`x'_pv'"," ","",.)

	}

* Doing separately by grade now:

foreach y in s n {
		
	if "`y'" == "s" local c = 2
	if "`y'" == "n" local c = 3
	
	* Estimating the mean of the variable for the bottom of the table
	sum  mathbelief_gd if $cond1 & grade`y' == 1
	local ts4p2c`c'_mean = r(mean)
	local ts4p2c`c'_mean: di %6.3f `ts4p2c`c'_mean'

	local varlist female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low i.math_grade6 agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math

	* Gender gap in math regressions
	reghdfe mathbelief_gd tsm tsmxfemale `varlist' if $cond1 & grade`y' == 1, vce(cluster grad_schid) absorb(grad_schid)

	matrix ts4p2c`c' = r(table)
	matrix list ts4p2c`c'

	forvalues x = 1/2 {
		local ts4p2c`c'var`x'_beta: di %6.3f ts4p2c`c'[1,`x']
		local ts4p2c`c'var`x'_lb: di %6.3f ts4p2c`c'[5,`x']
		local ts4p2c`c'var`x'_ub: di %6.3f ts4p2c`c'[6,`x']
		local ts4p2c`c'var`x'_pv: di %6.3f ts4p2c`c'[4,`x']
		local ts4p2c`c'var`x'_lb = subinstr("`ts4p2c`c'var`x'_lb'"," ","",.)
		local ts4p2c`c'var`x'_ub = subinstr("`ts4p2c`c'var`x'_ub'"," ","",.)
		local ts4p2c`c'var`x'_pv = subinstr("`ts4p2c`c'var`x'_pv'"," ","",.)

		}
	
	local ts4p2c`c'_N: di %6.0fc e(N)
}


putexcel set "$paperfolder/table_s4panelB.csv", replace 
* Column 1 variable labels
putexcel A1=("Gender gap in child's peers'") A2=("test scores")
putexcel A4=("Gender gap in child's peers'") A5=("test scores x child is female")
putexcel A7=("Mean in sample") A8=("Number of observations")

* Coefficients

forvalues c = 1/3 {

	
	if `c' == 1 local col = "B"
	if `c' == 2 local col = "C"
	if `c' == 3 local col = "D"

	forvalues x = 1/2 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `col'`beta`x''=("`ts4p2c`c'var`x'_beta'")
		putexcel `col'`ci`x''=("(`ts4p2c`c'var`x'_lb'-`ts4p2c`c'var`x'_ub')") 
		putexcel `col'`pv`x''=("[`ts4p2c`c'var`x'_pv']") 
	
	}
	putexcel `col'7=("`ts4p2c`c'_mean'")
	putexcel `col'8=("`ts4p2c`c'_N'")

	}
	
putexcel save


** Supplementary table 5

loaddata

local varlist female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

foreach b in a b g {
	
	reg mathbelief_sf ppb`b' ppb`b'xfemale mathbelief_gd fem_belief_gd avg_genderview fem_avg_genderview `varlist' if $cond1 ,vce(cluster grad_schid) absorb(grad_schid)	

	* Saving bits of the matrix for reporting
	matrix ts5ppb`b' = r(table)
	matrix list ts5ppb`b'
	forvalues x = 1/7 {
		local ts5ppb`b'var`x'_beta: di %6.3f ts5ppb`b'[1,`x']
		*display "Coefficient is `ts5ppb`b'var`x'_beta'"
		local ts5ppb`b'var`x'_lb: di %6.3f ts5ppb`b'[5,`x']
		local ts5ppb`b'var`x'_ub: di %6.3f ts5ppb`b'[6,`x']
		local ts5ppb`b'var`x'_pv: di %6.3f ts5ppb`b'[4,`x']
		local ts5ppb`b'var`x'_lb = subinstr("`ts5ppb`b'var`x'_lb'"," ","",.)
		local ts5ppb`b'var`x'_ub = subinstr("`ts5ppb`b'var`x'_ub'"," ","",.)
		local ts5ppb`b'var`x'_pv = subinstr("`ts5ppb`b'var`x'_pv'"," ","",.)

	}

	test ppb`b' + ppb`b'xfemale  = 0
	
	local rts5_`b': di %9.3fc e(r2)
	local Nts5_`b': di %9.0fc e(N)

}

putexcel set "$paperfolder/table_s5.csv", replace 

* Column 1 variable labels
putexcel A1=("Peer parent beliefs (PPB)") A4=("PPB x female") A7=("Own parent beliefs (OPB)") 
putexcel A10=("OPB x female") A13=("Peer beliefs (PB)") A16=("PB x female") 
putexcel A19=("Female") A22=("R-squared") A23=("Observations")

* Coefficients

foreach b in a b g {
	
	if "`b'" == "a" local c = "B"
	if "`b'" == "b" local c = "C"
	if "`b'" == "g" local c = "D"
	*if "`v'" == "g" local pref = "g"

	forvalues x = 1/7 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `c'`beta`x''=("`ts5ppb`b'var`x'_beta'")
		putexcel `c'`ci`x''=("(`ts5ppb`b'var`x'_lb'-`ts5ppb`b'var`x'_ub')") 
		putexcel `c'`pv`x''=("[`ts5ppb`b'var`x'_pv']") 

	}
	putexcel `c'22=("`rts5_`b''")
	putexcel `c'23=("`Nts5_`b''")
}

putexcel save
	
	
** Supplementary table 6 - math performance and peers

loaddata

replace stdmat = stdmat/10

local varlist female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

foreach b in a b g {
	reg stdmat ppb`b' ppb`b'xfemale mathbelief_gd fem_belief_gd avg_genderview fem_avg_genderview `varlist' if $cond1, vce(cluster grad_schid) absorb(grad_schid)	

	* Saving bits of the matrix for reporting
	matrix ts6ppb`b' = r(table)
	matrix list ts6ppb`b'
	
	forvalues x = 1/7 {
		local ts6ppb`b'var`x'_beta: di %6.3f ts6ppb`b'[1,`x']
		*display "Coefficient is `ts6ppb`b'var`x'_beta'"
		local ts6ppb`b'var`x'_lb: di %6.3f ts6ppb`b'[5,`x']
		local ts6ppb`b'var`x'_ub: di %6.3f ts6ppb`b'[6,`x']
		local ts6ppb`b'var`x'_pv: di %6.3f ts6ppb`b'[4,`x']
		local ts6ppb`b'var`x'_lb = subinstr("`ts6ppb`b'var`x'_lb'"," ","",.)
		local ts6ppb`b'var`x'_ub = subinstr("`ts6ppb`b'var`x'_ub'"," ","",.)
		local ts6ppb`b'var`x'_pv = subinstr("`ts6ppb`b'var`x'_pv'"," ","",.)
		
	}

	test ppb`b' + ppb`b'xfemale  = 0
	
	local rts6_`b': di %9.3fc e(r2)
	local Nts6_`b': di %9.0fc e(N)

}

putexcel set "$paperfolder/table_s6.csv", replace 
* Column 1 variable labels
putexcel A1=("Peer parent beliefs (PPB)") A4=("PPB x female") A7=("Own parent beliefs (OPB)") 
putexcel A10=("OPB x female") A13=("Peer beliefs (PB)") A16=("PB x female") 
putexcel A19=("Female") A22=("R-squared") A23=("Observations")

* Coefficients

foreach b in a b g {
	
	if "`b'" == "a" local c = "B"
	if "`b'" == "b" local c = "C"
	if "`b'" == "g" local c = "D"
	*if "`v'" == "g" local pref = "g"

	forvalues x = 1/7 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `c'`beta`x''=("`ts6ppb`b'var`x'_beta'")
		putexcel `c'`ci`x''=("(`ts6ppb`b'var`x'_lb'-`ts6ppb`b'var`x'_ub')") 
		putexcel `c'`pv`x''=("[`ts6ppb`b'var`x'_pv']") 

	}
	putexcel `c'22=("`rts6_`b''")
	putexcel `c'23=("`Nts6_`b''")
}

putexcel save


* Supplementary table 7 - channels

loaddata

standardize, invar(peerpar_belief1f) outvar(ppbf)
standardize, invar(peerpar_belief1m) outvar(ppbm)

foreach p in f m {
	gen ppb`p'xfemale = ppb`p'*female
}

local varlist female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

foreach i in a f m b g {
	
	* Regression
	reg mathbelief_sf ppb`i' ppb`i'xfemale mathbelief_gd fem_belief_gd `varlist' if $cond1 ,vce(cluster grad_schid) absorb(grad_schid)	

	* Saving bits of the matrix for reporting
	matrix t7ppb`i' = r(table)
	matrix list t7ppb`i'
	forvalues x = 1/5 {
	local t7ppb`i'var`x'_beta: di %6.3f t7ppb`i'[1,`x']
	local t7ppb`i'var`x'_lb: di %6.3f t7ppb`i'[5,`x']	
	local t7ppb`i'var`x'_ub: di %6.3f t7ppb`i'[6,`x']
	local t7ppb`i'var`x'_pv: di %6.3f t7ppb`i'[4,`x']
	local t7ppb`i'var`x'_lb = subinstr("`t7ppb`i'var`x'_lb'"," ","",.)
	local t7ppb`i'var`x'_ub = subinstr("`t7ppb`i'var`x'_ub'"," ","",.)
	local t7ppb`i'var`x'_pv = subinstr("`t7ppb`i'var`x'_pv'"," ","",.)
	
	}

	test ppb`i' + ppb`i'xfemale  = 0
	
	local rt7_`i': di %9.3fc e(r2)
	local Nt7_`i': di %9.0fc e(N)

}

putexcel set "$paperfolder/table_s7.csv", replace 
* Column 1 variable labels
putexcel A1=("Peer parent beliefs") A4=("PPB x female") A7=("Own parent beliefs (OPB)") 
putexcel A10=("OPB x female") A13=("Female") A16=("R-squared") A17=("Observations")

* Coefficients

foreach i in a f m b g {

	
	if "`i'" == "a" local c = "B"
	if "`i'" == "m" local c = "C"
	if "`i'" == "f" local c = "D"
	if "`i'" == "b" local c = "E"
	if "`i'" == "g" local c = "F"
	*if "`v'" == "g" local pref = "g"

	forvalues x = 1/5 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2

		* Putting the locals for beta, ci, p-value
		putexcel `c'`beta`x''=("`t7ppb`i'var`x'_beta'")
		putexcel `c'`ci`x''=("(`t7ppb`i'var`x'_lb'-`t7ppb`i'var`x'_ub')") 
		putexcel `c'`pv`x''=("[`t7ppb`i'var`x'_pv']") 

	}
	putexcel `c'16=("`rt7_`i''")
	putexcel `c'17=("`Nt7_`i''")
}

putexcel save

	
** Supplementary tables 8-15

/* Legend: these are tables examining heterogeneity: 
Supplementary tables 8-9: heterogeneity by baseline math ability
Supplementary tables 10-11: heterogeneity by high maternal education
Supplementary tables 12-13: heterogeneity by maternal education > paternal education
Supplementary tables 14-15: heterogeneity by own parent beliefs 
For these, table n has beliefs as the outcome variable, n+1 has math test scores as the outcome variable 
*/

loaddata
* High mother ed
recode edul_mo (1 2 = 0)(3 4 = 1), gen(momedhigh)
*recode edul_fa (1 2 = 0)(3 4 = 1), gen(dadedhigh)

* Baseline math ability
recode math_grade6 (1 2 = 0)(3 4 = 1), gen(himatabil)

* Parental ed gap
gen parentedgap = 0 if edul_mo != . & edul_fa != .
replace parentedgap = 1 if edul_mo > edul_fa & edul_mo != . & edul_fa != .

* Own parent beliefs
gen opb = mathbelief_gd

foreach var in momedhigh himatabil parentedgap opb {
	gen `var'xfem = `var'*female
	foreach b in a b g {
		gen `var'xppb`b' = `var'*ppb`b'
		gen `var'xppb`b'xfem = `var'*ppb`b'*female
	}
}

replace stdmat = stdmat/10

local varlist age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

foreach var in momedhigh himatabil parentedgap opb {
	if "`var'" == "momedhigh" local v = "meh"
	if "`var'" == "himatabil" local v = "hma"
	if "`var'" == "parentedgap" local v = "peg"
	if "`var'" == "opb" local v = "opb"
	if "`var'" == "momedhigh" local varname = "High maternal education"
	if "`var'" == "himatabil" local varname = "High baseline math ability"
	if "`var'" == "parentedgap" local varname = "Mother has more schooling than father"
	if "`var'" == "opb" local varname = "Own parent believes Bm > Gm"
	
	foreach b in a b g {

		reg mathbelief_sf ppb`b' ppb`b'xfemale `var' `var'xfem `var'xppb`b' `var'xppb`b'xfem female mathbelief_gd fem_belief_gd `varlist' if $cond1 ,vce(cluster grad_schid) absorb(grad_schid)	
	
		* Saving bits of the matrix for reporting
		matrix at`v'gb`b' = r(table)
		matrix list at`v'gb`b'
		forvalues x = 1/7 {
			local at`v'gb`b'var`x'_beta: di %6.3f at`v'gb`b'[1,`x']
			*display "Coefficient is `ts5ppb`b'var`x'_beta'"
			local at`v'gb`b'var`x'_lb: di %6.3f at`v'gb`b'[5,`x']
			local at`v'gb`b'var`x'_ub: di %6.3f at`v'gb`b'[6,`x']
			local at`v'gb`b'var`x'_pv: di %6.3f at`v'gb`b'[4,`x']
			local at`v'gb`b'var`x'_lb = subinstr("`at`v'gb`b'var`x'_lb'"," ","",.)
			local at`v'gb`b'var`x'_ub = subinstr("`at`v'gb`b'var`x'_ub'"," ","",.)
			local at`v'gb`b'var`x'_pv = subinstr("`at`v'gb`b'var`x'_pv'"," ","",.)
			}

	*test ppb`b' + ppb`b'xfemale  = 0
	
	local rat`v'gb_`b': di %9.3fc e(r2)
	local Nat`v'gb_`b': di %9.0fc e(N)

	}

putexcel set "$paperfolder/at`v'gb.csv", replace 
* Column 1 variable labels
putexcel A1=("Peer parent beliefs (PPB)") A4=("PPB x female") A7=("`varname'") 
putexcel A10=("`varname' x female") A13=("`varname' x PPB") A16=("`varname' x PPB x female") 
putexcel A19=("Female") A22=("R-squared") A23=("Observations")

* Coefficients

foreach b in a b g {
	
	if "`b'" == "a" local c = "B"
	if "`b'" == "b" local c = "C"
	if "`b'" == "g" local c = "D"
	*if "`v'" == "g" local pref = "g"

	forvalues x = 1/7 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `c'`beta`x''=("`at`v'gb`b'var`x'_beta'")
		putexcel `c'`ci`x''=("(`at`v'gb`b'var`x'_lb', `at`v'gb`b'var`x'_ub')") 
		putexcel `c'`pv`x''=("[`at`v'gb`b'var`x'_pv']") 

	}
	putexcel `c'22=("`rat`v'gb_`b''")
	putexcel `c'23=("`Nat`v'gb_`b''")
}

putexcel save

	
}

* Test scores

loaddata
* High mother ed
recode edul_mo (1 2 = 0)(3 4 = 1), gen(momedhigh)
*recode edul_fa (1 2 = 0)(3 4 = 1), gen(dadedhigh)

* Baseline math ability
recode math_grade6 (1 2 = 0)(3 4 = 1), gen(himatabil)

* Parental ed gap
gen parentedgap = 0 if edul_mo != . & edul_fa != .
replace parentedgap = 1 if edul_mo > edul_fa & edul_mo != . & edul_fa != .

* Own parent beliefs
gen opb = mathbelief_gd

foreach var in momedhigh himatabil parentedgap opb {
	gen `var'xfem = `var'*female
	foreach b in a b g {
		gen `var'xppb`b' = `var'*ppb`b'
		gen `var'xppb`b'xfem = `var'*ppb`b'*female
	}
}

replace stdmat = stdmat/10
local varlist age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

foreach var in momedhigh himatabil parentedgap opb {
	if "`var'" == "momedhigh" local v = "meh"
	if "`var'" == "himatabil" local v = "hma"
	if "`var'" == "parentedgap" local v = "peg"
	if "`var'" == "opb" local v = "opb"
	if "`var'" == "momedhigh" local varname = "High maternal education"
	if "`var'" == "himatabil" local varname = "High baseline math ability"
	if "`var'" == "parentedgap" local varname = "Mother has more schooling than father"
	if "`var'" == "opb" local varname = "Own parent believes Bm > Gm"
	
	foreach b in a b g {

		reg stdmat ppb`b' ppb`b'xfemale `var' `var'xfem `var'xppb`b' `var'xppb`b'xfem female mathbelief_gd fem_belief_gd `varlist' if $cond1 ,vce(cluster grad_schid) absorb(grad_schid)	
	
		* Saving bits of the matrix for reporting
		matrix at`v'ts`b' = r(table)
		matrix list at`v'ts`b'
		forvalues x = 1/7 {
			local at`v'ts`b'var`x'_beta: di %6.3f at`v'ts`b'[1,`x']
			*display "Coefficient is `ts5ppb`b'var`x'_beta'"
			local at`v'ts`b'var`x'_lb: di %6.3f at`v'ts`b'[5,`x']
			local at`v'ts`b'var`x'_ub: di %6.3f at`v'ts`b'[6,`x']
			local at`v'ts`b'var`x'_pv: di %6.3f at`v'ts`b'[4,`x']
			local at`v'ts`b'var`x'_lb = subinstr("`at`v'ts`b'var`x'_lb'"," ","",.)
			local at`v'ts`b'var`x'_ub = subinstr("`at`v'ts`b'var`x'_ub'"," ","",.)
			local at`v'ts`b'var`x'_pv = subinstr("`at`v'ts`b'var`x'_pv'"," ","",.)
			}

	*test ppb`b' + ppb`b'xfemale  = 0
	
	local rat`v'ts_`b': di %9.3fc e(r2)
	local Nat`v'ts_`b': di %9.0fc e(N)

	}

putexcel set "$paperfolder/at`v'ts.csv", replace 
* Column 1 variable labels
putexcel A1=("Peer parent beliefs (PPB)") A4=("PPB x female") A7=("`varname'") 
putexcel A10=("`varname' x female") A13=("`varname' x PPB") A16=("`varname' x PPB x female") 
putexcel A19=("Female") A22=("R-squared") A23=("Observations")

* Coefficients

foreach b in a b g {
	
	if "`b'" == "a" local c = "B"
	if "`b'" == "b" local c = "C"
	if "`b'" == "g" local c = "D"
	*if "`v'" == "g" local pref = "g"

	forvalues x = 1/7 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `c'`beta`x''=("`at`v'ts`b'var`x'_beta'")
		putexcel `c'`ci`x''=("(`at`v'ts`b'var`x'_lb', `at`v'ts`b'var`x'_ub')") 
		putexcel `c'`pv`x''=("[`at`v'ts`b'var`x'_pv']") 

	}
	putexcel `c'22=("`rat`v'ts_`b''")
	putexcel `c'23=("`Nat`v'ts_`b''")
}

putexcel save

}


** Supplementary tables 16 and 17 - Top school vs not

loaddata

replace stdmat = stdmat / 10

gen nottopschool = 1 - topschool

local varlist female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog ta4_mat

foreach var in topschool nottopschool {
	
	if "`var'" == "topschool" local v = "st"
	if "`var'" == "nottopschool" local v = "sn"
	
	foreach b in a b g {
		
		reg stdmat ppb`b' ppb`b'xfemale mathbelief_gd fem_belief_gd `varlist' if $cond1 & `var' == 1 ,vce(cluster grad_schid) absorb(grad_schid)	

		* Saving bits of the matrix for reporting
		matrix at`v'gb`b' = r(table)
		matrix list at`v'gb`b'
		forvalues x = 1/5 {
			local at`v'gb`b'var`x'_beta: di %6.3f at`v'gb`b'[1,`x']
			*display "Coefficient is `ts5ppb`b'var`x'_beta'"
			local at`v'gb`b'var`x'_lb: di %6.3f at`v'gb`b'[5,`x']
			local at`v'gb`b'var`x'_ub: di %6.3f at`v'gb`b'[6,`x']
			local at`v'gb`b'var`x'_pv: di %6.3f at`v'gb`b'[4,`x']
			local at`v'gb`b'var`x'_lb = subinstr("`at`v'gb`b'var`x'_lb'"," ","",.)
			local at`v'gb`b'var`x'_ub = subinstr("`at`v'gb`b'var`x'_ub'"," ","",.)
			local at`v'gb`b'var`x'_pv = subinstr("`at`v'gb`b'var`x'_pv'"," ","",.)
			}

	*test ppb`b' + ppb`b'xfemale  = 0
	
	local rat`v'gb_`b': di %9.3fc e(r2)
	local Nat`v'gb_`b': di %9.0fc e(N)

	}


putexcel set "$paperfolder/at`v'ts.csv", replace 
* Column 1 variable labels
putexcel A1=("Peer parent beliefs (PPB)") A4=("PPB x female") A7=("Own parent beliefs (OPB)") 
putexcel A10=("OPB x female") A13=("Female") A16=("R-squared") A17=("Observations")

* Coefficients

foreach b in a b g {
	
	if "`b'" == "a" local c = "B"
	if "`b'" == "b" local c = "C"
	if "`b'" == "g" local c = "D"
	*if "`v'" == "g" local pref = "g"

	forvalues x = 1/5 { 
		
		* Getting the rows right
		local beta`x'=1+(`x'-1)*3
		local ci`x' = `beta`x''+1
		local pv`x' = `beta`x''+2
		
		* Putting the locals for beta, ci, p-value
		putexcel `c'`beta`x''=("`at`v'gb`b'var`x'_beta'")
		putexcel `c'`ci`x''=("(`at`v'gb`b'var`x'_lb', `at`v'gb`b'var`x'_ub')") 
		putexcel `c'`pv`x''=("[`at`v'gb`b'var`x'_pv']") 

	}
	putexcel `c'16=("`rat`v'gb_`b''")
	putexcel `c'17=("`Nat`v'gb_`b''")
}

putexcel save

	
}	

*
*** Appendix figures
*

** Supplementary Figure 1 - distribution of scores by gender

loaddata

replace stdmat = (stdmat/10)-7
drop if abs(stdmat)>3.5
kdensity stdmat if female == 1, lp(dash) lw(thick) addplot(kdensity stdmat if female == 0) ///
	legend(order(1 "Girls" 2 "Boys") rows(1)) xti(Math test score) note("") ///
	graphregion(color(white)) ysc(noline) xlab(-3 -2 -1 0 1 2 3) ti("") legend(region(lcolor(white)))
graph export "$paperfolder/FigureS1.eps", as(eps) replace
graph export "$paperfolder/testscoredensity.png", as(png) replace


** Supplementary Figure S2 - describing parent beliefs

graph drop _all

use "$datafolder/peerpar", clear

keep if $cond1

** Panel A - Figure - raw data - just proportion of parents 

preserve
collapse classbias_raw, by(clsids)
gen percent =  classbias_raw*100
hist percent, w(2.5) freq xti(Percentage of parents who believe B{subscript:m} > G{subscript:m}) yti(Number of classrooms) graphregion(color(white)) ///
	ysc(noline) ylab(, angle(horizontal) notick) name(S2PanelA) ti("Panel A: Classroom average parent beliefs, raw data")
graph export "$paperfolder/FigureS2PanelA.eps", as(eps) replace
graph export "$paperfolder/distribution_pbraw.png", as(png) replace
restore

** Panel B - scatter plot of classroom pairs
 
use "$datafolder/peerpar", clear

keep if $cond1

* Collapsing to class-level mean peer parent beliefs
collapse ppb grad_schid, by(clsids)

* Generating class 1/2 for reshape
bysort grad_schid: gen clsnumb = _n
drop clsids

* Reshaping to grade-by-school observations, class 1/2
reshape wide ppb, i(grad_schid) j(clsnumb)
gen diff = ppb1 - ppb2
gen absdiff = abs(diff)

* Making the figure
graph twoway (scatter ppb1 ppb2, legend(off) xti("Standardized parent beliefs measure, class 1") ///
	yti("Standardized parent beliefs measure, class 2") graphregion(color(white)) ysc(noline) xlab(-3 -2 -1 0 1 2 3) ///
	ylab(-3 -2 -1 0 1 2, angle(horizontal) notick)) (function y=x, range(-3 2) lp(dash)), name(S2PanelB) ///
	ti("Panel B: Standardized classroom-level parent beliefs" "by within-school, within-grade classroom pair")
	
graph export "$paperfolder/FigureS2PanelB.eps", as(eps) replace


** Supplementary Figure 3 - histogram of c1c2 differences

use "$datafolder/peerpar", clear

keep if $cond1

collapse ppb grad_schid, by(clsids)

* Generating class 1/2 for reshape
bysort grad_schid: gen clsnumb = _n
drop clsids

* Reshaping to grade-by-school observations, class 1/2
reshape wide ppb, i(grad_schid) j(clsnumb)
gen diff = ppb1 - ppb2
gen absdiff = abs(diff)

* Making the figure
hist absdiff, graphregion(color(white)) w(.2) ///
	ysc(noline) xlab(0 1 2 3 4 5) ylab(, angle(horizontal) notick) freq ///
	xti("Absolute value: difference between proportion of parents" "who believe B{subscript:m} > G{subscript:m} in class 1 and in class 2")  
graph export "$paperfolder/FigureS3.eps", as(eps) replace
	graph export "$paperfolder/gradec1c2beldiffmagnitude.png", as(png) replace

	
*								
*** Permutation test to study relationship between opb and ppb
*

* Permutation test for "randvar"

* Load data

capture log close
set more off
clear 
set matsize 10000
	
set seed 1134
set maxvar 100000

program standardize
		syntax, invar(varname) outvar(name)
		sum `invar'
		local mu = r(mean)
		local sd = r(sd)
		gen `outvar' = (`invar' - `mu')/`sd'
	end

	
use "$datafolder/peerpar", clear

cd "$analysisfolder"

* Getting the denominator
gen one = 1 if female != .
by clsids, sort: egen n_kids = sum(one) 
drop one

* Posting the file
postfile listofcoeffs iteration ppbrv ppbrvxfemale using "ppb_rv", replace	

forvalues x = 1/1000 {
	
	* Generating the random variable
	gen rv`x' = round(runiform()-0.09,1)
	
	* Generating the other variables (standardized proportion of people with rv=1, that interacted with female)
	by clsids, sort: egen n_rv`x' = sum(rv`x')
	gen peersrv`x' = (n_rv`x' - rv`x')/(n_kids - 1) if female != .
	standardize, invar(peersrv`x') outvar(pprv`x')
	gen pprvxfemale`x' = pprv`x'*female
	
	* Running the regression
	local cond1 (ple1503 == 1 & hra05 == 2 & (grade9 == 0 | (ple17 == 5 & grade9 == 1))) & mathbelief_gd != . & math_grade6 != .
	
	local varlist2 female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog  ta4_mat

	reg rv`x' pprv`x' pprvxfemale`x' `varlist2' if `cond1' ,vce(cluster grad_schid) absorb(grad_schid)

	* Saving coefficient
	local beta_pprv`x' =_b[pprv`x']
	local beta_pprvxfemale`x' =_b[pprvxfemale`x']
	
	}

postclose listofcoeffs

use "$analysisfolder/ppb_rv", clear	
	
sum ppbrv

* Comparing to actual correlation
loaddata
local varlist2 female age i.edul_fa i.edul_fax i.edul_mo i.edul_mox agrhk agrhkxfemale nsibling nsiblingxfemale minority minorityxfemale hhinc_low hhinc_lowxfem female_math fsft_math awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math i.math_grade6 i.math_grade6x stcog  ta4_mat
reg mathbelief_gd ppb ppbxfemale `varlist2' if $cond1,vce(cluster grad_schid) absorb(grad_schid)


** Testing whether teacher gender influences parent beliefs

loaddata

keep if $t1sample

local varlist female age minority agrhk i.edul_fa i.edul_mo nsibling hhinc_low i.math_grade6 agrhkxfemale nsiblingxfemale minorityxfemale hhinc_lowxfem awardpro_math awardcity_math  teacol_math teachyr_math qual1_math age_math

reghdfe mathbelief_gd  female_math `varlist' if $cond1 , vce(cluster grad_schid) absorb(grad_schid)
			
* End.
