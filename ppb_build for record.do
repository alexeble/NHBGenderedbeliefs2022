/*

Date last modified: 2022-01-28

This code builds the data used to generate the analysis in
"Gendered beliefs about math ability transmit across generations through children's peers"
by Alex Eble and Feng Hu

*/

* Preamble

capture log close
set more off
clear *
set matsize 10000
	
global basefolder "***/build"
global datafolder "$basefolder/input/"
global analysisfolder "$basefolder/temp/"
global outputfolder "$basefolder/output/"

cd "$analysisfolder"

* Pulling data out of databank, storing in project folder

global CEPSfolder "***"
*unicode encoding set gb18030

cd "$CEPSfolder"
foreach var in student class parent school {
	clear
*	commenting out translation, since already done * unicode translate CEPS_`var'.dta, invalid  * Commented out because only need to do once
	use "$CEPSfolder/CEPS_`var'.dta", clear 
	save "$datafolder/CEPS_`var'", replace
	}

cd "$analysisfolder"

use "$datafolder/CEPS_student", clear

*================  individual characteristics

gen male = stsex
gen female = 1 - male

gen birthyr = a02a
gen birthmon = a02b

gen age = 2013 - birthyr
label variable age "2013 - birthyr"
* Note that 405 are missing birthyear

gen minority = 1 if a03 != .
replace minority = 0 if a03 == 1
label variable minority "dummy for being minority"
gen minorityxfemale = minority*female
* Missing 50

gen agrhk = sthktype
label variable agrhk "dummy for agricultural hukou"
gen agrhkxfemale = agrhk*female


*--- academic performance in grade 6 of primary school

gen math_grade6 = c1001 if c1001 != 5
label values math_grade6 LABX
label variable math_grade6 "recalled math performance in grade 6 of primary school"

gen chn_grade6 = c1002 if c1002 != 5
label values chn_grade6 LABX
label variable chn_grade6 "recalled Chinese performance in grade 6 of primary school"

gen eng_grade6 = c1003 if c1003 != 5
label values eng_grade6 LABX
label variable eng_grade6 "recalled English performance in grade 6 of primary school"

recode math_grade6 (1 2 = 1)(3 4 = 0), gen(math_grade6d)
recode chn_grade6 (1 2 = 1)(3 4 = 0), gen(chn_grade6d)
recode eng_grade6 (1 2 = 1)(3 4 = 0), gen(eng_grade6d)


*================  household characteristics

*--- parents' education

recode stmedu stfedu stprhedu (1 2 = 1)(3 = 2)(4 5 6 = 3)(7 8 9 = 4), gen(edul_mo edul_fa edul_mf)
label define edul 1 "primary education or below" 2 "middle school" 3 "high/technical/professional school" 4 "college or above"
label values edul_mo edul_fa edul_mf edul
label variable edul_mo "mother's education level"
label variable edul_fa "father's education level"
label variable edul_mf "the higher education level between the two parents"

recode stmedu stfedu stprhedu (1 = 0)(2 = 6)(3 = 9)(4 5 6 = 12)(7 = 15)(8 = 16)(9 = 19), gen(eduyr_mo eduyr_fa eduyr_mf)
label variable eduyr_mo "mother's years of schooling"
label variable eduyr_fa "father's years of schooling"
label variable eduyr_mf "mother/father's higher years of schooling"

tab edul_mo, gen(edul_mo)
tab edul_fa, gen(edul_fa)


*--- household income level: use variables "steco_5c" and "steco_3c" instead (AE: way fewer missing here too, 19,4xx/19,487)

gen hhinc_high = 1 if steco_3c == 3
replace hhinc_high = 0 if steco_3c == 1 | steco_3c == 2
label variable hhinc_high "dummy for high household income level"

gen hhinc_low = 1 if steco_3c == 1
replace hhinc_low = 0 if steco_3c == 2 | steco_3c == 3
label variable hhinc_low "dummy for low household income level"

gen hhinc_lowxfem = hhinc_low*female


*--- number of siblings

gen nsibling = stsib if b01 == 2
replace nsibling = 0 if b01 == 1
label variable nsibling "number of siblings, treating >=5 as 5"

gen nsiblingxfemale = nsibling*female


*================  covariates: class level

*--- class size

gen one = 1

sort clsids
by clsids: egen csize = sum(one)
label variable csize "class size"

drop one


*--- average cognitive ability scores of peers, excluding oneself

bysort clsids: egen sum_cog3pl = sum(cog3pl)
gen avg_cogabil = (sum_cog3pl - cog3pl)/(csize - 1)
label variable avg_cogabil "average cognitive ability scores of peers, excluding oneself"
gen avg_cogabilxfemale = avg_cogabil*female

* Generating "% peers' parents who are low income" variable

capture drop n_guards
gen one = 1 if hhinc_low != .
by clsids, sort: egen n_guards = sum(one) 
label variable n_guards "number of guardians who are low income"
drop one

* generate the variable "peerpar_hhinc_low"

by clsids, sort: egen n_hhinc_low = sum(hhinc_low)
label variable n_hhinc_low "number of parents in the class who are low income"

gen peerpar_hhinc_low = (n_hhinc_low - hhinc_low)/(n_guards - 1) if hhinc_low != .
replace peerpar_hhinc_low = n_hhinc_low/n_guards if hhinc_low == .
label variable peerpar_hhinc_low "proportion of peer parents who are low income"

gen peerpar_hhinc_lowxfemale = peerpar_hhinc_low*female


* Generating the "% peers' parents who are agricultural" variable

capture drop n_guards
gen one = 1 if agrhk != .
by clsids, sort: egen n_guards = sum(one) 
label variable n_guards "number of guardians who have agricultural hukou"
drop one

by clsids, sort: egen n_agrhk = sum(agrhk)
label variable n_agrhk "number of parents in the class who have agricultural hukou"

gen peerpar_agrhk = (n_agrhk - agrhk)/(n_guards - 1) if agrhk != .
replace peerpar_agrhk = n_agrhk/n_guards if agrhk == .
label variable peerpar_agrhk "proportion of peer parents who have agricultural hukou"

gen peerpar_agrhkxfemale = peerpar_agrhk*female

*--- grade-by-school id

gen grad_schid = schids * 10 + 7 if grade9 == 0
replace grad_schid = schids * 10 + 9 if grade9 == 1
label variable grad_schid "grade-by-school id"


* stereotypical beliefs about learning math

gen mathbelief_sf = 2 - c14
label variable mathbelief_sf "self: are boys better at learning math than girls?"


*--- average peers' gender attitudes towards math learning, excluding oneself

by clsids: egen sum_genderview = sum(mathbelief_sf)
gen avg_genderview = (sum_genderview - mathbelief_sf)/(csize - 1)
label variable avg_genderview "average gender views of peers, excluding oneself"

save peerpar, replace



*-------------------------------------------------------------
*              teacher data
*-------------------------------------------------------------

use "$datafolder/CEPS_class", clear

*--- teacher's individual information

* gender

gen male_math = 2 - matb01
replace male_math = 2 - hrc01 if hra01 == 1
label variable male_math "math teacher being male"

gen male_chn = 2 - chnb01
replace male_chn = 2 - hrc01 if hra01 == 2
label variable male_chn "Chinese teacher being male"

gen male_eng = 2 - engb01
replace male_eng = 2 - hrc01 if hra01 == 3
label variable male_eng "English teacher being male"

gen female_math = 1 - male_math
gen female_chn = 1 - male_chn
gen female_eng = 1 - male_eng


* age

gen age_math = 2013 - matb02 
replace age_math = 2013 - clshrbyr if hra01 == 1
label variable age_math "age of math teacher: 2013 - birth year"

gen age_chn = 2013 - chnb02 if chnb02 > 1900
replace age_chn = 2013 - clshrbyr if hra01 == 2
label variable age_chn "age of Chinese teacher: 2013 - birth year"

gen age_eng = 2013 - engb02 if engb02 > 1900
replace age_eng = 2013 - 1968 if engb02 == 68
replace age_eng = 2013 - clshrbyr if hra01 == 3
label variable age_eng "age of English teacher: 2013 - birth year"


* years of teaching experience

gen teachyr_math = matb07 
replace teachyr_math = hrc07 if hra01 == 1
label variable teachyr_math "teaching years of math teacher"

gen teachyr_chn = chnb07 
replace teachyr_chn = hrc07 if hra01 ==  2
label variable teachyr_chn "teaching years of Chinese teacher"

gen teachyr_eng = engb07 
replace teachyr_eng = hrc07 if hra01 ==  3
label variable teachyr_eng "teaching years of English teacher"


* qualification level

gen qual_math = matb12 
replace qual_math = hrc12 if hra01 == 1
label variable qual_math "qualification level of math teacher"

gen qual_chn = chnb12 
replace qual_chn = hrc12 if hra01 ==  2
label variable qual_chn "qualification level of Chinese teacher"

gen qual_eng = engb12 
replace qual_eng = hrc12 if hra01 ==  3
label variable qual_eng "qualification level of English teacher"

label values qual_math qual_chn qual_eng hrc12


* qualification level: senior

recode qual_math (0 1 2 3 = 0)(4 5 =1), gen(qual1_math)
label variable qual1_math "qualification level of math teacher: senior"

recode qual_chn (0 1 2 3 = 0)(4 5 =1), gen(qual1_chn)
label variable qual1_chn "qualification level of Chinese teacher: senior"

recode qual_eng (0 1 2 3 = 0)(4 5 =1), gen(qual1_eng)
label variable qual1_eng "qualification level of English teacher: senior"


* graduating from teachers' college 

gen teacol_math = 2 - matb05
replace teacol_math = 2 - hrc05 if hra01 == 1
label variable teacol_math "math teacher graduating from teachers' college"

gen teacol_chn = 2 - chnb05
replace teacol_chn = 2 - hrc05 if hra01 == 2
label variable teacol_chn "Chinese teacher graduating from teachers' college"

gen teacol_eng = 2 - engb05
replace teacol_eng = 2 - hrc05 if hra01 == 3
label variable teacol_eng "English teacher graduating from teachers' college"


* teaching awards

*/* math teacher */

gen awardpro_math = 0
replace awardpro_math = 1 if matb1601 == 1 | matb1602 == 1
replace awardpro_math = 1 if (hrc1701 == 1 | hrc1702 == 1) & hra01 == 1
label variable awardpro_math "math teacher having provincial or national level awards for teaching quality"

gen awardcity_math = 0
replace awardcity_math = 1 if matb1601 == 1 | matb1602 == 1 | matb1603 == 1
replace awardcity_math = 1 if (hrc1701 == 1 | hrc1702 == 1 | hrc1703 == 1) & hra01 == 1
label variable awardcity_math "math teacher having at least city level awards for teaching quality"

gen awardcou_math = 0
replace awardcou_math = 1 if matb1601 == 1 | matb1602 == 1 | matb1603 == 1 | matb1604 == 1
replace awardcou_math = 1 if (hrc1701 == 1 | hrc1702 == 1 | hrc1703 == 1 | hrc1704 == 1) & hra01 == 1
label variable awardcou_math "math teacher having at least county level awards for teaching quality"

gen awardsch_math = 0
replace awardsch_math = 1 if matb1601 == 1 | matb1602 == 1 | matb1603 == 1 | matb1604 == 1 | matb1605 == 1
replace awardsch_math = 1 if (hrc1701 == 1 | hrc1702 == 1 | hrc1703 == 1 | hrc1704 == 1 | hrc1705 == 1) & hra01 == 1
label variable awardsch_math "math teacher having at least school level awards for teaching quality"

* attitude 4: believe tiancai is most important

gen ta4_mat = 0 if mata1601 != . | hra1601 != .
replace ta4_mat = 1 if mata1601 == 3
replace ta4_mat = 0 if hra1601 < 3 & hra01 == 1
replace ta4_mat = 1 if hra1601 == 3 & hra01 == 1
label variable ta4_mat "Math teacher believes innate ability crucial for education"


save teacher, replace


*-------------------------------------------------------------
*              school data
*-------------------------------------------------------------

use "$datafolder/CEPS_school", clear

recode pla04 (1 2 3 4 = 0)(5 = 1), gen(topschool)
label variable topschool "Top school in prefecture"

save school, replace



*-------------------------------------------------------------
*              parent/guardian data
*-------------------------------------------------------------

use "$datafolder/CEPS_parent", clear


*------ stereotypical gender-specific beliefs about math

gen mathbelief_gd = 2 - bb13
label variable mathbelief_gd "guardian: are boys better at learning math than girls?"

gen parent_age = 2014-be01

save guardian, replace



*-------------------------------------------------------------
*              merge data
*-------------------------------------------------------------

*================ merge teacher's information into student's dataset

use peerpar, clear

merge m:1 clsids using teacher
drop _merge


*================ merge school information into student's dataset

merge m:1 schids using school
drop _merge


*================ merge guardian information into student's dataset

merge 1:1 ids using guardian
drop _merge

save peerpar, replace


*-------------------------------------------------------------
*              generate new variables for analysis
*-------------------------------------------------------------


*====== peer parents' gender-specific stereotypical beliefs about math learning: reported by parents

*--- fathers and mothers: excluding other guadians (7.5%)

* number of parents who answer gender-specific stereotypical beliefs question
gen one = 1 if mathbelief_gd != . & ba01 <= 4
by clsids, sort: egen n_parents = sum(one) 
label variable n_parents "number of parents who report gender-specific stereotypical beliefs"
drop one

* Generating own parent bias measure
gen mathbelief_gd_p = mathbelief_gd if ba01 <= 4
label variable mathbelief_gd_p "guardian(parent): are boys better at learning math than girls?"

* Generating number of children in class whose parents are biased
by clsids, sort: egen n_mathbelief_par1 = sum(mathbelief_gd_p) 
label variable n_mathbelief_par1 "number of parents in the class who display stereotypical beliefs: by parent"

* Generating class-level variation
gen classbias_raw = n_mathbelief_par1/n_parents
* Generating bias measure for those whose parents answered the question
gen peerpar_belief1 = (n_mathbelief_par1 - mathbelief_gd_p)/(n_parents - 1) if mathbelief_gd_p != .

* Replacing the missing value for those whose parents did not answer with the class average
replace peerpar_belief1 = n_mathbelief_par1/n_parents if mathbelief_gd_p == .
label variable peerpar_belief1 "peer parents' stereotypical beliefs about math learning: by parent"

* Generating the standardized variable
standardize, invar(peerpar_belief1) outvar(ppb)
label var ppb "Proportion of peer parents with stereotype, excluding non-parent guardians" 


*--- Peer boys' and peer girls' parent beliefs

* Boys

* number of fathers who report gender-specific stereotypical beliefs

* number of boys' parents who answer the gender-specific stereotypical beliefs question
gen one = 1 if mathbelief_gd != . & ba01 <= 4 & female == 0
by clsids, sort: egen n_parents_b = sum(one) 
label variable n_parents_b "number of boys' parents who answer gender-specific stereotypical beliefs question"
drop one

* Generating child-specific variable for all students who are boys
gen mathbelief_b = mathbelief_gd_p if female == 0
label variable mathbelief_b "Peer boys' parents: are boys better at learning math than girls?"

* Counting the class-specific number of these parents who hold the bias
by clsids, sort: egen n_mathbelief_b = sum(mathbelief_b) 
label variable n_mathbelief_b "number of peer boys' parents in the class who display stereotypical beliefs"

* Generating the 0 to 1 gender bias measure: dividing the class-specific sum of biases by the number of
	*  boys' parents who responded
gen peerpar_belief_b = (n_mathbelief_b - mathbelief_b)/(n_parents_b - 1) if mathbelief_b != .

* Now that the class-specific variable is generated, inputting it for those whose parents did not respond
replace peerpar_belief_b = n_mathbelief_b/n_parents_b if mathbelief_b == .

label variable peerpar_belief_b "peer boys' parents' stereotypical beliefs about math learning"

standardize, invar(peerpar_belief_b) outvar(ppbb)
label var ppbb "Proportion of peer boys' parents with stereotype" 

gen ppbbxfemale = ppbb*female


* Girls

* number of fathers who report gender-specific stereotypical beliefs

* number of boys' parents who answer the gender-specific stereotypical beliefs question
gen one = 1 if mathbelief_gd != . & ba01 <= 4 & female == 1
by clsids, sort: egen n_parents_g = sum(one) 
label variable n_parents_g "number of girls' parents who answer gender-specific stereotypical beliefs question"
drop one

* Generating child-specific variable for all students who are girls
gen mathbelief_g = mathbelief_gd_p if female == 1
label variable mathbelief_g "Peer girls' parents: are boys better at learning math than girls?"

* Counting the class-specific number of these parents who hold the bias
by clsids, sort: egen n_mathbelief_g = sum(mathbelief_g) 
label variable n_mathbelief_g "number of peer girls' parents in the class who display stereotypical beliefs"

* Generating the 0 to 1 gender bias measure: dividing the class-specific sum of biases by the number of
	*  boys' parents who responded
gen peerpar_belief_g = (n_mathbelief_g - mathbelief_g)/(n_parents_g - 1) if mathbelief_g != .

* Now that the class-specific variable is generated, inputting it for those whose parents did not respond
replace peerpar_belief_g = n_mathbelief_g/n_parents_g if mathbelief_g == .

label variable peerpar_belief_g "peer girls' parents' stereotypical beliefs about math learning"

standardize, invar(peerpar_belief_g) outvar(ppbg)
label var ppbg "Proportion of peer girls' parents with stereotype" 

gen ppbgxfemale = ppbg*female


*--- fathers

* number of fathers who report gender-specific stereotypical beliefs

gen one = 1 if mathbelief_gd != . & (ba01 == 1 | ba01 == 3)
by clsids, sort: egen n_fathers = sum(one) 
label variable n_fathers "number of fathers who report gender-specific stereotypical beliefs"
drop one

* generate the variable "peerpar_belief1f"

gen mathbelief_gd_f = mathbelief_gd if (ba01 == 1 | ba01 == 3)
label variable mathbelief_gd_f "guardian(father): are boys better at learning math than girls?"

by clsids, sort: egen n_mathbelief_par1f = sum(mathbelief_gd_f) 
label variable n_mathbelief_par1f "number of fathers in the class who display stereotypical beliefs: by father"

gen peerpar_belief1f = (n_mathbelief_par1f - mathbelief_gd_f)/(n_fathers - 1) if mathbelief_gd_f != .
replace peerpar_belief1f = n_mathbelief_par1f/n_fathers if mathbelief_gd_f == .
label variable peerpar_belief1f "peer fathers' stereotypical beliefs about math learning: by father"

*--- mothers

* number of mothers who report gender-specific stereotypical beliefs

gen one = 1 if mathbelief_gd != . & (ba01 == 2 | ba01 == 4)
by clsids, sort: egen n_mothers = sum(one) 
label variable n_mothers "number of mothers who report gender-specific stereotypical beliefs"
drop one

* generate the variable "peerpar_belief1f"

gen mathbelief_gd_m = mathbelief_gd if (ba01 == 2 | ba01 == 4)
label variable mathbelief_gd_m "guardian(mother): are boys better at learning math than girls?"

by clsids, sort: egen n_mathbelief_par1m = sum(mathbelief_gd_m) 
label variable n_mathbelief_par1m "number of mothers in the class who display stereotypical beliefs: by mother"

gen peerpar_belief1m = (n_mathbelief_par1m - mathbelief_gd_m)/(n_mothers - 1) if mathbelief_gd_m != .
replace peerpar_belief1m = n_mathbelief_par1m/n_mothers if mathbelief_gd_m == .
label variable peerpar_belief1m "peer mothers' stereotypical beliefs about math learning: by mother"


*====== proportion of class female

by clsids, sort: egen n_female = sum(female)
label variable n_female "number of girl students in class"

gen propfemale = (n_female - female)/(csize - 1)
label variable propfemale "proportion female"
gen propfemalexfemale = propfemale*female


*====== class level grades 
	
* Leave-one-out average - math only

gen stdmat_f = stdmat if female == 1 // girl's math score
gen stdmat_m = stdmat if female == 0 // boy's math score

gen one = 0
replace one = 1 if female == 1

sort clsids
by clsids: egen n_girl = sum(one)
label variable n_girl "number of girls in the class"
drop one

gen n_boy = csize - n_girl
label variable n_boy "number of boys in the class"

by clsids: egen sum_stdmat_f = sum(stdmat_f)
gen peermath_f = sum_stdmat_f/n_girl
replace peermath_f =  (sum_stdmat_f-stdmat)/(n_girl-1) if female == 1
label variable peermath_f "average math scores of peer girls"

by clsids: egen sum_stdmat_m = sum(stdmat_m)
gen peermath_m = sum_stdmat_m/n_boy
replace peermath_m =  (sum_stdmat_m-stdmat)/(n_boy-1) if female == 0
label variable peermath_m "average math scores of peer boys"

gen peermathgap =  peermath_m - peermath_f
label variable peermathgap "math gender gap of peers at the class level"

gen peermathgap_fem =  peermathgap*female

drop stdmat_f stdmat_m n_girl n_boy sum_stdmat_f sum_stdmat_m /* g6mat_f g6mat_m sum_g6mat_f sum_g6mat_m g6rank_f g6rank_m sum_g6rank_f sum_g6rank_m */
	


*================ generate other variables for analysis

*====== peer parents' years of education

* fathers

by clsids, sort: egen n_eduyr_fa = sum(eduyr_fa)
label variable n_eduyr_fa "sum of fathers' years of education"

gen av_eduyr_peerfa = (n_eduyr_fa - eduyr_fa)/(csize - 1)
label variable av_eduyr_peerfa "average years of education for peer fathers"

* mothers

by clsids, sort: egen n_eduyr_mo = sum(eduyr_mo)
label variable n_eduyr_mo "sum of mothers' years of education"

gen av_eduyr_peermo = (n_eduyr_mo - eduyr_mo)/(csize - 1)
label variable av_eduyr_peermo "average years of education for peer mothers"


*====== interactions

gen ppbxfemale = ppb*female
label variable ppbxfemale "female * ppb"

gen fem_belief_gd = female * mathbelief_gd
label variable fem_belief_gd "female * mathbelief_gd"

gen fem_avg_genderview = female * avg_genderview
label variable fem_avg_genderview "female * avg_genderview"


gen fem_av_eduyr_peerfa = female * av_eduyr_peerfa
label variable fem_av_eduyr_peerfa "female * av_eduyr_peerfa"

gen fem_av_eduyr_peermo = female * av_eduyr_peermo
label variable fem_av_eduyr_peermo "female * av_eduyr_peermo"

gen fsft_math = female * female_math
label variable fsft_math "female * female_math"


*====== dummies for self-rated math performance at grade 6

tab math_grade6, gen(math_grade6)

save peerpar, replace

use peerpar, clear

gen inv_test =  150 - stdmat
sort clsids inv_test
by clsids: gen studrank = _n
order clsids studrank inv_test stdmat
keep if studrank == 1
keep clsids studrank male
rename male tsm
sort clsids
save topstud_gender, replace


use peerpar, clear

gen inv_test =  150 - stdmat
sort clsids inv_test
by clsids: gen studrank = _n
order clsids studrank inv_test stdmat
keep if studrank <= 3
keep clsids male
bysort clsids: gen id = _n
tab id
reshape wide male, i(clsids) j(id)
egen t3sm = rowtotal(male*)
*replace t3sm = t3sm
sort clsids
save t3stud_gender, replace


use peerpar, clear

sort clsids
merge m:1 clsids using topstud_gender
gen tsmxfemale = tsm*female
drop _merge

sort clsids
merge m:1 clsids using t3stud_gender
gen t3smxfemale = t3sm*female
drop _merge

label var tsm "Top student is male"
label var tsmxf "Top student is male x obs is female"

label var tsm "Proportion of top three students who are male"
label var tsmxf "Proportion of top three students who are male x obs is female"

* Additional interactions:

gen ppba = ppb
gen ppbaxfemale = ppb*female

gen math_grade6xfemale = math_grade6*female

foreach var in edul_mo edul_fa {
	gen `var'xfemale = `var'*female
}

sort ids

save "$outputfolder/peerpar", replace


*================ generate new class-level datasets for analysis

use peerpar, clear

sort clsids
by clsids: gen count1 = _n
by clsids: gen count2 = _N
keep if count1 == count2
drop count1 count2

save "$outputfolder/peerpar_class", replace


* Put here to allow for space between last line of code and bottom of window
