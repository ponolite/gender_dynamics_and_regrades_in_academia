/*******************************************************************************
Main Tables and Figures

Part 1 of the program (Tables 1, 2, 3 and Figure 1.) requires access to 
restricted administrative CSU transcript data. Once researchers obtain the data
license directly from Colorado State University, be sure to run 0_dataprep.do 
to produce the finaldata.dta file which is required to run Part 1 of the 
program.


Restricted administrative data can be applied at Colorado State University:
1) Transcript data: Registrar office (registrarsoffice@colostate.edu)
2) Instructor data: Institutional Research office ((970) 491-7009)

Change the $Working directory to your own directory before running the program.

Version: V1.0	
Last Edited: 12/04/2021
Stata Version: STATA/MP 16.1
Edited by: Cher Li (cher.hh.li@gmail.com)
*******************************************************************************/

clear
clear matrix
clear mata
set more off
set maxvar 10000
set matsize 10000


/* Global Directories */
/* Note: Be sure to change the following directory of Working to where you save 
the DataCodes folder*/
global Working "/Users/cherli/Documents/Research/CSUTranscripts/AEJPolicy/FinalSubmission/DataCode"
global Data "$Working/Data"
global Output "$Working/Output"

capture log close
log using $Output/tables.log, replace

/******************************************************************************/
/*                       Part 1. Administrative Data                          */
/******************************************************************************/
use  $Data/finaldata.dta, clear


/*======== Figure 1. Upward grade changes conditional on initial grade =======*/

/* 1.	Plot the proportion of each gender asking for a regrade at each cost. 
        So the x-axis is the regrade cost (going from -1 to 3.5). 
		And then at each value, show the percent of males and females who 
		ask for a regrade. */

tabstat posgradechange, by(init_grade2) save

mat A1 = [1,2,3,4,5,6,7,8,9,10]

mat A2 = 100 * [r(Stat1),r(Stat2),r(Stat3),r(Stat4),r(Stat5), ///
			r(Stat6),r(Stat7),r(Stat8),r(Stat9),r(Stat10)]
matmap A2 A3, map(round(@, 0.001))	
mat A = [A1', A3']
mat list A

svmat A
		
label define gradelbl 1 "A+"
label define gradelbl 2 "A", add
label define gradelbl 3 "A-", add
label define gradelbl 4 "B+", add
label define gradelbl 5 "B", add
label define gradelbl 6 "B-", add
label define gradelbl 7 "C+", add
label define gradelbl 8 "C", add
label define gradelbl 9 "D", add
label define gradelbl 10 "F", add

label values A1 gradelbl

//mat colnames A = "A+" "A" "A-" "B+" "B" "B-" "C+" "C" "D" "F"
twoway (bar A2 A1, barwidth(0.5)) || ///
	scatter A2 A1, ms(none) mlabcolor(black)  mlabel(A2) mlabpos(12) ///
	ytitle(Percent positive grade changes (%), ///
	margin(small)) xtitle(Initial grade) ///
	ylabel(0(0.5)1.5)  xlabel(1(1)10, labels valuelabel) legend(off)
mat drop _all
graph export $Output/figure1.pdf, replace

/*============ Table 1. Summary stats of administrative records ==============*/
mat drop _all
mat All0 = 100

*** All

tabstat gradechange posgradechange neggradechange  ///
inst_female, stats(mean sd) save
* change fraction to percent
mat All1 = 100 * r(StatTotal)'
scalar posgradechange_admin = All1[2,1]

tabstat daysofchange if gradechange==1, stats(mean sd) save
mat All2 = r(StatTotal)'
tabstat cum_gpa , stats(mean sd) save
mat All2 = All2 \ r(StatTotal)'

tabstat ///
col_ag col_bu col_eg col_hs ///
col_la col_nr col_ns col_vm col_iu ///
, stats(mean) save
* change fraction to percent
mat All3 = 100 * r(StatTotal)'

*** By Student Gender

tabstat std_female std_male ///
, stats(mean) save
mat temp = r(StatTotal)
mat Male0 = 100 * temp[1,2]
mat Female0 = 100 * temp[1,1]

/*
tabstat gradechange posgradechange neggradechange  ///
inst_female ///
, stats(mean sd) 
*/

tabstat gradechange posgradechange neggradechange  ///
inst_female ///
, by(std_fem) stats(mean sd) save
* change fraction to percent
mat Male1 = 100 * r(Stat1)'
mat Female1 = 100 * r(Stat2)'


tabstat daysofchange  if gradechange==1 ///
, by(std_fem) stats(mean sd) save
mat Male2 = r(Stat1)'
mat Female2 = r(Stat2)'

tabstat cum_gpa ///
, by(std_fem) stats(mean sd) save
mat Male2 = Male2 \ r(Stat1)'
mat Female2 = Female2 \ r(Stat2)'

tabstat ///
col_ag col_bu col_eg col_hs ///
col_la col_nr col_ns col_vm col_iu ///
, by(std_fem) stats(mean) save
* change fraction to percent
mat Male3 = 100 * r(Stat1)'
mat Female3 = 100 * r(Stat2)'

local vars1 	gradechange posgradechange neggradechange ///
		inst_female 
		
		
local 	nvars1: word count `vars1'
matrix diff1 = J(`nvars1',2,.)
matrix star1 = J(`nvars1',2,0)
forvalues i=1/`nvars1' {
local var1_`i': word `i' of `vars1'
		reg `var1_`i'' std_female
		mat temp = r(table)
		mat diff1[`i',1] = 100 * temp[1,1]
		mat diff1[`i',2] = 100 * temp[2,1]
		mat star1[`i',1] = (temp[4,1]<0.1)+ (temp[4,1]<0.05)+ (temp[4,1]<0.01)
}

local vars2 	daysofchange cum_gpa 
		
local 	nvars2: word count `vars2'
matrix diff2 = J(`nvars2',2,.)
matrix star2 = J(`nvars2',2,0)
reg daysofchange std_female if gradechange==1
		mat temp = r(table)
		mat diff2[1,1] = temp[1,1]
		mat diff2[1,2] = temp[2,1]
		mat star2[1,1] = (temp[4,1]<0.1)+ (temp[4,1]<0.05)+ (temp[4,1]<0.01)
reg cum_gpa std_female if gradechange==1
		mat temp = r(table)
		mat diff2[2,1] = temp[1,1]
		mat diff2[2,2] = temp[2,1]
		mat star2[2,1] = (temp[4,1]<0.1)+ (temp[4,1]<0.05)+ (temp[4,1]<0.01)


local vars3 	///
		col_ag col_bu col_eg col_hs ///
		col_la col_nr col_ns col_vm col_iu 
		
		
local 	nvars3: word count `vars3'
matrix diff3 = J(`nvars3',1,.)
matrix star3 = J(`nvars3',1,0)
forvalues i=1/`nvars3' {
local var3_`i': word `i' of `vars3'
		reg `var3_`i'' std_female
		mat temp = r(table)
		mat diff3[`i',1] = 100 * temp[1,1]
		mat star3[`i',1] = (temp[4,1]<0.1)+ (temp[4,1]<0.05)+ (temp[4,1]<0.01)

}

tabstat std_id, stats(n) save
mat N = r(StatTotal)
scalar N_admin = N[1,1]
tabstat std_id, by(std_female) stats(n) save
mat N = [N,r(Stat2), r(Stat1),.]

tab inst_female, missing matcell(instsex)
local missinginstsex: display %9.0fc  instsex[3,1]

tab std_female inst_female, matcell(stdinstsex)
local stdfem_instfem: display %9.0fc  stdinstsex[2,1] + stdinstsex[2,2]
local stdm_instfem: display %9.0fc  stdinstsex[1,1] + stdinstsex[1,2]



* Column All (A)
#delimit;
frmttable using $Output/tab1, statmat(All0) plain ///
	substat(0) sdec(0) ctitle("","All") coljust(c) replace tex ///
	store(t2)
	rtitle("Percent (\%) of records");
	
frmttable using $Output/tab1, statmat(Female0) plain ///
	substat(0) sdec(1) ctitle("","Female") coljust(c) merge(t2) tex ///
	rtitle("Percent (\%) of records");
	
frmttable using $Output/tab1, statmat(Male0) plain ///
	substat(0) sdec(1) ctitle("","Male") coljust(c) merge(t2) tex
	rtitle("Percent (\%) of records");

#delimit;
frmttable using $Output/tab1a, statmat(All1) plain ///
	substat(1) sdec(3\2\3\2\3\2\1\1) ctitle("","All") coljust(c) replace tex ///
	store(t2a)
	rtitle("Grade change"\""\
	"Positive grade change"\""\
	"Negative grade change"\""\
	"Female instructor$^a$"\""
	);
	
frmttable using $Output/tab1a, statmat(Female1) plain ///
	substat(1) sdec(3\2\3\2\3\2\1\1) ctitle("","Female") coljust(c) merge(t2a) tex ///
	rtitle("Grade change"\""\
	"Positive grade change"\""\
	"Negative grade change"\""\
	"Female instructor$^a$"\"")
	;
	
frmttable using $Output/tab1a, statmat(Male1) plain ///
	substat(1) sdec(3\2\3\2\3\2\1\1) ctitle("","Male") coljust(c) merge(t2a) tex
	rtitle("Grade change"\""\
	"Positive grade change"\""\
	"Negative grade change"\""\
	"Female instructor$^a$"\"");

frmttable using $Output/tab1a, statmat(diff1) ///
	substat(1) sdec(3\3\3\3\3\3\1\3) ctitle("", "Gender Difference") ///
	rtitle("Grade change"\""\
	"Positive grade change"\""\
	"Negative grade change"\""\
	"Female instructor$^a$"\""
	)	
	plain annotate(star1) asymbol(*,**,***) 
	coljust(c) merge(t2a) tex sq;

* Column Female (B)
#delimit;
frmttable using $Output/tab1b, statmat(All2) plain ///
	substat(1) sdec(1\1\2\2) coljust(c) replace tex ///
	store(t2b) ctitle("","All") 
	rtitle(	"Days between grade changes"\""\ 
			"GPA"\"");
	
frmttable using $Output/tab1b, statmat(Female2) plain ///
	substat(1) sdec(1\1\2\2)  coljust(c) merge(t2b) tex ///
	rtitle(	"Days between grade changes"\""\ 
			"GPA"\"") ctitle("","Female") ;
	
frmttable using $Output/tab1b, statmat(Male2) plain ///
	substat(1) sdec(1\1\2\2)  coljust(c) merge(t2b) tex
	rtitle(	"Days between grade changes"\""\ 
			"GPA"\"") ctitle("","Male") ;

frmttable using $Output/tab1b, statmat(diff2) ///
	substat(1) sdec(2) ///
	rtitle(	"Days between grade changes"\""\ 
			"GPA"\"") ctitle("","Gender Difference") 
	plain annotate(star2) asymbol(*,**,***) 
	coljust(c) merge(t2b) tex sq;


* Column Male (C)
#delimit;
frmttable using $Output/tab1c, statmat(All3) plain ///
	substat(0) sdec(1) coljust(c) replace tex ///
	store(t2c) ctitle("","All") 
	rtitle("College of Agriculture"\
	"College of Business"\
	"College of Engineering"\
	"College of Human Sciences"\
	"College of Liberal Arts"\
	"College of Natural Resources"\
	"College of Natural Sciences"\
	"College of Veterinary Sciences"\
	"Intra-University");
	
frmttable using $Output/tab1c, statmat(Female3) plain ///
	substat(0) sdec(1)  coljust(c) merge(t2c) tex ///
	rtitle("College of Agriculture"\
	"College of Business"\
	"College of Engineering"\
	"College of Human Sciences"\
	"College of Liberal Arts"\
	"College of Natural Resources"\
	"College of Natural Sciences"\
	"College of Veterinary Sciences"\
	"Intra-University") ctitle("","Female") ;
	
frmttable using $Output/tab1c, statmat(Male3) plain ///
	substat(0) sdec(1)  coljust(c) merge(t2c) tex
	rtitle("College of Agriculture"\
	"College of Business"\
	"College of Engineering"\
	"College of Human Sciences"\
	"College of Liberal Arts"\
	"College of Natural Resources"\
	"College of Natural Sciences"\
	"College of Veterinary Sciences"\
	"Intra-University") ctitle("","Male") ;

frmttable using $Output/tab1c, statmat(diff3) ///
	substat(0) sdec(2) ///
	rtitle("College of Agriculture"\
	"College of Business"\
	"College of Engineering"\
	"College of Human Sciences"\
	"College of Liberal Arts"\
	"College of Natural Resources"\
	"College of Natural Sciences"\
	"College of Veterinary Sciences"\
	"Intra-University") ctitle("","Gender Difference") 
	plain annotate(star3) asymbol(*,**,***) 
	coljust(c) merge(t2c) tex sq;
#delimit cr

* Combine Columns

frmttable using $Output/tab1_summarystats, plain tex replay(t2) ///
	replace ctitles("","Students","","",""\ ///
	"","All","Female","Male","Gender Difference") ///
	addrows("Percent (\%) grades with a...")
	   
frmttable using $Output/tab1_summarystats, plain tex append(t2a) ///
	replace
	
	
frmttable using $Output/tab1_summarystats, plain tex append(t2b) ///
	addrows("Percent (\%) in...")
	
frmttable using $Output/tab1_summarystats, plain tex append(t2c) landscape
#delimit;	
frmttable using $Output/tab1_summarystats, plain tex statmat(N) append 
	note("$* p<0.10, ** p<0.05, *** p<0.01.$ Standard deviations are in parentheses. 
	Standard errors are in square brackets.
	a.`missinginstsex' administrative records have missing values for instructors' gender.
	The numbers of observations `stdfem_instfem' and `stdm_instfem' for female and 
	male student records, respectively.") sdec(0)
	title("Summary statistics of administrative data");
#delimit cr

erase $Output/tab1.tex
erase $Output/tab1a.tex
erase $Output/tab1b.tex
erase $Output/tab1c.tex


/*======== Table 2. Regrade regression based on administrative records =======*/
mat drop _all
* ---------------        Panel A. Upward grade changes            --------------

* 1) Male student
su posgradechange

reg posgradechange i.std_male, vce(cluster std_id)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01) 
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable, clear(reg1)
frmttable using $Output/regup, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape replace tex store(reg1) addrows(Observations, "`N'")

	   
* 2) Male stduent + Student class standing, discretized GPA, initial grade 	   
reg posgradechange i.std_male i.std_class i.GPA i.init_grade2 ///
	dist_termgpa, vce(cluster std_id)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape tex merge(reg1) addrows(Observations, "`N'")
	   
* 3) Male student + Coleges & Departments & Instructor sex and ranking
reg posgradechange i.std_male i.inst_female i.inst_position ///
	i.college i.CLAS_DEPT_CODE , vce(cluster std_id)	 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape tex merge(reg1) addrows(Observations, "`N'")	   

* 4) Male student + Class fixed effects
areg posgradechange i.std_male, vce(cluster std_id) a(crn)	 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape tex merge(reg1) addrows(Observations, "`N'")	   
	   
* 5) All controls
reg posgradechange i.std_male i.std_class i.GPA i.init_grade2 ///
	dist_termgpa i.inst_female i.inst_position i.college i.CLAS_DEPT_CODE ///
	, vce(cluster std_id) 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape tex merge(reg1) addrows(Observations, "`N'")
	   
	   
* 6) All controls + (discretized GPA x Grade x Dept)
areg posgradechange i.std_male i.std_class ///
	dist_termgpa i.inst_female i.inst_position i.college ///
	, vce(cluster std_id) a(GPAxGradexDept)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape tex merge(reg1) addrows(Observations, "`N'")	   	   
	   	   
	 


* -----------            Panel B. Downward grade changes      ------------------

* 1) Male student
reg neggradechange i.std_male, vce(cluster std_id)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01) 
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable, clear(reg2)
frmttable using $Output/regdown, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape replace tex store(reg2) addrows(Observations, "`N'")

	   
* 2) Male stduent + Student class standing, GPA, initial grade, probation 	   
reg neggradechange i.std_male i.std_class i.GPA i.init_grade2 dist_termgpa, ///
	vce(cluster std_id)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student)  ///
	   landscape tex merge(reg2) addrows(Observations, "`N'")
	   
* 3) Male student + Coleges & Departments & Instructor sex and ranking
reg neggradechange i.std_male i.inst_female i.inst_position ///
	i.college i.CLAS_DEPT_CODE , vce(cluster std_id)	 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student)  ///
	   landscape tex merge(reg2) addrows(Observations, "`N'")	   

* 4) Male student + Class fixed effects
areg neggradechange i.std_male, vce(cluster std_id) a(crn)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape tex merge(reg2) addrows(Observations, "`N'")	   

* 5) All controls
reg neggradechange i.std_male i.std_class i.GPA i.init_grade2 dist_termgpa ///
	i.inst_female i.inst_position i.college i.CLAS_DEPT_CODE ///
	, vce(cluster std_id) 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student)  ///
	   landscape tex merge(reg2) addrows(Observations, "`N'")
	   
	   
* 6) All controls + (discretized GPA x Grade x Dept)
areg neggradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	, vce(cluster std_id) a(GPAxGradexDept)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) ///
 	   rtitle(Male student) ///
	   landscape tex merge(reg2) addrows(Observations, "`N'")	   	   
	   	   

/* Combine Panels A and B */

su posgradechange
local Yplus = round(100*r(mean), .001)
su neggradechange
local Yminus = round(100*r(mean), .001)
	   
frmttable using $Output/tab2_reg, plain tex replay(reg1) landscape replace ///
	ctitle("","[1]","[2]","[3]","[4]","[5]","[6]"\ ///
	"\textbf{A. Dependent variable: Upward grade change $Y\in\{0,1\},\bar{Y}=`Yplus'\%$}" ///
	,"","","","","","") ///
	addrows( ///
	"\textbf{B. Dependent variable: Downward grade change $Y\in\{0,1\},\bar{Y}=`Yminus'\%$}" ///
	,"","","","","","") 
	
	
frmttable using $Output/tab2_reg, plain tex append(reg2) landscape ///
	addrows("Student controls$^{a}$", "No", "Yes", "No", "No", "Yes", "Yes"\ ///
			"Class controls$^{b}$", "No", "No", "Yes", "No", "Yes", "Yes"\ ///
			"Class fixed effects", "No", "No", "No", "Yes", "No", "No"\ /// 
			"GPA $\times$ Grade $\times$ Dept$^{c}$", "No", "No", "No", "No", "No", "Yes") ///
			posttext("Coefficients and standard errors are multiplied by 100 to be read as percentage points. \$* p<0.1; ** p<0.05; *** p<0.01.\$ Standard errors are clustered at the student level and reported in parentheses." ///
			"Male-to-female odds ratios (calculated as $\frac{Pr_{Male}(Y=1)/Pr_{Male}(Y=0)}{Pr_{Female}(Y=1)/Pr_{Female}(Y=0)}$ assuming the mean values for all variables other than \textit{Male} student are reported in brackets." ///
			"a. Student controls include: student's class standing (i.e., freshman, sophomore, junior, senior), discretized GPA (corresponds to letter grades), and initial grade." ///
			"b. Class controls include: college, department, instructors' gender and rank." ///
			"c. GPA $\times$ Grade $\times$ Dept includes 4,500 indicators for the full interactions of discretized GPAs (corresponds to letter grades), initial grades, and departments.") ///
			multicol(2,1,6; 7,1,6) coljust(lccccc) ///
			hlines(1010000100010001) 

			
erase $Output/regup.tex
erase $Output/regdown.tex

/*====== Table 3. Sensitivity Analysis based on administrative records =======*/
 
* --------------         Panel A. Upward sensitivity           -----------------
	
* 1) Baseline: all controls + (i.GPA x i.Grade x i.Dept)
areg posgradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	, vce(cluster std_id) a(GPAxGradexDept)	
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable, clear(reg3)
frmttable using $Output/regup_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape replace tex addrows(Observations, "`N'") store(reg3) 
	   
	   
* 2) Logit: all controls + (c.GPA x c.Grade x i.Dept)
logit posgradechange i.std_male i.std_class cum_gpa init_ngrade dist_termgpa ///
	i.CLAS_DEPT_CODE c.cum_gpa#c.init_ngrade#i.CLAS_DEPT_CODE  ///
	i.inst_female i.inst_position i.college ///
	, vce(cluster std_id) or
mat drop _all
mat A = r(table)
mat oddsratio = A[1,2]	
mat drop A
margins, dydx(std_male) vce(unconditional)
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
mat coef_se = coef_se, oddsratio
frmttable using $Output/regup_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg3) addrows(Observations, "`N'") 
	   
* 3) Remove students who got 2+ upward changes
areg posgradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if std_totposgradechange<=1, vce(cluster std_id) a(GPAxGradexDept)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg3) addrows(Observations, "`N'") 
	   
* 4) Remove instructors who made changes for 10%+ of all grades
areg posgradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if inst_pctgradechange<=0.10, vce(cluster std_id)  a(GPAxGradexDept)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg3) addrows(Observations, "`N'") 
	   
 	   
* 5) Remove A+ and A
areg posgradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if init_grade2>=3, vce(cluster std_id)  a(GPAxGradexDept)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg3) addrows(Observations, "`N'") 
	   
* 6) Remove F
areg posgradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if init_grade2<10, vce(cluster std_id) a(GPAxGradexDept) 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg3) addrows(Observations, "`N'") 

* 7) Large class
areg posgradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if largeclass==1, vce(cluster std_id) a(GPAxGradexDept) 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regup_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg3) addrows(Observations, "`N'") 
	   
* -------------         Panel B. Downward sensitivity           ----------------
 

* 1) Baseline: all controls + (Probation x Grade x Dept)
areg neggradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	, vce(cluster std_id) a(GPAxGradexDept)	
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable, clear(reg4)
frmttable using $Output/regdown_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape replace tex addrows(Observations, "`N'") store(reg4) 
   
* 2) Logit: all controls + (c.GPA x c.Grade x i.Dept)
logit neggradechange i.std_male i.std_class cum_gpa init_ngrade dist_termgpa ///
	i.CLAS_DEPT_CODE c.cum_gpa#c.init_ngrade#i.CLAS_DEPT_CODE  ///
	i.inst_female i.inst_position i.college ///
	, vce(cluster std_id) or
mat drop _all
mat A = r(table)
mat oddsratio = A[1,2]	
mat drop A
margins, dydx(std_male) vce(unconditional)
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
mat coef_se = coef_se, oddsratio
frmttable using $Output/regdown_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg4) addrows(Observations, "`N'") 
	   
* 3) Remove students who got 2+ upward changes
areg neggradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if std_totneggradechange<=1, vce(cluster std_id) a(GPAxGradexDept)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg4) addrows(Observations, "`N'")  
	   
* 4) Remove instructors who made changes for 10%+ of all grades
areg neggradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if inst_pctgradechange<=0.10, vce(cluster std_id)  a(GPAxGradexDept)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg4) addrows(Observations, "`N'") 
	   
 	   
* 5) Remove A+ and A
areg neggradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if init_grade2>=3, vce(cluster std_id)  a(GPAxGradexDept)
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg4) addrows(Observations, "`N'") 
	   
* 6) Remove F
areg neggradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if init_grade2<10, vce(cluster std_id) a(GPAxGradexDept) 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg4) addrows(Observations, "`N'") 
	   

* 7) Large class
areg neggradechange i.std_male i.std_class i.GPA dist_termgpa ///
	i.inst_female i.inst_position i.college ///
	if largeclass==1, vce(cluster std_id) a(GPAxGradexDept) 
mat drop _all
mat A = r(table)
mat coef_se = A[1,2], A[2,2]
mat coef_se = 100 * coef_se
mat star = [0,0,0]
mat star[1,1] = (A[4,2]<=.1) + (A[4,2]<=.05) + (A[4,2]<=.01)  
local N: display %9.0fc e(N)
margins std_male, atmeans noestimcheck
mat gen = r(b)
mat coef_se = coef_se, (gen[1,2]/(1-gen[1,2]))/(gen[1,1]/(1-gen[1,1]))
frmttable using $Output/regdown_sens, statmat(coef_se) substat(2) sdec(4\4\3) ///
	   plain annotate(star) asymbol(*,**,***) rtitle(Male student) ///
	   landscape tex merge(reg4) addrows(Observations, "`N'") 
   
* ---------------         Combine panels into table           ------------------
 
su posgradechange
local Yplus = round(100*r(mean), .001)
su neggradechange
local Yminus = round(100*r(mean), .001)

frmttable using $Output/tab3_sens, plain tex replay(reg3) landscape replace ///
	ctitle("", "[1]", "[2]", "[3]", "[4]", "[5]", "[6]", "[7]"\ ///
	"","Baseline","Logit$^{a}$","Excl. students", "Excl. instructors", "Excl. A+ and A", "Excl. F", "Large class", \ ///
	"", "", "","w/2+ changes", "w/10\%+ changes", "", "", "" \ ///
	"\textbf{A. Dependent variable: Upward grade change $Y\in\{0,1\},\bar{Y}=`Yplus'\%$}" ///
	,"","","","","","","") ///
	addrows("\textbf{B. Dependent variable: Downward grade change $Y\in\{0,1\},\bar{Y}=`Yminus'\%$}","","","","","","","") 


frmttable using $Output/tab3_sens, plain tex append(reg4) landscape	///
	multicol(4,1,8; 9,1,8) coljust(lccccccc) ///
	hlines(10001000010001) ///
	posttext("Coefficients and standard errors are multiplied by 100 to be read as percentage points." ///
	"\$* p<0.1; ** p<0.05; *** p<0.01.\$ Standard errors are clustered at the student level and reported in parentheses." ///
	"Male-to-female odds ratios (calculated as $\frac{P_{Male}(GradeChange=1)/P_{Male}(GradeChange=0)}{P_{Female}(GradeChange=1)/P_{Female}(GradeChange=0)}$ based on mean values for all other variables are reported in the square brackets." ///
	"All models control for students' class standing (i.e., freshman, sophomore, junior, senior), discretized GPA (corresponds to letter grades), and initial grade," ///
	"college, department, instructors' gender and rank," ///
	"and GPA $\times$ Grade $\times$ Dept indicators. a. Logit regression uses continuous values of GPA and Grade for convergence consideration.") 


erase $Output/regup_sens.tex
erase $Output/regdown_sens.tex


/******************************************************************************/
/*                        Part 2. Instructor Survey                           */
/******************************************************************************/
use $Data/instsurvey.dta, clear

* -------------   Figure 3.  Result distribution by student sex   --------------
 

* 1) End of Semester 

rename maleup_fn  up_fn0
rename femaleup_fn  up_fn1
rename malesame_fn  same_fn0
rename femalesame_fn  same_fn1
rename maledown_fn  down_fn0
rename femaledown_fn  down_fn1

reshape long up_fn same_fn down_fn, i(id) j(female)

svyset id  [weight=numstudents]

svy: reg up_fn female 
local pvalue_up_fn = ///
		round((2 * ttail(e(df_r), abs(_b[female]/_se[female]))), .01)

svy: reg same_fn female
local pvalue_same_fn = ///
		round((2 * ttail(e(df_r), abs(_b[female]/_se[female]))), .01)

svy: reg down_fn female
local pvalue_down_fn = ///
		round((2 * ttail(e(df_r), abs(_b[female]/_se[female]))), .01)


reshape wide up_fn same_fn down_fn, i(id) j(female)
rename up_fn0 maleup_fn
rename same_fn0 malesame_fn
rename down_fn0 maledown_fn
rename up_fn1 femaleup_fn
rename same_fn1 femalesame_fn
rename down_fn1 femaledown_fn
 

ttest maleup_fn = femaleup_fn if sample==1, unpaired
local n_fnmale = r(N_1)
local n_fnfemale = r(N_2)

graph bar (mean) maleup_fn malesame_fn maledown_fn ///
	[w=numstudents] if sample==1, ///
	bargap(10) blabel(bar, format(%3.1f)) ylabel(0(30)85) ///
	ytitle(Percent (%)) title("A. End of Semester, Male Students") ///
	ytitle(, margin(medium)) legend(off) ///
	bar(1,fintensity(inten60)) bar(3,fintensity(inten30)) fysize(28)
graph save $Output/male_fn.gph, replace

graph bar (mean) femaleup_fn femalesame_fn femaledown_fn ///
	[w=numstudents] if sample==1, ///
	bargap(10) blabel(bar, format(%3.1f)) ylabel(0(30)85) ///
	ytitle(Percent (%)) title("B. End of semester, Female Students") ///
	ytitle(, margin(medium)) legend(off) ///
	bar(1,fintensity(inten60)) bar(3,fintensity(inten30)) fysize(28)
graph save $Output/female_fn.gph, replace

* 2) During semester 

rename maleup_mt  up_mt0
rename femaleup_mt  up_mt1
rename malesame_mt  same_mt0
rename femalesame_mt  same_mt1
rename maledown_mt  down_mt0
rename femaledown_mt  down_mt1

reshape long up_mt same_mt down_mt, i(id) j(female)

svyset id  [weight=numstudents]

svy: reg up_mt female 
local pvalue_up_mt = ///
		round((2 * ttail(e(df_r), abs(_b[female]/_se[female]))), .01)

svy: reg same_mt female
local pvalue_same_mt = ///
		round((2 * ttail(e(df_r), abs(_b[female]/_se[female]))), .01)

svy: reg down_mt female
local pvalue_down_mt = ///
		round((2 * ttail(e(df_r), abs(_b[female]/_se[female]))), .01)


reshape wide up_mt same_mt down_mt, i(id) j(female)
rename up_mt0 maleup_mt
rename same_mt0 malesame_mt
rename down_mt0 maledown_mt
rename up_mt1 femaleup_mt
rename same_mt1 femalesame_mt
rename down_mt1 femaledown_mt
 
ttest maleup_mt = femaleup_mt if sample==1, unpaired
local n_mtmale = r(N_1)
local n_mtfemale = r(N_2)

graph bar (mean) maleup_mt malesame_mt maledown_mt ///
	[w=numstudents] if sample==1, ///
	bargap(10) blabel(bar, format(%3.1f)) ylabel(0(30)70) ///
	ytitle(Percent (%)) title("C. During Semester, Male Students") ///
	ytitle(, margin(medium)) ///
	legend(order(1 "Grade increase" 2 "No change" 3 "Grade decrease") rows(3)) ///
	bar(1,fintensity(inten60)) bar(3,fintensity(inten30))
graph save $Output/male_mt.gph, replace

graph bar (mean) femaleup_mt femalesame_mt femaledown_mt ///
	[w=numstudents] if sample==1, ///
	bargap(10) blabel(bar, format(%3.1f)) ylabel(0(30)70) ///
	ytitle(Percent (%)) title("D. During Semester, Female Students") ///
	ytitle(, margin(medium)) ///
	legend(order(1 "Grade increase" 2 "No change" 3 "Grade decrease") rows(3)) ///
	bar(1,fintensity(inten60)) bar(3,fintensity(inten30))
graph save $Output/female_mt.gph, replace


* 3) Combine graphs

#delimit;
graph combine $Output/male_fn.gph $Output/female_fn.gph ///
$Output/male_mt.gph $Output/female_mt.gph, ///
title("Instructor survey regrade request results") ///
note("Statistics are weighted by the number of students taught by instructors 
in the last five years. p-values for testing" "if the regrade request outcomes 
are the same for male and female are `pvalue_up_fn', `pvalue_same_fn', and 
`pvalue_down_fn' at the end of semester" "(panels A and B), and `pvalue_up_mt', `pvalue_same_mt', 
and `pvalue_down_mt' during the semester (panels C and D) for grade increase, no change, and" 
"grade decrease, respectively.");
#delimit cr
graph export $Output/figure3.pdf, replace

erase $Output/male_fn.gph 
erase $Output/female_fn.gph
erase $Output/male_mt.gph 
erase $Output/female_mt.gph


/*=========       Table 4. Summary stats of instructor survey      ===========*/


use $Data/instsurvey.dta, clear

mat drop _all

// Prepare summary stats table

local fnvars1  pctrequests_fn pctmale_fn
local fnvars2  aggressivemale_fn aggressivesame_fn aggressivefemale_fn
local mtvars1  pctrequests_mt pctmale_mt
local mtvars2  aggressivemale_mt aggressivesame_mt aggressivefemale_mt

tabstat `fnvars1' `mtvars1' if sample==1, stats(n)
tabstat `fnvars2' `mtvars2' if sample==1, stats(n)
				
foreach k in fn mt {
tabstat ``k'vars1' ///
	if sample==1 [w=numstudents], stats(mean sd)  save
mat `k'1 = r(StatTotal)'
}


foreach k in fn mt {
tabstat ``k'vars2'  ///
	if sample==1 [w=numstudents], stats(mean sd)  save
mat `k'2 = 100*r(StatTotal)'
}

// Does the gender dist of final regrade requests the same as class gender dist?
su pctmale_st [aw=numstudents]
local pctmalest = round(r(mean), .1)
rename pctmale_fn  pctmale1
rename pctmale_mt  pctmale2
rename pctmale_st  pctmale3
reshape long pctmale, i(id) j(time)

svyset id  [weight=numstudents]
gen base=(time==3)

mat fnpctstar = J(2,2,0)
mat mtpctstar = J(2,2,0)

* Percent male of regrade requests end of semester vs Percent male in class
svy: reg pctmale base if (time==1 | time==3) & sample==1
mat temp = r(table)
local p_pctmale_fn = round(temp[4,1], .001)
mat fnpctstar[2,1] = ///
	(`p_pctmale_fn'<=.1)+(`p_pctmale_fn'<=.05)+(`p_pctmale_fn'<=.01)

* Percent male of regrade requests duringsemester vs Percent male in class
svy: reg pctmale base if (time==2 | time==3) & sample==1
mat temp = r(table)
local p_pctmale_mt = round(temp[4,1], .001)
mat mtpctstar[2,1] = ///
	(`p_pctmale_mt'<=.1)+(`p_pctmale_mt'<=.05)+(`p_pctmale_mt'<=.01)


drop base
reshape wide pctmale, i(id) j(time)
rename pctmale1 pctmale_fn 
rename pctmale2 pctmale_mt
rename pctmale3 pctmale_st

// Difference between End-of-Semester and During-Semester

rename pctrequests_fn 		pctrequests1
rename pctmale_fn 			pctmale1
rename aggressivemale_fn 	aggressivemale1 
rename aggressivesame_fn 	aggressivesame1 
rename aggressivefemale_fn	aggressivefemale1
rename pctrequests_mt 		pctrequests2
rename pctmale_mt 			pctmale2
rename aggressivemale_mt 	aggressivemale2 
rename aggressivesame_mt 	aggressivesame2 
rename aggressivefemale_mt	aggressivefemale2
reshape long 				///
		pctrequests 		///
		pctmale				///
		aggressivemale		///
		aggressivesame		///
		aggressivefemale	///
		, i(id) j(time)

svyset id  [weight=numstudents]
gen base=(time==2)

local vars1  pctrequests pctmale
local vars2  aggressivemale aggressivesame aggressivefemale
local nvars1: word count `vars1'
local nvars2: word count `vars2'

forvalues i = 1/2 {
mat diff`i' = J(`nvars`i'',2,.)
mat star`i' = J(`nvars`i'',2,0)
forvalues j = 1/`nvars`i'' {
	local var`i'`j': word `j' of `vars`i''
	svy: reg `var`i'`j'' base if sample==1
	mat temp = r(table)
	mat diff`i'[`j',1] = temp[1,1]
	mat diff`i'[`j',2] = temp[2,1]
	mat star`i'[`j',1] = ///
		(temp[4,1]<=0.1) + (temp[4,1]<=0.05) + (temp[4,1]<=0.01)
}
}
mat diff2 = 100*diff2


frmttable, clear(inst1)
frmttable, clear(inst2)

frmttable, plain tex statmat(fn1) ///
	substat(1) sdec(2) rtitles("Percent requested regrades in a class (\%)"\""\ ///
	"Male students among requests (\%)"\"" ) ///
	ctitles("","End of semester"\"\textbf{A. Regrade requests}","") ///
	coljust(c) replace store(inst1) annotate(fnpctstar) asymbol(+,++,+++) 


frmttable, plain tex statmat(mt1) ///
	substat(1) sdec(2) rtitles("Percent requested regrades in a class (\%)"\""\ ///
	"Male students among requests (\%)"\"" ) ///
	ctitles("","During semester") ///
	coljust(c) merge(inst1) annotate(mtpctstar) asymbol(+,++,+++)

frmttable, plain tex statmat(diff1) ///
	substat(1) sdec(2) rtitles("Percent requested regrades in a class (\%)"\""\ ///
	"Male students among requests (\%)"\"" ) ///
	ctitles("","Difference") ///
	coljust(c) merge(inst1) annotate(star1) asymbol(*,**,***) sq
	

	
frmttable, tex statmat(fn2) substat(1) ///
	sdec(2) rtitles("Males more aggressive (\%)"\""\ ///
	"Similarly aggressive (\%)"\"" \ ///
	"Females more aggressive (\%)"\"" ) ///
	ctitles("","End of semester") ///
	coljust(c) replace store(inst2) 


frmttable, tex statmat(mt2) substat(1) ///
	sdec(2) rtitles("Males more aggressive (\%)"\""\ ///
	"Similarly aggressive (\%)"\"" \ ///
	"Females more aggressive (\%)"\"" ) ///
	ctitles("","During semester")coljust(c) merge(inst2) 


frmttable, plain tex statmat(diff2) ///
	substat(1) sdec(2) rtitles("Males more aggressive (\%)"\""\ ///
	"Similarly aggressive (\%)"\"" \ ///
	"Females more aggressive (\%)"\"" ) ///
	ctitles("","Difference") ///
	coljust(c) merge(inst2) annotate(star2) asymbol(*,**,***) sq
	
frmttable using $Output/tab4_sumstats_instsvy, tex replay(inst1) substat(1) ///
	sdec(2)  coljust(c) replace	multicol(2,1,4)
	
frmttable using $Output/tab4_sumstats_instsvy, plain tex  ///
	addrows("\textbf{B.Instructors' report on students aggressiveness in regrade requests}","","") multicol(7,1,4) replace

frmttable using $Output/tab4_sumstats_instsvy, plain tex append(inst2) ///
multicol(2,1,4; 7,1,4/*; 14,1,4; 27,1,4; 28,2,2; 29,2,2; 30,2,2*/) hlines(10100001000001) ///
		posttext("* p$<$0.10; ** p$<$0.05, *** p$<$<0.01 indicate the significance level of differences between end of the semester and during semester." ///
		"+ p$<$0.10; ++ p$<$0.05, +++ p$<$0.01 indicate the significance level of differences between regrade requests at the end of/during the semester against the male representation (`pctmalest'\%) in the class."  ///
		"Standard deviations are in parentheses. Standard errors are in square brackets. All statistics are weighted by class sizes.")



/******************************************************************************/
/*                            Part 3. Student Survey                          */
/******************************************************************************/
use $Data/stdsurvey.dta, clear

/*--------- Figure 2. Distribution of classes considered for regrades --------*/

/* Kolmogorov-Smirnov equality-of-distributions test */
ksmirnov num_class if round==1, by(std_female) exact
local p = r(p)
local p : di %4.2f `p'

catplot std_female num_class, ///
percent(std_female) ///
/*var1opts(label(labsize(medium)))*/ ///
asyvars bar(1,fintensity(inten60)) ///
bar(2, fcolor(maroon)) ///
var2opts(relabel(1 "0 class" 2 "1 class" 3 "2 classes" 4 "3 classes" 5 "4 classes" 6 "5+ classes")) ///
ytitle("Distribution by gender (%)") ///
title("Distribution of classes considered for regrades" "by students' gender") ///
blabel(bar, format(%4.1f)) ///
note("Kolmogorov-Smirnov equality-of-distributions test: p-value = `p'") ///
recast(bar)
graph export $Output/figure2.pdf, replace

/*------------------- Table 5. Summary stats of student survey ---------------*/

#delimit;

mat drop _all;

local svyvars1
		std_standing
		consider_regrade
		everasked
		everasked_fn
		everasked_mt
		num_class
		num_asked
		num_asked_imp	
		;
	
local 	nvars1: word count `svyvars1';


/* intensive margin */
local svyvars2
		num_asked
		num_asked_imp
		;
local 	nvars2: word count `svyvars2';


local svyvars3
		pr_up
		pr_same
		pr_down
		regret3
		regret4
		control
		stress		
		;
	
local 	nvars3: word count `svyvars3';

local svyvars4
		part_up
		part_down
		posgradechange
		neggradechange
		;
	
local 	nvars4: word count `svyvars4';

mat stars1 = J(`nvars1', 2, 0);
mat stars2 = J(`nvars2', 2, 0);
mat stars3 = J(`nvars3', 2, 0);
mat stars4 = J(`nvars4', 2, 0);

/* Unit: participant-level */
forvalues j=1/`nvars1' {;
local var1: word `j' of `svyvars1';
tabstat `var1' if round==1, stats(mean sd) by(std_female) save;
mat male1 = [nullmat(male1) \ r(Stat1)'];
mat female1 = [nullmat(female1) \ r(Stat2)'];
reg `var1' std_female if round==1;
mat temp1 = r(table);
mat diff1 = [nullmat(diff1) \ temp1[1,1],temp1[2,1]];
mat df1 = [nullmat(df1) \ e(df_r), . ];
mat stars1[`j',1] = 
	(abs(diff1[`j',1]/diff1[`j',2]) > invttail(df1[`j',1],0.10/2)) + 
	(abs(diff1[`j',1]/diff1[`j',2]) > invttail(df1[`j',1],0.05/2)) + 
	(abs(diff1[`j',1]/diff1[`j',2]) > invttail(df1[`j',1],0.01/2)) ; 
};


mat male1[2,1] = 100 * male1[2,1];
mat male1[2,2] = 100 * male1[2,2];
mat female1[2,1] = 100 * female1[2,1];
mat female1[2,2] = 100 * female1[2,2];
mat diff1[2,1] = 100 * diff1[2,1];
mat diff1[2,2] = 100 * diff1[2,2];

mat male1[3,1] = 100 * male1[3,1];
mat male1[3,2] = 100 * male1[3,2];
mat female1[3,1] = 100 * female1[3,1];
mat female1[3,2] = 100 * female1[3,2];
mat diff1[3,1] = 100 * diff1[3,1];
mat diff1[3,2] = 100 * diff1[3,2];

mat male1[4,1] = 100 * male1[4,1];
mat male1[4,2] = 100 * male1[4,2];
mat female1[4,1] = 100 * female1[4,1];
mat female1[4,2] = 100 * female1[4,2];
mat diff1[4,1] = 100 * diff1[4,1];
mat diff1[4,2] = 100 * diff1[4,2];

mat male1[5,1] = 100 * male1[5,1];
mat male1[5,2] = 100 * male1[5,2];
mat female1[5,1] = 100 * female1[5,1];
mat female1[5,2] = 100 * female1[5,2];
mat diff1[5,1] = 100 * diff1[5,1];
mat diff1[5,2] = 100 * diff1[5,2];

/* conditional on asking*/
forvalues j=1/`nvars2' {;
local var2: word `j' of `svyvars2';
tabstat `var2' if round==1 & `var2'>0, stats(mean sd) by(std_female) save;
mat male2 = [nullmat(male2) \ r(Stat1)'];
mat female2 = [nullmat(female2) \ r(Stat2)'];
reg `var2' std_female if round==1;
mat temp2 = r(table);
mat diff2 = [nullmat(diff2) \ temp2[1,1],temp2[2,1]];
mat df2 = [nullmat(df2) \ e(df_r), . ];
mat stars2[`j',1] = 
	(abs(diff2[`j',1]/diff2[`j',2]) > invttail(df2[`j',1],0.10/2)) + 
	(abs(diff2[`j',1]/diff2[`j',2]) > invttail(df2[`j',1],0.05/2)) + 
	(abs(diff2[`j',1]/diff2[`j',2]) > invttail(df2[`j',1],0.01/2)) ; 
};


forvalues j=1/`nvars3' {;
local var3: word `j' of `svyvars3';
tabstat `var3' if round==1, stats(mean sd) by(std_female) save;
mat male3 = [nullmat(male3) \ r(Stat1)'];
mat female3 = [nullmat(female3) \ r(Stat2)'];
reg `var3' std_female if round==1;
mat temp3 = r(table);
mat diff3 = [nullmat(diff3) \ temp3[1,1],temp3[2,1]];
mat df3 = [nullmat(df3) \ e(df_r), . ];
mat stars3[`j',1] = 
	(abs(diff3[`j',1]/diff3[`j',2]) > invttail(df3[`j',1],0.10/2)) + 
	(abs(diff3[`j',1]/diff3[`j',2]) > invttail(df3[`j',1],0.05/2)) + 
	(abs(diff3[`j',1]/diff3[`j',2]) > invttail(df3[`j',1],0.01/2)) ; 
};

/* Unit: observations (allow multiple obs from one participant) */
forvalues j=1/`nvars4' {;
local var4: word `j' of `svyvars4';
tabstat `var4' if /*round==1 &*/ asked==1, stats(mean sd) by(std_female) save;
mat male4 = [nullmat(male4) \ 100*r(Stat1)'];
mat female4 = [nullmat(female4) \ 100*r(Stat2)'];
reg `var4' std_female if /*round==1 &*/ asked==1;
mat temp4 = r(table);
mat diff4 = [nullmat(diff4) \ 100*temp4[1,1],100*temp4[2,1]];
mat df4 = [nullmat(df4) \ e(df_r), . ];
mat stars4[`j',1] = 
	(abs(diff4[`j',1]/diff4[`j',2]) > invttail(df4[`j',1],0.10/2)) + 
	(abs(diff4[`j',1]/diff4[`j',2]) > invttail(df4[`j',1],0.05/2)) + 
	(abs(diff4[`j',1]/diff4[`j',2]) > invttail(df4[`j',1],0.01/2)) ; 
};

tabstat std_female if round==1, by(std_female) stats(n) save;
mat N1 = [r(Stat2), r(Stat1),.];

tabstat std_female if round==1 & everasked==1, by(std_female) stats(n) save;
mat N2 = [r(Stat2), r(Stat1),.];

tabstat std_female if asked==1, by(std_female) stats(n) save;
mat N3 = [r(Stat2), r(Stat1),.];

#delimit;
frmttable, clear(tab5a1);
frmttable, clear(tab5a2);
frmttable, clear(tab5a3);
frmttable, clear(tab5b);


frmttable, tex statmat(female1) plain ///
	substat(1) sdec(2) varlabels ///
	ctitle("A. All students", "Female"\"A1. All","") ///
	coljust(c) replace store(tab5a1);

frmttable, tex statmat(male1) plain substat(1) sdec(2) ///
	varlabels ctitle("A. All students","Male"\"A1. All","") ///
	coljust(c) merge(tab5a1) ;

frmttable, tex statmat(diff1) plain substat(1) sdec(2) ///
	annotate(stars1) asymbol(*,**,***) varlabels ctitle("Difference") ///
	coljust(c) merge(tab5a1) sq; 

	
frmttable, tex statmat(female2) plain ///
	substat(1) sdec(2) varlabels ///
	coljust(c) replace store(tab5a2);

frmttable, tex statmat(male2) plain substat(1) sdec(2) ///
	varlabels coljust(c) merge(tab5a2) ;

frmttable, tex statmat(diff2) plain substat(1) sdec(2) ///
	annotate(stars2) asymbol(*,**,***) varlabels  ///
	coljust(c) merge(tab5a2) sq;

frmttable, tex statmat(female3) plain ///
	substat(1) sdec(2) varlabels  ///
	coljust(c) replace store(tab5a3);

frmttable, tex statmat(male3) plain substat(1) sdec(2) ///
	varlabels coljust(c) merge(tab5a3) ;

frmttable, tex statmat(diff3) plain substat(1) sdec(2) ///
	annotate(stars3) asymbol(*,**,***) varlabels  ///
	coljust(c) merge(tab5a3) sq;

frmttable, tex statmat(N1) plain sdec(0) ///
	varlabels rtitle(Number of participants) coljust(c) append(tab5a3);


frmttable, tex statmat(female4) plain substat(1) sdec(2) varlabels ///
	ctitle("B. Students who asked for regrades","Female") coljust(c) ///
	replace store(tab5b);

frmttable, tex statmat(male4) plain substat(1) sdec(2) ///
	varlabels ctitle("B. Students who asked for regrades","Male") ///
	coljust(c) merge(tab5b);

frmttable, tex statmat(diff4) plain substat(1) sdec(2) ///
	annotate(stars4) asymbol(*,**,***) varlabels ///
	ctitle("Difference") ///
	coljust(c) merge(tab5b) sq; 

frmttable, plain tex statmat(N2)  sdec(0) ///
	varlabels rtitle(Number of participants) coljust(c) append(tab5b) ///
	title("Summary statistics of the student survey");


frmttable, plain tex statmat(N3)  sdec(0) ///
	varlabels rtitle(Number of observations) coljust(c) append(tab5b) ///
	title("Summary statistics of the student survey");	
	
#delimit; 
frmttable using $Output/tab5_sumstats_stdsurvey, plain tex replay(tab5a1) 
	addrows("A2. Conditional on ever considered regrades") ///
	replace ; 
frmttable using $Output/tab5_sumstats_stdsurvey, tex append(tab5a2) ///
	addrows("A3. Student Perceptions","","","") ;
frmttable using $Output/tab5_sumstats_stdsurvey, tex append(tab5a3);

#delimit;	
frmttable using $Output/tab5_sumstats_stdsurvey, plain tex 
	addrows("B. Sub-sample of regrade requests","Female", "Male", "Difference") replace; 
#delimit;	
frmttable using $Output/tab5_sumstats_stdsurvey, tex append(tab5b)	///
	plain hlines(110000000000000000100001000000000000000110000000001)
	rtitle("Years in college"\""\
			"Percent ever considered regrades (\%)"\""\
			"Percent ever asked for regrades (\%)"\""\
			"Percent ever asked for regrades, end of semester (\%)"\""\
			"Percent ever asked for regrades, during semester (\%)"\""\
			"Number of classes considered regrades"\""\
			"Number of classes asked for regrades"\""\
			"Imputed number of classes asked for regrades"\""\
			"A2. Conditional on asking for regrades"\
			"Number of classes asked for regrades"\""\
			"Imputed number of classes asked for regrades"\""\
			"A3. Student Perceptions"\
			"Probability of grade increased if asked (\%)"\""\
			"Probability of grade unchanged if asked (\%)"\""\
			"Probability of grade decreased if asked (\%)"\""\
			"Regret asking aggressively, during semester$^a$"\""\
			"Regret asking aggressively, end of semester$^a$"\""\
			"Control over direction of life$^b$"\""\
			"Stress from asking for regrades$^c$"\""\
			"Number of participants"\
			"B. Sub-sample of regrade requests"\
			"Percent grade component increased (\%)"\""\
			"Percent grade component decreased (\%)"\""\
			"Percent final grade increased (\%)"\""\
			"Percent final grade decreased (\%)"\""\
			"Number of participants"\
			"Number of observations") title("Summary statistics of the student survey") 
	posttext("* p<0.10; ** p<0.05, *** p<0.01 indicate the significance 
	level of gender differences. Standard deviations are in parentheses. 
	Standard errors are in square brackets.
	a. Scale ranges between 1 (never true to me) to 7 (always true to me).
	b. Scale ranges between 1 (I don’t have any control) to 7 (I have full control).
	c. Scale ranges between 1 (not stressed at all) to 7 (extremely stressed).")
	;

/******************************************************************************/
/*                             Part 4. Experiment                              */
/******************************************************************************/
#delimit;
use $Data/experiment.dta, clear;
drop if irrational==1;
/*================      Figure 4. Distribution of WTP      ===================*/
/* Exclude people with inconsistent WTPs */


/* 1.	Plot the proportion of each gender asking for a regrade at each cost. 
        So the x-axis is the regrade cost (going from -1 to 3.5). 
		And then at each value, show the percent of males and females who 
		ask for a regrade. */

tabstat s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 ,by(std_female) save;

mat A = [3.5, 3, 2.5, 2, 1.5, 1, 0.5, 0, -0.5, -1];

mat A = [A', r(Stat1)', r(Stat2)'];

mat colnames A = Cost Male Female;
svmat A, names(col);
gen Diff = Male - Female;

forvalues i = 0/9 {;
ttest s`i', by(std_female);
mat se = [nullmat(se)\r(se)];
};
mat colnames se = se;
svmat se, names(col);
gen upper = Diff + (1.64*se);
gen lower = Diff - (1.64*se);

twoway (connected Male Cost, sort msymbol(square) lcolor(blue) lpattern(dash)) ///
 (connected Female Cost, sort msymbol(triangle) lcolor(red) lpattern(solid))  ///
 (scatter Diff Cost, mcolor(gray)) (rspike upper lower Cost, lcolor(gray))  ///
 , xtitle(Cost (in dollars)) xlabel(-1(0.5)3.5) ///
 ytitle(Percent willing to pay the cost for regrades (%)) ///
 ylabel(0 "0" .2 "20" .4 "20" .6 "60" .8 "80") ///
 yline(0, lcolor(gray)) title(Distribution of Willingness-to-Pay by Gender) ///
 legend(order(1 "Male" 2 "Female" 3 "Difference" 4 "90% C.I. of gender difference" ));
graph export $Output/figure4.pdf, replace;

drop Cost Male Female Diff se upper lower; 
#delimit cr 

/*========     Figure 5. WTP vs (Prior guessed score - initial score)   ======*/

mat drop _all
reg wtp prior_minus_orig  if irrational==0 
mat Table = r(table)
local b: display %9.3fc  _b[prior_minus_orig]
local p: display %9.4fc Table[4,1]

binscatter wtp prior_minus_orig if irrational==0 , ///
ytitle(, margin(medium)) ///
ytitle("WTP($)") ///
xtitle("Prior guessed score - initial score") ///
ylabel(-1.0(0.5)1.0) ///
title("Binned scatter plot of WTP(\$) against the gap" "between prior guessed score and intial score") ///
text(0 2.5 "Coefficient =`b'" "(p-value = `p')" , place(se) just(left)) graphregion(color(ltbluishgray))

graph export $Output/figure5.pdf, replace



/*==============     Figure 6. Regrade results by WTP          ===============*/
#delimit;

/* Payoff Increase */
mat drop _all;
tabstat betteroff_s0 betteroff_s1 betteroff_s2 betteroff_s3 betteroff_s4 
	betteroff_s5 betteroff_s6 betteroff_s7 betteroff_s8 betteroff_s9,
	by(std_female) save;

mat A = [3.5, 3, 2.5, 2, 1.5, 1, 0.5, 0, -0.5, -1];

mat A = [A', r(Stat1)', r(Stat2)'];

mat colnames A = Cost Male Female;
svmat A, names(col);
gen Diff = Male - Female;

forvalues i = 0/9 {;
ttest betteroff_s`i', by(std_female);
mat se = [nullmat(se)\r(se)];
};

mat colnames se = se;
svmat se, names(col);
gen upper = Diff + (1.64*se);
gen lower = Diff - (1.64*se);

twoway (connected Male Cost, sort msymbol(square) lcolor(blue) lpattern(dash)) ///
 (connected Female Cost, sort msymbol(triangle) lcolor(red) lpattern(solid))  ///
 (scatter Diff Cost, mcolor(gray)) (rspike upper lower Cost, lcolor(gray))  ///
 , xtitle(Regrade Cost (in dollars)) xlabel(-1(0.5)3.5) ylabel(-0.1(0.1)0.8)  ///
 ylabel(-0.1 "-10" 0 "0" .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60" .7 "70" .8 "80" ) ///
 ytitle(Percent (%))  ytitle(, margin(medium)) ///
 yline(0, lcolor(gray)) title(A. Payoff Increase) ///
 legend(on size(small)) ///
 legend(order(1 "Male" 2 "Female" 3 "Difference" 4 "90% C.I. of difference")) /*fysize(40)*/;
graph save $Output/exp_payoffup, replace;
drop Cost Male Female Diff se upper lower; 

/* Payoff Decrease */
mat drop _all;
tabstat worseoff_s0 worseoff_s1 worseoff_s2 worseoff_s3 worseoff_s4 
	worseoff_s5 worseoff_s6 worseoff_s7 worseoff_s8 worseoff_s9,
	by(std_female) save;

mat A = [3.5, 3, 2.5, 2, 1.5, 1, 0.5, 0, -0.5, -1];

mat A = [A', r(Stat1)', r(Stat2)'];

mat colnames A = Cost Male Female;
svmat A, names(col);
gen Diff = Male - Female;

forvalues i = 0/9 {;
ttest worseoff_s`i', by(std_female);
mat se = [nullmat(se)\r(se)];
};

mat colnames se = se;
svmat se, names(col);
gen upper = Diff + (1.64*se);
gen lower = Diff - (1.64*se);

twoway (connected Male Cost, sort msymbol(square) lcolor(blue) lpattern(dash)) ///
 (connected Female Cost, sort msymbol(triangle) lcolor(red) lpattern(solid))  ///
 (scatter Diff Cost, mcolor(gray)) (rspike upper lower Cost, lcolor(gray))  ///
 , xtitle(Regrade Cost (in dollars)) xlabel(-1(0.5)3.5)  ylabel(-0.1(0.1)0.8) ///
 ylabel(-0.1 "-10" 0 "0" .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60" .7 "70" .8 "80" ) ///
 ytitle(Percent (%))  ytitle(, margin(medium)) ///
 yline(0, lcolor(gray)) title(B. Payoff Decrease) ///
 legend(on size(small)) ///
 legend(order(1 "Male" 2 "Female" 3 "Difference" 4 "90% C.I. of difference" ));
graph save $Output/exp_payoffdown, replace;
drop Cost Male Female Diff se upper lower; 


cd $Output;

graph combine exp_payoffup.gph  ///
exp_payoffdown.gph ///
, title("Percent receiving a higher/lower payoff");
graph export $Output/figure6.pdf, replace;


erase $Output/exp_payoffup.gph;
erase $Output/exp_payoffdown.gph;

/*====== Figure 7. Correlation between regrade consideration and WTP   =======*/
#delimit;
/* Ever asked for regrades */
reg everasked wtp if irrational==0;
mat Table = r(table);
local b: display %9.4fc  _b[wtp];
local p: display %9.4fc Table[4,1];
binscatter everasked wtp if irrational==0, ///
nquantiles(5) ///
/*xlabel(-1.5(1)3.5)*/ ///
title("Ever asked for regrades") ///
ytitle(, margin(medium)) ///
ytitle("Percent (%)") ///
xtitle("Willingness to Pay ($)") ///
ylabel(.30 "30" .35 "35" .4 "40" .45 "45" .5 "50") ///
text(0.35 1 "Slope =`b'" "(p-value = `p')" , place(se) just(left));
graph save $Output/exp_asked, replace;

#delimit;
/* Number of classes considered for regrade requests */
mat drop _all;
reg num_asked_imp wtp if irrational==0;
mat Table = r(table);
local b: display %9.4fc  _b[wtp];
local p: display %9.4fc Table[4,1];
binscatter num_asked_imp wtp if irrational==0,  ///
nquantiles(5) ///
/*xlabel(-1.5(0.5)3.5)*/ ///
title("Imputed number of classes" "asked for regrades") ///
ytitle(, margin(medium)) ///
ytitle("Imputed number of classes") ///
xtitle("Willingness to Pay ($)") ///
text(.64 1 "Slope =`b'" "(p-value = `p')" , place(e) just(left))
;
graph save $Output/exp_numclass, replace;


#delimit;
cd $Output;
graph combine exp_asked.gph exp_numclass.gph, ///
title("Binned scatter plots of regrade requests in classes" "against willingness-to-pay") ///
note("Restricted to the subsample of 516 participants who completed both the experiment and the student survey.");
graph export $Output/figure7.pdf, replace;

erase $Output/exp_asked.gph;
erase $Output/exp_numclass.gph;
#delimit cr


/*===========        Table 6. Summary stats of experiment          ===========*/
use $Data/experiment.dta, clear
drop if irrational==1
mat drop _all

tabstat std_female, stats(mean) save
mat temp = r(StatTotal)

mat Female0 = 100 * temp[1,1]
mat Male0 = 100 - Female0


egen tot=rownonmiss(female_r1 female_r2 female_r3)
egen numfem=rowtotal(female_r1 female_r2 female_r3)
gen pctfeminst=100*numfem/tot
tabstat pctfeminst gpa, by(std_female) stats(mean sd) save
mat Male1 = r(Stat1)'
mat Female1 = r(Stat2)'
mat list Female1
mat list Male1

local vars1 	pctfeminst gpa		
		
local 	nvars1: word count `vars1'
matrix diff1 = J(`nvars1',2,.)
matrix star1 = J(`nvars1',2,0)
forvalues i=1/`nvars1' {
local var1_`i': word `i' of `vars1'
		reg `var1_`i'' std_female
		mat temp = r(table)
		mat diff1[`i',1] = temp[1,1]
		mat diff1[`i',2] = temp[2,1]
		mat star1[`i',1] = (temp[4,1]<0.1)+ (temp[4,1]<0.05)+ (temp[4,1]<0.01)
}

egen avggradeup = rowmean(gradeup_s0 gradeup_s1 gradeup_s2 gradeup_s3 ///
gradeup_s4 gradeup_s5 gradeup_s6 gradeup_s7 gradeup_s8 gradeup_s9)

egen avggradedown = rowmean(gradedown_s0 gradedown_s1 gradedown_s2 gradedown_s3 ///
gradedown_s4 gradedown_s5 gradedown_s6 gradedown_s7 gradedown_s8 gradedown_s9)

egen avgbetteroff = rowmean(betteroff_s0 betteroff_s1 betteroff_s2 betteroff_s3 ///
betteroff_s4 betteroff_s5 betteroff_s6 betteroff_s7 betteroff_s8 betteroff_s9)

egen avgworseoff = rowmean(worseoff_s0 worseoff_s1 worseoff_s2 worseoff_s3 ///
worseoff_s4 worseoff_s5 worseoff_s6 worseoff_s7 worseoff_s8 worseoff_s9)


#delimit;
local expvars
		riskaversion
		overconfidence
		underconfidence
		overoptimism
		uncertainty1
		prior_num_guess
		prior_downside1
		post_num_guess
		post_downside
		updates
		extrovert 
		agreeable 
		conscientious 
		neurotic 
		open
		answers_correct
		orig_correct
		fn_correct 
		wtp
		I
		avggradeup
		avggradedown
		avgbetteroff
		avgworseoff
		;

local 	nvars: word count `expvars';

mat stars = J(`nvars', 2, 0);

forvalues j=1/`nvars' {;
local var: word `j' of `expvars';
tabstat `var' if irrational==0 , stats(mean sd) by(std_female) save ;
mat male = [nullmat(male) \ r(Stat1)'];
mat female = [nullmat(female) \ r(Stat2)'];
reg `var' std_female if irrational==0 ;
mat temp = r(table);
mat diff = [nullmat(diff) \ temp[1,1],temp[2,1]];
mat df = [nullmat(df) \ e(df_r), . ];
mat stars[`j',1] = 
	(abs(diff[`j',1]/diff[`j',2]) > invttail(df[`j',1],0.10/2)) + 
	(abs(diff[`j',1]/diff[`j',2]) > invttail(df[`j',1],0.05/2)) + 
	(abs(diff[`j',1]/diff[`j',2]) > invttail(df[`j',1],0.01/2)) ; 
};
foreach i in 20 21 22 23 24 {;
mat female[`i',1]= 100*female[`i',1];
mat female[`i',2]= 100*female[`i',2];
mat male[`i',1]= 100*male[`i',1];
mat male[`i',2]= 100*male[`i',2];
mat diff[`i',1]= 100*diff[`i',1];
mat diff[`i',2]= 100*diff[`i',2];
};
#delimit;
encode participantcode, gen(std_id);
tabstat std_id if irrational==0, by(std_female) stats(n)  save;
mat N = [r(Stat2) , r(Stat1), .];
mat list N;



* Panel A
#delimit;	
frmttable using $Output/tab6a1, statmat(Female0) plain ///
	substat(0) sdec(1) ctitle("","","Female") coljust(c) store(panelA1) tex ///
	rtitle("A.","Percent(\%) participants") replace;
	
frmttable using $Output/tab6a1, statmat(Male0) plain ///
	substat(0) sdec(1) ctitle("","","Male") coljust(c) merge(panelA1) tex
	rtitle("A.","Percent(\%) participants");

#delimit;
frmttable using $Output/tab6a2, statmat(Female1) plain ///
	substat(1) sdec(1\1\2\2) ctitle("","","Female") coljust(c) store(panelA2) tex ///
	rtitle("","Percent(\%) female instructors$^a$"\"",""\"","GPA"\"","")
	replace ;
	
frmttable using $Output/tab6a2, statmat(Male1) plain ///
	substat(1) sdec(1\1\2\2) ctitle("","","Male") coljust(c) merge(panelA2) tex
	rtitle("","Percent(\%) female instructors$^a$"\"",""\"","GPA"\"","");

frmttable using $Output/tab6a2, statmat(diff1) ///
	substat(1) sdec(1\1\2\2) ctitle("","", "Gender Difference") ///
	rtitle("","Percent(\%) female instructors$^a$"\"",""\"","GPA"\"","")	
	plain annotate(star1) asymbol(*,**,***) 
	coljust(c) merge(panelA2) tex sq;

frmttable using $Output/tab6bcd, plain tex ///
	statmat(female) substat(1) sdec(2) ///
	ctitle("","",Female) coljust(c) replace ///
	rtitle("B.","Risk aversion coefficient"\"",""\
			"","Over-confidence$^b$"\"",""\
			"","Under-confidence$^b$"\"",""\
			"","Over-optimism$^c$"\"",""\
			"","Uncertainty$^d$"\"",""\	
			"","Prior guessed score"\"",""\	
			"","Prior downside risk$^e$"\"",""\	
			"","Posterior guessed score"\"",""\	
			"","Posterior downside risk$^e$"\"",""\	
			"","Belief revision$^f$"\"",""\	
			"C.","Big 5 trait: Extroversion$^g$"\"",""\
			"","Big 5 trait: Agreeableness$^g$"\"",""\
			"","Big 5 trait: Conscientiousness$^g$"\"",""\
			"","Big 5 trait: Neuroticism$^g$"\"",""\
			"","Big 5 trait: Openness$^g$"\"",""\			
			"D.","True score of the quiz"\"",""\
			"","Original score of the quiz"\"",""\ 
			"","Final score of the quiz"\"",""\
			"","WTP (\$)"\"",""\
			"","\% Willing to pay positive cost"\"",""\
			"","Percent score increased (\%)$^h$"\"",""\
			"","Percent score decreased (\%)$^h$"\"",""\
			"","Percent payoff increased (\%)$^h$"\"",""\
			"","Percent payoff decreased (\%)$^h$"\"","")
			store(panelBCD); 


frmttable using $Output/tab6bcd, plain tex ///
	statmat(male) substat(1) sdec(2) ///
	varlabels ctitle("","",Male) coljust(c) merge(panelBCD) ///
	rtitle("B.","Risk aversion coefficient"\"",""\
			"","Over-confidence$^b$"\"",""\
			"","Under-confidence$^b$"\"",""\
			"","Over-optimism$^c$"\"",""\
			"","Uncertainty$^d$"\"",""\	
			"","Prior guessed score"\"",""\	
			"","Prior downside risk$^e$"\"",""\	
			"","Posterior guessed score"\"",""\	
			"","Posterior downside risk$^e$"\"",""\	
			"","Belief revision$^f$"\"",""\	
			"C.","Big 5 trait: Extroversion$^g$"\"",""\
			"","Big 5 trait: Agreeableness$^g$"\"",""\
			"","Big 5 trait: Conscientiousness$^g$"\"",""\
			"","Big 5 trait: Neuroticism$^g$"\"",""\
			"","Big 5 trait: Openness$^g$"\"",""\			
			"D.","True score of the quiz"\"",""\
			"","Original score of the quiz"\"",""\ 
			"","Final score of the quiz"\"",""\
			"","WTP (\$)"\"",""\
			"","\% Willing to pay positive cost"\"",""\
			"","Percent score increased (\%)$^h$"\"",""\
			"","Percent score decreased (\%)$^h$"\"",""\
			"","Percent payoff increased (\%)$^h$"\"",""\
			"","Percent payoff decreased (\%)$^h$"\"","")
			; 

frmttable using $Output/tab6bcd, plain tex ///
	statmat(diff) substat(1) sdec(2) ///
	varlabels ctitle("","",Difference) coljust(c) ///
	annotate(stars) asymbol(*,**,***) merge(panelBCD) ///
	rtitle("B.","Risk aversion coefficient"\"",""\
			"","Over-confidence$^b$"\"",""\
			"","Under-confidence$^b$"\"",""\
			"","Over-optimism$^c$"\"",""\
			"","Uncertainty$^d$"\"",""\	
			"","Prior guessed score"\"",""\	
			"","Prior downside risk$^e$"\"",""\	
			"","Posterior guessed score"\"",""\	
			"","Posterior downside risk$^e$"\"",""\	
			"","Belief revision$^f$"\"",""\	
			"C.","Big 5 trait: Extroversion$^g$"\"",""\
			"","Big 5 trait: Agreeableness$^g$"\"",""\
			"","Big 5 trait: Conscientiousness$^g$"\"",""\
			"","Big 5 trait: Neuroticism$^g$"\"",""\
			"","Big 5 trait: Openness$^g$"\"",""\			
			"D.","True score of the quiz"\"",""\
			"","Original score of the quiz"\"",""\ 
			"","Final score of the quiz"\"",""\
			"","WTP (\$)"\"",""\
			"","\% Willing to pay positive cost"\"",""\
			"","Percent score increased (\%)$^h$"\"",""\
			"","Percent score decreased (\%)$^h$"\"",""\
			"","Percent payoff increased (\%)$^h$"\"",""\
			"","Percent payoff decreased (\%)$^h$"\"","")
			sq; 


#delimit cr

* Combine panels

frmttable using $Output/tab6_sumstats_experiment, plain tex replay(panelA1) ///
	replace ctitles("","","Students","",""\ ///
	"","","Female","Male","Gender Difference") 
	   
frmttable using $Output/tab6_sumstats_experiment, plain tex append(panelA2) ///
	replace

frmttable using $Output/tab6_sumstats_experiment, plain tex append(panelBCD) ///
	replace	

#delimit;
frmttable using $Output/tab6_sumstats_experiment, plain tex ///
	statmat(N) sdec(0) rtitle("",Observations) coljust(c) append /// 
	note("* p<0.10; ** p<0.05, *** p<0.01. Standard deviations are in parentheses. Standard errors are in square brackets." "a. Percent of the reported classes considered for regrades that were taught by a female instructor."
	"b. \textit{Over-confidence} measures participants' average absolute gap (between 0 and 1) between probability assignments and 0 for wrong answers in the quiz." ///
	"\textit{Under-confidence} measures participants average absolute gap (between 0 and 1) between probability assignments and 1 for correct answers in the quiz." ///
	"c. \textit{Over-optimism} measures the gap between participants' guessed score (prior belief) and true score of the quiz." ///
	"d. \textit{Uncertainty} measures how certain (in probability) they are about their guessed outcome (0 = completely certain to 1 = completely uncertain)." ///
	"e. \textit{Prior} (\textit{posterior}) \textit{downside risk} measures the prior (posterior) probability (between 0 and 1) participants assigned to adverse outcomes (i.e., true score is below the guessed score)." ///
	"f. \textit{Belief revision} measures the updates in the guessed score (posterior guessed score - prior guessed score)." ///
	"g. \textit{Extroversion}, \textit{Agreeableness}, \textit{Conscientiousness}, \textit{Neuroticism}, \textit{Openness} are the Big Five personal traits with the value ranging between 1 and 5." ///
	"h. Mean value over the 10 cost scenarios.") ///
	title("Summary statistics of the experiment") ///
	hlines(10100001000000000000000000010000000001000000000000000001);

#delimit cr

erase $Output/tab6a1.tex
erase $Output/tab6a2.tex
erase $Output/tab6bcd.tex





/*==== Table 7. Regress WTP on risk, condifence, uncertainty, and Big 5s  ====*/
#delimit;
use $Data/experiment.dta, clear;
drop if irrational==1;
mat drop _all;

su I;
local Y = round(r(mean), 0.001);

/* 1. Male + True score */
logit I i.std_male answers_correct;
margins, dydx(*);
outreg using $Output/tab7_exp_logit, replace tex  ///
 	   starlevels(10 5 1) /*se*/ starloc(1) summstat(N) landscape ///
	   ctitle("",""\"","[1]") stat(b_dfdx se_dfdx)
	   keep(1.std_male answers_correct);
#delimit cr

/* 2. Male + True score + Risk aversion*/	   
logit I i.std_male answers_correct riskaversion
margins, dydx(*)
outreg using $Output/tab7_exp_logit, merge tex ///
 	   starlevels(10 5 1) /*se*/ starloc(1) summstat(N) landscape ///
	   ctitle("",""\"","[2]") stat(b_dfdx se_dfdx) ///
	   keep(1.std_male answers_correct riskaversion)
	   

/* 3. Male + True score + Over-confidence + Under-confidence*/	   
logit I i.std_male answers_correct overconfidence underconfidence
margins, dydx(*)
outreg using $Output/tab7_exp_logit, merge tex ///
 	   starlevels(10 5 1) /*se*/ starloc(1) summstat(N) landscape ///
	   ctitle("",""\"","[3]") stat(b_dfdx se_dfdx) ///
	   keep(1.std_male answers_correct overconfidence underconfidence)
   

/* 4. Male + True score + Over-optimism, uncertainty, prior guessed score, prior downside risk  */
logit I i.std_male answers_correct overoptimism uncertainty1 prior_downside1
margins, dydx(*)
outreg using $Output/tab7_exp_logit, merge tex ///
 	   starlevels(10 5 1) /*se*/ starloc(1) summstat(N) landscape ///
	   ctitle("",""\"","[4]") stat(b_dfdx se_dfdx) ///
	   keep(1.std_male answers_correct overoptimism uncertainty1 prior_downside1)
	   
	   
/* 5. Male + True score + posterior guessed score, posterior downside risk*/
logit I i.std_male answers_correct post_num_guess post_downside 
margins, dydx(*)
outreg using $Output/tab7_exp_logit, merge tex ///
 	   starlevels(10 5 1) /*se*/ starloc(1) summstat(N) landscape ///
	   ctitle("",""\"","[5]") stat(b_dfdx se_dfdx) ///
	   keep(1.std_male answers_correct post_num_guess post_downside)
   
	   
/* 6. Male + 1)-6)*/
logit I i.std_male answers_correct riskaversion overconfidence underconfidence ///
	overoptimism uncertainty1 prior_downside1 ///
	post_num_guess post_downside 
margins, dydx(*)
outreg using $Output/tab7_exp_logit, merge tex  ///
 	   starlevels(10 5 1) /*se*/ starloc(1) summstat(N) landscape ///
	   ctitle("",""\"","[6]") stat(b_dfdx se_dfdx) ///
	   keep(1.std_male answers_correct riskaversion overconfidence underconfidence ///
		overoptimism uncertainty1 prior_downside1 ///
		post_num_guess post_downside)
	   
test answers_correct riskaversion overconfidence underconfidence ///
	overoptimism uncertainty1 prior_downside1 ///
	post_num_guess post_downside
local p_F6 = round(r(p), 0.0001)	


/* 7. Male + Big 5s	*/
logit I i.std_male answers_correct extrovert agreeable conscientious neurotic open
margins, dydx(*)

outreg using $Output/tab7_exp_logit, merge tex ///
 	   starlevels(10 5 1) /*se*/ starloc(1) summstat(N) landscape ///
	   ctitle("",""\"","[7]") stat(b_dfdx se_dfdx) ///
	   keep(1.std_male answers_correct extrovert agreeable conscientious neurotic open)
	   
test answers_correct extrovert agreeable conscientious neurotic open
local p_F7 = round(r(p), 0.001)	
	
	   
/* 8. Male + 1)-6) + Big 5s	*/   
logit I i.std_male answers_correct riskaversion overconfidence underconfidence ///
	overoptimism uncertainty1 prior_downside1 ///
	post_num_guess post_downside ///
	extrovert agreeable conscientious neurotic open
margins, dydx(*)


outreg using $Output/tab7_exp_logit, merge tex ///
   starlevels(10 5 1) /*se*/ starloc(1) summstat(N) landscape ///
   ctitle("",""\"","[8]") stat(b_dfdx se_dfdx) ///
   keep(1.std_male answers_correct riskaversion overconfidence underconfidence ///
	overoptimism uncertainty1 prior_downside1 ///
	post_num_guess post_downside ///
	extrovert agreeable conscientious neurotic open)
   
test answers_correct riskaversion overconfidence underconfidence ///
	overoptimism uncertainty1 prior_downside1 ///
	post_num_guess post_downside ///
	extrovert agreeable conscientious neurotic open
local p_F8 = round(r(p), 0.001)

frmttable using $Output/tab7_exp_logit, tex  ///
	plain landscape ///
	rtitle("Male student"\""\ ///
			"True score"\""\ ///	
			"Risk aversion$^a$"\""\ ///
			"Over-confidence$^b$"\""\ ///
			"Under-confidence$^c$"\""\ ///
			"Over-optimism$^d$"\""\ ///
			"Uncertainty$^e$"\""\ ///
			"Prior downside risk$^f$"\""\ ///
			"Posterior guessed score"\""\ ///
			"Posterior downside risk$^f$"\""\ ///
			"Extroversion$^g$"\""\ ///
			"Agreeableness$^g$"\""\ ///
			"Conscientiousness$^g$"\""\ ///
			"Neuroticism$^g$"\""\ ///
			"Openness$^g$"\""\ ///
			"Observations") ///
	addrows("F-test (p-value): all coefficients (other than \textit{Male}, \textit{True score}, \textit{Constant}) = 0","","","","","","`p_F6'","`p_F7'","`p_F8'") ///
	hlines(10100000000000000000000000000000011) multicol(1,2,8; 34,1,6) ///
	posttext("Marginal effects are reported in the table. Standard errors are reported in the parentheses." ///
	"a. \textit{Risk aversion} measures the risk aversion coefficient." ///
	"b. \textit{Over-confidence} measures participants' average absolute gap (between 0 and 1) between probability assignments and 0 for wrong answers in the quiz." ///
	"c. \textit{Under-confidence} measures participants average absolute gap (between 0 and 1) between probability assignments and 1 for correct answers in the quiz." ///
	"d. \textit{Over-optimism} measures the gap between participants' guessed score (prior belief) and true score of the quiz." ///
	"e. \textit{Uncertainty} measures how certain (in probability) they are about their guessed outcome (0 = completely certain to 1 = completely uncertain)." ///
	"f. \textit{Prior} (\textit{posterior}) \textit{downside risk} measures the prior (posterior) probability (between 0 and 1) participants assigned to adverse outcomes (i.e., true score is below the guessed score)." ///
	"g. \textit{Extroversion}, \textit{Agreeableness}, \textit{Conscientiousness}, \textit{Neuroticism}, \textit{Openness} are the Big Five personal traits with the value ranging between 1 and 5.") ///
	replace 	

log close	
