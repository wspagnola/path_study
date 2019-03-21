/*Mahathi Vojjala
11/12/2018
PATH Waves 1-3
Data Cleaning Do File*/


*************COMBINED********************
//Wave 1:
	use "E:\PATH\PATH Datasets\Wave 1 Adult\DS1001\36498-1001-Data.dta"
	run "E:\PATH\PATH Datasets\Wave 1 Adult\DS1001\36498-1001-Supplemental_syntax.do"
	run "E:\PATH\Legacy_PATH_W1-recode.do"
		gen wave1=1
		svyset [pw=r01_a_pwgt], brrweight(r01_a_pwgt1 - r01_a_pwgt100) mse vce(brr) fay(0.3)
	save "E:\PATH\PATH Datasets\W1 Adult Recoded.dta"

//Wave 2:
	use "E:\PATH\PATH Datasets\Wave 2 Adult\DS2001\36498-2001-Data.dta"
	run "E:\PATH\PATH Datasets\Wave 2 Adult\DS2001\36498-2001-Supplemental_syntax.do"
		gen wave2=1
		rename PERSONID personid
		svyset [pw=R02_A_PWGT], brrweight(R02_A_PWGT1 - R02_A_PWGT100) mse vce(brr) fay(0.3)
	save "E:\PATH\PATH Datasets\W2 Adult Recoded.dta"
		
//Create adult W1 and W2 combined dataset: 
	use "E:\PATH\PATH Datasets\W1 Adult Recoded.dta"
		merge 1:1 personid using "E:\PATH\PATH Datasets\W2 Adult Recoded.dta", nogen
		cap drop duplicate
		duplicates report personid
		duplicates tag personid, gen(duplicate)
		drop if duplicate==1
	save "E:\PATH\PATH Datasets\W1 W2 Adult Recoded Merged.dta"
	
//Wave 3:
	//Recode adult questionnaire data
		use "E:\PATH\PATH Datasets\Wave 3 Adult Questionnaire Data\DS3001\36498-3001-Data.dta"
		run "E:\PATH\PATH Datasets\Wave 3 Adult Questionnaire Data\DS3001\36498-3001-Supplemental_syntax.do"
			gen wave3=1
			rename PERSONID personid
		save "E:\PATH\PATH Datasets\W3 Adult Questionnaire Recoded No Wgts.dta"

	//Recode all-wave weights
		use "E:\PATH\PATH Datasets\Wave 3 Adult All Waves Weights\DS3101\36498-3101-Data.dta"
			rename PERSONID personid
			svyset [pw=R03_A_AWGT], brrweight(R03_A_AWGT1 - R03_A_AWGT100) mse vce(brr) fay(0.3)
		save "E:\PATH\PATH Datasets\W3 Adult All Waves Weights.dta"	
			
	//Recode single-wave weights
		use "E:\PATH\PATH Datasets\Wave 3 Adult Single Wave\DS3102\36498-3102-Data.dta"
			rename PERSONID personid
			svyset [pw=R03_A_SWGT], brrweight(R03_A_SWGT1 - R03_A_SWGT100) mse vce(brr) fay(0.3)
		save "E:\PATH\PATH Datasets\W3 Adult Single Wave Weights.dta"
		
	//Combine W3 questionnaire data with all-weights	
		use "E:\PATH\PATH Datasets\W3 Adult Questionnaire Recoded No Wgts.dta"
			merge 1:1 personid using "E:\PATH\PATH Datasets\W3 Adult All Waves Weights.dta", nogen
			cap drop duplicate
			duplicates report personid
			duplicates tag personid, gen(duplicate)
			drop if duplicate==1
		save "E:\PATH\PATH Datasets\W3 Adult Recoded All Weights Merge with No Wghts.dta"
		
	//Combine W3 questionnaire data & all-weight with single-weights
		use "E:\PATH\PATH Datasets\W3 Adult Recoded All Weights Merge with No Wghts.dta"
			merge 1:1 personid using "E:\PATH\PATH Datasets\W3 Adult Single Wave Weights.dta", nogen
			cap drop duplicate
			duplicates report personid
			duplicates tag personid, gen(duplicate)
			drop if duplicate==1
		save "E:\PATH\PATH Datasets\W3 Adult Weighted.dta", replace
		
//Create adult W1, W2, and W3 combined dataset:
	clear
	set maxvar 30000
	use "E:\PATH\PATH Datasets\W1 W2 Adult Recoded Merged.dta"
		merge 1:1 personid using "E:\PATH\PATH Datasets\W3 Adult Weighted.dta", nogen
		cap drop duplicate
		duplicates report personid
		duplicates tag personid, gen(duplicate)
		drop if duplicate==1
	save "E:\PATH\PATH Datasets\W1 W2 W3 Adult Recoded Data.dta", replace
	
****DATA CLEANING****DATA CLEANING****DATA CLEANING****

label define yesno 0"No" 1"Yes"

*Demographics*
/*Gender, Age, Race/Ethnicity, Hispanic, Education, Income, Sexual Orientation, Poverty Status, Census Region, Psychological Distress*/

	*Gender*
		*Wave 1*
		tab gender
		
		*Wave 2*
		gen gender_w2 = R02R_A_SEX
		label define gender 1"Male" 2"Female"
		label value gender_w2 gender
		label var gender_w2 "Gender at Wave 2"
		
		*Wave 3*
		gen gender_w3 = R03R_A_SEX
		label value gender_w3 gender
		label var gender_w3 "Gender at Wave 3"

	*Age 3 Category*
		*Wave 1*
		gen age3cat_w1=1 if r01r_a_agecat7>0 & r01r_a_agecat7<=2
		replace age3cat_w1=2 if r01r_a_agecat7>2 & r01r_a_agecat7<=5
		replace age3cat_w1=3 if r01r_a_agecat7>5 & r01r_a_agecat7<=7
		label define age3catlab 1"18-34 years" 2"35-64 years" 3"65+ years"
		label values age3cat_w1 age3catlab
		
		*Wave 2*
		gen age3cat_w2=1 if R02R_A_AGECAT7>0 & R02R_A_AGECAT7<=2
		replace age3cat_w2=2 if R02R_A_AGECAT7>2 & R02R_A_AGECAT7<=5
		replace age3cat_w2=3 if R02R_A_AGECAT7>5 & R02R_A_AGECAT7<=7
		label values age3cat_w2 age3catlab
		
		*Wave 3*
		gen age3cat_w3=1 if R03R_A_AGECAT7>0 & R03R_A_AGECAT7<=2
		replace age3cat_w3=2 if R03R_A_AGECAT7>2 & R03R_A_AGECAT7<=5
		replace age3cat_w3=3 if R03R_A_AGECAT7>5 & R03R_A_AGECAT7<=7
		label values age3cat_w3 age3catlab	
		
	*Race*
		*Wave 1*
		tab racecat
		
		*Wave 2*
		tab R02R_A_RACECAT3
		gen racew2= R02R_A_RACECAT3
		label define race 1"White" 2"Black" 3"Other"
		label value racew2 race
		label var racew2 "Race Wave 2"
		
		*Wave 3*
		tab R03R_A_RACECAT3
		gen racew3= R03R_A_RACECAT3
		label value racew3 race
		label var racew3 "Race Wave 3"
	
	*Hispanic*
		*Wave 1*
		gen hispanic_w1=0 if r01r_a_hisp==2
		replace hispanic_w1=1 if r01r_a_hisp==1
		label values hispanic_w1 yesno
		
		*Wave 2*
		gen hispanic_w2=0 if R02R_A_HISP==2
		replace hispanic_w2=1 if R02R_A_HISP==1
		label values hispanic_w2 yesno

		*Wave 3*
		gen hispanic_w3=0 if R03R_A_HISP==2
		replace hispanic_w3=1 if R03R_A_HISP==1
		label values hispanic_w3 yesno
		
	*Education 4 Category*
		*Wave 1*
		gen edu4cat_w1=1 if r01r_a_am0018==1
		replace edu4cat_w1=2 if r01r_a_am0018>1 & r01r_a_am0018<=3
		replace edu4cat_w1=3 if r01r_a_am0018==4
		replace edu4cat_w1=4 if r01r_a_am0018>4 & r01r_a_am0018<=6
		label define edu4catlab 1"<HS" 2"HS degree or GED" 3"Some college" 4"Bachelor's degree or more"
		label values edu4cat_w1 edu4catlab
		
		*Wave 2*
		gen edu4cat_w2=1 if R02R_A_AM0018==1
		replace edu4cat_w2=2 if R02R_A_AM0018>1 & R02R_A_AM0018<=3
		replace edu4cat_w2=3 if R02R_A_AM0018==4
		replace edu4cat_w2=4 if R02R_A_AM0018>4 & R02R_A_AM0018<=6
		label values edu4cat_w2 edu4catlab
		
		*Wave 3*
		gen edu4cat_w3=1 if R03R_A_AM0018==1
		replace edu4cat_w3=2 if R03R_A_AM0018>1 & R03R_A_AM0018<=3
		replace edu4cat_w3=3 if R03R_A_AM0018==4
		replace edu4cat_w3=4 if R03R_A_AM0018>4 & R03R_A_AM0018<=6
		label values edu4cat_w3 edu4catlab
		
	*Income 4 category*
		*Wave 1*
		gen income4cat_w1=1 if r01r_a_am0030>0 & r01r_a_am0030<=2
		replace income4cat_w1=2 if r01r_a_am0030==3
		replace income4cat_w1=3 if r01r_a_am0030==4
		replace income4cat_w1=4 if r01r_a_am0030==5
		label define income4catlab 1"<$25,000" 2"$25,000-$49,999" 3"$50,000-$99,999" 4"$100,000+"
		label values income4cat_w1 income4catlab
		
		*Wave 2*
		gen income4cat_w2=1 if R02R_A_AM0030>0 & R02R_A_AM0030<=2
		replace income4cat_w2=2 if R02R_A_AM0030==3
		replace income4cat_w2=3 if R02R_A_AM0030==4
		replace income4cat_w2=4 if R02R_A_AM0030==5
		label values income4cat_w2 income4catlab		

		*Wave 3*
		gen income4cat_w3=1 if R03R_A_AM0030>0 & R03R_A_AM0030<=2
		replace income4cat_w3=2 if R03R_A_AM0030==3
		replace income4cat_w3=3 if R03R_A_AM0030==4
		replace income4cat_w3=4 if R03R_A_AM0030==5
		label values income4cat_w3 income4catlab
	
	*Sexual Orientation*
		*Wave 1*
		tab r01r_a_sexorient2
		gen sexorient_w1 = r01r_a_sexorient2
		label define sexorient 1"Lesbian, Gay, Bisexual, Something Else" 2"Straight"
		label value sexorient_w1 sexorient
		label var sexorient_w1 "Sexual Orientation Wave 1"
		tab sexorient_w1

		*Wave 2*
		tab R02R_A_SEXORIENT2
		gen sexorient_w2 = R02R_A_SEXORIENT2
		label value sexorient_w2 sexorient
		label var sexorient_w2 "Sexual Orientation Wave 2"
		tab sexorient_w2

		*Wave 3*
		tab R03R_A_SEXORIENT2
		gen sexorient_w3 = R03R_A_SEXORIENT2
		label value sexorient_w3 sexorient
		label var sexorient_w3 "Sexual Orientation Wave 3"
		tab sexorient_w3
		
	*Poverty Status*
		*Wave 1*
		gen poverty= r01r_povcat2
		replace poverty=. if poverty== -99988 | poverty==-99977 | poverty==-97777
		label var poverty "Poverty at Wave 1"
		label define poverty 1"Below Poverty Level" 2"At or Above Poverty Level"
		label value poverty poverty

	*Census Region*
		*Wave 1*
		gen region= r01x_cb_region
		label define region 1"Northeast" 2"Midwest" 3"South" 4"West"
		label value region region
		label var region "Census Region at Wave 1"
		tab region
	
	*Past 30-Day Psychological Distress*
		*Wave 1*
		gen psychdist_w1=0 if (r01_ax0161>=2 & r01_ax0161 !=.) & (r01_ax0163>=2 & r01_ax0163 !=.)
		replace psychdist_w1=1 if r01_ax0161==1 | r01_ax0163==1
		label values psychdist_w1 yesno
		
		*Wave 2*
		gen psychdist_w2=0 if (R02_AX0161>=2 & R02_AX0161 !=.) & (R02_AX0163>=2 & R02_AX0163 !=.)
		replace psychdist_w2=1 if R02_AX0161==1 | R02_AX0163==1
		label values psychdist_w2 yesno

		*Wave 3*
		gen psychdist_w3=0 if (R03_AX0161>=2 & R03_AX0161 !=.) & (R03_AX0163>=2 & R03_AX0163 !=.)
		replace psychdist_w3=1 if R03_AX0161==1 | R03_AX0163==1
		label values psychdist_w3 yesno

*IVs*
	*Smoking Status*
		*Wave 1*
		gen smkstatus_w1=0 if r01r_a_nvr_cigs==1 & r01r_a_cur_estd_cigs==2
		replace smkstatus_w1=1 if r01r_a_cur_estd_cigs==1
		replace smkstatus_w1=2 if r01r_a_cur_estd_cigs==2 & r01r_a_nvr_cigs==2
		replace smkstatus_w1=3 if r01r_a_fmr_estd_cigs==1
		label define smkstatuslab1 0"Never Smoker" 1"Current Established Smoker" 2"Current Non-Estd Smoker" 3"Former Established Smoker"
		label values smkstatus_w1 smkstatuslab1
		
		*Without Current Non-Estd*
		gen smkstatusw1=0 if r01r_a_nvr_cigs==1 
		replace smkstatusw1=1 if r01r_a_cur_estd_cigs==1
		replace smkstatusw1=2 if r01r_a_fmr_estd_cigs==1
		label define smkstatuslab 0"Never Smoker" 1"Current Established Smoker" 2"Former Established Smoker"
		label values smkstatusw1 smkstatuslab
		
		*Established Cig Smoker Status W1*
		gen estd= smkstatus_w1 if smkstatus_w1==1 | smkstatus_w1==2
		label values estd smkstatuslab1
		
		*Wave 2*
		gen smkstatus_w2=0 if R02R_A_EVR_CIGS==2 & R02R_A_CUR_ESTD_CIGS==2
		replace smkstatus_w2=1 if R02R_A_CUR_ESTD_CIGS==1 
		replace smkstatus_w2=2 if R02R_A_CUR_ESTD_CIGS==2 & R02R_A_EVR_CIGS==1
		replace smkstatus_w2=3 if R02R_A_FMR_ESTD_CIGS==1
		label values smkstatus_w2 smkstatuslab1
		
		*Established Cig Smoker Status W2*
		gen estdw2= smkstatus_w2 if smkstatus_w2==1 | smkstatus_w2==2
		label values estdw2 smkstatuslab1
	
		*Wave 3*
		gen smkstatus_w3=0 if R03R_A_EVR_CIGS==2 & R03R_A_CUR_ESTD_CIGS==2
		replace smkstatus_w3=1 if R03R_A_CUR_ESTD_CIGS==1 
		replace smkstatus_w3=2 if R03R_A_CUR_ESTD_CIGS==2 & R03R_A_EVR_CIGS==1
		replace smkstatus_w3=3 if R03R_A_FMR_ESTD_CIGS==1
		label values smkstatus_w3 smkstatuslab1
		
	/*Smoking Status Including Experimental Smokers
		Using  R01R_A_FMR_EXPR_CIGS: DERIVED - Wave 1 Adult Former Experimental Cigarette User
		R01R_A_CUR_EXPR_CIGS: DERIVED - Wave 1 Adult Former Current Cigarette User
		Wave 1*/
		gen smokingstatuswave1=.
		replace smokingstatuswave1=0 if r01r_a_nvr_cigs==1
		replace smokingstatuswave1=1 if r01r_a_cur_estd_cigs==1 | r01r_a_cur_expr_cigs==1
		replace smokingstatuswave1=2 if r01r_a_fmr_estd_cigs==1 | r01r_a_fmr_expr_cigs==1
		label define smoking 0"Never" 1"Current" 2"Former"
		label value smokingstatuswave1 smoking
		tab smokingstatuswave1
		
	/*Using  R02R_A_FMR_EXPR_CIGS: DERIVED - Wave 2 Adult Former Experimental Cigarette User
		R02R_A_CUR_EXPR_CIGS: DERIVED - Wave 2 Adult Former Current Cigarette User
		Wave 2*/
		gen smokingstatuswave2=.
		replace smokingstatuswave2=0 if R02R_A_EVR_CIGS==2
		replace smokingstatuswave2=1 if R02R_A_CUR_ESTD_CIGS==1 | R02R_A_CUR_EXPR_CIGS==1
		replace smokingstatuswave2=2 if R02R_A_FMR_ESTD_CIGS==1 | R02R_A_FMR_EXPR_CIGS==1
		label value smokingstatuswave2 smoking
		tab smokingstatuswave2

	/*Using  R03R_A_FMR_EXPR_CIGS: DERIVED - Wave 2 Adult Former Experimental Cigarette User
		R03R_A_CUR_EXPR_CIGS: DERIVED - Wave 2 Adult Former Current Cigarette User
		Wave 2*/
		gen smokingstatuswave3=.
		replace smokingstatuswave3=0 if R03R_A_EVR_CIGS==2
		replace smokingstatuswave3=1 if R03R_A_CUR_ESTD_CIGS==1 | R03R_A_CUR_EXPR_CIGS==1
		replace smokingstatuswave3=2 if R03R_A_FMR_ESTD_CIGS==1 | R03R_A_FMR_EXPR_CIGS==1
		label value smokingstatuswave3 smoking
		tab smokingstatuswave3

	/*Smoking Status at Wave 1
	Using everyday/some day questions.
		R01_AC1002: Ever smoked a cigarette, even one or two puffs 
		R01_AC1003: Now smoke cigarettes 
		Now Cig defined as current smokers who said everyday/someday to 1003; former smokers who said not at all to 1003; never smokers from 1002 */
		gen nowcig_w1= r01_ac1002
		replace nowcig_w1=3 if nowcig_w1==2
		replace nowcig_w1=1 if r01_ac1003==1 | r01_ac1003==2
		replace nowcig_w1=2 if r01_ac1003==3
		tab nowcig_w1
		display 13545+11640
		label define nowcig 1"Current" 2"Former" 3"Never"
		label value nowcig_w1 nowcig
		label var nowcig_w1 "Smoking Status Wave 1 Using Ever and Now Smoke"
	
	/*Smoking Status at Wave 2
	Using everyday/some day questions.
		R02_AC1002_12M: In past 12 months, smoked a cigarette, even one or two puffs
		R02_AC1003: Now smoke cigarettes */
		gen nowcig_w2= R02_AC1002_12M
		replace nowcig_w2=3 if nowcig_w2==2
		replace nowcig_w2=1 if R02_AC1003==1 | R02_AC1003==2
		replace nowcig_w2=2 if R02_AC1003==3
		tab nowcig_w2
		label value nowcig_w2 nowcig
		label var nowcig_w2 "Smoking Status Wave 2 Using Ever and Now Smoke"

	/*Smoking Status at Wave 3
	Using everyday/some day questions.
		R03_AC1002_12M: In past 12 months, smoked a cigarette, even one or two puffs
		R03_AC1003: Now smoke cigarettes */	
		gen nowcig_w3= R03_AC1002_12M
		replace nowcig_w3=3 if nowcig_w3==2
		replace nowcig_w3=1 if R03_AC1003==1 | R03_AC1003==2
		replace nowcig_w3=2 if R03_AC1003==3
		tab nowcig_w3
		label value nowcig_w3 nowcig
		label var nowcig_w3 "Smoking Status Wave 3 Using Ever and Now Smoke"
	// 1: Current smokers; those who said everyday & some days
	// 2: Fmr smokers; those who said not at all to 1003
	// 3: Never smokers; those who said no p12month use	
	
	*Cig status at Wave 1*
		gen cigstatus_w1= r01_ac1003
		replace cigstatus_w1=. if cigstatus_w1==-1 | cigstatus_w1==-7 | cigstatus_w1==-8 | cigstatus_w1==-97777
		label define cigstatus_w1 1"Everyday" 2"Somedays" 3"Not at all"
		label value cigstatus_w1 cigstatus_w1
		label var cigstatus_w1 "Now Smoke Cigs at Wave 1"
		tab cigstatus_w1
	
	*ED/SD Cig Status Variable at Wave 1*
		gen edsdcigw1=cigstatus_w1
		replace edsdcigw1=1 if edsdcigw1==2
		label define edsdcig 1"ED/SD" 2"Not at all"
		label value edsdcigw1 edsdcig
		label var edsdcigw1 "Everyday/Someday Now Smoke Cigs W1"
	
	*Cig Status Wave 2*
	gen cigstatus_w2= R02_AC1003
	replace cigstatus_w2=. if cigstatus_w2==-1 | cigstatus_w2==-7 | cigstatus_w2==-8 | cigstatus_w2==-97777
	label value cigstatus_w2 cigstatus_w1
	label var cigstatus_w2 "Now Smoke Cigs at Wave 2"
	tab cigstatus_w2
	
	*Cig Status Wave 3*
	gen cigstatus_w3= R03_AC1003
	replace cigstatus_w3=. if cigstatus_w3==-1 | cigstatus_w3==-7 | cigstatus_w3==-8 | cigstatus_w3==-97777
	label value cigstatus_w3 cigstatus_w1
	label var cigstatus_w3 "Now Smoke Cigs at Wave 3"
	tab cigstatus_w3

*DVs*
	*Smoking trajectory ; no change=people who stayed the same; 
	//cessation who were current in wave 1 and who became former in wave 2; 
	//initiation - people who were never in wave 1 and who became current and former in wave 2 are initiation; 
	//relapsers - people who were never in wave 1 and who are current in wave 2 are relapsers.
	gen smokingtrajectory=.
	replace smokingtrajectory=0 if (smkstatus_w1==0 & smkstatus_w2==0 & smkstatus_w3==0) | (smkstatus_w1==1 & smkstatus_w2==1 & smkstatus_w3==1) | (smkstatus_w1==2 & smkstatus_w2==2 & smkstatus_w3==2) | (smkstatus_w1==3 & smkstatus_w2==3 & smkstatus_w3==3)
	replace smokingtrajectory=1 if (smkstatus_w1==1 | smkstatus_w1==2 ) & (smkstatus_w2==3 & smkstatus_w2==3)
	replace smokingtrajectory=2 if smkstatus_w1==0 & (smkstatus_w2==1 | smkstatus_w1==2 | smkstatus_w3==1 | smkstatus_w3==2)
	replace smokingtrajectory=3 if smkstatus_w1==3 & (smkstatus_w2==1 | smkstatus_w2==2 | smkstatus_w3==1 | smkstatus_w3==2)
	label value smokingtrajectory trajectory
	label var smokingtrajectory "Smoking Status Trajectory from Wave 1 to Wave 3"
	tab smokingtrajectory
	svy: tab smokingtrajectory
**************4.57 % Cessation Rate!!**************
*****Missing experimental smokers though*****

	*Smoking trajectory without no change*
	gen smktraj=smokingtrajectory
	replace smktraj=. if smktraj==0
	label value smktraj trajectory
	label var smoktraj "Smoking Status Trajectory from W1-W3"
	svy: tab smktraj
		
	*Initiators*
	gen initiation=0 if r01r_a_nvr_cigs==1 & R02R_A_EVR_CIGS==2
	replace initiation=1 if r01r_a_nvr_cigs==1 & (R02R_A_CUR_ESTD_CIGS==1 | R02R_A_FMR_ESTD_CIGS==1)
	label values initiation yesno
	
	*Quitters*
	gen cessation=0 if r01r_a_cur_estd_cigs==1 & R02R_A_CUR_ESTD_CIGS==1
	replace cessation=1 if r01r_a_cur_estd_cigs==1 & R02R_A_FMR_ESTD_CIGS==1
	label values cessation yesno
	*what about those who said no to curr estd users in wave 2? They are no longer current users but are still using/not quitting*
	
	*Relapsers*
	gen relapse=0 if r01r_a_fmr_estd_cigs==1 & R02R_A_FMR_ESTD_CIGS==1
	replace relapse=1 if r01r_a_fmr_estd_cigs==1 & R02R_A_CUR_ESTD_CIGS==1
	label values relapse yesno
	
	*Quit Rate Variables*
	/*Quit Rate at Wave 2 R02_AC1009_NN and R02_AC1009_UN
	How long since last smoked .. in numbers and units*/
	gen abstw2=.
	*One day pt prevalence – smoked earlier today. 
	*Num: 1; Unit: days
	replace abstw2=0 if (R02_AC1009_NN==1) & R02_AC1009_UN==1
	*Num: 2-6; Unit: days
	replace abstw2=1 if (R02_AC1009_NN>=2 & R02_AC1009_NN<=6) & R02_AC1009_UN==1
	**At least 7 day pt prevalence
	**Num: 7-29; Unit: days
	replace abstw2=2 if (R02_AC1009_NN>=7 & R02_AC1009_NN<=29) & R02_AC1009_UN==1
	**At least 30 day pt prev
	**Num: 30-90; unit: days; Num: 1; Unit: months
	replace abstw2=3 if (R02_AC1009_NN>=30 & R02_AC1009_NN<=90) & R02_AC1009_UN==1
	replace abstw2=3 if (R02_AC1009_NN==1) & R02_AC1009_UN==2
	replace abstw2=3 if (R02_AC1009_NN==2) & R02_AC1009_UN==2
	**At least 3 mo pt prev
	**Num: 91-181; Unit: days; Num: 3, 4, 5; Unit: mos
	replace abstw2=4 if (R02_AC1009_NN>=91 & R02_AC1009_NN<=181) & R02_AC1009_UN==1
	replace abstw2=4 if (R02_AC1009_NN>=3 & R02_AC1009_NN<=5) & R02_AC1009_UN==2
	**At least 6 mo pt prevalence
	**Num: 182-364; Unit: days; Num: 6,7,8,9,10,11; Unit: Mos
	replace abstw2=5 if (R02_AC1009_NN>=182 & R02_AC1009_NN<=364) & R02_AC1009_UN==1
	replace abstw2=5 if (R02_AC1009_NN>=6 & R02_AC1009_NN<=11) & R02_AC1009_UN==2
	**At least 1 year pt prev
	**Nums: 365+; Unit: days; Num: 12+; Unit: Mos
	replace abstw2=6 if (R02_AC1009_NN>=365) & R02_AC1009_UN==1
	replace abstw2=6 if (R02_AC1009_NN>=12) & R02_AC1009_UN==2
	tab abstw2
	label define abstw2 0"1 day" 1"2-6 day" 2"At Least 7 days" 3"1 mo" 4"3 month" 5"6 month" 6"1 year"
	label value abstw2 abstw2
	label var abstw2 "Abstinence at Wave 2"
	tab abstw2

	
	/*Quit Rate at Wave 3 R03_AC1009_NN and R03_AC1009_UN
	How long since last smoked .. in numbers and units*/
	gen abstw3=.
	*One day pt prevalence – smoked earlier today. 
	*Num: 1; Unit: days
	replace abstw3=0 if (R03_AC1009_NN==1) & R03_AC1009_UN==1
	*Num: 2-6; Unit: days
	replace abstw3=1 if (R03_AC1009_NN>=2 & R03_AC1009_NN<=6) & R03_AC1009_UN==1
	**At least 7 day pt prevalence
	**Num: 7-29; Unit: days
	replace abstw3=2 if (R03_AC1009_NN>=7 & R03_AC1009_NN<=29) & R03_AC1009_UN==1
	**At least 30 day pt prev
	**Num: 30-90; unit: days; Num: 1; Unit: months
	replace abstw3=3 if (R03_AC1009_NN>=30 & R03_AC1009_NN<=90) & R03_AC1009_UN==1
	replace abstw3=3 if (R03_AC1009_NN==1) & R03_AC1009_UN==2
	replace abstw3=3 if (R03_AC1009_NN==2) & R03_AC1009_UN==2
	**At least 3 mo pt prev
	**Num: 91-181; Unit: days; Num: 3, 4, 5; Unit: mos
	replace abstw3=4 if (R03_AC1009_NN>=91 & R03_AC1009_NN<=181) & R03_AC1009_UN==1
	replace abstw3=4 if (R03_AC1009_NN>=3 & R03_AC1009_NN<=5) & R03_AC1009_UN==2
	**At least 6 mo pt prevalence
	**Num: 182-364; Unit: days; Num: 6,7,8,9,10,11; Unit: Mos
	replace abstw3=5 if (R03_AC1009_NN>=182 & R03_AC1009_NN<=364) & R03_AC1009_UN==1
	replace abstw3=5 if (R03_AC1009_NN>=6 & R03_AC1009_NN<=11) & R03_AC1009_UN==2
	**At least 1 year pt prev
	**Nums: 365+; Unit: days; Num: 12+; Unit: Mos
	replace abstw3=6 if (R03_AC1009_NN>=365) & R03_AC1009_UN==1
	replace abstw3=6 if (R03_AC1009_NN>=12) & R03_AC1009_UN==2
	label value abstw3 abstw2
	label var abstw3 "Abstinence at Wave 2"
	tab abstw3

	**Quit Rate from W1 - W2*
	*Quit (y/n)*
	gen quit=0 if r01_ac1003>=1 & r01_ac1003<3 & R02_AC1003>=1 & R02_AC1003<3
	replace quit=1 if r01_ac1003>=1 & r01_ac1003<3 & (R02_AC1003==3 | R02_AC1002_12M==2)
	label values quit yesno
	tab quit
	
	*Quit 3 categories*
	gen quit1=0 if r01_ac1003>=1 & r01_ac1003<3 & R02_AC1003>=1 & R02_AC1003<3
	replace quit1=1 if r01_ac1003>=1 & r01_ac1003<3 & (R02_AC1003==3 | R02_AC1002_12M==2)
	replace quit1=2 if r01_ac1003==3 & (R02_AC1003==3 | R02_AC1002_12M==2)
	tab quit1
	label define quit1 0"No" 1"Yes" 2"Not at All/No Change"
	label value quit1 quit1
	label var quit1 "Quit at Wave 2"
	
	**Quit Rate from W1-W3*
	*Quit (y/n)
	gen quit2=0 if r01_ac1003>=1 & r01_ac1003<3 & R03_AC1003>=1 & R03_AC1003<3
	replace quit2=1 if r01_ac1003>=1 & r01_ac1003<3 & (R03_AC1003==3 | R03_AC1002_12M==2)
	label values quit2 yesno
	tab quit2

	*Quit 3 categories - Wave 3*
	gen quit3=0 if r01_ac1003>=1 & r01_ac1003<3 & R03_AC1003>=1 & R03_AC1003<3
	replace quit3=1 if r01_ac1003>=1 & r01_ac1003<3 & (R03_AC1003==3 | R03_AC1002_12M==2)
	replace quit3=2 if r01_ac1003==3 & (R03_AC1003==3 | R03_AC1002_12M==2)
	label value quit3 quit1
	label var quit3 "Quit at Wave 3-3 Cat"

	***FINAL QUIT VARIABLE FOR WAVE 3***
	***Baseline at w2***
	gen quit2_3=0 if (R02_AC1003>=1 & R02_AC1003<3) & (R03_AC1003>=1 & R03_AC1003<3)
	replace quit2_3=1 if (R02_AC1003>=1 & R02_AC1003<3) & (R03_AC1003==3 | R03_AC1002_12M==2)
	label value quit2_3 quit1
	label var quit2_3 "Quit at Wave 3-W2 Baseline"

	***Past 30 day cig use at Wave 2***
	//R02_AC1023_NN: In past 30 days, average number of cigarettes smoked per day on days smoked - Number
	//R02_AC1023_UN: In past 30 days, average number of cigarettes smoked per day on days smoked - Unit
	gen p30cigsmoke_w2=.
	replace p30cigsmoke_w2=0 if ( R02_AC1023_NN==0) & ( R02_AC1023_UN==1 & R02_AC1023_UN==2)
	replace p30cigsmoke_w2=0 if ( R02_AC1023_NN==0) & ( R02_AC1023_UN==1 | R02_AC1023_UN==2)
	replace p30cigsmoke_w2=1 if ( R02_AC1023_NN>=1) & ( R02_AC1023_UN==1 | R02_AC1023_UN==2)
	tab p30cigsmoke_w2
	
	***Derived P30 Day Cig Use at Wave 2***
	//R02R_A_P30D_CIGS: DERIVEDWave 2 Adult respondents who smoked a cigarette in the past 30 days. Algorithm: IF R02R_A_EVR_CIGS = 1 AND (R02_AC1004=1 OR R02_AC0100 in (1,2,3,4)) OR (0<=R02_AC1009_NN<=30 AND R02_AC1009_UN=1) OR (R02_AC1009_NN=1 AND R02_AC1009_UN=2) OR R02_AC1022>0), THEN R02R_A_P30D_CIGS=1; ELSE IF R02R_A_EVR_CIGS=2 OR R02_AC1004=2 OR (R02_AC1009_NN>30 AND R02_AC1009_UN=1) OR (R02_AC1009_NN>1 AND R02_AC1009_UN=2) OR R02_AC1022=0
	tab R02R_A_P30D_CIGS
	
	*Quit at Wave 2 including P30 day*
	gen quitw2_2=1 if (smkstatus_w1==1) & (R02_AC1003==3 & p30cigsmoke_w2==0)
	replace quitw2_2=0 if (smkstatus_w1==1) & (R02_AC1003>=1 & R02_AC1003<3) & (p30cigsmoke_w2==1)
	label value quitw2_2 quit1
	
	**
	gen quitw2_3=1 if (smkstatus_w1==1) & (R02_AC1003==3 & R02R_A_P30D_CIGS==2)
	replace quitw2_3=0 if (smkstatus_w1==1) & (R02_AC1003>=1 & R02_AC1003<3) & (R02R_A_P30D_CIGS==1)
	label value quitw2_3 quit1
	
	**FINAL ONE**
	gen quitw2_4=0 
	replace quitw2_4=1 if (smkstatus_w1==1) & (R02_AC1003==3 & R02R_A_P30D_CIGS==2)
	label value quitw2_4 quit1
	
	**FINAL QUIT FOR W3 FOR P30 Day (WAVE 2-WAVE 3)**
	gen quitw3_1=0
	replace quitw3_1=1 if (smkstatus_w2==1) & (R03_AC1003==3 & R03R_A_P30D_CIGS==2)
	label value quitw3_1 quit1
	
	**FINAL QUIT W1-W3 P30 DAY**
	gen quitw3_2=0
	replace quitw3_2=1 if (smkstatus_w1==1) & (R03_AC1003==3 & R03R_A_P30D_CIGS==2)
	label value quitw3_2 quit1
	
	gen smkw1 = smkstatus_w1
tab smkw1
tab estd
tab smkw1
label values smkw1 smkstatuslab1
tab smkw1
replace smkw1==0 ifsmkw1==3
replace smkw1=0 ifsmkw1==3
replace smkw1=0 if smkw1==3
replace smkw1=1 if smkw1==2
tab smkw1
tab smkstatus_w2 if smkw1==1
tab cigstatus_w2
tab cigstatus_w2 if smkw1==1
tab smkstatus_w3 if smkw1==1
tab cigstatus_w3 if smkw1==1
gen smkw2=smkstatus_w2
tab smkw2
tab smkstatus_w2
label values smkw2 smkstatuslab1
replace smkw2=0 if smkw2==3
replace smkw2=1 if smkw2==2
tab smkw2
tab smkstatus_w3 if smkw2==1
tab cigstatus_w3 if smkw2==1
tab estd
tab cigstatus_w2 if estd==1
tab estd if estd==1
tab smkstatus_w2 if estd==1
tab smkstatus_w 3 if estd==1
tab smkstatus_w3 if estd==1
tab cigstatus_w3 if estd==1
do "C:\Users\mv1459\AppData\Local\Temp\STD49d0_000000.tmp"
tab estdw2
tab cigstatus_w3 if estdw2==1
tab smkstatus_w3 if estdw2==1
tab smkw1
tab smkw2
tab estd
tab estdw2
tab smkstatus_w2 if estd==2
tab cigstatus_w2 if estd==2
tab cigstatus_w3 if estd==2
tab smkstatus_w3 if estd==2
tab estdw2
tab cigstatus_w3 if estdw2==2
tab smkstatus_w3 if estdw2==2


