*=================================================================================================================================================
********************************Codebook for data construction************************************************************************************
*=================================================================================================================================================
***********************A Micro-level Evidence of Impact of Armed Conflict on Child Health in Nigeria**********************************************
*=================================================================================================================================================
* Using DHS 2003, 2008 and 2013, and Aid data from spatial analysis
*=================================================================================================================================================
clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\lastest_data.dta" 
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC"

drop if b2==2009

rename Year Years
xtset Years

encode hw15, gen(nhw15) 
destring hw2, gen(nhw2) force
destring hw3, gen(nhw3) force
destring v730, gen(partnerage) force

//Child Gender//
order b4
tab b4
egen Gender= group( b4 )
tab Gender
gen Child_Gender=.
order Child_Gender
replace Child_Gender=1 if Gender==2
replace Child_Gender=0 if Gender==1
replace Child_Gender=. if Gender==.
tab Child_Gender
label var Child_Gender "Child Gender"

*===============================================================================
* Child Health Variables
*===============================================================================
//infant deaths//
tab b7
gen infantDeaths=.
replace infantDeaths=1 if b7<=12
replace infantDeaths=0 if b7>=13
replace infantDeaths=. if b7==.
tab infantDeaths
label var infantDeaths "Infant mortality"

//Moderate and Severe Stunting, respectively//
replace hw70=. if hw70==9999
tab hw70
gen moderatestunting=.
replace moderatestunting=1 if hw70<-200
replace moderatestunting=0 if hw70>=-200
replace moderatestunting=. if hw70==.
tab moderatestunting

tab hw70
gen severestunting=.
replace severestunting=1 if hw70<-300
replace severestunting=0 if hw70>=-300
replace severestunting=. if hw70==.
tab severestunting

//Moderate and Severe Underweight, respectively//
tab hw71
gen moderateunderweight=.
replace moderateunderweight=1 if hw71<-200
replace moderateunderweight=0 if hw71>=-200
replace moderateunderweight=. if hw71==.
tab moderateunderweight

tab hw71
gen severeunderweight=.
replace severeunderweight=1 if hw71<-300
replace severeunderweight=0 if hw71>=-300
replace severeunderweight=. if hw71==.
tab severeunderweight

//Moderate and Severe Wasting, respectively//
tab hw72
gen moderatewasting=.
replace moderatewasting=1 if hw72<-200
replace moderatewasting=0 if hw72>=-200
replace moderatewasting=. if hw72==.
tab moderatewasting

tab hw72
gen severewasting=.
replace severewasting=1 if hw72<-300
replace severewasting=0 if hw72>=-300
replace severewasting=. if hw72==.
tab severewasting

//z Score//
//simple sample weighting using DHS for clusters only, no strata; must multiply by 1000000

gen weight=v005/1000000
svyset [pweight=weight], psu(v021)

//hw15 is lying (1) and standing (2) height measure; clean DHS code

gen newm=nhw15 if nhw15!=9
replace newm=. if nhw15==1
replace newm=. if nhw15==3
replace newm=1 if nhw15==2
replace newm=2 if nhw15==4

//hw3 is height, clean DHS code and converts to cm
gen newh=nhw3/10 if nhw3!=9999

//hw2 is height in kg to one decimal w/o the decimal; convert to kg w/decimal; clean DHS code and
gen neww=nhw2/10 if nhw2!=9999

//run zscore06. age in months (hw1) and gender (b4) need no cleaning in this dataset
zscore06, a(hw1) s(Gender) h(newh) w(neww) measure(newm) male(2) female(1)

//remove biologically implausible scores
replace haz06=. if haz06<-6 | haz06>6
replace waz06=. if waz06<-6 | waz06>5
replace whz06=. if whz06<-5 | whz06>5
replace bmiz06=. if bmiz06<-5 | bmiz06>5

*Rename Month of Interview***
rename v006 monthofinterview

*===========================================================================
* Generating for the Buffer Zones 
*======================================================================
tab bufferkm
gen inter5km=.
replace inter5km=1 if bufferkm==5
replace inter5km=0 if bufferkm==0
replace inter5km=0 if bufferkm==15
replace inter5km=0 if bufferkm==25
replace inter5km=0 if bufferkm==45
tab inter5km

gen inter15km=.
replace inter15km=1 if bufferkm==15
replace inter15km=0 if bufferkm==0
replace inter15km=0 if bufferkm==5
replace inter15km=0 if bufferkm==25
replace inter15km=0 if bufferkm==45
tab inter15km

gen inter25km=.
replace inter25km=1 if bufferkm==25
replace inter25km=0 if bufferkm==0
replace inter25km=0 if bufferkm==5
replace inter25km=0 if bufferkm==15
replace inter25km=0 if bufferkm==45
tab inter25km

gen inter45km=.
replace inter45km=1 if bufferkm==45
replace inter45km=0 if bufferkm==0
replace inter45km=0 if bufferkm==5
replace inter45km=0 if bufferkm==15
replace inter45km=0 if bufferkm==25
tab inter45km

gen inter51km=.
replace inter51km=1 if bufferkm==0
replace inter51km=0 if bufferkm==5
replace inter51km=0 if bufferkm==15
replace inter51km=0 if bufferkm==25
replace inter51km=0 if bufferkm==45
tab inter51km

//Obtain the mean of the zscore variables, adjusting for the month of interview
svypxcat haz06 , xvar(inter15km) adjusted( monthofinterview)

svypxcat waz06 , xvar(inter15km) adjusted( monthofinterview)

svypxcat whz06 , xvar(inter15km) adjusted( monthofinterview)

*================================================================================
****************COMMON TREND GRAPH***********************************************
*================================================================================
use "C:\Users\samsung\Desktop\aerc\Graph Data.dta" 

twoway line Nonconflict_haz conflict_haz Years, xlabel(2003 2008 2013 2018) xline(2009,lcol(red))

twoway line Nonconflict_waz conflict_waz Years, xlabel(2003 2008 2013 2018) xline(2009,lcol(red))

twoway line nonconflict_whz conflict_whz Years, xlabel(2003 2008 2013 2018) xline(2009,lcol(red))
  
*===============================================================================
* Demographics Variables
*===============================================================================
//child age in month//
rename hw1 childagemonth

//Ethnicity//
tab v131 
encode v131, gen(ethnicity)

//month of birth//
rename b1 monthofbirth

//Head of Household Age//
rename v152 HHage
destring HHage, gen(nHHage) force
tab HHage

//Head of Household Gender//
tab v151 
egen HHGender= group(v151)
tab HHGender
gen HH_Gender=.
replace HH_Gender=1 if Gender==2
replace HH_Gender=0 if Gender==1
replace HH_Gender=. if Gender==.
tab HH_Gender
label var HH_Gender "Head of Household Gender"

//Mother's Height//
tab v438 
destring v438, gen(nv438) force
tab nv438
replace nv438=. if nv438==9999 
gen motherheight=log(nv438)

//Mother's Marital Status; Married (1), not married (0)//
tab v501 
egen marital= group(v501)
tab marital
gen maritalstatus=.
replace maritalstatus=1 if marital==1
replace maritalstatus=1 if marital==4
replace maritalstatus=0 if marital==3
replace maritalstatus=0 if marital==2
replace maritalstatus=0 if marital==5
replace maritalstatus=0 if marital==6
replace maritalstatus=0 if marital==7
replace maritalstatus=. if marital==.
tab maritalstatus

//Migration//*****Available for only 2008, 2013 and 2018********
tab v168
egen migrate= group(v168)
tab migrate
gen migration=.
replace migration=1 if migrate==3
replace migration=1 if migrate==5
replace migration=0 if migrate==2
replace migration=0 if migrate==4
replace migration=. if migrate==9
tab migration

//Mother's Education attainment//
tab v106
egen Edu_Attainment = group( v106)
tab Edu_Attainment
gen Education=.
replace Education=1 if Edu_Attainment==1
replace Education=1 if Edu_Attainment==3
replace Education=1 if Edu_Attainment==4
replace Education=0 if Edu_Attainment==2
replace Education=. if Edu_Attainment==.
tab Education
label var Education "Mother Education Status"

//Partner's Education attainment//
tab v701
egen education= group(v701)
tab education
gen partnereducation=.
replace partnereducation=1 if education==3
replace partnereducation=1 if education==8
replace partnereducation=1 if education==5
replace partnereducation=1 if education==10
replace partnereducation=1 if education==6
replace partnereducation=1 if education==11
replace partnereducation=0 if education==4
replace partnereducation=0 if education==9
replace partnereducation=. if education==1
replace partnereducation=. if education==2
replace partnereducation=. if education==7
replace partnereducation=. if education==.
tab partnereducation
label var partnereducation "Mother's Partner Education Status"

//Number of household members//
tab v136 
rename v136 No_household_members

//Number of children under the age of 5 in the household//
tab v137 
rename v137 no_childrenU5

//Birth order// 
tab bord
rename bord birth_order

//Birth Date//
tab b2
rename b2 Birth_Date

//Child age//
tab b8
rename b8 child_age

//Mother's age//
rename Motherage motherage

//Wealth index//
tab v190
egen Wealth_Ind= group(v190)
tab Wealth_Ind
gen WID=.
replace WID=1 if Wealth_Ind==2
replace WID=1 if Wealth_Ind==3
replace WID=0 if Wealth_Ind==1
replace WID=0 if Wealth_Ind==4
replace WID=0 if Wealth_Ind==5
replace WID=. if Wealth_Ind==.
tab WID
label var WID "Wealth Index"

//New Wealth index//
tab Wealth_Ind
gen WIDN=.
replace WIDN=2 if Wealth_Ind==1
replace WIDN=0 if Wealth_Ind==4
replace WIDN=0 if Wealth_Ind==5
replace WIDN=1 if Wealth_Ind==2
replace WIDN=1 if Wealth_Ind==3
replace WIDN=. if Wealth_Ind==.
tab WIDN
label var WIDN "New Wealth Index"

tab WIDN
gen Poor=.
replace Poor=1 if WIDN==1
replace Poor=0 if WIDN==2
replace Poor=0 if WIDN==0

gen Middle=.
replace Middle=1 if WIDN==2
replace Middle=0 if WIDN==1
replace Middle=0 if WIDN==0

gen Rich=.
replace Rich=1 if WIDN==0
replace Rich=0 if WIDN==1
replace Poor=0 if WIDN==2

tab Wealth_Ind
gen firstQuintle=.
replace firstQuintle=1 if Wealth_Ind==3
replace firstQuintle=0 if Wealth_Ind==1
replace firstQuintle=0 if Wealth_Ind==2
replace firstQuintle=0 if Wealth_Ind==4
replace firstQuintle=0 if Wealth_Ind==5
tab firstQuintle

tab Wealth_Ind
gen secondQuintle=.
replace secondQuintle=1 if Wealth_Ind==2
replace secondQuintle=0 if Wealth_Ind==1
replace secondQuintle=0 if Wealth_Ind==3
replace secondQuintle=0 if Wealth_Ind==4
replace secondQuintle=0 if Wealth_Ind==5
tab secondQuintle

tab Wealth_Ind
gen middleQuintle=.
replace middleQuintle=1 if Wealth_Ind==1
replace middleQuintle=0 if Wealth_Ind==2
replace middleQuintle=0 if Wealth_Ind==3
replace middleQuintle=0 if Wealth_Ind==4
replace middleQuintle=0 if Wealth_Ind==5
tab middleQuintle

tab Wealth_Ind
gen fourthQuintle=.
replace fourthQuintle=1 if Wealth_Ind==4
replace fourthQuintle=0 if Wealth_Ind==1
replace fourthQuintle=0 if Wealth_Ind==2
replace fourthQuintle=0 if Wealth_Ind==3
replace fourthQuintle=0 if Wealth_Ind==5
tab fourthQuintle

tab Wealth_Ind
gen HighestQuintle=.
replace HighestQuintle=1 if Wealth_Ind==5
replace HighestQuintle=0 if Wealth_Ind==1
replace HighestQuintle=0 if Wealth_Ind==2
replace HighestQuintle=0 if Wealth_Ind==3
replace HighestQuintle=0 if Wealth_Ind==4
tab HighestQuintle

//Mother's employment status//
tab v714
egen Mother_EMP=group( v714)
tab Mother_EMP
gen Mother_Employment=.
replace Mother_Employment=1 if Mother_EMP==3
replace Mother_Employment=0 if Mother_EMP==2
replace Mother_Employment=. if Mother_EMP==9
replace Mother_Employment=. if Mother_EMP==.
tab Mother_Employment
label var Mother_Employment "Mother's employment status"

//Religion//
tab v130
egen religion=group(v130)
tab religion

*Christainity*
tab v130
gen Christain=.
replace Christain=1 if religion==1
replace Christain=1 if religion==4
replace Christain=1 if religion==5
replace Christain=0 if religion==2
replace Christain=0 if religion==3
replace Christain=0 if religion==6
tab Christain

*Islam*
tab v130
gen Islam=.
replace Islam=1 if religion==2
replace Islam=0 if religion==1
replace Islam=0 if religion==3
replace Islam=0 if religion==4
replace Islam=0 if religion==5
replace Islam=0 if religion==6
tab Islam

*Other*
tab v130
gen Other=.
replace Other=1 if religion==3
replace Other=1 if religion==6
replace Other=0 if religion==1
replace Other=0 if religion==2
replace Other=0 if religion==4
replace Other=0 if religion==5
tab Other

label var Islam "Islam religion"
label var Christain "Christainity"
label var Other "Other religion"

//Residence//
tab v102
egen Residence=group( v102)
tab Residence
gen Residence_Area=.
replace Residence_Area=0 if Residence==2
replace Residence_Area=1 if Residence==1
replace Residence_Area=. if Residence==.
tab Residence_Area
label var Residence_Area "Residence Area"

*region
tab v024
encode v024, gen(region)
egen Region=group(v024)
tab Region
gen NC=1 if region==1
gen NE=1 if region==2
gen NW=1 if region==3
gen SE=1 if region==4
gen SS=1 if region==5
gen SW=1 if region==6
recode NC .=0
recode NE .=0
recode NW .=0
recode SE .=0
recode SS .=0
recode SW .=0
egen Regionn=group(Region)
tab Regionn
replace Regionn=1 if Regionn==1
replace Regionn=1 if Regionn==2
replace Regionn=1 if Regionn==3
replace Regionn=0 if Regionn==4
replace Regionn=0 if Regionn==5
replace Regionn=0 if Regionn==6

//Mother's Earnings//
tab v741
egen Earning_type=group( v741)
tab Earning_type
gen Earnings=.
replace Earnings=. if Earning_type==1
replace Earnings=0 if Earning_type==5
replace Earnings=1 if Earning_type==2
replace Earnings=1 if Earning_type==3
replace Earnings=1 if Earning_type==4
replace Earnings=. if Earning_type==.
tab Earnings
label var Earnings "Mother's earnings"

//Multiple Births//
tab b0
egen Multiple= group(b0)
tab Multiple
gen multiple_births=.
replace multiple_births=0 if Multiple==4
replace multiple_births=1 if Multiple<=3
replace multiple_births=. if Multiple==.
label var multiple_births "Multiple Births"

//Child Vaccinated//
tab h10
egen Vaccination= group( h10)
tab Vaccination
gen Vaccinated=.
replace Vaccinated=1 if Vaccination==5
replace Vaccinated=0 if Vaccination==4
replace Vaccinated=. if Vaccination==1
replace Vaccinated=. if Vaccination==2
replace Vaccinated=. if Vaccination==3
replace Vaccinated=. if Vaccination==.
label var Vaccinated "Is child vaccinated"
tab Vaccinated

//Hospital Visit//
tab v394
egen Visit_12_months= group(v394)
tab Visit_12_months
gen Hospital_visit=.
replace Hospital_visit=1 if Visit_12_months==3
replace Hospital_visit=0 if Visit_12_months==2
replace Hospital_visit=. if Visit_12_months==1
replace Hospital_visit=. if Visit_12_months==.
tab Hospital_visit
label var Hospital_visit "Visit to the hospital in the last 12 months"

*Generating for state
tab sstate
encode sstate, gen(state)

*============================================================================
*********************Survey Dummy*
*==============================================
tab Years
gen SurveyDummy=.
replace SurveyDummy=0 if Years==2003
replace SurveyDummy=0 if Years==2008
replace SurveyDummy=1 if Years==2013
replace SurveyDummy=1 if Years==2018
tab SurveyDummy

*============================================================================
* Generating data for birthx
*===============================================================================
tab Birth_Date
gen birthx=.
replace birthx=1 if Birth_Date==2010
replace birthx=1 if Birth_Date==2011
replace birthx=1 if Birth_Date==2012
replace birthx=1 if Birth_Date==2013
replace birthx=1 if Birth_Date==2014
replace birthx=1 if Birth_Date==2015
replace birthx=1 if Birth_Date==2016
replace birthx=1 if Birth_Date==2017
replace birthx=1 if Birth_Date==2018
replace birthx=0 if Birth_Date==2008
replace birthx=0 if Birth_Date==2007
replace birthx=0 if Birth_Date==2006
replace birthx=0 if Birth_Date==2005
replace birthx=0 if Birth_Date==2004
replace birthx=0 if Birth_Date==2003
replace birthx=0 if Birth_Date==2002
replace birthx=0 if Birth_Date==2001
replace birthx=0 if Birth_Date==2000
replace birthx=0 if Birth_Date==1999
replace birthx=0 if Birth_Date==1998
tab birthx 
rename birthx post2009
tab post2009

*==================================================
****** Generation of interraction terms ***********
*==================================================
gen inter5km_post2009=inter5km*post2009
gen inter15km_post2009=inter15km*post2009
gen inter25km_post2009=inter25km*post2009
gen inter45km_post2009=inter45km*post2009
gen inter51km_post2009=inter51km*post2009

tab inter5km*post2009
tab inter15km_post2009
tab inter25km*post2009
tab inter45km*post2009
tab inter51km*post2009

gen inter5km_post2009_rural=inter5km*post2009*Residence_Area
gen inter15km_post2009_rural=inter15km*post2009*Residence_Area
gen inter25km_post2009_rural=inter25km*post2009*Residence_Area
gen inter45km_post2009_rural=inter45km*post2009*Residence_Area
gen inter51km_post2009_rural=inter51km*post2009*Residence_Area

gen inter5km_post2009_islam=inter5km*post2009*Islam
gen inter15km_post2009_islam=inter15km*post2009*Islam
gen inter25km_post2009_islam=inter25km*post2009*Islam
gen inter45km_post2009_islam=inter45km*post2009*Islam
gen inter51km_post2009_islam=inter51km*post2009*Islam

gen inter5km_post2009_christain=inter5km*post2009*Christain
gen inter15km_post2009_christain=inter15km*post2009*Christain
gen inter25km_post2009_christain=inter25km*post2009*Christain
gen inter45km_post2009_christain=inter45km*post2009*Christain
gen inter51km_post2009_christain=inter51km*post2009*Christain

*=========================================================
***Square of Some variables*******************
*=========================================================
gen childage_square=childagemonth^2
gen birthorder_Squared=birth_order^2
gen No_household_members_squared=No_household_members^2
gen no_childrenU5_squared=no_childrenU5^2
gen motherage_squared=motherage^2


*============================================================================
********************* Herdsmen and Insurgency Conflict Active Areas
***********==================================================================
tab iha 
gen InsurgencyActiveArea=.
replace InsurgencyActiveArea=1 if iha ==1
replace InsurgencyActiveArea=0 if iha ==0
gen InsurgencyActiveArea_post2009=InsurgencyActiveArea*post2009

tab hfha
gen HerdsFarmersActiveArea=.
replace HerdsFarmersActiveArea=1 if hfha==1
replace HerdsFarmersActiveArea=0 if hfha==0
gen HerdsFarmersActiveArea_post2009= HerdsFarmersActiveArea*post2009

tab insurgencyconflictintensity
gen Insurgency_conflict_intensity=.
replace Insurgency_conflict_intensity=1 if insurgencyconflictintensity==1
replace Insurgency_conflict_intensity=0 if insurgencyconflictintensity==0
gen conflict_intensityIns_post2009=Insurgency_conflict_intensity*post2009

tab herdsmenconflictintensity
gen herdsmen_conflict_intensity=.
replace herdsmen_conflict_intensity=1 if insurgencyconflictintensity==1
replace herdsmen_conflict_intensity=0 if insurgencyconflictintensity==0
gen conflict_intensityHerds_post2009=herdsmen_conflict_intensity*post2009

*================================================================
***************MMEAN DIFFERENCES TEST****************************
***==============================================================
********t-test for differences in characteristics****************


moderatestunting moderateunderweight moderatewasting waz06 whz06 haz06

stddiff infantDeaths, by(inter15km*post2009)
tddiff stuntin, by(inter15km*post2009) 
stddiff wastin, by(inter15km*post2009) 
stddiff underweight, by(inter15km*post2009)

stddiff stuntin, by(inter15km*post2009) 
stddiff wastin, by(inter15km*post2009) 
stddiff underweight, by(inter15km*post2009)
stddiff Hospital_visit, by(inter15km*post2009)
stddiff Vaccinated, by(inter15km*post2009)
stddiff Christain, by(inter15km*post2009)
stddiff Islam, by(inter15km*post2009)
stddiff Other, by(inter15km*post2009)
stddiff Residence_Area, by(inter15km*post2009)
stddiff HighestQuintle, by(inter15km*post2009)
stddiff fourthQuintle, by(inter15km*post2009)
stddiff middleQuintle, by(inter15km*post2009)
stddiff secondQuintle, by(inter15km*post2009)
stddiff Mother_Employment, by(inter15km*post2009)
stddiff Earnings, by(inter15km*post2009)
stddiff No_household_members, by(inter15km*post2009)
stddiff Motherage, by(inter15km*post2009)
stddiff Education, by(inter15km*post2009)
stddiff birth_order, by(inter15km*post2009)
stddiff multiple_births, by(inter15km*post2009)
stddiff child_age, by(inter15km*post2009)
stddiff Child_Gender, by(inter15km*post2009)


stddiff infantDeaths, by(HerdsFarmersActiveArea_post2009)
stddiff stuntin, by(HerdsFarmersActiveArea_post2009)
stddiff wastin, by(HerdsFarmersActiveArea_post2009) 
stddiff underweight, by(HerdsFarmersActiveArea_post2009)
stddiff Hospital_visit, by(HerdsFarmersActiveArea_post2009)
stddiff Vaccinated, by(HerdsFarmersActiveArea_post2009)
stddiff Christain, by(HerdsFarmersActiveArea_post2009)
stddiff Islam, by(HerdsFarmersActiveArea_post2009)
stddiff Other, by(HerdsFarmersActiveArea_post2009)
stddiff Residence_Area, by(HerdsFarmersActiveArea_post2009)
stddiff HighestQuintle, by(HerdsFarmersActiveArea_post2009)
stddiff fourthQuintle, by(HerdsFarmersActiveArea_post2009)
stddiff middleQuintle, by(HerdsFarmersActiveArea_post2009)
stddiff secondQuintle, by(HerdsFarmersActiveArea_post2009)
stddiff Mother_Employment, by(HerdsFarmersActiveArea_post2009)
stddiff Earnings, by(HerdsFarmersActiveArea_post2009)
stddiff No_household_members, by(HerdsFarmersActiveArea_post2009)
stddiff Motherage, by(HerdsFarmersActiveArea_post2009)
stddiff Education, by(HerdsFarmersActiveArea_post2009)
stddiff birth_order, by(HerdsFarmersActiveArea_post2009)
stddiff multiple_births, by(HerdsFarmersActiveArea_post2009)
stddiff child_age, by(HerdsFarmersActiveArea_post2009)
stddiff Child_Gender, by(HerdsFarmersActiveArea_post2009)


stddiff infantDeaths, by(InsurgencyActiveArea_post2009)
stddiff stuntin, by(InsurgencyActiveArea_post2009)
stddiff wastin, by(InsurgencyActiveArea_post2009) 
stddiff underweight, by(InsurgencyActiveArea_post2009)
stddiff Hospital_visit, by(InsurgencyActiveArea_post2009)
stddiff Vaccinated, by(InsurgencyActiveArea_post2009)
stddiff Christain, by(InsurgencyActiveArea_post2009)
stddiff Islam, by(InsurgencyActiveArea_post2009)
stddiff Other, by(InsurgencyActiveArea_post2009)
stddiff Residence_Area, by(InsurgencyActiveArea_post2009)
stddiff HighestQuintle, by(InsurgencyActiveArea_post2009)
stddiff fourthQuintle, by(InsurgencyActiveArea_post2009)
stddiff middleQuintle, by(InsurgencyActiveArea_post2009)
stddiff secondQuintle, by(InsurgencyActiveArea_post2009)
stddiff Mother_Employment, by(InsurgencyActiveArea_post2009)
stddiff Earnings, by(InsurgencyActiveArea_post2009)
stddiff No_household_members, by(InsurgencyActiveArea_post2009)
stddiff Motherage, by(InsurgencyActiveArea_post2009)
stddiff Education, by(InsurgencyActiveArea_post2009)
stddiff birth_order, by(InsurgencyActiveArea_post2009)
stddiff multiple_births, by(InsurgencyActiveArea_post2009)
stddiff child_age, by(InsurgencyActiveArea_post2009)
stddiff Child_Gender, by(InsurgencyActiveArea_post2009)

******** T-test for differences in characteristics***************************
ttest infantDeaths, by(inter15km*post2009)
ttest stuntin, by(inter15km*post2009)
ttest wastin, by(inter15km*post2009) 
ttest underweight, by(inter15km*post2009)
ttest Hospital_visit, by(inter15km*post2009)
ttest Vaccinated, by(inter15km*post2009)
ttest Christain, by(inter15km*post2009)
ttest  Islam, by(inter15km*post2009)
ttest Residence_Area, by(inter15km*post2009)
ttest HighestQuintle, by(inter15km*post2009)
ttest fourthQuintle, by(inter15km*post2009)
ttest middleQuintle, by(inter15km*post2009)
ttest secondQuintle, by(inter15km*post2009)

*============================================================================================================================================================================================
**** Estimation of impact of conflict on child health in Nigeria using Difference in Difference approach (AGGREGATE DATA; USING BUFFER ZONES OF 15KM AND 45KM)
*============================================================================================================================================================================================
*******************15km Buffer Zone of Conflict Areas+==========================================================
*===============================================================================================================
clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\lastest_data.dta" 
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Aggregate_45km"

reg infantDeaths inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper.doc, replace title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))

*reg moderatestunting inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(stunting) addstat(F-test, e(F), Prob > F, e(p))

*reg moderateunderweight inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Underweight) addstat(F-test, e(F), Prob > F, e(p))

*reg  moderatewasting inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Wasting) addstat(F-test, e(F), Prob > F, e(p))

*===============================================================================================================
*******************45km Buffer Zone of Conflict Areas+==========================================================
*===============================================================================================================
reg infantDeaths inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper1.doc, replace title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(infant Mortality) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Height-for-Age Z-score) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Weight-for-Age Z-score) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Weight-for-Height Z-secore) addstat(F-test, e(F), Prob > F, e(p))

*reg moderatestunting inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(stunting) addstat(F-test, e(F), Prob > F, e(p))

*reg moderateunderweight inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Underweight) addstat(F-test, e(F), Prob > F, e(p))

*reg  moderatewasting inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Wasting) addstat(F-test, e(F), Prob > F, e(p))

******===========================================================================================================
************************ Effect of conflict on Mechanisms *******************************************************
*****============================================================================================================
*******************15km Buffer Zone of Conflict Areas+===========================================================
*===============================================================================================================
reg Vaccinated inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, replace title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of  Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****============================================================================================================
*******************45km Buffer Zone of Conflict Areas+===========================================================
*===============================================================================================================
reg Vaccinated inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, replace title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****************************************************************************************************************************************************************************
****************************************************************************************************************************************************************************

*=========================================================================================
*************************ROBUSTNESS CHECKS***********************************************
*=========================================================================================
***********Predictive Margin********************************************
*========================================================================
*****************15KM BUFFER ZONES*=================================
*========================================================================
reg infantDeaths inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state

margins, dydx(inter15km post2009 inter15km*post2009)

reg haz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins, dydx(inter15km post2009 inter15km*post2009)

reg waz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins, dydx(inter15km post2009 inter15km*post2009)

reg whz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins, dydx(inter15km post2009 inter15km*post2009)

reg moderatestunting inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins, dydx(inter15km post2009 inter15km*post2009)

reg moderateunderweight inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins, dydx(inter15km post2009 inter15km*post2009)

reg  moderatewasting inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins, dydx(inter15km post2009 inter15km*post2009)

*========================================================================
*****************45KM BUFFER ZONES*=================================+====
*========================================================================
reg infantDeaths inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state

margins i.inter45km i.post2009 i.inter45km_post2009 i.Child_Gender i.childagemonth i.no_childrenU5 i.birth_order i.Christain i.Islam i.Residence_Area i.multiple_births i.motherheight i.maritalstatus i.No_household_members i.motherage i.Poor i.Education i.Mother_Employment i.Earnings i.Hospital_visit i.Birth_Date i.state

margins, dydx(inter45km post2009 inter45km*post2009)


reg haz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins i.inter45km i.post2009 i.inter45km_post2009 i.Child_Gender i.childagemonth i.no_childrenU5 i.birth_order i.Christain i.Islam i.Residence_Area i.multiple_births i.motherheight i.maritalstatus i.No_household_members i.motherage i.Poor i.Birth_Date i.state

margins, dydx(inter45km post2009 inter45km*post2009)

reg waz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins i.inter45km i.post2009 i.inter45km_post2009 i.Child_Gender i.childagemonth i.no_childrenU5 i.birth_order i.Christain i.Islam i.Residence_Area i.multiple_births i.motherheight i.maritalstatus i.No_household_members i.motherage i.Poor i.Birth_Date i.state

margins, dydx(inter45km post2009 inter45km*post2009)

reg whz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins i.inter45km i.post2009 i.inter45km_post2009 i.Child_Gender i.childagemonth i.no_childrenU5 i.birth_order i.Christain i.Islam i.Residence_Area i.multiple_births i.motherheight i.maritalstatus i.No_household_members i.motherage i.Poor i.Birth_Date i.state

margins, dydx(inter45km post2009 inter45km*post2009)

reg moderatestunting inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins i.inter45km i.post2009 i.inter45km_post2009 i.Child_Gender i.childagemonth i.no_childrenU5 i.birth_order i.Christain i.Islam i.Residence_Area i.multiple_births i.motherheight i.maritalstatus i.No_household_members i.motherage i.Poor i.Birth_Date i.state

margins, dydx(inter45km post2009 inter45km*post2009)

reg moderateunderweight inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins i.inter45km i.post2009 i.inter45km_post2009 i.Child_Gender i.childagemonth i.no_childrenU5 i.birth_order i.Christain i.Islam i.Residence_Area i.multiple_births i.motherheight i.maritalstatus i.No_household_members i.motherage i.Poor i.Birth_Date i.state

margins, dydx(inter45km post2009 inter45km*post2009)

reg  moderatewasting inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state

margins i.inter45km i.post2009 i.inter45km_post2009 i.Child_Gender i.childagemonth i.no_childrenU5 i.birth_order i.Christain i.Islam i.Residence_Area i.multiple_births i.motherheight i.maritalstatus i.No_household_members i.motherage i.Poor i.Birth_Date i.state

margins, dydx(inter45km post2009 inter45km*post2009)

***************************************************************************************************************************************************************************
****************************************************************************************************************************************************************************

*============================================================================================================================================================================================
**** Estimation of impact of conflict on child health in Nigeria using Difference in Difference approach (DISAGGREGATED CONFLICT DATA; USING INSURGENCY AND HERDSMEN/FARMERS CONFLICTS)
*============================================================================================================================================================================================
*******************INSURGENCY CONFLICT TYPE (Insurgency Active Area)=========================================================
*============================================================================================================================
clear
use "C:\Users\LOLA\Dropbox\AERC\AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\LOLA\Dropbox\AERC\AERC\Insurgency"

*reg infantDeaths InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Years region Mother_Employment monthofinterview Hospital_visit i.Birth_Date i.state migration Islam Christain

reg infantDeaths InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state i.region i.ethnicity
outreg2 using conflictpaper6.doc, replace title("Table 3:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state i.region i.ethnicity
outreg2 using conflictpaper6.doc, append title("Table 3: Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state i.region i.ethnicity
outreg2 using conflictpaper6.doc, append title("Table 3:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state i.region i.ethnicity
outreg2 using conflictpaper6.doc, append title("Table 3:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))

*reg moderatestunting InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper6.doc, append title("Table 3: Impact of Insurgency on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(stunting) addstat(F-test, e(F), Prob > F, e(p))

*reg moderateunderweight InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper6.doc, append title("Table 3:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Underweight) addstat(F-test, e(F), Prob > F, e(p))

*reg  moderatewasting InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper6.doc, append title("Table 3:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Wasting) addstat(F-test, e(F), Prob > F, e(p))

*===============================================================================================================
*******************INSURGENCY CONFLICT TYPE (Insurgency Conflict Severity) =====================================
*===============================================================================================================
reg infantDeaths Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage partnerage HH_Gender partnereducation Poor i.Birth_Date i.state
outreg2 using conflictpaper7.doc, replace title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))

*reg moderatestunting Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(stunting) addstat(F-test, e(F), Prob > F, e(p))

*reg moderateunderweight Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Underweight) addstat(F-test, e(F), Prob > F, e(p))

*reg  moderatewasting Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Wasting) addstat(F-test, e(F), Prob > F, e(p))

******===========================================================================================================
************************ Effect of conflict on Mechanisms *******************************************************
*****============================================================================================================
*******************INSURGENCY CONFLICT TYPE (Insurgency Active Area)+===========================================================
*===============================================================================================================
reg Vaccinated InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state i.region 
outreg2 using conflictpaper8.doc, replace title("Table 5: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state i.region 
outreg2 using conflictpaper8.doc, append title("Table 5: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospittal Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state i.region 
outreg2 using conflictpaper8.doc, append title("Table 5: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state i.region 
outreg2 using conflictpaper8.doc, append title("Table 5: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members Poor motherage i.Birth_Date i.state i.region 
outreg2 using conflictpaper8.doc, append title("Table 5: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration InsurgencyActiveArea post2009 InsurgencyActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members  motherage Poor i.Birth_Date i.state i.region 
outreg2 using conflictpaper8.doc, append title("Table 5: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****============================================================================================================
*******************INSURGENCY CONFLICT TYPE (Insurgency Conflict Severity)===========================================================
*===============================================================================================================
reg Vaccinated Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, replace title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state 
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visits) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration Insurgencyconflictintensity post2009 conflict_intensityIns_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****************************************************************************************************************************************************************************
****************************************************************************************************************************************************************************
*============================================================================================================================================================================================
*******************HERDSMEN/FARMERS CONFLICT TYPE (Herdsmen/Farmers Active Area)=========================================================
*============================================================================================================================
clear
use "C:\Users\LOLA\Dropbox\AERC\AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\LOLA\Dropbox\AERC\AERC\Herder"

*reg infantDeaths HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor region Education Mother_Employment ethnicity monthofinterview nHHage HH_Gender Hospital_visit migration Islam Christain i.Birth_Date i.state

reg infantDeaths HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state i.region i.ethnicity
outreg2 using conflictpaper10.doc, replace title("Table 7:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state i.region i.ethnicity
outreg2 using conflictpaper10.doc, append title("Table 7: Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state i.region i.ethnicity
outreg2 using conflictpaper10.doc, append title("Table 7:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state i.region i.ethnicity
outreg2 using conflictpaper10.doc, append title("Table 7:Impact of Herdsmen/Farmers Conflict Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))

*reg moderatestunting HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper10.doc, append title("Table 7:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(stunting) addstat(F-test, e(F), Prob > F, e(p))

*reg moderateunderweight HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper10.doc, append title("Table 7:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Underweight) addstat(F-test, e(F), Prob > F, e(p))

*reg  moderatewasting HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper10.doc, append title("Table 7:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Wasting) addstat(F-test, e(F), Prob > F, e(p))

*===============================================================================================================
*******************HERDSMEN/FARMERS CONFLICT TYPE (Herdsmen/Farmers Conflict Severity) =====================================
*===============================================================================================================
reg infantDeaths HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state 
outreg2 using conflictpaper11.doc, replace title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage Mother_Employment Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage Mother_Employment Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage Mother_Employment Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))

*reg moderatestunting HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(stunting) addstat(F-test, e(F), Prob > F, e(p))

*reg moderateunderweight HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
*outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
**/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
**/ ctitle(Underweight) addstat(F-test, e(F), Prob > F, e(p))

*reg  moderatewasting HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Wasting) addstat(F-test, e(F), Prob > F, e(p))

******===========================================================================================================
************************ Effect of conflict on Mechanisms *******************************************************
*****============================================================================================================
*******************HERDSMEN/FARMERS CONFLICT TYPE (Herdsmen/Farmers Active Area)+===========================================================
*===============================================================================================================
reg Vaccinated HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state 
outreg2 using conflictpaper12.doc, replace title("Table 9: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper12.doc, append title("Table 9: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper12.doc, append title("Table 9: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper12.doc, append title("Table 9: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper12.doc, append title("Table 9: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration HerdsFarmersActiveArea post2009 HerdsFarmersActiveArea_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper12.doc, append title("Table 9: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****============================================================================================================
*******************HERDSMEN/FARMERS CONFLICT TYPE (Herdsmen/Farmers Conflict Severity)==========================
*===============================================================================================================
reg Vaccinated HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, replace title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visits) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration HerdsmenConflictintensity post2009 conflict_intensityHerds_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****************************************************************************************************************************************************************************
****************************************************************************************************************************************************************************



*       DICHOTOMISED ANALYSIS
**** North Anlaysis
*******************15km Buffer Zone of Conflict Areas+==========================================================
*===============================================================================================================
clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\North Analysis"
drop if Regionn == 0


reg infantDeaths inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper.doc, replace title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))

*===============================================================================================================
*******************45km Buffer Zone of Conflict Areas+==========================================================
*===============================================================================================================
reg infantDeaths inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper1.doc, replace title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))

******===========================================================================================================
************************ Effect of conflict on Mechanisms *******************************************************
*****============================================================================================================
*******************15km Buffer Zone of Conflict Areas+===========================================================
*===============================================================================================================
reg Vaccinated inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, replace title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of  Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****============================================================================================================
*******************45km Buffer Zone of Conflict Areas+===========================================================
*===============================================================================================================
reg Vaccinated inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, replace title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****************************************************************************************************************************************************************************
****************************************************************************************************************************************************************************


*******South Analysis
*******************15km Buffer Zone of Conflict Areas+==========================================================
*===============================================================================================================
clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\South Analysis"
drop if Regionn == 1

reg infantDeaths inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper.doc, replace title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))


*===============================================================================================================
*******************45km Buffer Zone of Conflict Areas+==========================================================
*===============================================================================================================
reg infantDeaths inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper1.doc, replace title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))



******===========================================================================================================
************************ Effect of conflict on Mechanisms *******************************************************
*****============================================================================================================
*******************15km Buffer Zone of Conflict Areas+===========================================================
*===============================================================================================================
reg Vaccinated inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, replace title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of  Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****============================================================================================================
*******************45km Buffer Zone of Conflict Areas+===========================================================
*===============================================================================================================
reg Vaccinated inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, replace title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****************************************************************************************************************************************************************************
****************************************************************************************************************************************************************************



*******Regional analysis******************************
clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\NC"
drop if NC == 0

clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\NE"
drop if NE == 0

clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\NW"
drop if NW == 0

clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\SE"
drop if SE == 0

clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\SW"
drop if SS == 0

clear
use "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Lastest Conflict Data_Without dropping.dta"
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\SS"
drop if SW == 0

*******************15km Buffer Zone of Conflict Areas+==========================================================
*===============================================================================================================
reg infantDeaths inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper.doc, replace title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))


*===============================================================================================================
*******************45km Buffer Zone of Conflict Areas+==========================================================
*===============================================================================================================
reg infantDeaths inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor Education Mother_Employment Earnings Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper1.doc, replace title("Table 1: Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper1.doc, append title("Table 1:Impact of Aggregate Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))



******===========================================================================================================
************************ Effect of conflict on Mechanisms *******************************************************
*****============================================================================================================
*******************15km Buffer Zone of Conflict Areas+===========================================================
*===============================================================================================================
reg Vaccinated inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, replace title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of  Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration inter15km post2009 inter15km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper2.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****============================================================================================================
*******************45km Buffer Zone of Conflict Areas+===========================================================
*===============================================================================================================
reg Vaccinated inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, replace title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visit) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration inter45km post2009 inter45km_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper3.doc, append title("Table 2: Impact of Aggregate Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****************************************************************************************************************************************************************************
****************************************************************************************************************************************************************************


***CONFLICT INTENSITY 
cd "C:\Users\USER\Dropbox\work files_dropbox_latest\conflict and health outcomes\Final revision_AERC\Intensity"
*===============================================================================================================
*******************INSURGENCY CONFLICT TYPE (Insurgency Conflict Severity) =====================================
*===============================================================================================================
reg infantDeaths insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage partnerage HH_Gender partnereducation Poor i.Birth_Date i.state
outreg2 using conflictpaper7.doc, replace title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper7.doc, append title("Table 4:Impact of Insurgency on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))

*****============================================================================================================
*******************INSURGENCY CONFLICT TYPE (Insurgency Conflict Severity)===========================================================
*===============================================================================================================
reg Vaccinated insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, replace title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state 
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visits) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext(State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration insurgencyintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor i.Birth_Date i.state
outreg2 using conflictpaper9.doc, append title("Table 6: Impact of Insurgency on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*===============================================================================================================
*******************HERDSMEN/FARMERS CONFLICT TYPE (Herdsmen/Farmers Conflict Severity) =====================================
*===============================================================================================================
reg infantDeaths herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage migration Mother_Employment Hospital_visit i.Birth_Date i.state 
outreg2 using conflictpaper11.doc, replace title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Infant Deaths) addstat(F-test, e(F), Prob > F, e(p))

reg haz06 herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage Mother_Employment Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(HAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg waz06 herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage Mother_Employment Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WAZ06) addstat(F-test, e(F), Prob > F, e(p))

reg whz06 herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnereducation partnerage Mother_Employment Hospital_visit i.Birth_Date i.state
outreg2 using conflictpaper11.doc, append title("Table 8:Impact of Herdsmen/Farmers Conflict on Child Health Outcomes in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(WHZ06) addstat(F-test, e(F), Prob > F, e(p))


*****============================================================================================================
*******************HERDSMEN/FARMERS CONFLICT TYPE (Herdsmen/Farmers Conflict Severity)==========================
*===============================================================================================================
reg Vaccinated herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, replace title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Vaccinated) addstat(F-test, e(F), Prob > F, e(p))

reg Hospital_visit herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Hospital Visits) addstat(F-test, e(F), Prob > F, e(p))

reg Earnings herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Earnings) addstat(F-test, e(F), Prob > F, e(p))

reg Mother_Employment herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Employment) addstat(F-test, e(F), Prob > F, e(p))

reg Education herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Education) addstat(F-test, e(F), Prob > F, e(p))

reg migration herdsmenintense post2009 insurgencyintense_post2009 Child_Gender childagemonth no_childrenU5 birth_order Christain Islam Residence_Area multiple_births motherheight maritalstatus No_household_members motherage Poor partnerage i.Birth_Date i.state
outreg2 using conflictpaper13.doc, append title("Table 10: Impact of Herdsmen/Farmers Conflict on Child Health Mechanisms in Nigeria") /*
*/ addtext( State fixed effects, YES,  Year of Birth fixed effects, YES) /*
*/ ctitle(Migration) addstat(F-test, e(F), Prob > F, e(p))

*****************************************************************************************************************************************************************************
****************************************************************************************************************************************************************************
