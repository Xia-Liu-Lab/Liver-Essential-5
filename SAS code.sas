/* loading data*/
%let home = D:\NHANES;
libname LF "&home\Lifestyle";

data MAFLD;
  set LF.nhanes0309;
run;

/*table one*/
/*For continuous variables: age (for example)*/
proc surveymeans data=MAFLD; missing min max median mean clm;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
	 where total_pattern>=0 and MAFLD >=0;
	 var age;
 run;

 proc surveymeans data=MAFLD; missing min max median mean clm;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
	 where total_pattern>=0 and MAFLD >=0 and sleep_group=0;
	 var age;
 run;

  proc surveymeans data=MAFLD; missing min max median mean clm;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
	 where total_pattern>=0 and MAFLD >=0 and sleep_group=1;
	 var age;
 run;

  proc surveymeans data=MAFLD; missing min max median mean clm;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
	 where total_pattern>=0 and MAFLD >=0 and sleep_group=2;
	 var age;
 run;

  PROC surveyreg data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
	 class sleep_group;
	 where total_pattern>=0 and MAFLD >=0;
	 Model age=sleep_group/solution clparm vadjust=none anova;
	 run;
/*For categorical variables: MAFLD (for example)*/
PROC surveyfreq data=MAFLD;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
    where total_pattern>=0 and MAFLD >=0;
     title "chi qaure";
     tables sleep_group*MAFLD/row chisq chisq1;/* rao-scott chi-square*/
run;

/* taking MAFLD as example*/
/*unadjust*/
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 where sleep_group>=0;
     model MAFLD (desc) =age gender race education_2 marriage income_level lifepattern/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 class lifepattern (PARAM=REF REF="2");
	 where sleep_group >=0;
     model MAFLD (desc) =age gender race education_2 marriage income_level lifepattern/clparm
vadjust=none;
run;
/*adjust for sleep group*/
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
     model MAFLD (desc) =age gender race education_2 marriage income_level lifepattern sleep_group/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 class lifepattern (PARAM=REF REF="2");
     model MAFLD (desc) =age gender race education_2 marriage income_level lifepattern sleep_group/clparm
vadjust=none;
run;
/*interaction*/
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
     model MAFLD (desc) =age gender race education_2 marriage income_level sleep_group|lifepattern/clparm
vadjust=none;
run;
/*association of lifestyle according to sleep quality*/
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 where sleep_group=0;
     model MAFLD (desc) =age gender race education_2 marriage income_level lifescore/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 where sleep_group=1;
     model MAFLD (desc) =age gender race education_2 marriage income_level lifescore/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 where sleep_group=2;
     model MAFLD (desc) =age gender race education_2 marriage income_level lifescore/clparm
vadjust=none;
run;
/*joint effect*/
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 class life_sleep(PAREM=REF REF="0");
     model MAFLD (desc) =age gender race education_2 marriage income_level life_sleep/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
     model MAFLD (desc) =age gender race education_2 marriage income_level life_sleep/clparm
vadjust=none;
run;
/*association of Liver Essential 5*/
/*reclassify*/
PROC surveyfreq data=MAFLD;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     where lifepattern=0 ;
     title "reclassify-poor";
     tables total_pattern*MAFLD/row chisq chisq1;/* rao-scott chi-square*/
run;
PROC surveyfreq data=MAFLD;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     where lifepattern=1;
     title "reclassify-poor";
     tables total_pattern*MAFLD/row chisq chisq1;/* rao-scott chi-square*/
run;
PROC surveyfreq data=MAFLD;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     where lifepattern=2;
     title "reclassify-poor";
     tables total_pattern*MAFLD/row chisq chisq1;/* rao-scott chi-square*/
run;
/*association of Liver Essential 5*/
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
     model MAFLD (desc) =age gender race education_2 marriage income_level total_score/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLD01 nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 class total_score01(PAREM=REF REF="1");
     model MAFLD (desc) =age gender race education_2 marriage income_level total_score01/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
	 class total_pattern(PAREM=REF REF="0");
     model MAFLD (desc) =age gender race education_2 marriage income_level total_pattern/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLD nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
     model MAFLD (desc) =age gender race education_2 marriage income_level total_pattern/clparm
vadjust=none;
run;

/*subgroup analysis*/
data MAFLDsub;
  set LF.nhanes240226;
run;
/*age group for example*/
PROC surveyfreq data=MAFLDsub;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
	 where MAFLD>=0 and total_score >=0;
     tables age_group*MAFLD/row chisq chisq1;/* rao-scott chi-square*/
run;
PROC surveylogistic data=MAFLDsub nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
     title "logistic regresson of MAFLD";
     model MAFLD (desc) =gender race education_2 marriage income_level total_score age_group total_score*age_group/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLDsub nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
	 where age_group =0;
     title "logistic regresson of MAFLD";
     model MAFLD (desc) =gender race education_2 marriage income_level total_score/clparm
vadjust=none;
run;
PROC surveylogistic data=MAFLDsub nomcar;
     stratum sdmvstra;
     cluster sdmvpsu;
     weight WTDR2D;
	 where age_group =1;
     title "logistic regresson of MAFLD";
     model MAFLD (desc) =gender race education_2 marriage income_level total_score/clparm
vadjust=none;
run;
