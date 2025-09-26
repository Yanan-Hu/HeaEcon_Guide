/*-------1---------2---------3---------4---------5---------6---------7--
 Module Name     : GREGWT.SAS (macro)                                 
 System          : Household Surveys Facilities (HSF)                   
 Subsystem       :                                                      
 Author          : Philip Adrian Bell                                   
 Date written    : August 1999                                           
 Language used   : SAS                                                  
 Inputs          : SAS dataset of unit data to be weighted
                   plus one or more SAS dataset of benchmark info               
 Outputs         : SAS dataset of unit data with weights
                   three SAS datasets of reports 
                   optionally a SAS dataset of estimates & variances              
 Parameters      :                                                                                      
 -----------------------------------------------------------------------
 UNITDSN = Name of dataset to be weighted
 OUTDSN  = Name for output weighted unit dataset
            Default _outdsn_
 BENOUT  = Prefix for report datasets on benchmark convergence.  First 
            six characters only are used, followed by an integer suffix.
            There will be one such dataset for each benchmark dataset
            with suffixes 1,...   
            Default value is benout (i.e dataset names benout1,... ) 
 BYOUT   = Name for report dataset on BY group convergence
            Default _byout_
 EXTOUT  = Name for report dataset of extreme units
            Default _extout_
 
 BY      = as for BY statement - optional
 STRATUM = Stratum - optional
 VARGRP  = Variance group - optional
            To get weighted residual variances,
            STRATUM and VARGRP must be specified
            and data must be sorted by BY STRATUM VARGRP 
 GROUP   = Level for weighting of groups rather than units.
            Optional
            In integrated weighting, specifies the grouping level
            at which the calibration is applied.  
            The input weight for a group is that of the first unit 
            in the group, unless OPTIONS=amean or OPTIONS=hmean.
            All units in a group will have the same final weight.   
 UNIT    = Other variables in the sort order below GROUP level.  
            Optional.  Used in integrated weighting: the list
              BY STRATUM VARGRP GROUP UNIT
            must contain any grouping variable used in the 
            BnGRP macro variables.  For the use of these 
            macro variables see description below.
 
 ** Unit data MUST be sorted by &BY &STRATUM &VARGRP &GROUP &UNIT 
 **  (although any of these can be left blank)
 
 INWEIGHT= Input weight (compulsory)
 or  INWT=
 REPID   = Gives the replicate identifier, a variable giving
            numbers 1 to m identifying the replicate a unit is in
            Use instead of setting up replicate input weights
            Optional
 INREPWTS= Lists 2 or more replicate input weights - if these are
            provided, REPID is ignored.  Optional
 
 WEIGHT  = Name for output weight, default is &INWEIGHT
 or    WT=
 REPWTS  = Name for output replicate weights, default &INREPWTS
 
 ID      = Output dataset will include variables used by the 
            macro plus any additional variables named in ID.
            To get all variables specify ID=_ALL_
 PENALTY = Variable used to specify a weighted distance function
            - the distance contributed by this unit or group
            is multiplied by the PENALTY value for the unit,
            or the PENALTY value of the first unit in the group
            A high PENALTY value makes the weight less subject
            to change than that of other units  
            Default is to use 1 
 
 Specifications for up to 30 datasets of benchmarks (n in 1-30):
 BnDSN   = Name of n-th dataset containing benchmarks
 BnCLASS = Variables defining category for these benchmarks
 BnVAR   = Variables on UNITDSN to be totaled to the benchmarks
            (default is to use weighted counts of units)
 BnTOT   = Variables on BnDSN giving the benchmarks
 BnREPS  = List of replicate benchmark totals
            - blank if this feature not used
            - otherwise lists <number of replicate weights> 
              variables for each benchmark total
              e.g. B1TOT=psntot hhldtot, B1REPS=pt1-pt30 ht1-ht30
 BnRVAR  = List of replicate variables to be totalled to add to
           the replicate benchmarks
            - blank if this feature not used
            - otherwise lists <number of replicate weights>
              variables for each benchmark total              
 BnGRP   = Used in integrated weighting.  Names a variable listed
            in &GROUP &UNITID that gives the grouping level at which 
            this benchmark applies. 
            Only the first record in the group is used in totals
            eg. if BnGRP=hhold then, for this benchmark,
            values from the first unit in the hhold are assumed
            to be hhold records and are used in totals.
 
 LOWER   = Smallest value which weights can take
            or (if followed by a % sign eg LOWER=50%)
            the smallest percentage that weights can be multiplied by
            (but if LOWER > 95% then 95% is used).
           The value may be a SAS expression involving variables
            available to the macro (can use ID to include them)
            eg. LOWER=max(1,0.7*weight)
 UPPER   = Largest value which weights can take
            or (if followed by a % sign eg UPPER=200%)
            the largest percentage that weights can be multiplied by
            (but if UPPER < 105% then 105% is used).
 
 OPTIONS = List of options - possible values are:
            NOPRINT: turn off printing of output reports
            BADPRINT:only print benchmark and BY group data
                     where a benchmark was not met
                     (also defaults EXTNO to 5)            
            NOLOG  : turn off log information about BY groups
            NOTES  : turn on notes 
                     (default is to turn most notes to log off)
            EXP or EXPONENTIAL : Use exponential distance function
                     instead of the default linear distance
            FIRSTWT: report on first weight (i.e. for iteration 1)
                     in addition to final weight
            REPS   : attach replicate estimates to any table produced
                     (use names est_1-est_n or names given by OUTREPS)         
            UNIV   : print distribution of weights and weight
                     changes from PROC UNIVARIATE
            DEBUG  : do not delete intermediate datasets         
            HMEAN  : Input weight for a group is the harmonic
                     mean of unit input weights 
            AMEAN  : Input weight for a group is the 
                     arithmetic mean of unit input weights
              Default input weight for a group (if neither HMEAN 
              or AMEAN are specified) is the input weight from the
              first unit in the group 
 
 MAXITER = Maximum numer of iterations in restricted version
            Default is 10
 EPSILON = Convergence criterion: how closely must benchmarks 
            be met, expressed as the discrepancy of estimate 
            from benchmark divided by benchmark
 
 Specification for an output table of estimates and variances:
 OUT     = Name for output dataset containing table
 CLASS   = Class variables defining categories for which
            estimates will be produced
            For ratio estimates, CLASS defines the categories
            to be used as denominators 
            Estimates for totals across a class variable can be 
            requested by giving the class variables prefixed by a #
            eg. CLASS = state #sex  gives estimates for states
            as well as for state by sex.  
 SUBCLASS= For ratio estimates, SUBCLASS defines the categories
            used as numerators   
 VAR     = List of continuous variables to be estimated for.
            For ratio estimates, these variables are used in the 
            numerator only. 
 DENOM   = For mean or ratio estimates:
            - contains a list of variables used as denominators 
              for the corresponding variables in VAR.  
            - If DENOM lists fewer variables than VAR, the last
              variable in DENOM is used for the extra VAR members.
            - If more variables in DENOM than VAR the macro stops.
            - The keyword _one_ signifies using 1 as the denominator,
              giving estimates of mean                        
 OUTGRP  = Used in integrated weighting.  Names a variable listed
            in &GROUP &UNITID that gives the grouping level at which 
            the table is being produced. 
            Only the first record in the group is used in totals
            eg. if OUTGRP=hhold then values from the first unit in the
            hhold are hhold records and are to be used in totals.
                
 OUTREPS = Replicate estimates will be attached to the table 
             if OUTREPS is given (in which case OUTREPS gives names
             for the variables).
 NPREDICT= Number of predicted values to be attached to unit data
            on &OUTDSN.  Predictions will be named 
            hat_1-hat_&NPREDICT and (for level estimates) 
            will correspond to the first &NPREDICT elements of the
            output tables. 
            hat_n is the prediction under the regression model 
            of the contribution of the unit to cell n of the 
            output table.
            If tables are not specified, NPREDICT has no effect.            
            (Note that for a table of ratio estimates or means
            the predictions of numerator and denominator 
            level estimates are produced )
 WROUT   = Name for file of weighted residuals for tabulated estimates
            Default is to not produce this file    
 WRLIST  = List of variable names to contain the weighted residuals.
            Residuals are output in the same order as the output tables.
            WRLIST is only used if WROUT is specified.
            Default is wr_1 i.e. only output the first residual
 WRWEIGHT= True final weight for use in tabulations and weighted 
            residual calculations.  This is only required when the 
            weighted residual calculations are being re-done using a
            different benchmark specification than that used for the
            original weighting.  This could be done to minimise 
            calculations (eg. reduce to a post-stratified ratio case),
            or because the original weighting scheme is unknown.
 
 TITLELOC= On output prints, the line at which GREGWT titles
            should appear (using a title<n> statment)
            leaving pre-existing titles on previous lines intact
            Default is the first line (i.e. a title statement)
            _NONE_ avoids printing any GREGWT titles
 REPORTID= Unit identifiers to be used on extreme values report
            (default uses as many as possible of the variables listed in
             &BY &B1CLASS... &STRATUM &VARGRP &GROUP &UNIT &B1VAR...)
              
 MAXSPACE= Space available in kilobytes (roughly) for table calcs
            (RAM, not hard drive space). Usually leave at 500 - 
            there is no advantage in specifying more unless 
            it is needed.
            Program requires for each table category (in bytes)
             total length of CLASS and SUBCLASS variables 
               + 8*(number of replicates + 1)
                  *(number of VAR and DENOM variables)
               + some extra    
 REPWTMAX= Maximum number of replicate weights, default 9999 
            - used only with REPID, values over REPWTMAX are  
            considered invalid values of the REPID variable
 REPWTLEN= Length in bytes for replicate weights (default 4, maximum 8)
           Note that the default length of 4 is applied at output - 
           this may lead to slight inconsistencies between replicate
           estimates produced in GREGWT and those obtained in a later 
           tabulation (due to rounding).  The difference should have 
           only a tiny impact on standard er-ror estimates (which are  
           the purpose of producing the replicate weights)            
 EXTNO   = Number of extreme values of each type to be printed
 LINESIZE= Number of characters per line on output file.
           ONLY needed if the version of SAS does not support SYSFUNC
           or to fool the extreme values report into changing the number
           of id variables it prints (it allows 8 characters/variable).  
 RUNID   = Not used in this version
 STEP    = Not used in this version
 -----------------------------------------------------------------------
              
2023   
*--------1---------2---------3---------4---------5---------6---------7*/

PROC IMPORT OUT= orig_data DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\base_data_2023.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="base_data_2023"; 
     GETNAMES=YES;
RUN;

data orig_data; set orig_data;
intwt=1;
run;

PROC IMPORT OUT= bench_AgeIndigenous DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeIndigenous_2023.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeParity DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeParity2023.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeCS DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeCS_2023.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;


%include "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\Gregwt.sas";
run;

%GREGWT
(UNITDSN= orig_data,OUTDSN= weighted_data2023,INWEIGHT= intwt,WEIGHT= newwt,
B1DSN= bench_AgeIndigenous, B1CLASS= ATSI mum_age,B1TOT= cases, 
B2DSN= bench_AgeParity, B2CLASS= Parity mum_age,B2TOT= cases, 
B3DSN= bench_AgeCS, B3CLASS=  Cs mum_age,B3TOT= cases,
id=_all_);

run;

data weighted_data2023; set weighted_data2023;
keep baby_ppn newwt;
run;

proc export 
  data=weighted_data2023 
  dbms=xlsx 
  outfile="G:\SAS Syntax_Papers\Microsim_hospital resources homebirth birthcentre COVID\Weights_2023.xlsx" 
  replace;
run;


/*2024   
*--------1---------2---------3---------4---------5---------6---------7*/

PROC IMPORT OUT= orig_data DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\base_data_2024.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="base_data_2024"; 
     GETNAMES=YES;
RUN;

data orig_data; set orig_data;
intwt=1;
run;

PROC IMPORT OUT= bench_AgeIndigenous DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeIndigenous_2024.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeParity DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeParity2024.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeCS DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeCS_2024.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;


%include "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\Gregwt.sas";
run;

%GREGWT
(UNITDSN= orig_data,OUTDSN= weighted_data2024,INWEIGHT= intwt,WEIGHT= newwt,
B1DSN= bench_AgeIndigenous, B1CLASS= ATSI mum_age,B1TOT= cases, 
B2DSN= bench_AgeParity, B2CLASS= Parity mum_age,B2TOT= cases, 
B3DSN= bench_AgeCS, B3CLASS=  Cs mum_age,B3TOT= cases,
id=_all_);

run;

data weighted_data2024; set weighted_data2024;
keep baby_ppn newwt;
run;

proc export 
  data=weighted_data2024 
  dbms=xlsx 
  outfile="G:\SAS Syntax_Papers\Microsim_hospital resources homebirth birthcentre COVID\Weights_2024.xlsx" 
  replace;
run;



/*2025   
*--------1---------2---------3---------4---------5---------6---------7*/

PROC IMPORT OUT= orig_data DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\base_data_2025.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="base_data_2025"; 
     GETNAMES=YES;
RUN;

data orig_data; set orig_data;
intwt=1;
run;

PROC IMPORT OUT= bench_AgeIndigenous DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeIndigenous_2025.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeParity DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeParity2025.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeCS DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeCS_2025.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;


%include "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\Gregwt.sas";
run;

%GREGWT
(UNITDSN= orig_data,OUTDSN= weighted_data2025,INWEIGHT= intwt,WEIGHT= newwt,
B1DSN= bench_AgeIndigenous, B1CLASS= ATSI mum_age,B1TOT= cases, 
B2DSN= bench_AgeParity, B2CLASS= Parity mum_age,B2TOT= cases, 
B3DSN= bench_AgeCS, B3CLASS=  Cs mum_age,B3TOT= cases,
id=_all_);

run;

data weighted_data2025; set weighted_data2025;
keep baby_ppn newwt;
run;

proc export 
  data=weighted_data2025 
  dbms=xlsx 
  outfile="G:\SAS Syntax_Papers\Microsim_hospital resources homebirth birthcentre COVID\Weights_2025.xlsx" 
  replace;
run;



/*2026   
*--------1---------2---------3---------4---------5---------6---------7*/

PROC IMPORT OUT= orig_data DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\base_data_2026.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="base_data_2026"; 
     GETNAMES=YES;
RUN;

data orig_data; set orig_data;
intwt=1;
run;

PROC IMPORT OUT= bench_AgeIndigenous DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeIndigenous_2026.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeParity DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeParity2026.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeCS DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeCS_2026.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;


%include "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\Gregwt.sas";
run;

%GREGWT
(UNITDSN= orig_data,OUTDSN= weighted_data2026,INWEIGHT= intwt,WEIGHT= newwt,
B1DSN= bench_AgeIndigenous, B1CLASS= ATSI mum_age,B1TOT= cases, 
B2DSN= bench_AgeParity, B2CLASS= Parity mum_age,B2TOT= cases, 
B3DSN= bench_AgeCS, B3CLASS=  Cs mum_age,B3TOT= cases,
id=_all_);

run;

data weighted_data2026; set weighted_data2026;
keep baby_ppn newwt;
run;

proc export 
  data=weighted_data2026 
  dbms=xlsx 
  outfile="G:\SAS Syntax_Papers\Microsim_hospital resources homebirth birthcentre COVID\Weights_2026.xlsx" 
  replace;
run;




/*2027   
*--------1---------2---------3---------4---------5---------6---------7*/

PROC IMPORT OUT= orig_data DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\base_data_2027.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="base_data_2027"; 
     GETNAMES=YES;
RUN;

data orig_data; set orig_data;
intwt=1;
run;

PROC IMPORT OUT= bench_AgeIndigenous DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeIndigenous_2027.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeParity DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeParity2027.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;

PROC IMPORT OUT= bench_AgeCS DATAFILE= "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\AgeCS_2027.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="Sheet1"; 
     GETNAMES=YES;
RUN;


%include "G:\SAS Syntax_Papers\Emily Callander\Microsimulation Model - Standard Model\Weighting Files\Gregwt.sas";
run;

%GREGWT
(UNITDSN= orig_data,OUTDSN= weighted_data2027,INWEIGHT= intwt,WEIGHT= newwt,
B1DSN= bench_AgeIndigenous, B1CLASS= ATSI mum_age,B1TOT= cases, 
B2DSN= bench_AgeParity, B2CLASS= Parity mum_age,B2TOT= cases, 
B3DSN= bench_AgeCS, B3CLASS=  Cs mum_age,B3TOT= cases,
id=_all_);

run;

data weighted_data2027; set weighted_data2027;
keep baby_ppn newwt;
run;

proc export 
  data=weighted_data2027 
  dbms=xlsx 
  outfile="G:\SAS Syntax_Papers\Microsim_hospital resources homebirth birthcentre COVID\Weights_2027.xlsx" 
  replace;
run;


data weighs; set weighted_data2023 weighted_data2024 weighted_data2025 weighted_data2026 weighted_data2027;
run;



