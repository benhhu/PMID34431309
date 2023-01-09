/*2001 diet without beans*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\xpt_data\CDBRFS01.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss01;
set dataout.CDBRFS01;
keep _STATE IYEAR _PSU AGE SEX _RACEGR2 EDUCA INCOME2 MARITAL HTM WTKG DIABETES BPHIGH2 TOLDHI2 SMOKE100 SMOKEDAY _FRTINDX _RFPAREC _FINALWT PREGNT2;
where SEX = 2;
run;
proc export 
data=work.brfss01
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\csv_data\brfss01.csv'
replace;
run;

/*2003 diet without beans*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\xpt_data\cdbrfs03.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss03;
set dataout.cdbrfs03;
keep _STATE IYEAR _PSU AGE SEX _RACEGR2 _EDUCAG _INCOMG MARITAL HTM2 WTKG DIABETES BPHIGH3 TOLDHI2 SMOKE100 SMOKEDAY _FRTINDX PACAT_ _FINALWT PREGNANT;
where SEX = 2;
run;
proc export 
data=work.brfss03
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\csv_data\brfss03.csv'
replace;
run;

/*2005 diet without beans*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\xpt_data\CDBRFS05.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss05;
set dataout.CDBRFS05;
keep _STATE IYEAR _PSU AGE SEX _RACEGR2 _EDUCAG _INCOMG MARITAL HTM3 WTKG2 DIABETE2 BPHIGH4 TOLDHI2 SMOKE100 SMOKDAY2 _FRTINDX PACAT_ _FINALWT PREGNANT RCSRELTN CVDINFR3 CVDCRHD3 CVDSTRK3;
where SEX = 2;
run;
proc export 
data=work.brfss05
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\csv_data\brfss05.csv'
replace;
run;

/*2007 diet without beans*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\xpt_data\CDBRFS07.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss07;
set dataout.CDBRFS07;
keep _STATE IYEAR _PSU AGE SEX _RACEGR2 _EDUCAG _INCOMG MARITAL HTM3 WTKG2 DIABETE2 BPHIGH4 TOLDHI2 SMOKE100 SMOKDAY2 _FRTINDX PACAT_ _FINALWT PREGNANT RCSRLTN2 CVDINFR4 CVDCRHD4 CVDSTRK3;
where SEX=2;
run;
proc export 
data=work.brfss07
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\csv_data\brfss07.csv'
replace;
run;

/*2009 diet without beans*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\xpt_data\CDBRFS09.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss09;
set dataout.CDBRFS09;
keep _STATE IYEAR _PSU AGE SEX _RACEGR2 _EDUCAG _INCOMG MARITAL HTM3 WTKG2 DIABETE2 BPHIGH4 TOLDHI2 SMOKE100 SMOKDAY2 _FRTINDX PACAT_ _FINALWT PREGNANT RCSRLTN2 CVDINFR4 CVDCRHD4 CVDSTRK3;
where SEX=2;
run;
proc export 
data=work.brfss09
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\PregnancyCVH\brfss_cvh\csv_data\brfss09.csv'
replace;
run;

/*2011 complete*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\xpt_data\LLCP2011.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss11;
set dataout.LLCP2011;
keep _STATE IYEAR _PSU AGE SEX _RACEGR2 _EDUCAG _INCOMG MARITAL HTM4 WTKG3 DIABETE3 BPHIGH4 TOLDHI2 SMOKE100 SMOKDAY2 FTJUDA1_ FRUTDA1_ BEANDAY_ GRENDAY_ ORNGDAY_ VEGEDA1_ _PA150R1 
_LLCPWT PREGNANT RCSRLTN2 CVDINFR4 CVDCRHD4 CVDSTRK3 HLTHPLN1;
where SEX=2;
run;
proc export 
data=work.brfss11
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\csv_data\brfss11.csv'
replace;
run;

/*2013 complete*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\xpt_data\LLCP2013.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss13;
set dataout.LLCP2013;
keep _STATE IYEAR _PSU _AGE80 SEX _RACEGR3 _EDUCAG _INCOMG MARITAL HTM4 WTKG3 DIABETE3 BPHIGH4 TOLDHI2 SMOKE100 SMOKDAY2 FTJUDA1_ FRUTDA1_ BEANDAY_ GRENDAY_ ORNGDAY_ VEGEDA1_ _PA150R2 
_LLCPWT PREGNANT RCSRLTN2 CVDINFR4 CVDCRHD4 CVDSTRK3 HLTHPLN1;
where SEX=2;
run;
proc export 
data=work.brfss13
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\csv_data\brfss13.csv'
replace;
run;

/*2015 complete*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\xpt_data\LLCP2015.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss15;
set dataout.LLCP2015;
keep _STATE IYEAR _PSU _AGE80 SEX _RACEGR3 _EDUCAG _INCOMG MARITAL HTM4 WTKG3 DIABETE3 BPHIGH4 TOLDHI2 SMOKE100 SMOKDAY2 FTJUDA1_ FRUTDA1_ BEANDAY_ GRENDAY_ ORNGDAY_ VEGEDA1_ _PA150R2 
_LLCPWT PREGNANT RCSRLTN2 CVDINFR4 CVDCRHD4 CVDSTRK3 HLTHPLN1;
where SEX=2;
run;
proc export 
data=work.brfss15
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\csv_data\brfss15.csv'
replace;
run;

/*2017 complete*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\xpt_data\LLCP2017.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss17;
set dataout.LLCP2017;
keep _STATE IYEAR _PSU _AGE80 SEX _RACEGR3 _EDUCAG _INCOMG MARITAL HTM4 WTKG3 DIABETE3 BPHIGH4 TOLDHI2 SMOKE100 SMOKDAY2 FTJUDA2_ FRUTDA2_ GRENDA1_ FRNCHDA_ POTADA1_ VEGEDA2_ _PA150R2 
_LLCPWT PREGNANT RCSRLTN2 CVDINFR4 CVDCRHD4 CVDSTRK3 HLTHPLN1;
where SEX=2;
run;
proc export 
data=work.brfss17
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\csv_data\brfss17.csv'
replace;
run;

/*2019 complete*/
DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;
OPTIONS PAGENO=1 NOFMTERR;
TITLE ;
FOOTNOTE ;
RUN ;
LIBNAME TRANSPRT XPORT  'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\xpt_data\LLCP2019.XPT';
LIBNAME DATAOUT 'C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\sasdata\'; 
PROC COPY IN=TRANSPRT OUT=DATAOUT;
RUN;
data WORK.brfss19;
set dataout.LLCP2019;
keep _STATE IYEAR _PSU _AGE80 SEXVAR _RACEGR3 _EDUCAG _INCOMG MARITAL HTM4 WTKG3 DIABETE4 BPHIGH4 TOLDHI2 SMOKE100 SMOKDAY2 FTJUDA2_ FRUTDA2_ GRENDA1_ FRNCHDA_ POTADA1_ VEGEDA2_ _PA150R3 
_LLCPWT PREGNANT RCSRLTN2 CVDINFR4 CVDCRHD4 CVDSTRK3 HLTHPLN1;
where SEXVAR=2;
run;
proc export 
data=work.brfss19
dbms=csv
outfile='C:\Users\arieszheng\OneDrive - University of Florida\Research Projects\BRFSS_CVH\brfss_childbearing\data\csv_data\brfss19.csv'
replace;
run;
