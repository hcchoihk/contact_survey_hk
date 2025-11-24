#### create tables for mean contact


## date of study period by 4 phase
date_5thwave_string = c("2022-01-07","2022-04-20")
date_5thwave = as.Date(date_5thwave_string, format="%Y-%m-%d")
date_5thwave_start = min(date_5thwave)
date_5thwave_end = max(date_5thwave)

# re. the dates and measures stated in measurs.csv
date_nomask2023 = as.Date("2023-02-08", format="%Y-%m-%d")
date_noisolation2023 = as.Date("2023-01-30", format="%Y-%m-%d")

## load data
if (!exists('chk_base')){ 
	load( 'data/chk_base.RData' )
}

# load the default tab2 for exporting
n_phases = 4 # whether to split after 5th wave into two phases
fname_table_blank = 'table_contact_4phases_blank.xlsx'
tab2_org = openxlsx::read.xlsx( paste0("data/", fname_table_blank), sheet = 'table_cnt_bycat', skipEmptyCols=FALSE)
tab2_blank = tab2_org


# check the variables and category names
var_list = c("sex", "agegp6", "contact_weekend", "hh_size", "income", "vaccinated", "occupation")
if (!(all(var_list %in% colnames(chk_base)))){ stop("Check the data")}
chk_base$hhsize = chk_base$hh_size;


fun_statout_text = function(x, IQR_CI="IQR"){
	spf_format = "%.1f" # "%.2f"
	dp_IQR = 1 # 2 # decimal places
	if (IQR_CI=="IQR"){
		out = paste0(
			sprintf(spf_format, mean(x, na.rm=TRUE)),' (',
			round(as.numeric(quantile(x,probs = 0.25, na.rm=TRUE)), dp_IQR),', ',
			round(as.numeric(quantile(x,probs = 0.75, na.rm=TRUE)), dp_IQR),')')
	} else if (IQR_CI=="CI"){
		unique_x = unique(x)
		unique_x = unique_x[!is.na(unique_x)];
		if (length(unique_x)<=1){
			# no CI for 1 data or all data are the same, no no data
			xstat = mean(x, na.rm=TRUE);
			out = paste0(
				sprintf(spf_format, xstat[1]),' (',
				"NA",', ',
				"NA",')')
		} else{
			xstat = unlist(t.test(x)[c("estimate","conf.int")])
			out = paste0(
				sprintf(spf_format, xstat[1]),' (',
				sprintf(spf_format, xstat[2]),', ',
				sprintf(spf_format, xstat[3]),')')
		}
	}
	return(out)
} # fun_statout_text


# whether to show IQR or CI stats. the comparisons are done by wilcox.test, including when showing IQR
for (IQR_CI in c("IQR", "CI")){


if (IQR_CI=="CI"){
	colnames(tab2_blank) = gsub("(IQR)", "(CI)", colnames(tab2_blank), fixed=TRUE)
}


## analyse for with and without high-contact group
tab2_out = rep(list(tab2_blank), 3)
names(tab2_out) = c("table_cnt_bycat_wSpg", "table_cnt_bycat_noSpg", "table_cnt_bycat_SpgOnly")

# get the numbers and stat
# jcol_time, jcol_comp_list are different from that for comparing genpop vs spg
jcol_itime = list(c(n=3, cnt=4, cnt_hr=12), c(n=5, cnt=6, cnt_hr=13), c(n=7, cnt=8, cnt_hr=14), c(n=9, cnt=10, cnt_hr=15))
jcol_comp_list = c("cnt_2_1"=16, "cnt_3_1"=17, "cnt_4_1"=18, "cntduration_2_1"=19, "cntduration_3_1"=20, "cntduration_4_1"=21)


for (itype in 1:3){

	chk_table = chk_base
	if (itype == 2){
		chk_table = subset(chk_base, subset=(chk_base$special_grps_YN==0))
	} else if (itype == 3){
		chk_table = subset(chk_base, subset=(chk_base$special_grps_YN==1))
	}
	
	tab2 = tab2_blank

	for (itime in 1:n_phases){
		jcol_n = jcol_itime[[itime]]["n"]
		jcol_cnt = jcol_itime[[itime]]["cnt"]
		jcol_cnt_hr = jcol_itime[[itime]]["cnt_hr"]
		
		chk_itime = chk_table;
		chk_itime = subset(chk_table, subset=chk_table$waveperiod_4gp==itime)
		cnt_T_all = chk_itime$contact_number;
		cnt_hr_T_all = chk_itime$contact_time_hours;
		# cnt_T will be changed based on irow_perCat, the irow per category

		# overall
		irow = 1
		tab2[irow,jcol_n] = nrow(chk_itime)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all, IQR_CI=IQR_CI);
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all, IQR_CI=IQR_CI);

		# sex, Female, Male
		irow = irow + 1;
		irow_perCat = (chk_itime$sex %in% 'Female');
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);
		  
		irow = irow + 1;
		irow_perCat = (chk_itime$sex %in% 'Male');
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);
		
		# age group
		for(a in 1:6)
		{
			irow = irow + 1;
			if (itype==3 & a %in% c(1:2, 6)){
				next # not to run for special groups
			}
			irow_perCat = (chk_itime$agegp6 %in% a);
			tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
			tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
			tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);
		}
		
		# age group; 0-20, 21-65, >65, not to run >65 as it's done in agegp6
		for(a in c(1,3,5))
		{
			irow = irow + 1;
			if (itype==3 & a %in% (1)){ next} # not to run for special groups
			irow_perCat = (chk_itime$agegp_020_2165_65p %in% a);
			tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
			tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
			tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);
		}

		# weekday, weekend
		irow = irow + 1;
		irow_perCat = (chk_itime$contact_weekend %in% 0);
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);
		
		irow = irow + 1;
		irow_perCat = (chk_itime$contact_weekend %in% 1);
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);

		# household
		for(a in 1:4)
		{
			irow = irow + 1;
			irow_perCat = (chk_itime$hhsize == a);
			tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
			tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
			tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);
		}
		irow = irow + 1;
		irow_perCat = (chk_itime$hhsize >= 5);
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);

		
		# income
		irow = irow + 1;
		irow_perCat = chk_itime$income1 %in% c("0-5,999","6,000-9,999") ;
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);

		irow = irow + 1;
		irow_perCat = chk_itime$income1 %in% c("10,000-14,999","15,000-19,999" ) 
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);

		irow = irow + 1;
		irow_perCat = (chk_itime$income1 %in% c("20,000-24,999" ,"25,000-29,999","30,000-39,999") );
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);

		irow = irow + 1;
		irow_perCat = (chk_itime$income1 %in% c("40,000-54,999" ,">= 60,000"));
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);


		# vaccinated or not
		irow = irow + 1;
		irow_perCat = (chk_itime$vaccinated %in% 1);
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);

		irow = irow + 1;
		irow_perCat = (chk_itime$vaccinated %in% 0);
		tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
		tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
		tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);


		# occupation / selected groups
		if (itype %in% c(1,3)){
			# belong to any of the selected groups
			irow = irow +1;
			irow_perCat = (chk_itime$special_grps_YN ==1);
			tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
			tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
			tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);
			
			# belong to a specific selected group
			for (a in 1:8){
				irow = irow + 1;
				irow_perCat = (chk_itime[, paste0("spg",a)] ==1);
				tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
				tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_all[irow_perCat], IQR_CI=IQR_CI)
				tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_all[irow_perCat], IQR_CI=IQR_CI);
			}
		} # if- itype==1
	} # end itime
	

	# comparison between phases
	# all are done by wilcox.test, including when showing IQR
	# refer to the definition of "jcol_comp_list" above
	itime_comp_list = sapply(names(jcol_comp_list), simplify=FALSE, function(x) as.numeric(strsplit(x, "_")[[1]][c(2,3)]))

	for (icomp in 1:length(jcol_comp_list)){
		jcol_comp = jcol_comp_list[icomp]
		itime_comp_T = itime_comp_list[[icomp]]
		
		itime1 = itime_comp_T[1]
		itime2 = itime_comp_T[2]
		
		chk_TT = chk_table
		if (grepl("cnt_", names(jcol_comp_list)[icomp])){
			cnt_T_all = chk_TT$contact_number
		} else if (grepl("cntduration_", names(jcol_comp_list)[icomp])){
			cnt_T_all = chk_TT$contact_time_hours
		}
		
		irow_dd_itime1_all = (chk_TT$waveperiod_4gp==itime1)
		irow_dd_itime2_all = (chk_TT$waveperiod_4gp==itime2)

		irow = 1
		
		# overall
		# use t.test to check the mean
		# print("262"); browser()
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all], cnt_T_all[irow_dd_itime2_all], exact=FALSE)$p.value
		
		
		# sex
		irow = irow + 1;
		irow_perCat = (chk_TT$sex %in% 'Female');
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value
		
		irow = irow + 1;
		irow_perCat = (chk_TT$sex %in% 'Male');
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value
		
		
		# age group
		for(a in 1:6)
		{
			irow = irow + 1;
			if (itype==3 & a %in% c(1:2, 6)){
				next # not to run for special groups
			}
			irow_perCat = (chk_TT$agegp6 %in% a);
			tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value
		}

		# age group; 0-20, 21-65, >65, not to run >65 as it's done in agegp6
		for(a in c(1,3,5))
		{
			irow = irow + 1;
			if (itype==3 & a %in% (1)){ next} # not to run for special groups
			irow_perCat = (chk_TT$agegp_020_2165_65p %in% a);

			tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value
		}


		# weekday, weekend
		irow = irow + 1;
		irow_perCat = (chk_TT$contact_weekend %in% 0);
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value
		
		irow = irow + 1;
		irow_perCat = (chk_TT$contact_weekend %in% 1);
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value

		# household
		for(a in 1:4)
		{
			irow = irow + 1;
			irow_perCat = (chk_TT$hhsize %in% a);
			tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value
		}
		irow = irow + 1;
		irow_perCat = (chk_TT$hhsize >= 5);
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value

		
		# income
		irow = irow + 1;
		irow_perCat = chk_TT$income1 %in% c("0-5,999","6,000-9,999") ;
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value

		irow = irow + 1;
		irow_perCat = chk_TT$income1 %in% c("10,000-14,999","15,000-19,999" ) 
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value

		irow = irow + 1;
		irow_perCat = (chk_TT$income1 %in% c("20,000-24,999" ,"25,000-29,999","30,000-39,999") );
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value

		irow = irow + 1;
		irow_perCat = (chk_TT$income1 %in% c("40,000-54,999" ,">= 60,000"));
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value


		# vaccinated or not
		irow = irow + 1;
		irow_perCat = (chk_TT$vaccinated %in% 1);
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value

		irow = irow + 1;
		irow_perCat = (chk_TT$vaccinated %in% 0);
		tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value


		# occupation / selected groups
		if (itype==1 | itype==3){
			irow = irow + 1;
			irow_perCat = (chk_TT$special_grps_YN ==1);
			tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value
			for (a in 1:8){
				irow = irow + 1;
				irow_perCat = (chk_TT[, paste0("spg",a)] ==1);
				tab2[irow, jcol_comp] = wilcox.test(cnt_T_all[irow_dd_itime1_all & irow_perCat], cnt_T_all[irow_dd_itime2_all & irow_perCat], exact=FALSE)$p.value
			}
		} # if- itype==1
	
	} # icomp

	tab2_out[[itype]] = tab2
} # for- itype


# comparison, gen pop vs Spg

tab2 = openxlsx::read.xlsx( paste0("data/", fname_table_blank), sheet = 'table_comp_genpop_Spg', skipEmptyCols=FALSE)
tab2_comp = list("table_comp_genpop_Spg"=tab2)

# get the numbers and stat
# jcol_time, jcol_comp_list are different from that for all participants, genpop, spg
jcol_itime = list(c(n=3, cnt=4, cnt_hr=13), c(n=5, cnt=6, cnt_hr=14), c(n=7, cnt=8, cnt_hr=15), c(n=9, cnt=10, cnt_hr=16), c(n=11, cnt=12, cnt_hr=17))

chk_table = chk_base

for (itime in 1:(n_phases+1)){
	jcol_n = jcol_itime[[itime]]["n"]
	jcol_cnt = jcol_itime[[itime]]["cnt"]
	jcol_cnt_hr = jcol_itime[[itime]]["cnt_hr"]

	if (itime %in% (1:n_phases)){
		chk_itime = chk_table; # run if match with the original
		chk_itime = subset(chk_table, subset=chk_table$waveperiod_4gp==itime)
	} else {
		chk_itime = chk_table
	}
	cnt_T_all = chk_itime$contact_number;
	cnt_hr_T_all = chk_itime$contact_time_hours;
	
	irow = 0
	# general population, not in the special group
	irow = irow + 1;
	irow_perCat = (chk_itime$special_grps_YN==0);
	cnt_T_genpop = cnt_T_all[irow_perCat];
	cnt_hr_T_genpop = cnt_hr_T_all[irow_perCat]
	tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
	tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_genpop, IQR_CI=IQR_CI)
	tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_genpop, IQR_CI=IQR_CI);
	
	# in the special group 
	irow = irow + 1;
	irow_perCat = (chk_itime$special_grps_YN==1);
	cnt_T_Spg = cnt_T_all[irow_perCat];
	cnt_hr_T_Spg = cnt_hr_T_all[irow_perCat]
	tab2[irow,jcol_n] = sum(irow_perCat, na.rm=TRUE)
	tab2[irow,jcol_cnt] = fun_statout_text(cnt_T_Spg, IQR_CI=IQR_CI)
	tab2[irow,jcol_cnt_hr] = fun_statout_text(cnt_hr_T_Spg, IQR_CI=IQR_CI);
	
	# compare cnt and cnt_hr
	ttest_out_cnt = t.test(cnt_T_genpop, cnt_T_Spg)
	wilcoxtest_out_cnt = wilcox.test(cnt_T_genpop, cnt_T_Spg, exact=FALSE)
	
	ttest_out_cnt_hr = t.test(cnt_hr_T_genpop, cnt_hr_T_Spg)
	wilcoxtest_out_cnt_hr = wilcox.test(cnt_hr_T_genpop, cnt_hr_T_Spg, exact=FALSE)
	
	# diff = Y-X >> Spg - genpop
	irow = irow + 1;	
	tab2[irow,jcol_cnt] = as.numeric(diff(ttest_out_cnt$estimate)) 
	tab2[irow,jcol_cnt_hr] = as.numeric(diff(ttest_out_cnt_hr$estimate)) 
	
	# t.test test the difference of X-Y, so pick reverse the conf.int
	irow = irow + 1:2;	
	tab2[irow,jcol_cnt] = as.numeric(-rev(ttest_out_cnt$conf.int)) 
	tab2[irow,jcol_cnt_hr] = as.numeric(-rev(ttest_out_cnt_hr$conf.int))
	
	# p-value for t-test
	irow = max(irow) + 1;
	tab2[irow,jcol_cnt] = as.numeric(ttest_out_cnt$p.value) 
	tab2[irow,jcol_cnt_hr] = as.numeric(ttest_out_cnt_hr$p.value) 

	# p-value for wilcox-test
	irow = irow + 1;
	tab2[irow,jcol_cnt] = as.numeric(wilcoxtest_out_cnt$p.value) 
	tab2[irow,jcol_cnt_hr] = as.numeric(wilcoxtest_out_cnt_hr$p.value) 
} # for- itime

tab2_comp[[1]] = tab2

# export to table
fname_out = sprintf("table_mean_contact_bycat_4phases_%s_%s.xlsx", update_outputdate(short=TRUE), IQR_CI);
openxlsx::write.xlsx(c(tab2_out,tab2_comp), fname_out)

} # for- IQR_CI
