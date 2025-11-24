# regression analysis with the resampled dataset


date_studystart <- as.Date("2021-09-18")
date_studyend <- as.Date("2023-12-31")

### load time-varying variables
load('data/data_govtstat.RData')


## cumulative vaccination coverage
# from data_govtstat.RData
cumVaccRate_date_18agegp_2sex = data_govtstat_cumVaccRate_bydate_18agegp_2sex;


## survey data
chk_reg = chk_base

chk_reg = within(chk_reg, {
	cnt_count = contact_number

	gender = sex
	gender = factor(gender, level=c("Male", "Female"))
	
	spc_grps_YN = special_grps_YN
	
	fifthwave_4grp = waveperiod_4gp;
	fifthwave_4grp = factor(fifthwave_4grp, level=1:4)
	
	contact_weekend = factor(contact_weekend, level=0:1)
	vaccinated = factor(vaccinated, level=0:1)
})


## append time variable to dataset
# vaccine uptake of the same age groups (cumlative vaccination rate)
chk_reg$pct_VaccRate_byAge_timet = NA;
for (ii_chk in 1:nrow(chk_reg)){
	xx_gender_chk = chk_reg[ii_chk, "gender"]
	xx_date_chk = chk_reg[ii_chk, "contact_date"]
	xx_agegp_chk = chk_reg[ii_chk, "agegp"]

	if (!is.na(xx_agegp_chk)){
		# by age group
		if (!is.na(xx_gender_chk)){
			irow_Date = match(xx_date_chk, cumVaccRate_date_18agegp_2sex[[xx_gender_chk]]$Date)
			chk_reg$pct_VaccRate_byAge_timet[ii_chk] = cumVaccRate_date_18agegp_2sex[[xx_gender_chk]][irow_Date, xx_agegp_chk]
		}
	}
} # for- ii_chk

## regression
deptvarname_reg = "cnt_count";

# indept variables
indeptvarnames_reg_base = c("part_age_3gps", "gender", "housesize_3grp", "contact_weekend", "vaccinated", "spc_grps_YN", "fifthwave_4grp")
indeptvarnames_reg_base_vaccRate_byAgeGp = union(indeptvarnames_reg_base, "pct_VaccRate_byAge_timet")
indeptvarnames_reg_list = list(base_vaccRate_byAgeGp = indeptvarnames_reg_base_vaccRate_byAgeGp
);

num_reg_out = length(indeptvarnames_reg_list)
reg_out_numcount_list = rep(list(NULL), num_reg_out);
reg_out_coef_list = rep(list(NULL), num_reg_out);
names(reg_out_numcount_list) = "numcount_base"
names(reg_out_coef_list) = "coef_base"

# start analysis
fname_out = sprintf("reg_output_%s.txt", update_outputdate(short=TRUE))
fname_out_xlsx = gsub('.txt', '.xlsx', fname_out, fixed=TRUE)
sink(fname_out)

for (ii_reg in 1:num_reg_out){
	indeptvarnames_reg = indeptvarnames_reg_list[[ii_reg]]

	varnames_reg = c(deptvarname_reg, indeptvarnames_reg)
	d2_contact_reg = subset(chk_reg, select=varnames_reg)
	d2_contact_reg = na.omit(d2_contact_reg)

	
	writeLines("\n\n")
	print( sprintf("reg model: %s", names(indeptvarnames_reg_list)[ii_reg]) )
	print( sprintf("var: y=%s; x=%s", deptvarname_reg, paste(indeptvarnames_reg, collapse=", ")) )
	print( sprintf("n of d2_contact_reg: %d", nrow(d2_contact_reg)) )

	num_indeptvar = length(indeptvarnames_reg);
	reg_out_numcount_TT = rep(list(NULL), num_indeptvar)
	names(reg_out_numcount_TT) = indeptvarnames_reg
	tab_TT_cont = matrix(NA, ncol=1, nrow=1, dimnames=list("cont_var", ""))
	for (ii_x in 1:num_indeptvar){ # table for categorical variables
		x_indeptvar = indeptvarnames_reg[ii_x]
		if ( !grepl("dailycases|Rt|vaccRate", x_indeptvar) ){
			if (length(table(d2_contact_reg[, x_indeptvar]))<10){
				tab_TT = table(d2_contact_reg[, x_indeptvar])
				print( x_indeptvar )
				print( tab_TT )
				
				tab_TT = as.matrix(tab_TT, ncol=1)
			} else{
				tab_TT = tab_TT_cont
			}
		} else {
			tab_TT = tab_TT_cont
		}
		colnames(tab_TT) = x_indeptvar
		reg_out_numcount_TT[[ii_x]] = tab_TT;

	}
	tab_TT = matrix(nrow(d2_contact_reg), ncol=1)
	dimnames(tab_TT) = list("overall", "overall")
	reg_out_numcount_TT = c("overall"=list(tab_TT), reg_out_numcount_TT)
	
	reg_out_numcount_TT = do.call(rbind, sapply(1:length(reg_out_numcount_TT), simplify=FALSE, function(ii) {
		out_TT = rbind(NA, reg_out_numcount_TT[[ii]], NA)
		rownames(out_TT) = paste(names(reg_out_numcount_TT)[ii], c("", rownames(reg_out_numcount_TT[[ii]]), "blank"), sep="_");
		return(out_TT)
	}));
	colnames(reg_out_numcount_TT) = "Num of counts"
	reg_out_numcount_list[[ii_reg]] = reg_out_numcount_TT;	
	
	# regression
	str_frmla_nb_base = 'MASS::glm.nb(cnt_count ~ %s , data = d2_contact_reg)'
	str_frmla_nb = sprintf(str_frmla_nb_base, paste(indeptvarnames_reg, collapse=" + "))
	nb_out = eval(parse(text=str_frmla_nb))
	coef_nb_out = nb_out$coefficient
	confint_nb_out = confint(profile(nb_out))
	coefout_nb_out = cbind(coef_nb_out, confint_nb_out[1:length(coef_nb_out),])
	coefout_exp_nb_out = exp(coefout_nb_out)
	print( summary(nb_out) )# p-value
	print( round(coefout_exp_nb_out,3) )
	print( sprintf("AIC: %.2f", AIC(nb_out)) )
	
	reg_out_coef_TT = cbind(round(coefout_exp_nb_out,2), round(coefout_exp_nb_out,4))
	pvalue_TT = summary(nb_out)$coefficients[, "Pr(>|z|)"]
	reg_out_coef_TT = cbind(reg_out_coef_TT, p_value = pvalue_TT)
	
	# drop1
	frm_drop1 = sapply(1:num_indeptvar, simplify=FALSE, function(x) paste(indeptvarnames_reg[setdiff(1:num_indeptvar, x)], collapse=" + "))
	frm_drop1 = sapply(frm_drop1, simplify=FALSE, function(x) sprintf(str_frmla_nb_base, x))

	writeLines("\n")
	pval_drop1_LRT = setNames(rep(0, num_indeptvar), indeptvarnames_reg)
	pval_drop1_Wald = setNames(rep(0, num_indeptvar), indeptvarnames_reg)
	for (ii_var in 1:num_indeptvar){
		nb_out_drop1 = eval(parse(text=frm_drop1[[ii_var]]))
		lmtest_out = lmtest::lrtest(nb_out_drop1, nb_out)
		pval_drop1_LRT[ii_var] = lmtest_out[2,"Pr(>Chisq)"]
		lmtest_out = lmtest::waldtest(nb_out_drop1, nb_out, test="Chisq")
		pval_drop1_Wald[ii_var] = lmtest_out[2,"Pr(>Chisq)"]
	}
	print( "p-value, drop1" )
	print(data.frame(pval_drop1_LRT))
	
	reg_out_coef_TT = cbind(reg_out_coef_TT, "drop1_pval_LRT"=NA, "drop1_pval_Wald"=NA)
	
	for (ii_var in 1:num_indeptvar){
		pval_TT = pval_drop1_LRT[ii_var]
		irow_TT = grep(names(pval_TT), rownames(reg_out_coef_TT))[1]
		reg_out_coef_TT[irow_TT, 'drop1_pval_LRT'] = as.numeric(pval_TT)
		reg_out_coef_TT[irow_TT, 'drop1_pval_Wald'] = as.numeric(pval_drop1_Wald[ii_var])
	} 

	reg_out_coef_TT = as.data.frame(reg_out_coef_TT)
	
	reg_out_coef_TT$text_out2_coef = sprintf("%.2f", coefout_exp_nb_out[,1])
	reg_out_coef_TT$text_out2_CI = apply(coefout_exp_nb_out,1, function(xcoef) sprintf("(%.2f, %.2f)", xcoef[2], xcoef[3]))
	reg_out_coef_TT$text_outp = ifelse(pvalue_TT<0.001, "<0.001", ifelse(pvalue_TT<0.1, sprintf("%.3f", pvalue_TT), sprintf("%.2f", pvalue_TT)))
	reg_out_coef_TT$text_out4_coef = sprintf("%.4f", coefout_exp_nb_out[,1])
	reg_out_coef_TT$text_out4_CI = apply(coefout_exp_nb_out,1, function(xcoef) sprintf("(%.4f, %.4f)", xcoef[2], xcoef[3]))
	reg_out_coef_TT$text_out2 = apply(coefout_exp_nb_out,1, function(xcoef) sprintf("%.2f (%.2f, %.2f)", xcoef[1], xcoef[2], xcoef[3]))
	reg_out_coef_TT$text_out4 = apply(coefout_exp_nb_out,1, function(xcoef) sprintf("%.4f (%.4f, %.4f)", xcoef[1], xcoef[2], xcoef[3]))
	
	reg_out_coef_list[[ii_reg]] = reg_out_coef_TT;

} # for-ii_reg

sink()

openxlsx::write.xlsx(c(reg_out_numcount_list, reg_out_coef_list), file=fname_out_xlsx, rowNames=TRUE)
