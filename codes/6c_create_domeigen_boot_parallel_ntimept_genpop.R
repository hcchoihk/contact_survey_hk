#### plot dominant eigenvalues for contact matrices

### load survey data
load( 'data/chk_part_contact_base.RData' )
if (!exists("selectedid_boot_simple_genpopOnly")){
	load( 'data/selectedid_genpopOnly_simple_nboot1000.RData' )
}

if (!exists("fun_cutdates_subsets")){
	source('codes/5_fun_create_date_subsets.R')
}

# run in parallel
library(future.apply)
if (!is(plan(), "multisession")){ # start a multisession if it does not yet exist
	num_Core = parallel::detectCores()
	plan(multisession, workers = min(12, num_Core-2)) 
}

## settings
age_limits_agegps = seq(1, num_agegps, by=1);
sym_cntmatrix_YN = FALSE;

# set upper bound for the num_cnt per age gp
uppbound_cnt_agegp = 25;

# CoMix, by multi phase, [, )
nsubsets_tocut = 14;
date_tocut = chk_part_base$date
date_tocut = date_tocut[chk_part_base$part_specialgroups_YN==0]
date_byphase = fun_cutdates_subsets(date_tocut, n_subsets=nsubsets_tocut)
date_byphase = c(as.Date("2021-09-01", format="%Y-%m-%d"), tail(head(date_byphase, -1),-1), as.Date('2023-12-31')) # use the self-defined start and end date
num_date_byphase = length(date_byphase)-1;
# table(cut(date_tocut, breaks=date_byphase, include.lowest=TRUE, right=FALSE)) # number of responses in each time phase

num_phycnt = 2;

names_phycnt = c("all", "phycnt")
labels_phycnt = c('Overall', 'Physical')

# bootstrapping

num_boot = 1000;

time1 = Sys.time()
fname_outputdate = update_outputdate();

boot_TT_chk = future_sapply(1:num_boot, simplify=FALSE, function(iboot) {
	## CoMix, contact matrices for each phase
	# based on chk_part_base, chk_contact_base
	chk_part_iboot = chk_part_base[unlist(selectedid_boot_simple_genpopOnly[[iboot]]),]
	rowidx_contact_list_iboot = sapply(chk_part_iboot$part_id, simplify=FALSE, function(x_part_id) which(chk_contact_base$part_id==x_part_id))
	rowidx_contact_iboot = unlist( rowidx_contact_list_iboot )

	chk_contact_iboot = chk_contact_base[rowidx_contact_iboot, , drop=FALSE]
	chk_contact_iboot = subset(chk_contact_iboot, subset=!is.na(cnt_age) )
	

	# by phase
	chk_socialmixr_byphase = rep(list(NULL), length=num_date_byphase)
	for (iphase in 1:num_date_byphase){
		irow_TT = (chk_part_iboot$date >= date_byphase[iphase]) & (chk_part_iboot$date < date_byphase[iphase+1])
		chk_part_TT = chk_part_iboot[irow_TT,];
		chk_contact_TT = chk_contact_iboot[chk_contact_iboot$part_id %in% chk_part_TT$part_id,]
		chk_socialmixr_byphase[[iphase]] <- survey(participants = chk_part_TT, contacts = chk_contact_TT)
	} # for-itime


	m_chk_list_phycnt_byphase = rep(list(NULL), length=num_date_byphase)

	for (iphase in 1:num_date_byphase){
		chk_socialmixr_TT = chk_socialmixr_byphase[[iphase]];

		m_chk_list_phycnt_byphase[[iphase]] = list(
		  all = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN)
		  , phys_contact = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(phys_contact = 1))
		  )
		
	} # for- iphase

	## eigenvalues
	chk_eigen_out_matrix = matrix(NA, nrow=num_date_byphase, ncol=num_phycnt)
	for (iphy in 1:num_phycnt){
		for (iphase in 1:num_date_byphase){
			m_contacts_TT = m_chk_list_phycnt_byphase[[iphase]][[iphy]]$matrix
			m_contacts_TT[is.na(m_contacts_TT)] = 0;
			m_contacts_TT[m_contacts_TT>uppbound_cnt_agegp] = uppbound_cnt_agegp;
			
			eigenval_TT = eigen(m_contacts_TT)$values;
			eigenval_TT = as.numeric(eigenval_TT[Im(eigenval_TT)==0])
			chk_eigen_out_matrix[iphase, iphy] = max(eigenval_TT)
		}
	} # for-irow

 	return( list(m_chk_phy=m_chk_list_phycnt_byphase, max_eigen_chk=chk_eigen_out_matrix) )

}) # iboot

cntmat_comix_boot_phy = lapply(boot_TT_chk, function(xout) xout$m_chk_phy)
max_eigenval_cntmat_comix_boot = lapply(boot_TT_chk, function(xout) xout$max_eigen_chk)


time2 = Sys.time()
print( time2 - time1)


# turn off multisession {future}
plan(sequential)
rm(boot_TT_chk)


fname_out_RData = sprintf("domeigen_cntmat_ntimept_nboot%d_genpop.RData", num_boot)
save(date_byphase, cntmat_comix_boot_phy, max_eigenval_cntmat_comix_boot,
 file=fname_out_RData)

# export to xlsx
names(max_eigenval_cntmat_comix_boot) = paste0("boot", 1:length(max_eigenval_cntmat_comix_boot))

# element-wise statistics 
array_max_eigenval_cntmat_comix_boot = simplify2array(max_eigenval_cntmat_comix_boot) # change from a list to an array
stat_max_eigenval_cntmat_comix_boot = list(mean=apply(array_max_eigenval_cntmat_comix_boot, 1:2, mean), # Reduce("+", eigenval_list) / length(eigenval_list)
	median=apply(array_max_eigenval_cntmat_comix_boot, 1:2, median),
	pct0025 = apply(array_max_eigenval_cntmat_comix_boot, 1:2, quantile, probs=0.025),
	pct0975 = apply(array_max_eigenval_cntmat_comix_boot, 1:2, quantile, probs=0.975)
	)

fname_out_RData = paste0("stat_", fname_out_RData)
save(stat_max_eigenval_cntmat_comix_boot, max_eigenval_cntmat_comix_boot, file=fname_out_RData)
fname_out_xlsx = gsub(".RData", sprintf("_%s.xlsx", fname_outputdate), fixed=TRUE, fname_out_RData)
openxlsx::write.xlsx(c(stat_max_eigenval_cntmat_comix_boot, max_eigenval_cntmat_comix_boot), file = fname_out_xlsx, rowNames=TRUE)
