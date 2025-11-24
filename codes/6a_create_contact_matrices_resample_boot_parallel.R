#### plot contact matrices for each of the four phases

### load 2015-2016 survey from Leung Sci Rep 2017 data
if (!exists("d17_selectedid_boot")){
	load( 'data/d17_selectedid_resample_nboot1000_16agegps.RData' )
	load( 'data/d17_part_contact.RData')
}

### load survey data
# load( 'data/chk_base.RData' )
load( 'data/chk_part_contact_base.RData' )
if (!exists("selectedid_boot")){
	load( 'data/selectedid_resample_nboot1000_16agegps.RData' )
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

# CoMix, by 4 phase, [, )
num_phases = 4
date_byphase = as.Date(c("2021-09-01", "2022-01-07", "2022-04-21", "2023-03-01", "2024-01-01"))
num_date_byphase = length(date_byphase)-1;


# bootstrapping

num_boot = 1000;


time1 = Sys.time()
fname_outputdate = update_outputdate();
fname_out_suffix = "_resample"
# run d17 and chk separately
boot_TT_d17 = future_sapply(1:num_boot, simplify=FALSE, function(iboot) {


	## d17
	d17_part_iboot = d17_part[d17_selectedid_boot[[iboot]], ]

	rowidx_contact_list_iboot = sapply(d17_part_iboot$part_id, simplify=FALSE, function(x_part_id) which(d17_contact$part_id==x_part_id))
	rowidx_contact_iboot = unlist( rowidx_contact_list_iboot )

	d17_contact_iboot = d17_contact[rowidx_contact_iboot, , drop=FALSE]
	d17_contact_iboot = subset(d17_contact_iboot, subset=!is.na(cnt_age) )

	d17_socialmixr <- survey(participants = d17_part_iboot, contacts = d17_contact_iboot)


	m_d17_list_loc = list(
	  all = contact_matrix(survey = d17_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN),
	  home = contact_matrix(survey = d17_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_home = 1)),
	  school = contact_matrix(survey = d17_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_school = 1)),
	  work = contact_matrix(survey = d17_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_work = 1)),
	  others = contact_matrix(survey = d17_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_others = 1))
	)

	m_d17_list_phycnt = list(
		all = m_d17_list_loc$all,
		phys_cnt = contact_matrix(survey = d17_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(phys_contact = 1)),
		nonphys_cnt = contact_matrix(survey = d17_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(phys_contact = 2))
	)

	# eigenvalues 
	d17_eigen_out_matrix = matrix(NA, nrow=1, ncol=3)
	for (iphy in 1:3){
		m_contacts_TT = m_d17_list_phycnt[[iphy]]$matrix
		m_contacts_TT[is.na(m_contacts_TT)] = 0;
		eigenval_TT = eigen(m_contacts_TT)$values;
		eigenval_TT = as.numeric(eigenval_TT[Im(eigenval_TT)==0])
		d17_eigen_out_matrix[1, iphy] = max(eigenval_TT)
	} # for-irow
	
	return( list(m_d17_loc=m_d17_list_loc, m_d17_phy=m_d17_list_phycnt, max_eigen_d17=d17_eigen_out_matrix) )

}) # for iboot- boot_TT_d17
time2 = Sys.time()
print( time2 - time1)


boot_TT_chk = future_sapply(1:num_boot, simplify=FALSE, function(iboot) {

	## CoMix, contact matrices for each phase
	# based on chk_part_base, chk_contact_base
	chk_part_iboot = chk_part_base[unlist(selectedid_boot[[iboot]]),]
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


	m_chk_list_loc_byphase = rep(list(NULL), length=num_date_byphase)
	m_chk_list_phycnt_byphase = rep(list(NULL), length=num_date_byphase)

	for (iphase in 1:num_date_byphase){
		chk_socialmixr_TT = chk_socialmixr_byphase[[iphase]];

		m_chk_list_loc_byphase[[iphase]] = list(
		  all = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN),
		  home = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_home = 1)),
		  school = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_school = 1)),
		  work = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_work = 1)),
		  others = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_others = 1)))

		m_chk_list_phycnt_byphase[[iphase]] = list(
		  all = m_chk_list_loc_byphase[[iphase]]$all
		  , phys_contact = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(phys_contact = 1))
		  , nonphys_contact = contact_matrix(survey = chk_socialmixr_TT, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(phys_contact = 2))
		  )
		
	} # for- iphase

	## eigenvalues
	chk_eigen_out_matrix = matrix(NA, nrow=num_date_byphase, ncol=3)
	for (iphy in 1:3){
		for (iphase in 1:num_date_byphase){
			m_contacts_TT = m_chk_list_phycnt_byphase[[iphase]][[iphy]]$matrix
			m_contacts_TT[is.na(m_contacts_TT)] = 0;
			m_contacts_TT[m_contacts_TT>uppbound_cnt_agegp] = uppbound_cnt_agegp;
			eigenval_TT = eigen(m_contacts_TT)$values;
			eigenval_TT = as.numeric(eigenval_TT[Im(eigenval_TT)==0])
			chk_eigen_out_matrix[iphase, iphy] = max(eigenval_TT)
		}
	} # for-irow

	return( list(m_chk_loc=m_chk_list_loc_byphase, m_chk_phy=m_chk_list_phycnt_byphase, max_eigen_chk=chk_eigen_out_matrix) )

}) # iboot

cntmat_d17_boot_loc = lapply(boot_TT_d17, function(xout) xout$m_d17_loc)
cntmat_d17_boot_phy = lapply(boot_TT_d17, function(xout) xout$m_d17_phy)
max_eigenval_cntmat_d17_boot = lapply(boot_TT_d17, function(xout) xout$max_eigen_d17)

cntmat_comix_boot_loc = lapply(boot_TT_chk, function(xout) xout$m_chk_loc)
cntmat_comix_boot_phy = lapply(boot_TT_chk, function(xout) xout$m_chk_phy)
max_eigenval_cntmat_comix_boot = lapply(boot_TT_chk, function(xout) xout$max_eigen_chk)


time2 = Sys.time()
print( time2 - time1)


# turn off multisession {future}
plan(sequential)
rm(boot_TT_d17, boot_TT_chk)

fname_out_RData = sprintf("cntmat_4phases_nboot%d%s.RData", num_boot, fname_out_suffix)
save(cntmat_d17_boot_loc, cntmat_d17_boot_phy, max_eigenval_cntmat_d17_boot, 
cntmat_comix_boot_loc, cntmat_comix_boot_phy, max_eigenval_cntmat_comix_boot,
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

save(stat_max_eigenval_cntmat_comix_boot,
max_eigenval_cntmat_comix_boot, file=sprintf("max_eigenval_cntmat_nboot%d_%s%s.RData", num_boot, fname_outputdate, fname_out_suffix))
fname_out_xlsx = paste0("eigenval_", gsub("RData", "xlsx", fname_out_RData))
openxlsx::write.xlsx(c(stat_max_eigenval_cntmat_comix_boot,
max_eigenval_cntmat_comix_boot), file = fname_out_xlsx, rowNames=TRUE)
