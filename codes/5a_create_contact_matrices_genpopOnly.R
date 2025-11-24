#### plot contact matrices for each of the four phases

library(socialmixr)

### load survey data
load( 'data/chk_part_contact_base.RData' )

### load 2015-2016 survey from Leung Sci Rep 2017 data
load( 'data/d17_part_contact.RData' )
d17_contact = subset(d17_contact, subset=!is.na(cnt_age))


## settings
age_limits_agegps = seq(1, num_agegps, by=1);
sym_cntmatrix_YN = FALSE;

# set upper bound for the num_cnt per age gp
uppbound_cnt_agegp = 25;

# CoMix, by 4 phase, [, )
num_phases = 4
date_byphase = as.Date(c("2021-09-01", "2022-01-07", "2022-04-21", "2023-03-01", "2024-01-01"))
num_date_byphase = length(date_byphase)-1;


num_loc = 5;
num_phycnt = 3;
num_time = num_phases + 1; # +1 for the 2015-2016 survey

names_loc = c('all', 'Home', 'School', 'Work', 'Others');
names_phycnt = c("all", "phycnt", "nonphycnt")
labels_loc = c("Overall", names_loc[-1])
labels_phycnt = c('Overall', 'Physical', 'Non-physical')
names_time = c("before5", "during5", "after5_p1", "after5_p2", "survey_1516")
labels_time = c("Pre-fifth wave", "Fifth wave", "Post-fifth wave", "Post-pandemic", "2015-2016 survey")
# add dates 
labels_time[1:4] = sapply(1:4, function(it) paste(labels_time[it], sprintf("%s to %s", date_byphase[it], date_byphase[it+1]-1), sep="\n") )


# based on the original data set
cntmat_loc_array_genpop = array(list(NULL), dim=c(num_loc, num_time));
dimnames(cntmat_loc_array_genpop) = list(names_loc, names_time);
cntmat_phy_array_genpop = array(list(NULL), dim=c(num_phycnt, num_time));
dimnames(cntmat_phy_array_genpop) = list(names_phycnt, names_time);
max_eigenval_cntmat_array_genpop = array(NA, dim=c(3, num_time)); # for all and phy/nonphy contacts 
dimnames(max_eigenval_cntmat_array_genpop) = list(names_phycnt, names_time)


# socialmixr for overall
d17_socialmixr_0 <- survey(participants = d17_part, contacts = d17_contact)
chk_socialmixr_byphase = rep(list(NULL), length=num_date_byphase)
for (iphase in 1:num_date_byphase){
	irow_TT = (chk_part_base$date >= date_byphase[iphase] & chk_part_base$date < date_byphase[iphase+1] & chk_part_base$part_specialgroups %in% 997 ); # among participants not in the special groups 
	chk_part_TT = chk_part_base[irow_TT,];
	chk_contact_TT = chk_contact_base[chk_contact_base$part_id %in% chk_part_TT$part_id,]
	chk_socialmixr_byphase[[iphase]] <- survey(participants = chk_part_TT, contacts = chk_contact_TT)
} # for-itime


# contact matrices by location and type (phy / nonphy)
for (itime in 1:num_time){

	if (itime %in% 1:4){
		TT_socialmixr = chk_socialmixr_byphase[[itime]]
	} else if (itime==5){
		TT_socialmixr = d17_socialmixr_0
	}
	for (iloc in 1:num_loc){
		cntmat_loc_array_genpop[['all', itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN);
		cntmat_loc_array_genpop[['Home', itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_home = 1));
		cntmat_loc_array_genpop[['School', itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_school = 1));
		cntmat_loc_array_genpop[['Work', itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_work = 1));
		cntmat_loc_array_genpop[['Others', itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(cnt_others = 1));
	} # for- iloc
	
	
	for (iphycnt in 1:num_phycnt){
		cntmat_phy_array_genpop[['all', itime]] = cntmat_loc_array_genpop[['all', itime]];
		cntmat_phy_array_genpop[['phycnt', itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(phys_contact = 1));
		cntmat_phy_array_genpop[['nonphycnt', itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(phys_contact = 2));
	} # for- iphycnt

} # for- itime


# dominant eigenvalues
for (itime in 1:num_time){
	for (iphycnt in 1:num_phycnt){
		m_contacts_TT = cntmat_phy_array_genpop[[iphycnt, itime]]$matrix
		m_contacts_TT[is.na(m_contacts_TT)] = 0;
		if (any(m_contacts_TT>uppbound_cnt_agegp)){
			browser()
			m_contacts_TT[m_contacts_TT>uppbound_cnt_agegp] = uppbound_cnt_agegp;
		}
		eigenval_TT = eigen(m_contacts_TT)$values;
		eigenval_TT = as.numeric(eigenval_TT[Im(eigenval_TT)==0])
		max_eigenval_cntmat_array_genpop[iphycnt, itime] = max(eigenval_TT);
	}
} # for- itime

