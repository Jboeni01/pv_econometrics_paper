##################################
# Estimation Final 

#-1- Load packages 
rm(list = ls())

#remove.packages("rlang")
#install.packages("rlang")#, type="source")

library(readxl)
library(tidyr)
library(tidyverse)
library(dbplyr)
library(dplyr) #arrange 
library(rlang)
library(ggthemes)
library(ggplot2)
library(xtable) #print descriptive tables 
library(psych) #descriptive statistics 
library(fixest) #fixed-effects panel data estimation 
library(car)
library(boot)

#install.packages("dotwhisker")
library(dotwhisker)

################################################################################
#-2- set wd and load data 
################################################################################
setwd("C:/Users/u0149894/Desktop/Code")

outputpath_plots <- paste0(getwd(),"/data/plots/")
outputpath_regtables <- paste0(getwd(),"/data/tables/")

load(paste0(getwd(),"/data/PV_total_estdata_monthly.Rdata")) #full data monthly 



################################################################################
#-3- define functions 
################################################################################

#-3.1- Calculate different benefits used in the regressions

#aggregate benefits
benvars_aggregate <- function(newvar,aggvars,data, monthly=F){ #aggregate over the different benefits
  if(monthly==F){
    aggtypes <- c("","_w8","_defl","_defl_w8")
  }else{
    aggtypes <- c("","_defl")
  }
  for(i in aggtypes){
    newv <- paste0(newvar,i)
    aggv <- paste0(aggvars,i)
    data <- data %>% mutate(!!newv := rowSums(select(.,all_of(aggv))))
  }
  data
}

#transform benefits
benvars_transform <- function(transvars,data,operation,monthly=F,capsize=1){ #transform data
  if(monthly==F){
    transvars_full <- c(transvars,paste0(transvars,"_defl"),
                        paste0(transvars,"_w8"),paste0(transvars,"_defl_w8"))    
  }else{
    transvars_full <- c(transvars,paste0(transvars,"_defl"))   
  }
  
  
  if(operation=="log"){
    data <- data %>% 
      mutate(across(all_of(transvars_full),
                    ~ log(ifelse(.x==0,1,.x)), .names="{.col}_log")) #take the log of benefits, add 1 to zero values 
  }
  if(operation=="/1000"){
    data <- data %>% 
      mutate(across(all_of(transvars_full),
                    ~ .x/1000, .names="{.col}_thous")) #take the log of benefits, add 1 to zero values 
  }
  if(operation=="*(-1)"){
    data <- data %>% 
      mutate(across(all_of(transvars_full),
                    ~ -1*.x,
                    .names="{substr(.col,1,nchar(transvars[1]))}_pos{substr(.col,(nchar(transvars[1])+1),nchar(.col))}")) #  
  }
  if(operation=="/capsize"){
    data <- data %>% 
      mutate(across(all_of(transvars_full), ~ .x/capsize))
  }
  data
}

#calculate benefit shares 
benvars_shares <- function(aggvars,denom, data, monthly=F) { #calculate shares 
  if(monthly==F){
    aggtypes <- c("","_w8","_defl","_defl_w8")
  }else{
    aggtypes <- c("","_defl")
  }
  for(i in aggtypes){
    aggv <- paste0(aggvars,i)
    data <- data %>% mutate(across(all_of(aggv), ~.x/!!sym(denom),.names="{.col}_sh"))
  }
  data
}

#-3.2- IV control-function approach functions 

#-3.2.1 - define bootstrapping function used for standard error calculation

bootstrap_IV2st <- function(data, indices, eq_2stage) {
  bootstrap_sample <- data[indices, ] #indices passed on by bootstrap function
  bootstrap_sample <- bootstrap_sample %>% unnest(cols = c(data)) #unnest previously nested data
  
  bootstrap_sample <- bootstrap_sample %>%
    mutate(newid = 1) %>%
    group_by(zip,date) %>% mutate(newid = cumsum(newid)) %>%
    mutate(zip = paste(zip,newid,sep=".")) #generate new zip group 
  #print(length(unique(bootstrap_sample$zip)))
  
  #print(dim(bootstrap_sample))
  #print(eq_2stage)
  
  regres_bootstrap <- feglm(eq_2stage,
                            data = bootstrap_sample,
                            family = quasipoisson,
                            cluster="zip") #rerun regression 
  print(regres_bootstrap$fixef_size)
  print(dim(bootstrap_sample))
  return(coef(regres_bootstrap))
  
  #return(replicated_data)
}


#-3.2.2 - Control function approach

regress_IV <- function(data,eq_2stage_lname, eq_1stage_lname, boot_iterations=50){
  
  
  regname_2stage <- eq_2stage_lname %>% substr(4, nchar(.)) #2nd stage reg name
  regname_1stage <- eq_1stage_lname %>% substr(4, nchar(.)) #1st stage reg name
  
  #1st stage regression
  eq_1stage <- eq_list_monthly_IV1st[[eq_1stage_lname]] #retrieve previously defined equation
  regres_1stage <- feols(eq_1stage, data=data) #run 1st stage regression
  
  data.IV <- data %>%
    mutate(res_1stage = NA_real_) #add column  
  
  data.IV$res_1stage[regres_1stage$obs_selection$obsRemoved] <- regres_1stage$residuals #fill column with 1st stage residuals
  
  data.IV <- data.IV%>%
    mutate(popsize_age_above64_sh = 1,
           popsize_nat_nbel_sh = 1,
           popsize_women_sh = 1,
           popsize_living_couplenochild_sh = 1,
           housing_singfam_open_sh = 1) #make sure to drop final share 
  
  #2nd stage regression 
  eq_2stage <- eq_list_monthly[[eq_2stage_lname]] #retrieve previously defined equation
  regres_2stage <- feglm(eq_2stage, data=data.IV, family=quasipoisson, cluster="zip") #run 2nd stage regression
  
  #Bootstrapping respecting municipality clustering for standard errors 
  D <- data.IV %>% nest(-zip) #one row for each zip code, ensures full municipality clustering
  
  set.seed(1) 
  
  boot_results_groups <- boot(data = D,
                              statistic = bootstrap_IV2st, #call function that runs 2nd stage on bootstrap
                              R = boot_iterations,
                              eq_2stage = eq_2stage)
  
  #Calculate the standard errors (standard deviation across coefficients), based on bootstrapping
  boot_coefficients <- boot_results_groups$t
  standard_errors <- apply(boot_coefficients, 2, sd) #calculate standard dev of coeff across bootstrap samples
  
  #Calculate the vcov matrix 
  coeff_mean <- colMeans(boot_coefficients) #coeff mean across bootstrap samples 
  coeff_mean_matrix <- matrix(rep(coeff_mean, each = nrow(boot_coefficients)), 
                        nrow = nrow(boot_coefficients), byrow = F) #fill mean to matrix (N_iter x k)
  
  coeff_err <- boot_coefficients - coeff_mean_matrix #V: subtract coeff from coeff mean

  coeff_varcov <- 1/(boot_iterations-1)*t(coeff_err)%*%coeff_err # 1/(N_iter-1)* V'V
  
  #Save results in the model
  regres_2stage[["bootstr"]] = list(coef= boot_coefficients, stderr= standard_errors, vcov = coeff_varcov)

  regression_output <- setNames( #capture both models in a list
    list(regres_1stage, regres_2stage),
    c(regname_1stage, regname_2stage)
  )
  
  return(regression_output)
  
}

#-3.3- Display final results 

#-3.3.1- Add rows to final table 
etable_yrange = function(x) paste(attributes(x$fixef_id$year)[[1]][c(1,length(attributes(x$fixef_id$year)[[1]]))],collapse="-")
extralines_register("yrange", etable_yrange, "-_Year Range")

etable_id = function(x) paste("",x$fixef_sizes["zip"][[1]])
extralines_register("ids", etable_id, "-_Nr of ID")

etable_loglik = function(x) paste("",format(x$loglik,digits=2))
extralines_register("loglik", etable_loglik, "_Log likelihood")

#-3.3.2 - Add omitted variables as rows to regression
etab_omitted <- function(model){
  allvars <- data.frame(exp_var = labels(terms(model$fml_all$linear)), #initial variable in the model + position
                        position=1:length(labels(terms(model$fml_all$linear))))
  
  coefftab <- data.frame(model$coeftable)
  coefftab$exp_var <- rownames(coefftab)
  
  coefftab_ommadd <- left_join(allvars,coefftab,by="exp_var")
  coefftab_ommadd_mat <- as.matrix(coefftab_ommadd[,3:length(coefftab_ommadd)]) 
  
  rownames(coefftab_ommadd_mat) <- coefftab_ommadd[,1]
  
  model$coeftable <- coefftab_ommadd_mat
  
  model
}

#-3.3.3 - Save tables as latex output
save_table_benefits <- function(model_list,model_names,model_ses,save_name,path=paste0(getwd(),"/data/tables/"),group=NULL, headers=NULL, order=NULL){
  table <- etable(model_list[model_names],
                  vcov=model_ses,
                  digits=3,
                  tex=T,
                  headers = headers,
                  order = order,
                  group=group,
                  fontsize = "footnotesize",
                  depvar=F,
                  tabular = "X",
                  #fixef.group = T,
                  fixef_sizes.simplify = F,
                  fixef_sizes = T)
  
  table <- table %>%
    gsub("\\\\emph\\{Variables\\}\\\\\\\\","", .) %>%
    gsub("NA",".",.) %>%
    gsub("-(?=\\d{1,2}\\.)", "$\\\\text{-}$", ., perl = TRUE) %>%
    gsub("\\\\emph\\{Fixed-effects\\\\\\}\\\\\\\\.*\\\\\\\\", "", ., perl = TRUE)
  #%>%  gsub("-[0-9].","$\\text{-}$[0-9].",.)
  
  print(paste0(outputpath_regtables,save_name,".tex"))
  
  capture.output(print(table),
                 file = paste0(outputpath_regtables,save_name,".tex"))
}

################################################################################
# -4- Label Data 
################################################################################

#-4.1- All benefit variables with labels in one vector
benefitvars_labeled <- c("b_net_solw_defl_log"="Net benefits (log)",
                         "b_net_solw_defl_thous"="Net benefits",
                         "b_gc_solw_defl_thous"="Output-based incentive",
                         "b_nm_solw_defl_thous"="Net metering",
                         "b_protar_pos_defl_thous"="Capacity-based cost",
                         "b_qw_defl_thous"="Capacity-based incentive",
                         "b_protar_pos_subreg_defl_thous"="Capacity-based cost",
                         "b_qw_subreg_defl_thous"="Capacity-based incentive",
                         "b_nm_mix_retail_solw_defl_thous"="Distr. tariff-free net metering (instrument)"  
                         )
#-4.2- All control variables with labels in one vector
controlvars_labeled <- 
  c("hh_size_log"="Household size (log)",
    "popsize_dens_log"="Population density (log)",
    "popsize_age_0_17_sh"="Age:below 18 (sh.)",
    "popsize_age_18_49_sh"="Age:18-49 (sh.)",
    "popsize_age_50_64_sh"="Age:50-64 (sh.)",
    "popsize_age_above64_sh"="Age:above 64 (sh.)",
    "popsize_nat_bel_sh"="Nationals (sh.)",
    "popsize_nat_nbel_sh"="Non-nationals (sh.)",
    "popsize_men_sh"="Male (sh.)",
    "popsize_women_sh"="Female (sh.)",
    "income_perdecl_med_defl_log"="Net median income (log)", #"income_perdecl_med_defl_log"
    "popsize_living_alone_sh"="Hh single (sh.)",
    "popsize_living_singlepar_sh"="Hh single parent (sh.)",
    "popsize_living_couplechild_sh"="Hh couple /w children (sh.)",
    "popsize_living_couplenochild_sh"="Hh couple w/o children (sh.)",
    "buildings_until1981_sh"="House age:until 1981 (sh.)",
    "buildings_after1981_sh"="House age:after 1981 (sh.)",
    "housing_apartments_sh"="House type:apartments (sh.)",
    "housing_singfam_closed_sh"="House type:single fam closed (sh.)",
    "housing_singfam_semidetached_sh"="House type:single fam semi-detached (sh.)",
    "housing_singfam_open_sh"="House type:single fam open (sh.)")


estvars_labeled <- c(benefitvars_labeled,controlvars_labeled) #all exp variales


#-4.3- Define ordering of variables 
etable_order <- c("^Net benefits [(]log[)]","^Net benefits",
                  "^Output-based incentive","^Net metering",
                  "^Capacity-based cost","^Capacity-based incentive") 
etable_extralines <- list("-_Nr of ID"=~ids,"-_Year Range"=~yrange) #list("-_Nr of ID"=~ids,"-_Year Range"=~yrange,"_Log likelihood"=~loglik)
etable_group_control <- list("^_\\midrule \\emph{Controls:} "=c("income","Household", "Population", "Age",
                                                                "Female","Male","Nationals","Hh","House","res"))


################################################################################
# -5- Define Sample 
################################################################################

#-5.1- Define dependent variables 
depvars <- c("pv_count","pv_meansize","pv_capkw")
depvars_labdict <- c(pv_count = "Nr. of PV installations (yearly)",
                     pv_meansize="average capacity per installation",
                     pv_capkw="Added capacity in kW")


#-5.2 - Define sample 
controlvars <- names(controlvars_labeled)

#Adoption & benefit data  
pv_cvar <- grep("c_[1-9]kw$", colnames(estimate.data.monthly), value=T)
pv_cvar_defl <- grep("c_[1-9]kw_defl$", colnames(estimate.data.monthly), value=T)
capsize <- gsub("[^1-9]","",pv_cvar) %>% as.numeric()

#grep("^b_",colnames(estimate.data.monthly),value=T)
#grep("^npv",colnames(estimate.data.monthly),value=T)

npv_vars <- c(pv_cvar,pv_cvar_defl,
              "npv","npv_defl","npv_solw","npv_solw_defl", #npv_subreg, "npv_subreg_solw",
              grep("^b_",colnames(estimate.data.monthly),value=T)) #%>%c(.,paste0(.,"_defl"))

benefit_vars <- grep("npv|^c_",npv_vars,invert=T,value=T)

#Estimation data set
estimate.data.monthly.final <- estimate.data.monthly %>% 
  subset(year%in%2008:2019,
         select=c("zip","year","month","date","region","dso",
                  depvars,
                  "prebunch_flag","postbunch_flag",
                  npv_vars,
                  controlvars)) %>%
  mutate(across(all_of(npv_vars), ~ .x/capsize))

#-5.3- Calculate additional variables 
#net benefits -baseline 3 percent DR

#net benefits - aggregate across incentive schemes with future financial benefits

#regional aggregates
estimate.data.monthly.final <- benvars_aggregate("b_net",c("b_gc","b_nm","b_protar","b_qw"), 
                                                 estimate.data.monthly.final, monthly=T) 
#sub-regional aggregates
estimate.data.monthly.final <- benvars_aggregate("b_net_subreg",c("b_gc","b_nm_subreg","b_protar_subreg","b_qw_subreg"), #"b_tc",
                                                 estimate.data.monthly.final, monthly=T) 
#regional aggregates with adjustment of GCs in Wallonia
estimate.data.monthly.final <- benvars_aggregate("b_net_solw",c("b_gc_solw","b_nm_solw","b_protar","b_qw"), #"b_tc",
                                                 estimate.data.monthly.final, monthly=T)
#sub-regional aggregates with adjustment of GCs in Wallonia
estimate.data.monthly.final <- benvars_aggregate("b_net_subreg_solw",c("b_gc_solw","b_nm_subreg_solw","b_protar_subreg","b_qw_subreg"), #"b_tc",
                                                 estimate.data.monthly.final, monthly=T)

#Repeat aggregation procedure for different discount rates (used in robustness section)
for(r in c(0,7,15)){
  benvars_agg <- paste0(c("b_gc","b_nm","b_protar","b_qw"),r) #define variables
  benvars_agg_subreg <- benvars_agg %>% paste0("_subreg") %>% gsub("(gc[0-9]{1,2})_subreg", "\\1", .) 
  benvars_agg_solw <- benvars_agg %>% gsub("(b_(gc|nm)[0-9]{1,2})", "\\1_solw",.)
  benvars_agg_subreg_solw <- benvars_agg_subreg %>% gsub("(gc[0-9]{1,2})|(nm[0-9]{1,2}_subreg)", "\\1\\2_solw",.)
  
  #repeat aggregation with different DRs
  estimate.data.monthly.final <- benvars_aggregate(paste0("b_net",r),benvars_agg, #"b_tc",
                                                   estimate.data.monthly.final, monthly=T) 
  estimate.data.monthly.final <- benvars_aggregate(paste0("b_net",r,"_subreg"),benvars_agg_subreg, #"b_tc",
                                                   estimate.data.monthly.final, monthly=T) 

  estimate.data.monthly.final <- benvars_aggregate(paste0("b_net",r,"_solw"),benvars_agg_solw, #"b_tc",
                                                   estimate.data.monthly.final, monthly=T)
  estimate.data.monthly.final <- benvars_aggregate(paste0("b_net",r,"_subreg_solw"),benvars_agg_subreg_solw, #"b_tc",
                                                   estimate.data.monthly.final, monthly=T)
}


#make sure that missing dsos are labelled as missing and not as zero
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(grep("b_net",colnames(.),value=T)),~if_else(is.na(dso)&.x==0,NA_real_,.x)))

#add new variables to vector of benefit and npv varnames 
benefit_vars <- grep("^b_",colnames(estimate.data.monthly.final), value=T)
npv_vars <- grep("^npv_",colnames(estimate.data.monthly.final), value=T) %>%
  c(.,benefit_vars)

#in Thousand EUR
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(npv_vars), ~.x/1000, .names = "{.col}_thous" ))


#in log 
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(grep("^b_net",colnames(.),value=T)), ~ log(.x), .names = "{.col}_log" ))

#prosumer tariff as positive benefit (more intuitive for results)
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(grep("^b_protar_",colnames(.),value=T)), ~(-1)*.x, 
                .names="{substr(.col,1,8)}_pos{substr(.col,9,nchar(.col))}"))

#prosumer tariff as positive benefits for discount rates 
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(grep("^b_protar[0-9]",colnames(.),value=T)), ~(-1)*.x
                ,.names="{.col}_pos")) %>%
  rename_with(
    ~ gsub("^b_protar([0-9]{1,2})(.*?)_pos", "b_protar\\1_pos\\2", .x),
    starts_with("b_protar")
  )


################################################################################
# -6- Set up regression formulas
################################################################################

#set up right hand side 

rhs_list <- list() 

rhs_list["rhs1"] <- "b_net_solw_defl_log"

rhs_list["rhs2"] <- paste("b_net_solw_defl_log",
                          paste(controlvars,collapse="+"),sep="+") 

rhs_list["rhs3"] <- paste("b_net_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

rhs_list["rhs4"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

rhs_list["rhsIV1.2st"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+res_1stage",
                          paste(controlvars,collapse="+"),sep="+")


# Loop through rhs_list and create equations

eq_list_monthly <- list() #list placeholder 
depvars <- c("pv_count","pv_meansize") 

fixed_effects_monthly <- "|year+date+zip"
time_trends <- "zip[year]"


for(depvar in c("pv_count","pv_meansize")){ #loop through dependent variables 
  for(rhs_name in names(rhs_list)) {
    regnr <- gsub("rhs","",rhs_name) #capture regression number 
    rhs_formula <- as.formula(paste(depvar, "~", rhs_list[[rhs_name]], fixed_effects_monthly)) #set up formula
    eq_list_monthly[[paste0("eq.pfe.m", regnr,".",depvar)]] <- rhs_formula #store formula in list 
  }
  
}


#Set up IV 1 Stage - IV Control function 
eq_list_monthly_IV1st <- list()

eq_list_monthly_IV1st[["eq.pfe.mIV1.1st"]] <- as.formula(
  paste0("b_nm_solw_defl_thous~b_nm_mix_retail_solw_defl_thous+b_gc_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+",
        paste(controlvars,collapse="+"),
        fixed_effects_monthly))


################################################################################
# -7- Run regressions - baseline   
################################################################################

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax

estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag<=1 & postbunch_flag<=1 )

pdat.monthly = panel(estimate.data.monthly.final.ysset, ~zip+date)

estimate.data.monthly.final.ysset %>%
  select(c(b_net_defl_log, b_net_thous, b_gc_defl_thous, b_qw_defl_thous, b_protar_defl_thous, b_nm_defl_thous)) %>%
  describe()

estimate.data.monthly.final.ysset %>%
  group_by(zip,year) %>%
  summarise(across(all_of(controlvars), mean)) %>%
  describe()

#regular regressions 
eq_monthly <- paste0("eq.pfe.m",c(1,2,3,4)) #12,16,8,9
regress_list_monthly = list()

for(depvar in depvars){
  for(n in eq_monthly){
    eq <- paste(n,depvar,sep=".")
    regname <- substr(eq,4,nchar(eq))
    print(regname)
    regress_list_monthly[[regname]] <- feglm(eq_list_monthly[[eq]],
                                            data=pdat.monthly,#data=estimate.data.final.ysset,
                                            family=quasipoisson, cluster=c("zip"))
  }
}

regress_list_monthly[["pfe.m3.pv_count"]]

summary(regress_list_monthly[["pfe.m4.pv_count"]])

linearHypothesis(regress_list_monthly[["pfe.m4.pv_count"]], c("b_qw_defl_thous=b_gc_solw_defl_thous"))
linearHypothesis(regress_list_monthly[["pfe.m4.pv_count"]], c("b_nm_solw_defl_thous=b_qw_defl_thous"))
#IV regressions 
#run IV regression 

iterations <- 100

regress_list_monthly_IV <- c(regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV1.2st.pv_meansize",
                                        eq_1stage_lname = "eq.pfe.mIV1.1st",
                                        boot_iterations = iterations),
                             regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV1.2st.pv_count",
                                        eq_1stage_lname = "eq.pfe.mIV1.1st",
                                        boot_iterations = iterations))


regress_list_monthly_full <- c(regress_list_monthly,regress_list_monthly_IV)



#Add omitted columns to display later in regression 
names(regress_list_monthly_full)

regress_omitted_monthly_list <- list()
expvars_omitted_monthly_list <- list()

for(i in names(regress_list_monthly_full)){
  regress_omitted_monthly_list[[i]] <- etab_omitted(regress_list_monthly_full[[i]])
  expvars_omitted_monthly_list[[i]] <- rownames(regress_omitted_monthly_list[[i]]$coeftable)
}

#etable(regres_list_monthly[["pfe.mIV1.2st"]], vcov = diag(regres_list_monthly[["pfe.mIV1.2st"]][["bootstr"]][["stderr"]]^2))


names(regress_omitted_monthly_list)
summary(regress_omitted_monthly_list[["pfe.m4.pv_count"]])

#do some hypothesis testing on equality of coefficients
linearHypothesis(regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]], c("b_qw_subreg_defl_thous=b_nm_solw_defl_thous"),
                 vcov.=regress_omitted_monthly_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["vcov"]])

linearHypothesis(regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]], c("b_qw_subreg_defl_thous=b_gc_solw_defl_thous"),
                 vcov.=regress_omitted_monthly_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["vcov"]])

linearHypothesis(regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]], c("1.20=b_gc_solw_defl_thous"),
                 vcov.=regress_omitted_monthly_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["vcov"]])

linearHypothesis(regress_list_monthly_full[["pfe.m4.pv_count"]], c("b_qw_defl_thous=b_gc_solw_defl_thous"))


################################################################################
# -8- Print results - baseline -Table 2 & Table 3, Table A3 
################################################################################

setFixest_dict(estvars_labeled) #attach labels to estimation sample
etable_headers <- list("^ " = list("Aggregate benefits" = 3, "Sep. benefits"=1, "Sep. benefits (Poisson CF)"=1))
etable_headers_full <- list("^ " = list("Aggregate benefits" = 3, "Sep. benefits"=1, "Sep. benefits (Poisson CF)"=1,"First stage"=1))

# Regression table number of PV installations 

pv_count_baseline <- c("pfe.m1.pv_count","pfe.m2.pv_count","pfe.m3.pv_count","pfe.m4.pv_count","pfe.mIV1.2st.pv_count")
pv_count_baseline_se <- list(~zip,~zip,~zip,~zip,diag(regress_omitted_monthly_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["stderr"]]^2))

print("Table 2")
etable(regress_omitted_monthly_list[pv_count_baseline],
       vcov=pv_count_baseline_se,
       digits=3,
       extralines = etable_extralines,
       headers = etable_headers,
       tex=F,
       order = etable_order,
       group=etable_group_control)

#Regression table aver installed capacity 

pv_meansize_baseline <- c("pfe.m1.pv_meansize","pfe.m2.pv_meansize","pfe.m3.pv_meansize","pfe.m4.pv_meansize","pfe.mIV1.2st.pv_meansize")
pv_meansize_baseline_se <- list(~zip,~zip,~zip,~zip,diag(regress_omitted_monthly_list[["pfe.mIV1.2st.pv_meansize"]][["bootstr"]][["stderr"]]^2))

print("Table 3")
etable(regress_omitted_monthly_list[pv_meansize_baseline],
       vcov=pv_count_baseline_se,
       digits=3,
       extralines = etable_extralines,
       headers = etable_headers,
       tex=F,
       order = etable_order,
       group=etable_group_control)

#####
#-8.1.- Extensiton Create bar plot for presentations
#####
check <- regress_omitted_monthly_list[pv_count_baseline][[5]]


confint(check, vcov = check$bootstr$vcov )

barplot_df = function(model, conf_level = 0.95, boostrse=F){
  reg_model <- model
  if(boostrse == T){
    est_conf <- confint(reg_model, level = conf_level, vcoc = reg_model$bootstr$vcov)  
  }else{
    est_conf <- confint(reg_model, level = conf_level)
  }
  
  est_df <- data.frame(term = rownames(reg_model$coeftable),
                       estimate = reg_model$coeftable[,1],
                       conf.low = est_conf[,1],
                       conf.high = est_conf[,2])
  return(est_df)
}

barplot_list = list("Baseline" = 
                      barplot_df(regress_omitted_monthly_list[pv_count_baseline][[4]],boostrse=F) %>%
                      mutate(model="Baseline") %>%
                      subset(term%in%c("b_gc_solw_defl_thous","b_qw_defl_thous",
                                       "b_protar_pos_defl_thous","b_nm_solw_defl_thous")),
                    "Control Function" = 
                      barplot_df(regress_omitted_monthly_list[pv_count_baseline][[5]], boostrse=T) %>%
                      mutate(model="Control Function", term = gsub("subreg_","",term)) %>%
                      subset(term%in%c("b_gc_solw_defl_thous","b_qw_defl_thous",
                                       "b_protar_pos_defl_thous","b_nm_solw_defl_thous")))

# Combine the two data frames
barplot_df <- reduce(barplot_list, rbind)

barplot_pres <- barplot_df %>%
  relabel_predictors(b_gc_solw_defl_thous = "Output-based", 
                     b_qw_defl_thous = "Capacity-based", 
                     b_protar_pos_defl_thous = "Capacity-based cost",
                     b_nm_solw_defl_thous = "Net metering") %>%
  dwplot(dot_args = list(size = 2),
         whisker_args = list(size = 1),
         model_order = c("Baseline","Control Function"),
         dodge_size = 0.2) +
  geom_vline(xintercept = 0, colour = "grey") +
  theme_bw() +
  theme(axis.text.y = element_text(margin = margin(0, 0, 0, 0, "cm"), size=12),
        ) + 
  #ggtitle("The Effectiveness on PV installations") +
  xlab("Coefficient Estimate") +
  theme(
    plot.title = element_text(face = "bold", hjust=0.5),
    legend.position = c(0.007, 0.01),
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5,
    #legend.name = "Model Specification"
  ) +
  scale_colour_grey(
    start = .3,
    end = .7,
    name = "Model Specification",
    #breaks = c(0, 1),
    #labels = c("Baseline", "Control Function")
  )

print(barplot_pres)  
#ggsave("barplot_pres.pdf",barplot_pres, path =outputpath_plots,  height=4, width=8.75)


names(regress_omitted_monthly_list)

pv_count_baseline_full <- c("pfe.m1.pv_count","pfe.m2.pv_count","pfe.m3.pv_count","pfe.m4.pv_count","pfe.mIV1.2st.pv_count","pfe.mIV1.1st")
pv_count_baseline_full_se <- list(~zip,~zip,~zip,~zip,diag(regress_omitted_monthly_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["stderr"]]^2),~zip)

pv_meansize_baseline_full <- c("pfe.m1.pv_meansize","pfe.m2.pv_meansize","pfe.m3.pv_meansize","pfe.m4.pv_meansize","pfe.mIV1.2st.pv_meansize","pfe.mIV1.1st")
pv_meansize_baseline_full_se <- list(~zip,~zip,~zip,~zip,diag(regress_omitted_monthly_list[["pfe.mIV1.2st.pv_meansize"]][["bootstr"]][["stderr"]]^2),~zip)


print("Table A3")
etable(regress_omitted_monthly_list[pv_count_baseline_full],
       vcov=pv_count_baseline_full_se,
       digits=3,
       extralines = etable_extralines,
       headers = etable_headers_full,
       tex=F,
       order = c(etable_order,"^res","^distr."))

print("Table A3")
etable(regress_omitted_monthly_list[pv_meansize_baseline_full],
       vcov=pv_meansize_baseline_full_se,
       digits=3,
       extralines = etable_extralines,
       headers = etable_headers_full,
       tex=F,
       order = c(etable_order,"^res","^distr."))

################################################################################
# -9- Save tables - baseline - commented out 
################################################################################


#save_table_benefits(model_list=regress_omitted_monthly_list,
#                    model_names=pv_meansize_baseline,
#                    model_ses=pv_meansize_baseline_se,
#                    save_name="baseline_pv_meansize_benefits_reg",
#                    group=etable_group_control,
#                    headers=etable_headers,
#                    order=etable_order)


#save_table_benefits(model_list=regress_omitted_monthly_list,
#                    model_names=pv_count_baseline,
#                    model_ses=pv_count_baseline_se,
#                    save_name="baseline_pv_count_benefits_reg",
#                    group=etable_group_control,
#                    headers=etable_headers,
#                    order=etable_order)

#save_table_benefits(model_list=regress_omitted_monthly_list,
#                    model_names=pv_meansize_baseline_full,
#                    model_ses=pv_meansize_baseline_full_se,
#                    save_name="baseline_pv_meansize_full_reg",
#                    headers=etable_headers_full,
#                    order=c(etable_order,"^res","^Distr."))

#save_table_benefits(model_list=regress_omitted_monthly_list,
#                    model_names=pv_count_baseline_full,
#                    model_ses=pv_count_baseline_full_se,
#                    save_name="baseline_pv_count_full_reg",
#                    headers=etable_headers_full,
#                    order=c(etable_order,"^res","^Distr."))

#some Hyothesis testing 

linearHypothesis(regress_list_monthly[["pfe.m4.pv_meansize"]], c("b_qw_defl_thous=b_nm_solw_defl_thous"))
linearHypothesis(regress_list_monthly[["pfe.m4.pv_meansize"]], c("0.312=b_gc_solw_defl_thous"))
linearHypothesis(regress_list_monthly[["pfe.m4.pv_meansize"]], c("b_protar_pos_defl_thous=0.429"))



#
################################################################################
# -11- Extension: control for short-term dynamics, 
# -11.1- run regressions
################################################################################
tmin <- 2008
tmax <- 2019

trange <- tmin:tmax


estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag==0 & postbunch_flag==0 )
#%>%
#mutate(prebunch_flag = if_else(prebunch_flag==1,NA_real_,prebunch_flag)) %>%# & region=="Flanders")
#mutate(postbunch_flag = if_else(postbunch_flag==1,NA_real_,postbunch_flag))# & region=="Flanders")

#check <- estimate.data.monthly.final.ysset %>% subset(select=c(zip,year,b_gc_solw_thous,b_qw_subreg_thous,b_protar_subreg_thous,b_certain_solw_thous,b_nm_subreg_solw,b_uncertain_solw_thous))

pdat.monthly.bunching = panel(estimate.data.monthly.final.ysset, ~zip+date)

#regular regressions 
eq_monthly <- paste0("eq.pfe.m",c(3,4)) #12,16,8,9
regress_list_monthly_bunching = list()

for(depvar in depvars){
  for(n in eq_monthly){
    eq <- paste(n,depvar,sep=".")
    regname <- substr(eq,4,nchar(eq))
    print(regname)
    regress_list_monthly_bunching[[regname]] <- feglm(eq_list_monthly[[eq]],
                                             data=pdat.monthly.bunching,#data=estimate.data.final.ysset,
                                             family=quasipoisson, cluster="zip")
  }
}
iterations <- 100

regress_list_monthly_IV_bunching <- c(regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV1.2st.pv_meansize",
                                        eq_1stage_lname = "eq.pfe.mIV1.1st",
                                        boot_iterations = iterations),
                                      regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV1.2st.pv_count",
                                        eq_1stage_lname = "eq.pfe.mIV1.1st",
                                        boot_iterations = iterations))


regress_list_monthly_full_bunching <- c(regress_list_monthly_bunching,regress_list_monthly_IV_bunching)


#Add omitted columns to display later in regression 
names(regress_list_monthly_full_bunching)
regress_omitted_monthly_bunching_list <- list()
expvars_omitted_monthly_bunching_list <- list()

for(i in names(regress_list_monthly_full_bunching)){
  regress_omitted_monthly_bunching_list[[i]] <- etab_omitted(regress_list_monthly_full_bunching[[i]])
  expvars_omitted_monthly_bunching_list[[i]] <- rownames(regress_omitted_monthly_bunching_list[[i]]$coeftable)
}

#test for equality of coefficients:
linearHypothesis(regress_list_monthly_full_bunching[["pfe.mIV1.2st.pv_count"]], c("b_qw_subreg_defl_thous=b_nm_solw_defl_thous"))
linearHypothesis(regress_list_monthly_full_bunching[["pfe.mIV1.2st.pv_count"]], c("b_qw_subreg_defl_thous=b_gc_solw_defl_thous"))


################################################################################
# -11.2- print & save tables - Table 4
################################################################################

#pv_bunching <- c("pfe.m3.pv_count","pfe.m4.pv_count","pfe.mIV1.2st.pv_count","pfe.m3.pv_meansize","pfe.m4.pv_meansize","pfe.mIV1.2st.pv_meansize")
#pv_bunching_se <- list(~zip,~zip,diag(regress_omitted_monthly_bunching_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["stderr"]]^2),
#                             ~zip,~zip,diag(regress_omitted_monthly_bunching_list[["pfe.mIV1.2st.pv_meansize"]][["bootstr"]][["stderr"]]^2))

pv_bunching <- c("pfe.m4.pv_count","pfe.mIV1.2st.pv_count","pfe.m4.pv_meansize","pfe.mIV1.2st.pv_meansize")

regress_shortterm_table_list <- regress_omitted_monthly_bunching_list[pv_bunching]
names(regress_shortterm_table_list) <- paste0(names(regress_shortterm_table_list),".bunching")
  
regress_shortterm_table_list <- c(regress_shortterm_table_list,regress_omitted_monthly_list[pv_bunching])

pv_bunching_se <- list(~zip,diag(regress_shortterm_table_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["stderr"]]^2),
                       ~zip,diag(regress_shortterm_table_list[["pfe.mIV1.2st.pv_count.bunching"]][["bootstr"]][["stderr"]]^2),
                       ~zip,diag(regress_shortterm_table_list[["pfe.mIV1.2st.pv_meansize"]][["bootstr"]][["stderr"]]^2),
                       ~zip,diag(regress_shortterm_table_list[["pfe.mIV1.2st.pv_meansize.bunching"]][["bootstr"]][["stderr"]]^2))


etable_headers_bunching <- list("^ " = list("Number of PV installations" = 4, "Average capacity size"=4),
                                "^ " = list("Baseline" = 2, "Short-term dynamics"=2,
                                            "Baseline" = 2, "Short-term dynamics"=2),
                                "^ " = list("PPMLE"=1, "P-CF"=1,
                                            "PPMLE"=1, "P-CF"=1,
                                            "PPMLE"=1, "P-CF"=1,
                                            "PPMLE"=1, "P-CF"=1))



pv_bunching_table <- c(
  c("pfe.m4.pv_count","pfe.mIV1.2st.pv_count"),
  paste0(c("pfe.m4.pv_count","pfe.mIV1.2st.pv_count"),".bunching"),
  c("pfe.m4.pv_meansize","pfe.mIV1.2st.pv_meansize"),
  paste0(c("pfe.m4.pv_meansize","pfe.mIV1.2st.pv_meansize"),".bunching")
  )

etable(regress_shortterm_table_list[pv_bunching_table],
       vcov=pv_bunching_se,
       digits=3,
       group=etable_group_control,
       extralines = etable_extralines,
       headers = etable_headers_bunching,
       tex=F,
       order = etable_order)

#Save table - commented out 
#save_table_benefits(model_list=regress_shortterm_table_list,
#                    model_names=pv_bunching_table,
#                    model_ses=pv_bunching_se,
#                    save_name="bunching_pv_benefits_reg",
#                    group=etable_group_control,
#                    headers=etable_headers_bunching)


################################################################################
# -12- Extension: different discount rates, 
# -12.1 - set up formulas 
################################################################################


rhs_list <- list() 

rhs_list["rhs1"] <- paste("b_net_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs2"] <- paste("b_net7_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs3"] <- paste("b_net15_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs4"] <- paste("b_net0_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")


rhs_list["rhs5"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs6"] <- paste("b_gc7_solw_defl_thous+b_nm7_solw_defl_thous+b_protar7_pos_defl_thous+b_qw7_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs7"] <- paste("b_gc15_solw_defl_thous+b_nm15_solw_defl_thous+b_protar15_pos_defl_thous+b_qw15_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs8"] <- paste("b_gc0_solw_defl_thous+b_nm0_solw_defl_thous+b_protar0_pos_defl_thous+b_qw0_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")


rhs_list["rhsIV1.2st"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+res_1stage",
                                paste(controlvars,collapse="+"),sep="+")
rhs_list["rhsIV2.2st"] <- paste("b_gc7_solw_defl_thous+b_nm7_solw_defl_thous+b_protar7_pos_subreg_defl_thous+b_qw7_subreg_defl_thous+res_1stage",
                                paste(controlvars,collapse="+"),sep="+")
rhs_list["rhsIV3.2st"] <- paste("b_gc15_solw_defl_thous+b_nm15_solw_defl_thous+b_protar15_pos_subreg_defl_thous+b_qw15_subreg_defl_thous+res_1stage",
                                paste(controlvars,collapse="+"),sep="+")
rhs_list["rhsIV4.2st"] <- paste("b_gc0_solw_defl_thous+b_nm0_solw_defl_thous+b_protar0_pos_subreg_defl_thous+b_qw0_subreg_defl_thous+res_1stage",
                                paste(controlvars,collapse="+"),sep="+")

# Loop through rhs_list and create equations

eq_list_monthly <- list() #list placeholder 
depvars <- c("pv_count","pv_meansize")

fixed_effects_monthly <- "|year+date+zip"
time_trends <- "zip[year]"


for(depvar in c("pv_count","pv_meansize")){ #loop through dependent variables 
  for(rhs_name in names(rhs_list)) {
    regnr <- gsub("rhs","",rhs_name) #capture regression number 
    rhs_formula <- as.formula(paste(depvar, "~", rhs_list[[rhs_name]], fixed_effects_monthly)) #set up formula
    eq_list_monthly[[paste0("eq.pfe.m", regnr,".",depvar)]] <- rhs_formula #store formula in list 
  }
  
}
"eq.pfe.mIV2.2st.pv_count"
names(eq_list_monthly)
names(rhs_list)

#Set up IV 1 Stage 

eq_list_monthly_IV1st <- list()

eq_list_monthly_IV1st[["eq.pfe.mIV1.1st"]] <- as.formula(
  paste0("b_nm_solw_defl_thous~b_nm_mix_retail_solw_defl_thous+b_gc_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+",
         paste(controlvars,collapse="+"),
         fixed_effects_monthly))

eq_list_monthly_IV1st[["eq.pfe.mIV2.1st"]] <- as.formula(
  paste0("b_nm7_solw_defl_thous~b_nm7_mix_retail_solw_defl_thous+b_gc7_solw_defl_thous+b_protar7_pos_subreg_defl_thous+b_qw7_subreg_defl_thous+",
         paste(controlvars,collapse="+"),
         fixed_effects_monthly))

eq_list_monthly_IV1st[["eq.pfe.mIV3.1st"]] <- as.formula(
  paste0("b_nm15_solw_defl_thous~b_nm15_mix_retail_solw_defl_thous+b_gc15_solw_defl_thous+b_protar15_pos_subreg_defl_thous+b_qw15_subreg_defl_thous+",
         paste(controlvars,collapse="+"),
         fixed_effects_monthly))

eq_list_monthly_IV1st[["eq.pfe.mIV4.1st"]] <- as.formula(
  paste0("b_nm0_solw_defl_thous~b_nm0_mix_retail_solw_defl_thous+b_gc0_solw_defl_thous+b_protar0_pos_subreg_defl_thous+b_qw0_subreg_defl_thous+",
         paste(controlvars,collapse="+"),
         fixed_effects_monthly))


################################################################################
# -12.2 - Run regressions Monthly  
################################################################################

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax

#write.csv(estimate.data.monthly.final.ysset,paste0(getwd(),"\\data\\PV_monthly_estdata_stata.csv"), row.names = FALSE)
#write.csv(estimate.data.monthly.final,paste0(getwd(),"\\data\\PV_monthly_estdata_stata.csv"), row.names = FALSE)

estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag<=1 & postbunch_flag<=1 )

pdat.monthly = panel(estimate.data.monthly.final.ysset, ~zip+date)

#regular regressions 
eq_monthly <- paste0("eq.pfe.m",c(1,2,3,4,5,6,7,8)) #12,16,8,9
regress_list_monthly = list()

for(depvar in depvars){
  for(n in eq_monthly){
    eq <- paste(n,depvar,sep=".")
    regname <- substr(eq,4,nchar(eq))
    print(regname)
    regress_list_monthly[[regname]] <- feglm(eq_list_monthly[[eq]],
                                             data=pdat.monthly,#data=estimate.data.final.ysset,
                                             family=quasipoisson, cluster="zip")
  }
}
#IV regressions 

#run IV regression 

iterations <- 100

regress_list_monthly_IV <- c(regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV1.2st.pv_count",
                                        eq_1stage_lname = "eq.pfe.mIV1.1st",
                                        boot_iterations = iterations),
                             regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV2.2st.pv_count",
                                        eq_1stage_lname = "eq.pfe.mIV2.1st",
                                        boot_iterations = iterations),
                             regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV3.2st.pv_count",
                                        eq_1stage_lname = "eq.pfe.mIV3.1st",
                                        boot_iterations = iterations),
                             regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV4.2st.pv_count",
                                        eq_1stage_lname = "eq.pfe.mIV4.1st",
                                        boot_iterations = iterations))


regress_list_monthly_full <- c(regress_list_monthly,regress_list_monthly_IV)


#Add omitted columns to display later in regression 
names(regress_list_monthly_full)

regress_omitted_monthly_list <- list()
expvars_omitted_monthly_list <- list()

for(i in names(regress_list_monthly_full)){
  regress_omitted_monthly_list[[i]] <- etab_omitted(regress_list_monthly_full[[i]])
  expvars_omitted_monthly_list[[i]] <- rownames(regress_omitted_monthly_list[[i]]$coeftable)
}

#etable(regres_list_monthly[["pfe.mIV1.2st"]], vcov = diag(regres_list_monthly[["pfe.mIV1.2st"]][["bootstr"]][["stderr"]]^2))


pv_dr <- c("pfe.m8.pv_count","pfe.m5.pv_count","pfe.m6.pv_count","pfe.m7.pv_count","pfe.mIV4.2st.pv_count","pfe.mIV1.2st.pv_count","pfe.mIV2.2st.pv_count","pfe.mIV3.2st.pv_count")
pv_dr_se <- list(~zip,~zip,~zip,~zip,
                 diag(regress_omitted_monthly_list[["pfe.mIV4.2st.pv_count"]][["bootstr"]][["stderr"]]^2),
                 diag(regress_omitted_monthly_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["stderr"]]^2),
                 diag(regress_omitted_monthly_list[["pfe.mIV2.2st.pv_count"]][["bootstr"]][["stderr"]]^2),
                 diag(regress_omitted_monthly_list[["pfe.mIV3.2st.pv_count"]][["bootstr"]][["stderr"]]^2))

#etable_headers_bunching <- list("^ " = list("Number of PV installations" = 3, "Average new installed capacity"=3),
#                                "^ " = list("Agg. benefits" = 1, "Sep. benefits"=1, "Sep. benefits (IV)"=1,
#                                            "Agg. benefits" = 1, "Sep. benefits"=1, "Sep. benefits (IV)"=1))


################################################################################
# -12.3 - Update labeling, print & save tables - Table 5
################################################################################

benefitvars_labeled <- c("b_net_solw_defl_log"="Net benefits (log)",
                         "b_net_solw_defl_thous"="Net benefits",
                         "b_gc_solw_defl_thous"="Output-based incentive",
                         "b_nm_solw_defl_thous"="Net metering",
                         "b_protar_pos_defl_thous"="Capacity-based cost",
                         "b_qw_defl_thous"="Capacity-based incentive",
                         "b_protar_pos_subreg_defl_thous"="Capacity-based cost",
                         "b_qw_subreg_defl_thous"="Capacity-based incentive")

benefitvars7_labeled <- benefitvars_labeled
names(benefitvars7_labeled) <-  gsub("(net|gc|nm|protar|qw)","\\17",names(benefitvars_labeled))

benefitvars15_labeled <- benefitvars_labeled
names(benefitvars15_labeled) <-  gsub("(net|gc|nm|protar|qw)","\\115",names(benefitvars_labeled))

benefitvars0_labeled <- benefitvars_labeled
names(benefitvars0_labeled) <-  gsub("(net|gc|nm|protar|qw)","\\10",names(benefitvars_labeled))

benefitvars_labeled <- c(benefitvars_labeled,
                         benefitvars7_labeled,
                         benefitvars15_labeled,
                         benefitvars0_labeled)

estvars_labeled <- c(benefitvars_labeled,controlvars_labeled)


setFixest_dict(estvars_labeled) #attach labels to estimation sample

etable_headers_dr <- list("^ " = list("Standard PPMLE" = 4, "IV Controlfunction"=4),
                          "^ " = list("0\\% DR" = 1, "3\\% DR (baseline)" = 1, "7\\% DR"=1, "15\\% DR"=1,
                                      "0\\% DR" = 1, "3\\% DR (baseline)" = 1, "7\\% DR"=1, "15\\% DR"=1))

print("Table 5")
etable(regress_omitted_monthly_list[pv_dr],
       vcov=pv_dr_se,
       digits=3,
       group=etable_group_control,
       extralines = etable_extralines,
       headers = etable_headers_dr,
       tex=F,
       order = etable_order)


etable(regress_omitted_monthly_list[pv_dr],
       vcov=pv_dr_se,
       digits=3,
       #group=etable_group_control,
       extralines = etable_extralines,
       headers = etable_headers_dr,
       tex=F,
       order = etable_order)

#save_table_benefits(model_list=regress_omitted_monthly_list,
#                    model_names=pv_dr,
#                    model_ses=pv_dr_se,
#                    save_name="dr_pvcount_benefits_reg",
#                    group=etable_group_control,
#                    headers=etable_headers_dr)

#Some hypthesis testing 
linearHypothesis(regress_list_monthly[["pfe.m7.pv_count"]], c("b_qw15_defl_thous=b_nm15_solw_defl_thous"))
linearHypothesis(regress_list_monthly[["pfe.m7.pv_count"]], c("b_gc15_solw_defl_thous=b_nm15_solw_defl_thous"))
linearHypothesis(regress_list_monthly[["pfe.m7.pv_count"]], c("b_gc15_solw_defl_thous=b_qw15_defl_thous"))

linearHypothesis(regress_list_monthly_IV[["pfe.mIV3.2st.pv_count"]], c("b_gc15_solw_defl_thous=b_nm15_solw_defl_thous"))
linearHypothesis(regress_list_monthly_IV[["pfe.mIV3.2st.pv_count"]], c("b_qw15_subreg_defl_thous=b_nm15_solw_defl_thous"))
linearHypothesis(regress_list_monthly_IV[["pfe.mIV3.2st.pv_count"]], c("b_qw15_subreg_defl_thous=b_gc15_solw_defl_thous"))


################################################################################
# -13- Extension: different electricity price assumptions, 
# -13.1 - set up formulas 
################################################################################


rhs_list <- list() 

rhs_list["rhs1"] <- paste("b_gc_solw_defl_thous+b_nm_mix_mavg_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs2"] <- paste("b_gc_solw_defl_thous+b_nm_ellt10_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")



rhs_list["rhsIV1.2st"] <- paste("b_gc_solw_defl_thous+b_nm_mix_mavg_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+res_1stage",
                                paste(controlvars,collapse="+"),sep="+")
rhs_list["rhsIV2.2st"] <- paste("b_gc_solw_defl_thous+b_nm_ellt10_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+res_1stage",
                                paste(controlvars,collapse="+"),sep="+")

# Loop through rhs_list and create equations

eq_list_monthly <- list() #list placeholder 
depvars <- c("pv_count","pv_meansize")

fixed_effects_monthly <- "|year+date+zip"
time_trends <- "zip[year]"


for(depvar in c("pv_count","pv_meansize")){ #loop through dependent variables 
  for(rhs_name in names(rhs_list)) {
    regnr <- gsub("rhs","",rhs_name) #capture regression number 
    rhs_formula <- as.formula(paste(depvar, "~", rhs_list[[rhs_name]], fixed_effects_monthly)) #set up formula
    eq_list_monthly[[paste0("eq.pfe.m", regnr,".",depvar)]] <- rhs_formula #store formula in list 
  }
  
}

names(eq_list_monthly)
names(rhs_list)

#Set up IV 1 Stage 

eq_list_monthly_IV1st <- list()

#b_nm_mix_retail_6mavg_solw_thous
#b_nm_mix_retail_solw_defl_thous

eq_list_monthly_IV1st[["eq.pfe.mIV1.1st"]] <- as.formula(
  paste0("b_nm_mix_mavg_solw_defl_thous~b_nm_mix_retail_solw_thous+b_gc_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+",
         paste(controlvars,collapse="+"),
         fixed_effects_monthly))

eq_list_monthly_IV1st[["eq.pfe.mIV2.1st"]] <- as.formula(
  paste0("b_nm_ellt10_solw_defl_thous~b_nm_mix_retail_ellt10_delf_solw_thous+b_gc_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+",
         paste(controlvars,collapse="+"),
         fixed_effects_monthly))

names(eq_list_monthly_IV1st)
################################################################################
# -13.2 - Run regressions Monthly  
################################################################################

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax

#write.csv(estimate.data.monthly.final.ysset,paste0(getwd(),"\\data\\PV_monthly_estdata_stata.csv"), row.names = FALSE)
#write.csv(estimate.data.monthly.final,paste0(getwd(),"\\data\\PV_monthly_estdata_stata.csv"), row.names = FALSE)

estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag<=1 & postbunch_flag<=1 )

pdat.monthly = panel(estimate.data.monthly.final.ysset, ~zip+date)

#regular regressions 
eq_monthly <- paste0("eq.pfe.m",c(1,2)) #12,16,8,9
regress_list_monthly = list()

for(depvar in depvars){
  for(n in eq_monthly){
    eq <- paste(n,depvar,sep=".")
    regname <- substr(eq,4,nchar(eq))
    print(regname)
    regress_list_monthly[[regname]] <- feglm(eq_list_monthly[[eq]],
                                             data=pdat.monthly,#data=estimate.data.final.ysset,
                                             family=quasipoisson, cluster="zip")
  }
}

#IV regressions 
names(regress_list_monthly)
regress_list_monthly[["pfe.m2.pv_count"]]
#run IV regression 

iterations <- 100

regress_list_monthly_IV <- c(regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV1.2st.pv_count",
                                        eq_1stage_lname = "eq.pfe.mIV1.1st",
                                        boot_iterations = iterations),
                             regress_IV(estimate.data.monthly.final.ysset,
                                        eq_2stage_lname = "eq.pfe.mIV2.2st.pv_count",
                                        eq_1stage_lname = "eq.pfe.mIV2.1st",
                                        boot_iterations = iterations))


regress_list_monthly_full <- c(regress_list_monthly,regress_list_monthly_IV)


#Add omitted columns to display later in regression 
names(regress_list_monthly_full)

regress_omitted_monthly_list <- list()
expvars_omitted_monthly_list <- list()

for(i in names(regress_list_monthly_full)){
  regress_omitted_monthly_list[[i]] <- etab_omitted(regress_list_monthly_full[[i]])
  expvars_omitted_monthly_list[[i]] <- rownames(regress_omitted_monthly_list[[i]]$coeftable)
}

#etable(regres_list_monthly[["pfe.mIV1.2st"]], vcov = diag(regres_list_monthly[["pfe.mIV1.2st"]][["bootstr"]][["stderr"]]^2))


pv_elec <- c("pfe.m1.pv_count","pfe.mIV1.2st.pv_count","pfe.m2.pv_count","pfe.mIV2.2st.pv_count")
pv_elec_se <- list(~zip,diag(regress_omitted_monthly_list[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["stderr"]]^2),
                   ~zip,diag(regress_omitted_monthly_list[["pfe.mIV2.2st.pv_count"]][["bootstr"]][["stderr"]]^2))

#etable_headers_bunching <- list("^ " = list("Number of PV installations" = 3, "Average new installed capacity"=3),
#                                "^ " = list("Agg. benefits" = 1, "Sep. benefits"=1, "Sep. benefits (IV)"=1,
#                                            "Agg. benefits" = 1, "Sep. benefits"=1, "Sep. benefits (IV)"=1))


#summary(regress_omitted_monthly_list[["pfe.mIV1.1st"]])
#names(regress_list_monthly_full[["pfe.mIV1.1st"]]$coefficients)
#wald(regress_list_monthly_full[["pfe.mIV1.1st"]],names(regress_list_monthly_full[["pfe.mIV1.1st"]]$coefficients)[1:11])


linearHypothesis(regress_omitted_monthly_list[["pfe.mIV1.1st"]],c("b_nm_mix_retail_solw_thous=0",
                                                                  "b_gc_solw_defl_thous=0",
                                                                  "b_protar_pos_subreg_defl_thous=0"),
                 test="F")
#summary(regress_omitted_monthly_list[["pfe.mIV1.1st"]])
#summary(regress_omitted_monthly_list[["pfe.mIV2.1st"]])

#corr(cbind(pdat.monthly$b_nm_mix_retail_solw_thous,pdat.monthly$b_nm_mix_6mavg_solw_defl_thous)) 
#corr(cbind(pdat.monthly$b_nm_mix_retail_6mavg_solw_thous,pdat.monthly$b_nm_mix_6mavg_solw_defl_thous))

#etable(regress_omitted_monthly_list[pv_elec],
#       vcov=pv_elec_se,
#       digits=3)

################################################################################
# -13.3 - Update labeling, print & save tables - Table 6 
################################################################################

benefitvars_labeled <- c("b_net_solw_defl_log"="Net benefits (log)",
                         "b_net_solw_defl_thous"="Net benefits",
                         "b_gc_solw_defl_thous"="Output-based incentive",
                         "b_nm_mix_mavg_solw_defl_thous"="Net metering",
                         "b_nm_ellt10_solw_defl_thous"="Net metering",
                         "b_protar_pos_defl_thous"="Capacity-based cost",
                         "b_qw_defl_thous"="Capacity-based incentive",
                         "b_protar_pos_subreg_defl_thous"="Capacity-based cost",
                         "b_qw_subreg_defl_thous"="Capacity-based incentive")

estvars_labeled <- c(benefitvars_labeled,controlvars_labeled)


setFixest_dict(estvars_labeled) #attach labels to estimation sample

etable_headers_elec <- list("^ " = list("3 Month moving average electricity price" = 2, "10\\% Long-term electricity price increase"=2),
                          "^ " = list("PPMLE" = 1, "P-CF" = 1, "PPMLE"=1, "P-CF"=1))

print("Table 6")
etable(regress_omitted_monthly_list[pv_elec],
       vcov=pv_elec_se,
       digits=3,
       group=etable_group_control,
       extralines = etable_extralines,
       headers = etable_headers_elec,
       tex=F,
       order = etable_order)


#save_table_benefits(model_list=regress_omitted_monthly_list,
#                    model_names=pv_elec,
#                    model_ses=pv_elec_se,
#                    save_name="elec_pvcount_benefits_reg",
#                    group=etable_group_control,
#                    headers=etable_headers_elec)



