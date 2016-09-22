

simulateKnowLab <- function(run, simenv) {
  
  #' Simulate the typology of family circumstances.
  #'
  #' Modifies the following in simframe in the enclosing environment:
  #' 
  #' onemorep - one more parent than last time 
  #' 
  #' onelessp - one less parent than last time 
  #' 
  #' samep - same number of parents as last time
  #' 
  #' sptype - one of
  #'  1 - become partnered (ie. onemorep)
  #'  2 - broke up (ie. onelessp)
  #'  3 - same status as before (ie. samep)
  #' 
  #' typnode - specifies which tree branch of the typology was selected, for debugging purposes
  #' 
  #' mage_years - mother's age in years
  #' 
  #' fage_years - father's age in years
  #' 
  #' Also generates, but not in simframe:
  #' 
  #' typeofchange - one of
  #'  1 - got new mother/mother back
  #'  2 - got new father/father back
  #'  3 - mother left
  #'  4 - father left
  #'  5 - have same number of parents
  #' 
  #' changewasmum - mum changed (integer vector, 1 = change, 0 = no change)
  #' 
  #' changewasdad - dad changed (integer vector, 1 = change, 0 = no change)
  #' 
  #'  
  #' change_children_num - the change in number of children
  #' 
  #' additional_household_size - additional changes in householdsize, over and above
  #' 		changes in children number, and changes in partnership
  #'  
  #' @param age
  #'  age of children, i.e: current iteration
  #' 
  #' @examples
  #' 
  #' simulate_family_household()
  #' 
  #' investigate mother changes
  #' familyChanges <- simulateFamilyChanges(simframe, models, 2)
  #' mdiffs <- (simframe$mage_years +1 != familyChanges$mage_years) & (simframe$mage_years != familyChanges$mage_years)
  #' cbind(simframe$mage_years_previous[mdiffs], familyChanges[mdiffs,])
  #' 
  #' investigate father changes
  #' familyChanges <- simulateFamilyChanges(simframe, models, 2)
  #' fdiffs <- (simframe$fage_years +1 != familyChanges$fage_years) & (simframe$fage_years != familyChanges$fage_years)
  #' cbind(simframe$fage_years_previous[fdiffs], familyChanges[fdiffs,])
  simulate_family_household <- function() {
    
    if (iteration>=2 & iteration<=5) {
      z1singleLvl1 <- predSimBinomsSelect_notChangeScores(z1single_previousLvl1, models$z1singlePrev0A2_5, models$z1singlePrev1A2_5)	
    } else if (iteration>=6) {
      z1singleLvl1 <- predSimBinomsSelect_notChangeScores(z1single_previousLvl1, models$z1singlePrev0A6_13, models$z1singlePrev1A6_13)
    }
    
    #generate propensities for scenario testing
    z1singlemodel <- PropensityModels[["z1single"]]
    z1singlePropensities <- predLogistic(z1singlemodel[[1]])
    #add some random variation to the propensity scores so that each run the units changed can differ
    #give each unit it's own standard deviation according to a binomial distribution
    s <- sqrt(z1singlePropensities*(1 - z1singlePropensities))
    z1singlePropensities <- rnorm(length(z1singlePropensities), z1singlePropensities, s)
    
    #CALIBRATION
    mult.factor <- c(NA, 1.208, 1.230, 1.148, 1.096, 1.134, 1.055, 1.043, 1.091, 1.068, 1.065, 1.071, 1.062, rep(1, 8))
    prop1 <- mean(z1singleLvl1)*mult.factor[iteration]
    calib.cat.adjust <- c(1-prop1, prop1)
    z1singleLvl1 <- adjustCatVarCalib(z1singleLvl1, "z1single", propens=z1singlePropensities, desiredProps=calib.cat.adjust, simenv = simenv, iteration = iteration)
    
    #scenarios
    z1singleLvl1 <<- adjustCatVar(z1singleLvl1, "z1single", propens=z1singlePropensities, simenv = simenv, iteration = iteration)
    z1singleLvl0 <<- as.integer(!z1singleLvl1)
    
    
    z1single_ethLvl1 <- z1singleLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    z1single_ethLvl2 <- z1singleLvl1*r1stchildethnLvl2
    z1single_ethLvl3 <- z1singleLvl1*r1stchildethnLvl3
    
    z1single_previous_ethLvl1 <- z1single_previousLvl1*(r1stchildethnLvl2 + r1stchildethnLvl4)
    z1single_previous_ethLvl2 <- z1single_previousLvl1*r1stchildethnLvl2
    z1single_previous_ethLvl3 <- z1single_previousLvl1*r1stchildethnLvl3
    
    z1single_ethLvl1 <<- z1single_ethLvl1
    z1single_ethLvl2 <<- z1single_ethLvl2
    z1single_ethLvl3 <<- z1single_ethLvl3
    z1single_previous_ethLvl1 <<- z1single_previous_ethLvl1
    z1single_previous_ethLvl2 <<- z1single_previous_ethLvl2
    z1single_previous_ethLvl3 <<- z1single_previous_ethLvl3
    
    NO_PARENT <- 99L
    
    randunif <- runif(NUMCHILDREN)
    randunif2 <- runif(NUMCHILDREN)
    
    onemorep <-  z1singleLvl0 & z1single_previousLvl1
    onelessp <-  z1singleLvl1 & !z1single_previousLvl1
    samep <-  z1singleLvl1 == z1single_previousLvl1
    #browser()
    assert(onemorep | onelessp | samep)
    
    sptype <- rep(NA_integer_, NUMCHILDREN)
    sptype[onemorep] <- 1L
    sptype[onelessp] <- 2L
    sptype[samep] <- 3L
    
    onelessp_mumleft <- onelessp & randunif <= 0.0738
    onelessp_dadleft <- onelessp & randunif > 0.0738
    
    onemorep_newdad <-	onemorep & fage_years_previous == NO_PARENT
    onemorep_newmum <-	onemorep & mage_years_previous == NO_PARENT
    #added for scenarios, in a scenario if we set a child to be in a single parent family, we
    #would ideally also set either fage_years or mage_years to be 99.  This is a work-around.
    prop.new.dad <- .95
    tot.num.new.dads <- max(round(sum(onemorep)*prop.new.dad), sum(onemorep_newdad))
    
    if (tot.num.new.dads>(sum(onemorep) - sum(onemorep_newmum))) {
      tot.num.new.dads <- sum(onemorep) - sum(onemorep_newmum)
    }
    
    
    #those left to assign new mum or dad
    to.assign <- onemorep & !onemorep_newdad & !onemorep_newmum
    num.new.dads.to.assign <- tot.num.new.dads - sum(onemorep_newdad)
    
    if (length(which(to.assign))>1){
      rand.nums <- sample(which(to.assign))
      onemorep_newdad[which(to.assign)][rank(rand.nums)<=num.new.dads.to.assign] <- TRUE
      onemorep_newmum[which(to.assign)][rank(rand.nums)>num.new.dads.to.assign] <- TRUE
    } else if ((length(which(to.assign))==1)&(num.new.dads.to.assign==1)) {
      onemorep_newdad[which(to.assign)] <- TRUE
    } else if ((length(which(to.assign))==1)&(num.new.dads.to.assign==0)) {
      onemorep_newmum[which(to.assign)] <- TRUE
    } else if (sum(to.assign)==0) {
      none.to.assign <- TRUE	
    } else {
      stop("Check assignment of new mums and dads in simulate_family_household()")
    }
    
    assert(onemorep_newdad | onemorep_newmum | onelessp_mumleft | onelessp_dadleft | samep)
    
    single_father_family <- (samep & mage_years_previous == NO_PARENT) | onelessp_mumleft
    single_mother_family <- ((samep & z1single_previousLvl1==1) & !single_father_family) | onelessp_dadleft
    
    new_mums <- rbinom(sum(single_mother_family), 1, .0128)
    new_mums <- new_mums==1
    sing.mums <- which(single_mother_family==TRUE)
    single_mother_family_new_mum <- rep(FALSE, length(single_mother_family))
    single_mother_family_new_mum[sing.mums] <- new_mums
    single_mother_family_same_mum <- single_mother_family & !single_mother_family_new_mum
    
    two_parent_family <- (samep & !z1singleLvl1) | onemorep
    
    assert(single_father_family | single_mother_family_new_mum | single_mother_family_same_mum| two_parent_family ) 
    
    num_two_parent_family <- sum(two_parent_family)
    
    two_parent_family_no_change <- two_parent_family & samep
    two_parent_family_mum_changed <- two_parent_family & onemorep_newmum
    two_parent_family_dad_changed <- two_parent_family & onemorep_newdad
    two_parent_family_both_changed <- two_parent_family & !samep & !onemorep_newmum & !onemorep_newdad
    
    typeofchangeLvl1 <- rep(0L, NUMCHILDREN)
    typeofchangeLvl2 <- rep(0L, NUMCHILDREN)
    typeofchangeLvl3 <- rep(0L, NUMCHILDREN)
    typeofchangeLvl4 <- rep(0L, NUMCHILDREN)
    typeofchangeLvl5 <- rep(0L, NUMCHILDREN)
    
    typeofchangeLvl1[onemorep_newmum] <- 1L
    typeofchangeLvl2[onemorep_newdad] <- 1L
    typeofchangeLvl3[onelessp_mumleft] <- 1L
    typeofchangeLvl4[onelessp_dadleft] <- 1L
    typeofchangeLvl5[samep] <- 1L
    
    typeofchangeLvl1 <<- typeofchangeLvl1
    typeofchangeLvl2 <<- typeofchangeLvl2
    typeofchangeLvl3 <<- typeofchangeLvl3
    typeofchangeLvl4 <<- typeofchangeLvl4
    typeofchangeLvl5 <<- typeofchangeLvl5
    
    eth_typeofchangeLvl1 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl1
    eth_typeofchangeLvl2 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl2
    eth_typeofchangeLvl3 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl3
    eth_typeofchangeLvl4 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl4
    eth_typeofchangeLvl5 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl5
    eth_typeofchangeLvl6 <<- r1stchildethnLvl2*typeofchangeLvl1
    eth_typeofchangeLvl7 <<- r1stchildethnLvl2*typeofchangeLvl2
    eth_typeofchangeLvl8 <<- r1stchildethnLvl2*typeofchangeLvl3
    eth_typeofchangeLvl9 <<- r1stchildethnLvl2*typeofchangeLvl4
    eth_typeofchangeLvl10 <<- r1stchildethnLvl2*typeofchangeLvl5
    eth_typeofchangeLvl11 <<- r1stchildethnLvl3*typeofchangeLvl1
    eth_typeofchangeLvl12 <<- r1stchildethnLvl3*typeofchangeLvl2
    eth_typeofchangeLvl13 <<- r1stchildethnLvl3*typeofchangeLvl3
    eth_typeofchangeLvl14 <<- r1stchildethnLvl3*typeofchangeLvl4
    eth_typeofchangeLvl15 <<- r1stchildethnLvl3*typeofchangeLvl5
    
    #typnode, specifies which typology tree branch was selected
    #for debugging purposes
    typnode <- rep(NA_integer_, NUMCHILDREN)
    typnode[onelessp_mumleft] <- 1L
    typnode[onelessp_dadleft] <- 2L
    typnode[onemorep_newdad] <- 3L
    typnode[onemorep_newmum] <- 4L
    typnode[single_father_family] <- 5L 
    typnode[single_mother_family_new_mum] <- 6L
    typnode[single_mother_family_same_mum] <- 7L
    #typnode[single_mother_family_mum_left_dad_return] <- 8L
    typnode[two_parent_family_both_changed] <- 9L 
    typnode[two_parent_family_mum_changed] <- 10L
    typnode[two_parent_family_dad_changed] <- 11L
    typnode[two_parent_family_no_change] <- 12L
    
    checkNAs(typnode)
    
    changewasmum <- rep(0L, NUMCHILDREN)
    changewasdad <- rep(0L, NUMCHILDREN)
    
    changewasmum[onelessp_mumleft | 
                   onemorep_newmum | 
                   single_mother_family_new_mum | 
                   two_parent_family_both_changed |
                   two_parent_family_mum_changed] <- 1L
    
    changewasdad[onelessp_dadleft |
                   onemorep_newdad |
                   two_parent_family_both_changed |
                   two_parent_family_dad_changed] <- 1L
    
    
    #fage_years
    fage_years <- rep(NA_integer_, NUMCHILDREN)
    
    fage_inc <- fage_years_previous!=NO_PARENT
    fage_years[fage_inc] <- fage_years_previous[fage_inc] + 1
    
    fage_years[onelessp_dadleft | single_mother_family_new_mum | single_mother_family_same_mum] <- NO_PARENT
    
    #when doing scenario testing it is possible to change children that were single to two-parent,
    #but they will still have 99s for fage_years_previous, so we need to impute a fathers age for this year
    #(it cannot just be incremented by 1) 
    #the model only depends on mage_years
    impute_fathers_age <- ((two_parent_family_no_change | single_father_family) & fage_years_previous==NO_PARENT) | onemorep_newdad
    
    fage_years[impute_fathers_age] <- as.integer(round(predSimNorm(models$fage_years, set=impute_fathers_age)))
    #fage years will be about 78 if mage_years 99 was used - write in some code to correct it if fage_years is this old
    if (any(fage_years[impute_fathers_age]>78)) {
      fage_years[(impute_fathers_age==TRUE)&(fage_years>78)] <- sample(fage_years[(!is.na(fage_years))&(fage_years<78)], length(which((impute_fathers_age==TRUE)&(fage_years>78))))
    }
    
    #double-check for NAs - there should be none
    if (sum(is.na(fage_years))>0) {
      warning("NAs in fage_years.  Mean fage_years imputed\n")
    }
    na.id <- which(is.na(fage_years))
    fage_years[na.id] <- round(mean(fage_years[fage_years!=99], na.rm=T))
    
    checkNAs(fage_years)
    
    #mage_years
    mage_years <- rep(NA_integer_, NUMCHILDREN)
    
    mage_years[onelessp_mumleft | single_father_family] <- NO_PARENT
    
    mage_inc <- (onelessp_dadleft | onemorep_newdad | two_parent_family | single_mother_family_same_mum) & mage_years_previous != NO_PARENT
    mage_prev_no_parent <- (onelessp_dadleft | onemorep_newdad | two_parent_family | single_mother_family_same_mum) & mage_years_previous == NO_PARENT
    mage_years[mage_inc] <- mage_years_previous[mage_inc] + 1
    mage_years[mage_prev_no_parent] <- NO_PARENT
    
    mage_years[onemorep_newmum] <- MAGE[onemorep_newmum] + iteration - 4
    
    mage_years_previous_smfnm <- mage_years_previous[single_mother_family_new_mum]
    
    if (length(mage_years_previous_smfnm) > 0) {
      differenceInMumAge <- rep(NA_integer_, length(mage_years_previous_smfnm))
      differenceInMumAge[mage_years_previous_smfnm >=55] <- -39
      differenceInMumAge[mage_years_previous_smfnm <=19] <- 41
      midgroup <- mage_years_previous_smfnm  > 19 & mage_years_previous_smfnm < 55
      differenceInMumAge[midgroup] <- sample(c(-1:3), size=sum(midgroup), replace=T)
      
      mage_years[single_mother_family_new_mum] <- mage_years_previous[single_mother_family_new_mum] + differenceInMumAge
    }
    
    checkNAs(mage_years)
    
    no99mage_years <- mage_years
    no99mage_years[mage_years==99] <- NA
    
    # mum group and dad group
    sameDadAsb4 <- !changewasdad & fage_years != NO_PARENT
    sameMumAsb4 <- !changewasmum & mage_years != NO_PARENT
    
    #if ever allow a scenario father's age then put an 'if' statement here that only
    #does this if a scenario is not being run on father's age
    fage_years_grouped_same_as_fage <- bin(fage_years - iteration, binbreaks$fage_years)
    at_birth_fage_years_grped <- bin(children$fage_years - 1, binbreaks$fage_years)
    #table(fage_years_grouped_same_as_fage, at_birth_fage_years_grped)
    current_fage_consistent_with_at_birth_fage <- fage_years_grouped_same_as_fage == at_birth_fage_years_grped
    
    birthfather <- sameDadAsb4 & current_fage_consistent_with_at_birth_fage
    
    if (any(is.na(simenv$cat.adjustments$MAGE))) {
      dif.in.mage <- (mage_years - iteration) - (children$mage_years - 1)
      birthmother <- sameMumAsb4 & (abs(dif.in.mage)<2)
    } else {
      #doing a scenario on mother's age
      #set a certain percentage to be the birthmothers
      #set 3.5% to be not birthmothers
      birthmother <- sameMumAsb4
      birthmother[birthmother==TRUE] <- rbinom(sum(sameMumAsb4), 1, 0.965) 
    }
    
    
    mumgroup <- rep(NA_integer_, NUMCHILDREN)
    mumgroup[birthmother==1] <- 1L
    mumgroup[!birthmother & !sameMumAsb4] <- 2L
    mumgroup[!birthmother & sameMumAsb4] <- 3L
    mumgroup[mage_years == NO_PARENT] <- 0L
    checkNAs(mumgroup)
    
    dadgroup <- rep(NA_integer_, NUMCHILDREN)
    dadgroup[birthfather] <- 1L
    dadgroup[!birthfather & !sameDadAsb4] <- 2L
    dadgroup[!birthfather & sameDadAsb4] <- 3L
    dadgroup[(fage_years == NO_PARENT)|single_mother_family] <- 0L
    #temp fix
    if (sum(is.na(dadgroup))>0) {
      na.id <- which(is.na(dadgroup))
      dadgroup[na.id] <- 1
    }
    checkNAs(dadgroup)
    
    #kids
    if (iteration<=5) {
      change_children_num <- predSimNorm(models$chkids2_5)
    } else if (iteration>5) {
      change_children_num <- predSimNorm(models$chkids6_13)
    }
    # else if (sum(is.na(z1singleLvl1))==0) {
    #z1single was simulated for year 14 to 18
    #bthorder1.change <- rep(0, NUMCHILDREN)
    #bthorder2.change <- rbinom(NUMCHILDREN, 1, .01) 
    #bthorder2.change <- bthorder2.change*(-1)
    #bthorder3.change <- rbinom(NUMCHILDREN, 1, .02)
    #bthorder3.change <- bthorder3.change*(-1)
    
    #	change_children_num <- rep(NA, NUMCHILDREN)
    #	change_children_num[bthorder==1] <- bthorder1.change[bthorder==1]
    #	change_children_num[bthorder==2] <- bthorder2.change[bthorder==2]
    #	change_children_num[bthorder>=3] <- bthorder3.change[bthorder>=3]
    
    #consistency tweak
    change_children_num[change_children_num < 1 - kids_previous] <- (1 - kids_previous)[change_children_num < 1 - kids_previous]
    
    kids <- kids_previous + change_children_num
    
    kids <- round(kids)
    
    #max cut-off tweak
    kids[kids>10] <- 10
    
    #generate propensities for scenario testing
    kidsmodels <- PropensityModels[["kids"]]
    kidsPropensities <- predictOrdinal(kidsmodels, NUMCHILDREN, stochastic=TRUE)
    
    kids <- adjustContVar(kids, "kids", propens=kidsPropensities[,-ncol(kidsPropensities)], simenv = simenv, iteration = iteration)
    
    kids[kids<1] <- 1
    kids[kids>10] <- 10
    checkNAs(kids)
    
    change_kids <- kids - kids_previous
    
    #household_size
    householdsize <- round(householdsize_previous - as.integer(onelessp) + as.integer(onemorep) + change_kids)
    householdsize[householdsize < 2] <- 2
    
    #CALIBRATION
    mult.factor <- c(NA, rep(1, 2), .989, .967, .965, .939, .934, .952, .946, .916, .983, .960, rep(1, 8))
    householdsize <- householdsize*mult.factor[iteration]
    #no rounding or else we just get exactly the same values as not aligning
    #note: this means that confreqs will have decimals in it.					
    
    #scenarios
    householdsize <- adjustContVar(householdsize, "householdsize", simenv = simenv, iteration = iteration)
    
    #max cut-off tweak
    householdsize[householdsize < 2] <- 2
    householdsize[householdsize > 14] <- 14
    checkNAs(householdsize)
    
    #modify enclosing environment
    
    onemorep <<- as.integer(onemorep)
    onelessp <<-  as.integer(onelessp)
    samep <<-  as.integer(samep)
    
    sptype <<- sptype
    sptypeLvl1 <<- rep(0, NUMCHILDREN)
    sptypeLvl1[sptype==1] <<- 1 
    sptypeLvl2 <<- rep(0, NUMCHILDREN)
    sptypeLvl2[sptype==2] <<- 1 
    sptypeLvl3 <<- rep(0, NUMCHILDREN)
    sptypeLvl3[sptype==3] <<- 1 
    typnode <<- typnode
    
    fage_years <<- fage_years
    mage_years <<- mage_years
    
    #no99fage_years <<- no99fage_years
    no99mage_years <<- no99mage_years
    
    kids <<- kids
    householdsize <<- householdsize
    
    mumgroup <<- mumgroup
    dadgroup <<- dadgroup
    
  }
  
  simulate_psychosocial_factors <- function() {
    
    ####################
    #####  z1chpar  ####
    ####################
    if (iteration<=5) {
      z1chparLvl1 <- predSimBinomsSelect_notChangeScores(z1chpar_previousLvl1, models$z1chparPrev0A2_5, models$z1chparPrev1A2_5)
    } else if (iteration>5) {
      z1chparLvl1 <- predSimBinomsSelect_notChangeScores(z1chpar_previousLvl1, models$z1chparPrev0A6_13, models$z1chparPrev1A6_13)
    } 
    ##switching 0s and 1s for children that previously had a change in parents 
    ##(z1chpar different kind of change score to e.g. z1single or welfare) 
    #z1chparLvl1[z1chpar_previousLvl1==1] <- (-1)*(z1chparLvl1[z1chpar_previousLvl1==1] - 1)
    
    
    #generate propensities for scenario testing
    z1chparmodel <- PropensityModels[["z1chpar"]]
    z1chparPropensities <- predLogistic(z1chparmodel[[1]])
    #add some random variation to the propensity scores so that each run the units changed can differ
    #give each unit it's own standard deviation according to a binomial distribution
    s <- sqrt(z1chparPropensities*(1 - z1chparPropensities))
    z1chparPropensities <- rnorm(length(z1chparPropensities), z1chparPropensities, s)
    
    z1chparLvl1 <<- adjustCatVar(z1chparLvl1, "z1chpar", propens=z1chparPropensities, simenv = simenv, iteration = iteration)
    
    z1chparLvl0 <<- as.integer(!z1chparLvl1)
    checkNAs(z1chparLvl1)
   
    
    
    z1chpar_ethLvl1 <- z1chparLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    z1chpar_ethLvl2 <- z1chparLvl1*r1stchildethnLvl2
    z1chpar_ethLvl3 <- z1chparLvl1*r1stchildethnLvl3
    
    z1chpar_previous_ethLvl1 <- z1chpar_previousLvl1*(r1stchildethnLvl2 + r1stchildethnLvl4)
    z1chpar_previous_ethLvl2 <- z1chpar_previousLvl1*r1stchildethnLvl2
    z1chpar_previous_ethLvl3 <- z1chpar_previousLvl1*r1stchildethnLvl3
    
    z1chpar_ethLvl1 <<- z1chpar_ethLvl1
    z1chpar_ethLvl2 <<- z1chpar_ethLvl2
    z1chpar_ethLvl3 <<- z1chpar_ethLvl3
    z1chpar_previous_ethLvl1 <<- z1chpar_previous_ethLvl1
    z1chpar_previous_ethLvl2 <<- z1chpar_previous_ethLvl2
    z1chpar_previous_ethLvl3 <<- z1chpar_previous_ethLvl3
    
    ####################
    #####  chres  ######
    ####################
    #create binary variable for whether parents changed previously or not

    z1chres.prev <- rep(NA, length(chres_previous))
    z1chres.prev[chres_previous>0] <- 1
    z1chres.prev[chres_previous==0] <- 0
    if (iteration<=5) {
      z1chres <- predSimBinomsSelect_notChangeScores(z1chres.prev, 
                                                     models$z1chresPrev0.a2_5, 
                                                     models$z1chresPrev1.a2_5)
    } else if (iteration>5) {
      z1chres <- predSimBinomsSelect_notChangeScores(z1chres.prev, 
                                                     models$z1chresPrev0.a6_13, 
                                                     models$z1chresPrev1.a6_13)
    } 
    
    mult.factor <- c(rep(NA, 17), 0.82, 0.79, 0.76, 0.73)/(sum(z1chres==0)/5000)
    
    if(iteration >= 18){
      z1chres[z1chres==0] <- 1-rbinom(sum(z1chres==0), 1, mult.factor[iteration])
    }
    #for children that do have a change in residence, simulate the number of changes
    if (iteration<=5) {
      chres.pre <- predSimNBinom(models$chres.a2_5) + 1
    } else if (iteration>5 ) {
      chres.pre <- predSimNBinom(models$chres.a6_13) + 1	
    } 
    #the '+ 1' is the backtransformation (chres - 1 is NB distributed)
    chres <- rep(NA, length(chres.pre))
    chres[z1chres==0] <- 0
    chres[z1chres==1] <- chres.pre[z1chres==1]
    #tweak for if the simulated value of chres is greater than 13
    chres[chres>13] <- sample(c(2:10, 13), sum(chres>13), 
                              prob=c(.656, .198, .072, .023, .017, 
                                     .008, .004, .019, .001, .001), replace=TRUE)

    checkNAs(chres)
    
    #generate propensities for scenario testing
    chresmodels <- PropensityModels[["chres"]]
    chresPropensities <- predictOrdinal(chresmodels, NUMCHILDREN, stochastic=TRUE)
    
    chres <- adjustContVar(chres, "chres", propens=chresPropensities[,-ncol(chresPropensities)], 
                           simenv = simenv, iteration = iteration)
 
    
    chres <<- chres		
  }
  
  #' modifies: welfareLvl1, welfareLvl0, mhrswrk, fhrswrk
  #' simulate_employment()
  simulate_employment <- function() {
    
    #mhrswrk
    #create binary variable for whether the mother worked previously or not
    z1mhrs.prev <- rep(NA, length(mhrswrk_previous))
    z1mhrs.prev[mhrswrk_previous>0] <- 1
    z1mhrs.prev[mhrswrk_previous==0] <- 0
    
    #simulate for the current iteration whether the mother works or not
    #first create separate vectors of length number of children for each mumgroup model
    if (iteration<=5) {
      z1mhrs1 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a2_5.mg1, models$z1mhrswrk.prev1.a2_5)	
      z1mhrs2 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a2_5.mg2, models$z1mhrswrk.prev1.a2_5)	
      z1mhrs3 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a2_5.mg3, models$z1mhrswrk.prev1.a2_5)
    } else if (iteration>5) {
      z1mhrs1 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a6_13.mg1 , models$z1mhrswrk.prev1.a6_13)	
      z1mhrs2 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a6_13.mg2, models$z1mhrswrk.prev1.a6_13)	
      z1mhrs3 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a6_13.mg3, models$z1mhrswrk.prev1.a6_13)
    }
    #then assign to the variable denoting whether the mother works or not, the values from the vector corresponding to her mumgroup
    z1mhrs <- rep(NA, length(mhrswrk_previous))
    z1mhrs[mumgroup==0] <- z1mhrs1[mumgroup==0] #0 #
    z1mhrs[mumgroup==1] <- z1mhrs1[mumgroup==1]
    z1mhrs[mumgroup==2] <- z1mhrs2[mumgroup==2]
    z1mhrs[mumgroup==3] <- z1mhrs3[mumgroup==3]
    
    #for mothers that do work, simulate the number of hours they work
    #create separate vectors of length number of children for each mumgroup model
    
    if (iteration<=5) {
      mhrswrk.pre1 <- predSimNBinom(models$mhrswrk.a2_5.mg1) + 1
      #the other mumgroups had small numbers and so a standard NB model with a single dispersion parameter was used
      mhrswrk.pre2 <- predSimNBinom(models$mhrswrk.a2_5.mg2) + 1
      mhrswrk.pre3 <- predSimNBinom(models$mhrswrk.a2_5.mg3) + 1
      #the '+ 1' is the backtransformation (mhrswrk - 1 is NB distributed)
    } else if (iteration>5) {
      #mumgroup1 uses a negative binomial with the dispersion parameter modeled as a function of age
      alpha <- .4203 - .0145*iteration 
      mhrswrk.pre1 <- predSimNBinom(models$mhrswrk.a6_13.mg1, alpha=alpha) + 1
      #the other mumgroups had small numbers and so a standard NB model with a single dispersion parameter was used
      mhrswrk.pre2 <- predSimNBinom(models$mhrswrk.a6_13.mg2) + 1
      mhrswrk.pre3 <- predSimNBinom(models$mhrswrk.a6_13.mg3) + 1
      #the '+ 1' is the backtransformation (mhrswrk - 1 is NB distributed)
    }
    
    #assign to mhrswrk, the values from the vector corresponding to her mumgroup
    mhrswrk[mumgroup==1] <- mhrswrk.pre1[mumgroup==1]
    mhrswrk[mumgroup==2] <- mhrswrk.pre2[mumgroup==2]
    mhrswrk[mumgroup==3] <- mhrswrk.pre3[mumgroup==3]
    mhrswrk[z1mhrs==0] <- 0
    mhrswrk <- round(mhrswrk)
    mhrswrk[mhrswrk < 0] <- 0
    mhrswrk[mhrswrk>73] <- 73
    
    #index <- which(mhrswrk > 40)
    #mhrswrk[index] <- mhrswrk[index] - 15
    
    #CALIBRATION
    #mult.factor <- c(NA, 1.22, 1.29, 1.36, 1.37, 1.30, 1.27, 1.24, 1.25, 1.15, 1.15, 1.10, 1.12, rep(1, 8))
    mult.factor <- c(NA, 1.22, 1.29, 1.36, 1.37, 1.30, 1.27, 1.24, 1.25, 1.15, 1.15, 1.10, 1.12, 
                     1.05, 1, 0.97, 0.95, 0.9, 0.87, 0.87, 0.85)
    mhrswrk <- mhrswrk*mult.factor[iteration]
    #no rounding or else we just get exactly the same values as not aligning
    
    #generate propensities for scenario testing
    mhrswrkmodels <- PropensityModels[["mhrswrk"]]
    mhrswrkPropensities <- predictOrdinal(mhrswrkmodels, NUMCHILDREN, stochastic=TRUE)
    
    #scenarios
    mhrswrk <- adjustContVar(mhrswrk, "mhrswrk", 
                             propens=mhrswrkPropensities[,-ncol(mhrswrkPropensities)], 
                             simenv = simenv, iteration = iteration)
    checkNAs(mhrswrk)
    
    #fhrswrk
    #create binary variable for whether the father worked previously or not
    z1fhrs.prev <- rep(NA, length(fhrswrk_previous))
    z1fhrs.prev[fhrswrk_previous>0] <- 1
    z1fhrs.prev[fhrswrk_previous==0] <- 0
    
    #simulate for the current iteration whether the father works or not
    #first create separate vectors of length number of children for each dadgroup model
    if (iteration<=5) {
      z1fhrs1 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a2_5.dg1 , models$z1fhrswrk.prev1.a2_5.dg1)	
      z1fhrs2 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a2_5.dg2 , models$z1fhrswrk.prev1.a2_5.dg2)	
      z1fhrs3 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a2_5.dg3 , models$z1fhrswrk.prev1.a2_5.dg3)
    } else if (iteration>5) {
      z1fhrs1 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a6_13.dg1 , models$z1fhrswrk.prev1.a6_13.dg1)	
      z1fhrs2 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a6_13.dg2 , models$z1fhrswrk.prev1.a6_13.dg2)	
      z1fhrs3 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a6_13.dg3 , models$z1fhrswrk.prev1.a6_13.dg3)
    }
    #then assign to the variable denoting whether the father works or not, the values from the vector corresponding to his dadgroup
    z1fhrs <- rep(NA, length(fhrswrk_previous))
    z1fhrs[dadgroup==0] <- z1fhrs1[dadgroup==0] #0 #
    z1fhrs[dadgroup==1] <- z1fhrs1[dadgroup==1]
    z1fhrs[dadgroup==2] <- z1fhrs2[dadgroup==2]
    z1fhrs[dadgroup==3] <- z1fhrs3[dadgroup==3]
    
    #for fathers that do work, simulate the number of hours they work
    #3 normal models are used
    if (iteration<=5) {
      fhrswrk.pre <- predSimNorm(models$fhrswrk.a2_5)
    } else if (iteration>5) {
      fhrswrk.pre <- predSimNormsSelect3Models(dadgroup, models$fhrswrk.a6_13.dg1, models$fhrswrk.a6_13.dg2, models$fhrswrk.a6_13.dg3)
    }
    
    fhrswrk <- fhrswrk.pre
    fhrswrk[z1fhrs==0] <- 0
    fhrswrk <- round(fhrswrk)
    fhrswrk[fhrswrk < 0] <- 0
    fhrswrk[fhrswrk > 100] <- 100

    # fhrswrk[fhrswrk > 40] <- fhrswrk[fhrswrk > 40] - 2
    # fhrswrk[fhrswrk < 36 & fhrswrk > 20 ] <- fhrswrk[fhrswrk < 36 & fhrswrk > 20 ] + 4
    
    # mult.factor <- c(NA, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95,
    #                  0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95)
    # fhrswrk <- fhrswrk*mult.factor[iteration]
    
    #generate propensities for scenario testing
    fhrswrkmodels <- PropensityModels[["fhrswrk"]]
    fhrswrkPropensities <- predictOrdinal(fhrswrkmodels, NUMCHILDREN, stochastic=TRUE)
    
    fhrswrk <- adjustContVar(fhrswrk, "fhrswrk", propens=mhrswrkPropensities[,-ncol(fhrswrkPropensities)], simenv = simenv, iteration = iteration)
    checkNAs(fhrswrk)
    
    
    mhrswrk <<- mhrswrk
    fhrswrk <<- fhrswrk
    
    #welfare
    
    if (iteration<=5) {
      welfareLvl1 <- predSimBinomsSelect(welfare_previousLvl1, models$welfarePrev0.a2_5, models$welfarePrev1.a2_5)
    } else if (iteration>5) {
      welfareLvl1 <- predSimBinomsSelect(welfare_previousLvl1, models$welfarePrev0.a6_13, models$welfarePrev1.a6_13)
    }
    
    #generate propensities for scenario testing
    welfaremodel <- PropensityModels[["welfare"]]
    welfarePropensities <- predLogistic(welfaremodel[[1]])
    #add some random variation to the propensity scores so that each run the units changed can differ
    #give each unit it's own standard deviation according to a binomial distribution
    s <- sqrt(welfarePropensities*(1 - welfarePropensities))
    welfarePropensities <- rnorm(length(welfarePropensities), welfarePropensities, s)
    
    #CALIBRATION
    mult.factor <- c(NA, 2.05, 1.22, 1.04, 1.03, 1.29, 1.26, 1.23, 1.23, 1.17, 1.15, 1.12, 1.09, rep(1, 8))
    prop1 <- mean(welfareLvl1)*mult.factor[iteration]
    calib.cat.adjust <- c(1-prop1, prop1)
    welfareLvl1 <- adjustCatVarCalib(welfareLvl1, "welfare", propens=welfarePropensities, desiredProps=calib.cat.adjust, simenv = simenv, iteration = iteration)
    
    #Scenarios
    welfareLvl1 <<- adjustCatVar(welfareLvl1, "welfare", propens=welfarePropensities, simenv = simenv, iteration = iteration)
    checkNAs(welfareLvl1)
    welfareLvl0 <<- as.integer(!welfareLvl1)
    
    
    welfare_ethLvl1 <<- welfareLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    welfare_ethLvl2 <<- welfareLvl1*r1stchildethnLvl2
    welfare_ethLvl3 <<- welfareLvl1*r1stchildethnLvl3
    
    welfare_previous_ethLvl1 <<- welfare_previousLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    welfare_previous_ethLvl2 <<- welfare_previousLvl1*r1stchildethnLvl2
    welfare_previous_ethLvl3 <<- welfare_previousLvl1*r1stchildethnLvl3
    
  }
  
  simulate_material_circumstances <- function() {
    
    if (iteration<=5) {
      z1accomLvl1 <- predSimBinomsSelect_notChangeScores(z1accom_previousLvl1, models$z1accomPrev0.a2_5, models$z1accomPrev1.a2_5)
      z1homeownLvl1 <- predSimBinomsSelect_notChangeScores(z1homeown_previousLvl1, models$z1homeownPrev0.a2_5, models$z1homeownPrev1.a2_5)
      z1overcrowdLvl1 <- predSimBinomsSelect_notChangeScores(z1overcrowd_previousLvl1, models$z1overcrowdPrev0.a2_5, models$z1overcrowdPrev1.a2_5)
    } else if (iteration>5) {
      z1accomLvl1 <- predSimBinomsSelect_notChangeScores(z1accom_previousLvl1, models$z1accomPrev0.a6_13, models$z1accomPrev1.a6_13)
      z1homeownLvl1 <- predSimBinomsSelect_notChangeScores(z1homeown_previousLvl1, models$z1homeownPrev0.a6_13, models$z1homeownPrev1.a6_13)
      z1overcrowdLvl1 <- predSimBinomsSelect_notChangeScores(z1overcrowd_previousLvl1, models$z1overcrowdPrev0.a6_13, models$z1overcrowdPrev1.a6_13)
    }	
    
    #generate propensities for scenario testing and calibration
    z1accommodel <- PropensityModels[["z1accom"]]
    z1homeownmodel <- PropensityModels[["z1homeown"]]
    z1overcrowdmodel <- PropensityModels[["z1overcrowd"]]
    
    z1accomPropensities <- predLogistic(z1accommodel[[1]])
    z1homeownPropensities <- predLogistic(z1homeownmodel[[1]])
    z1overcrowdPropensities <- predLogistic(z1overcrowdmodel[[1]])
    
    #add some random variation to the propensity scores so that each run the units changed can differ
    #give each unit it's own standard deviation according to a binomial distribution
    sz1accom <- sqrt(z1accomPropensities*(1 - z1accomPropensities))
    sz1homeown <- sqrt(z1homeownPropensities*(1 - z1homeownPropensities))
    sz1overcrowd <- sqrt(z1overcrowdPropensities*(1 - z1overcrowdPropensities))
    
    z1accomPropensities <- rnorm(length(z1accomPropensities), z1accomPropensities, sz1accom)
    z1homeownPropensities <- rnorm(length(z1homeownPropensities), z1homeownPropensities, sz1homeown)
    z1overcrowdPropensities <- rnorm(length(z1overcrowdPropensities), z1overcrowdPropensities, sz1overcrowd)
    
    #CALIBRATION
    mult.factor <- c(NA, 1.14, 1.17, 1.10, 1.07, 1.12, 1.12, 1.09, 1.11, 1.12, 1.09, 1.11, 1.13,  rep(1, 8))
    prop1 <- mean(z1homeownLvl1)*mult.factor[iteration]
    calib.cat.adjust <- c(1-prop1, prop1)
    z1homeownLvl1 <- adjustCatVarCalib(z1homeownLvl1, "z1homeown", propens=z1homeownPropensities, desiredProps=calib.cat.adjust, simenv = simenv, iteration = iteration)
    
    #SCENARIOS
    z1accomLvl1 <<- adjustCatVar(z1accomLvl1, "z1accom", propens=z1accomPropensities, simenv = simenv, iteration = iteration)
    z1homeownLvl1 <<- adjustCatVar(z1homeownLvl1, "z1homeown", propens=z1homeownPropensities, simenv = simenv, iteration = iteration)
    z1overcrowdLvl1 <<- adjustCatVar(z1overcrowdLvl1, "z1overcrowd", propens=z1overcrowdPropensities, simenv = simenv, iteration = iteration)
    
    checkNAs(z1accomLvl1)
    checkNAs(z1homeownLvl1)
    checkNAs(z1overcrowdLvl1)
    
    
    z1accom_ethLvl1 <<- z1accomLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    z1accom_ethLvl2 <<- z1accomLvl1*r1stchildethnLvl2
    z1accom_ethLvl2 <<- z1accomLvl1*r1stchildethnLvl2
    
    z1accom_previous_ethLvl1 <<- z1accom_previousLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    z1accom_previous_ethLvl2 <<- z1accom_previousLvl1*r1stchildethnLvl2
    z1accom_previous_ethLvl2 <<- z1accom_previousLvl1*r1stchildethnLvl2
    
    z1homeown_ethLvl1 <<- z1homeownLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    z1homeown_ethLvl2 <<- z1homeownLvl1*r1stchildethnLvl2
    z1homeown_ethLvl2 <<- z1homeownLvl1*r1stchildethnLvl2
    
    z1homeown_previous_ethLvl1 <<- z1homeown_previousLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    z1homeown_previous_ethLvl2 <<- z1homeown_previousLvl1*r1stchildethnLvl2
    z1homeown_previous_ethLvl2 <<- z1homeown_previousLvl1*r1stchildethnLvl2
    
    z1overcrowd_ethLvl1 <<- z1overcrowdLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    z1overcrowd_ethLvl2 <<- z1overcrowdLvl1*r1stchildethnLvl2
    z1overcrowd_ethLvl2 <<- z1overcrowdLvl1*r1stchildethnLvl2
    
    z1overcrowd_previous_ethLvl1 <<- z1overcrowd_previousLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
    z1overcrowd_previous_ethLvl2 <<- z1overcrowd_previousLvl1*r1stchildethnLvl2
    z1overcrowd_previous_ethLvl2 <<- z1overcrowd_previousLvl1*r1stchildethnLvl2
  }
  
  simulate_behavioural_factors <- function() {
 
      #msmoke
      #create binary variable for whether the mother smoked previously or not
      z1msmk.prev <- rep(NA, length(msmoke_previous))
      z1msmk.prev[msmoke_previous>0] <- 1
      z1msmk.prev[msmoke_previous==0] <- 0
      
      #simulate for the current iteration whether the mother works or not
      #first create separate vectors of length number of children for each mumgroup model
      if (iteration<=5) {
        z1msmk1 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a2_5.mg1, models$z1msmoke.prev1.a2_5)	
        z1msmk2 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a2_5.mg2, models$z1msmoke.prev1.a2_5)	
        z1msmk3 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a2_5.mg3, models$z1msmoke.prev1.a2_5)
      } else if (iteration>5) {
        z1msmk1 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a6_13, models$z1msmoke.prev1.a6_13.mg1)	
        z1msmk2 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a6_13, models$z1msmoke.prev1.a6_13.mg2)	
        z1msmk3 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a6_13, models$z1msmoke.prev1.a6_13.mg3)
      }
      #then assign to the variable denoting whether the mother works or not, the values from the vector corresponding to her mumgroup
      z1msmk <- rep(NA, length(mhrswrk_previous))
      z1msmk[mumgroup==0] <- 0 
      z1msmk[mumgroup==1] <- z1msmk1[mumgroup==1]
      z1msmk[mumgroup==2] <- z1msmk2[mumgroup==2]
      z1msmk[mumgroup==3] <- z1msmk3[mumgroup==3]
      
      #for mothers that do work, simulate the number of hours they work
      #3 normal models are used
      if (iteration<=5) {
        msmoke.pre <- predSimNorm(models$msmoke.a2_5)
      } else if (iteration>5) {
        msmoke.pre <- predSimNormsSelect3Models(mumgroup, models$msmoke.a6_13.mg1, models$msmoke.a6_13.mg2, models$msmoke.a6_13.mg3)
      }
      msmoke <- msmoke.pre
      msmoke[z1msmk==0] <- 0
      msmoke <- round(msmoke)
      msmoke[msmoke < 0] <- 0
      msmoke[msmoke > 70] <- 70
      
      #generate msmoke propensities for scenario testing
      msmokemodels <- PropensityModels[["msmoke"]]
      msmokePropensities <- predictOrdinal(msmokemodels, NUMCHILDREN, stochastic=TRUE)
      
      #CALIBRATION
      mult.factor <- c(NA, .993, .979, .991, 1.022, .964, .971, .974, .959, .957, .955, .962, .957, rep(1, 8))
      prop0 <- mean(msmoke==0)*mult.factor[iteration]
      m.bin <- bin(msmoke, binbreaks$msmoke)
      tab <- table(m.bin)/sum(table(m.bin))
      w <- tab[2:4]
      w2 <- w/sum(w)
      props1 <- (1-prop0)*w2
      calib.cat.adjust <- c(prop0, props1)
      msmoke <- adjustContVarCalib(msmoke, "msmoke", propens=msmokePropensities, desiredProps=calib.cat.adjust, simenv = simenv, iteration = iteration)
      
      #sceanrios
      msmoke <- adjustContVar(msmoke, "msmoke", propens=msmokePropensities[,-ncol(msmokePropensities)], simenv = simenv, iteration = iteration)
      checkNAs(msmoke)
      
      #fsmoke
      #create binary variable for whether the father smokeed previously or not
      z1fsmk.prev <- rep(NA, length(fsmoke_previous))
      z1fsmk.prev[fsmoke_previous>0] <- 1
      z1fsmk.prev[fsmoke_previous==0] <- 0
      
      #simulate for the current iteration whether the father works or not
      #first create separate vectors of length number of children for each dadgroup model
      ##z1fsmk0 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.dg0.prev0, models$z1fsmoke.dg0.prev1)
      if (iteration<=5) {
        z1fsmk1 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a2_5.dg1, models$z1fsmoke.prev1.a2_5.dg1)	
        z1fsmk2 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a2_5.dg2, models$z1fsmoke.prev1.a2_5.dg2)	
        z1fsmk3 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a2_5.dg3, models$z1fsmoke.prev1.a2_5.dg3)
      } else if (iteration>5) {
        z1fsmk1 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a6_13.dg1, models$z1fsmoke.prev1.a6_13)	
        z1fsmk2 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a6_13.dg2, models$z1fsmoke.prev1.a6_13)	
        z1fsmk3 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a6_13.dg3, models$z1fsmoke.prev1.a6_13)
      }
      #then assign to the variable denoting whether the father works or not, the values from the vector corresponding to his dadgroup
      z1fsmk <- rep(NA, length(fhrswrk_previous))
      z1fsmk[dadgroup==0] <- 0
      z1fsmk[dadgroup==1] <- z1fsmk1[dadgroup==1]
      z1fsmk[dadgroup==2] <- z1fsmk2[dadgroup==2]
      z1fsmk[dadgroup==3] <- z1fsmk3[dadgroup==3]
      
      #for fathers that do work, simulate the number of hours they work
      #3 normal models are used
      if (iteration<=5) {
        fsmoke.pre <- predSimNormsSelect3Models(dadgroup, models$fsmoke.a2_5.dg1, models$fsmoke.a2_5.dg2, models$fsmoke.a2_5.dg3)
      } else if (iteration>5) {
        fsmoke.pre <- predSimNormsSelect3Models(dadgroup, models$fsmoke.a6_13.dg1, models$fsmoke.a6_13.dg2, models$fsmoke.a6_13.dg3)
      }
      fsmoke <- fsmoke.pre
      fsmoke[z1fsmk==0] <- 0
      fsmoke <- round(fsmoke)
      fsmoke[fsmoke < 0] <- 0
      fsmoke[fsmoke > 70] <- 70
      
      #generate fsmoke propensities for scenario testing
      fsmokemodels <- PropensityModels[["fsmoke"]]
      fsmokePropensities <- predictOrdinal(fsmokemodels, NUMCHILDREN, stochastic=TRUE)
      
      #CALIBRATION
      mult.factor <- c(NA, .924, .957, .959, .959, .985, .975, .976, .966, .959, .960, .954, .954, rep(1, 8))
      prop0 <- mean(fsmoke==0)*mult.factor[iteration]
      f.bin <- bin(fsmoke, binbreaks$fsmoke)
      tab <- table(f.bin)/sum(table(f.bin))
      w <- tab[2:4]
      w2 <- w/sum(w)
      props1 <- (1-prop0)*w2
      calib.cat.adjust <- c(prop0, props1)
      fsmoke <- adjustContVarCalib(fsmoke, "fsmoke", propens=fsmokePropensities, desiredProps=calib.cat.adjust, simenv = simenv, iteration = iteration)
      
      #scenarios
      fsmoke <- adjustContVar(fsmoke, "fsmoke", propens=fsmokePropensities[,-ncol(fsmokePropensities)], simenv = simenv, iteration = iteration)
      checkNAs(fsmoke)
    
    
    msmoke <<- msmoke
    fsmoke <<- fsmoke
  }
  
  
  simulate_health_service_use <- function() {
    #gptotvis
    if (iteration == 2) {
      gptotvis <<- predSimNBinom(models$gptotvis2)	
    } else if (iteration >=3 & iteration <= 5) {
      gptotvis <<- predSimNBinom(models$gptotvis3_5)
    } else if (iteration == 6) {
      gptotvis <<- predSimNBinom(models$gptotvis6)
    } else if (iteration >=7 & iteration <= 10) {
      gptotvis <<- predSimNBinom(models$gptotvis7_10)
    } else {
      gptotvis  <<- NAs
    }
    gptotvis[gptotvis>46] <<- 46
    
    
    #hadmtot
    if (iteration <= 5) {
      hadmtot <<- predSimPois(models$hadmtot2_5)
    } else if (iteration >=6 & iteration<=10) {
      hadmtot <<- predSimPois(models$hadmtot6_10)
    } else {
      hadmtot <<- NAs
    }
    hadmtot[hadmtot>6] <<- 6
    
    
    #houtptot
    if (iteration >= 2 & iteration <= 5) {
      houtptot <<- predSimNBinom(models$houtptot2_5)
    } else if (iteration >= 6 & iteration <= 10) {
      houtptot <<- predSimNBinom(models$houtptot6_10)
    } else {
      houtptot <<- NAs
    }
    houtptot[houtptot>27] <<- 27
    
    
    #gpresp
    if (iteration >= 2 & iteration<=5) {
      alpha <- 0.3716 + 0.1608*iteration
      gpresp <<- predSimNBinom(models$gpresp2_5, alpha=alpha)
      gpresp[gpresp>17] <<- 17
    } else if (iteration >=6 & iteration<=10) {
      alpha <- -.1989 + .225*iteration
      gpresp <<- predSimNBinom(models$gpresp6_10, alpha=alpha)
      gpresp[gpresp>17] <<- 17
    } else {
      gpresp <<- NAs
    }
    
    
    #gpmorb
    if (iteration == 2) {
      gpmorb <<- predSimNBinom(models$gpmorb2)
      gpmorb[gpmorb>23] <<- 23
    } else if ((iteration>=3) & (iteration<=4)) {
      gpmorb <<- predSimNBinom(models$gpmorb3_4)
      gpmorb[gpmorb>23] <<- 23
    } else if (iteration==5) {
      gpmorb <<- predSimNBinom(models$gpmorb5)
      gpmorb[gpmorb>23] <<- 23
    } else if ((iteration>=6) & (iteration<=7)) {
      gpmorb <<- predSimNBinom(models$gpmorb6_7)
      gpmorb[gpmorb>23] <<- 23
    } else {
      gpmorb <<- NAs
    }
    
    
    #gpprev
    if (iteration == 2) {
      gpprev <<- predSimNorm(models$gpprev2)
      gpprev[gpprev < 0.7] <<- 0
      gpprev[gpprev >= 0.7 & gpprev < 1.3] <<- 1
      gpprev[gpprev >= 1.3 & gpprev < 2.7] <<- 2
      gpprev[gpprev >= 2.7 & gpprev < 4.5] <<- 3
      gpprev[gpprev>=4.5] <<- 4
      #assert(gpprev < 4.5)
    } else if (iteration %in% c(3,4)) {
      gpprev <<- predSimPois(models$gpprev3_4)
    } else if (iteration==5) {
      gpprev <<- predSimPois(models$gpprev5)
    } else if (iteration >= 6 & iteration <= 7) {
      gpprev <<- predSimPois(models$gpprev6_7)
      #NB better fit but model couldn't etsimate parameters
    } else {
      gpprev <<- NAs
    }
    gpprev[gpprev>6] <<- 6
    
  }
  
  simulate_conduct <- function() {
    if (iteration < 4) {
      z1condLvl1  <<- NAs
    } else if (iteration == 4) {
      mean_mhrswrk1_3 <<- apply(outcomes$mhrswrk[,1:3], ROW, mean) 
      mean_welfare1_3 <<- apply(outcomes$welfare[,1:3], ROW, mean) 
      mean_z1homeown1_3 <<- apply(outcomes$z1homeown[,1:3], ROW, mean) 
      #mean_msmoke1_3 <<- apply(outcomes$msmoke[,1:3], ROW, mean) 
      z1condLvl1 <<- predSimBinom(models$cond4)
    } else if (iteration==5) {
      z1condLvl1 <<- predSimBinomsSelect_notChangeScores(z1cond_previousLvl1, models$cond.prev0.5, models$cond.prev1.5)
    } else if (iteration>=6 & iteration<=13) {
      z1condLvl1 <<- predSimBinomsSelect_notChangeScores(z1cond_previousLvl1, models$cond.prev0.6_13, models$cond.prev1.6_13)
    } else {
      z1condLvl1 <<- NAs
    }
    z1condLvl1 <<- adjustCatVar(z1condLvl1, "z1cond", simenv = simenv, iteration = iteration)
  }
  
  simulate_reading <- function() {
    if (iteration < 8) {
      burt  <<- NAs
    } else if (iteration == 8) {
      mean_z1single1_7 <<- apply(outcomes$z1single[,1:7], ROW, mean)
      mean_fsmoke1_7 <<- apply(outcomes$fsmoke[,1:7], ROW, mean)
      mean_welfare1_7 <<- apply(outcomes$welfareLvl1[,1:7], ROW, mean) 
      burt <<- predSimNorm(models$burt8)
      burt <<- round(burt)
      burt[burt<0] <<- 0
      burt[burt>110] <<- 110
    } else if (iteration >= 9 & iteration <= 13) {
      burt_delta <- predSimNorm(models$burt9_13)
      burt <<- burt_previous + burt_delta
      burt <<- round(burt)
      burt[burt<0] <<- 0
      burt[burt>110] <<- sample(100:110, 1)
    } else {
      burt  <<- NAs
    }
  }
  
  simulate_interact_punish_npresch <- function() {
    if (iteration<5){
      INTERACT <<- NAs
      PUNISH <<- NAs
      NPRESCH <<- NAs
    } else if (iteration==5) {
      #NPRESCH
      mean_householdsize1_5 <<- apply(outcomes$householdsize[,1:5], ROW, mean)
      mean_z1accom1_5 <<- apply(outcomes$z1accomLvl1[,1:5], ROW, mean)
      mean_chres1_5 <<- apply(outcomes$chres[,1:5], ROW, mean) 
      mean_msmoke1_5 <<- apply(outcomes$msmoke[,1:5], ROW, mean) 
      tempNPRESCH <- predSimNorm(models$NPRESCH)
      tempNPRESCH <- round(tempNPRESCH)
      tempNPRESCH[tempNPRESCH<0] <- 0
      tempNPRESCH[tempNPRESCH>3] <- 3
      NPRESCH <<-tempNPRESCH
      
      #INTERACT
      mean_kids1_5 <<- apply(outcomes$kids[,1:5], ROW, mean)
      mean_mhrswrk1_5 <<- apply(outcomes$mhrswrk[,1:5], ROW, mean)
      mean_welfare1_5 <<- apply(outcomes$welfareLvl1[,1:5], ROW, mean) 
      mean_z1accom1_5 <<- apply(outcomes$z1accomLvl1[,1:5], ROW, mean) 
      mean_z1overcrowd1_5 <<- apply(outcomes$z1overcrowdLvl1[,1:5], ROW, mean) 
      mean_z1chpar1_5 <<- apply(outcomes$z1chparLvl1[,1:5], ROW, mean) 
      mean_chres1_5 <<- apply(outcomes$chres[,1:5], ROW, mean) 
      INTERACT.pre <- predSimNBinom(models$INTERACT)
      tempINTERACT <- 10 - INTERACT.pre
      tempINTERACT[tempINTERACT<0] <- 0
      tempINTERACT[tempINTERACT>10] <- 10
      INTERACT <<- tempINTERACT
      
      #PUNISH
      mean_mhrswrk1_5 <<- apply(outcomes$mhrswrk[,1:5], ROW, mean)
      mean_z1homeown1_5 <<- apply(outcomes$z1accomLvl1[,1:5], ROW, mean) 
      mean_z1chpar1_5 <<- apply(outcomes$z1chparLvl1[,1:5], ROW, mean) 
      tempPUNISH <- predSimPois(models$PUNISH)
      tempPUNISH[tempPUNISH<0] <- 0
      tempPUNISH[tempPUNISH>5] <- 5	
      PUNISH <<- tempPUNISH
      
    }
    
    z1INTERACTLvl1 <<- ifelse(INTERACT > 6, 1, 0)
    z1PUNISHLvl1 <<- ifelse(PUNISH > 1, 1, 0)
    
  }
  
  
  simulate_Sleep <- function(){
    
    
    sleepTime <- numeric(5000)
    r1Sleep <- numeric(5000)
    
    if(iteration>=2 & iteration<=3) {			
      #browser()
      
      sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0), 10.5, 0.6)
      sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1), 10.5 - 9/60, 0.6)
      
      r1Sleep[sleepTime>11.5] <- 3
      r1Sleep[sleepTime<=11.5 & sleepTime>=9.5] <- 2
      r1Sleep[sleepTime<9.5] <- 1
      
      
    } else if(iteration>=4 & iteration<=5) {			
      
      
      sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0), 12, 0.6)
      sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1), 12 - 11/60, 0.6)
      
      
      r1Sleep[sleepTime>13] <- 3
      r1Sleep[sleepTime<=13 & sleepTime>=11] <- 2
      r1Sleep[sleepTime<11] <- 1	
      
      
    } else if(iteration>=6 & iteration<=7) {			
      
      sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0), 10, 0.6)
      sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1), 10 - 16 /60, 0.6)
      
      
      r1Sleep[sleepTime>11] <- 3
      r1Sleep[sleepTime<=11& sleepTime>=9] <- 2
      r1Sleep[sleepTime<9] <- 1
      
      
      
    } else if(iteration>=8 & iteration<=9) {			
      #browser()
      
      sleepTime<- rnorm(5000, 10, 0.6)	
      
      
      r1Sleep[sleepTime>11] <- 3
      r1Sleep[sleepTime<=11 & sleepTime>=9] <- 2
      r1Sleep[sleepTime<9] <- 1
      
      
    } else if(iteration>=10 & iteration<=19) {			
      #browser()
      
      sleepTime <- rnorm(5000, 8.875, 0.6)
      
      
      r1Sleep[sleepTime>10] <- 3
      r1Sleep[sleepTime<=10 & sleepTime>=8] <- 2
      r1Sleep[sleepTime<8] <- 1	
      
    } else {		
      
      r1Sleep <- NAs  
      
    }	
    
    r1Sleep <<- r1Sleep
    
    
    r1SleepLvl1 <<- ifelse(r1Sleep == 1, 1, 0)
    r1SleepLvl2 <<- ifelse(r1Sleep == 2, 1, 0)		
    r1SleepLvl3 <<- ifelse(r1Sleep == 3, 1, 0)
  }
  
  
  simulate_BMI <- function(){
    
    bmiSD <- apply(children[,c( names(children)[grep("^BMI", names(children))])], 2, function(x) 
      tapply(x, children$z1gender, sd)) 
    
    bmiInter <- bmiSD * 0.81 * 0.4456
    
    z1WatchTVLvl1 <<- adjustCatVar(z1WatchTVLvl1, "z1WatchTVLvl1", simenv = simenv, iteration = iteration)
    
    # if( iteration >=3 & iteration <=12){
    #   BMI <<- apply(cbind(children[,c(paste0("BMI", iteration), "z1gender") ], z1WatchTVLvl1) , 1,
    #                 function(x)  ifelse(x[2]==1,
    #                                     rnorm(1, mean = bmiInter[2, iteration-1] + x[1] - bmiSD[2, iteration-1]*0.81 * x[3], sd = sdBMI[iteration-1, 1]/100),
    #                                     rnorm(1, mean = bmiInter[1, iteration-1] + x[1] - bmiSD[1, iteration-1]*0.81 * x[3], sd = sdBMI[iteration-1, 2]/100)))
    #   
    # }else { 
      BMI <<- apply(children[,c(paste0("BMI", iteration), "z1gender") ], 1,
                    function(x)  ifelse(x[2]==1,
                                        rnorm(1, mean = x[1], sd = sdBMI[iteration-1, 1]/50),
                                        rnorm(1, mean = x[1], sd = sdBMI[iteration-1, 2]/50)))
    # }
    
      
      
    z1OverweightBMILvl1 <<- numeric(5000) 
    z1ObeseLvl1 <<- numeric(5000) 
    
    z1OverweightBMILvl1[children$z1gender==1] <<- 
      ifelse(BMI[children$z1gender==1] >= overweightCutoff[iteration-1, 1],1,0)
    z1OverweightBMILvl1[children$z1gender==0] <<- 
      ifelse(BMI[children$z1gender==0] >= overweightCutoff[iteration-1, 2],1,0)
    
    z1ObeseLvl1[children$z1gender==1] <<- 
      ifelse(BMI[children$z1gender==1] >= obeseCutoff[iteration-1, 1],1,0)
    z1ObeseLvl1[children$z1gender==0] <<- 
      ifelse(BMI[children$z1gender==0] >= obeseCutoff[iteration-1, 2],1,0)
    
  }
  
  
  simulate_childrenOverweight <- function() {  	  
    
    # if(iteration<19)	  
    #   r1Sleep <<- get(paste("r1SleepA", iteration, sep =""))
    
    r1Sleep <- adjustCatVar(r1Sleep, "r1Sleep", simenv = simenv, iteration = iteration)
    
    r1SleepLvl1 <- ifelse(r1Sleep == 1, 1, 0)
    r1SleepLvl2 <- ifelse(r1Sleep == 2, 1, 0)		
    r1SleepLvl3 <- ifelse(r1Sleep == 3, 1, 0)
    
    
    z1breastLvl1 <- ifelse(BREAST == 0, 0, 1)
    z1pregsmkLvl1 <- ifelse(pregsmk == 0, 0, 1)		
    
    r1ParentEducLvl1 <- ifelse(r1ParentEduc == 1, 1, 0)
    r1ParentEducLvl2 <- ifelse(r1ParentEduc == 2, 1, 0)		
    r1ParentEducLvl3 <- ifelse(r1ParentEduc == 3, 1, 0)
    
    if(iteration>=2 ) {			
      
      z1OverweightLvl1 <<- 
        predSimBinom(models[[paste("z1OverweightA", iteration, sep = "")]])	   
      
    } else {		
      z1OverweightLvl1 <<- NAs     
      
    }	
    
    #browser()
  }
  
  
  
  simulate_IQ <- function() {	 	
    
    z1GALvl1 <- as.integer(ga<37)
    
    if(iteration == 2){
      IQ <- predSimNorm(models$IQA2)	
      
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)
    }else if (iteration == 3){
      
      IQ <- predSimNorm(models$IQA3)
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)
      
    }else if (iteration == 4){
      
      IQ_previous1 <<- outcomes$IQ[,2]
      
      IQ <- predSimNorm(models$IQA4)
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)
      
      
    }else if (iteration == 5){
      
      IQ_previous2 <<- outcomes$IQ[,2]   
      IQ_previous1 <<- outcomes$IQ[,3]
      
      IQ <- predSimNorm(models$IQA5)
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)		   
      
    }else if (iteration == 6){
      
      
      IQ_previous3 <<- outcomes$IQ[,2]   
      IQ_previous2 <<- outcomes$IQ[,3]   
      IQ_previous1 <<- outcomes$IQ[,4]
      
      IQ <- predSimNorm(models$IQA6)
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)		   
      
      
    }else if (iteration == 7){
      IQ_previous4 <<- outcomes$IQ[,2]      
      IQ_previous3 <<- outcomes$IQ[,3]   
      IQ_previous2 <<- outcomes$IQ[,4]   
      IQ_previous1 <<- outcomes$IQ[,5]
      
      IQ <- predSimNorm(models$IQA7)
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)		   
      
      
      
    }else if (iteration == 8){
      IQ_previous5 <<- outcomes$IQ[,2]
      IQ_previous4 <<- outcomes$IQ[,3]      
      IQ_previous3 <<- outcomes$IQ[,4]   
      IQ_previous2 <<- outcomes$IQ[,5]   
      IQ_previous1 <<- outcomes$IQ[,6]
      
      IQ <- predSimNorm(models$IQA8)
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)		   
      
      
    }else if (iteration == 9){
      
      IQ_previous6 <<- outcomes$IQ[,2]			
      IQ_previous5 <<- outcomes$IQ[,3]
      IQ_previous4 <<- outcomes$IQ[,4]      
      IQ_previous3 <<- outcomes$IQ[,5]   
      IQ_previous2 <<- outcomes$IQ[,6]   
      IQ_previous1 <<- outcomes$IQ[,7]
      
      
      IQ <- predSimNorm(models$IQA9)
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)		   
      
      
    }else if (iteration == 10){
      
      IQ_previous7 <<- outcomes$IQ[,2]			
      IQ_previous6 <<- outcomes$IQ[,3]			
      IQ_previous5 <<- outcomes$IQ[,4]
      IQ_previous4 <<- outcomes$IQ[,5]      
      IQ_previous3 <<- outcomes$IQ[,6]   
      IQ_previous2 <<- outcomes$IQ[,7]   
      IQ_previous1 <<- outcomes$IQ[,8]
      
      IQ <- predSimNorm(models$IQA10)
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)		
      
    }		  else if(iteration < 17 ) {	
      
      IQ_previous8 <<- outcomes$IQ[,iteration-8]			
      IQ_previous7 <<- outcomes$IQ[,iteration-7]			
      IQ_previous6 <<- outcomes$IQ[,iteration-6]			
      IQ_previous5 <<- outcomes$IQ[,iteration-5]
      IQ_previous4 <<- outcomes$IQ[,iteration-4]
      IQ_previous3 <<- outcomes$IQ[,iteration-3]   
      IQ_previous2 <<- outcomes$IQ[,iteration-2]   
      IQ_previous1 <<- outcomes$IQ[,iteration-1]
      
      IQ <- predSimNorm(models$IQA11)
      
      IQ <<- scale(IQ)* runif(1,14.5,15.5) + mean(IQ)
      
    } 
  }
  
  
  
  simulate_Score <- function() {	 		 
    
    if(iteration == 17){
      
      currectScore <- NAs
      
      currectScore[r1stchildethn != 2] <- predSimNorm(models$ScoreA17, 
                                                      set = r1stchildethn != 2)	 
      
      currectScore[r1stchildethn == 2] <- predSimNorm(models$ScoreA17Ethn2,
                                                      set = r1stchildethn == 2)	 		
      
      Score <<- currectScore
      
      z1ScoreLvl1 <<- ifelse(currectScore > 88, 1,0)
      
      z1FailLvl1 <<- ifelse(currectScore <= 78.5, 1,0)
      
      z1DropLvl1 <<- ifelse(currectScore > 78.5 & currectScore <= 88, 1,0)
      
    } 
  }
  
  
  simulate_NEET <- function() {	 		 
    
    if(iteration >=16 & iteration <= 21){
      
      z1NEETLvl1[z1genderLvl1 == 0] <<- 
        predSimBinom(models[[paste("z1NEETGender0A", iteration, sep = "")]], set = z1genderLvl1 == 0)	   
      z1NEETLvl1[z1genderLvl1 == 1] <<- 
        predSimBinom(models[[paste("z1NEETGender1A", iteration, sep = "")]], set = z1genderLvl1 == 1)	
      
    } else {
      z1NEETLvl1 <<- NAs
    }  
    
  }
  
  simulate_Bully <- function() {	 
    
    if(iteration < 13){
      z1BullyLvl1 <<- NAs
      
    } else if(iteration >=13 & iteration <= 17){
      
      z1BullyLvl1 <<- 
        predSimBinom(models[[paste("z1BullyA", iteration, sep = "")]])	   
      
    } 
    
  }
  
  simulate_AlcAbuse <- function() {	 		 
    
    z1PUNISHLvl1 <<- adjustCatVar(z1INTERACTLvl1, "z1PUNISHLvl1", simenv = simenv, iteration = iteration)
    
    fage_imputed[fage_imputed ==99] <- NA
    
    male = c(11.5, 43.2, 28.6, 27.1, 25.4, 20.8, 14.8, 5.4)/100
    female = c(10.1, 23.6, 15.5, 12.5, 11.7, 6.1, 3.1, 0.8)/100
    
    newMage <- MAGE+iteration
    newFage <- fage_imputed+iteration
    
    breaks <- c(14, 17, 24, 34, 44, 54, 64, 74, Inf)
    
    len <- table(bin(newMage, breaks= breaks), bin(newFage, breaks = breaks))
    interval <- cbind(breaks[-length(breaks)], breaks[-1])
    
    corMat <-  matrix(c(1,0.12,0.12,1), ncol = 2, nrow = 2)
    
    z1FatherAlcLvl1 <-rep(NA, 5000)
    z1MotherAlcLvl1 <-rep(NA, 5000)
    
    for(i in 1:nrow(len)){
      indexMage <- (newMage > interval[i,1] &  newMage <= interval[i,2]) & is.na(newFage)
      z1MotherAlcLvl1[which(indexMage)] <- rbinom(sum(indexMage), 1, female[i])
      
      for(j in 1:ncol(len)){
        
        if(len[i,j] == 0)
          next
        
        index <- (newMage > interval[i,1] &  newMage <= interval[i,2]) &  
          (newFage > interval[j,1] &  newFage <= interval[j,2])
        
        temp <- rmvbin(len[i,j], margprob = c(female[i], male[j]), bincorr =corMat)
        
        z1MotherAlcLvl1[which(index)] <- temp[,1]
        z1FatherAlcLvl1[which(index)] <- temp[,2]
      }
    }
    
    z1MotherAlcLvl1 <<- z1MotherAlcLvl1
    z1FatherAlcLvl1 <<- z1FatherAlcLvl1
    
    #browser()

    z1ParentAlcLvl1 <<- apply(cbind(z1MotherAlcLvl1, z1FatherAlcLvl1), 1, max, na.rm = TRUE)
    

    if( iteration > 2 ){
      temp <- t(chol(matrix(c(1,0.96, 0.96,1), ncol = 2))) %*% rbind(z1ParentAlcPreLvl1, z1ParentAlcLvl1)
      
      temp <- temp[2,]
      temp[temp>1] <- 1
      
      temp <- temp * mean(z1ParentAlcLvl1)/mean(temp)
      
      z1ParentAlcLvl1 <- sapply(temp, function(x) rbinom(1,1,x))
      
    }
    
    z1ParentAlcLvl1 <<- adjustCatVar(z1ParentAlcLvl1, "z1ParentAlcLvl1", 
                                     simenv = simenv, iteration = iteration)
    
    
    if(iteration >=15 & iteration <= 21){
           
      z1AlcAbuseLvl1 <<- 
        predSimBinom(models[[paste("z1AlcAbuseA", iteration, sep = "")]])	   
      
      
    } else      {
      z1AlcAbuseLvl1 <<- NAs
    }  
    
  }
  
  simulate_Depress <- function() {	 		 
    
    z1BullyLvl1 <<- adjustCatVar(z1BullyLvl1, "z1BullyLvl1", simenv = simenv, iteration = iteration)
    z1INTERACTLvl1 <<- adjustCatVar(z1INTERACTLvl1, "z1INTERACTLvl1", simenv = simenv, iteration = iteration)
    z1PUNISHLvl1 <<- adjustCatVar(z1PUNISHLvl1, "z1PUNISHLvl1", simenv = simenv, iteration = iteration)
    
    breaks <- c(14, 24, 34, 44, 54, 64, 74, Inf)
    
    fage_imputed[fage_imputed ==99] <- NA
    
    
    male = c(5.4, 9.2, 11.8, 13.3, 13.3, 10.4, 7.6)/100
    female = c(10.9, 18.8, 22.3, 21.6, 22.4, 19.3, 13.4)/100
    
    newMage <- MAGE+iteration
    newFage <- fage_imputed+iteration
    
    len <- table(bin(newMage, breaks= breaks), bin(newFage, breaks = breaks))
    interval <- cbind(breaks[-length(breaks)], breaks[-1])
    
    corMat <-  matrix(c(1,0.12,0.12,1), ncol = 2, nrow = 2)
    
    
    z1FatherDepressLvl1 <-rep(NA, 5000)
    z1MotherDepressLvl1 <-rep(NA, 5000)
    
    
    for(i in 1:nrow(len)){
      indexMage <- (newMage > interval[i,1] &  newMage <= interval[i,2]) & is.na(newFage)
      z1MotherDepressLvl1[which(indexMage)] <- rbinom(sum(indexMage), 1, female[i])
      
      for(j in 1:ncol(len)){
        
        if(len[i,j] == 0)
          next
        
        index <- (newMage > interval[i,1] &  newMage <= interval[i,2]) &  
          (newFage > interval[j,1] &  newFage <= interval[j,2])
        temp <- rmvbin(len[i,j], margprob = c(female[i], male[j]), bincorr =corMat)
        
        z1MotherDepressLvl1[which(index)] <- temp[,1]
        z1FatherDepressLvl1[which(index)] <- temp[,2]
      }
    }
    
    z1MotherDepressLvl1 <<- z1MotherDepressLvl1
    z1FatherDepressLvl1 <<- z1FatherDepressLvl1
    
    z1ParentDepressLvl1 <<- apply(cbind(z1MotherDepressLvl1, z1FatherDepressLvl1),1, max, na.rm=TRUE)
    
    if( iteration > 2 ){
      temp <- t(chol(matrix(c(1,0.93, 0.93,1), ncol = 2))) %*% 
        rbind(z1ParentDepressPreLvl1, z1ParentDepressLvl1)
      
      temp <- temp[2,]
      temp[temp>1] <- 1
      
      temp <- temp * mean(z1ParentDepressLvl1)/mean(temp)
      
      z1ParentDepressLvl1 <- sapply(temp, function(x) rbinom(1,1,x))
      
    }
    
    z1ParentDepressLvl1 <<- adjustCatVar(z1ParentDepressLvl1, "z1ParentDepressLvl1", 
                                         simenv = simenv, iteration = iteration)
    
    
    if(iteration >=15 & iteration <= 21){
          
      z1DepressLvl1 <<- 
        predSimBinom(models[[paste("z1DepressA", iteration, sep = "")]])	 
  
    } else {
      z1DepressLvl1 <<- NAs
    }  
    
  }
  
  pre_simulation_setup <- function() {
    
    # setup constants
    NUMCHILDREN <<- length(A0)
    NAs <<- rep(NA, NUMCHILDREN)
    
    # the propADDHS model uses MEDUC vars which are the inverse of r1stmeduc
    # since r1stmeduc can be modified for scenario testing, we will need
    # to set MEDUC to its (potentially modified) value here
    MEDUCLvl1 <<- r1stmeducLvl3
    MEDUCLvl2 <<- r1stmeducLvl2
    MEDUCLvl3 <<- r1stmeducLvl1
    
    #calculate once at the beginning and use in simulate_family_household  		
    fage_years1_grouped <<- cut(fage_years1, binbreaks$fage_years, labels=FALSE) 
    fage_years1_grouped_same_as_fage <<- fage_years1_grouped == fage
    fage_years1_grouped_same_as_fage[is.na(fage_years1_grouped_same_as_fage)] <<- FALSE
    mage_years1_same_as_MAGE <<- abs(mage_years1 - MAGE) < 2
    
    mage_years <<- MAGE + 1
    #when look at collated means - can see the effect from y2 onwards but doesn't show effect for y1
    
    sdBMI <<-  matrix(c( 2.318561061,	2.207931975,
                         2.116476357,	2.066416278,
                         2.057399303,	2.1583464,
                         2.33275033,	2.496312829,
                         2.636535808,	2.725100429,
                         3.116483382,	3.141235808,
                         3.564080752,	3.635437662,
                         4.02828569,	4.222055377,
                         4.405528134,	4.741941784,
                         4.763114459,	5.269560519,
                         4.962362565,	5.514722333,
                         5.342412979,	5.929690456,
                         5.428023871,	6.013529644,
                         5.670758792,	6.221955633,
                         5.771324301,	6.284254227,
                         5.683160601,	6.219164444,
                         5.708957279,	6.37075478,
                         5.779369385,	6.584913605,
                         5.627971777,	6.502829121,
                         5.453944955,	6.38980461), ncol = 2, byrow = TRUE)
    
    
    overweightCutoff <<-
      matrix(
        c(c(18.4, 17.9, 17.6, 17.4, 17.6, 17.9, 18.4, 19.1, 19.8, 20.6, 21.2, 21.9, 22.6, 23.3, 23.9, 24.5, rep(25, 4)),
          c(18, 17.6, 17.3, 17.1, 17.3, 17.8, 18.3, 19.1, 19.9, 20.7, 21.7, 22.6, 23.3, 23.9, 24.4, 24.7, rep(25, 4))),
        ncol = 2)

    obeseCutoff <<-
      matrix(c(20.1, 19.6, 19.3, 19.3, 19.8, 20.6, 21.6, 22.8, 24, 25.1, 26, 26.8, 27.6, 28.3, 28.9, 29.4, rep(30, 4)),
             c(20.1, 19.4, 19.1, 19.2, 19.7, 20.5, 21.6, 22.8, 24.1, 25.4, 26.7, 27.8, 28.6, 29.1, 29.4, 29.7, rep(30, 4)),
             ncol = 2)
    
    
    
    school <- apply(transition_probabilities$r1School, 1, function(x) sample(1:100, 1, prob = x))
    
    
    
    r1SchoolFunding <<-
      c(2, 2, 0, 2, 2, 2, 2, 0, 1, 2, 0, 2, 2, 1, 2, 0, 2, 2, 2, 
        2, 0, 2, 2, 0, 2, 2, 2, 2, 2, 0, 2, 2, 1, 2, 2, 1, 2,
        2, 2, 2, 2, 2 ,2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
        2, 1, 2, 2, 2, 2, 2, 2, 0, 1, 2, 2, 2, 2, 2, 2, 2, 1,
        2, 2, 2, 2, 2, 2, 0, 2 ,2, 2, 2, 2, 2, 2, 2, 0, 2, 1, 2, 
        1, 2, 1, 2, 1, 1, 2)[school]
    
    r1SchoolFundingLvl1 <<- ifelse(r1SchoolFunding == 1, 1,0)
    r1SchoolFundingLvl2 <<- ifelse(r1SchoolFunding == 2, 1,0)
    
    
    r1School <<- school
    

    z1WatchTVLvl1 <<- ifelse(rpois(5000, 1.52) >= 2, 1,0)
    
    
    
  }
  
  
  store_current_values_in_outcomes <- function(xcol) {
    outcomes <<- lapply(outcomes, function(x) {
      x[,xcol] <- get(attr(x,"varname"));
      x 
    }) 
  }
  
  
  # setup previous vars with values from current vars, 
  # eg: z1msmoke_previousLvl1 will be assigned the value in z1msmokeLvl1
  store_current_values_in_previous <- function() {
    previous <- attr(simenv$simframe, "previous")
    invisible(mapply(function(var.prev, var.current) {
      #var.prev <- previous[1] ; var.current <- names(previous)[1]
      #var.prev <- "z1msmoke_previousLvl1" ; var.current <- "z1msmokeLvl1"
      assign(var.prev, get(var.current), inherits=T)        
    }, previous, names(previous)))
  }
  
  
  # SIMULATION STARTS HERE
  
  
  attach(simenv$simframe, name="simframe")
  NUM_ITERATIONS <- 21
  
  outcomes <- createOutcomeMatrices(simenv$simframe, "years1_21", c(1:NUM_ITERATIONS))
  
  pre_simulation_setup()
  
  store_current_values_in_outcomes(1)
  
  for (iteration in 2:NUM_ITERATIONS) {
    #iteration =5
    
    #if(iteration ==5) browser()
    
    cat("Run", run, "year", iteration, "\n")
    
    # setup vars for this iteration
    store_current_values_in_previous()
    age <<- rep(iteration, NUMCHILDREN)
    age_minus1 <<- rep(iteration-1, NUMCHILDREN)
    age_minus1Lvl2 <- as.integer(age_minus1 == 2)
    age_minus1Lvl3 <- as.integer(age_minus1 == 3)
    
    
    #KNOWLAB models		  
    simulate_Sleep()	
    
    simulate_BMI()
    
    simulate_childrenOverweight()		
    
    simulate_IQ()	  
    # simulate_childrenObese()     
    
    simulate_Score()
    simulate_NEET()
    
    simulate_Bully()
    simulate_AlcAbuse()
    simulate_Depress()
    
    #MELC models	
    simulate_family_household()      
    simulate_psychosocial_factors()      
    simulate_employment()      
    simulate_material_circumstances()      
    simulate_behavioural_factors()      
    simulate_health_service_use()      
    simulate_conduct()      
    simulate_reading()      	  
    
    
    store_current_values_in_outcomes(iteration)
    
    simulate_interact_punish_npresch() 
    #The reason this simulation is here is because they uses values from year 3-5 in  
    #the outcomes, however the values for year 5 has not been stored in the outcomes yet. 
    
    store_current_values_in_outcomes(iteration)
    # store again for INTERACT, PUNISH and NPRESCH, so these three variable still sit on year 5  
    
  }
  
  
  detach("simframe")
  
  #outcomes
  
  lapply(outcomes, function(x) x[,!apply(x,2, function(y) all(is.na(y)))])
}





