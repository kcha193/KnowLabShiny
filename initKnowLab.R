
initSim <- function(num.iterations){
  
  #' List of breaks codings for adjustable continuous variables.  
  #' 
  #' binbreak names cannot have spaces - because in label_flattened_mx_grping.and.CIs() looks
  #' 	to see if there are any spaces to determine whether there is subgrouping or not.  An underscore
  #' 	can be used instead of a space
  #' 
  #' @examples
  #' createBinBreaks(children)
  createBinBreaks <- function(children) {
    binbreaks <- list()
    
    #NB: the very first cut point must be less than min(x)
    #subsequent cut points are the closed right bound,
    #and the final cut point should be max(x)
    # eg: breaks of c(0, 34, 35, 36, 37, 44)
    # will cut into the following bins
    #  (0,34] (34,35] (35,36] (36,37] (37,44] 
    
    binbreaks$pregalc <- c(-1,0,1,2,3,4,5,6,7,max(children$pregalc, na.rm=TRUE))
    names(binbreaks$pregalc) <- c(NA, 0:7, "8+")
    
    binbreaks$pregsmk <- c(-1,0,5,10,15,20,max(children$pregsmk, na.rm=TRUE))
    names(binbreaks$pregsmk) <- c(NA, "0", "1-5", "6-10", "11-15", "16-20", "21+")
    
    binbreaks$bwkg <- c(0,2.499,2.999,3.499,3.999,max(children$bwgrams, na.rm=TRUE))
    names(binbreaks$bwkg) <- c(NA, "<2500", "2500-2999", "3000-3499", "3500-3999", "4000+") 
    
    #binbreaks$ga <- c(0,34,35,36,37,max(children$ga, na.rm=TRUE))
    #names(binbreaks$ga) <- c(NA, "< 35", "35", "36", "37", "38+")
    binbreaks$ga <- c(0,34,35,36,42,max(children$ga, na.rm=TRUE))
    names(binbreaks$ga) <- c(NA, "<35", "35", "36", "37-42", "43+")
    
    binbreaks$BREAST <- c(-1:11, max(children$BREAST, na.rm=TRUE))
    names(binbreaks$BREAST) <- c(NA, 0:11, "12+")
    #names(binbreaks$BREAST) <- c(NA, names(dict.MELC$codings$BREAST))
    #table(bin(children$ga,binbreaks$ga))
    
    binbreaks$INTERACT <- c(-1, 2:10)
    names(binbreaks$INTERACT) <- c(NA, "<3", 3:10)
    
    binbreaks$NPRESCH <- c(-1:3)
    names(binbreaks$NPRESCH) <- c(NA, 0:3)
    
    binbreaks$PUNISH <- c(-1:5)
    names(binbreaks$PUNISH) <- c(NA, 0:5)
    
    binbreaks$MAGE <- c(0, 19, 24, 29, 34, 39, 98) 
    names(binbreaks$MAGE) <- c(NA, "<20", "20-24", "25-29", "30-34", "35-39", "40+")
    
    binbreaks$fage_years <- c(0, 19, 24, 29, 34, 39, 80, 999) 
    #names(binbreaks$fage_years) <- c(NA, "< 20", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40+", "No father")
    names(binbreaks$fage_years) <- c(NA, "1", "2", "3", "4", "5", "6", "7")
    
    #binbreaks$MAGE.4cat <- c(0, 19, 24, 29, 98)
   # names(binbreaks$MAGE.4cat) <- c(NA, "<20", "20 -24", "25-29", "30+")
    
    ##binbreaks$fage_years <- c(0, 19, 24, 29, 34, 39, 98)  
    ##names(binbreaks$fage_years) <- c(NA, "< 20", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40+")
    #structure(cut(c(0,1,18:41,98,99), binbreaks$fage_years), .Names=c(0,1,18:41,98,99))
    #cut(c(0,1,18:41,98,99), binbreaks$fage_years, labels=FALSE)
    
    binbreaks$chres <- c(-1, 0, 1, 999)
    names(binbreaks$chres) <- c(NA, "0", "1", "2+")
    
    binbreaks$kids <- c(0, 1, 2, 3, 4, 999)
    names(binbreaks$kids) <- c(NA, "1", "2", "3", "4", "5+")
    
    binbreaks$householdsize <- c(0, 2, 3, 4, 5, 999)
    names(binbreaks$householdsize) <- c(NA, "2", "3", "4", "5", "6+")
    
    binbreaks$msmoke <- c(-1, 0, 10, 20, 999)
    names(binbreaks$msmoke) <- c(NA, "0", "1-10", "11-20", "21+")
    
    binbreaks$fsmoke <- c(-1, 0, 10, 20, 999)
    names(binbreaks$fsmoke) <- c(NA, "0", "1-10", "11-20", "21+")
    
    binbreaks$mhrswrk <- c(-1, 0, 10, 20, 30, 40, 999)
    names(binbreaks$mhrswrk) <- c(NA, "0", "1-10", "11-20", "21-30", "31-40", "41+")
    
    binbreaks$fhrswrk <- c(-1, 0, 20, 35, 40, 45, 50, 999)
    names(binbreaks$fhrswrk) <- c(NA, "0", "1-20", "21-35", "36-40", "41-45", "46-50", "51+")
    
    binbreaks$INTERACT <- c(-1,2,3,4,5,6,7,8,9,10)
    names(binbreaks$INTERACT) <- c(NA, "<3", 3:10)
    
    binbreaks$PUNISH <- c(-1,0,1,2,3,4,5)
    names(binbreaks$PUNISH) <- c(NA, 0:5)
    
    binbreaks$NPRESCH <- c(-1,0,1,2,3)
    names(binbreaks$NPRESCH) <- c(NA, 0:3)
    
    binbreaks
  }
  
  #' Create logical arrays of certain children subsets
  #' 
  #' @examples
  #' childsets <- createChildSets(children) 
  createChildSets <- function(children, codings) {
    
    childsets <- list()
    childsets$females <- children$z1gender == codings$z1gender["Female"] 
    childsets$males <- children$z1gender == codings$z1gender["Male"]
    childsets$pacific <- children$r1stchildethn == codings$r1stchildethn["Pacific"]
    childsets$maori <- children$r1stchildethn == codings$r1stchildethn["Maori"]
    attr(childsets$females, "desc") <- "females only"
    attr(childsets$males, "desc") <- "males only"
    attr(childsets$pacific, "desc") <- "Pacific only"
    attr(childsets$maori, "desc") <- "Maori only"
    
    childsets
  }
  
  
  
  #' Load base file and perform post-processing.
  #' 
  #' @param basefiledir
  #'  directory holding base file
  #' @param basefilename
  #'  name of basefile
  #' 
  #' @examples
  #' basefiledir <- "D:/workspace.sim/MELC/CHDS/base/"
  #' basefilename <- "basefile20110805plusVersion3.csv"
  #' basefilename <- "basefile20111116.csv"
  #' children <- loadBaseFileCSV(basefiledir, basefilename)
  loadBaseFileCSV <- function(basefiledir, basefilename) {
    
    children <- read.csv(paste(basefiledir, basefilename, sep=""))
    
    # setup rownames to represent child identity (A0)
    rownames(children) <- children$A0		
    
    #setup additional basefile vars
    children$weightScenario <- 1
    children$weightBase <- 1
    children$bwgrams <- children$bwkg * 1000
    
    children
    
  }
  
  
  #' Old function - now not used.  Loads and merges propensity files with supplied 
  #' selected values, ie:
  #' returns propensity matrices for the supplied selected values that 
  #' exist in key_column_name of the propensity files.
  #'
  #' 
  #' @param  key_column_name 
  #'  a column in the propensity files to merge on, and select
  #'  those values that appear in selected_keys
  #' 
  #' @param selected_keys
  #'  a vector of selected keys that are to be retained in the propensities
  #' 
  #' @return 
  #' a list of 3D arrays. The Z dim is the iteration/year that the propensity matrix
  #' refers to. The cols of each matrix refer to the categories, with one less
  #' column than the number of categories. The rows refer to a child and
  #' are sorted sorted lexicographically by key_column_name. key_column_name values appear 
  #' as the rownames of the matrices.
  #' 
  #' @examples
  #' selected_keys <- children$A0
  #' key_column_name <- "A0"
  #' propensityfiledir <- "D:/workspace.sim/MELC/CHDS/propensityFiles/"
  #' propensities <- loadMELCPropensities(key_column_name, selected_keys, propensityfiledir)
  loadMELCPropensities <- function(key_column_name, selected_keys, propensityfiledir) {
    
    load2CategoryPropensityArray <- function(propensityfiledir, filename) {
      create2CategoryPropensityArray(loadMergedFile(propensityfiledir, filename, key_column_name, selected_keys))
    }
    
    loadSingleIterationPropensityArray <- function(propensityfiledir, filename, iteration_name) {
      createSingleIterationPropensityArray(loadMergedFile(propensityfiledir, filename, key_column_name, selected_keys), iteration_name)
    }
    
    propens.list <- list()
    
    propens.list$z1accom <- load2CategoryPropensityArray(propensityfiledir, "accom_Propens.csv")
    propens.list$z1chpar <- load2CategoryPropensityArray(propensityfiledir, "chpar_Propens.csv")
    propens.list$z1homeown <- load2CategoryPropensityArray(propensityfiledir, "homeown_Propens.csv")
    propens.list$z1single <- load2CategoryPropensityArray(propensityfiledir, "single_Propens.csv")
    propens.list$welfare <- load2CategoryPropensityArray(propensityfiledir, "welfare_Propens.csv")
    propens.list$SESBTH <- loadSingleIterationPropensityArray(propensityfiledir, "sesbth_Propens.csv", iteration_name="At Birth") 
    
    #propens.list$fhrswrk <- loadSingleIterationPropensityArray(propensityfiledir, "fhrswrk_Propens.csv", iteration_name="At Birth")
    
    propens.list
  }
  
  
  #' Calculate propensities for use in scenario testing from propensity models
  #' Only used in generating the propensity arrays that are used for any presim or year 1 scenarios
  #' Gets the propensity models from the PropensityModels list generated by the 
  #' loadPropensityModels() function.
  #' @return a list of propensities.
  loadMELCPropensities2 <- function(propensityfiledir, stochastic=FALSE) {
    
    num.children <- length(simframe.master[[1]])
    
    #generate msmoke propensities for scenario testing
    SESBTHmodels <- PropensityModels[["SESBTH"]]
    NPRESCHmodels <- PropensityModels[["NPRESCH"]]
    pregalcmodels <- PropensityModels[["pregalc"]]
    fagemodels <- PropensityModels[["fage"]]
    r1stchildethnmodels <- PropensityModels[["r1stchildethn"]]
    r1stmeducmodels <- PropensityModels[["r1stmeduc"]]
    r1stfeducmodels <- PropensityModels[["r1stfeduc"]]
    bwkgmodels <- PropensityModels[["bwkg"]]
    z1single0model <- PropensityModels[["z1single0"]]
    BREASTmodels <- PropensityModels[["BREAST"]]
    INTERACTmodels <- PropensityModels[["INTERACT"]]
    PUNISHmodels <- PropensityModels[["PUNISH"]]
    MAGEmodels <- PropensityModels[["MAGE"]]
    pregsmkmodels <- PropensityModels[["pregsmk"]]
    
    z1accommodel <- PropensityModels[["z1accom"]]
    msmokemodels <- PropensityModels[["msmoke"]]
    fsmokemodels <- PropensityModels[["fsmoke"]]
    kidsmodels <- PropensityModels[["kids"]]
    z1singlemodel <- PropensityModels[["z1single"]]
    z1chparmodel <- PropensityModels[["z1chpar"]]
    chresmodels <- PropensityModels[["chres"]]
    welfaremodel <- PropensityModels[["welfare"]]
    mhrswrkmodels <- PropensityModels[["mhrswrk"]]
    fhrswrkmodels <- PropensityModels[["fhrswrk"]]
    z1homeownmodel <- PropensityModels[["z1homeown"]]
    z1overcrowdmodel <- PropensityModels[["z1overcrowd"]]
    
    #set-up empty arrays for each variable, rows=children, columns=number of models for the variable,
    SESBTHPropensities <- array(dim=c(num.children, length(SESBTHmodels), 1))
    pregalcPropensities <- array(dim=c(num.children, length(pregalcmodels), 1))
    NPRESCHPropensities <- array(dim=c(num.children, length(NPRESCHmodels), 1))
    fagePropensities <- array(dim=c(num.children, length(fagemodels), 1))
    r1stchildethnPropensities <- array(dim=c(num.children, length(r1stchildethnmodels), 1))
    r1stmeducPropensities <- array(dim=c(num.children, length(r1stmeducmodels), 1))
    r1stfeducPropensities <- array(dim=c(num.children, length(r1stfeducmodels), 1))
    bwkgPropensities <- array(dim=c(num.children, length(bwkgmodels), 1))
    z1single0Propensities <- array(dim=c(num.children, length(z1single0model), 1))
    BREASTPropensities <- array(dim=c(num.children, length(BREASTmodels), 1))
    INTERACTPropensities <- array(dim=c(num.children, length(INTERACTmodels), 1))
    PUNISHPropensities <- array(dim=c(num.children, length(PUNISHmodels), 1))
    MAGEPropensities <- array(dim=c(num.children, length(MAGEmodels), 1))
    pregsmkPropensities <- array(dim=c(num.children, length(pregsmkmodels), 1))
    
    z1accomPropensities <- array(dim=c(num.children, length(z1accommodel), 1))
    msmokePropensities <- array(dim=c(num.children, length(msmokemodels), 1))
    fsmokePropensities <- array(dim=c(num.children, length(fsmokemodels), 1))
    kidsPropensities <- array(dim=c(num.children, length(kidsmodels), 1))
    z1singlePropensities <- array(dim=c(num.children, length(z1singlemodel), 1))
    z1chparPropensities <- array(dim=c(num.children, length(z1chparmodel), 1))
    chresPropensities <- array(dim=c(num.children, length(chresmodels), 1))
    welfarePropensities <- array(dim=c(num.children, length(welfaremodel), 1))
    mhrswrkPropensities <- array(dim=c(num.children, length(mhrswrkmodels), 1))
    fhrswrkPropensities <- array(dim=c(num.children, length(fhrswrkmodels), 1))
    z1homeownPropensities <- array(dim=c(num.children, length(z1homeownmodel), 1))
    z1overcrowdPropensities <- array(dim=c(num.children, length(z1overcrowdmodel), 1))
    
    #calculate propensities/probabilities from the propensity models
    #then fill the above arrays with the propensities
    SESPropensitiesMatrix <- predictOrdinal(SESBTHmodels, num.children, envir=simframe.master, stochastic=stochastic)
    SESBTHPropensities[,,1] <- SESPropensitiesMatrix[,-ncol(SESPropensitiesMatrix)]
    pregalcPropensitiesMatrix <- predictOrdinal(pregalcmodels, num.children, envir=simframe.master, stochastic=stochastic)
    
    pregalcPropensities[,,1] <- pregalcPropensitiesMatrix[,-ncol(pregalcPropensitiesMatrix)]
    NPRESCHPropensitiesMatrix <- predictOrdinal(NPRESCHmodels, num.children, envir=simframe.master, stochastic=stochastic)
    
    NPRESCHPropensities[,,1] <- NPRESCHPropensitiesMatrix[,-ncol(NPRESCHPropensitiesMatrix)]
    fagePropensitiesMatrix <- predictOrdinal(fagemodels, num.children, envir=simframe.master, stochastic=stochastic)
    
    fagePropensities[,,1] <- fagePropensitiesMatrix[,-ncol(fagePropensitiesMatrix)]
    r1stchildethnPropensitiesMatrix <- predictOrdinal(r1stchildethnmodels, num.children, envir=simframe.master, stochastic=stochastic)
    r1stchildethnPropensities[,,1] <- r1stchildethnPropensitiesMatrix[,-ncol(r1stchildethnPropensitiesMatrix)]
    r1stmeducPropensitiesMatrix <- predictOrdinal(r1stmeducmodels, num.children, envir=simframe.master, stochastic=stochastic)
    r1stmeducPropensities[,,1] <- r1stmeducPropensitiesMatrix[,-ncol(r1stmeducPropensitiesMatrix)]
    r1stfeducPropensitiesMatrix <- predictOrdinal(r1stfeducmodels, num.children, envir=simframe.master, stochastic=stochastic)
    r1stfeducPropensities[,,1] <- r1stfeducPropensitiesMatrix[,-ncol(r1stfeducPropensitiesMatrix)]
    bwkgPropensitiesMatrix <- predictOrdinal(bwkgmodels, num.children, envir=simframe.master, stochastic=stochastic)
    bwkgPropensities[,,1] <- bwkgPropensitiesMatrix[,-ncol(bwkgPropensitiesMatrix)]
    z1single0PropensitiesMatrix <- predictOrdinal(z1single0model, num.children, envir=simframe.master, stochastic=stochastic)
    z1single0Propensities[,,1] <- z1single0PropensitiesMatrix[,-ncol(z1single0PropensitiesMatrix)]
    BREASTPropensitiesMatrix <- predictOrdinal(BREASTmodels, num.children, envir=simframe.master, stochastic=stochastic)
    BREASTPropensities[,,1] <- BREASTPropensitiesMatrix[,-ncol(BREASTPropensitiesMatrix)]
    INTERACTPropensitiesMatrix <- predictOrdinal(INTERACTmodels, num.children, envir=simframe.master, stochastic=stochastic)
    INTERACTPropensities[,,1] <- INTERACTPropensitiesMatrix[,-ncol(INTERACTPropensitiesMatrix)]
    PUNISHPropensitiesMatrix <- predictOrdinal(PUNISHmodels, num.children, envir=simframe.master, stochastic=stochastic)
    PUNISHPropensities[,,1] <- PUNISHPropensitiesMatrix[,-ncol(PUNISHPropensitiesMatrix)]
    MAGEPropensitiesMatrix <- predictOrdinal(MAGEmodels, num.children, envir=simframe.master, stochastic=stochastic)
    MAGEPropensities[,,1] <- MAGEPropensitiesMatrix[,-ncol(MAGEPropensitiesMatrix)]
    pregsmkPropensitiesMatrix <- predictOrdinal(pregsmkmodels, num.children, envir=simframe.master, stochastic=stochastic)
    pregsmkPropensities[,,1] <- pregsmkPropensitiesMatrix[,-ncol(pregsmkPropensitiesMatrix)]
    
    z1accomPropensitiesMatrix <- predictOrdinal(z1accommodel, num.children, envir=simframe.master, stochastic=stochastic)
    z1accomPropensities[,,1] <- z1accomPropensitiesMatrix[,-ncol(z1accomPropensitiesMatrix)]
    msmokePropensitiesMatrix <- predictOrdinal(msmokemodels, num.children, envir=simframe.master, stochastic=stochastic)
    msmokePropensities[,,1] <- msmokePropensitiesMatrix[,-ncol(msmokePropensitiesMatrix)]
    fsmokePropensitiesMatrix <- predictOrdinal(fsmokemodels, num.children, envir=simframe.master, stochastic=stochastic)
    fsmokePropensities[,,1] <- fsmokePropensitiesMatrix[,-ncol(fsmokePropensitiesMatrix)]
    kidsPropensitiesMatrix <- predictOrdinal(kidsmodels, num.children, envir=simframe.master, stochastic=stochastic)
    kidsPropensities[,,1] <- kidsPropensitiesMatrix[,-ncol(kidsPropensitiesMatrix)]
    z1singlePropensitiesMatrix <- predictOrdinal(z1singlemodel, num.children, envir=simframe.master, stochastic=stochastic)
    z1singlePropensities[,,1] <- z1singlePropensitiesMatrix[,-ncol(z1singlePropensitiesMatrix)]
    z1chparPropensitiesMatrix <- predictOrdinal(z1chparmodel, num.children, envir=simframe.master, stochastic=stochastic)
    z1chparPropensities[,,1] <- z1chparPropensitiesMatrix[,-ncol(z1chparPropensitiesMatrix)]
    chresPropensitiesMatrix <- predictOrdinal(chresmodels, num.children, envir=simframe.master, stochastic=stochastic)
    chresPropensities[,,1] <- chresPropensitiesMatrix[,-ncol(chresPropensitiesMatrix)]
    welfarePropensitiesMatrix <- predictOrdinal(welfaremodel, num.children, envir=simframe.master, stochastic=stochastic)
    welfarePropensities[,,1] <- welfarePropensitiesMatrix[,-ncol(welfarePropensitiesMatrix)]
    mhrswrkPropensitiesMatrix <- predictOrdinal(mhrswrkmodels, num.children, envir=simframe.master, stochastic=stochastic)
    mhrswrkPropensities[,,1] <- mhrswrkPropensitiesMatrix[,-ncol(mhrswrkPropensitiesMatrix)]
    fhrswrkPropensitiesMatrix <- predictOrdinal(fhrswrkmodels, num.children, envir=simframe.master, stochastic=stochastic)
    fhrswrkPropensities[,,1] <- fhrswrkPropensitiesMatrix[,-ncol(fhrswrkPropensitiesMatrix)]
    z1homeownPropensitiesMatrix <- predictOrdinal(z1homeownmodel, num.children, envir=simframe.master, stochastic=stochastic)
    z1homeownPropensities[,,1] <- z1homeownPropensitiesMatrix[,-ncol(z1homeownPropensitiesMatrix)]
    z1overcrowdPropensitiesMatrix <- predictOrdinal(z1overcrowdmodel, num.children, envir=simframe.master, stochastic=stochastic)
    z1overcrowdPropensities[,,1] <- z1overcrowdPropensitiesMatrix[,-ncol(z1overcrowdPropensitiesMatrix)]
    
    #combine all the individual arrays into a list
    propens.list <- list()
    
    propens.list$SESBTH <- SESBTHPropensities
    propens.list$pregalc <- pregalcPropensities
    propens.list$NPRESCH <- NPRESCHPropensities
    propens.list$fage <- fagePropensities
    propens.list$r1stchildethn <- r1stchildethnPropensities
    propens.list$r1stmeduc <- r1stmeducPropensities
    propens.list$r1stfeduc <- r1stfeducPropensities
    propens.list$bwkg <- bwkgPropensities
    propens.list$z1single0 <- z1single0Propensities
    propens.list$BREAST <- BREASTPropensities
    propens.list$INTERACT <- INTERACTPropensities
    propens.list$PUNISH <- PUNISHPropensities
    propens.list$MAGE <- MAGEPropensities
    propens.list$pregsmk <- pregsmkPropensities
    
    propens.list$z1accom <- z1accomPropensities
    propens.list$msmoke <- msmokePropensities
    propens.list$fsmoke <- fsmokePropensities
    propens.list$kids <- kidsPropensities
    propens.list$z1single <- z1singlePropensities
    propens.list$z1chpar <- z1chparPropensities
    propens.list$chres <- chresPropensities
    propens.list$welfare <- welfarePropensities
    propens.list$mhrswrk <- mhrswrkPropensities
    propens.list$fhrswrk <- fhrswrkPropensities
    propens.list$z1homeown <- z1homeownPropensities
    propens.list$z1overcrowd <- z1overcrowdPropensities
    
    propens.list
  }
  
  
  #' Load each model from an xls file, and construct a GLM object from it
  #' @examples
  #' modelfiledir <- "D:/workspace.sim/MELC/CHDS/models/"
  #' models <- loadMELCModels(modelfiledir)
  loadMELCModels <- function(modelfiledir) {
    models <- list()
    
    models$z1singlePrev0A2_5 <- loadGLMCSV(modelfiledir, "z1singlePrev0A2_5.csv")
    models$z1singlePrev1A2_5 <- loadGLMCSV(modelfiledir, "z1singlePrev1A2_5.csv")
    models$z1singlePrev0A6_13 <- loadGLMCSV(modelfiledir, "z1singlePrev0A6_13.csv")
    models$z1singlePrev1A6_13 <- loadGLMCSV(modelfiledir, "z1singlePrev1A6_13.csv")
    
    models$fage_years <- loadGLMCSV(modelfiledir, "fage_years.csv")
    models$chkids2_5 <- loadGLMCSV(modelfiledir, "increaseChild2_5.csv")
    models$chkids6_13 <- loadGLMCSV(modelfiledir, "increaseChild6_13.csv")
    #models$propAddHS <- loadGLMCSV(modelfiledir, "paddhsbetas.csv")
    
    models$z1chparPrev0A2_5 <- loadGLMCSV(modelfiledir, "z1chparPrev0A2_5.csv")
    models$z1chparPrev1A2_5 <- loadGLMCSV(modelfiledir, "z1chparPrev1A2_5.csv")
    models$z1chparPrev0A6_13 <- loadGLMCSV(modelfiledir, "z1chparPrev0A6_13.csv")
    models$z1chparPrev1A6_13 <- loadGLMCSV(modelfiledir, "z1chparPrev1A6_13.csv")
    
    models$chres.a2_5 <- loadGLMCSV(modelfiledir, "ChresA2_5.csv")
    models$chres.a6_13 <- loadGLMCSV(modelfiledir, "ChresA6_13.csv")
    models$z1chresPrev0.a2_5 <- loadGLMCSV(modelfiledir, "z1chresPrev0A2_5.csv")
    models$z1chresPrev0.a6_13 <- loadGLMCSV(modelfiledir, "z1chresPrev0A6_13.csv")
    models$z1chresPrev1.a2_5 <- loadGLMCSV(modelfiledir, "z1chresPrev1A2_5.csv")
    models$z1chresPrev1.a6_13 <- loadGLMCSV(modelfiledir, "z1chresPrev1A6_13.csv")
    
    models$welfarePrev0.a2_5 <- loadGLMCSV(modelfiledir, "WelfarePrev0A2_5.csv")
    models$welfarePrev0.a6_13 <- loadGLMCSV(modelfiledir, "WelfarePrev0A6_13.csv")
    models$welfarePrev1.a2_5 <- loadGLMCSV(modelfiledir, "WelfarePrev1A2_5.csv")
    models$welfarePrev1.a6_13 <- loadGLMCSV(modelfiledir, "WelfarePrev1A6_13.csv")
    
    models$mhrswrk.a2_5.mg1 <- loadGLMCSV(modelfiledir, "MhrswrkA2_5Mg1.csv")
    models$mhrswrk.a2_5.mg2 <- loadGLMCSV(modelfiledir, "MhrswrkA2_5Mg2.csv")
    models$mhrswrk.a2_5.mg3 <- loadGLMCSV(modelfiledir, "MhrswrkA2_5Mg3.csv")
    models$mhrswrk.a6_13.mg1 <- loadGLMCSV(modelfiledir, "MhrswrkA6_13Mg1.csv")
    models$mhrswrk.a6_13.mg2 <- loadGLMCSV(modelfiledir, "MhrswrkA6_13Mg2.csv")
    models$mhrswrk.a6_13.mg3 <- loadGLMCSV(modelfiledir, "MhrswrkA6_13Mg3.csv")
    models$z1mhrswrk.prev0.a2_5.mg1 <- loadGLMCSV(modelfiledir, "z1mhrswrkPrev0A2_5Mg1.csv")
    models$z1mhrswrk.prev0.a2_5.mg2 <- loadGLMCSV(modelfiledir, "z1mhrswrkPrev0A2_5Mg2.csv")
    models$z1mhrswrk.prev0.a2_5.mg3 <- loadGLMCSV(modelfiledir, "z1mhrswrkPrev0A2_5Mg3.csv")
    models$z1mhrswrk.prev0.a6_13.mg1 <- loadGLMCSV(modelfiledir, "z1mhrswrkPrev0A6_13Mg1.csv")
    models$z1mhrswrk.prev0.a6_13.mg2 <- loadGLMCSV(modelfiledir, "z1mhrswrkPrev0A6_13Mg2.csv")
    models$z1mhrswrk.prev0.a6_13.mg3 <- loadGLMCSV(modelfiledir, "z1mhrswrkPrev0A6_13Mg3.csv")
    models$z1mhrswrk.prev1.a2_5 <- loadGLMCSV(modelfiledir, "z1mhrswrkPrev1A2_5.csv")
    models$z1mhrswrk.prev1.a6_13 <- loadGLMCSV(modelfiledir, "z1mhrswrkPrev1A6_13.csv")
    
    
    models$z1fhrswrk.prev0.a2_5.dg1 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev0A2_5Dg1.csv")
    models$z1fhrswrk.prev0.a2_5.dg2 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev0A2_5Dg2.csv")
    models$z1fhrswrk.prev0.a2_5.dg3 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev0A2_5Dg3.csv")
    models$z1fhrswrk.prev0.a6_13.dg1 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev0A6_13Dg1.csv")
    models$z1fhrswrk.prev0.a6_13.dg2 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev0A6_13Dg2.csv")
    models$z1fhrswrk.prev0.a6_13.dg3 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev0A6_13Dg3.csv")
    models$z1fhrswrk.prev1.a2_5.dg1 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev1A2_5Dg1.csv")
    models$z1fhrswrk.prev1.a2_5.dg2 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev1A2_5Dg2.csv")
    models$z1fhrswrk.prev1.a2_5.dg3 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev1A2_5Dg3.csv")
    models$z1fhrswrk.prev1.a6_13.dg1 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev1A6_13Dg1.csv")
    models$z1fhrswrk.prev1.a6_13.dg2 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev1A6_13Dg2.csv")
    models$z1fhrswrk.prev1.a6_13.dg3 <- loadGLMCSV(modelfiledir, "z1fhrswrkPrev1A6_13Dg3.csv")
    models$fhrswrk.a2_5 <- loadGLMCSV(modelfiledir, "FhrswrkA2_5.csv")
    models$fhrswrk.a6_13.dg1 <- loadGLMCSV(modelfiledir, "FhrswrkA6_13Dg1.csv")
    models$fhrswrk.a6_13.dg2 <- loadGLMCSV(modelfiledir, "FhrswrkA6_13Dg2.csv")
    models$fhrswrk.a6_13.dg3 <- loadGLMCSV(modelfiledir, "FhrswrkA6_13Dg3.csv")
    
    
    models$z1accomPrev0.a2_5 <- loadGLMCSV(modelfiledir, "z1accomPrev0A2_5.csv")
    models$z1accomPrev0.a6_13 <- loadGLMCSV(modelfiledir, "z1accomPrev0A6_13.csv")
    models$z1accomPrev1.a2_5 <- loadGLMCSV(modelfiledir, "z1accomPrev1A2_5.csv")
    models$z1accomPrev1.a6_13 <- loadGLMCSV(modelfiledir, "z1accomPrev1A6_13.csv")
    
    models$z1homeownPrev0.a2_5 <- loadGLMCSV(modelfiledir, "z1homeownPrev0A2_5.csv")
    models$z1homeownPrev0.a6_13 <- loadGLMCSV(modelfiledir, "z1homeownPrev0A6_13.csv")
    models$z1homeownPrev1.a2_5 <- loadGLMCSV(modelfiledir, "z1homeownPrev1A2_5.csv")
    models$z1homeownPrev1.a6_13 <- loadGLMCSV(modelfiledir, "z1homeownPrev1A6_13.csv")
    
    models$z1overcrowdPrev0.a2_5 <- loadGLMCSV(modelfiledir, "z1overcrowdPrev0A2_5.csv")
    models$z1overcrowdPrev0.a6_13 <- loadGLMCSV(modelfiledir, "z1overcrowdPrev0A6_13.csv")
    models$z1overcrowdPrev1.a2_5 <- loadGLMCSV(modelfiledir, "z1overcrowdPrev1A2_5.csv")
    models$z1overcrowdPrev1.a6_13 <- loadGLMCSV(modelfiledir, "z1overcrowdPrev1A6_13.csv")
    
    
    models$msmoke.a2_5 <- loadGLMCSV(modelfiledir, "MsmokeA2_5.csv")
    models$msmoke.a6_13.mg1 <- loadGLMCSV(modelfiledir, "MsmokeA6_13Mg1.csv")
    models$msmoke.a6_13.mg2 <- loadGLMCSV(modelfiledir, "MsmokeA6_13Mg2.csv")
    models$msmoke.a6_13.mg3 <- loadGLMCSV(modelfiledir, "MsmokeA6_13Mg3.csv")
    models$z1msmoke.prev0.a2_5.mg1 <- loadGLMCSV(modelfiledir, "z1msmokePrev0A2_5Mg1.csv")
    models$z1msmoke.prev0.a2_5.mg2 <- loadGLMCSV(modelfiledir, "z1msmokePrev0A2_5Mg2.csv")
    models$z1msmoke.prev0.a2_5.mg3 <- loadGLMCSV(modelfiledir, "z1msmokePrev0A2_5Mg3.csv")
    models$z1msmoke.prev0.a6_13 <- loadGLMCSV(modelfiledir, "z1msmokePrev0A6_13.csv")
    models$z1msmoke.prev1.a2_5 <- loadGLMCSV(modelfiledir, "z1msmokePrev1A2_5.csv")
    models$z1msmoke.prev1.a6_13.mg1 <- loadGLMCSV(modelfiledir, "z1msmokePrev1A6_13Mg1.csv")
    models$z1msmoke.prev1.a6_13.mg2 <- loadGLMCSV(modelfiledir, "z1msmokePrev1A6_13Mg2.csv")
    models$z1msmoke.prev1.a6_13.mg3 <- loadGLMCSV(modelfiledir, "z1msmokePrev1A6_13Mg3.csv")
    
    models$fsmoke.a2_5.dg1 <- loadGLMCSV(modelfiledir, "FsmokeA2_5Dg1.csv")
    models$fsmoke.a2_5.dg2 <- loadGLMCSV(modelfiledir, "fsmokeA2_5Dg2.csv")
    models$fsmoke.a2_5.dg3 <- loadGLMCSV(modelfiledir, "FsmokeA2_5Dg3.csv")
    models$fsmoke.a6_13.dg1 <- loadGLMCSV(modelfiledir, "FsmokeA6_13Dg1.csv")
    models$fsmoke.a6_13.dg2 <- loadGLMCSV(modelfiledir, "fsmokeA6_13Dg2.csv")
    models$fsmoke.a6_13.dg3 <- loadGLMCSV(modelfiledir, "FsmokeA6_13Dg3.csv")
    models$z1fsmoke.prev0.a2_5.dg1 <- loadGLMCSV(modelfiledir, "z1fsmokePrev0A2_5Dg1.csv")
    models$z1fsmoke.prev0.a2_5.dg2 <- loadGLMCSV(modelfiledir, "z1fsmokePrev0A2_5Dg2.csv")
    models$z1fsmoke.prev0.a2_5.dg3 <- loadGLMCSV(modelfiledir, "z1fsmokePrev0A2_5Dg3.csv")
    models$z1fsmoke.prev0.a6_13.dg1 <- loadGLMCSV(modelfiledir, "z1fsmokePrev0A6_13Dg1.csv")
    models$z1fsmoke.prev0.a6_13.dg2 <- loadGLMCSV(modelfiledir, "z1fsmokePrev0A6_13Dg2.csv")
    models$z1fsmoke.prev0.a6_13.dg3 <- loadGLMCSV(modelfiledir, "z1fsmokePrev0A6_13Dg3.csv")
    models$z1fsmoke.prev1.a2_5.dg1 <- loadGLMCSV(modelfiledir, "z1fsmokePrev1A2_5Dg1.csv")
    models$z1fsmoke.prev1.a2_5.dg2 <- loadGLMCSV(modelfiledir, "z1fsmokePrev1A2_5Dg2.csv")
    models$z1fsmoke.prev1.a2_5.dg3 <- loadGLMCSV(modelfiledir, "z1fsmokePrev1A2_5Dg3.csv")
    models$z1fsmoke.prev1.a6_13 <- loadGLMCSV(modelfiledir, "z1fsmokePrev1A6_13.csv")
    
    models$hadmtot2_5 <- loadGLMCSV(modelfiledir, "HadmtotA2_5.csv")
    models$hadmtot6_10 <- loadGLMCSV(modelfiledir, "HadmtotA6_10.csv")
    models$houtptot2_5 <- loadGLMCSV(modelfiledir, "HoutptotA2_5.csv")
    models$houtptot6_10 <- loadGLMCSV(modelfiledir, "HoutptotA6_10.csv")
    models$gptotvis2 <- loadGLMCSV(modelfiledir, "GptotvisA2.csv")
    models$gptotvis3_5 <- loadGLMCSV(modelfiledir, "GptotvisA3_5.csv")
    models$gptotvis6 <- loadGLMCSV(modelfiledir, "GptotvisA6.csv")
    models$gptotvis7_10 <- loadGLMCSV(modelfiledir, "GptotvisA7_10.csv")
    
    models$gpresp2_5 <- loadGLMCSV(modelfiledir, "GprespA2_5.csv")
    models$gpresp6_10 <- loadGLMCSV(modelfiledir, "GprespA6_10.csv")
    models$gpmorb2 <- loadGLMCSV(modelfiledir, "GpmorbA2.csv")
    models$gpmorb3_4 <- loadGLMCSV(modelfiledir, "GpmorbA3_4.csv")
    models$gpmorb5 <- loadGLMCSV(modelfiledir, "GpmorbA5.csv")
    models$gpmorb6_7 <- loadGLMCSV(modelfiledir, "gpmorbA6_7.csv")
    models$gpprev2 <- loadGLMCSV(modelfiledir, "GpprevA2.csv")
    models$gpprev3_4 <- loadGLMCSV(modelfiledir, "GpprevA3_4.csv")
    models$gpprev5 <- loadGLMCSV(modelfiledir, "GpprevA5.csv")
    models$gpprev6_7 <- loadGLMCSV(modelfiledir, "GpprevA6_7.csv")
    
    models$cond4 <- loadGLMCSV(modelfiledir, "Z1condA4.csv")
    models$cond.prev0.5 <- loadGLMCSV(modelfiledir, "Z1condA5Prev0.csv")
    models$cond.prev1.5 <- loadGLMCSV(modelfiledir, "Z1condA5Prev1.csv")
    models$cond.prev0.6_13 <- loadGLMCSV(modelfiledir, "Z1condPrev0A6_13.csv")
    models$cond.prev1.6_13 <- loadGLMCSV(modelfiledir, "Z1condPrev1A6_13.csv")
    models$burt8 <- loadGLMCSV(modelfiledir, "BurtA8.csv")
    models$burt9_13 <- loadGLMCSV(modelfiledir, "BurtA9_13.csv")
    
    models$NPRESCH <- loadGLMCSV(modelfiledir, "Npresch.csv")
    models$INTERACT <- loadGLMCSV(modelfiledir, "Interact.csv")
    models$PUNISH <- loadGLMCSV(modelfiledir, "Punish.csv")	
    models$depression <- loadGLMCSV(modelfiledir, "depression.csv")
    
    models$z1OverweightA2 <- loadGLMCSV(modelfiledir, "z1OverweightA2.csv")
    models$z1OverweightA3 <- loadGLMCSV(modelfiledir, "z1OverweightA3.csv")
    models$z1OverweightA4 <- loadGLMCSV(modelfiledir, "z1OverweightA4.csv")
    models$z1OverweightA5 <- loadGLMCSV(modelfiledir, "z1OverweightA5.csv")
    models$z1OverweightA6 <- loadGLMCSV(modelfiledir, "z1OverweightA6.csv")
    models$z1OverweightA7 <- loadGLMCSV(modelfiledir, "z1OverweightA7.csv")
    models$z1OverweightA8 <- loadGLMCSV(modelfiledir, "z1OverweightA8.csv")
    models$z1OverweightA9 <- loadGLMCSV(modelfiledir, "z1OverweightA9.csv")
    models$z1OverweightA10 <- loadGLMCSV(modelfiledir, "z1OverweightA10.csv")	
    models$z1OverweightA11 <- loadGLMCSV(modelfiledir, "z1OverweightA11.csv")
    models$z1OverweightA12 <- loadGLMCSV(modelfiledir, "z1OverweightA12.csv")
    models$z1OverweightA13 <- loadGLMCSV(modelfiledir, "z1OverweightA13.csv")
    models$z1OverweightA14 <- loadGLMCSV(modelfiledir, "z1OverweightA14.csv")
    models$z1OverweightA15 <- loadGLMCSV(modelfiledir, "z1OverweightA15.csv")
    models$z1OverweightA16 <- loadGLMCSV(modelfiledir, "z1OverweightA16.csv")
    models$z1OverweightA17 <- loadGLMCSV(modelfiledir, "z1OverweightA17.csv")
    models$z1OverweightA18 <- loadGLMCSV(modelfiledir, "z1OverweightA18.csv")
    models$z1OverweightA19 <- loadGLMCSV(modelfiledir, "z1OverweightA19.csv")
    models$z1OverweightA20 <- loadGLMCSV(modelfiledir, "z1OverweightA20.csv")	
    models$z1OverweightA21 <- loadGLMCSV(modelfiledir, "z1OverweightA21.csv")
    
    models$IQA2 <- loadGLMCSV(modelfiledir, "IQA2.csv")
    #models$IQA3_16 <- loadGLMCSV(modelfiledir, "IQA3_16.csv")
    
    models$IQA3 <- loadGLMCSV(modelfiledir, "IQA3.csv")
    models$IQA4 <- loadGLMCSV(modelfiledir, "IQA4.csv")
    models$IQA5 <- loadGLMCSV(modelfiledir, "IQA5.csv")
    models$IQA6_16 <- loadGLMCSV(modelfiledir, "IQA6.csv")
    models$IQA7 <- loadGLMCSV(modelfiledir, "IQA7.csv")
    models$IQA8 <- loadGLMCSV(modelfiledir, "IQA8.csv")
    models$IQA9 <- loadGLMCSV(modelfiledir, "IQA9.csv")
    models$IQA10 <- loadGLMCSV(modelfiledir, "IQA10.csv")
    models$IQA11 <- loadGLMCSV(modelfiledir, "IQA11_16.csv")
    # models$IQA12 <- loadGLMCSV(modelfiledir, "IQA12.csv")
    # models$IQA13 <- loadGLMCSV(modelfiledir, "IQA13.csv")
    # models$IQA14 <- loadGLMCSV(modelfiledir, "IQA14.csv")
    # models$IQA15 <- loadGLMCSV(modelfiledir, "IQA15.csv")
    # models$IQA16 <- loadGLMCSV(modelfiledir, "IQA16.csv")
    
    models$ScoreA17 <- loadGLMCSV(modelfiledir, "ScoreA17.csv")
    models$ScoreA17Ethn2 <- loadGLMCSV(modelfiledir, "ScoreA17Ethn2.csv")
    
    models$z1NEETGender0A16 <- loadGLMCSV(modelfiledir, "z1NEETGender0A16.csv")
    models$z1NEETGender0A18 <- loadGLMCSV(modelfiledir, "z1NEETGender0A18.csv")
    models$z1NEETGender0A20 <- loadGLMCSV(modelfiledir, "z1NEETGender0A20.csv")
    models$z1NEETGender0A17 <- loadGLMCSV(modelfiledir, "z1NEETGender0A17.csv")
    models$z1NEETGender0A19 <- loadGLMCSV(modelfiledir, "z1NEETGender0A19.csv")
    models$z1NEETGender0A21 <- loadGLMCSV(modelfiledir, "z1NEETGender0A21.csv")
    
    models$z1NEETGender1A16 <- loadGLMCSV(modelfiledir, "z1NEETGender1A16.csv")
    models$z1NEETGender1A18 <- loadGLMCSV(modelfiledir, "z1NEETGender1A18.csv")
    models$z1NEETGender1A20 <- loadGLMCSV(modelfiledir, "z1NEETGender1A20.csv")
    models$z1NEETGender1A17 <- loadGLMCSV(modelfiledir, "z1NEETGender1A17.csv")
    models$z1NEETGender1A19 <- loadGLMCSV(modelfiledir, "z1NEETGender1A19.csv")
    models$z1NEETGender1A21 <- loadGLMCSV(modelfiledir, "z1NEETGender1A21.csv")
    
    models$z1BullyA15 <- loadGLMCSV(modelfiledir, "z1BullyA15.csv")
    models$z1BullyA16 <- loadGLMCSV(modelfiledir, "z1BullyA16.csv")
    models$z1BullyA17_21 <- loadGLMCSV(modelfiledir, "z1BullyA17_21.csv")
    
    
    cat("Loaded models\n")
    models
  }
  
  #' Load models for mapping from categorical to continuous (in sceanrio testing of continuous 
  #' variables) 
  #' Load each model from an xls file, and construct a GLM object from it
  #' @examples
  #' modelfiledir <- "D:/workspace.sim/MELC/CHDS/models_CatToCont/"
  #' models <- loadMELCModels(modelfiledir)
  loadCatToContModels <- function(modelfiledir) {
    catToContModels <- list()
    
    catToContModels$MAGE <- list()
    catToContModels$fhrswrk <- list()
    catToContModels$mhrswrk <- list()
    catToContModels$chres <- list()
    catToContModels$kids <- list()
    catToContModels$msmoke <- list()
    catToContModels$fsmoke <- list()
    catToContModels$pregsmk <- list()
    catToContModels$INTERACT <- list()
    catToContModels$NPRESCH <- list()
    
    catToContModels$MAGE <- list(loadGLMCSV(modelfiledir, "mage1_20.csv"),
                                 loadGLMCSV(modelfiledir, "mage20_24.csv"), loadGLMCSV(modelfiledir, "mage25_29.csv"),
                                 loadGLMCSV(modelfiledir, "mage30_34.csv"), loadGLMCSV(modelfiledir, "mage35_39.csv"),
                                 loadGLMCSV(modelfiledir, "mage40Plus.csv"))
    catToContModels$fhrswrk <- list(loadGLMCSV(modelfiledir, "fhrs0.csv"), 
                                    loadGLMCSV(modelfiledir, "fhrs1_20.csv"), loadGLMCSV(modelfiledir, "fhrs21_35.csv"),
                                    loadGLMCSV(modelfiledir, "fhrs36_40.csv"), loadGLMCSV(modelfiledir, "fhrs41_45.csv"),
                                    loadGLMCSV(modelfiledir, "fhrs46_50.csv"), loadGLMCSV(modelfiledir, "fhrsGT50.csv"))
    catToContModels$mhrswrk <- list(loadGLMCSV(modelfiledir, "mhrs0.csv"), 
                                    loadGLMCSV(modelfiledir, "mhrs1_10.csv"), loadGLMCSV(modelfiledir, "mhrs11_20.csv"),
                                    loadGLMCSV(modelfiledir, "mhrs21_30.csv"), loadGLMCSV(modelfiledir, "mhrs31_40.csv"),
                                    loadGLMCSV(modelfiledir, "mhrsGT40.csv"))
    catToContModels$chres <- list(loadGLMCSV(modelfiledir, "chres0.csv"), 
                                  loadGLMCSV(modelfiledir, "chres1.csv"), loadGLMCSV(modelfiledir, "chres2plus.csv"))
    catToContModels$kids <- list(loadGLMCSV(modelfiledir, "kids1.csv"), 
                                 loadGLMCSV(modelfiledir, "kids2.csv"), loadGLMCSV(modelfiledir, "kids3.csv"),
                                 loadGLMCSV(modelfiledir, "kids4.csv"), loadGLMCSV(modelfiledir, "kids5plus.csv"))
    catToContModels$householdsize <- list(loadGLMCSV(modelfiledir, "householdsize2.csv"), 
                                          loadGLMCSV(modelfiledir, "householdsize3.csv"), loadGLMCSV(modelfiledir, "householdsize4.csv"),
                                          loadGLMCSV(modelfiledir, "householdsize5.csv"), loadGLMCSV(modelfiledir, "householdsize6plus.csv"))
    catToContModels$msmoke <- list(loadGLMCSV(modelfiledir, "msmoke0.csv"), 
                                   loadGLMCSV(modelfiledir, "msmoke1_10.csv"), loadGLMCSV(modelfiledir, "msmoke11_20.csv"),
                                   loadGLMCSV(modelfiledir, "msmokeGT20.csv"))
    catToContModels$fsmoke <- list(loadGLMCSV(modelfiledir, "fsmoke0.csv"), 
                                   loadGLMCSV(modelfiledir, "fsmoke1_10.csv"), loadGLMCSV(modelfiledir, "fsmoke11_20.csv"),
                                   loadGLMCSV(modelfiledir, "fsmokeGT20.csv"))
    catToContModels$pregsmk <- list(loadGLMCSV(modelfiledir, "pregsmk0.csv"), 
                                    loadGLMCSV(modelfiledir, "pregsmk1_5.csv"), loadGLMCSV(modelfiledir, "pregsmk6_10.csv"),
                                    loadGLMCSV(modelfiledir, "pregsmk11_15.csv"), loadGLMCSV(modelfiledir, "pregsmk16_20.csv"),
                                    loadGLMCSV(modelfiledir, "pregsmk21Plus.csv"))
    catToContModels$pregalc <- list(loadGLMCSV(modelfiledir, "pregalc0.csv"), 
                                    loadGLMCSV(modelfiledir, "pregalc0.csv"), loadGLMCSV(modelfiledir, "pregalc1.csv"),
                                    loadGLMCSV(modelfiledir, "pregalc2.csv"), loadGLMCSV(modelfiledir, "pregalc3.csv"),
                                    loadGLMCSV(modelfiledir, "pregalc4.csv"), loadGLMCSV(modelfiledir, "pregalc5.csv"),
                                    loadGLMCSV(modelfiledir, "pregalc6.csv"), loadGLMCSV(modelfiledir, "pregalc7.csv"),
                                    loadGLMCSV(modelfiledir, "pregalc8Plus.csv"))
    catToContModels$bwkg <- list(loadGLMCSV(modelfiledir, "bwkgLT25.csv"), 
                                 loadGLMCSV(modelfiledir, "bwkg25_29.csv"), loadGLMCSV(modelfiledir, "bwkg30_34.csv"),
                                 loadGLMCSV(modelfiledir, "bwkg35_39.csv"), loadGLMCSV(modelfiledir, "bwkg4Plus.csv"))
    catToContModels$ga <- list(loadGLMCSV(modelfiledir, "gaLT35.csv"), 
                               loadGLMCSV(modelfiledir, "ga35.csv"), loadGLMCSV(modelfiledir, "ga36.csv"),
                               loadGLMCSV(modelfiledir, "ga37_42.csv"), loadGLMCSV(modelfiledir, "gaGT42.csv"))
    catToContModels$BREAST <- list(loadGLMCSV(modelfiledir, "breast0.csv"), 
                                   loadGLMCSV(modelfiledir, "breast1.csv"), loadGLMCSV(modelfiledir, "breast2.csv"),
                                   loadGLMCSV(modelfiledir, "breast3.csv"), loadGLMCSV(modelfiledir, "breast4.csv"),
                                   loadGLMCSV(modelfiledir, "breast5.csv"), loadGLMCSV(modelfiledir, "breast6.csv"),
                                   loadGLMCSV(modelfiledir, "breast7.csv"), loadGLMCSV(modelfiledir, "breast8.csv"),
                                   loadGLMCSV(modelfiledir, "breast9.csv"), loadGLMCSV(modelfiledir, "breast10.csv"),
                                   loadGLMCSV(modelfiledir, "breast11.csv"), loadGLMCSV(modelfiledir, "breast12.csv"))
    catToContModels$INTERACT <- list(loadGLMCSV(modelfiledir, "interact0_2.csv"), 
                                     loadGLMCSV(modelfiledir, "interact3.csv"), loadGLMCSV(modelfiledir, "interact4.csv"),
                                     loadGLMCSV(modelfiledir, "interact5.csv"), loadGLMCSV(modelfiledir, "interact6.csv"),
                                     loadGLMCSV(modelfiledir, "interact7.csv"), loadGLMCSV(modelfiledir, "interact8.csv"),
                                     loadGLMCSV(modelfiledir, "interact9.csv"), loadGLMCSV(modelfiledir, "interact10.csv"))
    catToContModels$NPRESCH <- list(loadGLMCSV(modelfiledir, "NPRESCH0.csv"), 
                                    loadGLMCSV(modelfiledir, "NPRESCH1.csv"), loadGLMCSV(modelfiledir, "NPRESCH2.csv"),
                                    loadGLMCSV(modelfiledir, "NPRESCH3.csv"))
    
    cat("Loaded CatToCont models\n")
    catToContModels
  }
  
  #' Loads the propensity models (models used for deciding who to change in a scenario).
  #' The function uses loadGLMCSV to load excel files and returns the object PropensityModels which
  #' is a list with each element being a list of propensity models (glm objects) for each variable. 
  loadPropensityModels <- function(modelfiledir) {
    PropensityModels <- list()
    
    PropensityModels$msmoke <- list()
    PropensityModels$fsmoke <- list()
    PropensityModels$kids <- list()
    PropensityModels$z1single <- list()
    PropensityModels$z1chpar <- list()
    PropensityModels$chres <- list()
    PropensityModels$welfare <- list()
    PropensityModels$mhrswrk <- list()
    PropensityModels$fhrswrk <- list()
    PropensityModels$z1accom <- list()
    PropensityModels$z1homeown <- list()
    PropensityModels$z1overcrowd <- list()
    
    PropensityModels$z1single0 <- list()
    PropensityModels$BREAST <- list()
    PropensityModels$SESBTH <- list()
    PropensityModels$r1stchildethn <- list()
    PropensityModels$bwkg <- list()
    PropensityModels$r1stmeduc <- list()
    PropensityModels$r1stfeduc <- list()
    PropensityModels$fage <- list()
    PropensityModels$NPRESCH <- list()
    PropensityModels$pregalc <- list()
    PropensityModels$INTERACT <- list()
    PropensityModels$PUNISH <- list()
    PropensityModels$MAGE <- list()
    PropensityModels$pregsmk <- list()
    
    PropensityModels$msmoke <- list(loadGLMCSV(modelfiledir, "msmokeProb1.csv"),
                                    loadGLMCSV(modelfiledir, "msmokeProb2.csv"), loadGLMCSV(modelfiledir, "msmokeProb3.csv"))
    PropensityModels$fsmoke <- list(loadGLMCSV(modelfiledir, "msmokeProb1.csv"),
                                    loadGLMCSV(modelfiledir, "msmokeProb2.csv"), loadGLMCSV(modelfiledir, "msmokeProb3.csv"))
    PropensityModels$kids <- list(loadGLMCSV(modelfiledir, "TempKidsProb1.csv"), 
                                  loadGLMCSV(modelfiledir, "TempKidsProb2.csv"), loadGLMCSV(modelfiledir, "TempKidsProb3.csv"),
                                  loadGLMCSV(modelfiledir, "TempKidsProb4.csv"))
    PropensityModels$z1single <- list(loadGLMCSV(modelfiledir, "TempZ1singleProb.csv"))
    PropensityModels$z1chpar <- list(loadGLMCSV(modelfiledir, "TempZ1chparProb.csv"))
    PropensityModels$chres <- list(loadGLMCSV(modelfiledir, "TempChresProb1.csv"), 
                                   loadGLMCSV(modelfiledir, "TempChresProb2.csv"))
    PropensityModels$welfare <- list(loadGLMCSV(modelfiledir, "TempWelfareProb.csv"))
    PropensityModels$mhrswrk <- list(loadGLMCSV(modelfiledir, "TempMhrswrkProb1.csv"),
                                     loadGLMCSV(modelfiledir, "TempMhrswrkProb2.csv"), loadGLMCSV(modelfiledir, "TempMhrswrkProb3.csv"),
                                     loadGLMCSV(modelfiledir, "TempMhrswrkProb4.csv"), loadGLMCSV(modelfiledir, "TempMhrswrkProb5.csv"))
    PropensityModels$fhrswrk <- list(loadGLMCSV(modelfiledir, "TempMhrswrkProb1.csv"),
                                     loadGLMCSV(modelfiledir, "TempMhrswrkProb2.csv"), loadGLMCSV(modelfiledir, "TempMhrswrkProb3.csv"),
                                     loadGLMCSV(modelfiledir, "TempMhrswrkProb4.csv"), loadGLMCSV(modelfiledir, "TempMhrswrkProb5.csv"))
    PropensityModels$z1accom <- list(loadGLMCSV(modelfiledir, "TempZ1accomProb.csv"))
    PropensityModels$z1homeown <- list(loadGLMCSV(modelfiledir, "TempZ1homeownProb.csv"))
    PropensityModels$z1overcrowd <- list(loadGLMCSV(modelfiledir, "TempZ1overcrowdProb.csv"))
    
    PropensityModels$z1single0 <- list(loadGLMCSV(modelfiledir, "TempZ1single0Prob.csv"))
    PropensityModels$BREAST <- list(loadGLMCSV(modelfiledir, "TempBreastProb0.csv"),
                                    loadGLMCSV(modelfiledir, "TempBreastProb1.csv"), loadGLMCSV(modelfiledir, "TempBreastProb2.csv"),
                                    loadGLMCSV(modelfiledir, "TempBreastProb3.csv"), loadGLMCSV(modelfiledir, "TempBreastProb4.csv"),
                                    loadGLMCSV(modelfiledir, "TempBreastProb5.csv"), loadGLMCSV(modelfiledir, "TempBreastProb6.csv"),
                                    loadGLMCSV(modelfiledir, "TempBreastProb7.csv"), loadGLMCSV(modelfiledir, "TempBreastProb8.csv"),
                                    loadGLMCSV(modelfiledir, "TempBreastProb9.csv"), loadGLMCSV(modelfiledir, "TempBreastProb10.csv"),
                                    loadGLMCSV(modelfiledir, "TempBreastProb11.csv"))
    PropensityModels$INTERACT <- list(loadGLMCSV(modelfiledir, "TempInteractProb2.csv"),
                                      loadGLMCSV(modelfiledir, "TempInteractProb3.csv"), loadGLMCSV(modelfiledir, "TempInteractProb4.csv"),
                                      loadGLMCSV(modelfiledir, "TempInteractProb5.csv"), loadGLMCSV(modelfiledir, "TempInteractProb6.csv"),
                                      loadGLMCSV(modelfiledir, "TempInteractProb7.csv"), loadGLMCSV(modelfiledir, "TempInteractProb8.csv"),
                                      loadGLMCSV(modelfiledir, "TempInteractProb9.csv"))
    PropensityModels$PUNISH <- list(loadGLMCSV(modelfiledir, "TempPunishProb0.csv"),
                                    loadGLMCSV(modelfiledir, "TempPunishProb1.csv"), loadGLMCSV(modelfiledir, "TempPunishProb2.csv"),
                                    loadGLMCSV(modelfiledir, "TempPunishProb3.csv"), loadGLMCSV(modelfiledir, "TempPunishProb4.csv"))
    PropensityModels$SESBTH <- list(loadGLMCSV(modelfiledir, "TempSESBTHProb1.csv"),
                                    loadGLMCSV(modelfiledir, "TempSESBTHProb2.csv"))
    PropensityModels$NPRESCH <- list(loadGLMCSV(modelfiledir, "TempNpreschProb0.csv"),
                                     loadGLMCSV(modelfiledir, "TempNpreschProb1.csv"), loadGLMCSV(modelfiledir, "TempNpreschProb2.csv"))
    PropensityModels$r1stchildethn <- list(loadGLMCSV(modelfiledir, "TempR1stchildethnProb1.csv"),
                                           loadGLMCSV(modelfiledir, "TempR1stchildethnProb2.csv"))
    PropensityModels$bwkg <- list(loadGLMCSV(modelfiledir, "TempBwkgProb1.csv"),
                                  loadGLMCSV(modelfiledir, "TempBwkgProb2.csv"), loadGLMCSV(modelfiledir, "TempBwkgProb3.csv"),
                                  loadGLMCSV(modelfiledir, "TempBwkgProb4.csv"))
    PropensityModels$r1stmeduc <- list(loadGLMCSV(modelfiledir, "TempR1stmeducProb1.csv"),
                                       loadGLMCSV(modelfiledir, "TempR1stmeducProb2.csv"))
    PropensityModels$r1stfeduc <- list(loadGLMCSV(modelfiledir, "TempR1stfeducProb1.csv"),
                                       loadGLMCSV(modelfiledir, "TempR1stfeducProb2.csv"))
    PropensityModels$fage <- list(loadGLMCSV(modelfiledir, "TempFageProb1.csv"),
                                  loadGLMCSV(modelfiledir, "TempFageProb2.csv"), loadGLMCSV(modelfiledir, "TempFageProb3.csv"),
                                  loadGLMCSV(modelfiledir, "TempFageProb4.csv"), loadGLMCSV(modelfiledir, "TempFageProb5.csv"))
    PropensityModels$pregalc <- list(loadGLMCSV(modelfiledir, "TempPregalcProb0.csv"),
                                     loadGLMCSV(modelfiledir, "TempPregalcProb1.csv"), loadGLMCSV(modelfiledir, "TempPregalcProb2.csv"),
                                     loadGLMCSV(modelfiledir, "TempPregalcProb3.csv"), loadGLMCSV(modelfiledir, "TempPregalcProb4.csv"),
                                     loadGLMCSV(modelfiledir, "TempPregalcProb5.csv"), loadGLMCSV(modelfiledir, "TempPregalcProb6.csv"),
                                     loadGLMCSV(modelfiledir, "TempPregalcProb7.csv"))
    PropensityModels$MAGE <- list(loadGLMCSV(modelfiledir, "TempMageProb1.csv"),
                                  loadGLMCSV(modelfiledir, "TempMageProb2.csv"), loadGLMCSV(modelfiledir, "TempMageProb3.csv"),
                                  loadGLMCSV(modelfiledir, "TempMageProb4.csv"), loadGLMCSV(modelfiledir, "TempMageProb5.csv"))
    PropensityModels$pregsmk <- list(loadGLMCSV(modelfiledir, "TempPregsmkProb1.csv"),
                                     loadGLMCSV(modelfiledir, "TempPregsmkProb2.csv"), loadGLMCSV(modelfiledir, "TempPregsmkProb3.csv"),
                                     loadGLMCSV(modelfiledir, "TempPregsmkProb4.csv"), loadGLMCSV(modelfiledir, "TempPregsmkProb5.csv"))
    
    cat("Loaded Propensity models\n")
    PropensityModels
  }
  
  #' Load the transition probabilities.
  #' 
  #' @param dir
  #'  directory holding transition probabilities
  #' 
  #' @return
  #'  a list of transition probabilities
  #' 
  #' @export
  #' @examples
  #' dir <- "C:/Workspace/simario/src/demo/data/transition_probabilities/"
  #' transition_probabilities <- loadTransitionProbabilities(dir)
  loadTransitionProbabilities <- function(dir) {
    transition_probabilities <- list()
    
    transition_probabilities$r1School <- read_csv(dir, "r1School_transition_probabilities.csv")
   
    transition_probabilities
  }
  
  
  basefiledir <- paste(getwd() ,"/base/",sep="")
  modelfiledir <- paste(getwd(),"/models/",sep="")
  catToCont.modelfiledir <- paste(getwd(), "/models_CatToCont/", sep="")
  propensityfiledir <- paste(getwd(), "/models_Propensities/", sep="")
  transition.probabilitiesfiledir <- paste(getwd(), "/transition_probabilities/", sep="")
  
  
  cat("Initialising KnowLab\n")
  
  descriptions_dataframe <- read_file(basefiledir, "KnowLab data dictionary.csv")
  codings_dataframe <- descriptions_dataframe
  dict<- createDict(descriptions_dataframe, codings_dataframe)
  
  #load initial basefile
  children <- loadBaseFileCSV(basefiledir, "synthBasefile_MhrswrkFixed_5000_New.csv") 
  
  #create simframe
  sfdef <- read_file(basefiledir, "simframedef.csv")
  simframe.master <- loadSimFrame(sfdef, children)
  
  #get indices of observations (ie: rows) basefile that have NAs
  #in at least one of their values used in the simulation (ie: simframe)
  nas <- attr(simframe.master, "na.action")
  
  #remove basefile rows with NAs in simframe
  children <- remove_rows_by_index(children, nas)
  cat(dim(children)[1], "children loaded\n")
  
  #setup rownames same as basefile
  rownames(simframe.master) <- rownames(children)		
  
  #load models
  models <- loadMELCModels(modelfiledir)
  checkModelVars(models, simframe.master)
  
  #load catToCont models
  catToContModels <- loadCatToContModels(catToCont.modelfiledir)
  #lapply(catToContModels, checkModelVars, simframe=simframe.master)
  
  #Load propensity models
  PropensityModels <- loadPropensityModels(propensityfiledir)
  #lapply(PropensityModels, checkModelVars, simframe=simframe.master)
  
  ###load propensities (calculates them from the propensity models)
  propensities <- loadMELCPropensities2(propensityfiledir, stochastic=TRUE)
  ##propensities <<- loadMELCPropensities("A0", children$A0, propensityfiledir, num.iterations)
  
  #load transition_probabilities
  transition_probabilities<- loadTransitionProbabilities(transition.probabilitiesfiledir)
  
  #load aux
  binbreaks <- createBinBreaks(children)
  childsets <- createChildSets(children, dict$codings)
  
  #add in limits
  limits <- list()
  limits$kids <- 10
  limits$householdsize <- 14
  limits$chres <- 13
  limits$mhrswrk <- 100
  limits$fhrswrk <- 100
  limits$msmoke <- 70
  limits$fsmoke <- 70
  
  
  cat("KnowLab initialised\n")
  
  NUM_ITERATIONS <<- num.iterations
	dict <<- dict
	limits <<- limits
	binbreaks <<- binbreaks
	catToContModels <<- catToContModels
	models <<- models
	PropensityModels <<- PropensityModels
	transition_probabilities<<- transition_probabilities
	children <<- children

  list(dict = dict, 
       binbreaks = binbreaks,
       childsets = childsets,
       children = children, 
       simframe = simframe.master ,
       models = models,
       catToContModels = catToContModels,
       PropensityModels = PropensityModels,
       propensities = propensities,
       transition_probabilities = transition_probabilities,
       limits = limits)
}

