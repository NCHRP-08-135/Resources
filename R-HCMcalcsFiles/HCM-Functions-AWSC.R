# HCM All-Way Stop Control Intersection Calculations
# Version 1.0, 10/17/23
# By Scott Washburn, Ryan Kenis, Emily Dang; University of Florida


AWSCMainFunction <- function(PHF, EBThrough, EBLeft, EBRight, NBThrough, NBLeft, NBRight, WBThrough, WBLeft, WBRight, SBThrough, SBLeft, SBRight, BaseSatHeadway, Convergence, ConvergenceCriteria, DepartureHeadwayLast, EBPHV, NBPHV, WBPHV, SBPHV, MoveUpTime14){
  StopYieldLOSDF <- data.frame(ControlDelayThreshold <- c(0, 10, 15, 25, 35, 50, 100), NumericLOSthresholds <- c(0, 1, 2, 3, 4, 5, 5))
  EBTotalVolume <- DirectionalVolumeFcn(EBThrough, EBLeft, EBRight)
  NBTotalVolume <- DirectionalVolumeFcn(NBThrough, NBLeft, NBRight)
  WBTotalVolume <- DirectionalVolumeFcn(WBThrough, WBLeft, WBRight)
  SBTotalVolume <- DirectionalVolumeFcn(SBThrough, SBLeft, SBRight)
  DemandFlowRateEBThrough <- DemandFlowRateFcn(EBThrough, PHF)
  DemandFlowRateEBLeft <- DemandFlowRateFcn(EBLeft, PHF)
  DemandFlowRateEBRight <- DemandFlowRateFcn(EBRight, PHF)
  DemandFlowRateNBThrough <- DemandFlowRateFcn(NBThrough, PHF)
  DemandFlowRateNBLeft <- DemandFlowRateFcn(NBLeft, PHF)
  DemandFlowRateNBRight <- DemandFlowRateFcn(NBRight, PHF)
  DemandFlowRateWBThrough <- DemandFlowRateFcn(WBThrough, PHF)
  DemandFlowRateWBLeft <- DemandFlowRateFcn(WBLeft, PHF)
  DemandFlowRateWBRight <- DemandFlowRateFcn(WBRight, PHF)
  DemandFlowRateSBThrough <- DemandFlowRateFcn(SBThrough, PHF)
  DemandFlowRateSBLeft <- DemandFlowRateFcn(SBLeft, PHF)
  DemandFlowRateSBRight <- DemandFlowRateFcn(SBRight, PHF)
  EBDemandFlowTotal <- TotalDemandFcn(DemandFlowRateEBThrough, DemandFlowRateEBLeft, DemandFlowRateEBRight)
  NBDemandFlowTotal <- TotalDemandFcn(DemandFlowRateNBThrough, DemandFlowRateNBLeft, DemandFlowRateNBRight)
  WBDemandFlowTotal <- TotalDemandFcn(DemandFlowRateWBThrough, DemandFlowRateWBLeft, DemandFlowRateWBRight)
  SBDemandFlowTotal <- TotalDemandFcn(DemandFlowRateSBThrough, DemandFlowRateSBLeft, DemandFlowRateSBRight)
  EBPLT <- SatHeadwayVariablesFcn(DemandFlowRateEBLeft, EBDemandFlowTotal)
  EBPRT <- SatHeadwayVariablesFcn(DemandFlowRateEBRight, EBDemandFlowTotal)
  NBPLT <- SatHeadwayVariablesFcn(DemandFlowRateNBLeft, NBDemandFlowTotal)
  NBPRT <- SatHeadwayVariablesFcn(DemandFlowRateNBRight, NBDemandFlowTotal)
  WBPLT <- SatHeadwayVariablesFcn(DemandFlowRateWBLeft, WBDemandFlowTotal)
  WBPRT <- SatHeadwayVariablesFcn(DemandFlowRateWBRight, WBDemandFlowTotal)
  SBPLT <- SatHeadwayVariablesFcn(DemandFlowRateSBLeft, SBDemandFlowTotal)
  SBPRT <- SatHeadwayVariablesFcn(DemandFlowRateSBRight, SBDemandFlowTotal)
  EBHeadwayAdjustment <- SatHeadwayAdjFcn(EBPLT, EBPRT, EBPHV)
  NBHeadwayAdjustment <- SatHeadwayAdjFcn(NBPLT, NBPRT, NBPHV)
  WBHeadwayAdjustment <- SatHeadwayAdjFcn(WBPLT, WBPRT, WBPHV)
  SBHeadwayAdjustment <- SatHeadwayAdjFcn(SBPLT, SBPRT, SBPHV)
  DegreeOfUtilization <- vector()
  DepartureHeadway <- vector()
  HdwyDelta <- vector()
  ProbabilityCase1 <- vector()
  ProbabilityCase2 <- vector()
  ProbabilityCase3 <- vector()
  ProbabilityCase4 <- vector()
  ProbabilityCase5 <- vector()
  EBAdjustedProbability <- vector()   #The indexes correspond to the five conflict cases
  ProbabilityAdjustment1 <- vector()  #The indexes correspond to conflicts scenario numbers - 1, 2, 5, 16, 45
  EBSaturationHeadway <- vector()
  NBSaturationHeadway <- vector()
  WBSaturationHeadway <- vector()
  SBSaturationHeadway <- vector()
  ApprEB <- 1
  ApprNB <- 2
  ApprWB <- 3
  ApprSB <- 4
  
  while(Convergence == FALSE){
    #DemandFlowTotal can be converted to a vector and then we can use a loop to set deg of utilization
    DegreeOfUtilization[ApprEB] <- DegOfUtilFcn(EBDemandFlowTotal, DepartureHeadwayLast[ApprEB])
    DegreeOfUtilization[ApprNB] <- DegOfUtilFcn(NBDemandFlowTotal, DepartureHeadwayLast[ApprNB])
    DegreeOfUtilization[ApprWB] <- DegOfUtilFcn(WBDemandFlowTotal, DepartureHeadwayLast[ApprWB])
    DegreeOfUtilization[ApprSB] <- DegOfUtilFcn(SBDemandFlowTotal, DepartureHeadwayLast[ApprSB])
    
    ProbabilityCase1[ApprEB] <- ProbabilityCase1Fcn(DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprNB])
    ProbabilityCase1[ApprNB] <- ProbabilityCase1Fcn(DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprWB])
    ProbabilityCase1[ApprWB] <- ProbabilityCase1Fcn(DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprSB])
    ProbabilityCase1[ApprSB] <- ProbabilityCase1Fcn(DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprEB])
    ProbabilityCase2[ApprEB] <- ProbabilityCase2Fcn(DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprNB])
    ProbabilityCase2[ApprNB] <- ProbabilityCase2Fcn(DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprWB])
    ProbabilityCase2[ApprWB] <- ProbabilityCase2Fcn(DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprSB])
    ProbabilityCase2[ApprSB] <- ProbabilityCase2Fcn(DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprEB])
    ProbabilityCase3[ApprEB] <- ProbabilityCase3Fcn(DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprNB])
    ProbabilityCase3[ApprNB] <- ProbabilityCase3Fcn(DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprWB])
    ProbabilityCase3[ApprWB] <- ProbabilityCase3Fcn(DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprSB])
    ProbabilityCase3[ApprSB] <- ProbabilityCase3Fcn(DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprEB])
    ProbabilityCase4[ApprEB] <- ProbabilityCase4Fcn(DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprNB])
    ProbabilityCase4[ApprNB] <- ProbabilityCase4Fcn(DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprWB])
    ProbabilityCase4[ApprWB] <- ProbabilityCase4Fcn(DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprSB])
    ProbabilityCase4[ApprSB] <- ProbabilityCase4Fcn(DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprEB])
    ProbabilityCase5[ApprEB] <- ProbabilityCase5Fcn(DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprNB])
    ProbabilityCase5[ApprNB] <- ProbabilityCase5Fcn(DegreeOfUtilization[ApprSB], DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprWB])
    ProbabilityCase5[ApprWB] <- ProbabilityCase5Fcn(DegreeOfUtilization[ApprEB], DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprSB])
    ProbabilityCase5[ApprSB] <- ProbabilityCase5Fcn(DegreeOfUtilization[ApprNB], DegreeOfUtilization[ApprWB], DegreeOfUtilization[ApprEB])
    
    ProbabilityAdjustment1[ApprEB] <- ProbabilityAdjustment1Fcn(ProbabilityCase2[ApprEB], ProbabilityCase3[ApprEB], ProbabilityCase4[ApprEB], ProbabilityCase5[ApprEB])
    NBProbabilityAdjustment1 <- ProbabilityAdjustment1Fcn(ProbabilityCase2[ApprNB], ProbabilityCase3[ApprNB], ProbabilityCase4[ApprNB], ProbabilityCase5[ApprNB])
    WBProbabilityAdjustment1 <- ProbabilityAdjustment1Fcn(ProbabilityCase2[ApprWB], ProbabilityCase3[ApprWB], ProbabilityCase4[ApprWB], ProbabilityCase5[ApprWB])
    SBProbabilityAdjustment1 <- ProbabilityAdjustment1Fcn(ProbabilityCase2[ApprSB], ProbabilityCase3[ApprSB], ProbabilityCase4[ApprSB], ProbabilityCase5[ApprSB])
    EBProbabilityAdjustment2 <- ProbabilityAdjustment2Fcn(ProbabilityCase2[ApprEB], ProbabilityCase3[ApprEB], ProbabilityCase4[ApprEB], ProbabilityCase5[ApprEB])
    NBProbabilityAdjustment2 <- ProbabilityAdjustment2Fcn(ProbabilityCase2[ApprNB], ProbabilityCase3[ApprNB], ProbabilityCase4[ApprNB], ProbabilityCase5[ApprNB])
    WBProbabilityAdjustment2 <- ProbabilityAdjustment2Fcn(ProbabilityCase2[ApprWB], ProbabilityCase3[ApprWB], ProbabilityCase4[ApprWB], ProbabilityCase5[ApprWB])
    SBProbabilityAdjustment2 <- ProbabilityAdjustment2Fcn(ProbabilityCase2[ApprSB], ProbabilityCase3[ApprSB], ProbabilityCase4[ApprSB], ProbabilityCase5[ApprSB])
    EBProbabilityAdjustment5 <- ProbabilityAdjustment5Fcn(ProbabilityCase3[ApprEB], ProbabilityCase4[ApprEB], ProbabilityCase5[ApprEB])
    NBProbabilityAdjustment5 <- ProbabilityAdjustment5Fcn(ProbabilityCase3[ApprNB], ProbabilityCase4[ApprNB], ProbabilityCase5[ApprNB])
    WBProbabilityAdjustment5 <- ProbabilityAdjustment5Fcn(ProbabilityCase3[ApprWB], ProbabilityCase4[ApprWB], ProbabilityCase5[ApprWB])
    SBProbabilityAdjustment5 <- ProbabilityAdjustment5Fcn(ProbabilityCase3[ApprSB], ProbabilityCase4[ApprSB], ProbabilityCase5[ApprSB])
    EBProbabilityAdjustment16 <- ProbabilityAdjustment16Fcn(ProbabilityCase4[ApprEB], ProbabilityCase5[ApprEB])
    NBProbabilityAdjustment16 <- ProbabilityAdjustment16Fcn(ProbabilityCase4[ApprNB], ProbabilityCase5[ApprNB])
    WBProbabilityAdjustment16 <- ProbabilityAdjustment16Fcn(ProbabilityCase4[ApprWB], ProbabilityCase5[ApprWB])
    SBProbabilityAdjustment16 <- ProbabilityAdjustment16Fcn(ProbabilityCase4[ApprSB], ProbabilityCase5[ApprSB])
    EBProbabilityAdjustment45 <- ProbabilityAdjustment45Fcn(ProbabilityCase5[ApprEB])
    NBProbabilityAdjustment45 <- ProbabilityAdjustment45Fcn(ProbabilityCase5[ApprNB])
    WBProbabilityAdjustment45 <- ProbabilityAdjustment45Fcn(ProbabilityCase5[ApprWB])
    SBProbabilityAdjustment45 <- ProbabilityAdjustment45Fcn(ProbabilityCase5[ApprSB])
    EBAdjustedProbability1 <- ProbabilityCase1[ApprEB]+ProbabilityAdjustment1[ApprEB]
    EBAdjustedProbability2 <- ProbabilityCase2[ApprEB]+EBProbabilityAdjustment2
    EBAdjustedProbability3 <- ProbabilityCase3[ApprEB]+EBProbabilityAdjustment5
    EBAdjustedProbability4 <- ProbabilityCase4[ApprEB]+EBProbabilityAdjustment16
    EBAdjustedProbability5 <- ProbabilityCase5[ApprEB]+EBProbabilityAdjustment45
    
    #for(i in 1:5){ #iterate through the 5 conflict cases
    #  EBAdjustedProbability[i] <- AdjustedProbabilityFcn(ProbabilityCase1[ApprEB],ProbabilityAdjustment1[i])
    #}
    
    #EBAdjustedProbability1 <- AdjustedProbabilityFcn(ProbabilityCase1[ApprEB],EBProbabilityAdjustment1)
    #EBAdjustedProbability2 <- AdjustedProbabilityFcn(ProbabilityCase2[ApprEB],EBProbabilityAdjustment2)
    #EBAdjustedProbability3 <- AdjustedProbabilityFcn(ProbabilityCase3[ApprEB],EBProbabilityAdjustment5)
    #EBAdjustedProbability4 <- AdjustedProbabilityFcn(ProbabilityCase4[ApprEB],EBProbabilityAdjustment16)
    #EBAdjustedProbability5 <- AdjustedProbabilityFcn(ProbabilityCase5[ApprEB],EBProbabilityAdjustment45)
    
    NBAdjustedProbability1 <- AdjustedProbabilityFcn(ProbabilityCase1[ApprNB],NBProbabilityAdjustment1)
    NBAdjustedProbability2 <- AdjustedProbabilityFcn(ProbabilityCase2[ApprNB],NBProbabilityAdjustment2)
    NBAdjustedProbability3 <- AdjustedProbabilityFcn(ProbabilityCase3[ApprNB],NBProbabilityAdjustment5)
    NBAdjustedProbability4 <- AdjustedProbabilityFcn(ProbabilityCase4[ApprNB],NBProbabilityAdjustment16)
    NBAdjustedProbability5 <- AdjustedProbabilityFcn(ProbabilityCase5[ApprNB],NBProbabilityAdjustment45)
    
    WBAdjustedProbability1 <- AdjustedProbabilityFcn(ProbabilityCase1[ApprWB],WBProbabilityAdjustment1)
    WBAdjustedProbability2 <- AdjustedProbabilityFcn(ProbabilityCase2[ApprWB],WBProbabilityAdjustment2)
    WBAdjustedProbability3 <- AdjustedProbabilityFcn(ProbabilityCase3[ApprWB],WBProbabilityAdjustment5)
    WBAdjustedProbability4 <- AdjustedProbabilityFcn(ProbabilityCase4[ApprWB],WBProbabilityAdjustment16)
    WBAdjustedProbability5 <- AdjustedProbabilityFcn(ProbabilityCase5[ApprWB],WBProbabilityAdjustment45)
    
    SBAdjustedProbability1 <- AdjustedProbabilityFcn(ProbabilityCase1[ApprSB],SBProbabilityAdjustment1)
    SBAdjustedProbability2 <- AdjustedProbabilityFcn(ProbabilityCase2[ApprSB],SBProbabilityAdjustment2)
    SBAdjustedProbability3 <- AdjustedProbabilityFcn(ProbabilityCase3[ApprSB],SBProbabilityAdjustment5)
    SBAdjustedProbability4 <- AdjustedProbabilityFcn(ProbabilityCase4[ApprSB],SBProbabilityAdjustment16)
    SBAdjustedProbability5 <- AdjustedProbabilityFcn(ProbabilityCase5[ApprSB],SBProbabilityAdjustment45)
    
    for(i in 1:5){
      EBSaturationHeadway[i] <- SatHeadwayFcn(BaseSatHeadway[i], EBHeadwayAdjustment)
      NBSaturationHeadway[i] <- SatHeadwayFcn(BaseSatHeadway[i], NBHeadwayAdjustment)
      WBSaturationHeadway[i] <- SatHeadwayFcn(BaseSatHeadway[i], WBHeadwayAdjustment)
      SBSaturationHeadway[i] <- SatHeadwayFcn(BaseSatHeadway[i], SBHeadwayAdjustment)
    }
    
    
    #EBSaturationHeadway1 <- SatHeadwayFcn(BaseSatHeadway[1], EBHeadwayAdjustment)
    #EBSaturationHeadway2 <- SatHeadwayFcn(BaseSatHeadway[2], EBHeadwayAdjustment)
    #EBSaturationHeadway3 <- SatHeadwayFcn(BaseSatHeadway[3], EBHeadwayAdjustment)
    #EBSaturationHeadway4 <- SatHeadwayFcn(BaseSatHeadway[4], EBHeadwayAdjustment)
    #EBSaturationHeadway5 <- SatHeadwayFcn(BaseSatHeadway[5], EBHeadwayAdjustment)
    
    #NBSaturationHeadway1 <- SatHeadwayFcn(BaseSatHeadway[1], NBHeadwayAdjustment)
    #NBSaturationHeadway2 <- SatHeadwayFcn(BaseSatHeadway[2], NBHeadwayAdjustment)
    #NBSaturationHeadway3 <- SatHeadwayFcn(BaseSatHeadway[3], NBHeadwayAdjustment)
    #NBSaturationHeadway4 <- SatHeadwayFcn(BaseSatHeadway[4], NBHeadwayAdjustment)
    #NBSaturationHeadway5 <- SatHeadwayFcn(BaseSatHeadway[5], NBHeadwayAdjustment)
    
    #WBSaturationHeadway1 <- SatHeadwayFcn(BaseSatHeadway[1], WBHeadwayAdjustment)
    #WBSaturationHeadway2 <- SatHeadwayFcn(BaseSatHeadway[2], WBHeadwayAdjustment)
    #WBSaturationHeadway3 <- SatHeadwayFcn(BaseSatHeadway[3], WBHeadwayAdjustment)
    #WBSaturationHeadway4 <- SatHeadwayFcn(BaseSatHeadway[4], WBHeadwayAdjustment)
    #WBSaturationHeadway5 <- SatHeadwayFcn(BaseSatHeadway[5], WBHeadwayAdjustment)
    
    #SBSaturationHeadway1 <- SatHeadwayFcn(BaseSatHeadway[1], SBHeadwayAdjustment)
    #SBSaturationHeadway2 <- SatHeadwayFcn(BaseSatHeadway[2], SBHeadwayAdjustment)
    #SBSaturationHeadway3 <- SatHeadwayFcn(BaseSatHeadway[3], SBHeadwayAdjustment)
    #SBSaturationHeadway4 <- SatHeadwayFcn(BaseSatHeadway[4], SBHeadwayAdjustment)
    #SBSaturationHeadway5 <- SatHeadwayFcn(BaseSatHeadway[5], SBHeadwayAdjustment)
    
    DepartureHeadway[ApprEB] <- DepartureHeadwayFcn(ProbabilityCase1[ApprEB], EBSaturationHeadway[1], ProbabilityCase2[ApprEB], EBSaturationHeadway[2], ProbabilityCase3[ApprEB], EBSaturationHeadway[3], ProbabilityCase4[ApprEB], EBSaturationHeadway[4], ProbabilityCase5[ApprEB], EBSaturationHeadway[5])
    
    DepartureHeadway[ApprNB] <- DepartureHeadwayFcn(ProbabilityCase1[ApprNB], NBSaturationHeadway[1], ProbabilityCase2[ApprNB], NBSaturationHeadway[2], ProbabilityCase3[ApprNB], NBSaturationHeadway[3], ProbabilityCase4[ApprNB], NBSaturationHeadway[4], ProbabilityCase5[ApprNB], NBSaturationHeadway[5])
    
    DepartureHeadway[ApprWB] <- DepartureHeadwayFcn(ProbabilityCase1[ApprWB], WBSaturationHeadway[1], ProbabilityCase2[ApprWB], WBSaturationHeadway[2], ProbabilityCase3[ApprWB], WBSaturationHeadway[3], ProbabilityCase4[ApprWB], WBSaturationHeadway[4], ProbabilityCase5[ApprWB], WBSaturationHeadway[5])
    
    DepartureHeadway[ApprSB] <- DepartureHeadwayFcn(ProbabilityCase1[ApprSB], SBSaturationHeadway[1], ProbabilityCase2[ApprSB], SBSaturationHeadway[2], ProbabilityCase3[ApprSB], SBSaturationHeadway[3], ProbabilityCase4[ApprSB], SBSaturationHeadway[4], ProbabilityCase5[ApprSB], SBSaturationHeadway[5])
    
    for(i in 1:4){ #iterate through the 4 approaches
      HdwyDelta[i] <- DepartureHeadway[i]-DepartureHeadwayLast[i]
      DepartureHeadwayLast[i] <- DepartureHeadway[i]
    }
    
    
    #HdwyDelta[ApprEB] <- DepartureHeadway[ApprEB]-DepartureHeadwayLast[ApprEB]
    #HdwyDelta[ApprNB] <- NBDepartureHeadway-DepartureHeadwayLast[2]
    #HdwyDelta[ApprWB] <- WBDepartureHeadway-DepartureHeadwayLast[3]
    #HdwyDelta[ApprSB] <- SBDepartureHeadway-DepartureHeadwayLast[4]
    
    #DepartureHeadwayLast[ApprEB] <- DepartureHeadway[ApprEB]
    #DepartureHeadwayLast[ApprNB] <- DepartureHeadway[ApprNB]
    #DepartureHeadwayLast[ApprWB] <- DepartureHeadway[ApprWB]
    #DepartureHeadwayLast[ApprSB] <- DepartureHeadway[ApprSB]
    
    if (HdwyDelta[1]<ConvergenceCriteria&HdwyDelta[2]<ConvergenceCriteria&HdwyDelta[3]<ConvergenceCriteria&HdwyDelta[4]<ConvergenceCriteria){
      Convergence = TRUE
    }
  }
  
  EBServiceTime <- ServiceTimeFcn(DepartureHeadway[ApprEB], MoveUpTime14)
  ControlDelay <- ControlDelayFcn(DegreeOfUtilization[1], DepartureHeadway[ApprEB], EBServiceTime)
  LOS <- LOSFcn(ControlDelay)
  LOSNumber <- approx(StopYieldLOSDF$ControlDelayThreshold, StopYieldLOSDF$NumericLOSthresholds, xout = ControlDelay)
  OutputDisplayFcn(EBTotalVolume, EBHeadwayAdjustment, DepartureHeadway[1], DegreeOfUtilization[1], ControlDelay, LOS[1], LOSNumber)
  Results <- c(ControlDelay, LOSNumber[[2]])
  return(Results)
}


##Eq. 21-12 Demand Flow Rate

DemandFlowRateFcn <- function(DemandVolume, PHF){
  DemandFlowRate <- DemandVolume/PHF
  return(DemandFlowRate)
}
TotalDemandFcn <- function(Through, Left, Right){
  DemandFlowTotal <- Through+Left+Right
  return(DemandFlowTotal)
}

#IndividualDemandChunk
DirectionalVolumeFcn <- function(Through, Left, Right){
  TotalVolume <- Through+Left+Right
  return(TotalVolume)
}

##SaturationHeadwayAdjustmentVariables Chunk
SatHeadwayVariablesFcn <- function(DemandFlowRate, TotalDemandFlow){
  SatHeadwayAdjustment <- DemandFlowRate/TotalDemandFlow
  return(SatHeadwayAdjustment)
}


# SatruationHeadwayAdjustment Chuck
SatHeadwayAdjFcn <- function(PLT, PRT, PHV){
  HLT <- 0.2 #Given from Exhibit 21-12 and Step 3
  HRT <- -0.6 #Given from Exhibit 21-12 and Step 3
  HHV <- 1.7 #Given from Exhibit 21-12 and Step 3
  EBPHV <- 0.03 #Given
  NBPHV <- 0.02 #Given
  WBPHV <- 0.03 #Given
  SBPHV <- 0.02 #Given
  HeadwayAdjustment <- HLT*PLT+HRT*PRT+HHV*PHV
  return(HeadwayAdjustment)
}

##Eq. 21-14 Degree of Utilization

DegOfUtilFcn <- function(DemandFlowTotal, Headway){
  DegreeOfUtilization <- (DemandFlowTotal*Headway)/3600
  return(DegreeOfUtilization)
}

##Exhibit 21-5

ProbabilityCase1Fcn <- function(OpposingDegreeofUtilization, LeftDegreeofUtilization, RightDegreeofUtilization){
  ProbabliltyCase1 <- (1-OpposingDegreeofUtilization)*(1-LeftDegreeofUtilization)*(1-RightDegreeofUtilization)
  return(ProbabliltyCase1)
}

ProbabilityCase2Fcn <- function(OpposingDegreeofUtilization, LeftDegreeofUtilization, RightDegreeofUtilization){
  ProbabliltyCase2 <- (OpposingDegreeofUtilization)*(1-LeftDegreeofUtilization)*(1-RightDegreeofUtilization)
  return(ProbabliltyCase2)
}

ProbabilityCase3Fcn <- function(OpposingDegreeofUtilization, LeftDegreeofUtilization, RightDegreeofUtilization){
  ProbabliltyCase3 <- (1-OpposingDegreeofUtilization)*(LeftDegreeofUtilization)*(1-RightDegreeofUtilization)+(1-OpposingDegreeofUtilization)*(1-LeftDegreeofUtilization)*(RightDegreeofUtilization)
  return(ProbabliltyCase3)
}

ProbabilityCase4Fcn <- function(OpposingDegreeofUtilization, LeftDegreeofUtilization, RightDegreeofUtilization){
  ProbabliltyCase4 <- (OpposingDegreeofUtilization)*(1-LeftDegreeofUtilization)*(RightDegreeofUtilization)+(OpposingDegreeofUtilization)*(LeftDegreeofUtilization)*(1-RightDegreeofUtilization)+(1-OpposingDegreeofUtilization)*(LeftDegreeofUtilization)*(RightDegreeofUtilization)
  return(ProbabliltyCase4)
}

ProbabilityCase5Fcn <- function(OpposingDegreeofUtilization, LeftDegreeofUtilization, RightDegreeofUtilization){
  ProbabliltyCase5 <- (OpposingDegreeofUtilization)*(LeftDegreeofUtilization)*(RightDegreeofUtilization)
  return(ProbabliltyCase5)
}

##Eq. 21-21 Probability Adjustment

ProbabilityAdjustment1Fcn <- function(ProbabilityCase2, ProbabilityCase3, ProbabilityCase4, ProbabilityCase5){
  Alpha <- 0.01 #Given in Step 8
  ProbabliltyAdjustment1 <- Alpha*(ProbabilityCase2+2*ProbabilityCase3+3*ProbabilityCase4+4*ProbabilityCase5)/1
  return(ProbabliltyAdjustment1)
}

##Eq. 21-22 Probability Adjustment

ProbabilityAdjustment2Fcn <- function(ProbabilityCase2, ProbabilityCase3, ProbabilityCase4, ProbabilityCase5){
  ApprEB <- 1
  ApprNB <- 2
  ApprWB <- 3
  ApprSB <- 4
  Alpha <- 0.01 #Given in Step 8
  ProbabliltyAdjustment2 <- Alpha*(ProbabilityCase3[ApprEB]+2*ProbabilityCase4[ApprEB]+3*ProbabilityCase5[ApprEB]-ProbabilityCase2[ApprEB])/3
  return(ProbabliltyAdjustment2)
}

##Eq. 21-23 Probability Adjustment

ProbabilityAdjustment5Fcn <- function(ProbabilityCase3, ProbabilityCase4, ProbabilityCase5){
  ApprEB <- 1
  ApprNB <- 2
  ApprWB <- 3
  ApprSB <- 4
  Alpha <- 0.01 #Given in Step 8
  ProbabliltyAdjustment5 <- Alpha*(ProbabilityCase4[ApprEB]+2*ProbabilityCase5[ApprEB]+3*ProbabilityCase3[ApprEB])/6
  return(ProbabliltyAdjustment5)
}

##Eq. 21-24 Probability Adjustment

ProbabilityAdjustment16Fcn <- function(ProbabilityCase4, ProbabilityCase5){
  ApprEB <- 1
  ApprNB <- 2
  ApprWB <- 3
  ApprSB <- 4
  Alpha <- 0.01 #Given in Step 8
  ProbabliltyAdjustment16 <- Alpha*(ProbabilityCase5[ApprEB]+6*ProbabilityCase4[ApprEB])/27
  return(ProbabliltyAdjustment16)
}

##Eq. 21-25 Probability Adjustment

ProbabilityAdjustment45Fcn <- function(ProbabilityCase5){
  ApprEB <- 1
  ApprNB <- 2
  ApprWB <- 3
  ApprSB <- 4
  Alpha <- 0.01 #Given in Step 8
  ProbabliltyAdjustment45 <- -Alpha*(10*ProbabilityCase5[ApprEB])/27
  return(ProbabliltyAdjustment45)
}

##Eq. 21-26 Probability Adjustment

AdjustedProbabilityFcn <- function(ProbabilityCase, ProbabilityAdjustment){
  AdjustedProbability <- ProbabilityCase+ProbabilityAdjustment
  return(AdjustedProbability)
}

##Eq. 21-27 Saturation Headways

SatHeadwayFcn <- function(BaseSatHeadway, HeadwayAdjustment){
  SaturationHeadway <- BaseSatHeadway+HeadwayAdjustment
  return(SaturationHeadway)
}

##Eq. 21-28 Departure Headways

DepartureHeadwayFcn <- function(ProbabilityCase1, SaturationHeadway1, ProbabilityCase2, SaturationHeadway2, ProbabilityCase3, SaturationHeadway3, ProbabilityCase4, SaturationHeadway4, ProbabilityCase5, SaturationHeadway5){
  DepartureHeadway <- ProbabilityCase1*SaturationHeadway1+ProbabilityCase2*SaturationHeadway2+ProbabilityCase3*SaturationHeadway3+ProbabilityCase4*SaturationHeadway4+ProbabilityCase5*SaturationHeadway5
  return(DepartureHeadway)
}

#Eq. 21-29 Service Time
ServiceTimeFcn <- function(DepartureHeadway, MoveUpTime){
  ServiceTime <- DepartureHeadway-MoveUpTime
  return(ServiceTime)
}

#Eq. 21-30 Control Delay

ControlDelayFcn <- function(DegreeOfUtilization, DepartureHeadway, ServiceTime){
  AnalysisPeriod <- 0.25
  Term2 <- 900*AnalysisPeriod
  Term3 <- DegreeOfUtilization-1
  Term4 <- sqrt((Term3^2)+(DepartureHeadway*DegreeOfUtilization)/(450*AnalysisPeriod))
  ControlDelay <- ServiceTime+Term2*(Term3+Term4)+5
  return(ControlDelay)
}


# HCM Exhibit 21-8
LOSFcn <- function(ControlDelay){
  if (ControlDelay <= 10) {
    LOSvalue <- "A"
  }
  else if (ControlDelay <= 15) {
    LOSvalue <- "B"
  }
  else if (ControlDelay <= 25) {
    LOSvalue <- "C"
  }
  else if (ControlDelay <= 35) {
    LOSvalue <- "D"
  }
  else if (ControlDelay <= 50) {
    LOSvalue <- "E"
  }
  else {
    LOSvalue <- "F"
  }
  
  return(LOSvalue)
}


OutputDisplayFcn <- function(TotalVolume, HeadwayAdjustment, DepartureHeadway, DegreeOfUtilization, ControlDelay, LOS, LOSNumber){
  cat("Eastbound Total Volume =", format(round(TotalVolume, 1), nsmall=1), "veh/h \n")
  cat("Eastbound Adjusted Headway =", format(round(HeadwayAdjustment, 4), nsmall=4), "s \n")  
  cat("Degree of Utilization =", format(round(DegreeOfUtilization, 3), nsmall=3), "\n")
  cat("Eastbound Departure Headway =", format(round(DepartureHeadway, 1), nsmall=1), "s \n")
  cat("Eastbound Control Delay =", format(round(ControlDelay, 1), nsmall=1), "s \n")
  cat("LOS =", LOS, "\n")
  cat("LOS numeric value =", LOSNumber[[2]])
}