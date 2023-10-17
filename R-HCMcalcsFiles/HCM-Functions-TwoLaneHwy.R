# HCM Two-Lane Highway Segment Calculations
# Version 1.0, 10/17/23
# By Scott Washburn, Ryan Kenis, Emily Dang; University of Florida


TwoLaneMainFcn <- function(SegmentType, DemandVolAnalysisDir, DemandVolOpposingDir, PHF, HeavyVehPercent, PostedSpeedLimit, SegLength, AccessPointDensity, LaneWidth, ShoulderWidth, VertAlignmentClass, UpstreamSegResults, IsPassingLaneUpStream, UpStreamPassLaneLengthMi, DownStreamDistMi, EffectiveLengthMi){
  ReturnAvgSpeed <- 0
  ReturnPctFollowers <- 0
  ReturnFollowerDensity <- 0
  ReturnEffectiveLength <- 0

  TwoLaneLOSDF <- data.frame(FollowerDensityThresholds <- c(0, 2, 4, 8, 12, 18), NumericLOSthresholds <- c(0, 1, 2, 3, 4, 5))
  Capacity <- CapacityFcn(SegmentType,VertAlignmentClass,HeavyVehPercent)
  AnalysisFlowRate <- TwoLaneAnalysisFlowRateFcn(DemandVolAnalysisDir, PHF)
  BFFS <- TwoLaneBFFSFcn(PostedSpeedLimit)
  AcoeffsBFFS <- Coeffa(VertAlignmentClass)
  a = aFcn(AcoeffsBFFS, BFFS, SegLength, DemandVolOpposingDir)
  LaneShoulderAdj <- TwoLaneShoulderAdjFcn(LaneWidth, ShoulderWidth)
  AccessPointAdj <- TwoLaneAccessDensityFcn(AccessPointDensity)
  MinSegmentLength <- MinSegmentLengthFcn(VertAlignmentClass, SegmentType)
  MaxSegmentLength <- MaxSegmentLengthFcn(VertAlignmentClass, SegmentType)
  FFS <- TwoLaneFFSFcn(BFFS, a, HeavyVehPercent, LaneShoulderAdj, AccessPointAdj)
  
  SpeedcCoeffsSpeed <- SpeedcCoeffFcn(SegmentType, VertAlignmentClass)
  SpeeddCoeffsSpeed <- SpeeddCoeffFcn(SegmentType, VertAlignmentClass)
  
  PFbCoeffs <- PFbCoeffFcn(SegmentType, VertAlignmentClass)
  PFcCoeffs <- PFcCoeffFcn(SegmentType, VertAlignmentClass)
  PFdCoeffs <- PFCoeffD(SegmentType)
  PFeCoeffs <- PFCoeffE(SegmentType)
  
  if (SegmentType != "PL"){
    BcoeffsSpeed <- SpeedCoeffbNonPassingLane(VertAlignmentClass, SpeedcCoeffsSpeed, SpeeddCoeffsSpeed, SegLength, FFS, HeavyVehPercent)
    FcoeffsSpeed <- SpeedfCoeffNonPassingLane(VertAlignmentClass)
    AvgSpeedSlopeCoeff <- TwoLaneAvgSpeedSlopeCoeffFcn(BcoeffsSpeed, FFS, DemandVolOpposingDir, SegLength, HeavyVehPercent)
    AvgSpeedPowerCoeff <- TwoLaneAvgSpeedPowerCoeffFcn(FcoeffsSpeed, FFS, SegLength, DemandVolOpposingDir, HeavyVehPercent)
    AvgSpeed <- TwoLaneAvgSpeedFcn(AnalysisFlowRate, FFS, AvgSpeedSlopeCoeff, AvgSpeedPowerCoeff)

    PercentFollowerCapacity <- TwoLanePFCapacityFcn(SegmentType, PFbCoeffs, SegLength,FFS, HeavyVehPercent,DemandVolOpposingDir)
    PercentFollower25Capacity <- TwoLanePF25CapacityFcn(SegmentType, PFcCoeffs, SegLength, FFS, HeavyVehPercent, DemandVolOpposingDir)
    PercentFollowerSlopeCoeff <- TwoLanePFSlopeCoeffFcn(PFdCoeffs, PercentFollower25Capacity, Capacity, PercentFollowerCapacity)
    PercentFollowerPowerCoeff <- TwoLanePFPowerCoeffFcn(PFeCoeffs, PercentFollower25Capacity, Capacity, PercentFollowerCapacity)
    PercentFollowers <- TwoLanePercentFollowersFcn(PercentFollowerSlopeCoeff, AnalysisFlowRate, PercentFollowerPowerCoeff)
    FollowerDensity <- TwoLaneFollowerDensityFcn(PercentFollowers, AnalysisFlowRate, AvgSpeed)
    if (IsPassingLaneUpStream==TRUE){
      if (UpStreamPassLaneLengthMi+DownStreamDistMi<EffectiveLengthMi){
        AdjustedFollowerDensity <- AdjustedFollowerDensityFcn(UpStreamPassLaneLengthMi, UpstreamSegResults[5], UpstreamSegResults[3], AnalysisFlowRate, PercentFollowers, AvgSpeed, DownStreamDistMi)
        FollowerDensity <- AdjustedFollowerDensity
      }
    }
    ReturnAvgSpeed <- AvgSpeed
    ReturnPctFollowers <- PercentFollowers
    ReturnFollowerDensity <- FollowerDensity
    LOSvalue <- TwoLaneLOSfcn(FollowerDensity, PostedSpeedLimit)
    LOSNumber <- approx(TwoLaneLOSDF$FollowerDensityThresholds, TwoLaneLOSDF$NumericLOSthresholds, xout = ReturnFollowerDensity)
  }
  #Passing Lane Functions
  else if (SegmentType == "PL"){
    NumHV <- NumHVFcn(AnalysisFlowRate, HeavyVehPercent)
    PropFlowRateFL <- PropFlowRateFLFcn(AnalysisFlowRate, NumHV)
    FlowRateFL <- FlowRateFLFcn(AnalysisFlowRate, PropFlowRateFL)
    FlowRateSL <- FlowRateSLFcn(AnalysisFlowRate, PropFlowRateFL)
    HeavyVehPercentFL <- HeavyVehPercentFLFcn(HeavyVehPercent)
    NumHVSL <- NumHVSLFcn(NumHV, FlowRateFL, HeavyVehPercentFL)
    HeavyVehPercentSL <- HeavyVehPercentSLFcn(NumHVSL, FlowRateSL)
    AvgSpeedDiffAdj <- AvgSpeedDiffAdjFcn(AnalysisFlowRate, HeavyVehPercent) #SW added
    SpeedbcoeffsSpeed <- SpeedbCoeffFcn(SegmentType, VertAlignmentClass, SpeedcCoeffsSpeed, SpeeddCoeffsSpeed, SegLength, FFS, HeavyVehPercent) #SW added
    SpeedfcoeffsSpeed <- SpeedfCoeffFcn(SegmentType, VertAlignmentClass)
    InitialFLAvgSpeedPowerCoeff <- TwoLaneAvgSpeedPowerCoeffFcn(SpeedfcoeffsSpeed, FFS, SegLength, DemandVolOpposingDir, HeavyVehPercentFL)
    InitialFLAvgSpeedSlopeCoeff <- TwoLaneAvgSpeedSlopeCoeffFcn(SpeedbcoeffsSpeed, FFS, DemandVolOpposingDir, SegLength, HeavyVehPercentFL)
    InitialAvgSpeedFL <- TwoLaneAvgSpeedFcn(FlowRateFL, FFS, InitialFLAvgSpeedSlopeCoeff, InitialFLAvgSpeedPowerCoeff)
    AvgSpeedPLMidFL <- AvgSpeedPLMidFLFcn(InitialAvgSpeedFL, AvgSpeedDiffAdj)
    InitialSLAvgSpeedPowerCoeff <- TwoLaneAvgSpeedPowerCoeffFcn(SpeedfcoeffsSpeed, FFS, SegLength, DemandVolOpposingDir, HeavyVehPercentSL)
    InitialSLAvgSpeedSlopeCoeff <- TwoLaneAvgSpeedSlopeCoeffFcn(SpeedbcoeffsSpeed, FFS, DemandVolOpposingDir, SegLength, HeavyVehPercentSL)
    InitialAvgSpeedSL <- TwoLaneAvgSpeedFcn(FlowRateSL, FFS, InitialSLAvgSpeedSlopeCoeff, InitialSLAvgSpeedPowerCoeff)
    AvgSpeedPLMidSL <- AvgSpeedPLMidSLFcn(InitialAvgSpeedSL, AvgSpeedDiffAdj)
    AvgSpeedPLMid <- AvgSpeedPLMidFcn(FlowRateFL, AvgSpeedPLMidFL, FlowRateSL, AvgSpeedPLMidSL)
    PercentFollowerPLMidFLCapacity <- PFPLMidCapacityFcn(PFbCoeffs, SegLength, FFS, HeavyVehPercentFL)
    PercentFollowerPLMidFL25Capacity <- PFPLMid25CapacityFcn(PFcCoeffs, SegLength, FFS, HeavyVehPercentFL)
    PercentFollowerPLMidFLPowerCoeff <- TwoLanePFPowerCoeffFcn(PFeCoeffs, PercentFollowerPLMidFL25Capacity, Capacity, PercentFollowerPLMidFLCapacity)
    PercentFollowerPLMidFLSlopeCoeff <- TwoLanePFSlopeCoeffFcn(PFdCoeffs, PercentFollowerPLMidFL25Capacity, Capacity, PercentFollowerPLMidFLCapacity)
    PercentFollowerPLMidFL <- TwoLanePercentFollowersFcn(PercentFollowerPLMidFLSlopeCoeff, FlowRateFL, PercentFollowerPLMidFLPowerCoeff)
    PercentFollowerPLMidSLCapacity <- PFPLMidCapacityFcn(PFbCoeffs, SegLength, FFS, HeavyVehPercentSL)
    PercentFollowerPLMidSL25Capacity <- PFPLMid25CapacityFcn(PFcCoeffs, SegLength, FFS, HeavyVehPercentSL)
    PercentFollowerPLMidSLPowerCoeff <- TwoLanePFPowerCoeffFcn(PFeCoeffs, PercentFollowerPLMidSL25Capacity, Capacity, PercentFollowerPLMidSLCapacity)
    PercentFollowerPLMidSLSlopeCoeff <- TwoLanePFSlopeCoeffFcn(PFdCoeffs, PercentFollowerPLMidSL25Capacity, Capacity, PercentFollowerPLMidSLCapacity)
    PercentFollowerPLMidSL <- TwoLanePercentFollowersFcn(PercentFollowerPLMidSLSlopeCoeff, FlowRateSL, PercentFollowerPLMidSLPowerCoeff)
    AvgPercentFollowersPL <- AvgPercentFollowersPLFcn(FlowRateFL, PercentFollowerPLMidFL, FlowRateSL, PercentFollowerPLMidSL)
    FollowerDensityPLMid <- FollowerDensityPLMidFcn(SegmentType,PercentFollowerPLMidFL,FlowRateFL,AvgSpeedPLMidFL, PercentFollowerPLMidSL, FlowRateSL, AvgSpeedPLMidSL)
    ReturnEffectiveLength <- PassingLaneEffectiveLengthFcn(UpstreamSegResults[5], UpstreamSegResults[2], UpstreamSegResults[3], UpstreamSegResults[4], SegLength) 
    ReturnAvgSpeed <- AvgSpeedPLMid
    ReturnPctFollowers <- AvgPercentFollowersPL
    ReturnFollowerDensity <- FollowerDensityPLMid
    LOSvalue <- TwoLaneLOSfcn(FollowerDensityPLMid, PostedSpeedLimit)
    LOSNumber <- approx(TwoLaneLOSDF$FollowerDensityThresholds, TwoLaneLOSDF$NumericLOSthresholds, xout = ReturnFollowerDensity)
    AvgSpeedSlopeCoeff <- 0
    AvgSpeedPowerCoeff <- 0
    }
  
  TwoLaneOutputDisplayFcnCommon(SegmentType, AnalysisFlowRate, BFFS, a, LaneShoulderAdj, AccessPointAdj, MinSegmentLength, MaxSegmentLength, FFS)
  
  if (SegmentType != "PL"){
    TwoLaneOutputDisplayFcnNonPassingLane(AvgSpeedSlopeCoeff, AvgSpeedPowerCoeff, ReturnAvgSpeed, PercentFollowerCapacity, PercentFollower25Capacity, PercentFollowerSlopeCoeff, PercentFollowerPowerCoeff, ReturnPctFollowers, ReturnFollowerDensity, LOSvalue, LOSNumber)
  } else {
    TwoLaneOutputDisplayFcnPassingLane(NumHV, PropFlowRateFL, FlowRateFL, FlowRateSL, HeavyVehPercentFL, NumHVSL, HeavyVehPercentSL, AvgSpeedDiffAdj, InitialAvgSpeedFL, AvgSpeedPLMidFL, InitialAvgSpeedSL, AvgSpeedPLMidSL, AvgSpeedPLMid, PercentFollowerPLMidFL, PercentFollowerPLMidSL, AvgPercentFollowersPL, FollowerDensityPLMid,ReturnFollowerDensity,LOSvalue, LOSNumber, ReturnEffectiveLength)
  }
  #ReturnFollowerDensity <- AdjustedFollowerDensity
  Results <- c(FFS, ReturnAvgSpeed, ReturnPctFollowers, ReturnFollowerDensity, AnalysisFlowRate, LOSNumber[[2]], ReturnEffectiveLength)
  return(Results)
}


#Function to set passing lane capacity (HCM Exhibit 15-5)
CapacityFcn <- function(SegmentType,VertAlignClass,HeavyVehPercent){
  #cat("VertClass =", VertAlignClass, "\n")
  if (SegmentType != "PL"){
    return(1700)
  }
  if (VertAlignClass == 1 || VertAlignClass == 2 || VertAlignClass == 3) {
    if (HeavyVehPercent < 10) {
      PLCapacity <- 1500
    }
    else if (HeavyVehPercent >= 10 && HeavyVehPercent < 15) {
      PLCapacity <- 1400
    }
    else if (HeavyVehPercent >= 15 && HeavyVehPercent < 25) {
      PLCapacity <- 1300
    }
    else if (HeavyVehPercent >= 25) {
      PLCapacity <- 1100
    }
  }
  if (VertAlignClass == 4) {
    if (HeavyVehPercent < 10) {
      PLCapacity <- 1500
    }
    else if (HeavyVehPercent >= 10 && HeavyVehPercent < 20) {
      PLCapacity <- 1300
    }
    else if (HeavyVehPercent >= 20 && HeavyVehPercent < 25) {
      PLCapacity <- 1200
    }
    else if (HeavyVehPercent >= 25) {
      PLCapacity <- 1100
    }
  }
  if (VertAlignClass == 5) {
    if (HeavyVehPercent < 5) {
      PLCapacity <- 1500
    }
    else if (HeavyVehPercent >= 5 && HeavyVehPercent < 10) {
      PLCapacity <- 1400
    }
    else if (HeavyVehPercent >= 10 && HeavyVehPercent < 15) {
      PLCapacity <- 1300
    }
    else if (HeavyVehPercent >= 15 && HeavyVehPercent < 20) {
      PLCapacity <- 1200
    }
    else if (HeavyVehPercent >= 20) {
      PLCapacity <- 1100
    }
  }
  return(PLCapacity)
}


#HCM Eq. 15-1
TwoLaneAnalysisFlowRateFcn <- function(DemandVolume, PHF){
  AnalysisFlowRate <- DemandVolume/PHF
  return(AnalysisFlowRate)
}

#HCM Eq. 15-2
TwoLaneBFFSFcn <- function(PostedSpeedLimit){
  BFFS <- 1.14*PostedSpeedLimit
  return(BFFS)
}

#HCM Eq. 15-4
aFcn <- function(Acoeffs, BFFS, SegLength, v0){
  a = max(0.0333, Acoeffs[1] + Acoeffs[2]*BFFS + Acoeffs[3]*SegLength + (max(0, Acoeffs[4] + Acoeffs[5]*BFFS + Acoeffs[6]*SegLength))*(v0/1000))
  return(a)
}


#HCM Eq. 15-5
TwoLaneShoulderAdjFcn <- function(LaneWidth, ShoulderWidth){
  LaneShoulderAdj <- 0.6 * (12 - LaneWidth) + 0.7 * (6 - ShoulderWidth)
  return(LaneShoulderAdj)
}
  
#HCM Eq. 15-6
TwoLaneAccessDensityFcn <- function(AccessPointDensity){
  AccessPointAdj <- min((AccessPointDensity/4),10)
  return(AccessPointAdj)
}

#HCM Eq. 15-3
TwoLaneFFSFcn <- function(BFFS, a, HeavyVehPercent, LaneShoulderAdj, AccessPointAdj){
  FFS <- BFFS - a*(HeavyVehPercent) - LaneShoulderAdj - AccessPointAdj
  return(FFS)
}

#HCM Eq. 15-9 (Avg Speed b3 equation)
b3Fcn <- function(c0, c1, SegLength, c2, FFS, c3){
  b3 <- (c0 + c1 * sqrt(SegLength) + c2 * FFS + c3 * (FFS*sqrt(SegLength)))
  return(b3)
}

#HCM Eq. 15-10 (Avg Speed b4 equation)
b4Fcn <- function(d0, d1, HeavyVehPercent, d2, FFS, d3){
  b4 <- (d0 + d1 * sqrt(HeavyVehPercent) + d2 * FFS + d3 * (FFS*sqrt(HeavyVehPercent)))
  return(b4)
}


#HCM Eq. 15-8 (Avg Speed m Coefficient)
TwoLaneAvgSpeedSlopeCoeffFcn <- function(Bcoeffs, FFS, v0, SegLength, HeavyVehPercent){
  AvgSpeedSlopeCoeff <- max(Bcoeffs[6], Bcoeffs[1] + Bcoeffs[2] * FFS + Bcoeffs[3] * sqrt(v0/1000) + max(0,Bcoeffs[4]) * sqrt(SegLength) + max(0,Bcoeffs[5]) * sqrt(HeavyVehPercent))
  return(AvgSpeedSlopeCoeff)
}


#HCM Eq. 15-11 (Avg Speed p Coefficient)
TwoLaneAvgSpeedPowerCoeffFcn <- function(Fcoeffs, FFS, SegLength, v0, HeavyVehPercent){
  AvgSpeedPowerCoeff <- max(Fcoeffs[9], Fcoeffs[1] + Fcoeffs[2]*FFS + Fcoeffs[3]*SegLength + Fcoeffs[4]*(v0/1000) + Fcoeffs[5]*sqrt(v0/1000) + Fcoeffs[6]*HeavyVehPercent + Fcoeffs[7]*sqrt(HeavyVehPercent) + Fcoeffs[8]*(SegLength*HeavyVehPercent))
  return(AvgSpeedPowerCoeff)
}

#HCM Eq. 15-7 (Avg Speed)
TwoLaneAvgSpeedFcn <- function(AnalysisFlowRate, FFS, AvgSpeedSlopeCoeff, AvgSpeedPowerCoeff){
  if (AnalysisFlowRate < 100){
    AvgSpeed <- FFS
  }
  else if (AnalysisFlowRate >= 100){
    AvgSpeed <- FFS - AvgSpeedSlopeCoeff*((AnalysisFlowRate/1000)-0.1)^AvgSpeedPowerCoeff
  }
  return(AvgSpeed)
}

#HCM Eq. 15-18 (% Followers at Capacity)

TwoLanePFCapacityFcn <- function(SegmentType, PFbCoeffs, SegLength, FFS, HeavyVehPercent, DemandVolOppDir){
  if (SegmentType == "PZ" | SegmentType == "NPZ"){
    PctFollowCapacity <- PFbCoeffs[1] + (PFbCoeffs[2]*SegLength) + (PFbCoeffs[3]*sqrt(SegLength)) + (PFbCoeffs[4]*FFS) + (PFbCoeffs[5]*sqrt(FFS)) + (PFbCoeffs[6]*HeavyVehPercent) + (PFbCoeffs[7]*(FFS*(DemandVolOppDir/1000))) + (PFbCoeffs[8]*sqrt(DemandVolOppDir/1000))
    #PercentFollowerCapacity <- TwoLanePFNonPassingCapacityFcn(PFbCoeffs, SegLength, FFS, HeavyVehPercent, DemandVolOpposingDir)
  }
  else if (SegmentType == "PL"){
    PctFollowCapacity <- PFbCoeffs[1] + (PFbCoeffs[2]*SegLength) + (PFbCoeffs[3]*sqrt(SegLength)) + (PFbCoeffs[4]*FFS) + (PFbCoeffs[5]*sqrt(FFS)) + (PFbCoeffs[6]*HeavyVehPercent) + (PFbCoeffs[7]*sqrt(HeavyVehPercent)) + (PFbCoeffs[8]*(FFS*HeavyVehPercent))
    #PercentFollowerCapacity <- TwoLanePFPassingLaneCapacityFcn(PFbCoeffs, SegLength, FFS, HeavyVehPercent)
  }
    return(PctFollowCapacity)
}

#HCM Eq. 15-20 (% Followers at 25% Capacity)
#TwoLanePF25CapacityFcn <- function(PFcCoeffs, SegLength, FFS, HeavyVehPercent, DemandVolOppDir){}

TwoLanePF25CapacityFcn <- function(SegmentType, PFcCoeffs, SegLength, FFS, HeavyVehPercent, DemandVolOppDir){
  if (SegmentType == "PZ" | SegmentType == "NPZ"){
    PercentFollower25Capacity <- PFcCoeffs[1] + (PFcCoeffs[2]*SegLength) + (PFcCoeffs[3]*sqrt(SegLength)) + (PFcCoeffs[4]*FFS) + (PFcCoeffs[5]*sqrt(FFS)) + (PFcCoeffs[6]*HeavyVehPercent) + (PFcCoeffs[7]*(FFS*(DemandVolOppDir/1000))) + (PFcCoeffs[8]*sqrt(DemandVolOppDir/1000))
  }
  else if (SegmentType == "PL"){
    PercentFollower25Capacity <- PFcCoeffs[1] + (PFcCoeffs[2]*SegLength) + (PFcCoeffs[3]*sqrt(SegLength)) + (PFcCoeffs[4]*FFS) + (PFcCoeffs[5]*sqrt(FFS)) + (PFcCoeffs[6]*HeavyVehPercent) + (PFcCoeffs[7]*sqrt(HeavyVehPercent)) + (PFcCoeffs[8]*(FFS*HeavyVehPercent))
  }
  return(PercentFollower25Capacity)
}


#HCM Eq. 15-22 (% Follower m Coefficient)
TwoLanePFSlopeCoeffFcn <- function(PFdCoeffs, PercentFollower25Capacity, Capacity, PercentFollowerCapacity){
  PercentFollowerSlopeCoeff <- (PFdCoeffs[1]*((0-log(1-(PercentFollower25Capacity/100)))/(0.25*(Capacity/1000)))) + (PFdCoeffs[2]*((0 - log(1-(PercentFollowerCapacity/100)))/(Capacity/1000)))
  return(PercentFollowerSlopeCoeff)
}


#HCM Eq. 15-23 (% Follower p Coefficient)
TwoLanePFPowerCoeffFcn <- function(PFeCoeffs, PercentFollower25Capacity, Capacity, PercentFollowerCapacity){
  PercentFollowerPowerCoeff <- PFeCoeffs[1] + (PFeCoeffs[2]*((0-log(1-(PercentFollower25Capacity/100)))/(0.25*(Capacity/1000)))) + (PFeCoeffs[3]*((0-log(1-(PercentFollowerCapacity/100)))/(Capacity/1000))) + (PFeCoeffs[4]*sqrt((0-log(1-(PercentFollower25Capacity/100)))/(0.25*(Capacity/1000)))) + (PFeCoeffs[5]*sqrt((0-log(1-(PercentFollowerCapacity/100)))/(Capacity/1000)))
  return(PercentFollowerPowerCoeff)
}


#HCM Eq. 15-17 (% Followers)
TwoLanePercentFollowersFcn <- function(PercentFollowerSlopeCoeff, AnalysisFlowRate, PercentFollowerPowerCoeff){
  PercentFollowers <- 100*(1-exp(PercentFollowerSlopeCoeff*(AnalysisFlowRate/1000)^PercentFollowerPowerCoeff))
  return(PercentFollowers)
}


#HCM Equation 15-35 (Follower Density)
TwoLaneFollowerDensityFcn <- function(PercentFollowers, AnalysisFlowRate, AvgSpeed){
  FollowDensity <- (PercentFollowers/100) * (AnalysisFlowRate/AvgSpeed)
  return(FollowDensity)
}


#Exhibit 12-6 
TwoLaneLOSfcn <- function(FollowerDensity, PostedSpeedLimit){
  if (PostedSpeedLimit >= 50){
    if (FollowerDensity <= 2.0) {
      LOSvalue <- "A"
    }
    else if (2.0 < FollowerDensity && FollowerDensity < 4.0) {
      LOSvalue <- "B"
    }
    else if (4.0 < FollowerDensity && FollowerDensity < 8.0) {
      LOSvalue <- "C"
    }
    else if (8.0 < FollowerDensity && FollowerDensity < 12.0) {
      LOSvalue <- "D"
    }
    else if (FollowerDensity > 12) {
      LOSvalue <- "E"
    }
    else {
      LOSvalue <- "F"
    }
  if (PostedSpeedLimit < 50){
    if (FollowerDensity <= 2.5) {
      LOSvalue <- "A"
    }
    else if (2.5 < FollowerDensity && FollowerDensity < 5.0) {
      LOSvalue <- "B"
    }
    else if (5.0 < FollowerDensity && FollowerDensity < 10.0) {
      LOSvalue <- "C"
    }
    else if (10.0 < FollowerDensity && FollowerDensity < 15.0) {
      LOSvalue <- "D"
    }
    else if (FollowerDensity > 15.0) {
      LOSvalue <- "E"
    }
    else {
      LOSvalue <- "F"
    }
  }
  }
  return(LOSvalue)
}


#For 'a' coefficients, used for determining FFS
Coeffa <- function(VertAlignClass){
  Acoeffs <- vector()
  switch (VertAlignClass,
          "1"={Acoeffs <- c(0.00000,0.00000,0.00000,0.00000,0.00000,0.00000)
          },
          "2"={Acoeffs <- c(-0.45036,0.00814,0.01543,0.01358,0.00000,0.00000)
          },
          "3"={Acoeffs <- c(-0.29591,0.00743,0.00000,0.01246,0.00000,0.00000)
          },
          "4"={Acoeffs <- c(-0.40902,0.00975,0.00767,-0.18363,0.00423,0.00000)
          },
          "5"={Acoeffs <- c(-0.38360,0.01074,0.01945,-0.69848,0.01069,0.12700)
          },
  )
  return (Acoeffs)
}

#Need to first get 'c' and 'd' coefficients, since they may be needed in determination of 'b' coefficients
SpeedcCoeffFcn <- function(SegmentType, VertAlignmentClass){
  Ccoeffs <- vector()
  if (SegmentType == "NPZ" | SegmentType == "PZ"){
    Ccoeffs <- SpeedCoeffcNonPassingLane(VertAlignmentClass)
  }
  else if (SegmentType == "PL"){
    Ccoeffs <- SpeedCoeffcPassingLane(VertAlignmentClass)
  }
  return (Ccoeffs)
}

SpeedCoeffcNonPassingLane <- function(VertAlignmentClass){
  Ccoeffs <- vector()
  switch (VertAlignmentClass,
    "1"={Ccoeffs <- c(0.1029,0.0000,0.0000,0.0000)
    },
    "2"={Ccoeffs <- c(-13.8036,0.0000,0.2446,0.0000)
    },
    "3"={Ccoeffs <- c(-11.9703,0.0000,0.2542, 0.0000)
    },
    "4"={Ccoeffs <- c(-12.5113,0.0000,0.2656,0.0000)
    },
    "5"={Ccoeffs <- c(-14.8961,0.0000, 0.4370,0.0000)
    },
  )
  return (Ccoeffs)
}

SpeedCoeffcPassingLane <- function(VertAlignmentClass){
  Ccoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={Ccoeffs <- c(0.0000,0.2667,0.0000,0.0000)
          },
          "2"={Ccoeffs <- c(0.0000,0.4479,0.0000,0.0000)
          },
          "3"={Ccoeffs <- c(0.0000,0.0000,0.0000,0.0000)
          },
          "4"={Ccoeffs <- c(-27.1244,11.5196,0.4681,-0.1873)
          },
          "5"={Ccoeffs <- C(-45.3391,17.3749,1.0587,-0.3729)
          },
  )
  return (Ccoeffs)
}


SpeeddCoeffFcn <- function(SegmentType, VertAlignmentClass){
  Dcoeffs <- vector()
  if (SegmentType == "NPZ" | SegmentType == "PZ"){
    Dcoeffs <- SpeedCoeffdNonPassingLane(VertAlignmentClass)
  }
  else if (SegmentType == "PL"){
    Dcoeffs <- SpeedCoeffdPassingLane(VertAlignmentClass)
  }
  return (Dcoeffs)
}

SpeedCoeffdNonPassingLane <- function(VertAlignmentClass){
  Dcoeffs <- vector()
  switch (VertAlignmentClass,
    "1"={Dcoeffs <- c(0.0000,0.0000,0.0000,0.0000)
    },
    "2"={Dcoeffs <- c(-1.7765,0.0000,0.0392,0.0000)
    },
    "3"={Dcoeffs <- c(-3.5550,0.0000,0.0826,0.0000)
    },
    "4"={Dcoeffs <- c(-5.7775,0.0000,0.1373,0.0000)
    },
    "5"={Dcoeffs <- C(-18.2910,2.3875,0.4494,-0.0520)
    },
  )
  return (Dcoeffs)
}

SpeedCoeffdPassingLane <- function(VertAlignmentClass){
  Dcoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={Dcoeffs <- c(0.0000,0.1252,0.0000,0.0000)
          },
          "2"={Dcoeffs <- c(0.0000,0.1631,0.0000,0.0000)
          },
          "3"={Dcoeffs <- c(0.0000,-0.2201,0.0000,0.0072)
          },
          "4"={Dcoeffs <- c(0.0000,-0.7506,0.0000,0.0193)
          },
          "5"={Dcoeffs <- C(3.8457,-0.9112,0.0000,0.0170)
          },
  )
  return (Dcoeffs)
}

SpeedbCoeffFcn <- function(SegmentType, VertAlignmentClass, CcoeffsSpeed, DcoeffsSpeed, SegLength, FFS, HeavyVehPercent){
  Bcoeffs <- vector()
  if (SegmentType == "NPZ" | SegmentType == "PZ"){
    Bcoeffs <- SpeedCoeffbNonPassingLane(VertAlignmentClass, CcoeffsSpeed, DcoeffsSpeed, SegLength, FFS, HeavyVehPercent)
  }
  else if (SegmentType == "PL"){
    Bcoeffs <- SpeedCoeffbPassingLane(VertAlignmentClass, CcoeffsSpeed, DcoeffsSpeed, SegLength, FFS, HeavyVehPercent)
  }
  return (Bcoeffs)
}

SpeedCoeffbNonPassingLane <- function(VertAlignmentClass, Ccoeffs, Dcoeffs, SegLength, FFS, HVpct){
  Bcoeffs <- vector()
    switch (VertAlignmentClass,
      "1"={Bcoeffs <- c(0.0558,0.0542,0.3278,0.1029,0.0000,0.0000)
      },
      "2"={
        #this can be simplified to just pass in the vector variable, and then use indexing as shown here in the equation in the b3Fcn function
        b3coeff <- b3Fcn(Ccoeffs[1], Ccoeffs[2], SegLength, Ccoeffs[3], FFS, Ccoeffs[4])
        b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct,Dcoeffs[3], FFS, Dcoeffs[4])
        Bcoeffs <- c(5.7280,-0.0809,0.7404,b3coeff,b4coeff,3.1155)
      },
      "3"={
        b3coeff <- b3Fcn(Ccoeffs[1], Ccoeffs[2], SegLength, Ccoeffs[3], FFS, Ccoeffs[4])
        b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct,Dcoeffs[3], FFS, Dcoeffs[4])                 
        Bcoeffs <- c(9.3079,-0.1706,1.1292,b3coeff,b4coeff,3.1155)
      },
      "4"={
        b3coeff <- b3Fcn(Ccoeffs[1], Ccoeffs[2], SegLength, Ccoeffs[3], FFS, Ccoeffs[4])
        b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct,Dcoeffs[3], FFS, Dcoeffs[4])                 
        Bcoeffs <- c(9.0115,-0.1194,1.8252,b3coeff,b4coeff,3.2685)
      },
      "5"={
        b3coeff <- b3Fcn(Ccoeffs[1], Ccoeffs[2], SegLength, Ccoeffs[3], FFS, Ccoeffs[4])
        b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct,Dcoeffs[3], FFS, Dcoeffs[4])                 
        Bcoeffs <- c(23.9144,-0.6925,1.9473,b3coeff,b4coeff,3.5115)
      }
    )
}

SpeedCoeffbPassingLane <- function(VertAlignmentClass, Ccoeffs, Dcoeffs, SegLength, FFS, HVpct){
  Bcoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={
            b3coeff <- b3Fcn(Ccoeffs[1], Ccoeffs[2], SegLength, Ccoeffs[3], FFS, Ccoeffs[4])
            b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct,Dcoeffs[3], FFS, Dcoeffs[4])
            Bcoeffs <- c(-1.1379,0.0941,0.0000,b3coeff,b4coeff,0.0000)
          },
          "2"={
            b3coeff <- b3Fcn(Ccoeffs[1], Ccoeffs[2], SegLength, Ccoeffs[3], FFS, Ccoeffs[4])
            b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct, Dcoeffs[3], FFS, Dcoeffs[4])
            Bcoeffs <- c(-2.0688,0.1053,0.0000,b3coeff,b4coeff,0.0000)
          },
          "3"={
            b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct,Dcoeffs[3], FFS, Dcoeffs[4])                 
            Bcoeffs <- c(-0.5074,0.0935,0.0000,0.0000,b4coeff,0.0000)
          },
          "4"={
            b3coeff <- b3Fcn(Ccoeffs[1], Ccoeffs[2], SegLength, Ccoeffs[3], FFS, Ccoeffs[4])
            b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct,Dcoeffs[3], FFS, Dcoeffs[4])                 
            Bcoeffs <- c(8.0354,-0.0860,0.0000,b3coeff,b4coeff,4.1900)
          },
          "5"={
            b3coeff <- b3Fcn(Ccoeffs[1], Ccoeffs[2], SegLength, Ccoeffs[3], FFS, Ccoeffs[4])
            b4coeff <- b4Fcn(Dcoeffs[1], Dcoeffs[2], HVpct,Dcoeffs[3], FFS, Dcoeffs[4])                 
            Bcoeffs <- c(7.2991,-0.3535,0.0000,b3coeff,b4coeff,4.8700)
          }
  )
}

SpeedfCoeffFcn <- function(SegmentType, VertAlignmentClass){
  Fcoeffs <- vector()
  if (SegmentType == "NPZ" | SegmentType == "PZ"){
    Fcoeffs <- SpeedfCoeffNonPassingLane(VertAlignmentClass)
  }
  else if (SegmentType == "PL"){
    Fcoeffs <- SpeedfCoeffPassingLane(VertAlignmentClass)
  }
  return (Fcoeffs)
}

SpeedfCoeffNonPassingLane <- function(VertAlignmentClass){
  Fcoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={Fcoeffs <- c(0.67576,0.00000,0.00000,0.12060,-0.35919,0.00000,0.00000,0.00000,0.00000)
          },
          "2"={Fcoeffs <- c(0.34524,0.00591,0.02031,0.14911,-0.43784,-0.00296,0.02956,0.00000,0.41622)
          },
          "3"={Fcoeffs <- c(0.17291,0.00917,0.05698,0.27734,-0.61893,-0.00918,0.09184,0.00000,0.41622)
          },
          "4"={Fcoeffs <- c(0.67689,0.00534,-0.13037,0.25699,-0.68465,-0.00709,0.07087,0.00000,0.33950)
          },
          "5"={Fcoeffs <- C(1.13262,0.00000,-0.26367,0.18811,-0.64304,-0.00867,0.08675,0.00000,0.30590)
          },
  )
  return (Fcoeffs)
}

SpeedfCoeffPassingLane <- function(VertAlignmentClass){
  Fcoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={Fcoeffs <- c(0.91793,-0.00557,0.36862,0.00000,0.00000,0.00611,0.00000,-0.00419,0.00000)
          },
          "2"={Fcoeffs <- c(0.65105,0.00000,0.34931,0.00000,0.00000,0.00722,0.00000,-0.00391,0.00000)
          },
          "3"={Fcoeffs <- c(0.40117,0.00000,0.68633,0.00000,0.00000,0.02350,0.00000,-0.02088,0.00000)
          },
          "4"={Fcoeffs <- c(1.13282,-0.00798,0.35425,0.00000,0.00000,0.01521,0.00000,-0.00987,0.00000)
          },
          "5"={Fcoeffs <- C(1.12077,-0.00550,0.25431,0.00000,0.00000,0.01269,0.00000,-0.01053,0.00000)
          },
  )
  return (Fcoeffs)
}


PFbCoeffFcn <- function(SegmentType, VertAlignmentClass){
  PFbCoeffs <- vector()
  if (SegmentType == "NPZ" | SegmentType == "PZ"){
    PFbCoeffs <- PFCoeffbNonPassingLane(VertAlignmentClass)
  }
  else if (SegmentType == "PL"){
    PFbCoeffs <- PFCoeffbPassingLane(VertAlignmentClass)
  }
  return (PFbCoeffs)
}

PFCoeffbNonPassingLane <- function(VertAlignmentClass){
  PFbCoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={PFbCoeffs <- c(37.68080,3.05089,-7.90866,-0.94321,13.64266,-0.00050,-0.05500,7.1376)
          },
          "2"={PFbCoeffs <- c(58.21104,5.73387,-13.66293,-0.66126,9.08575,-0.00950,-0.03602,7.1462)
          },
          "3"={PFbCoeffs <- c(113.20439,10.01778,-18.90000,0.46542,-6.75338,-0.03000,-0.05800,10.0324)
          },
          "4"={PFbCoeffs <- c(58.29978,-0.53611,7.35076,-0.27046,4.49850,-0.01100,-0.02968,8.8968)
          },
          "5"={PFbCoeffs <- C(3.32968,-0.84377,7.08952,-1.32089,19.98477,-0.01250,-0.02960,9.9945)
          },
  )
  return (PFbCoeffs)
}

PFCoeffbPassingLane <- function(VertAlignmentClass){
  PFbCoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={PFbCoeffs <- c(61.73075,6.73922,-23.68853,-0.84126,11.44533,-1.05124,1.50390,0.00491)
          },
          "2"={PFbCoeffs <- c(12.30096,9.57465,-30.79427,-1.79448,25.76436,-0.66350,1.26039,-0.00323)
          },
          "3"={PFbCoeffs <- c(206.07369,-4.29885,0.00000,1.96483,-30.32556,-0.75812,1.06453,-0.00839)
          },
          "4"={PFbCoeffs <- c(263.13428,5.38749,-19.04859,2.73018,-42.76919,-1.31277,-0.32242,0.01412)
          },
          "5"={PFbCoeffs <- C(126.95629,5.95754,-19.22229,0.43238,-7.35636,-1.03017,-2.66026,0.01389)
          },
  )
  return (PFbCoeffs)
}

PFcCoeffFcn <- function(SegmentType,VertAlignmentClass){
  PFcCoeffs <- vector()
  if (SegmentType == "NPZ" | SegmentType == "PZ"){
    PFcCoeffs <- PFCoeffcNonPassingLane(VertAlignmentClass)
  }
  else if (SegmentType == "PL"){
    PFcCoeffs <- PFCoeffcPassingLane(VertAlignmentClass)
  }
  return (PFcCoeffs)
}

PFCoeffcNonPassingLane <- function(VertAlignmentClass){
  PFcCoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={PFcCoeffs <- c(18.01780,10.00000,-21.60000,-0.97853,12.05214,-0.00750,-0.06700,11.6041)
          },
          "2"={PFcCoeffs <- c(47.83887,12.80000,-28.20000,-0.61758,5.80000,-0.04550,-0.03344,11.3557)
          },
          "3"={PFcCoeffs <- c(125.40000,19.50000,-34.90000,0.90672,-16.10000,-0.11000,-0.06200,14.7114)
          },
          "4"={PFcCoeffs <- c(103.13534,14.68459,-23.72704,0.664436,-11.95763,-0.10000,0.00172,14.7007)
          },
          "5"={PFcCoeffs <- C(89.00000,19.02642,-34.54240,0.29792,-6.62528,-0.16000,0.00480,17.5661)
          },
  )
  return (PFcCoeffs)
}


PFCoeffcPassingLane <- function(VertAlignmentClass){
  PFcCoeffs <- vector()
  switch (VertAlignmentClass,
          "1"={PFcCoeffs <- c(80.37105,14.44997,-46.41831,-0.23367,0.84914,-0.56747,0.89427,0.00119)
          },
          "2"={PFcCoeffs <- c(18.37886,14.71856,-47.78892,-1.43373,18.32040,-0.13226,0.77217,-0.00778)
          },
          "3"={PFcCoeffs <- c(239.98930,15.90683,-46.87525,2.73582,-42.88130,-0.53746,0.76271,-0.00428)
          },
          "4"={PFcCoeffs <- c(223.68435,10.26908,-35.60830,2.31877,-38.30034,-0.60275,-0.67758,0.00117)
          },
          "5"={PFcCoeffs <- C(137.37633,11.00106,-38.89043,0.78501,-14.88672,-0.72576,-2.49546,0.00872)
          },
  )
  return (PFcCoeffs)
}


PFCoeffD <- function(SegmentType){
  PFdCoeffs <- vector()
  switch (SegmentType,
          "PZ"={PFdCoeffs <- c(-0.29764,-0.71917)
          },
          "NPZ"={PFdCoeffs <- c(-0.29764,-0.71917)
          },
          "PL"={PFdCoeffs <- c(-0.15808,-0.83732)
          },
  )
  return (PFdCoeffs)
}


PFCoeffE <- function(SegmentType){
  PFeCoeffs <- vector()
  switch (SegmentType,
          "PZ"={PFeCoeffs <- c(0.81165,0.37920,-0.49524,-2.11289,2.41146)
          },
          "NPZ"={PFeCoeffs <- c(0.81165,0.37920,-0.49524,-2.11289,2.41146)
          },
          "PL"={PFeCoeffs <- c(-1.63246,1.64960,-4.45823,-4.89119,10.33057)
          },
  )
  return (PFeCoeffs)
}


#HCM Exhibit 15-10 (Minimum Segment Length)
MinSegmentLengthFcn <- function(VertAlignClass, SegmentType){
  MinSegmentLength <- vector()
  if (SegmentType == "NPZ" | SegmentType == "PZ"){
    switch (VertAlignClass,
            "1"={MinSegmentLength <- c(0.25)
            },
            "2"={MinSegmentLength <- c(0.25)
            },
            "3"={MinSegmentLength <- c(0.25)
            },
            "4"={MinSegmentLength <- c(0.5)
            },
            "5"={MinSegmentLength <- c(0.5)
            },
    )
  }
  else if (SegmentType == "PL"){
    switch (VertAlignClass,
            "1"={MinSegmentLength <- c(0.5)
            },
            "2"={MinSegmentLength <- c(0.5)
            },
            "3"={MinSegmentLength <- c(0.5)
            },
            "4"={MinSegmentLength <- c(0.5)
            },
            "5"={MinSegmentLength <- c(0.5)
            },
    )
  }
  return (MinSegmentLength)
}


#HCM Exhibit 15-10 (Maximum Segment Length)
MaxSegmentLengthFcn <- function(VertAlignClass, SegmentType){
  MaxSegmentLength <- vector()
  if (SegmentType == "NPZ"){
    switch (VertAlignClass,
            "1"={MaxSegmentLength <- c(3.0)
            },
            "2"={MaxSegmentLength <- c(3.0)
            },
            "3"={MaxSegmentLength <- c(1.1)
            },
            "4"={MaxSegmentLength <- c(3.0)
            },
            "5"={MaxSegmentLength <- c(3.0)
            },
    )
  }
  else if (SegmentType == "PZ"){
    switch (VertAlignClass,
            "1"={MaxSegmentLength <- c(2.0)
            },
            "2"={MaxSegmentLength <- c(2.0)
            },
            "3"={MaxSegmentLength <- c(1.1)
            },
            "4"={MaxSegmentLength <- c(2.0)
            },
            "5"={MaxSegmentLength <- c(2.0)
            },
    )
  }
  else if (SegmentType == "PL"){
    switch (VertAlignClass,
            "1"={MaxSegmentLength <- c(3.0)
            },
            "2"={MaxSegmentLength <- c(3.0)
            },
            "3"={MaxSegmentLength <- c(1.1)
            },
            "4"={MaxSegmentLength <- c(3.0)
            },
            "5"={MaxSegmentLength <- c(3.0)
            },
    )
  }
  return (MaxSegmentLength)
}

#Functions for Passing Lane Segments (Steps 7 and 8)
#HCM Equation 15-24
NumHVFcn <- function(AnalysisFlowRate,HeavyVehPercent){
  NumHv <- AnalysisFlowRate * (HeavyVehPercent/100)
  return(NumHv)
}

#HCM Equation 15-25
PropFlowRateFLFcn <- function(AnalysisFlowRate,NumHv){
  PropFlowRateFL <- 0.92183 - (0.05022*log(AnalysisFlowRate)) - (0.00030*NumHv)
  return(PropFlowRateFL)
}

#HCM Equation 15-26
FlowRateFLFcn <- function(AnalysisFlowRate,PropFlowRateFL){
  FlowRateFL <- AnalysisFlowRate*PropFlowRateFL
  return(FlowRateFL)
}

#HCM Equation 15-27
FlowRateSLFcn <- function(AnalysisFlowRate,PropFlowRateFL){
  FlowRateSL <- AnalysisFlowRate*(1-PropFlowRateFL)
  return(FlowRateSL)
}

#HCM Equation 15-28
HeavyVehPercentFLFcn <- function(HeavyVehPercent){
  HVPropMultiplierFL = 0.4
  HeavyVehPercentFL <- HeavyVehPercent * HVPropMultiplierFL
  return(HeavyVehPercentFL)
}

#HCM Equation 15-29
NumHVSLFcn <- function(NumHV, FlowRateFL, HeavyVehPercentFL){
  NumHVSL <- NumHV - (FlowRateFL*(HeavyVehPercentFL/100))
  return(NumHVSL)
}

#HCM Equation 15-30
HeavyVehPercentSLFcn <- function(NumHVSL, FlowRateSL){
  HeavyVehPercentSL <- (NumHVSL/FlowRateSL)*100
  return(HeavyVehPercentSL)
}

#HCM Equation 15-31
AvgSpeedDiffAdjFcn <- function(AnalysisFlowRate, HeavyVehPercent){
  AvgSpeedDiffAdj <- 2.750 + (0.00056*AnalysisFlowRate) + 3.8521*(HeavyVehPercent/100)
  return(AvgSpeedDiffAdj)
}

#HCM Equation 15-32
AvgSpeedPLMidFLFcn <- function(InitialAvgSpeedFL, AvgSpeedDiffAdj){
  AvgSpeedPLMidFL <- InitialAvgSpeedFL + (AvgSpeedDiffAdj/2)
  return(AvgSpeedPLMidFL)
}

#HCM Equation 15-33
AvgSpeedPLMidSLFcn <- function(InitialAvgSpeedSL, AvgSpeedDiffAdj){
  AvgSpeedPLMidSL <- InitialAvgSpeedSL - (AvgSpeedDiffAdj/2)
  return(AvgSpeedPLMidSL)
}

#Passing Lane PF Capacity in FL at Midpoint of segment
PFPLMidCapacityFcn <- function(PFbCoeffs, SegLength, FFS, HeavyVehPercent){
  PercentFollowerCapacity <- PFbCoeffs[1] + (PFbCoeffs[2]*SegLength) + (PFbCoeffs[3]*sqrt(SegLength)) + (PFbCoeffs[4]*FFS) + (PFbCoeffs[5]*sqrt(FFS)) + (PFbCoeffs[6]*HeavyVehPercent) + (PFbCoeffs[7]*sqrt(HeavyVehPercent)) + (PFbCoeffs[8]*(FFS*HeavyVehPercent))
  return(PercentFollowerCapacity)
}

#Passing Lane PF 25% Capacity in FL at Midpoint of segment
PFPLMid25CapacityFcn <- function(PFcCoeffs, SegLength, FFS, HeavyVehPercent){
  PercentFollower25Capacity <-  PFcCoeffs[1] + (PFcCoeffs[2]*SegLength) + (PFcCoeffs[3]*sqrt(SegLength)) + (PFcCoeffs[4]*FFS) + (PFcCoeffs[5]*sqrt(FFS)) + (PFcCoeffs[6]*HeavyVehPercent) + (PFcCoeffs[7]*sqrt(HeavyVehPercent)) + (PFcCoeffs[8]*(FFS*HeavyVehPercent))
  return(PercentFollower25Capacity)
}

#Avg Speed for Passing Lane Segment (Equation not in HCM)
AvgSpeedPLMidFcn <- function(FlowRateFL, AvgSpeedPLMidFL, FlowRateSL, AvgSpeedPLMidSL){
  AvgSpeedPLMid <- ((FlowRateFL*AvgSpeedPLMidFL) + (FlowRateSL*AvgSpeedPLMidSL))/(FlowRateFL+FlowRateSL)
  return(AvgSpeedPLMid)
}

#Percent Followers Passing Lane (Weighted Avg Equation not included in HCM)
AvgPercentFollowersPLFcn <- function(FlowRateFL, PercentFollowerPLMidFL, FlowRateSL, PercentFollowerPLMidSL){
  AvgPercentFollowersPL <- ((FlowRateFL*PercentFollowerPLMidFL) + (FlowRateSL*PercentFollowerPLMidSL))/(FlowRateFL+FlowRateSL)
return(AvgPercentFollowersPL)
}

#HCM Equation 15-34
FollowerDensityPLMidFcn <- function(SegmentType,PercentFollowerPLMidFL,FlowRateFL,AvgSpeedPLMidFL, PercentFollowerPLMidSL, FlowRateSL, AvgSpeedPLMidSL){
  if (SegmentType == "PL"){
    FollowerDensityPLMid <- (((PercentFollowerPLMidFL/100)*(FlowRateFL/AvgSpeedPLMidFL)) + ((PercentFollowerPLMidSL/100)*(FlowRateSL/AvgSpeedPLMidSL)))/2
    return(FollowerDensityPLMid)
  }
  else {
    FollowerDensityPLMid <- "N/A"
    return(FollowerDensityPLMid)
  }
}

#HCM Equation 15-36 - 15-38

AdjustedFollowerDensityFcn <- function(PassLaneLengthMi, UpstreamFlowRate, UpstreamPctFollowers, FlowRate, PctFollowers, AvgSpeed, DownStreamDistMi){
    #Equation 15-36
    PctImprovePctFollowers <- (27 - 8.75 * log(max(0.1, DownStreamDistMi)) + 0.1 * max(0, UpstreamPctFollowers - 30) + 3.5 * log(max(0.3, PassLaneLengthMi)) - 0.01 * UpstreamFlowRate)
    #Equation 15-37
    PctImproveAvgSpeed <- (3 - 0.8 * DownStreamDistMi + 0.1 * max(0, UpstreamPctFollowers - 30) + 0.75 * PassLaneLengthMi - 0.005 * UpstreamFlowRate)
    PctImproveAvgSpeed <- max(PctImproveAvgSpeed, 0)
    #Equation 15-38
    FollowerDensityAdjusted <- (PctFollowers/100)*(1-PctImprovePctFollowers/100)*(FlowRate/(AvgSpeed*(1+PctImproveAvgSpeed/100)))
    return(FollowerDensityAdjusted)
}

#Output Display Functions
TwoLaneOutputDisplayFcnCommon <- function(SegmentType, AnalysisFlowRate, BFFS, a, LaneShoulderAdj, AccessPointAdj, MinSegmentLength, MaxSegmentLength, FFS){
  cat("The peak 15-min flow rate =", format(round(AnalysisFlowRate, 1), nsmall=1), "veh/h", "\n")
  cat("BFFS =", format(round(BFFS, 1), nsmall=1), "mi/h", "\n")
  cat("a =", format(round(a, 3), nsmall=3), "\n")
  cat("Lane and Shoulder Adjustment =", format(round(LaneShoulderAdj, 1), nsmall=1), "mi/h", "\n")
  cat("Access Point Adjustment =", format(round(AccessPointAdj, 1), nsmall=1), "mi/h", "\n")
  cat("Minimum Segment Length =",MinSegmentLength,"mi,"," ")
  cat("Maximum Segment Length =",MaxSegmentLength,"mi", "\n")
  cat("Free-Flow Speed =", format(round(FFS, 1), nsmall=1), "mi/h", "\n")
}

TwoLaneOutputDisplayFcnNonPassingLane <- function(AvgSpeedSlopeCoeff, AvgSpeedPowerCoeff, AvgSpeed, PercentFollowerCapacity, PercentFollower25Capacity, PercentFollowerSlopeCoeff, PercentFollowerPowerCoeff, PercentFollowers, FollowerDensity, LOSvalue, LOSnumber){
  cat("m =", format(round(AvgSpeedSlopeCoeff, 3), nsmall=3), "\n")
  cat("p =", format(round(AvgSpeedPowerCoeff, 3), nsmall=3), "\n")
  cat("Average Speed =", format(round(AvgSpeed, 1), nsmall=1), "mi/h", "\n")
  cat("Percent Followers at Capacity =", format(round(PercentFollowerCapacity, 2), nsmall=2),"%", "\n")
  cat("Percent Followers at 25% Capacity =", format(round(PercentFollower25Capacity, 2), nsmall=2),"%", "\n")
  cat("PF_m =", format(round(PercentFollowerSlopeCoeff, 3), nsmall=3),"\n")
  cat("PF_p =", format(round(PercentFollowerPowerCoeff, 3), nsmall=3),"\n")
  cat("Percent Followers =", format(round(PercentFollowers, 1), nsmall=),"%","\n")
  cat("Follower Density =", format(round(FollowerDensity, 3), nsmall=3),"followers/mi/ln", "\n")
  cat("LOS is", LOSvalue, "\n")
  cat("LOS numeric value =", LOSnumber[[2]])
}

TwoLaneOutputDisplayFcnPassingLane <- function(NumHV, PropFlowRateFL, FlowRateFL, FlowRateSL, HeavyVehPercentFL, NumHVSL, HeavyVehPercentSL, AvgSpeedDiffAdj, InitialAvgSpeedFL, AvgSpeedPLMidFL, InitialAvgSpeedSL, AvgSpeedPLMidSL ,AvgSpeedPLMid, PercentFollowerPLMidFL, PercentFollowerPLMidSL, AvgPercentFollowersPL, FollowerDensityPLMid,FollowerDensity, LOSvalue, LOSnumber, EffectiveLengthPL){
    cat("NumHV =", format(round(NumHV,0),nsmall=0),"veh", "\n")
    cat("PropFlowRate_FL =", format(round(PropFlowRateFL,3),nsmall=3), "\n")
    cat("FlowRate_FL =", format(round(FlowRateFL,0),nsmall=0), "veh/h", "\n")
    cat("FlowRate_SL =", format(round(FlowRateSL,0),nsmall=0), "veh/h", "\n")
    cat("HV%_FL =", format(round(HeavyVehPercentFL, 1), nsmall=1),"%", "\n")
    cat("NumHV_SL =", format(round(NumHVSL,0),nsmall=0), "veh", "\n")
    cat("HV%_SL =", format(round(HeavyVehPercentSL, 1), nsmall=1),"%", "\n")
    cat("AvgSpeedDiffAdj =", format(round(AvgSpeedDiffAdj, 2), nsmall=2),"mi/h", "\n")
    cat("S_init_FL =", format(round(InitialAvgSpeedFL, 2), nsmall=2),"mi/h", "\n")
    cat("S_PLmid_FL =", format(round(AvgSpeedPLMidFL, 2), nsmall=2),"mi/h", "\n")
    cat("S_init_SL =", format(round(InitialAvgSpeedSL, 2), nsmall=2),"mi/h", "\n")
    cat("S_PLmid_SL =", format(round(AvgSpeedPLMidSL, 2), nsmall=2),"mi/h", "\n")
    cat("S_PLMid =", format(round(AvgSpeedPLMid, 2), nsmall=2),"mi/h", "\n")
    cat("PF_PLmid_FL =", format(round(PercentFollowerPLMidFL, 2), nsmall=2),"%", "\n")
    cat("PF_PLmid_SL =", format(round(PercentFollowerPLMidSL, 2), nsmall=2),"%", "\n")
    cat("PF_PLMid =", format(round(AvgPercentFollowersPL, 2), nsmall=2),"%", "\n")
    cat("FD_PLmid =", format(round(FollowerDensityPLMid, 3), nsmall=3),"followers/mi/ln", "\n")
    cat("Follower Density =", format(round(FollowerDensity, 3), nsmall=3),"followers/mi/ln", "\n")
    cat("LOS is", LOSvalue, "\n")
    cat("LOS numeric value =", LOSnumber[[2]], "\n")
    cat("Effective Length =", format(round(EffectiveLengthPL, 1), nsmall=1), "mi", "\n")
    
}


PassingLaneEffectiveLengthFcn <- function(UpstreamFlowRate, UpstreamAvgSpeed, UpstreamPctFollowers, UpstreamFollowerDensity, PassLaneLengthMiles){
  
  PassLaneEffectiveLengthMiles <- 0
  DistanceDownstreamMilesMax <- 40
  for (DistanceDownstreamMiles in seq(from=0.1, to=DistanceDownstreamMilesMax, by=0.1)){
    PassLaneEffectiveLengthMiles <- DistanceDownstreamMiles
    PctImprovePctFollowers <- (27 - 8.75 * log(max(0.1, DistanceDownstreamMiles)) + 0.1 * max(0, UpstreamPctFollowers - 30) + 3.5 * log(max(0.3, PassLaneLengthMiles)) - 0.01 * UpstreamFlowRate)
    
    if (PctImprovePctFollowers <= 0)
    {
      DistanceDownstreamMiles <- DistanceDownstreamMiles - 0.1
      PassLaneEffectiveLengthMiles <- DistanceDownstreamMiles
      break
    }
    
    PctImproveAvgSpeed <- (3 - 0.8 * DistanceDownstreamMiles + 0.1 * max(0, UpstreamPctFollowers - 30) + 0.75 * PassLaneLengthMiles - 0.005 * UpstreamFlowRate)
    PctImproveAvgSpeed <- max(PctImproveAvgSpeed, 0)
    
    AdjustedFollowerDensity <- UpstreamPctFollowers / 100 * (1 - PctImprovePctFollowers / 100) * (UpstreamFlowRate / (UpstreamAvgSpeed * (1 + PctImproveAvgSpeed / 100)))
    
    if (AdjustedFollowerDensity >= UpstreamFollowerDensity * 0.95)  #this follower density needs to be for segment prior to passing lane segment
    {
      DistanceDownstreamMiles <- DistanceDownstreamMiles - 0.1
      PassLaneEffectiveLengthMiles <- DistanceDownstreamMiles
      break
    }
  }
  return(PassLaneEffectiveLengthMiles)
}