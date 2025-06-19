# HCM Signalized Intersection Calculations
# Version 1.0, 10/17/23
# By Scott Washburn, Ryan Kenis, Emily Dang; University of Florida
# -----------------------------------------------------------------
# Version 1.1, 6/18/25
# By Scott Washburn
# Prefix all function names with "Sig_" to be able to uniquely identify functions within the main AWSC methodology calculations file once all methodology calculation files are loaded into memory
# Several 'editorial' revisions that do not affect results


SignalMainFcn <- function(DemandLeftVehHr, DemandThruVehHr, DemandRightVehHr, PeakHourFact, PctTrucksThru, BaseSatFlowRateVehHrLane, NumThruLanes, EffGreenTimeSec, CycleLengthSec, ArrivalType){
  SignalLOSDF <- data.frame(LaneGroupDelayThreshold <- c(0, 10.0, 20.0, 35.0, 55.0, 80.0), NumericLOSthresholds <- c(0, 1, 2, 3, 4, 5))
  TotalApproachDemandVehHr <- DemandThruVehHr+DemandLeftVehHr+DemandRightVehHr
  AnalysisFlowRateVehHr <- Sig_SignalAnalysisFlowRateFcn(DemandThruVehHr+DemandRightVehHr, PeakHourFact)
  AdjSatFlowRateVehHrLane <- Sig_AdjSatFlowFcn(BaseSatFlowRateVehHrLane, PctTrucksThru, DemandRightVehHr, TotalApproachDemandVehHr)
  gCratio <- EffGreenTimeSec/CycleLengthSec
  PlatoonRatio <- Sig_PlatoonRatioFcn(ArrivalType)
  PropArrGreen <- PlatoonRatio*gCratio
  CapacityVehHr <- Sig_SignalCapacityFcn(AdjSatFlowRateVehHrLane, NumThruLanes, EffGreenTimeSec, CycleLengthSec)
  vcRatio <- Sig_vcRatioFcn(AnalysisFlowRateVehHr, CapacityVehHr)
  PF <- Sig_ProgAdjFactFcn(PropArrGreen, gCratio, vcRatio, CycleLengthSec, EffGreenTimeSec)
  LaneGroupDelay <- Sig_LaneGroupDelayFcn(CycleLengthSec, EffGreenTimeSec, CapacityVehHr, vcRatio, PF)
  LaneGroupLOS <- Sig_LOSfcn(LaneGroupDelay)
  LOSNumber <- approx(SignalLOSDF$LaneGroupDelayThreshold, SignalLOSDF$NumericLOSthresholds, xout = LaneGroupDelay)
  Sig_OutputDisplayFcn(AnalysisFlowRateVehHr, AdjSatFlowRateVehHrLane, CapacityVehHr, vcRatio, LaneGroupDelay, LaneGroupLOS, LOSNumber)
  Results <- c(LaneGroupDelay, LOSNumber[[2]])
    return(Results)
}

# Calculate Analysis Flow Rate
Sig_SignalAnalysisFlowRateFcn <- function(Demand, PHF){
  AnalysisFlowRate <- Demand/PHF
  return(AnalysisFlowRate)
}

# Calculate Adjusted Saturation Flow Rate
Sig_AdjSatFlowFcn <- function(BaseSatFlow, PctHV, RightTurnVol, TotalApproachVol){
  EsubT <- 2.3  #per Washburn/Cruz-Casas FDOT research
  fHV = 1 / (1 + PctHV * 0.01 * (EsubT - 1))
  PctRT = RightTurnVol / TotalApproachVol
  fRT = 1 - 0.15 * PctRT
  AdjSatFlow <- BaseSatFlow * fHV * fRT

  return(AdjSatFlow)
}

# Calculate Flow Ratio
Sig_vsRatioFcn <- function(AnalysisFlowRate, SaturationFlowRate){
  vsRatio <- AnalysisFlowRate / SaturationFlowRate
  return(vsRatio)
}

Sig_SignalCapacityFcn <- function(SatFlowRate, NumLanes, EffGreenTime, CycleLength){
  CapValue <- SatFlowRate * NumLanes * EffGreenTime / CycleLength
  return(CapValue)
}

Sig_vcRatioFcn <- function(AnalysisFlowRate, Capacity){
  vcValue <- AnalysisFlowRate / Capacity
  return(vcValue)
}

# Exhibit 19-13
Sig_PlatoonRatioFcn <- function(arrivalType){
  PlatoonRatioArray <- c(0.33, 0.67, 1.00, 1.33, 1.67, 2.00)
  PlatoonRatio <- PlatoonRatioArray[arrivalType]
  return(PlatoonRatio)
}

# Calculate progression adjustment factor (PF)
Sig_ProgAdjFactFcn <- function(propArrGreen, gCratio, vcRatio, cycleLenSec, effGreenSec){
  #Eq. 19-21
  vcRatioConstrained <- min(vcRatio, 1)
  y <- vcRatioConstrained*gCratio

  #Eq. 19-20
  PFterm1 <- (1-propArrGreen)/(1-gCratio)
  PFterm2 <- (1-y)/(1-vcRatioConstrained*propArrGreen)
  PFterm3 <- (1+y*(1-propArrGreen*cycleLenSec/effGreenSec)/(1-gCratio))
  PF <- PFterm1*PFterm2*PFterm3
  return(PF)
}

# Calculate lane group delay
Sig_LaneGroupDelayFcn <- function(CycleLength, EffGreenTime, Capacity, vcRatio, PF){
  k <- 0.5 #control type adjustment factor, 0.5 corresponds to pretimed control
  I <- 1 #metering/filtering factor, 1.0 corresponds to isolated intersection
  T <- 0.25 #15 minute analysis period, units of hours
  UniformDelay <- round((0.5*CycleLength*((1-EffGreenTime/CycleLength)^2)) / (1 - (vcRatio * EffGreenTime / CycleLength)), 2)
  RandomDelay <- round(900 * T * ((vcRatio - 1) + sqrt(((vcRatio - 1)^2) + 8 * k * I * vcRatio / (Capacity * T))), 2)
  TotalDelay <- UniformDelay*PF + RandomDelay
  return(TotalDelay)
}


# LOS Function, Exhibit 19-8
Sig_LOSfcn <- function(delay){
  if (delay <= 10) {
    LOSvalue <- "A"
  }
  else if (delay <= 20) {
    LOSvalue <- "B"
  }
  else if (delay <= 35) {
    LOSvalue <- "C"
  }
  else if (delay <= 55) {
    LOSvalue <- "D"
  }
  else if (delay <= 80) {
    LOSvalue <- "E"
  }
  else {
    LOSvalue <- "F"
  }

  return(LOSvalue)
}


Sig_OutputDisplayFcn <- function(AnalysisFlowRateVehHr, AdjSatFlowRateVehHrLane, CapacityVehHr, vcRatio, LaneGroupDelay, LaneGroupLOS, LOSNumber){
  cat("Analysis Flow Rate =", format(round(AnalysisFlowRateVehHr, digits = 1), nsmall=1), "veh/h", "\n")
  cat("Adjusted Saturation Flow Rate =", format(round(AdjSatFlowRateVehHrLane, digits = 1), nsmall=1), "veh/h", "\n")
  cat("Capacity =", format(round(CapacityVehHr, digits = 1), nsmall=1), "veh/h", "\n")
  cat("v/c Ratio =", format(round(vcRatio, digits = 3), nsmall=3), "\n")
  cat("Thru Delay =", format(round(LaneGroupDelay, digits = 1), nsmall=1), "s", "\n")
  cat("LOS is", LaneGroupLOS, "\n")
  cat("LOS numeric value =", LOSNumber[[2]])
}
