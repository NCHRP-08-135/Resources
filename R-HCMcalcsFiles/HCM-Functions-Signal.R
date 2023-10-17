# HCM Signalized Intersection Calculations
# Version 1.0, 10/17/23
# By Scott Washburn, Ryan Kenis, Emily Dang; University of Florida


SignalMainFcn <- function(DemandLeftVehHr, DemandThruVehHr, DemandRightVehHr, PeakHourFact, PctTrucksThru, BaseSatFlowRateVehHrLane, NumThruLanes, EffGreenTimeSec, CycleLengthSec, ArrivalType){
  SignalLOSDF <- data.frame(LaneGroupDelayThreshold <- c(0, 10.0, 20.0, 35.0, 55.0, 80.0), NumericLOSthresholds <- c(0, 1, 2, 3, 4, 5))
  TotalApproachDemandVehHr <- DemandThruVehHr+DemandLeftVehHr+DemandRightVehHr
  AnalysisFlowRateVehHr <- SignalAnalysisFlowRateFcn(DemandThruVehHr+DemandRightVehHr, PeakHourFact)
  AdjSatFlowRateVehHrLane <- AdjSatFlowFcn(BaseSatFlowRateVehHrLane, PctTrucksThru, DemandRightVehHr, TotalApproachDemandVehHr)
  gCratio <- EffGreenTimeSec/CycleLengthSec
  PlatoonRatio <- PlatoonRatioFcn(ArrivalType)
  PropArrGreen <- PlatoonRatio*gCratio
  CapacityVehHr <- SignalCapacityFcn(AdjSatFlowRateVehHrLane, NumThruLanes, EffGreenTimeSec, CycleLengthSec)
  vcRatio <- vcRatioFcn(AnalysisFlowRateVehHr, CapacityVehHr)
  PF <- ProgAdjFactFcn(PropArrGreen, gCratio, vcRatio, CycleLengthSec, EffGreenTimeSec)
  LaneGroupDelay <- LaneGroupDelayFcn(CycleLengthSec, EffGreenTimeSec, CapacityVehHr, vcRatio, PF)
  LaneGroupLOS <- LOSfcn(LaneGroupDelay)
  LOSNumber <- approx(SignalLOSDF$LaneGroupDelayThreshold, SignalLOSDF$NumericLOSthresholds, xout = LaneGroupDelay)
  SignalOutputDisplayFcn(AnalysisFlowRateVehHr, AdjSatFlowRateVehHrLane, CapacityVehHr, vcRatio, LaneGroupDelay, LaneGroupLOS, LOSNumber)
  Results <- c(LaneGroupDelay, LOSNumber[[2]])
    return(Results)
}

# Calculate Analysis Flow Rate
SignalAnalysisFlowRateFcn <- function(Demand, PHF){
  AnalysisFlowRate <- Demand/PHF
  return(AnalysisFlowRate)
}


# Calculate Adjusted Saturation Flow Rate
AdjSatFlowFcn <- function(BaseSatFlow, PctHV, RightTurnVol, TotalApproachVol){
  EsubT <- 2.3  #per Washburn/Cruz-Casas FDOT research
  fHV = 1 / (1 + PctHV * 0.01 * (EsubT - 1))
  PctRT = RightTurnVol / TotalApproachVol
  fRT = 1 - 0.15 * PctRT
  AdjSatFlow <- BaseSatFlow * fHV * fRT
  
  return(AdjSatFlow)
}


# Calculate Flow Ratio
vsRatioFcn <- function(AnalysisFlowRate, SaturationFlowRate){
  vsRatio <- AnalysisFlowRate / SaturationFlowRate
  return(vsRatio)
}


SignalCapacityFcn <- function(SatFlowRate, NumLanes, EffGreenTime, CycleLength){
  CapValue <- SatFlowRate * NumLanes * EffGreenTime / CycleLength
  return(CapValue)
}

vcRatioFcn <- function(AnalysisFlowRate, Capacity){
  vcValue <- AnalysisFlowRate / Capacity
  return(vcValue)
}

# Exhibit 19-13
PlatoonRatioFcn <- function(arrivalType){
  PlatoonRatioArray <- c(0.33, 0.67, 1.00, 1.33, 1.67, 2.00)
  PlatoonRatio <- PlatoonRatioArray[arrivalType]
  return(PlatoonRatio)
}

# Calculate progression adjustment factor (PF)
ProgAdjFactFcn <- function(propArrGreen, gCratio, vcRatio, cycleLenSec, effGreenSec){
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

LaneGroupDelayFcn <- function(CycleLength, EffGreenTime, Capacity, vcRatio, PF){
  k <- 0.5 #control type adjustment factor, 0.5 corresponds to pretimed control
  I <- 1 #metering/filtering factor, 1.0 corresponds to isolated intersection
  T <- 0.25 #15 minute analysis period, units of hours
  UniformDelay <- round((0.5*CycleLength*((1-EffGreenTime/CycleLength)^2)) / (1 - (vcRatio * EffGreenTime / CycleLength)), 2)
  RandomDelay <- round(900 * T * ((vcRatio - 1) + sqrt(((vcRatio - 1)^2) + 8 * k * I * vcRatio / (Capacity * T))), 2)
  TotalDelay <- UniformDelay*PF + RandomDelay
  return(TotalDelay)
}


# LOS Function, Exhibit 19-8
LOSfcn <- function(delay){
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


SignalOutputDisplayFcn <- function(AnalysisFlowRateVehHr, AdjSatFlowRateVehHrLane, CapacityVehHr, vcRatio, LaneGroupDelay, LaneGroupLOS, LOSNumber){
  cat("Analysis Flow Rate =", format(round(AnalysisFlowRateVehHr, digits = 1), nsmall=1), "veh/h", "\n")
  cat("Adjusted Saturation Flow Rate =", format(round(AdjSatFlowRateVehHrLane, digits = 1), nsmall=1), "veh/h", "\n")
  cat("Capacity =", format(round(CapacityVehHr, digits = 1), nsmall=1), "veh/h", "\n")
  cat("v/c Ratio =", format(round(vcRatio, digits = 3), nsmall=3), "\n")
  cat("Thru Delay =", format(round(LaneGroupDelay, digits = 1), nsmall=1), "s", "\n")
  cat("LOS is", LaneGroupLOS, "\n")
  cat("LOS numeric value =", LOSNumber[[2]])
}