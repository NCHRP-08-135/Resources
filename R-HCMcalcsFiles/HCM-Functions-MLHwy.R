# HCM Multilane Highway Segment Calculations
# Version 1.0, 10/17/23
# By Scott Washburn, Ryan Kenis, Emily Dang; University of Florida


MLMainFcn <- function(PostedSpeedLimit, LaneWidth, RightSideLatClear, LeftSideLatClear, NumLanes, MedianType, AccessDensity, Terrain, PctHeavyVeh, Volume, PeakHourFactor){
  MultilaneLOSDF <- data.frame(DensityThresholds <- c(0, 11, 18, 26, 35, 45), NumericLOSthresholds <- c(0, 1, 2, 3, 4, 5))
  BFFS <- BFFSFcn(PostedSpeedLimit)
  LaneWidthAdj <- LaneWidthAdjFcn(LaneWidth)
  TotalLateralClearance <- RightSideLatClear + LeftSideLatClear
  TotalLatClearAdj <- LatClearAdjFcn(TotalLateralClearance, NumLanes)
  MedianTypeAdj <- MedianTypeAdjFcn(MedianType)
  AccessDensityAdj <- AccessPointAdjFcn(AccessDensity)
  FreeFlowSpeed <- FreeFlowSpeedFcn(BFFS, LaneWidthAdj, TotalLatClearAdj, MedianTypeAdj, AccessDensityAdj)
  Capacity <- CapacityFcn(FreeFlowSpeed)
  TruckPCE <- GeneralTerrainPCEfcn(Terrain)
  HeavyVehFactor <- MLHeavyVehFactorFcn(PctHeavyVeh, TruckPCE)
  AnalysisFlowRate <- AnalysisFlowRateFcn(Volume, PeakHourFactor, NumLanes, HeavyVehFactor)
  Breakpoint <- BreakpointFcn(FreeFlowSpeed)
  AvgSpeed <- AverageSpeedFcn(AnalysisFlowRate, FreeFlowSpeed, Breakpoint, Capacity)
  Density <- DensityFcn(AnalysisFlowRate,AvgSpeed)
  LOSvalue <- LOSfcn(Density)
  LOSNumber <- approx(MultilaneLOSDF$DensityThresholds, MultilaneLOSDF$NumericLOSthresholds, xout = Density)
  MHOutputDisplayFcn(BFFS, LaneWidthAdj, TotalLateralClearance, TotalLatClearAdj, MedianTypeAdj, AccessDensityAdj, FreeFlowSpeed, Capacity, TruckPCE, PctHeavyVeh, HeavyVehFactor, AnalysisFlowRate, Breakpoint, AvgSpeed, Density, LOSvalue, LOSNumber)
  Results <- c(FreeFlowSpeed, AvgSpeed,LOSNumber[[2]])
  return(Results)
  }


LinearInterpolationFcn <- function(actualValue, lowerBound, upperBound, lowerValue, upperValue){
  InterpolatedValue <- lowerValue + (upperBound-actualValue)*(upperValue-lowerValue)/(upperBound-lowerBound)
}

LinearInterpolationFcn2 <- function(actualValue, lowerBound, upperBound, lowerValue, upperValue){
  InterpolatedValue <- upperValue - (upperBound-actualValue)*(upperValue-lowerValue)/(upperBound-lowerBound)
}

# BFFS Estimation
BFFSFcn <- function(PostedSpeedLimit){
  if (PostedSpeedLimit >= 50) {
    BFFS <- PostedSpeedLimit + 5
  }
  else if (PostedSpeedLimit < 50) {
    BFFS <- PostedSpeedLimit + 7
  }
  return(BFFS)
}


# Exhibit 12-20
LaneWidthAdjFcn <- function(laneWidth){
  if (laneWidth >= 12) {
    AdjFact <- 0
  }
  else if (laneWidth >= 11) {
    AdjFact <- 1.9
  }
  else if (laneWidth >= 11) {
    AdjFact <- 6.6
  }
  else {
    AdjFact <- -99  #Cannot use a lane width less than 10 ft
  }
  
  return(AdjFact)
}


# Exhibit 12-22
LatClearAdjFcn <- function(totLateralClear, numDirLanes){
  if (numDirLanes == 2){
    if (totLateralClear >= 12){
      AdjFact <- 0
    }
    else if (totLateralClear >= 10){
      AdjFact <- LinearInterpolationFcn(totLateralClear, 10, 12, 0, 0.4)
    }
    else if (totLateralClear >= 8){
      AdjFact <- LinearInterpolationFcn(totLateralClear, 8, 10, 0.4, 0.9)
    }
    else if (totLateralClear >= 6){
      AdjFact <- LinearInterpolationFcn(totLateralClear, 6, 8, 0.9, 1.3)
    }
    else if (totLateralClear >= 4){
      AdjFact <- LinearInterpolationFcn(totLateralClear, 4, 6, 1.3, 1.8)
    }
    else if (totLateralClear >= 2){
      AdjFact <- LinearInterpolationFcn(totLateralClear, 2, 4, 1.8, 3.6)
    }
    else if (totLateralClear >= 0){
      AdjFact <- LinearInterpolationFcn(totLateralClear, 0, 2, 3.6, 5.4)
    }
  } else if (numDirLanes == 4){
    
  }
}


# Exhibit 12-23
MedianTypeAdjFcn <- function(medianType){
  if (medianType == "Undivided") {
    AdjFact <- 1.6
  }
  else {
    AdjFact <- 0 # TWLTL and Divided
  }
  
  return(AdjFact)
}

# Exhibit 12-24
AccessPointAdjFcn <- function(accessPointDensity){
    if (accessPointDensity >= 40){
      AdjFact <- 10
    }
    else if (accessPointDensity >= 30){
      AdjFact <- LinearInterpolationFcn2(accessPointDensity, 30, 40, 7.5, 10)
    }
    else if (accessPointDensity >= 20){
      AdjFact <- LinearInterpolationFcn2(accessPointDensity, 20, 30, 5.0, 7.5)
    }
    else if (accessPointDensity >= 10){
      AdjFact <- LinearInterpolationFcn2(accessPointDensity, 10, 20, 2.5, 5.0)
    }
    else if (accessPointDensity >= 0){
      AdjFact <- LinearInterpolationFcn2(accessPointDensity, 0, 10, 0, 2.5)
    }
  } 

# Eq. Eq. 12-3 (MH Segment)
FreeFlowSpeedFcn <- function(BFFS, LaneWidthAdj, TotalLatClearAdj, MedianTypeAdj, AccessDensityAdj){
  FFS <- BFFS - LaneWidthAdj - TotalLatClearAdj - MedianTypeAdj - AccessDensityAdj
  return(FFS)
}


# Exhibit 12-6
CapacityFcn <- function(FFS){
  CapacityValue <- min(2200+10*(FFS - 50), 2400)
  return(CapacityValue)
}


# Exhibit 12-6
BreakpointFcn <- function(FFS){
  Breakpoint <- 1000 + 40*(75 - FFS)
  return(Breakpoint)
}


# Eq. 12-1
AverageSpeedFcn <- function(flowRate, ffs, breakPoint, capacity){
  DensityAtCapacity <- 45
  
  if(flowRate <= breakPoint){
    AvgSpeed <- ffs
  } else{
    AvgSpeed <- ffs-(((ffs-capacity/DensityAtCapacity)*(flowRate-breakPoint)^2)/(capacity-breakPoint)^2)
  }

}

# HCM Exhibit 12-25
GeneralTerrainPCEfcn <- function(terrain){
  if (terrain == "Level"){
    TruckPCE <- 2.0
  } else{
    TruckPCE <- 3.0
  }
}


#HCM Eq. 12-10
MLHeavyVehFactorFcn <- function(PctHV, truckPCE){
  #PropTrucks <- PctHV/100
  HeavyVehFactor <- 1/(1+PctHV/100 * (truckPCE-1))
  return(HeavyVehFactor)
}


# Eq. 12-9
AnalysisFlowRateFcn <- function(volume, phf, numLanes, heavyVehFact){
  AnalysisFlowRate <- volume/(phf*numLanes*heavyVehFact)
  return(AnalysisFlowRate)
}


# Eq. 12-11
DensityFcn <- function(flowRate, avgSpeed){
  Density = flowRate/avgSpeed
  return(Density)
}


# Exhibit 12-15
LOSfcn <- function(density){
  if (density <= 11) {
    LOSvalue <- "A"
  }
  else if (density <= 18) {
    LOSvalue <- "B"
  }
  else if (density <= 26) {
    LOSvalue <- "C"
  }
  else if (density <= 35) {
    LOSvalue <- "D"
  }
  else if (density <= 45) {
    LOSvalue <- "E"
  }
  else {
    LOSvalue <- "F"
  }
  
  return(LOSvalue)
}


MHOutputDisplayFcn <- function(BFFS, LaneWidthAdj, TotalLateralClearance, TotalLatClearAdj, MedianTypeAdj, AccessDensityAdj, FreeFlowSpeed, Capacity, TruckPCE, PctHeavyVeh, HeavyVehFactor, AnalysisFlowRate, Breakpoint, AvgSpeed, Density, LOSvalue, LOSNumber){
  cat("BFFS =", format(round(BFFS, 1), nsmall=1), "\n")
  cat("Lane Width Adjustment Factor Value =", format(round(LaneWidthAdj, 1), nsmall=1), "\n")
  cat("Total Lateral Clearance =", format(round(TotalLateralClearance, 1), nsmall=1), "ft", "\n")
  cat("Total Lateral Clearance Adjustment Factor Value =", format(round(TotalLatClearAdj, 1), nsmall=1), "\n")
  cat("Median Type Adjustment Factor Value =", format(round(MedianTypeAdj, 1), nsmall=1), "\n")
  cat("Access Point Density Adjustment Factor Value =", format(round(AccessDensityAdj, 1), nsmall=1), "\n")
  cat("FFS =", format(round(FreeFlowSpeed, 1), nsmall=1), "mi/h \n")
  cat("Capacity =", format(round(Capacity, 2), nsmall=2), "pc/h/ln \n")
  cat("E_T =", format(round(TruckPCE, 1), nsmall=1), "\n")
  cat("P_T =", format(round(PctHeavyVeh, 2), nsmall=2), "\n") #show proportion rather than percent
  cat("Heavy vehicle factor =", format(round(HeavyVehFactor, 3), nsmall=3), "\n")
  cat("Analysis flow rate =", format(round(AnalysisFlowRate, 1), nsmall=1), "pc/h/ln", "\n")
  cat("Breakpoint =", format(round(Breakpoint, 1), nsmall=1), "pc/h/ln", "\n")
  cat("Average Speed =", format(round(AvgSpeed, 1), nsmall=1), "mi/h", "\n")
  cat("Density =", format(round(Density, 1), nsmall=1), "pc/mi/ln", "\n")
  cat("LOS is", LOSvalue, "\n")
  cat("LOS numeric value =", LOSNumber[[2]])
}