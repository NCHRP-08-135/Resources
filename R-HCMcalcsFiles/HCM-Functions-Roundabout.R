# HCM Roundabout Intersection Calculations
# Version 1.0, 10/17/23
# By Scott Washburn, Ryan Kenis, Emily Dang; University of Florida


RoundaboutMainFcn <- function(VehicleType, ProportionOfHeavyVeh, PHF, EBThrough, EBLeft, EBRight, NBThrough, NBLeft, NBRight, WBThrough, WBLeft, WBRight, SBThrough, SBLeft, SBRight){
  StopYieldLOSDF <- data.frame(ControlDelayThreshold <- c(0, 10, 15, 25, 35, 50), NumericLOSthresholds <- c(0, 1, 2, 3, 4, 5))
  PassengerCarEquivalent <- VehicleTypePCEfcn(VehicleType)
  HeavyVehAdjustmentFactor <- HeavyVehFactorFcn(ProportionOfHeavyVeh, PassengerCarEquivalent)
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
  AdjustedEBThrough <- DemandFlowRatePCEFcn(DemandFlowRateEBThrough, HeavyVehAdjustmentFactor)
  AdjustedEBLeft <- DemandFlowRatePCEFcn(DemandFlowRateEBLeft, HeavyVehAdjustmentFactor)
  AdjustedEBRight <- DemandFlowRatePCEFcn(DemandFlowRateEBRight, HeavyVehAdjustmentFactor)
  AdjustedNBThrough <- DemandFlowRatePCEFcn(DemandFlowRateNBThrough, HeavyVehAdjustmentFactor)
  AdjustedNBLeft <- DemandFlowRatePCEFcn(DemandFlowRateNBLeft, HeavyVehAdjustmentFactor)
  AdjustedNBRight <- DemandFlowRatePCEFcn(DemandFlowRateNBRight, HeavyVehAdjustmentFactor)
  AdjustedWBThrough <- DemandFlowRatePCEFcn(DemandFlowRateWBThrough, HeavyVehAdjustmentFactor)
  AdjustedWBLeft <- DemandFlowRatePCEFcn(DemandFlowRateWBLeft, HeavyVehAdjustmentFactor)
  AdjustedWBRight <- DemandFlowRatePCEFcn(DemandFlowRateWBRight, HeavyVehAdjustmentFactor)
  AdjustedSBThrough <- DemandFlowRatePCEFcn(DemandFlowRateSBThrough, HeavyVehAdjustmentFactor)
  AdjustedSBLeft <- DemandFlowRatePCEFcn(DemandFlowRateSBLeft, HeavyVehAdjustmentFactor)
  AdjustedSBRight <- DemandFlowRatePCEFcn(DemandFlowRateSBRight, HeavyVehAdjustmentFactor)
  EBCirculatingFlowRate <- CirculatingFlowRateFcn(AdjustedWBLeft, AdjustedSBThrough, AdjustedSBLeft)
  NBCirculatingFlowRate <- CirculatingFlowRateFcn(AdjustedSBLeft, AdjustedEBThrough, AdjustedEBLeft)
  WBCirculatingFlowRate <- CirculatingFlowRateFcn(AdjustedEBLeft, AdjustedNBThrough, AdjustedNBLeft)
  SBCirculatingFlowRate <- CirculatingFlowRateFcn(AdjustedNBLeft, AdjustedWBThrough, AdjustedWBLeft)
  TotalAdjustedEB <- DirectionalVolumeFcn(AdjustedEBThrough, AdjustedEBLeft, AdjustedEBRight)
  TotalAdjustedNB <- DirectionalVolumeFcn(AdjustedNBThrough, AdjustedNBLeft, AdjustedNBRight)
  TotalAdjustedWB <- DirectionalVolumeFcn(AdjustedWBThrough, AdjustedWBLeft, AdjustedWBRight)
  TotalAdjustedSB <- DirectionalVolumeFcn(AdjustedSBThrough, AdjustedSBLeft, AdjustedSBRight)
  EBCapacityPCE <- EntryLaneCapacityFcn(EBCirculatingFlowRate)
  NBCapacityPCE <- EntryLaneCapacityFcn(NBCirculatingFlowRate)
  WBCapacityPCE <- EntryLaneCapacityFcn(WBCirculatingFlowRate)
  SBCapacityPCE <- EntryLaneCapacityFcn(SBCirculatingFlowRate)
  EBFlowRateVPH <- FlowRateToVPHFcn(TotalAdjustedEB, HeavyVehAdjustmentFactor)
  NBFlowRateVPH <- FlowRateToVPHFcn(TotalAdjustedNB, HeavyVehAdjustmentFactor)
  WBFlowRateVPH <- FlowRateToVPHFcn(TotalAdjustedWB, HeavyVehAdjustmentFactor)
  SBFlowRateVPH <- FlowRateToVPHFcn(TotalAdjustedSB, HeavyVehAdjustmentFactor)
  EBCapacity <- CapcityToVPHFcn(EBCapacityPCE, HeavyVehAdjustmentFactor)
  NBCapacity <- CapcityToVPHFcn(NBCapacityPCE, HeavyVehAdjustmentFactor)
  WBCapacity <- CapcityToVPHFcn(WBCapacityPCE, HeavyVehAdjustmentFactor)
  SBCapacity <- CapcityToVPHFcn(SBCapacityPCE, HeavyVehAdjustmentFactor)
  EBVolumeToCapacity <- VolumeToCapacityFcn(EBFlowRateVPH, EBCapacity)
  NBVolumeToCapacity <- VolumeToCapacityFcn(NBFlowRateVPH, NBCapacity)
  WBVolumeToCapacity <- VolumeToCapacityFcn(WBFlowRateVPH, WBCapacity)
  SBVolumeToCapacity <- VolumeToCapacityFcn(SBFlowRateVPH, SBCapacity)
  EBControlDelay <- ControlDelayRoundaboutFcn(EBCapacity, EBVolumeToCapacity)
  EBLOS <- LOSFcn(EBControlDelay)
  LOSNumber <- approx(StopYieldLOSDF$ControlDelayThreshold, StopYieldLOSDF$NumericLOSthresholds, xout = EBControlDelay)
  OutputDisplayRoundaboutFcn(EBTotalVolume, HeavyVehAdjustmentFactor, AdjustedEBThrough, EBCirculatingFlowRate, EBCapacityPCE, EBVolumeToCapacity, EBControlDelay, EBLOS, LOSNumber)
  Results <- c(EBControlDelay, LOSNumber[[2]])
  return(Results)
}


##Eq. 22-8 Demand Flow Rate

DemandFlowRateFcn <- function(DemandVolume, PHF){
  DemandFlowRate <- DemandVolume/PHF
  return(DemandFlowRate)
}

# HCM Exhibit 22-11
VehicleTypePCEfcn <- function(VehicleType){
  if (VehicleType == "Passenger Car"){
    PassengerCarEquivalent <- 1.0
  } else if (VehicleType == "Heavy Vehicle"){
    PassengerCarEquivalent <- 2.0
  }
  return(PassengerCarEquivalent)
}


#HCM Eq. 22-10
HeavyVehFactorFcn <- function(ProportionOfHeavyVeh, PassengerCarEquivalent){
  HeavyVehAdjustmentFactor <- 1/(1+ProportionOfHeavyVeh*(PassengerCarEquivalent-1))
  return(HeavyVehAdjustmentFactor)
}

#HCM Eq. 22-9
DemandFlowRatePCEFcn <- function(DemandFlowRate, HeavyVehAdjustmentFactor){
  DemandFlowRatePCE <- DemandFlowRate/HeavyVehAdjustmentFactor
  return(DemandFlowRatePCE)
}

#EBFlowRateChunk
DirectionalVolumeFcn <- function(Through, Left, Right){
  TotalVolume <- Through+Left+Right
  return(TotalVolume)
}

CirculatingFlowRateFcn <- function(OppositeAdjustedLeft, AdjacentAdjustedThrough, AdjacentAdjustedLeft){
  CirculatingFlowRate <- OppositeAdjustedLeft+AdjacentAdjustedThrough+AdjacentAdjustedLeft
  return(CirculatingFlowRate)
}

#EastboundEntryFlowRate are the same as IndividualDemandFlowRate (totals as well)

#TotalEntryFlowRate


#EntryLaneCapacities
EntryLaneCapacityFcn <- function(CirculatingFlowRate){
  CapacityPCE <- 1380*exp((-1.02*10^-3)*CirculatingFlowRate)
  return(CapacityPCE)
}

#HCM Eq. 22-13
FlowRateToVPHFcn <- function(TotalAdjusted, HeavyVehAdjustmentFactor){
  FlowRateToVPH <- TotalAdjusted*HeavyVehAdjustmentFactor
  return(FlowRateToVPH)
}

#HCM Eq. 22-14
CapcityToVPHFcn <- function(CapacityPCE, HeavyVehAdjustmentFactor){
  CapacityToVPH <- CapacityPCE*HeavyVehAdjustmentFactor
  return(CapacityToVPH)
}

#HCM Eq. 22-16
VolumeToCapacityFcn <- function(FlowRateVPH, Capcity){
  VolumeToCapacity <- FlowRateVPH/Capcity
  return(VolumeToCapacity)
}

#HCM Eq. 22-17
ControlDelayRoundaboutFcn <- function(Capacity, VolumeToCapacity){
  AnalysisPeriod <- 0.25
  Term1 <- (3600/Capacity)
  Term2 <- 900*AnalysisPeriod
  Term3 <- VolumeToCapacity-1
  Term4 <- sqrt((Term3^2)+(Term1)*VolumeToCapacity/(450*AnalysisPeriod))
  ControlDelay <- Term1+Term2*(Term3+Term4)+5*min(VolumeToCapacity,1)
  return(ControlDelay)
}

# HCM Exhibit 22-8
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

OutputDisplayRoundaboutFcn <- function(DemandFlowRate, HeavyVehAdjustmentFactor, DemandFlowRatePCE, CirculatingFlowRate, CapacityPCE, VolumeToCapacity, ControlDelay, LOS, LOSNumber){
  cat("Demand Flow Rate =", format(round(DemandFlowRate, 1), nsmall=1), "veh/h \n")
  cat("Heavy Vehicle Adjustment Factor =", format(round(HeavyVehAdjustmentFactor, 3), nsmall=3), "veh/h \n")
  cat("Demand Flow Rate =", format(round(DemandFlowRatePCE, 1), nsmall=1), "pce/h \n")
  cat("Eastbound Circulating Flow Rate =", format(round(CirculatingFlowRate, 1), nsmall=1), "pce/h \n")
  #cat("Eastbound Entry Flow Rate =", format(round(EBEntryFlowRate, 1), nsmall=1), "veh/h \n")
  cat("Eastbound Entry Lane Capacity =", format(round(CapacityPCE, 1), nsmall=1), "pce/h \n")
  cat("Eastbound v/c Ratio =", format(round(VolumeToCapacity, 3), nsmall=3), "\n")
  cat("Eastbound Control Delay =", format(round(ControlDelay, 1), nsmall=1), "s/veh \n")  
  cat("LOS =", LOS, "\n")
  cat("LOS numeric value =", LOSNumber[[2]])
}