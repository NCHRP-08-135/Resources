# HCM Roundabout Intersection Calculations
# Version 1.0, 10/17/23
# By Scott Washburn, Ryan Kenis, Emily Dang; University of Florida
# -----------------------------------------------------------------
# Version 1.1, 6/18/25
# By Scott Washburn
# Prefix all function names with "RBT_" to be able to uniquely identify functions within the main Roundabout methodology calculations file once all methodology calculation files are loaded into memory
# Several 'editorial' revisions that do not affect results


RoundaboutMainFcn <- function(VehicleType, ProportionOfHeavyVeh, PHF, EBThrough, EBLeft, EBRight, NBThrough, NBLeft, NBRight, WBThrough, WBLeft, WBRight, SBThrough, SBLeft, SBRight){
  StopYieldLOSDF <- data.frame(ControlDelayThreshold <- c(0, 10, 15, 25, 35, 50), NumericLOSthresholds <- c(0, 1, 2, 3, 4, 5))
  PassengerCarEquivalent <- RBT_VehicleTypePCEfcn(VehicleType)
  HeavyVehAdjustmentFactor <- RBT_HeavyVehFactorFcn(ProportionOfHeavyVeh, PassengerCarEquivalent)
  EBTotalVolume <- RBT_DirectionalVolumeFcn(EBThrough, EBLeft, EBRight)
  NBTotalVolume <- RBT_DirectionalVolumeFcn(NBThrough, NBLeft, NBRight)
  WBTotalVolume <- RBT_DirectionalVolumeFcn(WBThrough, WBLeft, WBRight)
  SBTotalVolume <- RBT_DirectionalVolumeFcn(SBThrough, SBLeft, SBRight)
  DemandFlowRateEBThrough <- RBT_DemandFlowRateFcn(EBThrough, PHF)
  DemandFlowRateEBLeft <- RBT_DemandFlowRateFcn(EBLeft, PHF)
  DemandFlowRateEBRight <- RBT_DemandFlowRateFcn(EBRight, PHF)
  DemandFlowRateNBThrough <- RBT_DemandFlowRateFcn(NBThrough, PHF)
  DemandFlowRateNBLeft <- RBT_DemandFlowRateFcn(NBLeft, PHF)
  DemandFlowRateNBRight <- RBT_DemandFlowRateFcn(NBRight, PHF)
  DemandFlowRateWBThrough <- RBT_DemandFlowRateFcn(WBThrough, PHF)
  DemandFlowRateWBLeft <- RBT_DemandFlowRateFcn(WBLeft, PHF)
  DemandFlowRateWBRight <- RBT_DemandFlowRateFcn(WBRight, PHF)
  DemandFlowRateSBThrough <- RBT_DemandFlowRateFcn(SBThrough, PHF)
  DemandFlowRateSBLeft <- RBT_DemandFlowRateFcn(SBLeft, PHF)
  DemandFlowRateSBRight <- RBT_DemandFlowRateFcn(SBRight, PHF)
  AdjustedEBThrough <- RBT_DemandFlowRatePCEFcn(DemandFlowRateEBThrough, HeavyVehAdjustmentFactor)
  AdjustedEBLeft <- RBT_DemandFlowRatePCEFcn(DemandFlowRateEBLeft, HeavyVehAdjustmentFactor)
  AdjustedEBRight <- RBT_DemandFlowRatePCEFcn(DemandFlowRateEBRight, HeavyVehAdjustmentFactor)
  AdjustedNBThrough <- RBT_DemandFlowRatePCEFcn(DemandFlowRateNBThrough, HeavyVehAdjustmentFactor)
  AdjustedNBLeft <- RBT_DemandFlowRatePCEFcn(DemandFlowRateNBLeft, HeavyVehAdjustmentFactor)
  AdjustedNBRight <- RBT_DemandFlowRatePCEFcn(DemandFlowRateNBRight, HeavyVehAdjustmentFactor)
  AdjustedWBThrough <- RBT_DemandFlowRatePCEFcn(DemandFlowRateWBThrough, HeavyVehAdjustmentFactor)
  AdjustedWBLeft <- RBT_DemandFlowRatePCEFcn(DemandFlowRateWBLeft, HeavyVehAdjustmentFactor)
  AdjustedWBRight <- RBT_DemandFlowRatePCEFcn(DemandFlowRateWBRight, HeavyVehAdjustmentFactor)
  AdjustedSBThrough <- RBT_DemandFlowRatePCEFcn(DemandFlowRateSBThrough, HeavyVehAdjustmentFactor)
  AdjustedSBLeft <- RBT_DemandFlowRatePCEFcn(DemandFlowRateSBLeft, HeavyVehAdjustmentFactor)
  AdjustedSBRight <- RBT_DemandFlowRatePCEFcn(DemandFlowRateSBRight, HeavyVehAdjustmentFactor)
  EBCirculatingFlowRate <- RBT_CirculatingFlowRateFcn(AdjustedWBLeft, AdjustedSBThrough, AdjustedSBLeft)
  NBCirculatingFlowRate <- RBT_CirculatingFlowRateFcn(AdjustedSBLeft, AdjustedEBThrough, AdjustedEBLeft)
  WBCirculatingFlowRate <- RBT_CirculatingFlowRateFcn(AdjustedEBLeft, AdjustedNBThrough, AdjustedNBLeft)
  SBCirculatingFlowRate <- RBT_CirculatingFlowRateFcn(AdjustedNBLeft, AdjustedWBThrough, AdjustedWBLeft)
  TotalAdjustedEB <- RBT_DirectionalVolumeFcn(AdjustedEBThrough, AdjustedEBLeft, AdjustedEBRight)
  TotalAdjustedNB <- RBT_DirectionalVolumeFcn(AdjustedNBThrough, AdjustedNBLeft, AdjustedNBRight)
  TotalAdjustedWB <- RBT_DirectionalVolumeFcn(AdjustedWBThrough, AdjustedWBLeft, AdjustedWBRight)
  TotalAdjustedSB <- RBT_DirectionalVolumeFcn(AdjustedSBThrough, AdjustedSBLeft, AdjustedSBRight)
  EBCapacityPCE <- RBT_EntryLaneCapacityFcn(EBCirculatingFlowRate)
  NBCapacityPCE <- RBT_EntryLaneCapacityFcn(NBCirculatingFlowRate)
  WBCapacityPCE <- RBT_EntryLaneCapacityFcn(WBCirculatingFlowRate)
  SBCapacityPCE <- RBT_EntryLaneCapacityFcn(SBCirculatingFlowRate)
  EBFlowRateVPH <- RBT_FlowRateToVPHFcn(TotalAdjustedEB, HeavyVehAdjustmentFactor)
  NBFlowRateVPH <- RBT_FlowRateToVPHFcn(TotalAdjustedNB, HeavyVehAdjustmentFactor)
  WBFlowRateVPH <- RBT_FlowRateToVPHFcn(TotalAdjustedWB, HeavyVehAdjustmentFactor)
  SBFlowRateVPH <- RBT_FlowRateToVPHFcn(TotalAdjustedSB, HeavyVehAdjustmentFactor)
  EBCapacity <- RBT_CapcityToVPHFcn(EBCapacityPCE, HeavyVehAdjustmentFactor)
  NBCapacity <- RBT_CapcityToVPHFcn(NBCapacityPCE, HeavyVehAdjustmentFactor)
  WBCapacity <- RBT_CapcityToVPHFcn(WBCapacityPCE, HeavyVehAdjustmentFactor)
  SBCapacity <- RBT_CapcityToVPHFcn(SBCapacityPCE, HeavyVehAdjustmentFactor)
  EBVolumeToCapacity <- RBT_VolumeToCapacityFcn(EBFlowRateVPH, EBCapacity)
  NBVolumeToCapacity <- RBT_VolumeToCapacityFcn(NBFlowRateVPH, NBCapacity)
  WBVolumeToCapacity <- RBT_VolumeToCapacityFcn(WBFlowRateVPH, WBCapacity)
  SBVolumeToCapacity <- RBT_VolumeToCapacityFcn(SBFlowRateVPH, SBCapacity)
  EBControlDelay <- RBT_ControlDelayRoundaboutFcn(EBCapacity, EBVolumeToCapacity)
  EBLOS <- RBT_LOSFcn(EBControlDelay)
  LOSNumber <- approx(StopYieldLOSDF$ControlDelayThreshold, StopYieldLOSDF$NumericLOSthresholds, xout = EBControlDelay)
  RBT_OutputDisplayFcn(EBTotalVolume, HeavyVehAdjustmentFactor, AdjustedEBThrough, EBCirculatingFlowRate, EBCapacityPCE, EBVolumeToCapacity, EBControlDelay, EBLOS, LOSNumber)
  Results <- c(EBControlDelay, LOSNumber[[2]])
  return(Results)
}


#Eq. 22-8 Demand Flow Rate
RBT_DemandFlowRateFcn <- function(DemandVolume, PHF){
  DemandFlowRate <- DemandVolume/PHF
  return(DemandFlowRate)
}

#HCM Exhibit 22-11
RBT_VehicleTypePCEfcn <- function(VehicleType){
  if (VehicleType == "Passenger Car"){
    PassengerCarEquivalent <- 1.0
  } else if (VehicleType == "Heavy Vehicle"){
    PassengerCarEquivalent <- 2.0
  }
  return(PassengerCarEquivalent)
}

#HCM Eq. 22-10
RBT_HeavyVehFactorFcn <- function(ProportionOfHeavyVeh, PassengerCarEquivalent){
  HeavyVehAdjustmentFactor <- 1/(1+ProportionOfHeavyVeh*(PassengerCarEquivalent-1))
  return(HeavyVehAdjustmentFactor)
}

#HCM Eq. 22-9
RBT_DemandFlowRatePCEFcn <- function(DemandFlowRate, HeavyVehAdjustmentFactor){
  DemandFlowRatePCE <- DemandFlowRate/HeavyVehAdjustmentFactor
  return(DemandFlowRatePCE)
}

RBT_DirectionalVolumeFcn <- function(Through, Left, Right){
  TotalVolume <- Through+Left+Right
  return(TotalVolume)
}

RBT_CirculatingFlowRateFcn <- function(OppositeAdjustedLeft, AdjacentAdjustedThrough, AdjacentAdjustedLeft){
  CirculatingFlowRate <- OppositeAdjustedLeft+AdjacentAdjustedThrough+AdjacentAdjustedLeft
  return(CirculatingFlowRate)
}

#EastboundEntryFlowRate are the same as IndividualDemandFlowRate (totals as well)

#TotalEntryFlowRate


#EntryLaneCapacities
RBT_EntryLaneCapacityFcn <- function(CirculatingFlowRate){
  CapacityPCE <- 1380*exp((-1.02*10^-3)*CirculatingFlowRate)
  return(CapacityPCE)
}

#HCM Eq. 22-13
RBT_FlowRateToVPHFcn <- function(TotalAdjusted, HeavyVehAdjustmentFactor){
  FlowRateToVPH <- TotalAdjusted*HeavyVehAdjustmentFactor
  return(FlowRateToVPH)
}

#HCM Eq. 22-14
RBT_CapcityToVPHFcn <- function(CapacityPCE, HeavyVehAdjustmentFactor){
  CapacityToVPH <- CapacityPCE*HeavyVehAdjustmentFactor
  return(CapacityToVPH)
}

#HCM Eq. 22-16
RBT_VolumeToCapacityFcn <- function(FlowRateVPH, Capcity){
  VolumeToCapacity <- FlowRateVPH/Capcity
  return(VolumeToCapacity)
}

#HCM Eq. 22-17
RBT_ControlDelayRoundaboutFcn <- function(Capacity, VolumeToCapacity){
  AnalysisPeriod <- 0.25
  Term1 <- (3600/Capacity)
  Term2 <- 900*AnalysisPeriod
  Term3 <- VolumeToCapacity-1
  Term4 <- sqrt((Term3^2)+(Term1)*VolumeToCapacity/(450*AnalysisPeriod))
  ControlDelay <- Term1+Term2*(Term3+Term4)+5*min(VolumeToCapacity,1)
  return(ControlDelay)
}

#HCM Exhibit 22-8
RBT_LOSFcn <- function(ControlDelay){
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

RBT_OutputDisplayFcn <- function(DemandFlowRate, HeavyVehAdjustmentFactor, DemandFlowRatePCE, CirculatingFlowRate, CapacityPCE, VolumeToCapacity, ControlDelay, LOS, LOSNumber){
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
