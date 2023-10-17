# Intersection Influence Area Calculations
# Version 1.0, 10/17/23
# By Scott Washburn, Ryan Kenis, Emily Dang; University of Florida


InfAreaSignalUSFcn <- function(AvgSpeed, PctHV, SegNumLanes){
  #IML <- 1 #0 = 1 lane, 1 = multiple lanes
  IML <- 0
  if (SegNumLanes > 1){
    IML <- 1
  }
  IASignalUS <- -923.89+35.92*AvgSpeed+1.23*PctHV-374.05*IML
  return(IASignalUS)
}

InfAreaSignalUS8Fcn <- function(AvgSpeed, PctHV){
  IML <- 1
  IASignalUS <- -923.89+35.92*AvgSpeed+1.23*PctHV-374.05*IML
  return(IASignalUS)
}

#Influence Area Signalized Intersection Downstream

InfAreaSignalDSFcn <- function(AvgSpeed, PctHV, SegNumLanes){
  #IML <- 1 #0 = 1 lane, 1 = multiple lanes
  IML <- 0
  if (SegNumLanes > 1){
    IML <- 1
    }
  IASignalDS <- -1929.64+60.25*AvgSpeed+7.23*PctHV-154.15*IML
  return(IASignalDS)
}

InfAreaSignalDS8Fcn <- function(AvgSpeed, PctHV, SegNumLanes){
  IML <- 1
  IASignalDS <- -1929.64+60.25*AvgSpeed+7.23*PctHV-154.15*IML
  return(IASignalDS)
}

#Influence Area Roundabout Intersection Upstream

InfAreaRoundaboutUSFcn <- function(AvgSpeed){
  AvgSpeedCirc <- 15
  IARoundaboutUS <- 402.15+10.21*AvgSpeed-15.27*AvgSpeedCirc
  return(IARoundaboutUS)
}

#Influence Area Roundabout Intersection Downstream

InfAreaRoundaboutDSFcn <- function(AvgSpeed){
  AvgSpeedCirc <- 15
  IARoundaboutDS <- -313.80+32.73*AvgSpeed-27.01*AvgSpeedCirc
  return(IARoundaboutDS)
}

#Influence Area All Way Stop Control Intersection Upstream

InfAreaAWSCUSFcn <- function(AvgSpeed){
  IAAWSCUS <- -1147.62+38.82*AvgSpeed
  return(IAAWSCUS)
}

#Influence Area All Way Stop Control Intersection Downstream

InfAreaAWSCDSFcn <- function(AvgSpeed){
  IAAWSCDS <- -1067.63+44.38*AvgSpeed
  return(IAAWSCDS)
}

#Difference in Area

DifferenceInAreaFcn <- function(InfluenceAreaUS, InfluenceAreaDS){
  Area <- abs(InfluenceAreaUS-InfluenceAreaDS)
  return(Area)
}

#Intersection Adjusted Length

IntAdjustedLengthFcn <- function(IAUpstream, IADownstream){
  FeetInMiles <- 5280
  AdjustedLength <- (IAUpstream+IADownstream)/FeetInMiles
  return(AdjustedLength)
}

#Adjusted Length Calculation

AdjustedLengthFcn <- function(SegLength, SegmentIA, SegGeometricDistance){
  FeetInMiles <- 5280
  AdjustedLength <- ((SegLengthMiles*FeetInMiles)-(SegmentIA-SegGeometricDistance))/FeetInMiles
  return(AdjustedLength)
}

#Adjusted Length Calculation Inbetween Intersections

AdjustedLengthBetweenFcn <- function(SegLength, SegmentDS, SegDSGeometricDistance, SegmentUS, SegUSGeometricDistance){
  FeetInMiles <- 5280
    AdjustedLength <- (SegLengthMiles*FeetInMiles)-(SegmentDS-SegDSGeometricDistance)-(SegmentUS-SegUSGeometricDistance)
  return(AdjustedLength)
}

#Model Effective Length UpStream

ModelEffectiveLengthUSFcn <- function(FlowRate){
  CycleLength <- 90
  EffectiveGreen <- 40
  PctLT <- 0.1
  ProportionOfHeavyVeh <- 0.1
  EffLengthUS <- 266.66+3.047*(FlowRate/100)^2+8.626*CycleLength-0.972*(FlowRate/100)*PctLT-14.102*EffectiveGreen
  return(EffLengthUS)
}

#Model Effective Length UpStream

ModelEffectiveLengthUSLTFcn <- function(FlowRate){
  CycleLength <- 90
  EffectiveGreen <- 40
  PctLT <- 0.1
  ProportionOfHeavyVeh <- 0.1
  OpposingFlowRate <- FlowRate
  EffLengthUSLT <- 412.02+57.997*(FlowRate/500)^3+85.158*(OpposingFlowRate/500)^3-3.656*CycleLength+0.033*((FlowRate/500)*PctLT)^3
  return(EffLengthUSLT)
}

#Model Effective Length DownStream

ModelEffectiveLengthDSFcn <- function(FlowRate){
  CycleLength <- 90
  EffectiveGreen <- 40
  PctLT <- 0.1
  ProportionOfHeavyVeh <- 0.1
  EffLengthDS <- 701.34+51.016*(FlowRate/100)+43.353*ProportionOfHeavyVeh+13.833*CycleLength-1.701*(FlowRate/100)*PctLT-16.760*EffectiveGreen
  return(EffLengthDS)
}