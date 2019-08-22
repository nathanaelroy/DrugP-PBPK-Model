# Generated from DrugPmodel2.csl by acsl2r v0.4.0 on 2018-06-22T15:16:47.547Z

# Review the following before use!

# Definition is VPlasma. Found and corrected: Vplasma.

# Help improve this tool: please submit faults you find to
# https://github.com/acsl2r/acsl2r/issues

#pragma exec run_model parameters

if (!require(deSolve))
{
  stop("The deSolve package is required. Please install it.")
}

# INITIAL

assign_parameters = function()
{
  # code that is executed once at the beginning of a simulation run goes here
  # Changing code from learning model to create small (wrong) DrugP model
  # !!!!constants

  # !blood flow constants
  QCC <- 15.87 #@p L/h/kg^.75 Cardiac output

  QLiverC <- 0.194 #@p Fractional blood flow to liver

  # Body weight
  BW <- 80 #@p (kg) simulated body weight of model in kg

  VPlasmaC <- 0.0627 #@p (%BW) |Fractional volume of plasma

  VLiverC <- 0.03 #@p (%BW) |Fractional volume of liver

  #@p Partition coefficients for drug
  PLiver <- 0.42 

  PSlow <- 0.33 #@p

  PRich <- 0.42 #@p

  # Parameter for unbound percentage of drug
  Funbound <- 0.31 #@p Drugbank suggests 69% will be bound

  # Uptake from IV
  # metabolism in the liver we might decide to use M-M equation to deal with metabolism
  # !CONSTANT KmLiver = 147 !(UNITS)
  # !CONSTANT VmaxLiverC = 147 !UNITS/H/BW^.75

  # For now, metabolism and glomular filtration are the clearnance terms
  GFR <- 7.2 #@p (L/h) clearancce by glomelar filtration

  # Have set liver constant equivalent to GFR to start with since literature suggests metabolism is primary mode of clearance
  ClLiverC <- 0.269 #@p (L/h/kg^.75) Clearance from the liver through metabolism

  # dosing parameters

  # iv dosing in microg/kg
  IVdoseC <- 4000 #@p (microg/kg) |IV dose

  TSTOP <- 24 #@p (h) Simulation Period

  tlen <- 2 #@p (h) length of iv infusion

  tstart <- 0 #@p delay time for start of dosing

  CINT <- 0.1 #@p

  # return all variables in this function's environment
  as.list(sys.frame(sys.nframe()))
}

calculate_variables = function(parameters)
{
  with(parameters,
  {
    # return all variables in this function's environment
    as.list(sys.frame(sys.nframe()))

  }) # end with
}

# END!INITIAL



# DYNAMIC

# DERIVATIVE

derivative = function(t, y, parameters, ...)
{
  with(parameters,
  {
    AIV <- y[1]
    AClurine <- y[2]
    AMET <- y[3]
    AUCCV <- y[4]
    APlasma <- y[5]
    ALiver <- y[6]
    ASlow <- y[7]
    ARich <- y[8]

    # code for calulating the derivative goes here

    # Scaled Cardiac outputs and blood flows
    QC <- QCC * (BW ^ 0.75) # Cardiac output

    QLiver <- QLiverC * QC 

    QS <- 0.24 * QC 

    QR <- (0.76 * QC) - QLiver 

    # Scaled tissue volume, for pbpk model we usually assume density is 1kg/L (density of water
    VLiver <- VLiverC * BW # Volume of Liver

    VPlasma <- VPlasmaC * BW # Volume of plasma

    VS <- 0.6 * BW 

    VR <- (0.33 * BW) - VLiver 

    # Scaled kinetic parameters again, may need to use in future model
    # Vmaxliver = VmaxliverC*BW**.75 !Vmax of drug

    # Single IV dosing code
    IVdose <- IVdoseC * BW 

    ivon <- ifelse((t > tlen), 0, 1) # GT means greater than

    IVR <- ivon * IVdose / tlen 

    ClLiver <- ClLiverC * (BW ^ 0.75) 

    Dailyauccv <- AUCCV / ((t + 1e-33) / 24) 

    CA <- APlasma / VPlasma # arterial/venous concentration

    # Excretion in the Urine
    # Scaled parameter for GFR and metabolism
    Clurine <- GFR * CA * Funbound # uses the concentration in the plasma as the clearance term

    CA_free <- CA * Funbound 

    CVLiver <- ALiver / (VLiver * PLiver) # Venous blood concentration of drug leaving liver

    MET <- ClLiver * CVLiver # Amount metabolized by the liver

    # Compartment for liver
    RALiver <- (QLiver * (CA_free - CVLiver)) - MET # Amount change in the unbound concentration in the liver

    CLiver <- ALiver / VLiver # Concentration of drug in the liver

    CVSlow <- ASlow / (VS * PSlow) # Venous blood concentration of drug leaving the slowly perfused tissues

    # Compartment in the slowly perfused tissues
    RSlow <- QS * (CA_free - CVSlow) # Rate of drug change in the slowly perfused tissues

    CSlow <- ASlow / VS # Concentration of drug in the slowly perfused tissues

    CVRich <- ARich / (VR * PRich) # Venous blood concentration of drug leaving the Richly perfused tissues

    # Compartment in the richly perfused tissues
    RRich <- QR * (CA_free - CVRich) # Rate of drug change in the Richly perfused tissues

    # ***************************Model for drug******!

    # Compartment for the plasma, added in Funbound to account for binding in plasma
    CV <- ((CVLiver * QLiver) + (CVSlow * QS) + (CVRich * QR)) / QC # amount of free concentration coming from tissues

    Rplasma <- (QC * (CV - CA_free)) + IVR - Clurine # Coming from tissues is entirely free drugP, IV will be bound and free, urine

    CRich <- ARich / VR # Concentration of drug in the Richly perfused tissues

    # Mass balance
    Qtotal <- QLiver + QR + QS 

    Qbal <- Qtotal - QC 

    BWorgans <- VLiver + VS + VR 

    TMASSdrug <- APlasma + ALiver + ARich + ASlow 

    Lossdrug <- AClurine + AMET 

    BAL <- AIV - (Lossdrug + TMASSdrug) 


    list(c(
      # pack and return derivatives
      IVR,
      Clurine,
      MET,
      CV,
      Rplasma,
      RALiver,
      RSlow,
      RRich
    ), c(
      # pack and return outputs
      #  QC = unname(QC)
      #, QLiver = unname(QLiver)
      #, QS = unname(QS)
      #, QR = unname(QR)
      #, VLiver = unname(VLiver)
      #, VPlasma = unname(VPlasma)
      #, VS = unname(VS)
      #, VR = unname(VR)
      #, IVdose = unname(IVdose)
      #, ivon = unname(ivon)
      #, ClLiver = unname(ClLiver)
      #, Dailyauccv = unname(Dailyauccv)
      #, CA = unname(CA)
      #, CA_free = unname(CA_free)
      #, CVLiver = unname(CVLiver)
      #, CLiver = unname(CLiver)
      #, CVSlow = unname(CVSlow)
      #, CSlow = unname(CSlow)
      #, CVRich = unname(CVRich)
      #, CRich = unname(CRich)
      #, Qtotal = unname(Qtotal)
      #, Qbal = unname(Qbal)
      #, BWorgans = unname(BWorgans)
      #, TMASSdrug = unname(TMASSdrug)
      #, Lossdrug = unname(Lossdrug)
      #, BAL = unname(BAL)
      )
    ) # end list

  }) # end with
}



run_model <- function(parameters)
{
  parameters <- calculate_variables(parameters)

  with(parameters, {

    TSTART <- 0.0
    times <- seq.int(TSTART, TSTOP, CINT)

    y <- c(
      AIV = 0,
      AClurine = 0,
      AMET = 0,
      AUCCV = 0,
      APlasma = 0,
      ALiver = 0,
      ASlow = 0,
      ARich = 0
      )

    solution <- deSolve::ode(
      y, 
      times, 
      derivative, 
      parameters, 
      method = "lsodes"
      )

    return(as.matrix(unclass(solution)))
  })
}

parameters <- assign_parameters()
solution <- run_model(parameters)

# END ! DERIVATIVE
# END ! DYNAMIC
# END ! PROGRAM


if (F)
{
  parameters <- calculate_variables(parameters)
  
  with(parameters, {
    with(as.data.frame(solution), {
  
      # TERMINAL

      # code that is executed once at the end of a simulation run goes here
      
      # plot(time, BAL, type = "l", xlab = "time [units]", ylab = "[m]", main = "[main]")
      
      # END ! TERMINAL
    })
  })
}

