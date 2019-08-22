# Generated from MCDrugP.csl by acsl2r v0.4.0 on 2018-07-06T13:37:02.427Z

# Review the following before use!

# Definition is VPlasma. Found and corrected: Vplasma.

# Help improve this tool: please submit faults you find to
# https://github.com/acsl2r/acsl2r/issues

#pragma exec run_model parameters

source("TruncatedNormal.R")

if (!require(deSolve))
{
  stop("The deSolve package is required. Please install it.")
}

# INITIAL

assign_parameters = function()
{
  # code that is executed once at the beginning of a simulation run goes here
  # Monte Carlo simulation for DrugP model
  # These constants will be used for generating a truncated normal random variable:
  # ------------------Monte Carlo Section, everything initialized to 0 --------------------!

  # Constants for log-normally distributed blood flow
  WSV_QCC <- normrnd() #@p Total cardiac output will be log-normally distributed

  WSV_QLiverC <- normrnd() #@p Cardiac output of the liver will be normally distributed

  WSV_QRC <- normrnd() #@p Cardiac output of richly perfused tissues will be normally distributed

  WSV_QSC <- normrnd() #@p Cardiac output of slowly perfused tissues will be normally distributed

  # Need to normalize to total cardiac output

  # BW And tissue volume
  WSV_BW <- normrnd() #@p Body weight will be lognormally distributed

  WSV_VPlasmaC <- normrnd() #@p VPlasmaC will be normally distributed

  WSV_VLiverC <- normrnd() #@p VLiverC will be normally distributed

  WSV_VRC <- normrnd() #@p Richly perfused tissue will be normally distributed

  WSV_VSC <- normrnd() #@p Slowly perfused tissue will be normally distributed

  # Note: need to normalize tissue volume to body weight after

  #@p Variability in partition coefficients; all partition coefficients will be lognormally distributed
  WSV_PLiver <- normrnd()

  WSV_PSlow <- normrnd() #@p

  WSV_PRich <- normrnd() #@p

  # Metabolism rate parameters
  WSV_ClurineC <- normrnd(0,.415,-.83,.83) #@p lognormally distributed urine clearance rate

  WSV_ClLiverC <- normrnd(0,.49,-.98,.98) #@p lognormally distributed liver clearance rate

  BWC <- 60 #@p

  # Parameters for tissue binding, association/disociation rate constants
  KALiver <- 312 #@p 6.921125e+00 !Association rate constant for liver

  KDLiver <- 6.1e-4 #@p 6.131478e-17 !Disociation rate constant for liver

  BMAXLivC <- 203704 #@p !385000 !Maximal Liver capacity

  KAS <- 40 #@p !2.916667e-01 !Association rate for slowly perfused tissue

  KDS <- 0.03 #@p !3.166667e-02 !Disociation rate constant for slowly perfused tissue

  BMAXSC <- 2142 #@p 3300 !Maximal slowly perfused capacity

  KAR <- 0.2 #@p !3.393341e-02 !Association rate for richly perfused tissue

  KDR <- 40 #@p 4.492256e-01 !Disociation rate constant for richly perfused tissue

  BMAXRC <- 8783 #@p !166000 !Maximal richly perfused capacity

  # Parameter for unbound percentage of drug
  Funbound <- 0.31 #@p Drugbank suggests 69% will be bound

  # Uptake from IV
  # metabolism in the liver we might decide to use M-M equation to deal with metabolism
  # !CONSTANT KmLiver = 147 !(UNITS)
  # !CONSTANT VmaxLiverC = 147 !UNITS/H/BW^.75
  # dosing parameters

  # iv dosing in microg/kg
  IVdoseC <- 3800 #@p (microg/kg) |IV dose

  TSTOP <- 24 #@p (h) Simulation Period

  tlen <- 2 #@p (h) length of iv infusion

  tstart <- 0 #@p delay time for start of dosing

  CINT <- 0.1 #@p
  
  # !blood flow constants
  QCC <- 15.87 * exp(WSV_QCC) # L/h/kg^.75 Cardiac output
  
  QLiverC <- 0.194 * (1 + WSV_QLiverC) # Fractional blood flow to liver (unscaled)
  
  QSC <- 0.24 * (1 + WSV_QSC) # Fractional blood flow to the slowly perfused (unscaled)
  
  QRC <- (0.76 - QLiverC) * (1 + WSV_QRC) # Fractional blood flow to the richly perfused tissue (unscaled)
  
  QScalingC <- 1 / (QLiverC + QSC + QRC) 
  
  # Body weight
  BW <- BWC * exp(WSV_BW) # (kg) simulated body weight of model in kg
  
  VPlasmaC <- 0.0627 * (1 + WSV_VPlasmaC) # (%BW) |Fractional volume of plasma (unscaled)
  
  VLiverC <- 0.03 * (1 + WSV_VLiverC) # (%BW) |Fractional volume of liver (unscaled)
  
  VSC <- 0.6 * (1 + WSV_VSC) # (%BW) |Fractional volume of slowly perfused tissues (unscaled)
  
  VRC <- (0.33 - VLiverC) * (1 + WSV_VRC) # (%BW) |Fractional volume of richly perfused tissues (unscaled)
  
  VScalingC <- (0.6 + 0.33 + 0.0627) / (VPlasmaC + VLiverC + VSC + VRC) 
  
  # Partition coefficients for drug, based on Table 2 of paper:
  
  # Successful Treatment with Aerosolized DrugP of disease in Rats
  PLiver <- 0.42 * exp(WSV_PLiver) # .42 !for 1 hr 438 !!.42  !130 if 24 hr
  
  PSlow <- 0.33 * exp(WSV_PSlow) # .33 !for 1 hr 654 !!.33 !1500 if 24 hr
  
  PRich <- 0.42 * exp(WSV_PRich) # .42 ! for 1 hr 5541 !!.42 !8461 if 24 hr
  
  # For now, metabolism and glomular filtration are the clearnance terms
  ClurineC <- 0.147 * exp(WSV_ClurineC) # This is now a clearance term from paper! GFR is 7.2 !(L/h) clearancce by glomelar filtration
  
  # Have set liver constant equivalent to GFR to start with since literature suggests metabolism is primary mode of clearance
  ClLiverC <- 3.26 * exp(WSV_ClLiverC) # 3.26 !!!Paper fit 3.26 !!.269 !(L/h/BW**.75) Clearance from the liver through metabolism
  
  # code for calulating the derivative goes here
  
  # Scaled Cardiac outputs and blood flows
  QC <- QCC * (BW ^ 0.75) # Cardiac output
  
  QLiver <- QLiverC * QC * QScalingC 
  
  QS <- QSC * QC * QScalingC 
  
  QR <- QRC * QC * QScalingC 
  
  # Scaled tissue volume, for pbpk model we usually assume density is 1kg/L (density of water
  VLiver <- VLiverC * VScalingC * BW # Volume of Liver
  
  VPlasma <- VPlasmaC * VScalingC * BW # Volume of plasma
  
  VS <- VSC * VScalingC * BW 
  
  VR <- VRC * VScalingC * BW 
  
  # Tissue maximum binding capacity
  BMAXLiv <- BMAXLivC * VLiver # Liver capacity scaled
  
  BMAXS <- BMAXSC * VS # Slowly perfused tissue capacity scaled
  
  BMAXR <- BMAXRC * VR # Richly perfused tissue capacity scaled
  
  # Scaled kinetic parameters again, may need to use in future model
  # Vmaxliver = VmaxliverC*BW**.75 !Vmax of drug
  # Single IV dosing code
  # IVdose = IVdoseC*BW
  # IVR = ivon*IVdose/tlen
  # AIV = Integ(IVR,0.)
  # ivon = RSW(T __gt tlen,0.0,1.0) !GT means greater than
  
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

pulse <- function(t, tz, p, w)
{
  if(t < tz) return(0)
  t <- t - tz
  t <- t %% p
  return(ifelse(t <= w, 1, 0))
}


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
    BALiver <- y[7]
    ASlow <- y[8]
    BAS <- y[9]
    ARich <- y[10]
    BARich <- y[11]

    # constants: These "constants" have been converted in order to do monte carlo analysis

   
    # Multiple dosing
    IVdose <- IVdoseC * BW 

    ivon <- pulse(t, 0, 24, tlen) 

    IVR <- ivon * IVdose / tlen 

    Dailyauccv <- AUCCV / ((t + 1e-33) / 24) 

    CA <- APlasma / VPlasma # arterial/venous concentration

    CA_free <- CA * Funbound 

    MET <- ClLiverC * (BW ^ 0.75) * CA_free # Amount metabolized by the liver

    # Excretion in the Urine
    # Scaled parameter for GFR and metabolism
    Clurine <- ClurineC * (BW ^ 0.75) * CA_free # uses the concentration in the plasma as the clearance term

    APLiver <- ALiver / PLiver 

    CVLiver <- ALiver / (PLiver * VLiver) # Venous blood concentration of drug leaving liver

    CLiver <- ALiver / VLiver # Concentration of drug in the liver

    CBLiver <- BALiver / VLiver 

    BLivCap <- BMAXLiv - BALiver # Capacity for binding remaining

    # Compartment for liver: amount bound and capacity for binding
    RBLiver <- ((-KDLiver) * BALiver) + (KALiver * APLiver * BLivCap) 

    # Compartment for liver: Amount free
    RALiver <- (QLiver * (CA_free - CVLiver)) + (KDLiver * BALiver) - (KALiver * APLiver * BLivCap) # Amount change in the unbound concentration in the liver

    AtotLiv <- ALiver + BALiver 

    CtotLiv <- AtotLiv / VLiver 

    APS <- ASlow / PSlow 

    CVSlow <- ASlow / (VS * PSlow) # Venous blood concentration of drug leaving the slowly perfused tissues

    CSlow <- ASlow / VS # Concentration of drug in the slowly perfused tissues

    ASCap <- BMAXS - BAS 

    RBS <- ((-KDS) * BAS) + (KAS * APS * ASCap) 

    # Compartment in the slowly perfused tissues
    RSlow <- (QS * (CA_free - CVSlow)) + (KDS * BAS) - (KAS * APS * ASCap) # Rate of drug change in the slowly perfused tissues

    CtotSlow <- (ASlow + BAS) / VLiver 

    APRich <- ARich / PRich 

    CVRich <- ARich / (VR * PRich) # Venous blood concentration of drug leaving the Richly perfused tissues

    # ***************************Model for drug******!

    # Compartment for the plasma, added in Funbound to account for binding in plasma
    CV <- ((CVLiver * QLiver) + (CVSlow * QS) + (CVRich * QR)) / QC # amount of free concentration coming from tissues

    Rplasma <- (QC * (CV - CA_free)) + IVR - Clurine - MET # Coming from tissues is entirely free DrugP, IV will be bound and free, urine

    CRich <- ARich / VR # Concentration of drug in the Richly perfused tissues

    CBRich <- BARich / VR 

    BRichCap <- BMAXR - BARich # Capacity for binding remaining

    # Compartment for liver: amount bound and capacity for binding
    RBRich <- ((-KDR) * BARich) + (KAR * APRich * BRichCap) 

    # Compartment in the richly perfused tissues
    RRich <- (QR * (CA_free - CVRich)) + (KDR * BARich) - (KAR * APRich * BRichCap) # Rate of drug change in the Richly perfused tissues

    AtotRich <- ARich + BARich 

    CtotRich <- AtotRich / VR 

    # Mass balance
    Qtotal <- QLiver + QR + QS 

    Qbal <- Qtotal - QC 

    BWorgans <- VLiver + VS + VR 

    TMASSdrug <- APlasma + ALiver + ARich + ASlow + BALiver + BARich + BAS 

    Lossdrug <- AClurine + AMET 

    BAL <- AIV - (Lossdrug + TMASSdrug) 

    KidneyClearance <- AClurine / (AIV + 1e-33) 

    DoseInLiver <- AtotLiv / (AIV + 1e-33) 


    list(c(
      # pack and return derivatives
      IVR,
      Clurine,
      MET,
      CV,
      Rplasma,
      RALiver,
      RBLiver,
      RSlow,
      RBS,
      RRich,
      RBRich
    ), c(
      # pack and return outputs
      #  QCC = unname(QCC)
      #, QLiverC = unname(QLiverC)
      #, QSC = unname(QSC)
      #, QRC = unname(QRC)
      #, QScalingC = unname(QScalingC)
      #, BW = unname(BW)
      #, VPlasmaC = unname(VPlasmaC)
      #, VLiverC = unname(VLiverC)
      #, VSC = unname(VSC)
      #, VRC = unname(VRC)
      #, VScalingC = unname(VScalingC)
      #, PLiver = unname(PLiver)
       PSlow = unname(PSlow)
      #, PRich = unname(PRich)
      #, ClurineC = unname(ClurineC)
      #, ClLiverC = unname(ClLiverC)
      #, QC = unname(QC)
      #, QLiver = unname(QLiver)
      #, QS = unname(QS)
      #, QR = unname(QR)
      #, VLiver = unname(VLiver)
      #, VPlasma = unname(VPlasma)
      #, VS = unname(VS)
      #, VR = unname(VR)
      #, BMAXLiv = unname(BMAXLiv)
      #, BMAXS = unname(BMAXS)
      #, BMAXR = unname(BMAXR)
      #, IVdose = unname(IVdose)
      #, ivon = unname(ivon)
      #, Dailyauccv = unname(Dailyauccv)
      , CA = unname(CA)
      #, CA_free = unname(CA_free)
      #, APLiver = unname(APLiver)
      #, CVLiver = unname(CVLiver)
      #, CLiver = unname(CLiver)
      #, CBLiver = unname(CBLiver)
      #, BLivCap = unname(BLivCap)
      #, AtotLiv = unname(AtotLiv)
      #, CtotLiv = unname(CtotLiv)
      #, APS = unname(APS)
      #, CVSlow = unname(CVSlow)
      #, CSlow = unname(CSlow)
      #, ASCap = unname(ASCap)
      #, CtotSlow = unname(CtotSlow)
      #, APRich = unname(APRich)
      #, CVRich = unname(CVRich)
      #, CRich = unname(CRich)
      #, CBRich = unname(CBRich)
      #, BRichCap = unname(BRichCap)
      #, AtotRich = unname(AtotRich)
      #, CtotRich = unname(CtotRich)
      #, Qtotal = unname(Qtotal)
      #, Qbal = unname(Qbal)
      #, BWorgans = unname(BWorgans)
      #, TMASSdrug = unname(TMASSdrug)
      #, Lossdrug = unname(Lossdrug)
      , BAL = unname(BAL)
      #, KidneyClearance = unname(KidneyClearance)
      #, DoseInLiver = unname(DoseInLiver)
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
      BALiver = 0,
      ASlow = 0,
      BAS = 0,
      ARich = 0,
      BARich = 0
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


set.seed(47)
#plots credible interval for concentration data
plotCI <- function(solution,total = 1000){
  h <- seq(0,24,.1)
  ca025 <- c()
  ca975 <- c()
  camed <- c()
  med <- .5*total
  lb <- .025*total
  ub <- .975*total
  for(i in 1:241){
    cati <- sort(solution[i,])
    camed <- c(camed,cati[med])
    ca025 <- c(ca025,cati[lb])
    ca975 <- c(ca975,cati[ub])
  }
  plot(h,ca025,type="l",log="y",ylim = c(1,1300),main= paste("Confidence Interval for Model ",total, "trials"),col="red",ylab = "Concentration",xlab = "Time (h)")
  lines(h,ca975,col = "blue")
  lines(h,camed)
}


run.plot.sol <- function(total=1000){
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = total, width = 300)
  parameters <- replicate(total,assign_parameters())
  solution <- c()
  for(i in 1:total){
    solution <- cbind(solution,run_model(parameters[,i])[,14])
    setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),
                                          "% done"))
  }
  close(pb)
  plotCI(solution,total)
  return(solution)
}
#Based on these trials 2000 trials is enough for the monte carlo simulation to converge
#t.1 <- run.plot.sol()
#t.2 <- run.plot.sol(2000)
#t.3 <- run.plot.sol(3000)
#t.4 <- run.plot.sol(4000)
#t.5 <- run.plot.sol(5000)

plotCI(t.1,1000)
plotCI(t.2,2000)
plotCI(t.3,3000)
plotCI(t.4,4000)
plotCI(t.5,5000)


#write.csv(t.2,file= "C:\\Users\\Nathaneal.Roy\\Desktop\\Data Folder\\GeneratedData\\Trial2000.csv")
#write.csv(t.1,file= "C:\\Users\\Nathaneal.Roy\\Desktop\\Data Folder\\GeneratedData\\Trial1000.csv")
#write.csv(t.3,file= "C:\\Users\\Nathaneal.Roy\\Desktop\\Data Folder\\GeneratedData\\Trial3000.csv")
#write.csv(t.4,file= "C:\\Users\\Nathaneal.Roy\\Desktop\\Data Folder\\GeneratedData\\Trial4000.csv")
#write.csv(t.5,file= "C:\\Users\\Nathaneal.Roy\\Desktop\\Data Folder\\GeneratedData\\Trial5000.csv")


# total <- 1000
# pb <- winProgressBar(title = "progress bar", min = 0,
#                      max = total, width = 300)
# parameters <- replicate(total,assign_parameters())
# solution <- c()
# for(i in 1:total){
#   solution <- cbind(solution,run_model(parameters[,i])[,14])
#   setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),
#                                         "% done"))
# }
# close(pb)
# 
# #Time series data
# 
# 
# 
# ca025 <- c()
# ca975 <- c()
# camed <- c()
# med <- .5*total
# lb <- .025*total
# ub <- .975*total
# for(i in 1:241){
#   cati <- sort(solution[i,])
#   camed <- c(camed,cati[med])
#   ca025 <- c(ca025,cati[lb])
#   ca975 <- c(ca975,cati[ub])
#   }
# plot(h,ca025,type="l",log="y",ylim = c(1,1300),main= "Rough Confidence Interval for Model 2000 runs",col="red",ylab = "Concentration",xlab = "Time (h)")
# lines(h,ca975,col = "blue")
# lines(h,camed)
# 


# END ! DERIVATIVE
# END ! DYNAMIC
# END ! PROGRAM


##if (F)
#{
 # parameters <- calculate_variables(parameters)
  
 # with(parameters, {
  #  with(as.data.frame(solution), {
  
      # TERMINAL

      # code that is executed once at the end of a simulation run goes here
      
      # plot(time, DoseInLiver, type = "l", xlab = "time [units]", ylab = "[m]", main = "[main]")
      
      # END ! TERMINAL
 #   })
 # })
#}

