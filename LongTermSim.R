source("PModelDeterministic.R")
days = 14
parameters$TSTOP = days*24.0
solution = run_model(parameters)

plot(x=seq(.1,24*days,by=.1),y=solution[-1,"CA"],log="y",type="l")

plot(x=seq(.1,24*days,by=.1),
     y=(solution[-1,"ALiver"]/(parameters$VLiverC*parameters$BW)),
     log="y",type="l")
