# Solo metadatos de paquetes. No instala, lee datos ni ajusta modelos.
required<-c('readxl','openxlsx','jsonlite','digest','logistf','car','lpSolve','ggplot2','brms','cmdstanr','posterior')
for(p in required)cat(p,if(requireNamespace(p,quietly=TRUE))as.character(packageVersion(p)) else 'NO_INSTALADO','\n')
cat('R',getRversion() |> as.character(),'\n')
if(requireNamespace('cmdstanr',quietly=TRUE))cat('CmdStan',tryCatch(as.character(cmdstanr::cmdstan_version()),error=function(e)'NO_CONFIGURADO'),'\n')
