# Adaptación de fase 5: rutas privadas y protección contra ejecución accidental.
.hfile <- grep("^--file=", commandArgs(trailingOnly=FALSE), value=TRUE)
if(!length(.hfile)) stop("Ejecute mediante Rscript; no source() interactivo.")
.hroot <- dirname(normalizePath(sub("^--file=", "", .hfile[1]), mustWork=TRUE))
while(!file.exists(file.path(.hroot, "config", "project.json"))) {
 .hparent <- dirname(.hroot); if(identical(.hparent,.hroot)) stop("No se encontró la raíz del proyecto")
 .hroot <- .hparent
}
source(file.path(.hroot,"scripts","verificacion","rutas.R"))
root <- .hroot
setwd(root)
if(Sys.getenv("HGDC_PRIVATE_WORKFLOW") != "YES") stop("Flujo privado deshabilitado. Consulte README; esta fase no ejecuta modelos.")
if(dir.exists(file.path(root,"renv","library"))) {
 if(!requireNamespace("renv",quietly=TRUE)) stop("Se requiere renv para cargar la biblioteca restaurada")
 renv::load(project=root)
}


suppressPackageStartupMessages({library(cmdstanr);library(posterior);library(brms);library(openxlsx);library(ggplot2)})
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else hgdc_results()
out <- hgdc_private_path(out)
r<-readRDS(file.path(out,'modelos_nuevos.rds'));stopifnot(isTRUE(r$modelos_verificados),isTRUE(attr(r$d,'validacion')$aprobada))
stopifnot(!file.exists(file.path(out,'05b_modelos_bayesianos.xlsx')))
f<-r$formula;z<-r$d[r$d$FILA_EXCEL%in%r$fits$Principal$filas,];stopifnot(identical(z$FILA_EXCEL,r$fits$Principal$filas))
mu<-mean(z$EDAD_ANALITICA);age_sd<-sd(z$EDAD_ANALITICA);z$EDAD_ANALITICA_z<-(z$EDAD_ANALITICA-mu)/age_sd
fb<-update(f,.~.-EDAD_ANALITICA+EDAD_ANALITICA_z)
X<-model.matrix(fb,z);X<-X[,colnames(X)!='(Intercept)',drop=FALSE];means<-colMeans(X);Xc<-sweep(X,2,means)
# Verify identical design/parameterization to brms: Intercept is internally centered over all columns.
pri<-c(prior(normal(0,0.7),class='b'),prior(normal(-4,1.5),class='Intercept'))
bd<-make_standata(fb,data=z,family=bernoulli('logit'),prior=pri)
stopifnot(isTRUE(all.equal(unname(bd$X[, -1,drop=FALSE]),unname(X),tolerance=1e-12)),identical(as.integer(bd$Y),as.integer(z$Y)))
writeLines(make_stancode(fb,data=z,family=bernoulli('logit'),prior=pri),file.path(out,'parametrizacion_brms_referencia.stan'))
code <- 'data {
 int<lower=1> N;
 int<lower=1> K;
 array[N] int<lower=0,upper=1> y;
 matrix[N,K] Xc;
 vector[K] means_X;
 real<lower=0> prior_sd;
}
parameters {
 real alpha_centered;
 vector[K] beta;
}
model {
 alpha_centered ~ normal(-4,1.5);
 beta ~ normal(0,prior_sd);
 y ~ bernoulli_logit_glm(Xc,alpha_centered,beta);
}
generated quantities {
 real alpha_baseline = alpha_centered-dot_product(means_X,beta);
}
'
stanfile<-file.path(out,'modelo_bayesiano.stan');writeLines(code,stanfile)
model<-cmdstan_model(stanfile,quiet=TRUE)
design<-list(N=nrow(z),K=ncol(Xc),y=as.integer(z$Y),Xc=Xc,means_X=means,filas=z$FILA_EXCEL,terminos=colnames(X),media_edad=mu,DE_edad=age_sd)
if(file.exists(file.path(out,'datos_bayes_nuevos.rds')))stopifnot(identical(design,readRDS(file.path(out,'datos_bayes_nuevos.rds'))))
saveRDS(design,file.path(out,'datos_bayes_nuevos.rds'))
res<-list();diag<-list();chain_diag<-list();ppc<-list()
for(s in c(.7,1.5,2.5)){
 label<-gsub('[.]','_',format(s,nsmall=1));data<-list(N=nrow(z),K=ncol(Xc),y=as.integer(z$Y),Xc=Xc,means_X=as.vector(means),prior_sd=s)
 cat('BAYES INICIO SD',s,'N',nrow(z),'Y1',sum(z$Y),'\n');flush.console()
 fitpath<-file.path(out,paste0('bayes_sd_',label,'.rds'))
 if(file.exists(fitpath)){fit<-readRDS(fitpath);cat('Retomando ajuste nuevo ya terminado con diseño idéntico\n')}else{fit<-model$sample(data=data,seed=1409,chains=4,parallel_chains=4,iter_warmup=2000,iter_sampling=2000,adapt_delta=.99,max_treedepth=15,refresh=500,output_dir=out,output_basename=paste0('cmdstan_sd_',label))
 fit$save_object(fitpath)}
 draws<-as_draws_matrix(fit$draws());summ<-fit$summary();diagnostic_draws<-as_draws_array(fit$sampler_diagnostics())
 res[[label]]<-do.call(rbind,lapply(seq_len(ncol(X)),function(k){p<-paste0('beta[',k,']');b<-draws[,p];term<-colnames(X)[k];if(term=='EDAD_ANALITICA_z'){b<-b/age_sd;term<-'EDAD_ANALITICA'};v<-exp(b);dg<-summ[summ$variable==p,];data.frame(poblacion='Principal',prior_sd=s,termino=term,N=nrow(z),Y1=sum(z$Y),OR=median(v),LI95=quantile(v,.025),LS95=quantile(v,.975),P_OR_mayor1=mean(b>0),Rhat=dg$rhat,ESS_bulk=dg$ess_bulk,ESS_tail=dg$ess_tail,row.names=NULL)}))
 chain_diag[[label]]<-do.call(rbind,lapply(seq_len(4),function(ch){e<-diagnostic_draws[,ch,'energy__'];data.frame(prior_sd=s,cadena=ch,divergencias=sum(diagnostic_draws[,ch,'divergent__']),saturaciones=sum(diagnostic_draws[,ch,'treedepth__']>=15),EBFMI=mean(diff(e)^2)/var(e))}))
 vars<-summ[summ$variable!='lp__',];dg<-chain_diag[[label]]
 diag[[label]]<-data.frame(prior_sd=s,max_Rhat=max(vars$rhat),min_ESS_bulk=min(vars$ess_bulk),min_ESS_tail=min(vars$ess_tail),divergencias=sum(dg$divergencias),saturaciones=sum(dg$saturaciones),min_EBFMI=min(dg$EBFMI),convergencia=max(vars$rhat)<1.01&&min(vars$ess_bulk)>=400&&min(vars$ess_tail)>=400&&sum(dg$divergencias)==0&&sum(dg$saturaciones)==0&&min(dg$EBFMI)>.3)
 set.seed(1409);sel<-sample(seq_len(nrow(draws)),1000);betas<-draws[sel,paste0('beta[',seq_len(ncol(X)),']'),drop=FALSE];eta<-sweep(betas%*%t(Xc),1,as.numeric(draws[sel,'alpha_centered']),'+');pred<-matrix(rbinom(length(eta),1,plogis(eta)),nrow=nrow(eta));groups<-list(Total=rep(TRUE,nrow(z)),Ecuatoriana=z$NACIONALIDAD=='ECUATORIANA',Extranjera=z$NACIONALIDAD=='EXTRANJERA',DPP_no=z$DPP=='NO',DPP_si=z$DPP=='SI')
 ppc[[label]]<-do.call(rbind,lapply(names(groups),function(g){q<-groups[[g]];sim<-rowMeans(pred[,q,drop=FALSE]);data.frame(prior_sd=s,grupo=g,N=sum(q),Y1=sum(z$Y[q]),observado=mean(z$Y[q]),prediccion_mediana=median(sim),LI95=quantile(sim,.025),LS95=quantile(sim,.975),P_replicado_mayor_observado=mean(sim>=mean(z$Y[q])),row.names=NULL)}))
 write.csv(summ,file.path(out,paste0('diagnosticos_parametros_sd_',label,'.csv')),row.names=FALSE)
 # Trace plot for centered intercept and the four focus associations.
 tracevars<-c('alpha_centered',paste0('beta[',which(grepl('^DPP|^NACIONALIDAD|^RPM|^ANTECEDENTE_CIRUGIA_UTERINA',colnames(X))),']'))
 arr<-as_draws_array(fit$draws(tracevars));pdf(file.path(out,'09_figuras',paste0('trazas_sd_',label,'.pdf')),width=10,height=9);par(mfrow=c(length(tracevars),1),mar=c(2,4,2,1));for(v in tracevars)matplot(matrix(as.numeric(arr[,,v]),nrow=dim(arr)[1],ncol=dim(arr)[2]),type='l',lty=1,col=1:4,main=v,xlab='Iteración posterior',ylab='Coeficiente');dev.off()
 cat('BAYES FIN SD',s,'Rhat',diag[[label]]$max_Rhat,'divergencias',diag[[label]]$divergencias,'\n');flush.console()
}
table<-do.call(rbind,res);diagnostics<-do.call(rbind,diag);chains<-do.call(rbind,chain_diag);ppcs<-do.call(rbind,ppc)
write.xlsx(list(posteriores=table,diagnosticos_globales=diagnostics,diagnosticos_cadenas=chains,PPC=ppcs,escala=data.frame(variable='EDAD_ANALITICA',media=mu,DE=age_sd),identidad_muestra=data.frame(N=nrow(z),Y1=sum(z$Y),mismas_filas_Firth=identical(z$FILA_EXCEL,r$fits$Principal$filas))),file.path(out,'05b_modelos_bayesianos.xlsx'),overwrite=FALSE)
write.csv(table,file.path(out,'estimaciones_bayesianas.csv'),row.names=FALSE);write.csv(diagnostics,file.path(out,'diagnosticos_bayesianos.csv'),row.names=FALSE)
write.csv(ppcs,file.path(out,'bayes_PPC.csv'),row.names=FALSE)
write.csv(chains,file.path(out,'bayes_diagnosticos_cadenas.csv'),row.names=FALSE)
write.csv(diagnostics,file.path(out,'bayes_diagnosticos_globales.csv'),row.names=FALSE)
focus<-table[grepl('^DPP|^NACIONALIDAD|^RPM|^ANTECEDENTE_CIRUGIA_UTERINA',table$termino),]
write.xlsx(list(comparacion_priors=focus,todos=table,diagnosticos=diagnostics),file.path(out,'06b_sensibilidad_priors.xlsx'),overwrite=FALSE)
cat('\n## Modelo bayesiano\n',file=file.path(out,'07_diagnosticos_modelos.md'),append=TRUE)
cat(paste(c('Modelo logístico con diseño verificado contra make_standata/brms. Edad estandarizada yOR reexpresada poraño. Todas las columnas se centran internamente para conservar el significado del prior delIntercept debrms. Normal(-4,1.5) delintercepto centrado yNormal(0,SD) decoeficientes, SD0.7/1.5/2.5. Cuatro cadenas;2000warmup+2000muestras porcadenas;semilla1409;adapt_delta=.99;treedepth15. No se reutilizaron cadenas/modelos históricos.','Criterios diagnósticos:Rhat<1.01;ESS_bulk/tail>=400;0divergencias;0saturaciones;EBFMI>.3. Las estimaciones se reportan comoOR posterior mediana eintervalo creíble95%, diferente delIC frecuentista.',capture.output(print(diagnostics,row.names=FALSE))),collapse='\n'),file=file.path(out,'07_diagnosticos_modelos.md'),append=TRUE)
capture.output(sessionInfo(),file=file.path(out,'sessionInfo_bayes.txt'))
print(diagnostics);print(focus)
if(!all(diagnostics$convergencia))stop('Diagnósticos bayesianos no satisfactorios; requieren revisión antes de manuscrito definitivo.')
