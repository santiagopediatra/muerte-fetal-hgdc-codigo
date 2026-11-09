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


suppressPackageStartupMessages({library(cmdstanr);library(posterior);library(brms);library(openxlsx)})
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else hgdc_results()
out <- hgdc_private_path(out)
r<-readRDS(file.path(out,'modelos_nuevos.rds'));s<-readRDS(file.path(out,'modelos_secundarios_nuevos.rds'));stopifnot(r$modelos_verificados,s$verificados)
z<-s$d[s$d$FILA_EXCEL%in%s$pares$SCORE$filas,];stopifnot(identical(z$FILA_EXCEL,s$pares$SCORE$filas))
means<-sapply(z[c('EDAD_ANALITICA','SCORE_ANALITICO')],mean);sds<-sapply(z[c('EDAD_ANALITICA','SCORE_ANALITICO')],sd)
for(v in names(means))z[[paste0(v,'_z')]]<-(z[[v]]-means[v])/sds[v]
model<-cmdstan_model(file.path(out,'modelo_bayesiano.stan'),quiet=TRUE)
res<-list();diags<-list();chain_diag<-list()
for(add in c(FALSE,TRUE)){
 label<-if(add)'SCORE_con' else 'SCORE_sin'
 f<-update(r$formula,.~.-EDAD_ANALITICA+EDAD_ANALITICA_z);if(add)f<-update(f,.~.+SCORE_ANALITICO_z)
 X<-model.matrix(f,z)[,-1,drop=FALSE];meanX<-colMeans(X);Xc<-sweep(X,2,meanX)
 bd<-make_standata(f,data=z,family=bernoulli(),prior=c(prior(normal(0,.7),class='b'),prior(normal(-4,1.5),class='Intercept')))
 stopifnot(isTRUE(all.equal(unname(bd$X[,-1,drop=FALSE]),unname(X),tolerance=1e-12)))
 cat('BAYES',label,nrow(z),sum(z$Y),'\n');flush.console()
 fit<-model$sample(data=list(N=nrow(z),K=ncol(X),y=as.integer(z$Y),Xc=Xc,means_X=as.vector(meanX),prior_sd=.7),chains=4,parallel_chains=4,seed=1409,iter_warmup=2000,iter_sampling=2000,adapt_delta=.99,max_treedepth=15,refresh=1000,output_dir=out,output_basename=paste0('cmdstan_',label))
 fit$save_object(file.path(out,paste0('bayes_',label,'.rds')))
 draws<-as_draws_matrix(fit$draws());summary<-fit$summary();np<-as_draws_array(fit$sampler_diagnostics())
 res[[label]]<-do.call(rbind,lapply(seq_len(ncol(X)),function(k){p<-paste0('beta[',k,']');term<-colnames(X)[k];orig<-sub('_z$','',term);b<-draws[,p];if(orig%in%names(sds))b<-b/sds[orig];dg<-summary[summary$variable==p,];data.frame(modelo=label,termino=orig,N=nrow(z),Y1=sum(z$Y),OR=median(exp(b)),LI95=quantile(exp(b),.025),LS95=quantile(exp(b),.975),P_OR_mayor1=mean(b>0),Rhat=dg$rhat,ESS_bulk=dg$ess_bulk,ESS_tail=dg$ess_tail,row.names=NULL)}))
 chain_diag[[label]]<-do.call(rbind,lapply(1:4,function(ch){e<-np[,ch,'energy__'];data.frame(modelo=label,cadena=ch,divergencias=sum(np[,ch,'divergent__']),saturaciones=sum(np[,ch,'treedepth__']>=15),EBFMI=mean(diff(e)^2)/var(e))}))
 dg<-chain_diag[[label]];a<-summary[summary$variable!='lp__',];diags[[label]]<-data.frame(modelo=label,N=nrow(z),Y1=sum(z$Y),max_Rhat=max(a$rhat),min_ESS_bulk=min(a$ess_bulk),min_ESS_tail=min(a$ess_tail),divergencias=sum(dg$divergencias),saturaciones=sum(dg$saturaciones),min_EBFMI=min(dg$EBFMI),convergencia=max(a$rhat)<1.01&&min(a$ess_bulk)>=400&&min(a$ess_tail)>=400&&sum(dg$divergencias)==0&&sum(dg$saturaciones)==0&&min(dg$EBFMI)>.3)
 write.csv(summary,file.path(out,paste0('diagnosticos_parametros_',label,'.csv')),row.names=FALSE)
 vars<-c('alpha_centered',paste0('beta[',which(grepl('DPP|RPM|SCORE',colnames(X))),']'));arr<-as_draws_array(fit$draws(vars));pdf(file.path(out,'09_figuras',paste0('trazas_',label,'.pdf')),width=10,height=8);par(mfrow=c(length(vars),1),mar=c(2,4,2,1));for(v in vars)matplot(matrix(as.numeric(arr[,,v]),nrow=dim(arr)[1],ncol=dim(arr)[2]),type='l',lty=1,col=1:4,main=v,xlab='Iteración',ylab='Coeficiente');dev.off()
}
tab<-do.call(rbind,res);diag<-do.call(rbind,diags);chains<-do.call(rbind,chain_diag)
write.xlsx(list(estimaciones=tab,diagnosticos=diag,cadenas=chains,escalas=data.frame(variable=names(means),media=means,DE=sds),muestra=data.frame(N=nrow(z),Y1=sum(z$Y),mismas_filas=TRUE)),file.path(out,'08c_sensibilidad_bayes_SCORE.xlsx'),overwrite=FALSE)
write.csv(tab,file.path(out,'estimaciones_bayes_SCORE.csv'),row.names=FALSE);write.csv(diag,file.path(out,'diagnosticos_bayes_SCORE.csv'),row.names=FALSE)
cat('\n## Comparación bayesiana SCORE\n',file=file.path(out,'07_diagnosticos_modelos.md'),append=TRUE)
cat(paste(c('Modelos sin/conSCORE estandarizado, ambos en las mismas filas/datos. Priors ymuestreo iguales al principalSD0.7. Coeficientes reexpresados poraño/punto. TemporalidadSCORE no demostrada; no predictorcausal antecedente.',capture.output(print(diag,row.names=FALSE))),collapse='\n'),file=file.path(out,'07_diagnosticos_modelos.md'),append=TRUE)
print(diag);print(tab[tab$termino=='SCORE_ANALITICO',]);if(!all(diag$convergencia))stop('Diagnósticos bayesianosSCORE requieren revisión.')
