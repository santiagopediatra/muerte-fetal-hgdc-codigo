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


suppressPackageStartupMessages({library(logistf);library(openxlsx);library(posterior);library(cmdstanr)})
src<-hgdc_results();out<-hgdc_revision();dir.create(out,recursive=TRUE,showWarnings=FALSE)
s<-readRDS(file.path(src,'modelos_secundarios_nuevos.rds'));r<-readRDS(file.path(src,'modelos_nuevos.rds'))
checks<-list();add<-function(label,ok){checks[[length(checks)+1]]<<-data.frame(control=paste('SEC',label),ok=all(ok),detalle='Objeto y exportación corregidos; sin reajuste')};eq<-function(x,y)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-10,check.attributes=FALSE))
checkfit<-function(f,z,name){
 b<-coef(f);idx<-match(names(b),z$termino)
 if(inherits(f,'logistf')){lo<-exp(f$ci.lower);hi<-exp(f$ci.upper);pv<-f$prob;con<-all(abs(f$conv)<=1e-7)&&max(f$pl.iter)<500}else{se<-coef(summary(f))[,2];lo<-exp(b-qnorm(.975)*se);hi<-exp(b+qnorm(.975)*se);pv<-coef(summary(f))[,4];con<-f$converged}
 add(paste(name,'OR IC p N Y1'),!anyNA(idx)&&eq(z$OR[idx],exp(b))&&eq(z$LI95[idx],lo)&&eq(z$LS95[idx],hi)&&eq(z$p[idx],pv)&&all(z$N==nrow(f$model))&&all(z$Y1==sum(f$y)))
 add(paste(name,'convergencia'),con)
}
domain<-read.csv(file.path(src,'estimaciones_dominios.csv'));pair<-read.csv(file.path(src,'estimaciones_pares_covariables.csv'))
for(name in names(s$dominios))checkfit(s$dominios[[name]],domain[domain$modelo==name,],name)
for(name in names(s$pares)){
 a<-s$pares[[name]];add(paste(name,'muestras pareadas idénticas'),identical(rownames(a$sin$model),rownames(a$con$model))&&identical(as.numeric(a$sin$y),as.numeric(a$con$y))&&all(sapply(names(a$sin$model),function(v)identical(as.character(a$sin$model[[v]]),as.character(a$con$model[[v]])))))
 for(k in c('sin','con'))checkfit(a[[k]],pair[pair$modelo==paste0(name,'_',k),],paste(name,k))
}
z<-s$d[s$d$FILA_EXCEL%in%s$pares$SCORE$filas,];sds<-sapply(z[c('EDAD_ANALITICA','SCORE_ANALITICO')],sd);for(v in names(sds))z[[paste0(v,'_z')]]<-(z[[v]]-mean(z[[v]]))/sds[v]
tab<-read.csv(file.path(src,'estimaciones_bayes_SCORE.csv'))
for(addscore in c(FALSE,TRUE)){
 label<-if(addscore)'SCORE_con' else 'SCORE_sin';f<-update(r$formula,.~.-EDAD_ANALITICA+EDAD_ANALITICA_z);if(addscore)f<-update(f,.~.+SCORE_ANALITICO_z)
 X<-model.matrix(f,z)[,-1,drop=FALSE];fit<-readRDS(file.path(src,paste0('bayes_',label,'.rds')));draws<-as_draws_matrix(fit$draws())
 for(k in seq_len(ncol(X))){term<-sub('_z$','',colnames(X)[k]);beta<-draws[,paste0('beta[',k,']')];if(term%in%names(sds))beta<-beta/sds[term];row<-tab[tab$modelo==label&tab$termino==term,];q<-quantile(exp(beta),c(.025,.975));add(paste(label,term,'cuantiles verificados'),nrow(row)==1&&eq(row$OR,median(exp(beta)))&&eq(row$LI95,q[1])&&eq(row$LS95,q[2])&&eq(row$P_OR_mayor1,mean(beta>0)))}
}
before<-read.xlsx(file.path(src,'06c_comparacion_antes_despues.xlsx'),sheet='fuente_antes_despues');oldfit<-readRDS(file.path(src,'firth_comparador_fuente_anterior.rds'));idx<-match(names(coef(oldfit)),before$termino)
add('Comparador anterior objeto versus tabla',eq(before$OR_fuente_anterior[idx],exp(coef(oldfit)))&&eq(before$LI95_fuente_anterior[idx],exp(oldfit$ci.lower))&&eq(before$LS95_fuente_anterior[idx],exp(oldfit$ci.upper)))
for(label in c('sd_0_7','sd_1_5','sd_2_5','SCORE_sin','SCORE_con')){
 fit<-readRDS(file.path(src,paste0('bayes_',label,'.rds')));sm<-fit$summary();sm<-sm[sm$variable!='lp__',];np<-as_draws_array(fit$sampler_diagnostics());eb<-sapply(1:4,function(ch){e<-np[,ch,'energy__'];mean(diff(e)^2)/var(e)})
 if(grepl('^sd_',label)){prior<-as.numeric(sub('_','.',sub('sd_','',label)));row<-read.csv(file.path(src,'diagnosticos_bayesianos.csv'));row<-row[row$prior_sd==prior,]}else{row<-read.csv(file.path(src,'diagnosticos_bayes_SCORE.csv'));row<-row[row$modelo==label,]}
 add(paste(label,'diagnósticos desde cadenas'),eq(row$max_Rhat,max(sm$rhat))&&eq(row$min_ESS_bulk,min(sm$ess_bulk))&&eq(row$min_ESS_tail,min(sm$ess_tail))&&eq(row$divergencias,sum(np[,,'divergent__']))&&eq(row$saturaciones,sum(np[,,'treedepth__']>=15))&&eq(row$min_EBFMI,min(eb)))
}
extra<-do.call(rbind,checks);base<-read.csv(file.path(out,'controles_auditoria.csv'));base<-base[!grepl('^SEC ',base$control),];all<-rbind(base,extra)
write.csv(all,file.path(out,'controles_auditoria.csv'),row.names=FALSE)
w<-loadWorkbook(file.path(out,'02_comparacion_modelos.xlsx'));if('control_secundarios'%in%names(w))removeWorksheet(w,'control_secundarios');addWorksheet(w,'control_secundarios');writeData(w,'control_secundarios',extra);saveWorkbook(w,file.path(out,'02_comparacion_modelos.xlsx'),overwrite=TRUE)
print(extra[!extra$ok,]);cat(nrow(extra),'controles secundarios;',sum(extra$ok),'satisfactorios;',nrow(all),'totales\n');stopifnot(all(extra$ok))
