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


suppressPackageStartupMessages({library(logistf);library(openxlsx);library(jsonlite);library(posterior);library(cmdstanr)})
src<-hgdc_results();out<-hgdc_revision();dir.create(out,recursive=TRUE,showWarnings=FALSE)
r<-readRDS(file.path(src,'modelos_nuevos.rds'));d<-readRDS(file.path(src,'base_analitica_corregida.rds'))
checks<-list();add<-function(label,ok,detail=''){checks[[length(checks)+1]]<<-data.frame(control=label,ok=all(ok),detalle=detail)}
eq<-function(x,y)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-10,check.attributes=FALSE))
add('Base y objeto de modelos idénticos en filas',identical(d$FILA_EXCEL,r$d$FILA_EXCEL))
add('Totales fuente',nrow(d)==23743&&sum(d$Y)==494)
add('Principal y casos completos',sum(d$POB_PRINCIPAL)==23542&&sum(d$Y[d$POB_PRINCIPAL])==428&&length(r$fits$Principal$filas)==23138&&sum(r$fits$Principal$firth$y)==418)
add('Pérdidas por covariables',all(r$flujo$N_elegible-r$flujo$N_modelo==r$flujo$N_excluido_covariables)&&all(r$flujo$Y1_elegible-r$flujo$Y1_modelo==r$flujo$Y1_excluido_covariables))
refs<-do.call(rbind,lapply(names(r$refs),function(v)data.frame(variable=v,referencia=r$refs[[v]],categorias=paste(levels(r$d[[v]]),collapse=' | '))))
freq<-read.csv(file.path(src,'estimaciones_frecuentistas.csv'));objects<-list()
for(p in names(r$fits)){
 f<-r$fits[[p]]$firth;m<-r$fits[[p]]$ml
 add(paste(p,'Firth y ML mismas covariables/respuesta'),identical(names(f$model),names(m$model))&&all(sapply(names(m$model),function(v)identical(as.character(f$model[[v]]),as.character(m$model[[v]]))))&&identical(as.numeric(f$y),as.numeric(m$y)))
 add(paste(p,'Firth penalización, perfil y convergencia'),isTRUE(f$firth)&&f$conflev==.95&&all(abs(f$conv)<=1e-7)&&all(is.finite(f$ci.lower))&&all(is.finite(f$ci.upper))&&max(f$pl.iter)<500)
 add(paste(p,'ML Bernoulli logit y convergencia'),m$family$family=='binomial'&&m$family$link=='logit'&&m$converged&&!m$boundary)
 for(method in c('Firth','ML_Wald')){
  z<-freq[freq$poblacion==p&freq$metodo==method,];if(!nrow(z)){print(unique(freq$metodo));stop('Nombre método inesperado')}
  if(method=='Firth'){b<-coef(f);lo<-exp(f$ci.lower);hi<-exp(f$ci.upper);pv<-f$prob}else{ss<-coef(summary(m));b<-coef(m);se<-ss[,2];lo<-exp(b-qnorm(.975)*se);hi<-exp(b+qnorm(.975)*se);pv<-ss[,4]}
  idx<-match(names(b),z$termino);add(paste(p,method,'OR IC p exportados'),!anyNA(idx)&&eq(z$OR[idx],exp(b))&&eq(z$LI95[idx],lo)&&eq(z$LS95[idx],hi)&&eq(z$p[idx],pv))
 }
}
ct<-r$descriptivos$categoricas;group<-interaction(ct$poblacion,ct$grupo,ct$variable,drop=TRUE)
add('Descriptivos: niveles y faltantes suman denominador',all(sapply(split(ct,group),function(x)sum(x$n)==unique(x$N_grupo))))
# All estimates already exported in Phase 2; no new fitted model.
hist<-read.xlsx(file.path(src,'06c_comparacion_antes_despues.xlsx'),sheet='referencia_historica')
hist$Y1<-hist$Eventos
before<-read.xlsx(file.path(src,'06c_comparacion_antes_despues.xlsx'),sheet='fuente_antes_despues')
b<-read.csv(file.path(src,'estimaciones_bayesianas.csv'));design<-readRDS(file.path(src,'datos_bayes_nuevos.rds'))
add('Bayes y Firth mismas filas',identical(design$filas,r$fits$Principal$filas)&&design$N==23138&&sum(design$y)==418)
for(s in c(.7,1.5,2.5)){
 label<-gsub('[.]','_',format(s,nsmall=1));fit<-readRDS(file.path(src,paste0('bayes_sd_',label,'.rds')));dr<-as_draws_matrix(fit$draws());tab<-b[b$prior_sd==s,]
 for(k in seq_along(design$terminos)){
  term<-design$terminos[k];v<-dr[,paste0('beta[',k,']')];if(term=='EDAD_ANALITICA_z'){term<-'EDAD_ANALITICA';v<-v/design$DE_edad};row<-tab[tab$termino==term,];q<-quantile(exp(v),c(.025,.975))
  add(paste('Bayes',s,term,'posterior reproducida'),nrow(row)==1&&eq(row$OR,median(exp(v)))&&eq(row$LI95,q[1])&&eq(row$LS95,q[2])&&eq(row$P_OR_mayor1,mean(v>0)))
 }
 rm(dr,fit);gc()
}
add('Diagnósticos Bayes y SCORE',all(read.csv(file.path(src,'diagnosticos_bayesianos.csv'))$convergencia)&&all(read.csv(file.path(src,'diagnosticos_bayes_SCORE.csv'))$convergencia))
# Add metadata to every row; do not infer a missing historical N or p.
common<-c('familia','poblacion','metodo','termino','OR','LI95','LS95','p','N','Y1','intervalo','comparabilidad')
make<-function(x,family,method,pop,N=NA,Y1=NA,interval,comp){data.frame(familia=family,poblacion=pop,metodo=method,termino=x$termino,OR=x$OR,LI95=x$LI95,LS95=x$LS95,p=if('p'%in%names(x))x$p else NA_real_,N=if('N'%in%names(x))x$N else N,Y1=if('Y1'%in%names(x))x$Y1 else Y1,intervalo=interval,comparabilidad=comp)}
long<-make(freq,'Corregido',freq$metodo,freq$poblacion,interval=ifelse(freq$metodo=='Firth','IC95 perfil penalizado','IC95 Wald'),comp='Misma fórmula; muestra idéntica Firth/ML dentro de población')
old<-data.frame(termino=before$termino,OR=before$OR_fuente_anterior,LI95=before$LI95_fuente_anterior,LS95=before$LS95_fuente_anterior)
long<-rbind(long,make(old,'Anterior reconstruido','Firth','Principal anterior',23137,417,'IC95 perfil penalizado','Mismas reglas: aisla cambios de fuente; un caso completo adicional en corregido'))
long<-rbind(long,make(hist,'Histórico exportado','Método histórico según exportación','Histórico global',interval='IC95 histórico; método pendiente de objeto original',comp='Incluye múltiples y distinta elegibilidad; NO atribuir delta exclusivamente a correcciones'))
long<-rbind(long,make(b,'Corregido',paste0('Bayes SD ',b$prior_sd),'Principal',interval='CrI95 cuantiles posteriores',comp='Mismas filas y predictores principal; penalización distinta; p no aplica'))
long$direccion<-ifelse(long$OR>1,'Positiva',ifelse(long$OR<1,'Inversa','Nula'))
long$intervalo_incluye_1<-long$LI95<=1&long$LS95>=1
# p for Bayesian rows is NA, never posterior directional probability.
wide<-reshape(long[,c('termino','familia','poblacion','metodo','OR','LI95','LS95','p','N','Y1')],idvar='termino',timevar=c('familia','poblacion','metodo'),direction='wide')
focus<-long[long$termino%in%c('DPPSI','RPMSI','ANTECEDENTE_CIRUGIA_UTERINASI','NACIONALIDADEXTRANJERA'),]
controls<-do.call(rbind,checks);write.csv(controls,file.path(out,'controles_auditoria.csv'),row.names=FALSE)
write.csv(long,file.path(out,'comparacion_completa.csv'),row.names=FALSE)
current<-long[long$familia=='Corregido' & long$termino!='(Intercept)',]
consistency<-do.call(rbind,lapply(split(current,current$termino),function(x)data.frame(termino=x$termino[1],OR_min=min(x$OR),OR_max=max(x$OR),direccion_consistente=length(unique(x$direccion))==1,intervalo_incluye_1_en=sum(x$intervalo_incluye_1),modelos=nrow(x),advertencia='Rango de estimaciones; muestras/estimadores distintos, no heterogeneidad formal ni causalidad')))
write.xlsx(list(consistencia_covariables=consistency,todos_modelos=long,comparacion_por_covariable=wide,cuatro_covariables=focus,fuente_antes_despues=before,referencias=refs,poblaciones=r$flujo,diagnosticos_Firth_ML=r$diagnosticos,diagnosticos_Bayes=read.csv(file.path(src,'diagnosticos_bayesianos.csv')),diagnosticos_SCORE=read.csv(file.path(src,'diagnosticos_bayes_SCORE.csv')),PPC=read.csv(file.path(src,'bayes_PPC.csv')),dominios=read.csv(file.path(src,'estimaciones_dominios.csv')),pares_covariables=read.csv(file.path(src,'estimaciones_pares_covariables.csv')),Bayes_SCORE=read.csv(file.path(src,'estimaciones_bayes_SCORE.csv')),controles=controls),file.path(out,'02_comparacion_modelos.xlsx'),overwrite=TRUE)
write_json(list(formula=paste(deparse(r$formula),collapse=' '),referencias=refs,controles=controls,diagnosticos=r$diagnosticos),file.path(out,'auditoria_objetos.json'),pretty=TRUE,dataframe='rows',auto_unbox=TRUE)
print(controls[!controls$ok,]);cat(nrow(controls),'controles;',sum(controls$ok),'satisfactorios\n');stopifnot(all(controls$ok))
