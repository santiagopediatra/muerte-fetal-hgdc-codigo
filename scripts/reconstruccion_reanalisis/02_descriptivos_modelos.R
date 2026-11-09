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


suppressPackageStartupMessages({library(openxlsx);library(logistf);library(lpSolve);library(ggplot2)})
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else hgdc_results()
out <- hgdc_private_path(out)
d<-readRDS(file.path(out,'base_analitica_corregida.rds'));stopifnot(isTRUE(attr(d,'validacion')$aprobada),identical(attr(d,'procedencia')$hoja,'BASE_ORIGINAL'))
stopifnot(!file.exists(file.path(out,'modelos_nuevos.rds')))
refs<-c(NACIONALIDAD='ECUATORIANA',PAREJA_ESTABLE='SI',INSTRUCCION='SECUNDARIA',ETNIA_MINORITARIA='NO',HIPERTENSIVOS='Ninguno',DIABETES_EN_EL_EMBARAZO='NO',IVU='NO',ANTECEDENTE_CIRUGIA_UTERINA='NO',RPM='NO',DPP='NO')
for(v in names(refs))d[[v]]<-relevel(factor(d[[v]]),refs[[v]])
# Original specification restricted to single gestations: multiplicity is constant and removed.
f<-Y~EDAD_ANALITICA+HIPERTENSIVOS+DIABETES_EN_EL_EMBARAZO+IVU+ANTECEDENTE_CIRUGIA_UTERINA+NACIONALIDAD+PAREJA_ESTABLE+INSTRUCCION+ETNIA_MINORITARIA+RPM+DPP
pop<-list(Principal=d$POB_PRINCIPAL,A_original_unicas=d$POB_A,B_tardia=d$POB_B,C_todas_indeterminadas=d$POB_PRINCIPAL|d$POB_INDETERMINADA)
description_pop<-c(pop,list(Todos_eventos=rep(TRUE,nrow(d)),Multiples=!is.na(d$EMBARAZO_MULTIPLE_ANALITICO)&d$EMBARAZO_MULTIPLE_ANALITICO==1,Multiplicidad_indeterminada=is.na(d$EMBARAZO_MULTIPLE_ANALITICO)))
catvars<-c('EDAD_CATEGORIA','NACIONALIDAD','PAREJA_ESTABLE','ESTADO_CIVIL','INSTRUCCION','ETNIA_MINORITARIA','GRUPO_CULTURAL','HIPERTENSIVOS','DIABETES_EN_EL_EMBARAZO','IVU','ANTECEDENTE_CIRUGIA_UTERINA','OBESIDAD','ANEMIA','VIH','RPM','DPP','PRESENTACION_FETAL','SUFRIMIENTO_FETAL','TIPO_PARTO','RIESGO_AL_INGRESO','ACOMPANAMIENTO','ACOMPANADA','SEGURO_ANALITICO','PSICOPROFILAXIS_ANALITICA','EG_CAT','EG_ESTADO','ESTADO_PESO_1','EMBARAZO_MULTIPLE_ANALITICO','ATENCION_CAUSAS_FETALES','OTRAS_PATOLOGIAS','ANOMALIAS_GENITALES','ALTERACION_PLACENTA','ISOINMUNIZACION','INFECCION_GENITAL','TORCH','ITS','TRASTRONOS_DE_LIQUIDO_AMNIOTICO','RUPTURA_UTERINA','ALTERACIONES_DEL_CORDON','PARTO_PROLONGADO','PARTO_PRECIPITADO','OBSTRUCCION_DEL_PARTO','HEMORRAGIA')
catvars<-intersect(catvars,names(d));contvars<-c('EDAD_ANALITICA','EDAD_GESTACIONAL_SEMANAS','PESO_1_ANALITICO','NO_CONTROLES_NUM','SESIONES_NUM','SCORE_ANALITICO')
ct<-list();cn<-list();annual<-list();totals<-list()
for(n in names(description_pop)){p<-description_pop[[n]];nall<-sum(p);bt<-binom.test(sum(d$Y[p]),nall);totals[[n]]<-data.frame(poblacion=n,N=nall,Y1=sum(d$Y[p]),Y0=sum(d$Y[p]==0),proporcion=sum(d$Y[p])/nall,LI95=bt$conf.int[1],LS95=bt$conf.int[2])
 for(g in c('Todos','Sin muerte registrada','Muerte fetal registrada')){q<-p&if(g=='Todos')rep(TRUE,nrow(d)) else d$Y==as.integer(g=='Muerte fetal registrada');ng<-sum(q)
 for(v in catvars){x<-as.character(d[[v]][q]);nv<-sum(!is.na(x));levels<-sort(unique(x[!is.na(x)]));if(!length(levels))levels<-character();for(l in levels)ct[[length(ct)+1]]<-data.frame(poblacion=n,grupo=g,variable=v,nivel=l,N_grupo=ng,N_valido=nv,faltantes=ng-nv,n=sum(x==l,na.rm=TRUE),porcentaje_valido=100*sum(x==l,na.rm=TRUE)/nv,porcentaje_total=if(ng)100*sum(x==l,na.rm=TRUE)/ng else NA)
 ct[[length(ct)+1]]<-data.frame(poblacion=n,grupo=g,variable=v,nivel='[FALTANTE]',N_grupo=ng,N_valido=nv,faltantes=ng-nv,n=ng-nv,porcentaje_valido=NA,porcentaje_total=if(ng)100*(ng-nv)/ng else NA)
 }
 for(v in contvars){x<-d[[v]][q];x<-x[!is.na(x)];qs<-if(length(x))quantile(x,c(.25,.5,.75),names=FALSE) else rep(NA,3);cn[[length(cn)+1]]<-data.frame(poblacion=n,grupo=g,variable=v,N_grupo=ng,N_valido=length(x),faltantes=ng-length(x),media=if(length(x))mean(x) else NA,DE=if(length(x)>1)sd(x) else NA,P25=qs[1],mediana=qs[2],P75=qs[3],minimo=if(length(x))min(x) else NA,maximo=if(length(x))max(x) else NA)}
 }
 years<-ifelse(is.na(d$ANIO_FECHA_INGRESO),'NO_INTERPRETABLE',as.character(d$ANIO_FECHA_INGRESO));for(a in sort(unique(years[p]))){q<-p&years==a;annual[[length(annual)+1]]<-data.frame(poblacion=n,ano_ingreso=a,N=sum(q),Y1=sum(d$Y[q]),Y0=sum(d$Y[q]==0),EG_faltante=sum(is.na(d$EDAD_GESTACIONAL_SEMANAS[q])),peso_faltante=sum(is.na(d$PESO_1_ANALITICO[q])),ambos_faltantes=sum(is.na(d$EDAD_GESTACIONAL_SEMANAS[q])&is.na(d$PESO_1_ANALITICO[q])),proporcion=sum(d$Y[q])/sum(q))}
}
cat_table<-do.call(rbind,ct);cont_table<-do.call(rbind,cn);annual<-do.call(rbind,annual);totals<-do.call(rbind,totals)
stopifnot(all(aggregate(N~poblacion,annual,sum)$N==totals$N[match(aggregate(N~poblacion,annual,sum)$poblacion,totals$poblacion)]))
write.xlsx(list(categoricas=cat_table,continuas=cont_table,resumen=totals,anual=annual),file.path(out,'04_tabla_descriptiva.xlsx'),overwrite=FALSE)
write.csv(cat_table,file.path(out,'descriptivos_categoricos.csv'),row.names=FALSE);write.csv(cont_table,file.path(out,'descriptivos_continuos.csv'),row.names=FALSE);write.csv(annual,file.path(out,'distribucion_anual.csv'),row.names=FALSE)
flow<-list();missing<-list();cells<-list();diag<-list();fits<-list();est<-list();sample<-list()
extract<-function(m,n,z,type){b<-coef(m);se<-sqrt(diag(vcov(m)));if(type=='Firth'){lo<-m$ci.lower;hi<-m$ci.upper;p<-m$prob}else{lo<-b-qnorm(.975)*se;hi<-b+qnorm(.975)*se;p<-coef(summary(m))[,4]};data.frame(poblacion=n,metodo=type,termino=names(b),beta=b,SE=se,OR=exp(b),LI95=exp(lo),LS95=exp(hi),p=p,N=nrow(z),Y1=sum(z$Y),row.names=NULL)}
control<-logistf.control(maxit=500,lconv=1e-7,gconv=1e-7,xconv=1e-7);plcontrol<-logistpl.control(maxit=500)
for(n in names(pop)){eligible<-pop[[n]];cc<-complete.cases(d[all.vars(f)]);q<-eligible&cc;z<-droplevels(d[q,]);
 # Keep the same factor definitions and references across populations; dropped levels require review.
 z<-d[q,]
 flow[[n]]<-data.frame(poblacion=n,N_elegible=sum(eligible),Y1_elegible=sum(d$Y[eligible]),N_excluido_covariables=sum(eligible&!cc),Y1_excluido_covariables=sum(d$Y[eligible&!cc]),N_modelo=nrow(z),Y1_modelo=sum(z$Y))
 sample[[n]]<-data.frame(poblacion=n,fila_excel=z$FILA_EXCEL,Y=z$Y)
 missing[[n]]<-do.call(rbind,lapply(all.vars(f)[-1],function(v){x<-eligible&is.na(d[[v]]);data.frame(poblacion=n,variable=v,N_faltante=sum(x),Y1_faltante=sum(d$Y[x]))}))
 cells[[n]]<-do.call(rbind,lapply(names(refs),function(v){t<-table(z[[v]],factor(z$Y,levels=0:1));data.frame(poblacion=n,variable=v,nivel=rownames(t),Y0=t[,1],Y1=t[,2],cero=apply(t,1,min)==0,menor5=apply(t,1,min)<5)}))
 X<-model.matrix(f,z);rank<-qr(X)$rank;stopifnot(rank==ncol(X),length(unique(z$Y))==2)
 A<-X*(2*z$Y-1);sep<-lp('min',rep(0,2*ncol(X)),rbind(cbind(A,-A),c(colSums(A),-colSums(A))),c(rep('>=',nrow(A)),'>='),c(rep(0,nrow(A)),1))
 warnings<-character();cat('Ajustando',n,'N',nrow(z),'Y1',sum(z$Y),'\n');flush.console()
 fm<-withCallingHandlers(logistf(f,z,control=control,plcontrol=plcontrol),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
 ml<-withCallingHandlers(glm(f,z,family=binomial(),na.action=na.fail),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
 convergence<-all(abs(fm$conv)<=c(control$lconv,control$gconv,control$xconv))
 profile_ok<-all(is.finite(fm$ci.lower))&&all(is.finite(fm$ci.upper))&&max(fm$pl.iter)<plcontrol$maxit&&max(abs(fm$pl.conv[,c(1,3)]))<plcontrol$lconv&&max(abs(fm$pl.conv[,c(2,4)]))<plcontrol$xconv
 scaled<-X;scaled[,'EDAD_ANALITICA']<-as.numeric(scale(scaled[,'EDAD_ANALITICA']))
 gv<-tryCatch(as.data.frame(car::vif(ml)),error=function(e)data.frame(error=conditionMessage(e)))
 write.csv(gv,file.path(out,paste0('colinealidad_',n,'.csv')))
 diag[[n]]<-data.frame(poblacion=n,N=nrow(z),Y1=sum(z$Y),parametros=ncol(X),rango=rank,condicion_edad_escalada=kappa(scaled,exact=TRUE),eventos_por_pendiente=sum(z$Y)/(ncol(X)-1),separacion=if(sep$status%in%c(0,2))sep$status==0 else NA,separacion_lp_status=sep$status,Firth_convergente=convergence,Firth_perfiles_convergentes=profile_ok,Firth_coef_finitos=all(is.finite(coef(fm))),ML_convergente=ml$converged,ML_coef_finitos=all(is.finite(coef(ml))),avisos=paste(unique(warnings),collapse=' | '))
 fits[[n]]<-list(firth=fm,ml=ml,formula=f,filas=z$FILA_EXCEL)
 est[[paste0(n,'_Firth')]]<-extract(fm,n,z,'Firth');est[[paste0(n,'_ML')]]<-extract(ml,n,z,'ML_Wald')
 capture.output(list(formula=f,control=control,plcontrol=plcontrol,conv=fm$conv,pl_conv=fm$pl.conv,pl_iter=fm$pl.iter,diagnostico=diag[[n]]),file=file.path(out,paste0('diagnostico_',n,'.txt')))
 cat('Finalizado',n,'Firth',convergence,'perfiles',profile_ok,'\n');flush.console()
}
est<-do.call(rbind,est);flow<-do.call(rbind,flow);diag<-do.call(rbind,diag);cells<-do.call(rbind,cells);missing<-do.call(rbind,missing);sample<-do.call(rbind,sample)
stopifnot(all(flow$N_elegible==flow$N_modelo+flow$N_excluido_covariables),all(flow$Y1_elegible==flow$Y1_modelo+flow$Y1_excluido_covariables))
passed<-all(diag$Firth_convergente&diag$Firth_perfiles_convergentes&diag$Firth_coef_finitos)
saveRDS(list(fits=fits,d=d,poblaciones=pop,formula=f,refs=refs,descriptivos=list(categoricas=cat_table,continuas=cont_table,anual=annual,totales=totals),estimaciones=est,flujo=flow,diagnosticos=diag,celdas=cells,faltantes=missing,filas=sample,modelos_verificados=passed),file.path(out,'modelos_nuevos.rds'))
write.xlsx(list(principal=est[est$poblacion=='Principal',],todas_estimaciones=est,diagnosticos=diag,flujo=flow,celdas=cells,faltantes=missing,referencias=data.frame(variable=names(refs),referencia=unname(refs))),file.path(out,'05_modelos_principales.xlsx'),overwrite=FALSE)
focus<-grepl('^DPP|^NACIONALIDAD|^RPM|^ANTECEDENTE_CIRUGIA_UTERINA',est$termino)&est$metodo=='Firth'
write.xlsx(list(comparacion=est[focus,],todos=est[est$metodo=='Firth',],flujo=flow,escenarios=data.frame(escenario=c('Principal/C ninguno','C todos indeterminados','A únicas originales','B tardía'),regla=c('Indeterminados excluidos simétricamente','Todos indeterminados únicos considerados elegibles;no se inventan EG/pesos','Todas únicas, incluidos indeterminados y conocidos bajo umbral','EG>=28;si faltaEG,peso>=1000'))),file.path(out,'06_analisis_sensibilidad.xlsx'),overwrite=FALSE)
write.xlsx(list(flujo=flow,poblaciones=totals,filas=sample,faltantes_covariables=missing),file.path(out,'03_flujo_poblaciones.xlsx'),overwrite=FALSE)
write.xlsx(list(multiples_categoricas=cat_table[cat_table$poblacion=='Multiples',],multiples_continuas=cont_table[cont_table$poblacion=='Multiples',],anual=annual,diagnosticos_pequenos=cells,faltantes_covariables=missing),file.path(out,'08_tablas_suplementarias.xlsx'),overwrite=FALSE)
write.csv(est,file.path(out,'estimaciones_frecuentistas.csv'),row.names=FALSE);write.csv(diag,file.path(out,'diagnosticos_frecuentistas.csv'),row.names=FALSE);write.csv(flow,file.path(out,'flujo_modelos.csv'),row.names=FALSE)
writeLines(c('# Diagnósticos de modelos',paste('Control global Firth/perfiles:',passed),'Fórmula original conservada salvo multiplicidad, constante al restringir a únicas. EDAD por año; factores con referencias originales. Casos completos específicos de cada población. ML usa exactamente la misma fórmula, filas y desenlace que Firth. Sus IC sonWald y requieren cautela si hay separación/celdas escasas.','RPM/DPP se interpretan como asociaciones con diagnósticos registrados; temporalidad respecto del fallecimiento no acreditada. No se atribuye protección causal aRPM/cirugía. No se seleccionaron covariables porp.','C incluye simétricamente todos los eventos únicos de elegibilidad desconocida, sin asignarEG ni peso; escenarios extremos por desenlace no se modelan. A/B/C son poblaciones hospitalarias, no tasas poblacionales.','Imputación múltiple no ejecutada: no hay justificación documentada del mecanismo de ausencia diferencial ni de un modelo predictor válido; no se asumeMAR.','',capture.output(print(diag,row.names=FALSE)),'',capture.output(print(flow,row.names=FALSE))),file.path(out,'07_diagnosticos_modelos.md'))
dir.create(file.path(out,'09_figuras'),showWarnings=FALSE)
pp<-est[focus,];pp$variable<-sub('SI$|EXTRANJERA$','',pp$termino)
g<-ggplot(pp,aes(x=OR,y=variable,color=poblacion))+geom_vline(xintercept=1,linetype=2)+geom_point(position=position_dodge(width=.6))+geom_errorbar(aes(xmin=LI95,xmax=LS95),orientation='y',width=.12,position=position_dodge(width=.6))+scale_x_log10()+labs(x='OR ajustada (IC95% penalizado)',y=NULL,color='Población')+theme_minimal(base_size=12)
ggsave(file.path(out,'09_figuras/comparacion_OR.png'),g,width=10,height=5,dpi=300,bg='white');ggsave(file.path(out,'09_figuras/comparacion_OR.pdf'),g,width=10,height=5)
for(n in names(pop)){a<-flow[flow$poblacion==n,];ind<-sum(d$POB_INDETERMINADA);s<-paste0('flowchart TD\n  T["23.743 eventos"] --> U["23.666 gestaciones únicas"]\n  T --> M["70 múltiples y7 indeterminadas"]\n  U --> E["',n,': ',a$N_elegible,' elegibles; ',a$Y1_elegible,' muertes registradas"]\n  E --> C["',a$N_modelo,' casos completos; ',a$Y1_modelo,' muertes"]\n  E --> X["',a$N_excluido_covariables,' excluidos por covariables incompletas"]\n');writeLines(s,file.path(out,'09_figuras',paste0('flujo_',n,'.mmd')))}
capture.output(sessionInfo(),file=file.path(out,'sessionInfo_modelos.txt'))
print(flow);print(diag);print(est[focus,c('poblacion','termino','OR','LI95','LS95','N','Y1')])
if(!passed)stop('ConvergenciaFirth/perfiles no verificada; detener reporte inferencial definitivo ybayes.')
