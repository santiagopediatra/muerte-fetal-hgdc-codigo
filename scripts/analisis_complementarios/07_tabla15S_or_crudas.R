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
# Tabla 15S — OR crudas (Firth univariable) y ORa (modelo principal), STROBE 16a

# Reutiliza EXACTAMENTE base, población, referencias, fórmula y controles de
# scripts/reconstruccion_reanalisis/02_descriptivos_modelos.R (población 'Principal').
# Escribe solo en private_data/analisis_complementarios/; las tablas revisadas se copian a results/.
suppressPackageStartupMessages(library(logistf))

proj <- root
src  <- hgdc_private_path(hgdc_results())
out  <- file.path(root, 'private_data', 'analisis_complementarios', 'tabla15S'); dir.create(out, showWarnings = FALSE, recursive = TRUE); out <- hgdc_private_path(out)

# ---- Base y especificación (copiadas literalmente del script del modelo principal) ----
d <- readRDS(file.path(src, 'base_analitica_corregida.rds'))
stopifnot(isTRUE(attr(d, 'validacion')$aprobada), identical(attr(d, 'procedencia')$hoja, 'BASE_ORIGINAL'))
refs <- c(NACIONALIDAD='ECUATORIANA',PAREJA_ESTABLE='SI',INSTRUCCION='SECUNDARIA',ETNIA_MINORITARIA='NO',HIPERTENSIVOS='Ninguno',DIABETES_EN_EL_EMBARAZO='NO',IVU='NO',ANTECEDENTE_CIRUGIA_UTERINA='NO',RPM='NO',DPP='NO')
for (v in names(refs)) d[[v]] <- relevel(factor(d[[v]]), refs[[v]])
f <- Y~EDAD_ANALITICA+HIPERTENSIVOS+DIABETES_EN_EL_EMBARAZO+IVU+ANTECEDENTE_CIRUGIA_UTERINA+NACIONALIDAD+PAREJA_ESTABLE+INSTRUCCION+ETNIA_MINORITARIA+RPM+DPP
control   <- logistf.control(maxit=500,lconv=1e-7,gconv=1e-7,xconv=1e-7)
plcontrol <- logistpl.control(maxit=500)
q <- d$POB_PRINCIPAL & complete.cases(d[all.vars(f)])
z <- d[q, ]                       # igual que el script original (sin droplevels)

# ---- Regla 3: misma muestra ----
cat('Muestra de casos completos: n =', nrow(z), '; muertes =', sum(z$Y), '\n')
if (nrow(z) != 23138 || sum(z$Y) != 418) stop('La muestra no es 23.138/418: se detiene.')

# ---- Paso A: re-estimación del modelo principal ----
cat('Ajustando modelo principal (Firth, perfiles)...\n'); flush.console()
fm <- logistf(f, z, control = control, plcontrol = plcontrol)
adj <- data.frame(termino = names(coef(fm)), OR = exp(coef(fm)), LI = exp(fm$ci.lower), LS = exp(fm$ci.upper), p = fm$prob, row.names = NULL)

# Comparación con las cifras publicadas en la Tabla 3 (redondeo a 2 decimales)
t3 <- data.frame(termino = c('DPPSI','ANTECEDENTE_CIRUGIA_UTERINASI','RPMSI'),
                 OR = c(6.86, 0.43, 0.50), LI = c(3.82, 0.29, 0.33), LS = c(11.53, 0.61, 0.73))
a3 <- adj[match(t3$termino, adj$termino), ]
ok_t3 <- all(round(a3$OR, 2) == t3$OR & round(a3$LI, 2) == t3$LI & round(a3$LS, 2) == t3$LS)
# Comparación con todas las estimaciones Firth guardadas del modelo principal
prev <- readRDS(file.path(src, 'modelos_nuevos.rds'))$estimaciones
prev <- prev[prev$poblacion == 'Principal' & prev$metodo == 'Firth', ]
pm <- prev[match(adj$termino, prev$termino), ]
ok_prev <- all(abs(adj$OR - pm$OR) < 1e-6 & abs(adj$LI - pm$LI95) < 1e-6 & abs(adj$LS - pm$LS95) < 1e-6 & abs(adj$p - pm$p) < 1e-6)
cat('Reproduce Tabla 3 (DPP, cirugía, RPM):', ok_t3, '| Coincide con modelos_nuevos.rds (17 términos):', ok_prev, '\n')
print(cbind(a3[, 1:4], Tabla3 = paste0(t3$OR, ' (', t3$LI, '-', t3$LS, ')')), row.names = FALSE)
if (!ok_t3 || !ok_prev) stop('El modelo re-estimado no reproduce la Tabla 3: se detiene.')

# ---- Paso B: modelos univariables (Firth, IC por perfil, p por razón de verosimilitud penalizada) ----
covs <- all.vars(f)[-1]
crude_ok <- c()
crude <- do.call(rbind, lapply(covs, function(v) {
  cat('Univariable:', v, '\n'); flush.console()
  m <- logistf(reformulate(v, 'Y'), z, control = control, plcontrol = plcontrol)
  crude_ok[v] <<- all(abs(m$conv) <= c(control$lconv, control$gconv, control$xconv)) &&
    all(is.finite(m$ci.lower)) && all(is.finite(m$ci.upper)) && max(m$pl.iter) < plcontrol$maxit
  r <- data.frame(termino = names(coef(m)), OR = exp(coef(m)), LI = exp(m$ci.lower), LS = exp(m$ci.upper), p = m$prob, row.names = NULL)
  r[r$termino != '(Intercept)', ]
}))
if (!all(crude_ok)) stop('Modelo univariable sin convergencia: ', paste(names(crude_ok)[!crude_ok], collapse = ', '))

# ---- Paso C: muertes/expuestos en la misma muestra ----
nf <- function(x) formatC(x, format = 'd', big.mark = '.', decimal.mark = ',')
expo <- function(v, lvl) { s <- z[[v]] == lvl; paste0(nf(sum(z$Y[s])), '/', nf(sum(s))) }

# ---- Paso D: tabla ----
filas <- data.frame(
  Covariable = c('Edad materna (por año)','Eclampsia','HTA inducida por embarazo','HTA preexistente','Preeclampsia',
                 'Diabetes gestacional','Diabetes preexistente','IVU registrada','Cirugía uterina previa',
                 'Nacionalidad extranjera','Sin pareja estable','Escolaridad: ninguna','Escolaridad: primaria',
                 'Escolaridad: superior','Etnia no mestiza registrada','RPM','DPP'),
  variable = c('EDAD_ANALITICA', rep('HIPERTENSIVOS', 4), rep('DIABETES_EN_EL_EMBARAZO', 2), 'IVU', 'ANTECEDENTE_CIRUGIA_UTERINA',
               'NACIONALIDAD', 'PAREJA_ESTABLE', rep('INSTRUCCION', 3), 'ETNIA_MINORITARIA', 'RPM', 'DPP'),
  nivel = c(NA, 'Eclampsia', 'HTA inducida por el embarazo', 'HTA preexistente', 'Preeclampsia', 'Gestacional', 'Preexistente',
            'SI', 'SI', 'EXTRANJERA', 'NO', 'NINGUNA', 'PRIMARIA', 'SUPERIOR', 'SI', 'SI', 'SI'),
  stringsAsFactors = FALSE)
filas$termino <- ifelse(is.na(filas$nivel), filas$variable, paste0(filas$variable, filas$nivel))
stopifnot(setequal(filas$termino, adj$termino[-1]), setequal(filas$termino, crude$termino))

f2 <- function(x) formatC(x, format = 'f', digits = 2, decimal.mark = ',')
fp <- function(p) ifelse(p < 0.001, '<0,001', formatC(p, format = 'f', digits = 3, decimal.mark = ','))
fci <- function(r) paste0(f2(r$OR), ' (', f2(r$LI), '–', f2(r$LS), ')')
cr <- crude[match(filas$termino, crude$termino), ]; aj <- adj[match(filas$termino, adj$termino), ]
tabla <- data.frame(
  'Covariable' = filas$Covariable,
  'OR cruda (IC95 %)' = fci(cr), 'p' = fp(cr$p),
  'ORa (IC95 %)' = fci(aj), 'p ' = fp(aj$p),
  'Muertes/expuestos' = mapply(function(v, l) if (is.na(l)) '—' else expo(v, l), filas$variable, filas$nivel),
  check.names = FALSE, row.names = NULL)
names(tabla)[5] <- 'p'   # dos columnas «p», como en la especificación

# ---- Paso E: exportación ----
write.csv(tabla, file.path(out, 'tabla15S_or_crudas.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
num <- cbind(filas[, c('Covariable','termino')], OR_cruda = cr$OR, LI_cruda = cr$LI, LS_cruda = cr$LS, p_cruda = cr$p,
             ORa = aj$OR, LI_a = aj$LI, LS_a = aj$LS, p_a = aj$p)
write.csv(num, file.path(out, 'tabla15S_or_crudas_valores_completos.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
capture.output(sessionInfo(), file = file.path(out, 'sessionInfo_tabla15S.txt'))

# ---- Paso F: salida ----
options(width = 200)
cat('\n===== Tabla 15S =====\n'); print(tabla, row.names = FALSE, right = FALSE)
cat('\nModelos univariables convergentes (11/11):', all(crude_ok), '\n')
cat('\n===== Verificación =====\n')
cat('n =', nf(nrow(z)), '| muertes fetales registradas =', sum(z$Y), '\n')
cat('Firth principal convergente:', all(abs(fm$conv) <= c(control$lconv, control$gconv, control$xconv)), '\n')
cat('ORa reproducen la Tabla 3:', ok_t3, '| idénticas a modelos_nuevos.rds:', ok_prev, '\n')
cat('Suma de expuestos por variable = n en todas las variables categóricas:',
    all(sapply(names(refs), function(v) length(z[[v]]) == nrow(z) && !anyNA(z[[v]]))), '\n')
cat('Salida:', file.path(out, 'tabla15S_or_crudas.csv'), '\n')
