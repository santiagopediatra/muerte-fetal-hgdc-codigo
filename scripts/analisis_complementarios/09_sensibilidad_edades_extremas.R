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

# Sensibilidad a edades maternas extremas (<14 y >50 años) en el modelo principal (Firth)
# Reutiliza EXACTAMENTE base, población, referencias, fórmula y controles de
# scripts/reconstruccion_reanalisis/02_descriptivos_modelos.R (población 'Principal').
# Las edades no se recodifican: los eventos extremos se excluyen solo en este análisis.
# Escribe solo en private_data/analisis_complementarios/; las tablas revisadas se copian a results/.
suppressPackageStartupMessages(library(logistf))

proj <- root
src  <- hgdc_private_path(hgdc_results())
out  <- file.path(root, 'private_data', 'analisis_complementarios', 'edades_extremas'); dir.create(out, showWarnings = FALSE, recursive = TRUE); out <- hgdc_private_path(out)

# ---- Base y especificación (copiadas literalmente del script del modelo principal) ----
d <- readRDS(file.path(src, 'base_analitica_corregida.rds'))
stopifnot(isTRUE(attr(d, 'validacion')$aprobada), identical(attr(d, 'procedencia')$hoja, 'BASE_ORIGINAL'))
refs <- c(NACIONALIDAD='ECUATORIANA',PAREJA_ESTABLE='SI',INSTRUCCION='SECUNDARIA',ETNIA_MINORITARIA='NO',HIPERTENSIVOS='Ninguno',DIABETES_EN_EL_EMBARAZO='NO',IVU='NO',ANTECEDENTE_CIRUGIA_UTERINA='NO',RPM='NO',DPP='NO')
for (v in names(refs)) d[[v]] <- relevel(factor(d[[v]]), refs[[v]])
f <- Y~EDAD_ANALITICA+HIPERTENSIVOS+DIABETES_EN_EL_EMBARAZO+IVU+ANTECEDENTE_CIRUGIA_UTERINA+NACIONALIDAD+PAREJA_ESTABLE+INSTRUCCION+ETNIA_MINORITARIA+RPM+DPP
control   <- logistf.control(maxit=500,lconv=1e-7,gconv=1e-7,xconv=1e-7)
plcontrol <- logistpl.control(maxit=500)
z <- d[d$POB_PRINCIPAL & complete.cases(d[all.vars(f)]), ]
conv_ok <- function(m) all(abs(m$conv) <= c(control$lconv, control$gconv, control$xconv)) &&
  all(is.finite(m$ci.lower)) && all(is.finite(m$ci.upper)) && max(m$pl.iter) < plcontrol$maxit
fit <- function(data) logistf(f, data, control = control, plcontrol = plcontrol)
nf <- function(x) formatC(x, format = 'd', big.mark = '.', decimal.mark = ',')
f2 <- function(x) formatC(x, format = 'f', digits = 2, decimal.mark = ',')

# ---- Paso A: verificación del modelo principal ----
cat('Ajustando modelo principal...\n'); flush.console()
m0 <- fit(z)
dpp <- round(exp(c(coef(m0)['DPPSI'], m0$ci.lower['DPPSI'], m0$ci.upper['DPPSI'])), 2)
okA <- nrow(z) == 23138 && sum(z$Y) == 418 && all(dpp == c(6.86, 3.82, 11.53)) && conv_ok(m0)
verifA <- sprintf('Paso A: n = %s; muertes = %d; DPP ORa %s (%s–%s); convergente = %s -> %s',
                  nf(nrow(z)), sum(z$Y), f2(dpp[1]), f2(dpp[2]), f2(dpp[3]), conv_ok(m0), if (okA) 'VERIFICADO' else 'NO COINCIDE')
cat(verifA, '\n')
if (!okA) stop('El modelo principal no se reproduce: se detiene.')

# ---- Paso B: eventos con edad extrema en la muestra del modelo principal ----
lo <- z$EDAD_ANALITICA < 14; hi <- z$EDAD_ANALITICA > 50; ext <- lo | hi
pasoB <- data.frame(Grupo = c('Edad <14 años', 'Edad >50 años', 'Total edades extremas', 'Muestra restante'),
                    Eventos = nf(c(sum(lo), sum(hi), sum(ext), sum(!ext))),
                    Muertes_fetales_registradas = nf(c(sum(z$Y[lo]), sum(z$Y[hi]), sum(z$Y[ext]), sum(z$Y[!ext]))),
                    check.names = FALSE)

# ---- Paso C: mismo modelo sin edades extremas ----
z1 <- z[!ext, ]
# Ninguna categoría de referencia o comparación puede quedar vacía al excluir eventos
for (v in names(refs)) if (any(table(z1[[v]]) == 0)) stop('Categoría vacía tras excluir edades extremas en ', v)
cat('Ajustando modelo sin edades extremas (n =', nf(nrow(z1)), '; muertes =', sum(z1$Y), ')...\n'); flush.console()
m1 <- fit(z1)
if (!conv_ok(m1)) stop('El modelo sin edades extremas no converge.')

# ---- Paso D: tabla de las 17 covariables ----
covs <- data.frame(
  termino = c('EDAD_ANALITICA','HIPERTENSIVOSEclampsia','HIPERTENSIVOSHTA inducida por el embarazo','HIPERTENSIVOSHTA preexistente',
              'HIPERTENSIVOSPreeclampsia','DIABETES_EN_EL_EMBARAZOGestacional','DIABETES_EN_EL_EMBARAZOPreexistente','IVUSI',
              'ANTECEDENTE_CIRUGIA_UTERINASI','NACIONALIDADEXTRANJERA','PAREJA_ESTABLENO','INSTRUCCIONNINGUNA','INSTRUCCIONPRIMARIA',
              'INSTRUCCIONSUPERIOR','ETNIA_MINORITARIASI','RPMSI','DPPSI'),
  Covariable = c('Edad materna (por año)','Eclampsia','HTA inducida por embarazo','HTA preexistente','Preeclampsia','Diabetes gestacional',
                 'Diabetes preexistente','IVU registrada','Cirugía uterina previa','Nacionalidad extranjera','Sin pareja estable',
                 'Escolaridad: ninguna','Escolaridad: primaria','Escolaridad: superior','Etnia no mestiza registrada','RPM','DPP'),
  stringsAsFactors = FALSE)
stopifnot(setequal(covs$termino, setdiff(names(coef(m0)), '(Intercept)')), setequal(covs$termino, setdiff(names(coef(m1)), '(Intercept)')))
orci <- function(m, t) paste0(f2(exp(coef(m)[t])), ' (', f2(exp(m$ci.lower[t])), '–', f2(exp(m$ci.upper[t])), ')')
b0 <- coef(m0)[covs$termino]; b1 <- coef(m1)[covs$termino]
cambio <- 100 * (b1 - b0) / b0
tabla <- data.frame(Covariable = covs$Covariable,
                    'ORa (IC95 %) modelo principal' = orci(m0, covs$termino),
                    'ORa (IC95 %) sin edades extremas' = orci(m1, covs$termino),
                    'Cambio relativo log-OR (%)' = paste0(formatC(cambio, format = 'f', digits = 1, decimal.mark = ','), ' %'),
                    check.names = FALSE, row.names = NULL)

# ---- Paso E: exportación ----
write.csv(tabla, file.path(out, 'edades_extremas_ORa.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(pasoB, file.path(out, 'edades_extremas_conteos.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
capture.output(sessionInfo(), file = file.path(out, 'sessionInfo_edades_extremas.txt'))

options(width = 200)
cat('\n===== Verificación (paso A) =====\n', verifA, '\n', sep = '')
cat('\n===== Paso B: edades extremas en la muestra del modelo principal =====\n'); print(pasoB, row.names = FALSE, right = FALSE)
cat(sprintf('\nModelo sin edades extremas: n = %s; muertes = %d; convergente = %s\n', nf(nrow(z1)), sum(z1$Y), conv_ok(m1)))
cat('\n===== Paso D: ORa con y sin edades extremas =====\n'); print(tabla, row.names = FALSE, right = FALSE)
s <- covs$Covariable[abs(cambio) > 10]
cat('\nCambio >10 % en log-OR:', if (length(s)) paste(s, collapse = ', ') else 'ninguna', '\n')
cat('Nota: el cambio relativo en log-OR se inestabiliza cuando la log-OR del modelo principal es cercana a 0 (OR ≈ 1).\n')
cat('Salida:', out, '\n')
