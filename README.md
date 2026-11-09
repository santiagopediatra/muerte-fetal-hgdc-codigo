# Muerte fetal registrada en gestaciones únicas — HGDC, Quito, 2018–2023

Código de análisis y resultados agregados del estudio *Características asociadas con muerte fetal registrada en gestaciones únicas del Hospital General Docente Calderón, Quito, Ecuador, 2018–2023*.

> **Este repositorio no contiene datos.**
> - La base de datos es propiedad del Hospital General Docente Calderón (HGDC) y no se distribuye en ningún formato.
> - Acceder a ella requiere la autorización del HGDC. Sin esa autorización no pueden ejecutarse los análisis.
> - Sin acceso, solo pueden consultarse el código, la documentación y los resultados agregados de `results/`.

## Objetivo y diseño

- **Objetivo:** evaluar asociaciones entre características maternas, sociodemográficas y diagnósticos registrados y la muerte fetal registrada en gestaciones únicas atendidas en el HGDC.
- **Diseño:** estudio observacional analítico, transversal, sobre registros hospitalarios anonimizados. Período 2018–2023.
- **Unidad de análisis:** evento obstétrico, que corresponde a una fila de la base institucional.
- **Desenlace:** muerte fetal registrada, según el campo institucional `MUERTE INTRAUTERINA`. No hubo adjudicación clínica independiente.
- **Población principal:** gestaciones únicas con edad gestacional (EG) válida ≥ 22 semanas o, solo si falta la EG, peso fetal ≥ 500 g.
  - Elegibles: 23.542 eventos, con 428 muertes.
  - Modelo ajustado: 23.138 casos completos, con 418 muertes.
- **Métodos:**
  - Modelo principal: regresión logística penalizada de Firth (`logistf`).
  - Comparadores: logística convencional y modelos bayesianos (CmdStan) con tres distribuciones previas.
  - Análisis de sensibilidad: criterios de elegibilidad, modelos por dominio, forma funcional de la edad (splines naturales) y edades extremas.
  - OR crudas (STROBE 16a).
- **Interpretación:** las asociaciones no son efectos causales. Véase [docs/metodologia.md](docs/metodologia.md).

## Estructura

```
config/                       project.json (marcador de raíz y rutas relativas) y versiones del entorno
docs/                         metodología, estructura de entrada, reproducibilidad, divulgación, Zenodo
renv.lock                     dependencias R del entorno original (R 4.5.0); sin modificar
scripts/
  verificacion/               rutas, auditoría del paquete, comprobaciones y restauración del entorno
  reconstruccion_reanalisis/  01–06b: reconstrucción de la base y análisis, en orden de ejecución
  revision_cientifica_final/  comprobaciones de los modelos guardados
  analisis_complementarios/   07 OR crudas (Tabla 15S); 08 splines de edad; 09 edades extremas
results/
  tablas/principales/         tablas agregadas; correspondencia con el manuscrito vigente en results/README.md
  tablas/complementarias/     Tabla 15S y sensibilidades de la edad
  figuras/                    flujo de la población principal y OR por población (salidas de 06/06b)
  informes/                   resumen de resultados definitivos y revisión STROBE
private_data/                 carpeta local para datos autorizados; excluida de Git
```

## Requisitos

- R 4.5.0 y renv 1.1.2. Las versiones de los paquetes están en `renv.lock`.
- CmdStan 2.39.0, solo para los modelos bayesianos (scripts 03 y 05).
- Python 3 con la biblioteca estándar, solo para `scripts/verificacion/auditar_paquete.py`.

## Ejecución

Ejecute todos los comandos desde la raíz del repositorio. Los scripts:

- localizan la raíz mediante `config/project.json`;
- usan solo rutas relativas;
- escriben exclusivamente dentro de `private_data/`.

**1. Comprobaciones sin datos**

```bash
python3 scripts/verificacion/auditar_paquete.py   # lista cerrada, huellas y patrones sensibles
Rscript scripts/verificacion/comprobar_rutas.R    # raíz y sintaxis de todos los scripts R
Rscript scripts/verificacion/comprobar_dependencias.R
```

**2. Restaurar el entorno R**

```bash
HGDC_INSTALL_DEPENDENCIES=YES Rscript scripts/verificacion/restaurar_entorno.R
```

**3. Colocar los datos.** Solo con autorización. La estructura requerida está en [docs/estructura_datos_entrada.md](docs/estructura_datos_entrada.md).

```
private_data/fuentes/BASE_OBITO_MAESTRA.xlsx     # fuente corregida, hoja BASE_ORIGINAL
private_data/fuentes/BASE_OBITO_HISTORICA.xlsx   # versión anterior, solo para controles de comparación
```

**4. Análisis, en este orden.** Los scripts no se ejecutan salvo que se habilite expresamente el flujo privado.

```bash
export HGDC_PRIVATE_WORKFLOW=YES
HGDC_RECONSTRUCTION_APPROVED=YES Rscript scripts/reconstruccion_reanalisis/01_reconstruir_validar.R
Rscript scripts/reconstruccion_reanalisis/02_descriptivos_modelos.R   # descriptivos; Firth y ML en 4 poblaciones
Rscript scripts/reconstruccion_reanalisis/03_bayes_priors.R           # Bayes con 3 priors (CmdStan)
Rscript scripts/reconstruccion_reanalisis/04_secundarios.R            # modelos por dominio y pares de covariables
Rscript scripts/reconstruccion_reanalisis/05_bayes_score.R            # Bayes sin y con Score Mamá (CmdStan)
Rscript scripts/reconstruccion_reanalisis/06_comparacion_control_figuras.R
Rscript scripts/reconstruccion_reanalisis/06b_render_figuras.R
Rscript scripts/revision_cientifica_final/01_auditar.R
Rscript scripts/revision_cientifica_final/03_comprobar_secundarios.R
Rscript scripts/analisis_complementarios/07_tabla15S_or_crudas.R
Rscript scripts/analisis_complementarios/08_sensibilidad_edad_spline.R
Rscript scripts/analisis_complementarios/09_sensibilidad_edades_extremas.R
```

`02b_recuperar_exportacion.R` es auxiliar: se usa solo si falla la exportación de 02.

Las salidas quedan en `private_data/` y pueden contener información individual. No las copie a `results/` sin revisión de divulgación ([docs/revision_divulgacion.md](docs/revision_divulgacion.md)).

## Alcance de la reproducibilidad

[docs/reproducibilidad.md](docs/reproducibilidad.md) distingue lo ejecutado y verificado de lo no comprobado. No se distribuyen:

- la base institucional;
- el validador que generó los documentos Word del manuscrito, porque contiene texto del manuscrito;
- tres tablas suplementarias retiradas por riesgo de identificación: S2, S3 y S10 del manuscrito vigente;
- el código de los análisis añadidos al manuscrito después del 10/10/2026: modelos guiados por DAG (Tabla 5 y S13–S18, S24), sensibilidad temporal de 2021 (Tabla 6 y S19–S21) y modelo sin RPM ni DPP (S22). Este depósito no reproduce esas tablas.

## Cita, licencia y acceso a los datos

- **Cita:** véase `CITATION.cff`. Los autores están pendientes de confirmación.
- **Licencia:** el titular de los derechos aún no ha definido la licencia; mientras tanto no se concede ninguna.
- **Acceso a los datos:** requiere la autorización del Hospital General Docente Calderón (HGDC), conforme a la aprobación ética del estudio (CEISH-HGDC). Las solicitudes se dirigen al HGDC.
