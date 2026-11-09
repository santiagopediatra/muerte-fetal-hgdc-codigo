# Reproducibilidad: qué se verificó y qué no

## Verificación realizada (5–6 de noviembre de 2026)

**Condiciones de la prueba:**

- **Copia:** una copia aislada de este repositorio, en otra ruta de la misma computadora y fuera de cualquier repositorio Git.
- **Fuentes privadas:** se colocaron temporalmente en `private_data/fuentes/` y se eliminaron al terminar.
- **Fuente:** se comprobó que era el mismo archivo usado en la reconstrucción original.
- **Ejecución:** toda la secuencia del README, de 01 a 09, incluida la réplica de 01. Se comparó contra las salidas originales que sustentan el manuscrito.

| Paso | Resultado frente a las salidas originales |
|---|---|
| 01 reconstrucción | Base analítica idéntica, celda a celda: 23.743 × 121, mismas columnas. Iguales los 46 controles y el flujo. Única diferencia: el texto depurado de un control (sin referencia a una fila) |
| 01 réplica | Réplica idéntica a la base, comprobada por 06 |
| 02 Firth y ML, 4 poblaciones | 144 estimaciones idénticas (diferencia 0). Descriptivos, flujos y diagnósticos idénticos byte a byte |
| 03 Bayes (3 priors) y 05 Bayes Score | Estimaciones, diagnósticos y comprobaciones predictivas idénticos byte a byte. Muestras MCMC idénticas; cambian solo la fecha y las rutas en los comentarios de CmdStan |
| 04 secundarios | Dominios y pares de Firth idénticos byte a byte |
| 06 y 06b comparación y figuras | Libro de comparación antes/después idéntico. Figuras de flujo y de comparación de OR idénticas byte a byte a las de `results/figuras/` (Figuras 1 y 2 del manuscrito del 09/10/2026) |
| Revisión científica 01 y 03 | `controles_auditoria.csv` (149 controles satisfactorios) y `comparacion_completa.csv` idénticos |
| 07, 08, 09 complementarios | Tabla 15S, splines de edad y edades extremas idénticos byte a byte a `results/tablas/complementarias/` |

**Entorno de la prueba:** R 4.5.0, logistf 1.26.1 y CmdStan 2.39.0. La biblioteca local actual difiere de `renv.lock` en cuatro paquetes (cpp11, digest, readxl y tibble, con versiones más recientes). Pese a ello, los resultados fueron idénticos.

## No comprobado

- **Restauración del entorno:** no se probó `renv::restore()` desde `renv.lock` en una computadora limpia, ni la ejecución en otro sistema operativo.
- **Lockfile:** `renv.lock` es el registrado en el entorno original, con R 4.5.0 y 133 paquetes. Se conserva sin cambios y no incluye `renv`; se usa renv 1.1.2.
- **Resultados bayesianos en otra máquina:** su igualdad exacta depende de CmdStan 2.39.0, del compilador y de la semilla 1409.
- **Tablas del manuscrito:** las 14 tablas de `results/tablas/principales/` (de las 17 originales; tres retiradas por divulgación) las generó un validador no incluido (`validar_hgdc.R`), porque contiene texto del manuscrito. Sus cifras proceden de los objetos que esta secuencia reproduce. Ese validador no se ejecutó en esta verificación. Sus cifras se cotejaron con el manuscrito vigente (11/10/2026): coinciden, con diferencias solo de redondeo y formato.
- **Análisis posteriores al 10/10/2026:** los modelos guiados por DAG, la sensibilidad temporal de 2021 y el modelo sin RPM ni DPP del manuscrito vigente no forman parte de este depósito.
- **Datos:** sin la autorización del HGDC para acceder a la base no puede ejecutarse ningún análisis. Este repositorio no permite la reproducción sin datos restringidos.

## Restaurar el entorno

1. Instale R 4.5.0, las herramientas de compilación del sistema y renv 1.1.2.
2. Instale renv 1.1.2, por ejemplo:

   ```r
   install.packages("https://cran.r-project.org/src/contrib/Archive/renv/renv_1.1.2.tar.gz", repos = NULL, type = "source")
   ```

3. Desde la raíz del repositorio:

   ```bash
   HGDC_INSTALL_DEPENDENCIES=YES Rscript scripts/verificacion/restaurar_entorno.R
   ```

4. Si va a ajustar los modelos bayesianos, instale CmdStan 2.39.0 aparte ([guía de CmdStanR](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)).

Los scripts cargan la biblioteca restaurada mediante `renv::load()` cuando existe `renv/library`.
