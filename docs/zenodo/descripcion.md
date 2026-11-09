# Descripción para Zenodo (borrador)

Código de análisis y resultados agregados del estudio *Características asociadas con muerte fetal registrada en gestaciones únicas del Hospital General Docente Calderón, Quito, Ecuador, 2018–2023*. Se trata de un estudio transversal retrospectivo sobre registros hospitalarios anonimizados.

**Contenido del depósito:**

- Scripts de R para:
  - reconstruir y validar la base analítica desde la fuente institucional;
  - el modelo principal de regresión logística de Firth y la logística convencional;
  - los modelos bayesianos (CmdStan) con tres distribuciones previas;
  - los modelos secundarios por dominio;
  - los análisis de sensibilidad de elegibilidad, de forma funcional de la edad y de edades extremas;
  - las OR crudas (STROBE 16a).
- Dependencias registradas (`renv.lock`, R 4.5.0).
- Documentación de la estructura de entrada requerida, sin registros.
- Tablas agregadas del manuscrito (Tablas 1–4, S1, S4–S9, S11, S12 y S23 del manuscrito vigente), tablas complementarias y figuras, revisadas para su difusión. Se excluyeron tres tablas suplementarias por riesgo de identificación.

**Población:** 23.542 gestaciones únicas elegibles, con 428 muertes fetales registradas. El modelo ajustado usa 23.138 casos completos, con 418 muertes.

**Acceso a los datos:** el depósito **no contiene datos individuales**. La base de datos es propiedad del Hospital General Docente Calderón (HGDC) y no se distribuye. Acceder a ella requiere la autorización del HGDC, conforme a la aprobación ética del estudio (CEISH-HGDC).

**Verificación:** con la autorización del HGDC, la secuencia completa de scripts reprodujo de forma idéntica las estimaciones, tablas y figuras incluidas en este depósito.

**Interpretación:** las asociaciones no son efectos causales.
