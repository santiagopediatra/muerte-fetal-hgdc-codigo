# Datos de entrada

**La base de datos no forma parte de este repositorio.** No contiene registros, valores, extractos, huellas ni descripciones detalladas del contenido de la base.

**Acceso:** la base es propiedad del Hospital General Docente Calderón (HGDC). Acceder a ella requiere la autorización del HGDC, conforme a la aprobación ética del estudio (CEISH-HGDC). Las solicitudes se dirigen al HGDC; los autores no pueden distribuirla.

## Ubicación esperada por el código

Quien disponga de autorización coloca los archivos recibidos del HGDC, con estos nombres, en una carpeta local que Git excluye:

```
private_data/fuentes/BASE_OBITO_MAESTRA.xlsx    # versión corregida (hoja BASE_ORIGINAL)
private_data/fuentes/BASE_OBITO_HISTORICA.xlsx  # versión anterior, solo para controles de comparación
```

El script 01 valida la estructura y el contenido del archivo recibido y se detiene si no corresponde a la versión usada en el estudio.

## Variables que usa el análisis

| Papel | Variables (nombre normalizado en el código) |
|---|---|
| Desenlace | `MUERTE_INTRAUTERINA` |
| Elegibilidad | `SEMANAS_GESTACION`, `PESO`, `EMBARAZO_MULTIPLE`, `SEXO` |
| Covariables del modelo principal | `EDAD`, `HIPERTENSIVOS`, `DIABETES_EN_EL_EMBARAZO`, `IVU`, `ANTECEDENTE_CIRUGIA_UTERINA`, `NACIONALIDAD`, `PAREJA_ESTABLE`, `INSTRUCCION`, `ETNIA_MINORITARIA`, `RPM`, `DPP` |

Las variables descriptivas y secundarias, y las reglas exactas de lectura, codificación y elegibilidad, están en `scripts/reconstruccion_reanalisis/01_reconstruir_validar.R`.

## Salidas

Todas las salidas de los scripts se escriben en `private_data/` y pueden contener información individual. Nunca se versionan ni se depositan.
