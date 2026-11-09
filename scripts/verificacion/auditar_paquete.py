"""Auditoría del paquete publicable: lista cerrada, huellas y patrones sensibles.

No lee private_data/, no ejecuta análisis ni modifica Git.
Uso:  python3 scripts/verificacion/auditar_paquete.py            # auditar
      python3 scripts/verificacion/auditar_paquete.py --generar  # regenerar manifiesto tras una revisión
"""
from pathlib import Path
import hashlib, json, re, sys

root = Path(__file__).resolve().parents[2]
MANIFEST = "manifiesto_permitidos.json"
SKIP_DIRS = {"private_data", ".git", ".venv", "renv"}
DATA_EXT = {".xlsx", ".xls", ".xlsm", ".rds", ".rdata", ".rda", ".sav", ".dta", ".sas7bdat", ".parquet",
            ".feather", ".sqlite", ".db", ".zip", ".gz", ".7z", ".tar", ".bz2", ".xz", ".docx", ".doc", ".json.gz"}
EXCLUDED_TABLES = {"tabla_07.csv", "tabla_08.csv", "tabla_15.csv"}  # retiradas por divulgación
MAX_BYTES = 2_000_000
patterns = {
    "ruta_personal_absoluta": re.compile(r"/(?:home|Users)/[A-Za-z0-9_]|[A-Za-z]:[\\/](?:Users|Documents)[\\/]"),
    "clave_privada": re.compile("-----BEGIN " + r"(?:RSA |EC |OPENSSH )?PRIVATE KEY-----"),
    "token_github": re.compile("gh" + r"[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,}"),
    "credencial_literal": re.compile(r"(?i)(?:password|passwd|api_key|access_token|secret)\s*[:=]\s*['\"][^'\"]{8,}['\"]"),
    "puntero_LFS": re.compile("version https://git-lfs." + "github.com/spec/v1"),
    "referencia_individual_literal": re.compile(r"(?i)fila(?:_excel)?\s*(?:=|:|\s)\s*\d{2,}\b"),
    "correo_electronico": re.compile(r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}"),
}
id_columns = re.compile(r"(?i)^\"?(fila|fila_excel|row_id|id|historia|cedula|hc|nombre)\"?$")


def candidates():
    for q in sorted(root.rglob("*")):
        rel = q.relative_to(root)
        if rel.parts[0] in SKIP_DIRS or "__pycache__" in rel.parts or not q.is_file():
            continue
        yield q, rel.as_posix()


def sha(q):
    return hashlib.sha256(q.read_bytes()).hexdigest()


if "--generar" in sys.argv:
    files = [{"ruta": n, "sha256": sha(q), "bytes": q.stat().st_size} for q, n in candidates() if n != MANIFEST]
    (root / MANIFEST).write_text(json.dumps({"descripcion": "Lista cerrada del contenido publicable", "archivos": files},
                                            ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    print(f"Manifiesto regenerado: {len(files)} archivos")
    sys.exit(0)

expected = {x["ruta"]: x["sha256"] for x in json.loads((root / MANIFEST).read_text(encoding="utf-8"))["archivos"]}
issues, count = [], 0
for q, n in candidates():
    if n == MANIFEST:
        continue
    if q.is_symlink():
        issues.append({"archivo": n, "riesgo": "symlink"}); continue
    if n not in expected:
        issues.append({"archivo": n, "riesgo": "fuera_de_lista"}); continue
    count += 1
    if sha(q) != expected[n]:
        issues.append({"archivo": n, "riesgo": "huella_distinta"})
    suf = "".join(q.suffixes[-2:]).lower() if q.name.endswith(".json.gz") else q.suffix.lower()
    if suf in DATA_EXT:
        issues.append({"archivo": n, "riesgo": "formato_de_datos_no_autorizado"})
    if q.name in EXCLUDED_TABLES:
        issues.append({"archivo": n, "riesgo": "tabla_retirada_por_divulgacion"})
    if q.stat().st_size > MAX_BYTES:
        issues.append({"archivo": n, "riesgo": "tamano_excesivo"})
    if suf == ".csv" and not n.startswith("results/"):
        issues.append({"archivo": n, "riesgo": "csv_fuera_de_results"})
    if suf in (".png", ".pdf"):
        if not n.startswith("results/figuras/"):
            issues.append({"archivo": n, "riesgo": "binario_fuera_de_figuras"})
        continue  # figuras revisadas visualmente; no se analizan como texto
    try:
        s = q.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        issues.append({"archivo": n, "riesgo": "binario_no_revisado"}); continue
    for label, pat in patterns.items():
        # renv.lock incluye por diseño los correos públicos de los mantenedores de paquetes CRAN
        if label == "correo_electronico" and n == "renv.lock":
            continue
        if pat.search(s):
            issues.append({"archivo": n, "riesgo": label})
    if suf == ".csv":
        header = s.splitlines()[0].split(",") if s else []
        if any(id_columns.match(h.strip()) for h in header):
            issues.append({"archivo": n, "riesgo": "columna_identificadora"})
for n in expected:
    if not (root / n).is_file():
        issues.append({"archivo": n, "riesgo": "ausente"})
print(json.dumps({"archivos_verificados": count, "hallazgos": issues, "modelos_ejecutados": 0}, ensure_ascii=False, indent=2))
sys.exit(bool(issues))
