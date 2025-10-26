#!/usr/bin/env python3
import json, math, argparse, unicodedata, re

# ------------------ utilidades ------------------

def haversine_km(p1, p2):
    # p = (lon, lat) en grados
    lon1, lat1 = p1
    lon2, lat2 = p2
    R = 6371.0088
    phi1 = math.radians(lat1); phi2 = math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dl   = math.radians(lon2 - lon1)
    a = math.sin(dphi/2)**2 + math.cos(phi1)*math.cos(phi2)*math.sin(dl/2)**2
    return 2*R*math.asin(math.sqrt(a))

def geom_length_km(geom):
    t = geom.get("type")
    coords = geom.get("coordinates")
    if not coords:
        return 0.0
    if t == "LineString":
        return sum(haversine_km(coords[i], coords[i+1]) for i in range(len(coords)-1))
    if t == "MultiLineString":
        total = 0.0
        for line in coords:
            total += sum(haversine_km(line[i], line[i+1]) for i in range(len(line)-1))
        return total
    return 0.0

def strip_accents(s):
    return ''.join(c for c in unicodedata.normalize('NFD', s)
                   if unicodedata.category(c) != 'Mn')

def norm_city(s):
    if not s:
        return ""
    s = str(s)
    s = strip_accents(s).lower().strip()
    s = re.sub(r'\s+', ' ', s)
    s = s.replace(' ', '_')
    return s

def infer_type_from_ref(ref):
    """Clasifica por rango de la Red Vial (wiki OSM/MOPT):
       1–39 primaria, 100–257 secundaria, 301–939 terciaria (aquí no la exportamos)."""
    try:
        n = int(re.sub(r'\D', '', str(ref)))
    except Exception:
        return "primaria"
    if 1 <= n <= 39:
        return "primaria"
    if 100 <= n <= 257:
        return "secundaria"
    if 301 <= n <= 939:
        return "terciaria"
    return "primaria"

def guess_toll(ref, props):
    # Marca la 27 como peaje conocido; y si viene toll=yes en propiedades
    if str(ref) == "27" or str(props.get("ref","")) == "27":
        return "peaje"
    t = str(props.get("toll","")).lower()
    return "peaje" if t in ("yes","true","1") else "sin_peaje"

# ------------------ pipeline ------------------

def convert(in_path, out_path, keep_tertiary=False):
    with open(in_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    feats_out = []
    features = data.get("features", [])

    for f in features:
        props = f.get("properties", {})
        geom  = f.get("geometry", {})

        # Sacar ref de la relation/way; en OSM suele llamarse "ref"
        ref = props.get("route_id") or props.get("ref") or props.get("name") or ""
        ref_num = re.sub(r'\D', '', str(ref))
        if not ref_num:
            # sin ref: no es una ruta nacional; saltar
            continue

        rtype = infer_type_from_ref(ref_num)
        if rtype == "terciaria" and not keep_tertiary:
            continue  # sólo primarias/ secundarias

        # Longitud
        dist_km = round(geom_length_km(geom), 3)

        # from/to si vienen en properties (relations Overpass suelen traerlos)
        from_raw = props.get("from") or props.get("desde") or ""
        to_raw   = props.get("to")   or props.get("hasta") or ""

        feature_out = {
            "type": "Feature",
            "properties": {
                "route_id": f"r{ref_num}",
                "from": norm_city(from_raw),
                "to":   norm_city(to_raw),
                "dist_km": dist_km if dist_km > 0 else props.get("dist_km", 0),
                "type": "autopista" if str(ref_num) == "27" else rtype,
                "toll": guess_toll(ref_num, props)
            },
            "geometry": geom  # puedes quitarla si no la necesitas
        }
        feats_out.append(feature_out)

    coll = {
        "type": "FeatureCollection",
        "name": "rutas_cr_filtradas",
        "features": feats_out
    }
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(coll, f, ensure_ascii=False, indent=2)
    print(f"Listo: {len(feats_out)} features -> {out_path}")

# ------------------ CLI ------------------

if __name__ == "__main__":
    ap = argparse.ArgumentParser(
        description="Filtra rutas primarias/secundarias de Costa Rica y las adapta a tu Prolog.")
    ap.add_argument("--in",  dest="inp",  required=True, help="GeoJSON de entrada (Overpass/QGIS/MOPT)")
    ap.add_argument("--out", dest="out", required=True, help="GeoJSON de salida (viascr.geojson)")
    ap.add_argument("--keep-tertiary", action="store_true", help="incluir terciarias (301–939)")
    args = ap.parse_args()
    convert(args.inp, args.out, keep_tertiary=args.keep_tertiary)
