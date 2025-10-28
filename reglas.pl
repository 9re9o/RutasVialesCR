/* ================= Reglas + Loader de GeoJSON (robusto y silencioso) ========== */
:- module(reglas, [
    load_kb_geojson/1,
    segmento/6, congestion/3, city_seen/1,
    velocidad_base/2,
    conecta/3, distancia_directa/3, tiene_peaje/2,
    tiempo_directo/5, ruta_sugerida/5
]).

:- dynamic segmento/6.     % segmento(Ruta, Desde, Hacia, DistKm, Tipo, Peaje)
:- dynamic congestion/3.   % congestion(Hora, Ruta, Factor)
:- dynamic city_seen/1.    % city_seen(CiudadNormalizada)

:- use_module(library(http/json)).   % json_read_dict/2
:- use_module(library(lists)).
:- use_module(library(pcre)).

/* ----- Velocidades base por tipo de vía ----- */
velocidad_base(autopista, 90).
velocidad_base(primaria,  70).
velocidad_base(secundaria,60).

/* ==================== NORMALIZACIÓN ==================== */
normalize_city(In, OutAtom) :-
  (   var(In)    -> OutAtom = ''
  ;   string(In) -> InStr = In
  ;   atom(In)   -> atom_string(InStr, In)
  ;   is_list(In)-> string_codes(InStr, In)
  ;   term_string(InStr, In)
  ),
  string_lower(InStr, S1),
  strip_accents(S1, S2),
  re_replace('^\\s+'/g, '', S2, S2a),
  re_replace('\\s+$'/g, '', S2a, S2b),
  re_replace('\\s+'/g, ' ', S2b, S3),
  re_replace(' '/g, '_', S3, S4),
  atom_string(OutAtom, S4).

strip_accents(S, R) :-
  Acc = ["á"-"a","é"-"e","í"-"i","ó"-"o","ú"-"u","ñ"-"n",
         "Á"-"a","É"-"e","Í"-"i","Ó"-"o","Ú"-"u","Ñ"-"n"],
  foldl(replace_pair, Acc, S, R).

replace_pair(A-B, In, Out) :- re_replace(A/g, B, In, Out).

/* ==================== LECTURA SEGURA DE DICTS ==================== */
dict_get_ci(Dict, KeyAtom, Val) :-
  ( get_dict(KeyAtom, Dict, Val)
  ; atom_string(KeyAtom, KS),
    string_lower(KS, KL), atom_string(KLAtom, KL),
    KeyAtom \= KLAtom, get_dict(KLAtom, Dict, Val)
  ; atom_string(KeyAtom, KS),
    string_upper(KS, KU), atom_string(KUAtom, KU),
    KeyAtom \= KUAtom, get_dict(KUAtom, Dict, Val)
  ; atom_string(KeyAtom, KS),
    string_lower(KS, KL),
    sub_string(KL, 0, 1, _, H), sub_string(KL, 1, _, 0, T),
    string_upper(H, HU), string_concat(HU, T, Title),
    atom_string(TitleAtom, Title), KeyAtom \= TitleAtom,
    get_dict(TitleAtom, Dict, Val)
  ).

prop_text_ci(P, Keys, Val) :-
  member(K, Keys),
  ( dict_get_ci(P, K, V), V \= "" -> Val = V, !
  ; fail ).

/* ==================== LOADER DE GEOJSON ==================== */
load_kb_geojson(File) :-
    retractall(segmento(_,_,_,_,_,_)),
    retractall(congestion(_,_,_)),
    retractall(city_seen(_)),
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8), bom(true)]),
        json_read_dict(In, G),
        close(In)
    ),
    ( is_dict(G), _ = G.get(features) -> Features = G.features ; Features = [] ),
    forall(member(F, Features), asserta_feature(F)),
    % Si quieres log, activa: ?- debug(kb(load)).
    findall(_, segmento(_,_,_,_,_,_), L), length(L, NTramos),
    ( setof(C, city_seen(C), Cs) -> length(Cs, NCiudades) ; NCiudades = 0 ),
    debug(kb(load), 'KB cargada (~w tramos, ~w ciudades).', [NTramos, NCiudades]).

/* ---- Aserción de una feature (acepta alias, genera city_seen/1) ---- */
asserta_feature(F) :-
    ( _ = F.get(properties) -> P = F.properties ; P = _{} ),
    % Intentar extraer extremos con varios alias
    ( get_from_to(P, From0, To0)
      -> true
      ;  try_split_pair(P, From0, To0)
      ;  (From0="", To0="") % no hay datos suficientes
    ),
    ( From0 \= "" -> normalize_city(From0, A) ; A = "" ),
    ( To0   \= "" -> normalize_city(To0,   B) ; B = "" ),
    ( A \= "" -> (city_seen(A) -> true ; assertz(city_seen(A))) ; true ),
    ( B \= "" -> (city_seen(B) -> true ; assertz(city_seen(B))) ; true ),
    % Si tenemos ambos, asertar segmento/6
    ( A \= "", B \= "" ->
        get_route_id(P, Ruta),
        get_dist_km(P, Dist),
        get_type(P, Tipo),
        get_toll(P, Peaje),
        assertz(segmento(Ruta, A, B, Dist, Tipo, Peaje)),
        maybe_congestion(P, Ruta)
      ; true ).

/* ---- helpers de properties ---- */
get_route_id(P, Ruta) :-
  ( dict_get_ci(P, route_id, R0) -> atom_string(Ruta, R0)
  ; dict_get_ci(P, id,       R0) -> atom_string(Ruta, R0)
  ; Ruta = 'r?'
  ).

get_from_to(P, From, To) :-
  FromKeys = [from, origen, desde, a, source, start,
              origen_nombre, origen_ciudad, origen_canton, desde_ciudad],
  ToKeys   = [to, destino, hasta, hacia, b, target, end,
              destino_nombre, destino_ciudad, destino_canton, hasta_ciudad],
  prop_text_ci(P, FromKeys, From),
  prop_text_ci(P, ToKeys,   To).

% Fallback: intenta dividir "A - B", "A->B", "A→B", "A — B", etc.
try_split_pair(P, From, To) :-
  JoinKeys = [name, label, ruta, tramo, tramo_id, tramo_name, road, camino],
  prop_text_ci(P, JoinKeys, S0),
  ( re_matchsub('(.+?)\\s*(-|->|→|=>|—|–)\\s*(.+)', S0, D, [])
    -> From = D.1, To = D.3
    ; fail ).

get_dist_km(P, Dist) :-
  ( get_dict(dist_km,  P, D) -> Dist = D
  ; get_dict(distance, P, D) -> Dist = D
  ; Dist = 0
  ).

get_type(P, Tipo) :-
  ( get_dict(type,      P, T0) -> atom_string(Tipo, T0)
  ; get_dict(road_type, P, T0) -> atom_string(Tipo, T0)
  ; Tipo = secundaria
  ).

get_toll(P, Peaje) :-
  ( get_dict(toll,  P, L0) -> atom_string(Peaje, L0)
  ; get_dict(peaje, P, L0) -> atom_string(Peaje, L0)
  ; Peaje = sin_peaje
  ).

maybe_congestion(P, Ruta) :-
  ( get_dict(congestion_hourpico, P, CF)
    -> assertz(congestion(hora_pico, Ruta, CF))
    ;  true ).

/* ==================== REGLAS DE CONSULTA ==================== */
conecta(A,B,R) :- segmento(R,A,B,_,_,_) ; segmento(R,B,A,_,_,_).

distancia_directa(A,B,D) :-
    ( segmento(_,A,B,D,_,_) ; segmento(_,B,A,D,_,_) ), !.

tiene_peaje(A,B) :-
    ( segmento(_,A,B,_,_,SiNo) ; segmento(_,B,A,_,_,SiNo) ),
    ( SiNo = peaje ; SiNo = "Si" ; SiNo = si ; SiNo = con_peaje ).

congestion_factor(Hora,R,F) :- congestion(Hora,R,F), !.
congestion_factor(_,_,1.0).

tiempo_directo(A,B,Hora,R,T) :-
    ( segmento(R,A,B,Dist,Tipo,_)
    ; segmento(R,B,A,Dist,Tipo,_)
    ),
    velocidad_base(Tipo,V0),
    congestion_factor(Hora,R,F),
    V is V0 * F,
    T is Dist / V.

ruta_sugerida(A,B,Hora,Rbest,Tbest) :-
    findall(T-R, tiempo_directo(A,B,Hora,R,T), Pares),
    Pares \= [],
    min_member(Tbest-Rbest, Pares).
