/* ================= Reglas + Loader de GeoJSON ==================== */
:- dynamic segmento/6.
:- dynamic congestion/3.
:- use_module(library(http/json)).   % json_read_dict/2
:- use_module(library(lists)).

% ----- Velocidades base por tipo de vía -----
velocidad_base(autopista, 90).
velocidad_base(primaria,  70).
velocidad_base(secundaria,60).

% ========== Loader de GeoJSON ==========
load_kb_geojson(File) :-
    retractall(segmento(_,_,_,_,_,_)),
    retractall(congestion(_,_,_)),
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        json_read_dict(In, G),
        close(In)
    ),
    Features = G.features,
    forall(member(F, Features), asserta_feature(F)).


asserta_feature(F) :-
    P = F.properties,
    Ruta  = P.route_id,
    atom_string(A, P.from),
    atom_string(B, P.to),
    Dist  = P.dist_km,
    tipo_atom(P.type, Tipo),
    peaje_atom(P.toll, Peaje),
    assertz(segmento(Ruta, A, B, Dist, Tipo, Peaje)),
    (   _ = P.get(congestion_hourpico)
    ->  CF = P.get(congestion_hourpico),
        assertz(congestion(hora_pico, Ruta, CF))
    ;   true
    ).

tipo_atom(S, A)  :- atom_string(A, S).
peaje_atom(S, A) :- atom_string(A, S).

% ========== Reglas de consulta (como antes) ==========
conecta(A,B,R) :- segmento(R,A,B,_,_,_) ; segmento(R,B,A,_,_,_).

distancia_directa(A,B,D) :-
    ( segmento(_,A,B,D,_,_) ; segmento(_,B,A,D,_,_) ), !.

tiene_peaje(A,B) :-
    ( segmento(_,A,B,_,_,peaje) ; segmento(_,B,A,_,_,peaje) ).

congestion_factor(Hora,R,F) :- congestion(Hora,R,F), !.
congestion_factor(_,_,1.0).

% tiempo_directo(Origen, Destino, Hora, Ruta, TiempoHoras)
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
