/* server.pl — Interfaz web para la KB de tránsito (SWI-Prolog)
   Requiere: reglas.pl con load_kb_geojson/1 y las reglas de consulta.
*/

:- set_prolog_flag(encoding, utf8).
:- use_module(library(http/http_json)). 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/json)).
:- use_module(library(pcre)).         % re_replace/4
:- use_module(library(lists)).

:- [reglas].    % tu loader + reglas (conecta/3, ruta_sugerida/5, etc.)

/* --------------------- Archivos estáticos (CSS) -------------------- */
user:file_search_path(css, './css').
:- http_handler(root(css), serve_files_in_directory(css), [prefix]).

/* ------------------------- HTTP handlers --------------------------- */
:- http_handler(root(.),               home,            []).
:- http_handler(root(load),            load_kb,         []).
:- http_handler(root(conecta),         ui_conecta,      []).
:- http_handler(root(sugerida),        ui_sugerida,     []).
:- http_handler(root(sugerida_multi),  ui_sug_multi,    []).
:- http_handler(root(api_conecta),     api_conecta,     []).
:- http_handler(root(api_sugerida),    api_sugerida,    []).

/* ------------------------- Server control -------------------------- */
start        :- start(8080).

start(Port) :-
    % Si ya hay un server en ese puerto, no crees otro
    ( http_current_server(http_dispatch, Port) ->
        format('Server already running at http://localhost:~w/~n', [Port])
    ; % Si hay cualquier otro server del mismo dispatcher en otro puerto, lo aviso
      ( findall(P, http_current_server(http_dispatch, P), Ps),
        Ps \= [] ->
          format('Other server(s) running on: ~w~n', [Ps])
        ; true),
      http_server(http_dispatch, [port(Port)]),
      format('Started server at http://localhost:~w/~n', [Port])
    ).

stop :-
    (   findall(P, http_current_server(http_dispatch, P), [])
    ->  writeln('No server is running.')
    ;   forall(http_current_server(http_dispatch, P),
           ( http_stop_server(P, []),
             format('Stopped server at port ~w~n', [P])
           ))
    ).

/* -------------------------- Helpers UI ----------------------------- */
head_common(Title) -->
    html([
      meta([charset='UTF-8']),
      title(Title),
      \base_css,
      \base_js
    ]).

base_css --> html(link([rel('stylesheet'), href('/css/style.css')])).

/* JS mínimo para añadir campos de paradas dinámicamente */
base_js -->
  {
    Script =
"function addStopField(){
  const cont = document.getElementById('stops');
  const inp  = document.createElement('input');
  inp.type   = 'text';
  inp.name   = 'p';                 // parámetro repetido
  inp.setAttribute('list','cities');
  inp.placeholder = 'Otra parada...';
  inp.className   = 'stop-input';
  cont.appendChild(inp);
}"
  },
  html(script(type('text/javascript'), Script)).


/* Ciudades únicas a partir de los hechos cargados */
all_cities(Cities) :-
    setof(C, R^D^T^P^(segmento(R,C,_,D,T,P); segmento(R,_,C,D,T,P)), Cs),
    sort(Cs, Cities), !.
all_cities([]).

/* Datalist de ciudades para autocompletado */
cities_datalist -->
  { all_cities(Cs) },
  html(datalist([id(cities)], \datalist_options(Cs))).

datalist_options([]) --> [].
datalist_options([C|T]) --> html(option([value=C], C)), datalist_options(T).

/* Etiqueta amigable para la ruta (r27 -> "Ruta 27") */
route_label(R, Label) :-
    atom_string(R, RS),
    ( sub_string(RS, 0, 1, _, "r"),
      sub_string(RS, 1, _, 0, Num)
    -> format(string(Label), "Ruta ~s", [Num])
    ;  Label = RS ).

hora_label(hora_pico,  "hora pico").
hora_label(hora_valle, "hora valle").
hora_label(H, S) :- atom_string(H, S).

two_decimals(T) --> { format(string(S), "~2f", [T]) }, html(S).

/* ----------------- Normalización de ciudades ----------------------- */

/* Convierte "San José" -> san_jose (átomo).
   Acepta: string, átomo, o lista de códigos. */
normalize_city(In, OutAtom) :-
    % 1) Convertir a string
    (   string(In)           -> InStr = In
    ;   atom(In)             -> atom_string(InStr, In)
    ;   is_list(In)          -> string_codes(InStr, In)
    ;   term_string(InStr, In)
    ),
    string_lower(InStr, S1),
    strip_accents(S1, S2),

    % 2) Recortar espacios al inicio/fin
    re_replace('^\\s+'/g, '', S2, S2a),
    re_replace('\\s+$'/g, '', S2a, S2b),

    % 3) Comprimir espacios internos a uno
    re_replace('\\s+'/g, ' ', S2b, S3),

    % 4) Cambiar espacios por guión bajo
    re_replace(' '/g, '_', S3, S4),

    atom_string(OutAtom, S4).



strip_accents(S, R) :-
    Acc = ["á"-"a","é"-"e","í"-"i","ó"-"o","ú"-"u","ñ"-"n",
           "Á"-"a","É"-"e","Í"-"i","Ó"-"o","Ú"-"u","Ñ"-"n"],
    foldl(replace_pair, Acc, S, R).
replace_pair(A-B, In, Out) :- re_replace(A/g, B, In, Out).

/* --------------------------- PAGES --------------------------------- */
home(_Req) :-
    findall(_, segmento(_,_,_,_,_,_), L), length(L,N),
    ( N>0 -> Status = ['Hechos cargados: ', b(N)]
           ; Status = ['Sin hechos cargados. Primero usa “Cargar GeoJSON”.'] ),
    reply_html_page(
      \head_common('Tránsito y rutas viales de Costa Rica — Demo'),
      [
        h1('Tránsito y rutas viales de Costa Rica — Demo'),
        p(class(muted), 'Consulta educativa sobre tramos directos.'),

        div(class(card), [
          h2('1) Cargar GeoJSON'),
          form([action('/load'), method('GET'), class(row)], [
            input([type(text), name(file), value('rutas.geojson'), size(40),
                   placeholder('nombre-del-archivo.geojson')]),
            input([type(submit), value('Cargar')])
          ]),
          p(class(muted), 'Escribe el nombre si es distinto.')
        ]),

        div(class(card), [
          h2('2) Conexiones directas'),
          form([action('/conecta'), method('GET')], [
            \cities_datalist,
            div(class(row), [
              span('Origen:'),  input([type(text), name(a), list(cities), placeholder('San José')]),
              span('Destino:'), input([type(text), name(b), list(cities), placeholder('Caldera')]),
              input([type(submit), value('Consultar')])
            ]),
            p(class(muted), 'Muestra todas las rutas que conectan directamente dos ciudades (escribe o elige de la lista).')
          ])
        ]),

        div(class(card), [
          h2('3) Ruta sugerida (más rápida)'),
          form([action('/sugerida'), method('GET')], [
            \cities_datalist,
            div(class(row), [
              span('Origen:'),  input([type(text), name(a), list(cities), placeholder('San José')]),
              span('Destino:'), input([type(text), name(b), list(cities), placeholder('Limón')]),
              span('Hora:'),    select([name(hora)], [
                                     option([value(hora_pico)],  'hora_pico'),
                                     option([value(hora_valle), selected], 'hora_valle')
                                   ]),
              input([type(submit), value('Calcular')])
            ])
          ]),
          hr([]),
          h2('Ruta con múltiples paradas'),
          p(class(muted), 'Añade una o más paradas intermedias; se calcula cada tramo consecutivo.'),
          form([action('/sugerida_multi'), method('GET')], [
            \cities_datalist,
            div([id(stops), class(row)], [
              input([type(text), name(p), list(cities), placeholder('Origen (p.ej., San José)')]),
              input([type(text), name(p), list(cities), placeholder('Destino (p.ej., Limón)')])
            ]),
            div(class(row), [
              button([type(button), class(btn), onclick('addStopField()')], 'Agregar parada'),
              span('Hora:'), select([name(hora)], [
                option([value(hora_pico)],  'hora_pico'),
                option([value(hora_valle), selected], 'hora_valle')
              ]),
              input([type(submit), value('Calcular')])
            ])
          ])
        ]),

        p(class(muted), ['APIs JSON: ',
          a([href('/api_conecta?a=san_jose&b=caldera')], '/api_conecta?a=san_jose&b=caldera'),
          ' · ',
          a([href('/api_sugerida?a=san_jose&b=limon&hora=hora_pico')],
            '/api_sugerida?a=san_jose&b=limon&hora=hora_pico')
        ]),

        div(class(muted), Status)
      ]).

/* ----------------------- Acciones (pages) -------------------------- */
load_kb(Request) :-
    http_parameters(Request, [ file(File, [default('rutas.geojson')]) ]),
    (   exists_file(File)
    ->  load_kb_geojson(File),
        findall(_, segmento(_,_,_,_,_,_), L), length(L,N),
        Msg = ['Se cargaron ', b(N), ' segmentos desde ', code(File), '.']
    ;   Msg = ['Archivo no encontrado: ', code(File)]
    ),
    reply_html_page(\head_common('Cargar GeoJSON'),
      [ h1('Resultado de carga'), p(Msg), p(a([href('/')],'Volver a inicio')) ]).

ui_conecta(Request) :-
    http_parameters(Request, [
      a(AStr, [string, default("san jose")]),
      b(BStr, [string, default("caldera")])
    ]),
    normalize_city(AStr, A),
    normalize_city(BStr, B),
    findall(t(R,Dist,Tipo,Peaje,TH,TV),
            tramo_info(A,B,R,Dist,Tipo,Peaje,TH,TV),
            Filas),
    ( Filas == [] ->
        Body = p(['No hay tramo directo entre ', b(A),' y ', b(B), '.'])
    ;   Body = \tabla_conexiones(A,B,Filas)
    ),
    reply_html_page(\head_common('Conexiones directas'),
      [ h1('Conexiones directas'), Body, p(a([href('/')],'Volver a inicio')) ]).

ui_sugerida(Request) :-
    http_parameters(Request, [
      a(AStr, [string, default("san jose")]),
      b(BStr, [string, default("limon")]),
      hora(H, [atom,   default(hora_valle)])
    ]),
    normalize_city(AStr, A),
    normalize_city(BStr, B),
    ( ruta_sugerida(A,B,H,R,T) ->
        route_label(R, RL), hora_label(H, HL),
        reply_html_page(\head_common('Ruta sugerida'),
          [ h1('Ruta sugerida'),
            p(['Origen: ', b(A)]),
            p(['Destino: ', b(B)]),
            p(['Hora: ', b(HL)]),
            p(['Ruta: ', b(RL)]),
            p(['Tiempo estimado: ', b(\two_decimals(T)), ' horas']),
            p(a([href('/')],'Volver a inicio'))
          ])
    ; reply_html_page(\head_common('Ruta sugerida'),
          [ h1('Sin ruta directa'),
            p(['No hay ruta directa entre ', b(A),' y ', b(B), '.']),
            p(a([href('/')],'Volver a inicio'))
          ])
    ).

/* -------- Ruta sugerida con múltiples paradas ---------------------- */
ui_sug_multi(Request) :-
    http_parameters(Request, [hora(H, [atom, default(hora_valle)])],
                    [form_data(Data)]),
    findall(S, member(p=S, Data), RawStops0),
    ( RawStops0 = [] ->
        Stops = []
    ; maplist(normalize_city, RawStops0, Stops)
    ),
    ( Stops = [_,_|_] ->                % <— no nombramos A,B porque no se usan
        build_multi_legs(Stops, H, Rows, Total, Faltantes),
        Body = \tabla_multitramos(Stops, H, Rows, Total, Faltantes)
    ; Body = p('Necesitas al menos un origen y un destino (dos campos).')
    ),
    reply_html_page(\head_common('Ruta con múltiples paradas'),
      [ h1('Ruta con múltiples paradas'), Body, p(a([href('/')],'Volver a inicio')) ]).

build_multi_legs([_], _, [], 0, []) :- !.
build_multi_legs([A,B|T], Hora, [row(A,B,Ok,R,Dist,TP,TV)|Rows], Total, Faltantes) :-
    ( ruta_sugerida(A,B,Hora,R,TP) ->
        distancia_directa(A,B,Dist),
        tiempo_directo(A,B,hora_valle,R,TV),
        Ok = ok,
        build_multi_legs([B|T], Hora, Rows, TotalRest, Faltantes)
    ; Ok = fail, R = '-', Dist = '-', TP = '-', TV = '-',
      build_multi_legs([B|T], Hora, Rows, TotalRest, F0),
      Faltantes = [A-B|F0]
    ),
    ( number(TP) -> Total is TP + TotalRest ; Total = TotalRest ).

tabla_multitramos(Stops, Hora, Rows, Total, Faltantes) -->
  { hora_label(Hora, HL) },
  html([
    p(['Paradas ingresadas: ', code(Stops)]),
    p(['Hora: ', b(HL)]),
    table([
      tr([th('Desde'), th('Hasta'), th('Ruta'), th('Dist. (km)'),
          th('Tiempo (pico)'), th('Tiempo (valle)'), th('Estado')]),
      \rows_multi(Rows)
    ]),
    \resumen_multi(Total, Faltantes)
  ]).

rows_multi([]) --> [].
rows_multi([row(A,B,ok,R,D,TP,TV)|T]) -->
  { route_label(R, RL),
    format(string(SP), "~2f h", [TP]),
    format(string(SV), "~2f h", [TV]) },
  html(tr([td(A), td(B), td(RL), td(D), td(SP), td(SV), td('OK')])),
  rows_multi(T).
rows_multi([row(A,B,fail,_,_,_,_)|T]) -->
  html(tr([td(A), td(B), td('—'), td('—'), td('—'), td('—'),
           td([style='color:#b91c1c'], 'Sin tramo directo')])),
  rows_multi(T).

resumen_multi(Total, []) -->
  { format(string(S), "~2f horas", [Total]) },
  html([p(b(['Tiempo total (sólo tramos directos): ', S]))]).
resumen_multi(Total, Faltan) -->
  { format(string(S), "~2f horas", [Total]) },
  html([
    p(b(['Tiempo total (sólo tramos directos): ', S])),
    p([span([style='color:#b91c1c'], 'Atención: '),
       'no existe conexión directa para: ', code(Faltan), '.'])
  ]).

/* --------------------- Render de tablas ---------------------------- */
tramo_info(A,B,R,Dist,Tipo,Peaje,THPico,THValle) :-
    ( segmento(R,A,B,Dist,Tipo,Peaje)
    ; segmento(R,B,A,Dist,Tipo,Peaje)
    ),
    tiempo_directo(A,B,hora_pico,  R, THPico),
    tiempo_directo(A,B,hora_valle, R, THValle).

tabla_conexiones(A,B,Filas) -->
  html([
    p(['Tramos directos entre ', b(A), ' y ', b(B), ':']),
    table([
      tr([th('Ruta'), th('Tipo'), th('Peaje'), th('Dist. (km)'),
          th('Tiempo (pico)'), th('Tiempo (valle)')]),
      \rows_conexiones(Filas)
    ])
  ]).

rows_conexiones([]) --> [].
rows_conexiones([t(R,D,Tipo,Peaje,TP,TV)|T]) -->
  { route_label(R, RL),
    format(string(SP), "~2f h", [TP]),
    format(string(SV), "~2f h", [TV]) },
  html(tr([td(RL), td(Tipo), td(Peaje), td(D), td(SP), td(SV)])),
  rows_conexiones(T).

/* ------------------------ APIs JSON -------------------------------- */
api_conecta(Request) :-
    http_parameters(Request, [ a(AStr, [string]), b(BStr, [string]) ]),
    normalize_city(AStr, A), normalize_city(BStr, B),
    findall(_{route:R, label:RL, type:Tipo, toll:Peaje, dist_km:D,
              time_peak:TP, time_off:TV},
            ( tramo_info(A,B,R,D,Tipo,Peaje,TP,TV),
              route_label(R, RL) ),
            Rows),
    reply_json_dict(_{from:A, to:B, results:Rows}).

api_sugerida(Request) :-
    http_parameters(Request, [ a(AStr,[string]), b(BStr,[string]), hora(H,[atom]) ]),
    normalize_city(AStr, A), normalize_city(BStr, B),
    ( ruta_sugerida(A,B,H,R,T) ->
        route_label(R, RL),
        reply_json_dict(_{from:A,to:B,hour:H,route:R,label:RL,time:T})
    ;   reply_json_dict(_{from:A,to:B,hour:H,error:"no_direct_route"}, [status(404)])
    ).
