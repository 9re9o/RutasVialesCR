:- encoding(utf8).
/* --- Streams en UTF-8 antes de iniciar --- */
setup_io :-
    catch(set_stream(user_input,  encoding(utf8)), _, true),
    catch(set_stream(user_output, encoding(utf8)), _, true),
    catch(set_stream(user_error,  encoding(utf8)), _, true).
:- initialization(setup_io, now).

/* --------------------- Librerías --------------------- */
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(pcre)).
:- use_module(library(lists)).

/* --------------------- Reglas (tu módulo) ------------- */
:- use_module(reglas).  % exporta load_kb_geojson/1, segmento/6, city_seen/1, etc.

/* ---------------- Archivos estáticos (CSS) ------------ */
user:file_search_path(css, './css').
:- http_handler(root(css), serve_files_in_directory(css), [prefix]).

/* -------------------- HTTP handlers ------------------ */
:- http_handler(root(.),               home,            []).
:- http_handler(root(load),            load_kb,         []).
:- http_handler(root(conecta),         ui_conecta,      []).
:- http_handler(root(sugerida),        ui_sugerida,     []).
:- http_handler(root(sugerida_multi),  ui_sug_multi,    []).
:- http_handler(root(api_cities),      api_cities,      []).
:- http_handler(root(api_conecta),     api_conecta,     []).
:- http_handler(root(api_sugerida),    api_sugerida,    []).

/* ------------------------- Server control -------------------------- */
start        :- start(8080).
start(Port) :-
  ( http_current_server(http_dispatch, Port) ->
      format('Server already running at http://localhost:~w/~n', [Port])
  ; http_server(http_dispatch, [port(Port)]),
    format('Started server at http://localhost:~w/~n', [Port])
  ).

stop :-
  forall(http_current_server(http_dispatch, P),
         ( http_stop_server(P, []),
           format('Stopped server at port ~w~n', [P]) )).

/* -------------------------- Helpers UI ----------------------------- */
head_common(Title) -->
  html([
    meta([charset='UTF-8']),
    meta([http_equiv='Content-Type', content='text/html; charset=UTF-8']),
    meta([name='viewport', content='width=device-width, initial-scale=1']),
    title(Title),
    \base_css,
    \base_js
  ]).

base_css --> html(link([rel('stylesheet'), href('/css/style.css')])).

/* JS para añadir selects de paradas dinámicamente (clonado del primero) */
base_js -->
{
Script = "
function addStopSelect(){
  const cont = document.getElementById('stops');
  const sel0 = cont.querySelector('select[name=\"p\"]');
  if(!sel0){ return; }
  const clone = sel0.cloneNode(true);
  clone.id = 'p' + (document.querySelectorAll('select[name=\"p\"]').length+1);
  cont.appendChild(clone);
}"
},
  html(script(type('text/javascript'), Script)).

/* -------------- Normalización (entrada UI) ----------------- */
normalize_city(In, OutAtom) :-
  (   string(In)  -> InStr = In
  ;   atom(In)    -> atom_string(InStr, In)
  ;   is_list(In) -> string_codes(InStr, In)
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

fmt_horas(Term, Out) :- ( number(Term) -> format(string(Out), "~2f h", [Term]) ; Out = "N/A" ).
fmt_total(Term, Out) :- ( number(Term) -> format(string(Out), "~2f horas", [Term]) ; Out = "0.00 horas" ).
two_dec(T) --> { format(string(S), "~2f", [T]) }, html(S).

route_label(R, Label) :-
  atom_string(R, RS),
  ( sub_string(RS, 0, 1, _, "r"), sub_string(RS, 1, _, 0, Num)
  -> format(string(Label), "Ruta ~s", [Num])
  ;  Label = RS ).

hora_label(hora_pico,  "hora_pico").
hora_label(hora_valle, "hora_valle").
hora_label(H, S) :- atom_string(H, S).

/* -------------------- Listado de ciudades --------------------------
   Unión de extremos en segmento/6 + city_seen/1 del loader           */
all_cities(Cities) :-
  findall(C, (segmento(_,C,_,_,_,_), C \= ''), L1),
  findall(C, (segmento(_,_,C,_,_,_), C \= ''), L2),
  findall(C, city_seen(C), L3),
  append([L1,L2,L3], Lall),
  sort(Lall, Cities), !.
all_cities([]).

pretty_label_from_atom(Norm, Label) :-
  atom_string(Norm, S),
  split_string(S, "_", "", Parts),
  maplist(cap_first, Parts, Caps),
  atomic_list_concat(Caps, " ", Label).

cap_first(In, Out) :-
  ( In = "" -> Out = ""
  ; string_chars(In, [H|R]),
    char_type(H, to_upper(UH)),
    string_chars(Out, [UH|R])
  ).

cities_select(Name, Id) -->
  { all_cities(Cs) },
  html(select([name(Name), id(Id), class('city-select')], \options_cities(Cs))).

options_cities([]) --> [].
options_cities([C|T]) -->
  { pretty_label_from_atom(C, Label) },
  html(option([value=C], Label)),
  options_cities(T).
  
/* Helpers para listar faltantes de forma segura */
miss_to_str(miss(A,B), S) :-
    % Convierte miss(origen,destino) en "origen→destino"
    format(string(S), "~w~w~w", [A, "→", B]).

faltantes_join([], "—").
faltantes_join(Fs, Joined) :-
    maplist(miss_to_str, Fs, Parts),
    atomic_list_concat(Parts, ", ", Joined).
/* --------------------------- PAGES -------------------------------- */
home(Request) :-
  % Absorbe cualquier query string mal formada tipo "/?KB cargada..."
  catch(http_parameters(Request, [], [form_data(_), on_error(continue)]), _, true),

  ( exists_file('rutas.geojson')
    -> catch(reglas:load_kb_geojson('rutas.geojson'), _, true)
     ; true ),
  findall(_, segmento(_,_,_,_,_,_), L), length(L,NTramos),
  ( all_cities(LCs) -> length(LCs, NCities) ; NCities = 0 ),
  ( NTramos>0 -> Status = div(class(success),
              ['KB cargada: ', b(NTramos), ' tramos - ', b(NCities), ' ciudades'])
         ; Status = div(class(warn),
              'Cargue el archivo de datos para comenzar (rutas.geojson).')
  ),
  reply_html_page(
    \head_common('Rutas viales de Costa Rica — Demo'),
    [
      h1('Tránsito y rutas viales de Costa Rica'),

      div(class(card), [
        h2('1) Cargar GeoJSON'),
        form([action('/load'), method('GET'), class(row)], [
          input([type(text), name(file), value('rutas.geojson'), size(40),
                 placeholder('nombre-del-archivo.geojson')]),
          input([type(submit), value('Cargar datos'), class('btn')])
        ]),
        p(class(muted), 'Si desea usar otro archivo, escriba su nombre y presione "Cargar datos".'),
        Status
      ]),

      div(class(card), [
        h2('2) Conexiones directas'),
        p(class(help), 'Elija dos lugares. Si existe un tramo directo, se mostrará aquí.'),
        form([action('/conecta'), method('GET')], [
          div(class(row), [
            label([for(a)], 'Origen:'),  \cities_select(a, a),
            label([for(b)], 'Destino:'), \cities_select(b, b),
            input([type(submit), value('Consultar rutas'), class('btn')])
          ])
        ])
      ]),

      div(class(card), [
        h2('3) Ruta sugerida (más rápida)'),
        form([action('/sugerida'), method('GET')], [
          div(class(row), [
            label([for(a2)], 'Origen:'),  \cities_select(a, a2),
            label([for(b2)], 'Destino:'), \cities_select(b, b2),
            label([for(hora)], 'Hora:'),
            select([name(hora), id(hora)], [
              option([value(hora_pico)],  'hora_pico'),
              option([value(hora_valle), selected], 'hora_valle')
            ]),
            input([type(submit), value('Calcular'), class('btn')])
          ])
        ])
      ]),

      div(class(card), [
        h2('4) Ruta con múltiples paradas'),
        p(class(muted), 'Añada una o más paradas intermedias; se calcula cada tramo consecutivo.'),
        form([action('/sugerida_multi'), method('GET')], [
          div([id(stops), class(row)], [
            \cities_select(p, p1),
            \cities_select(p, p2)
          ]),
          div(class(row), [
            button([type(button), class(btn), onclick('addStopSelect()')], 'Agregar parada'),
            label([for(hora_m)], 'Hora:'),
            select([name(hora), id(hora_m)], [
              option([value(hora_pico)],  'hora_pico'),
              option([value(hora_valle), selected], 'hora_valle')
            ]),
            input([type(submit), value('Calcular'), class('btn')])
          ])
        ])
      ])
    ]).

/* ----------------------- Acciones (pages) -------------------------- */
load_kb(Request) :-
  catch(http_parameters(Request, [], [form_data(_), on_error(continue)]), _, true),
  http_parameters(Request,
    [ file(File, [default('rutas.geojson')]) ],
    [ form_data(_), on_error(continue) ]),
  ( exists_file(File)
    -> ( catch(reglas:load_kb_geojson(File), E, (print_message(error,E), fail)),
         findall(_, segmento(_,_,_,_,_,_), L), length(L,N),
         ( all_cities(Cs) -> length(Cs,NC) ; NC=0 ),
         Msg = div(class(success), ['Se cargaron ', b(N), ' segmentos desde ', code(File),
                                    '. Ciudades detectadas: ', b(NC), '.'])
       )
     ;  Msg = div(class(error), ['Archivo no encontrado: ', code(File)])
  ),
  reply_html_page(\head_common('Carga de datos'),
    [ h1('Carga de datos'), Msg, p(a([href('/')],'Volver a inicio')) ]).

ui_conecta(Request) :-
  catch(http_parameters(Request, [], [form_data(_), on_error(continue)]), _, true),
  http_parameters(Request,
    [ a(AStr, [string, default("san jose")]),
      b(BStr, [string, default("caldera")]) ],
    [ form_data(_), on_error(continue) ]),
  normalize_city(AStr, A), normalize_city(BStr, B),
  findall(t(R,Dist,Tipo,Peaje,TH,TV),
          tramo_info(A,B,R,Dist,Tipo,Peaje,TH,TV),
          Filas),
  ( Filas == [] ->
      Body = div(class(warn), ['No hay tramo directo entre ', b(A),' y ', b(B), '.'])
  ;   Body = \tabla_conexiones(A,B,Filas)
  ),
  reply_html_page(\head_common('Rutas directas'),
    [ h1('Rutas directas'), Body, p(a([href('/')],'Volver a inicio')) ]).

ui_sugerida(Request) :-
  catch(http_parameters(Request, [], [form_data(_), on_error(continue)]), _, true),
  http_parameters(Request,
    [ a(AStr, [string, default("san jose")]),
      b(BStr, [string, default("limon")]),
      hora(H, [atom, default(hora_valle)]) ],
    [ form_data(_), on_error(continue) ]),
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
          p(['Tiempo estimado: ', b(\two_dec(T)), ' horas']),
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
  catch(http_parameters(Request, [], [form_data(_), on_error(continue)]), _, true),
  http_parameters(Request,
    [ hora(H, [atom, default(hora_valle)]) ],
    [ form_data(Data), on_error(continue) ]),
  findall(S, member(p=S, Data), RawStops0),
  ( RawStops0 = [] -> Stops = [] ; maplist(normalize_city, RawStops0, Stops) ),
  ( Stops = [_,_|_] ->
      build_multi_legs(Stops, H, Rows, Total, Faltantes),
      Body = \tabla_multitramos(Stops, H, Rows, Total, Faltantes)
  ; Body = div(class(warn), 'Necesitas al menos un origen y un destino (dos campos).')
  ),
  reply_html_page(\head_common('Ruta con múltiples paradas'),
    [ h1('Ruta con múltiples paradas'), Body, p(a([href('/')],'Volver a inicio')) ]).
build_multi_legs([_], _, [], 0, []) :- !.
build_multi_legs([A,B|T], Hora, [row(A,B,Ok,R,Dist,TP,TV)|Rows], Total, Faltantes) :-
  ( ruta_sugerida(A,B,Hora,R,TP) ->
      ( distancia_directa(A,B,Dist) -> true ; Dist = 'N/A' ),
      ( tiempo_directo(A,B,hora_valle,R,TV) -> true ; TV  = 'N/A' ),
      Ok = ok,
      build_multi_legs([B|T], Hora, Rows, TotalRest, FPrev),
      Faltantes = FPrev
  ; % no hay ruta directa A->B
    Ok = fail, R = '-', Dist = '-', TP = '-', TV = '-',
    build_multi_legs([B|T], Hora, Rows, TotalRest, FPrev),
    Faltantes = [miss(A,B)|FPrev]
  ),
  ( number(TP) -> Total is TP + TotalRest ; Total = TotalRest ).


tabla_multitramos(Stops, Hora, Rows, Total, Faltantes) -->
  { hora_label(Hora, HL),
    with_output_to(string(StopsStr), write(Stops)) },
  html([
    p(['Paradas ingresadas: ', code(StopsStr)]),
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
  { route_label(R, RL), fmt_horas(TP, SP), fmt_horas(TV, SV) },
  html(tr([td(A), td(B), td(RL), td(D), td(SP), td(SV), td('OK')])),
  rows_multi(T).
rows_multi([row(A,B,fail,_,_,_,_)|T]) -->
  html(tr([td(A), td(B), td('N/A'), td('N/A'), td('N/A'), td('N/A'),
           td([style='color:#b91c1c'], 'Sin tramo directo')])),
  rows_multi(T).
resumen_multi(Total, []) -->
  { fmt_total(Total, S) },
  html([p(b(['Tiempo total (solo tramos directos): ', S]))]).

resumen_multi(Total, Faltan) -->
  { fmt_total(Total, S),
    faltantes_join(Faltan, J) },
  html([
    p(b(['Tiempo total (solo tramos directos): ', S])),
    p([span([style='color:#b91c1c'], 'Atención: '),
       'no existe conexión directa para: ', code(J), '.'])
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
    div(class('card result'), [
      h3(['Tramos directos: ', A, ' - ', B]),
      table([
        tr([th('Ruta'), th('Tipo'), th('Peaje'), th('Dist. (km)'),
            th('Tiempo (pico)'), th('Tiempo (valle)')]),
        \rows_conexiones(Filas)
      ])
    ])
  ]).

rows_conexiones([]) --> [].
rows_conexiones([t(R,D,Tipo,Peaje,TP,TV)|T]) -->
  { route_label(R, RL), fmt_horas(TP, SP), fmt_horas(TV, SV) },
  html(tr([td(RL), td(Tipo), td(Peaje), td(D), td(SP), td(SV)])),
  rows_conexiones(T).

/* ------------------------ APIs JSON -------------------------------- */
api_cities(_Request) :-
  ( all_cities(Cs) -> true ; Cs = [] ),
  reply_json_dict(_{cities: Cs}).

api_conecta(Request) :-
  catch(http_parameters(Request, [], [form_data(_), on_error(continue)]), _, true),
  http_parameters(Request,
    [ a(AStr,[string]), b(BStr,[string]) ],
    [ form_data(_), on_error(continue) ]),
  normalize_city(AStr, A), normalize_city(BStr, B),
  findall(_{route:R, label:RL, type:Tipo, toll:Peaje, dist_km:D,
            time_peak:TP, time_off:TV},
          ( tramo_info(A,B,R,D,Tipo,Peaje,TP,TV),
            route_label(R, RL) ),
          Rows),
  reply_json_dict(_{from:A, to:B, results:Rows}).

api_sugerida(Request) :-
  catch(http_parameters(Request, [], [form_data(_), on_error(continue)]), _, true),
  http_parameters(Request,
    [ a(AStr,[string]), b(BStr,[string]), hora(H,[atom]) ],
    [ form_data(_), on_error(continue) ]),
  normalize_city(AStr, A), normalize_city(BStr, B),
  ( ruta_sugerida(A,B,H,R,T) ->
      route_label(R, RL),
      reply_json_dict(_{from:A,to:B,hour:H,route:R,label:RL,time:T})
  ; reply_json_dict(_{from:A,to:B,hour:H,error:"no_direct_route"}, [status(404)])
  ).
