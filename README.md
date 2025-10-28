# Tránsito y Rutas Viales de Costa Rica

Aplicación educativa desarrollada en **SWI-Prolog** que permite consultar, analizar y visualizar la **red vial de Costa Rica** a partir de un archivo **GeoJSON**.  
Ofrece una **interfaz web local** para calcular rutas, tiempos de viaje y conexiones entre ciudades o cantones.

---

## 📘 Descripción general

Este proyecto demuestra cómo utilizar **Prolog** para manejar bases de conocimiento dinámicas a partir de datos geográficos.  
Permite explorar las rutas primarias y secundarias de Costa Rica, consultando la distancia y el tiempo estimado entre distintos puntos.

**Características principales:**

- Carga automática de un archivo `GeoJSON` con los tramos viales.  
- Consulta de **rutas directas** entre dos ciudades.  
- Cálculo de la **ruta más rápida** considerando hora pico o valle.  
- Función para **múltiples paradas** consecutivas.   
- Interfaz web ligera con HTML + CSS integrada.

---

## 🧩 Requisitos del sistema

### 🧰 Software necesario
- **SWI-Prolog** (versión 9.0 o superior)  
  👉 [Descargar desde swi-prolog.org](https://www.swi-prolog.org/download/stable)
- **Navegador web moderno** (Chrome, Edge, Firefox, etc.)

### 📂 Archivos incluidos
- `server.pl` → Servidor web principal y control de interfaz  
- `reglas.pl` → Base de conocimiento y carga del archivo GeoJSON  
- `rutas.geojson` → Datos con los tramos y conexiones viales  
- `css/style.css` → Estilos visuales del sistema 

---

## ⚙️ Instalación y ejecución

Sigue estos pasos para ejecutar el sistema en tu computadora local:

### 1️⃣ Abrir la carpeta del proyecto
```bash 
    cd proyecto_rutas
```
### 2️⃣ Iniciar SWI-Prolog

### 3️⃣ Cargar el servidor principal
```bash 
?- [server].
```
### 4️⃣ Iniciar el servidor web
```bash 
?- start.
```
Si la configuración es correcta, deberías ver el mensaje (opcional cargar reglas):

Started server at http://localhost:8080/

![inicio](https://github.com/9re9o/RutasVialesCR/blob/7d9037616ded6a80db826dadafabedc8eaed41d6/startServer.png)

### 5️⃣ Abrir en el navegador
Abre tu navegador preferido y visita:
```bash 
👉 http://localhost:8080/
```
![inicio](https://github.com/9re9o/RutasVialesCR/blob/473234a5dce3eaac2e294eb917e6d2459ecef743/image.png)

### 6️⃣ Detener el servidor al finalizar
```bash 
?- stop.
```

## 🧠 Uso del sistema

El sistema cuenta con una interfaz sencilla e intuitiva dividida en varias secciones principales:

---

### 🔹 1. Cargar archivo GeoJSON
Permite cargar el archivo `rutas.geojson`, que contiene los tramos viales del país.  
Cada tramo define su punto de inicio, fin, tipo de vía, distancia, peaje y congestión estimada.  
También puedes escribir el nombre de otro archivo si deseas probar una versión distinta.

---

### 🔹 2. Conexiones directas
Permite consultar todos los tramos directos entre dos ciudades o cantones.  
El sistema mostrará:

- Tipo de carretera (primaria, secundaria, autopista)  
- Presencia o ausencia de peaje  
- Distancia total en kilómetros  
- Tiempo estimado tanto en hora pico como en hora valle  

---

### 🔹 3. Ruta sugerida (más rápida)
Calcula la ruta más eficiente considerando el tipo de carretera, la distancia y la congestión promedio.  
Puedes elegir entre hora pico o hora valle para ajustar el tiempo de viaje estimado.  
La interfaz mostrará la ruta sugerida junto con su tiempo total.

---

### 🔹 4. Ruta con múltiples paradas
Permite ingresar varias ciudades consecutivas (por ejemplo: `San José → Cartago → Turrialba → Limón`).  
El sistema calculará:

- El tiempo parcial de cada tramo  
- El tiempo total acumulado  
- Los tramos inexistentes o no conectados  

Los resultados se muestran en una tabla con indicadores visuales.


## 🔗 API JSON disponible

Además de la interfaz web, el sistema ofrece **endpoints HTTP** para consultas programáticas.

| Endpoint | Ejemplo | Descripción |
|-----------|----------|-------------|
| `/api_conecta?a=A&b=B` | `/api_conecta?a=san_jose&b=limon` | Devuelve las rutas directas entre dos ciudades. |
| `/api_sugerida?a=A&b=B&hora=H` | `/api_sugerida?a=san_jose&b=caldera&hora=hora_pico` | Devuelve la ruta más rápida entre A y B. |

---

### 🧾 Ejemplo de respuesta JSON
```json
{
  "from": "san_jose",
  "to": "caldera",
  "hour": "hora_valle",
  "route": "r27",
  "label": "Ruta 27",
  "time": 1.42
}
```

## 🧩 Base de conocimiento (`reglas.pl`)

El archivo `reglas.pl` contiene toda la lógica declarativa del sistema y las reglas que permiten interpretar los datos del archivo **GeoJSON**.

---

### 🧠 Hechos definidos
```prolog
segmento(Ruta, Desde, Hacia, Distancia, Tipo, Peaje).
```
Cada hecho representa un tramo de carretera entre dos puntos del país.

### ⚖️ Reglas principales
```prolog
load_kb_geojson(File).             % Carga un archivo GeoJSON y genera hechos dinámicos.
conecta(A, B, Ruta).               % Determina si existe conexión directa entre A y B.
distancia_directa(A, B, D).        % Devuelve la distancia total entre dos puntos.
tiempo_directo(A, B, Hora, Ruta, Tiempo). % Calcula el tiempo según tipo de vía y hora.
ruta_sugerida(A, B, Hora, Ruta, Tiempo).  % Selecciona la ruta más rápida entre dos ciudades.

```



