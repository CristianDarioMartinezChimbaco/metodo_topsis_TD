# TOPSIS

Sistema de decisión multicriterio desarrollado en **COBOL**, que implementa el método **TOPSIS (Technique for Order Preference by Similarity to Ideal Solution)**.
Permite evaluar alternativas con múltiples criterios y calcular el **índice de cercanía relativa** que clasifica las opciones según su similitud con la solución ideal.

---

## Descripción general

El programa procesa datos de criterios, pesos y tipo de solución (beneficio o costo) desde archivos **CSV** o mediante ingreso manual.
Genera los cálculos del método TOPSIS y muestra los resultados directamente en consola.

---

## Archivos de entrada y salida

| Archivo          | Descripción                                                  | Ejemplo                     |
| ---------------- | ------------------------------------------------------------ | --------------------------- |
| `PESOS.csv`      | Contiene los pesos de los criterios.                         | `0.3,0.2,0.5`               |
| `PAGOS.csv`      | Matriz de decisión (valores de cada alternativa).            | `2500,3200,4000`            |
| `SOLUCIONES.csv` | Tipo de criterio: BENEFICIO o COSTO.                         | `BENEFICIO,COSTO,BENEFICIO` |
| `SOLUCIONES.csv` | También puede escribirse automáticamente según la ejecución. |                             |

---

## Requisitos

* **Compilador:** [GnuCOBOL 3.1 o superior](https://gnucobol.sourceforge.io/)
* **Sistema operativo:** Linux, Windows o macOS
* **Terminal:** Soporta entrada/salida estándar (para mostrar el arte ASCII y menús)

---

## Ejecución

### 1. Compilar el programa

```bash
cobc -x main.cob -o main
```

### 2. Ejecutar

```bash
./main
```

### 3. Menú principal

Al ejecutar, el programa mostrará un menú ASCII:

```
SELECCIONE [1] LEER CSV [2] MANUALMENTE
```

* Opción **1:** Lee `PESOS.csv`, `PAGOS.csv`, y `SOLUCIONES.csv`.La idea es que puedas editar estos archivos csv ingresando tus propias matrices, ten cuidado de que el numero de criterios y de pesos sea el mismo para que todo funcione correctamente.
* Opción **2:** Permite ingresar los datos directamente desde el teclado.

---

## Etapas del método TOPSIS implementadas

1. **Lectura y separación de datos** (`LEER-ARCHIVO`, `SEPARAR-PESOS`, `SEPARAR-PAGOS`, `SEPARAR-SOLUCIONES`)
2. **Normalización de la matriz de decisión**
3. **Ponderación de criterios según pesos**
4. **Cálculo de soluciones ideales positiva y negativa**
5. **Cálculo de las separaciones (positiva y negativa)**
6. **Obtención del índice relativo (IR)**
7. **Clasificación de alternativas**

---

## Fórmulas clave

**Normalización:**
rᵢⱼ = xᵢⱼ / √(Σ (xᵢⱼ²) para i = 1 hasta m)

**Matriz ponderada:**
vᵢⱼ = wⱼ × rᵢⱼ

**Distancia a la solución ideal positiva:**
S⁺ᵢ = √(Σ (vᵢⱼ − v⁺ⱼ)²)

**Distancia a la solución ideal negativa:**
S⁻ᵢ = √(Σ (vᵢⱼ − v⁻ⱼ)²)

**Índice de cercanía relativa:**
Cᵢ = S⁻ᵢ / (S⁺ᵢ + S⁻ᵢ)

## Autor

**Cristian Darío Martínez Chimbaco**
Desarrollador del sistema TOPSIS-RIM en COBOL.

---

## Licencia

Proyecto distribuido bajo la licencia **MIT**, de libre uso académico y educativo.
