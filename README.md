# Simulador de Máquinas Expendedoras en Clojure

Este proyecto implementa un simulador de máquinas expendedoras con soporte para procesamiento paralelo de múltiples máquinas.

## Archivos

### Simulador de una máquina (original)
- `main.clj` - Simulador básico para una sola máquina expendedora
- `maquina.txt` - Configuración de la máquina (productos y dinero inicial)
- `transacciones.txt` - Lista de transacciones a procesar

### Simulador paralelo de múltiples máquinas
- `simulador_paralelo.clj` - Simulador con soporte para múltiples máquinas y procesamiento paralelo
- `config_maquinas.txt` - Configuración que define las máquinas y sus archivos de configuración
- `maquina1.txt`, `maquina2.txt`, `maquina3.txt` - Configuración individual de cada máquina
- `transacciones_multiples.txt` - Transacciones agrupadas por máquina

## Ejecución

### Requisitos
- Java 17 o superior
- Clojure 1.11.1 (o usar los JARs de clojure directamente)

### Ejecutar simulador de una máquina
```bash
java -cp "clojure-1.11.1.jar:spec.alpha-0.3.218.jar:core.specs.alpha-0.2.62.jar:." clojure.main main.clj
```

### Ejecutar simulador paralelo
```bash
java -cp "clojure-1.11.1.jar:spec.alpha-0.3.218.jar:core.specs.alpha-0.2.62.jar:." clojure.main simulador_paralelo.clj
```

## Formato de archivos de configuración

### Configuración de máquina (maquina.txt)
```clojure
(
  (productos
    (A1 precio cantidad)
    (A2 precio cantidad)
    ...)
  (dinero
    (denominacion cantidad)
    ...)
)
```

### Transacciones (transacciones.txt)
```clojure
(
  (venta SLOT (denominaciones...))
  (resurtir SLOT cantidad)
  (retiro-dinero (denominaciones...))
)
```

### Configuración de múltiples máquinas (config_maquinas.txt)
```clojure
(
  (id-maquina1 "archivo-config1.txt")
  (id-maquina2 "archivo-config2.txt")
  ...
)
```

### Transacciones para múltiples máquinas (transacciones_multiples.txt)
```clojure
(
  (id-maquina1
    (venta SLOT (denominaciones...))
    (resurtir SLOT cantidad)
    ...)
  (id-maquina2
    (venta SLOT (denominaciones...))
    ...)
)
```

## Funcionalidades del simulador paralelo

1. **Procesamiento paralelo**: Usa `pmap` de Clojure para procesar transacciones de múltiples máquinas concurrentemente
2. **Gestión de estado**: Usa atoms para manejo seguro de estado concurrente
3. **Resultados finales**:
   - Ganancia total del negocio
   - Top 10% de máquinas con mayor ganancia
   - Máquinas que necesitan resurtido de producto (punto de reorden: 2 unidades)
   - Máquinas que necesitan resurtido de dinero (punto de reorden: 5 unidades)
   - Máquinas que necesitan retiro de dinero (límite: 30 unidades)

## Configuración de umbrales

Los umbrales se pueden modificar en `simulador_paralelo.clj`:
- `punto-reorden-producto`: 2 (stock mínimo antes de alertar resurtido)
- `punto-reorden-dinero`: 5 (cantidad mínima de cada denominación)
- `limite-retiro-dinero`: 30 (cantidad máxima antes de alertar retiro)
