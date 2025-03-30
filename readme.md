# Gestor de Proyectos en Haskell

## Descripción

Este es un sistema de **gestión de proyectos** desarrollado en **Haskell** que permite:

- Crear y administrar proyectos con tareas.
- Asignar tareas a empleados.
- Marcar tareas como completadas o pendientes.
- Visualizar el estado de los proyectos.
- Eliminar tareas y proyectos del sistema.

Este sistema sigue los principios de **programación funcional**, asegurando que todas las estructuras de datos sean **inmutables** y utilizando **tipos algebraicos** (`Maybe`, `Either`, listas) para manejar errores y representar datos.

## Requisitos

Antes de ejecutar el programa, asegúrate de tener instalados los siguientes paquetes y herramientas:

- **GHC** (Glasgow Haskell Compiler) 8.6 o superior.
- **Cabal** (Sistema de build de Haskell).
- **Paquetes adicionales:** `uuid`, `containers`.

### Instalación de dependencias

```bash
# Instalar GHC y Cabal (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install ghc cabal-install

# Instalar paquetes requeridos
cabal update
cabal install --lib uuid containers
```

## Compilación y ejecución

Para compilar y ejecutar el programa, usa los siguientes comandos:

```bash
# Compilar el proyecto
ghc -package containers -package uuid -o gestor Main.hs Gestor.hs

# Ejecutar el programa
./gestor
```

### Ejecutar sin compilar (usando runghc)

```bash
runghc Main.hs
```

### Ejecutar en modo interactivo (GHCI)

```bash
ghci -package containers -package uuid Main.hs
```

## Estructura del código

El código está organizado de la siguiente manera:

- **Main.hs** → Punto de entrada principal con ejemplos de uso.
- **Gestor.hs** → Contiene la lógica del programa:
  - Tipos de datos (`Proyecto`, `Tarea`, `Empleado`).
  - Funciones de gestión de proyectos y tareas.
  - Manejo de estados de tareas y validaciones.

## Ejemplo de ejecución

```haskell
------ Creando Proyecto ------
Proyecto creado: Software Manager
Inicio: 2025-04-01 | Fin: 2025-12-31

------ Creando Tareas ------
Tarea agregada: Implementar Backend | Prioridad: Alta | Fecha Límite: 2025-06-30
Tarea agregada: Diseñar UI | Prioridad: Media | Fecha Límite: 2025-05-15

------ Creando Empleado ------
Empleado registrado: Juan Pérez | Cargo: Desarrollador Backend

------ Asignando Tarea ------
Tarea "Implementar Backend" asignada a Juan Pérez

------ Marcando Tarea Como Completada ------
Tarea "Implementar Backend" completada ✅

------ Contando Tareas ------
Proyecto: Software Manager
Completadas: 1 | Pendientes: 1

------ Mostrando Proyectos Disponibles ------
------ Proyecto ------
Nombre: Software Manager
Inicio: 2025-04-01 | Fin: 2025-12-31
Tareas:
  - Implementar Backend (✅ Completada) | Asignada a: Juan Pérez
  - Diseñar UI (⏳ Pendiente) | No asignada

------ Mostrando Empleados ------
Empleado: Juan Pérez
Tareas asignadas:
  - Implementar Backend (✅ Completada)

------ Eliminando Tarea ------
Tarea "Diseñar UI" eliminada del proyecto.

------ Eliminando Proyecto ------
Proyecto "Software Manager" eliminado.
Proyectos restantes: Ninguno.
```

## Principios de Programación Funcional Aplicados

- **Inmutabilidad** → Los proyectos y tareas nunca se modifican directamente, siempre se generan nuevas estructuras actualizadas.
- **Funciones puras** → Cada función devuelve un nuevo estado sin afectar datos globales.
- **Uso de tipos algebraicos** → `Maybe` y `Either` para manejar errores y datos opcionales.
- **Expresividad** → Código modular, con funciones pequeñas y responsabilidad clara.

## Solución de problemas

### Error de paquetes no encontrados:

```bash
ghc-pkg list  # Ver paquetes instalados
cabal install --force-reinstalls uuid containers
```

### Error de módulos ocultos:

```bash
ghc -package-env=- -package uuid -package containers -o gestor Main.hs Gestor.hs
```

### Limpiar compilaciones previas:

```bash
rm -f *.hi *.o gestor
```

---
