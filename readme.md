# Gestor de Proyectos Haskell

## Descripción

Este es un sistema de gestión de proyectos desarrollado en Haskell que permite:

- Crear y administrar proyectos  
- Gestionar tareas con prioridades y fechas límite  
- Asignar tareas a empleados  
- Seguir el progreso de las tareas (pendientes/completadas)  

## Requisitos

- **GHC** (Glasgow Haskell Compiler) 8.6 o superior  
- **Cabal** (sistema de build de Haskell)  
- **Paquetes Haskell:** `uuid`, `containers`  

## Instalación de dependencias

```bash
# Instalar GHC y Cabal (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install ghc cabal-install

# Instalar paquetes requeridos
cabal update
cabal install --lib uuid containers
```

## Compilación y ejecución

```bash
# Compilar el proyecto
ghc -package containers -package uuid -o gestor Main.hs Gestor.hs

# Ejecutar el programa
./gestor
```

## Comandos útiles adicionales

### Ejecutar sin compilar (usando runghc)

```bash
runghc Main.hs
```

### Generar documentación (opcional)

```bash
haddock -h -o docs Gestor.hs
```

### Ejecutar en modo interactivo (GHCI)

```bash
ghci -package containers -package uuid Main.hs
```

## Estructura del código

- **Main.hs:** Punto de entrada principal con ejemplos de uso.  
- **Gestor.hs:** Módulo principal con toda la lógica de negocio, incluyendo:  
  - Tipos de datos para **Proyectos**, **Tareas** y **Empleados**.  
  - Funciones para gestionar el ciclo de vida completo de los proyectos.  

## Ejemplo de uso en código

```haskell
-- Crear proyecto
pid <- nextRandom
proyecto <- crearProyecto pid "Mi Proyecto"

-- Crear tarea
tid <- nextRandom
let tarea = Tarea tid "Implementar login" "2023-12-31" 1 Pendiente Nothing

-- Agregar tarea al proyecto
proyecto' <- agregarTarea proyecto tarea

-- Asignar tarea a empleado
eid <- nextRandom
proyecto'' <- asignarTarea (tareaId tarea) eid proyecto'
```

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