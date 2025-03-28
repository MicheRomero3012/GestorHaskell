module Gestor where

import Data.UUID
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)


-- Definición de tipos
data EstadoTarea = Pendiente | Completada deriving (Eq, Show)
data Empleado = Empleado { empleadoId :: UUID, nombre :: String } deriving (Eq, Show)
data Tarea = Tarea { tareaId :: UUID, descripcion :: String, fechaLimite :: String, 
                    prioridad :: Int, estadoTarea :: EstadoTarea, asignadaA :: Maybe UUID } 
                    deriving (Eq, Show)
data Proyecto = Proyecto { proyectoId :: UUID, nombreProyecto :: String, tareasProyecto :: [Tarea] } 
                    deriving (Eq, Show)

-- Función que crea un nuevo proyecto vacío
crearProyecto :: UUID -> String -> Proyecto
crearProyecto pid nombreProyecto = 
    Proyecto { proyectoId = pid, nombreProyecto = nombreProyecto, tareasProyecto = [] }

-- Función para agregar una tarea a un proyecto
agregarTarea :: Proyecto -> Tarea -> Proyecto
agregarTarea proyecto tarea = 
    proyecto { tareasProyecto = tarea : tareasProyecto proyecto }

-- Función para asignar una tarea a un empleado
asignarTarea :: Tarea -> UUID -> Tarea
asignarTarea tarea empleadoId = 
    tarea { asignadaA = Just empleadoId }

-- Función para marcar una tarea como completada
marcarTareaCompletada :: Tarea -> Tarea
marcarTareaCompletada tarea = 
    tarea { estadoTarea = Completada }

-- Función para ver todos los proyectos con sus tareas, indicando si están completas o pendientes
verProyectos :: [Proyecto] -> String
verProyectos proyectos = 
    unlines $ map (\proyecto -> nombreProyecto proyecto ++ ": " ++ showTareas (tareasProyecto proyecto)) proyectos
    where
        showTareas tareas = unlines $ map (\tarea -> descripcion tarea ++ " - " ++ show (estadoTarea tarea)) tareas

-- Función para eliminar un proyecto
eliminarProyecto :: [Proyecto] -> UUID -> [Proyecto]
eliminarProyecto proyectos pid = filter (\p -> proyectoId p /= pid) proyectos

-- Función para eliminar una tarea de un proyecto
eliminarTarea :: Proyecto -> UUID -> Proyecto
eliminarTarea proyecto tid = 
    proyecto { tareasProyecto = filter (\t -> tareaId t /= tid) (tareasProyecto proyecto) }

-- Función para contar las tareas completadas y pendientes en un proyecto
contarTareas :: Proyecto -> (Int, Int)
contarTareas proyecto = 
    let (completadas, pendientes) = foldr (\tarea (com, pen) -> 
                                            if estadoTarea tarea == Completada then (com + 1, pen) 
                                            else (com, pen + 1)) 
                                            (0, 0) 
                                            (tareasProyecto proyecto)
    in (completadas, pendientes)

-- Función para actualizar una tarea en un proyecto
actualizarTarea :: Proyecto -> UUID -> Tarea -> Proyecto
actualizarTarea proyecto tid tareaActualizada =
    let tareasActualizadas = map (\tarea -> 
                                    if tareaId tarea == tid 
                                    then tareaActualizada 
                                    else tarea) 
                                (tareasProyecto proyecto)
    in proyecto { tareasProyecto = tareasActualizadas }

-- Función para actualizar el estado de una tarea
actualizarEstadoTarea :: Proyecto -> UUID -> EstadoTarea -> Proyecto
actualizarEstadoTarea proyecto tid nuevoEstado =
    let tareasActualizadas = map (\tarea -> 
                                    if tareaId tarea == tid 
                                    then tarea { estadoTarea = nuevoEstado } 
                                    else tarea) 
                                (tareasProyecto proyecto)
    in proyecto { tareasProyecto = tareasActualizadas }

-- Función para actualizar un proyecto (asignar empleado a tarea)
actualizarProyecto :: Proyecto -> UUID -> UUID -> Proyecto
actualizarProyecto proyecto tid eid = 
    let tareasActualizadas = map (\tarea -> 
                                    if tareaId tarea == tid 
                                    then tarea { asignadaA = Just eid } 
                                    else tarea) 
                                (tareasProyecto proyecto)
    in proyecto { tareasProyecto = tareasActualizadas }