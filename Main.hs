module Main where

import Gestor
import Data.UUID.V4 (nextRandom)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    -- Crear un proyecto
    pid <- nextRandom
    let proyecto = crearProyecto pid "Proyecto Principal con Django"
    
    -- Crear algunas tareas
    tid1 <- nextRandom
    tid2 <- nextRandom
    eid1 <- nextRandom
    
    let tarea1 = Tarea {
        tareaId = tid1,
        descripcion = "Diseñar interfaz",
        fechaLimite = "2025-12-15",
        prioridad = 1,
        estadoTarea = Pendiente,
        asignadaA = Nothing
    }
    
    let tarea2 = Tarea {
        tareaId = tid2,
        descripcion = "Implementar backend",
        fechaLimite = "2025-12-20",
        prioridad = 2,
        estadoTarea = Pendiente,
        asignadaA = Nothing
    }
    
    -- Agregar tareas al proyecto
    let proyecto' = agregarTarea (agregarTarea proyecto tarea1) tarea2
    
    -- Asignar una tarea a un empleado
    let proyecto'' = actualizarProyecto proyecto' tid1 eid1
    
    -- Marcar una tarea como completada
    let proyecto''' = actualizarEstadoTarea proyecto'' tid1 Completada
    
    -- Mostrar el proyecto
    putStrLn $ verProyectos [proyecto''']
    
    -- Contar tareas
    let (completas, pendientes) = contarTareas proyecto'''
    putStrLn $ "Tareas completadas: " ++ show completas
    putStrLn $ "Tareas pendientes: " ++ show pendientes