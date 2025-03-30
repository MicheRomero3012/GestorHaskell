-- Definición de tipos
import Data.UUID (UUID, nil)
import Data.UUID.V4 (nextRandom)
import Data.List (partition)
import Data.Maybe (fromMaybe)

-- Definiendo estructuras de datos
data Tarea = Tarea {
    tareaId :: UUID,
    descripcion :: String,
    fechaLimite :: String,
    prioridad :: Int,
    completada :: Bool,
    asignadoA :: Maybe UUID
} deriving (Show)

data Proyecto = Proyecto {
    proyectoId :: UUID,
    nombreProyecto :: String,
    fechaInicio :: String,
    fechaFin :: String,
    tareasProyecto :: [Tarea]
} deriving (Show)

data Empleado = Empleado {
    empleadoId :: UUID,
    nombreEmpleado :: String
} deriving (Show)

-- Función para crear un proyecto
crearProyecto :: String -> String -> String -> IO Proyecto
crearProyecto nombre inicio fin = do
    uuid <- nextRandom
    return Proyecto { proyectoId = uuid, nombreProyecto = nombre, fechaInicio = inicio, fechaFin = fin, tareasProyecto = [] }

-- Función para agregar una tarea a un proyecto
agregarTarea :: Tarea -> Proyecto -> Proyecto
agregarTarea tarea proyecto =
    proyecto { tareasProyecto = tarea : tareasProyecto proyecto }

-- Función para crear una tarea
crearTarea :: String -> String -> Int -> IO Tarea
crearTarea desc fecha prioridad = do
    uuid <- nextRandom
    return Tarea { tareaId = uuid, descripcion = desc, fechaLimite = fecha, prioridad = prioridad, completada = False, asignadoA = Nothing }

-- Función para asignar una tarea a un empleado
asignarTarea :: UUID -> UUID -> Proyecto -> Proyecto
asignarTarea tareaID empleadoID proyecto =
    let actualizarTarea tarea = if tareaId tarea == tareaID then tarea { asignadoA = Just empleadoID } else tarea
    in proyecto { tareasProyecto = map actualizarTarea (tareasProyecto proyecto) }

-- Función para marcar una tarea como completada
marcarTareaCompletada :: UUID -> Proyecto -> Proyecto
marcarTareaCompletada tareaID proyecto =
    let actualizarTarea tarea = if tareaId tarea == tareaID then tarea { completada = True } else tarea
    in proyecto { tareasProyecto = map actualizarTarea (tareasProyecto proyecto) }

-- Función para contar tareas completadas y pendientes
contarTareas :: Proyecto -> (Int, Int)
contarTareas proyecto =
    let (completadas, pendientes) = partition completada (tareasProyecto proyecto)
    in (length completadas, length pendientes)

-- Función para eliminar una tarea de un proyecto
eliminarTarea :: UUID -> Proyecto -> Proyecto
eliminarTarea tareaID proyecto =
    proyecto { tareasProyecto = filter ((/= tareaID) . tareaId) (tareasProyecto proyecto) }

-- Función para eliminar un proyecto de la lista
eliminarProyecto :: UUID -> [Proyecto] -> [Proyecto]
eliminarProyecto proyectoID = filter ((/= proyectoID) . proyectoId)

-- Función para mostrar proyectos y sus tareas
mostrarProyectos :: [Proyecto] -> IO ()
mostrarProyectos proyectos = mapM_ mostrarProyecto proyectos
  where
    mostrarProyecto p = do
        putStrLn "------ Proyecto ------"
        putStrLn $ "Nombre: " ++ nombreProyecto p
        putStrLn "Tareas:"
        mapM_ mostrarTarea (tareasProyecto p)
    mostrarTarea t = putStrLn $ " - " ++ descripcion t ++ " (" ++ (if completada t then "Completada" else "Pendiente") ++ ")"

-- Función principal de prueba
main :: IO ()
main = do
    putStrLn "------ Creando Proyecto ------"
    proyecto <- crearProyecto "Gestor de PLF" "01-04-2025" "30-04-2025"
    
    putStrLn "------ Creando Tareas ------"
    tarea1 <- crearTarea "Codificar las funciones" "10-04-2025" 1
    tarea2 <- crearTarea "Documentacion" "20-04-2025" 2
    let proyectoConTareas = agregarTarea tarea2 (agregarTarea tarea1 proyecto)
    
    putStrLn "------ Creando Empleado ------"
    empleadoId1 <- nextRandom
    let empleado = Empleado empleadoId1 "Brian Romero"
    
    putStrLn "------ Asignando Tarea ------"
    let proyectoAsignado = asignarTarea (tareaId tarea1) (empleadoId empleado) proyectoConTareas
    
    putStrLn "------ Marcando Tarea Como Completada ------"
    let proyectoCompletado = marcarTareaCompletada (tareaId tarea1) proyectoAsignado
    
    putStrLn "------ Contando Tareas ------"
    let (completadas, pendientes) = contarTareas proyectoCompletado
    putStrLn $ "Completadas: " ++ show completadas ++ " | Pendientes: " ++ show pendientes
    
    putStrLn "------ Eliminando Tarea ------"
    let proyectoFinal = eliminarTarea (tareaId tarea1) proyectoCompletado
    
    putStrLn "------ Mostrando Proyectos Disponibles ------"
    mostrarProyectos [proyectoFinal]
    
    putStrLn "------ Eliminando Proyecto ------"
    let proyectosRestantes = eliminarProyecto (proyectoId proyecto) [proyectoFinal]
    putStrLn "Proyectos restantes:"
    mostrarProyectos proyectosRestantes
