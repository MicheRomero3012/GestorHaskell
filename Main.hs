-- Definición de tipos
import Data.UUID (UUID, nil, toString)
import Data.UUID.V4 (nextRandom)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import System.IO ( hSetBuffering, stdout, BufferMode(NoBuffering) )
import Control.Monad (when)

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

-- Funciones auxiliares para mostrar listas numeradas
mostrarProyectosNumerados :: [Proyecto] -> IO ()
mostrarProyectosNumerados proyectos = do
    putStrLn "\nProyectos disponibles:"
    mapM_ (\(i, p) -> putStrLn $ show i ++ ". " ++ nombreProyecto p) (zip [1..] proyectos)

mostrarTareasNumeradas :: Proyecto -> IO ()
mostrarTareasNumeradas proyecto = do
    putStrLn $ "\nTareas del proyecto '" ++ nombreProyecto proyecto ++ "':"
    mapM_ (\(i, t) -> putStrLn $ show i ++ ". " ++ descripcion t ++ 
                      " (" ++ (if completada t then "Completada" else "Pendiente") ++ ")") 
          (zip [1..] (tareasProyecto proyecto))

mostrarEmpleadosNumerados :: [Empleado] -> IO ()
mostrarEmpleadosNumerados empleados = do
    putStrLn "\nEmpleados disponibles:"
    mapM_ (\(i, e) -> putStrLn $ show i ++ ". " ++ nombreEmpleado e) (zip [1..] empleados)

mostrarEmpleado :: Empleado -> String
mostrarEmpleado emp = nombreEmpleado emp ++ " (ID: " ++ toString (empleadoId emp) ++ ")"    

-- Función principal de prueba
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Sistema de Gestión de Proyectos - Haskell"
    loop [] []  -- Listas vacías para proyectos y empleados iniciales

loop :: [Proyecto] -> [Empleado] -> IO ()
loop proyectos empleados = do
    putStrLn "\nMenú Principal:"
    putStrLn "1. Crear nuevo proyecto"
    putStrLn "2. Listar todos los proyectos"
    putStrLn "3. Agregar tarea a proyecto"
    putStrLn "4. Asignar tarea a empleado"
    putStrLn "5. Marcar tarea como completada"
    putStrLn "6. Crear nuevo empleado"
    putStrLn "7. Listar empleados"
    putStrLn "8. Mostrar estadísticas de proyecto"
    putStrLn "9. Eliminar tarea"
    putStrLn "0. Salir"
    putStr "Seleccione una opción: "
    
    opcion <- getLine
    
    case opcion of
        "1" -> do
            putStr "Nombre del proyecto: "
            nombre <- getLine
            putStr "Fecha de inicio (YYYY-MM-DD): "
            inicio <- getLine
            putStr "Fecha de fin (YYYY-MM-DD): "
            fin <- getLine
            nuevoProyecto <- crearProyecto nombre inicio fin
            loop (nuevoProyecto:proyectos) empleados
        
        "2" -> do
            if null proyectos
                then putStrLn "No hay proyectos registrados."
                else mostrarProyectos proyectos
            loop proyectos empleados
        
        "3" -> do
            if null proyectos
                then putStrLn "Primero debe crear un proyecto."
                else do
                    mostrarProyectosNumerados proyectos
                    putStr "Seleccione el número del proyecto: "
                    numProyecto <- getLine
                    case reads numProyecto of
                        [(n, _)] | n > 0 && n <= length proyectos -> do
                            let proyecto = proyectos !! (n - 1)
                            putStr "Descripción de la tarea: "
                            desc <- getLine
                            putStr "Fecha límite (YYYY-MM-DD): "
                            fecha <- getLine
                            putStr "Prioridad (1-5): "
                            prioridadStr <- getLine
                            case reads prioridadStr of
                                [(p, _)] | p >= 1 && p <= 5 -> do
                                    nuevaTarea <- crearTarea desc fecha p
                                    let proyectoActualizado = agregarTarea nuevaTarea proyecto
                                    let proyectosActualizados = take (n - 1) proyectos ++ [proyectoActualizado] ++ drop n proyectos
                                    putStrLn "Tarea agregada con éxito!"
                                    loop proyectosActualizados empleados
                                _ -> putStrLn "Prioridad inválida. Debe ser entre 1 y 5."
                        _ -> putStrLn "Número de proyecto inválido."
            loop proyectos empleados
        
        "4" -> do
            if null empleados
                then putStrLn "No hay empleados registrados."
                else if null proyectos
                    then putStrLn "No hay proyectos registrados."
                    else do
                        mostrarProyectosNumerados proyectos
                        putStr "Seleccione el número del proyecto: "
                        numProyecto <- getLine
                        case reads numProyecto of
                            [(n, _)] | n > 0 && n <= length proyectos -> do
                                let proyecto = proyectos !! (n - 1)
                                mostrarTareasNumeradas proyecto
                                putStr "Seleccione el número de la tarea: "
                                numTarea <- getLine
                                mostrarEmpleadosNumerados empleados
                                putStr "Seleccione el número del empleado: "
                                numEmpleado <- getLine
                                case (reads numTarea, reads numEmpleado) of
                                    ([(t, _)], [(e, _)]) | t > 0 && t <= length (tareasProyecto proyecto) && 
                                                        e > 0 && e <= length empleados -> do
                                        let tareaID = tareaId (tareasProyecto proyecto !! (t - 1))  -- Cambiado a tareaID
                                        let empleadoID = empleadoId (empleados !! (e - 1))           -- Cambiado a empleadoID
                                        let proyectoActualizado = asignarTarea tareaID empleadoID proyecto
                                        let proyectosActualizados = take (n - 1) proyectos ++ [proyectoActualizado] ++ drop n proyectos
                                        putStrLn "Tarea asignada con éxito!"
                                        loop proyectosActualizados empleados
                                    _ -> putStrLn "Números inválidos."
        
        "5" -> do
            if null proyectos
                then putStrLn "No hay proyectos registrados."
                else do
                    mostrarProyectosNumerados proyectos
                    putStr "Seleccione el número del proyecto: "
                    numProyecto <- getLine
                    case reads numProyecto of
                        [(n, _)] | n > 0 && n <= length proyectos -> do
                            let proyecto = proyectos !! (n - 1)
                            mostrarTareasNumeradas proyecto
                            putStr "Seleccione el número de la tarea a completar: "
                            numTarea <- getLine
                            case reads numTarea of
                                [(t, _)] | t > 0 && t <= length (tareasProyecto proyecto) -> do
                                    let tareaID = tareaId (tareasProyecto proyecto !! (t - 1))  -- Cambiado a tareaID
                                    let proyectoActualizado = marcarTareaCompletada tareaID proyecto
                                    let proyectosActualizados = take (n - 1) proyectos ++ [proyectoActualizado] ++ drop n proyectos
                                    putStrLn "Tarea marcada como completada!"
                                    loop proyectosActualizados empleados
                                _ -> putStrLn "Número de tarea inválido."

        "6" -> do
            putStr "Nombre del empleado: "
            nombre <- getLine
            nuevoEmpleadoId <- nextRandom
            let nuevoEmpleado = Empleado { empleadoId = nuevoEmpleadoId, nombreEmpleado = nombre }
            putStrLn $ "Empleado creado: " ++ nombre
            loop proyectos (nuevoEmpleado:empleados)
        
        "7" -> do
            if null empleados
                then putStrLn "No hay empleados registrados."
                else do
                    putStrLn "\nLista de empleados:"
                    mapM_ (putStrLn . mostrarEmpleado) empleados
            loop proyectos empleados
        
        "8" -> do
            if null proyectos
                then putStrLn "No hay proyectos registrados."
                else do
                    mostrarProyectosNumerados proyectos
                    putStr "Seleccione el número del proyecto: "
                    numProyecto <- getLine
                    case reads numProyecto of
                        [(n, _)] | n > 0 && n <= length proyectos -> do
                            let proyecto = proyectos !! (n - 1)
                            let (completadas, pendientes) = contarTareas proyecto
                            putStrLn $ "\nEstadísticas del proyecto '" ++ nombreProyecto proyecto ++ "':"
                            putStrLn $ "Tareas completadas: " ++ show completadas
                            putStrLn $ "Tareas pendientes: " ++ show pendientes
                        _ -> putStrLn "Número de proyecto inválido."
            loop proyectos empleados
        
        "9" -> do
            if null proyectos
                then putStrLn "No hay proyectos registrados."
                else do
                    mostrarProyectosNumerados proyectos
                    putStr "Seleccione el número del proyecto: "
                    numProyecto <- getLine
                    case reads numProyecto of
                        [(n, _)] | n > 0 && n <= length proyectos -> do
                            let proyecto = proyectos !! (n - 1)
                            mostrarTareasNumeradas proyecto
                            putStr "Seleccione el número de la tarea a eliminar: "
                            numTarea <- getLine
                            case reads numTarea of
                                [(t, _)] | t > 0 && t <= length (tareasProyecto proyecto) -> do
                                    let tareaID = tareaId (tareasProyecto proyecto !! (t - 1))  -- Cambiado a tareaID
                                    let proyectoActualizado = eliminarTarea tareaID proyecto
                                    let proyectosActualizados = take (n - 1) proyectos ++ [proyectoActualizado] ++ drop n proyectos
                                    putStrLn "Tarea eliminada con éxito!"
                                    loop proyectosActualizados empleados
                                _ -> putStrLn "Número de tarea inválido."
                                    
        "0" -> putStrLn "Saliendo del sistema..."
        
        _ -> do
            putStrLn "Opción no válida. Intente nuevamente."
            loop proyectos empleados