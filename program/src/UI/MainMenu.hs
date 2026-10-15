module UI.MainMenu
    ( runMainMenu
    ) where

import MenuManagement.MenuEngine (MenuOption, runMenu)
import UI.AdminActions
import UI.Common (runAction)
import UI.GeneralActions
import UI.StatisticsActions

-- Objetivo: mostrar el menu principal del sistema
-- Entradas: ninguna
-- Salidas: termina cuando el usuario elige salir
-- Restricciones: ninguna
runMainMenu :: IO ()
runMainMenu = runMenu "MENÚ PRINCIPAL" mainOptions

-- Objetivo: opciones del menu principal
-- Entradas: ninguna
-- Salidas: lista de opciones (texto, accion)
-- Restricciones: ninguna
mainOptions :: [MenuOption]
mainOptions =
    [ ("Opciones Administrativas", runAdminMenu)
    , ("Opciones Generales", runGeneralMenu)
    ]

-- Objetivo: mostrar el menu administrativo
-- Entradas: ninguna
-- Salidas: termina cuando el usuario vuelve al menu principal
-- Restricciones: ninguna
runAdminMenu :: IO ()
runAdminMenu = runMenu "MENÚ ADMINISTRATIVO" adminOptions

-- Objetivo: opciones del menu administrativo
-- Entradas: ninguna
-- Salidas: lista de opciones (texto, accion)
-- Restricciones: cada accion se ejecuta de forma segura y con pausa final
adminOptions :: [MenuOption]
adminOptions =
    [ ("Cargar tipo de habitaciones", runAction actionLoadRoomTypes)
    , ("Asignar Cantidad de habitaciones por tipo", runAction actionAssignRooms)
    , ("Ver habitaciones", runAction actionViewRooms)
    , ("Carga de Tarifas", runAction actionLoadRates)
    , ("Consultar Reservaciones", runAction actionConsultReservations)
    , ("Consultar estado de ocupación", runAction actionConsultOccupancy)
    , ("Estadísticas de ocupación", runStatisticsMenu)
    ]

-- Objetivo: mostrar el submenu de estadisticas de ocupacion
-- Entradas: ninguna
-- Salidas: termina cuando el usuario vuelve al menu administrativo
-- Restricciones: ninguna
runStatisticsMenu :: IO ()
runStatisticsMenu = runMenu "ESTADÍSTICAS DE OCUPACIÓN" statisticsOptions

-- Objetivo: opciones del submenu de estadisticas
-- Entradas: ninguna
-- Salidas: lista de opciones (texto, accion)
-- Restricciones: cada accion se ejecuta de forma segura y con pausa final
statisticsOptions :: [MenuOption]
statisticsOptions =
    [ ("Total de huéspedes por mes-año", runAction actionStatGuests)
    , ("Habitaciones ocupadas en un día específico", runAction actionStatOccupied)
    , ("Total de habitaciones no ocupadas en un día", runAction actionStatUnoccupied)
    , ("Monto recaudado con impuestos", runAction actionStatTaxes)
    , ("Top 3 de días de la semana con más ocupaciones", runAction actionStatTopDays)
    ]

-- Objetivo: mostrar el menu general
-- Entradas: ninguna
-- Salidas: termina cuando el usuario vuelve al menu principal
-- Restricciones: ninguna
runGeneralMenu :: IO ()
runGeneralMenu = runMenu "MENÚ GENERAL" generalOptions

-- Objetivo: opciones del menu general
-- Entradas: ninguna
-- Salidas: lista de opciones (texto, accion)
-- Restricciones: cada accion se ejecuta de forma segura y con pausa final
generalOptions :: [MenuOption]
generalOptions =
    [ ("Ver disponibilidad", runAction actionViewAvailability)
    , ("Reservación", runAction actionMakeReservation)
    , ("Anular reservación", runAction actionCancelReservation)
    , ("Facturar reservación", runAction actionInvoiceReservation)
    ]
