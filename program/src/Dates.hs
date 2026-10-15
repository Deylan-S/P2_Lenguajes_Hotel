module Dates
    ( dayOfWeek
    , nextDay
    , formatDate
    , isValidDate
    ) where

import Types (Date)
import Utils.DateUtils (addDays, dayOfWeek, formatDate, isValidDate)

-- Objetivo: obtener el dia siguiente a una fecha
-- Entradas: una fecha valida
-- Salidas: la fecha del dia siguiente
-- Restricciones: este modulo es el que importan Rates y Reservations; el resto de funciones
--                (dayOfWeek con 0 domingo a 6 sabado, formatDate dd/mm/aaaa, isValidDate)
--                se reexportan desde Utils.DateUtils
nextDay :: Date -> Date
nextDay = addDays 1
