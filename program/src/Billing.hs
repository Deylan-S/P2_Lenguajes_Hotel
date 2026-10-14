module Billing
    ( taxRate
    , invoiceReservation
    , formatInvoice
    , printInvoice
    ) where

import Control.Exception (SomeException, evaluate, try)
import Data.List (find, intercalate)
import HotelDM (getHotelInfo)
import InvoicesDM (addInvoice, getInvoices)
import ReservationDM (changeReservationStatus, getReservations)
import Reservations (formatMoney, reservationDetailLines)
import Types

-- Objetivo: impuesto de ventas
-- Entradas: ninguna
-- Salidas: 0.13 (13%)
-- Restricciones: ninguna
taxRate :: Double
taxRate = 0.13

-- Objetivo: facturar una reservación activa y cambiarla a estado Invoiced
-- Entradas: el número de la reservación
-- Salidas: Right con la factura creada y guardada, o Left con el mensaje si la reservación
--          no existe, ya fue facturada o está anulada
-- Restricciones: solo se pueden facturar reservaciones en estado Active
invoiceReservation :: Int -> IO (Either String Invoice)
invoiceReservation targetId = do
    reservations <- getReservations
    case find (\r -> resId r == targetId) reservations of
        Nothing -> pure (Left ("No existe una reservación con el número " ++ show targetId))
        Just reservation
            | resStatus reservation == Active -> do
                invoices <- getInvoices
                let invoice = makeInvoice (nextInvoiceId invoices) reservation
                addInvoice invoice
                changeReservationStatus targetId Invoiced
                pure (Right invoice)
            | resStatus reservation == Invoiced ->
                pure (Left ("La reservación " ++ show targetId ++ " ya fue facturada"))
            | otherwise ->
                pure (Left ("La reservación " ++ show targetId ++ " está anulada y no se puede facturar"))

-- Objetivo: calcular los montos de la factura de una reservación
-- Entradas: el identificador de la nueva factura y la reservación
-- Salidas: la factura con subtotal (monto de la reservación), impuesto (13% del subtotal) y total
-- Restricciones: el impuesto se redondea a dos decimales
makeInvoice :: Int -> Reservation -> Invoice
makeInvoice newId reservation = Invoice
    { invId            = newId
    , invReservationId = resId reservation
    , invSubtotal      = subtotal
    , invTax           = tax
    , invTotal         = subtotal + tax
    }
  where
    subtotal = resTotal reservation
    tax      = roundTwo (subtotal * taxRate)

-- Objetivo: calcular el identificador de la próxima factura
-- Entradas: las facturas guardadas
-- Salidas: 1 si no hay ninguna, o el mayor identificador más 1
-- Restricciones: ninguna
nextInvoiceId :: [Invoice] -> Int
nextInvoiceId invoices =
    if null invoices
    then 1
    else maximum (map invId invoices) + 1

-- Objetivo: redondear un monto a dos decimales
-- Entradas: un monto
-- Salidas: el monto redondeado
-- Restricciones: ninguna
roundTwo :: Double -> Double
roundTwo amount = fromIntegral (round (amount * 100) :: Integer) / 100

-- Objetivo: armar el texto de una factura
-- Entradas: la información del hotel (si se pudo leer), la reservación facturada y la factura
-- Salidas: la factura con información del local, detalle de la
--          reservación, subtotal, impuesto y total
-- Restricciones: si no hay información del hotel se muestra un aviso en su lugar
formatInvoice :: Maybe HotelInfo -> Reservation -> Invoice -> String
formatInvoice maybeHotel reservation invoice = intercalate "\n" $
    [ "================== FACTURA =================="
    , "Identificador de factura:   " ++ show (invId invoice)
    , "Identificador de reserva:   " ++ show (invReservationId invoice)
    , ""
    , "--- Información del local ---"
    ]
    ++ hotelLines
    ++ [ "", "--- Detalle de la reservación ---" ]
    ++ reservationDetailLines reservation
    ++ [ ""
       , "--- Montos ---"
       , amountLine "Subtotal:" (invSubtotal invoice)
       , amountLine ("Impuesto de ventas (" ++ show (round (taxRate * 100) :: Int) ++ "%):") (invTax invoice)
       , amountLine "Total:" (invTotal invoice)
       , "============================================"
       ]
  where
    amountLine label amount = label ++ replicate (28 - length label) ' ' ++ formatMoney amount
    hotelLines = case maybeHotel of
        Just hotel ->
            [ "Nombre:           " ++ hotelName hotel
            , "Cédula jurídica:  " ++ hotelId hotel
            , "Sitio web:        " ++ hotelWebsite hotel
            , "Teléfono:         " ++ hotelPhone hotel
            , "País:             " ++ hotelCountry hotel
            , "Provincia:        " ++ hotelProvince hotel
            ]
        Nothing -> [ "(Información del local no disponible)" ]

-- Objetivo: mostrar una factura
-- Entradas: una factura ya guardada
-- Salidas: ninguna (imprime la factura con la información del hotel y el detalle de su reservación)
-- Restricciones: si no se puede leer la información del hotel, la factura se muestra igual
--                con un aviso en la sección del local
printInvoice :: Invoice -> IO ()
printInvoice invoice = do
    reservations <- getReservations
    hotelResult <- try (getHotelInfo >>= evaluate) :: IO (Either SomeException HotelInfo)
    let maybeHotel = either (const Nothing) Just hotelResult
    case find (\r -> resId r == invReservationId invoice) reservations of
        Just reservation -> putStrLn (formatInvoice maybeHotel reservation invoice)
        Nothing          -> putStrLn "No se encontró la reservación de esta factura"