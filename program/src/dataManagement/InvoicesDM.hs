module InvoicesDM (getInvoices, addInvoice) where

import Control.Exception (evaluate)
import Types (Invoice (..))

filePath :: FilePath
filePath = "program/data/invoicesDB.txt"

getInvoices :: IO [Invoice]
getInvoices = do
    contents <- readFile filePath
    _ <- evaluate (length contents) -- evaluar la longitud para forzar que Haskell no sea perezoso.
    pure (read contents)

addInvoice :: Invoice -> IO ()
addInvoice newInvoice = do
    invoices <- getInvoices
    let updatedInvoices = newInvoice : invoices
    writeFile filePath (show updatedInvoices)