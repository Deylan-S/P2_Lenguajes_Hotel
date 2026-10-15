module Types where

data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  } deriving (Eq, Ord, Show, Read)

data DateTime = DateTime
  { dtDate :: Date
  , dtHour :: Int
  , dtMin  :: Int
  } deriving (Eq, Ord, Show, Read)

data HotelInfo = HotelInfo
  { hotelName     :: String
  , hotelId  :: String
  , hotelWebsite  :: String
  , hotelPhone    :: String
  , hotelCountry  :: String
  , hotelProvince :: String
  } deriving (Eq, Show, Read)

data RoomType = RoomType
  { rtName        :: String
  , rtDescription :: String
  , rtMaxGuests   :: Int
  } deriving (Eq, Show, Read)

data Room = Room
  { roomId   :: Int
  , roomType :: String
  } deriving (Eq, Show, Read)

--   1 verde semana adulto   |  2 verde semana niño
--   3 verde finde adulto    |  4 verde finde niño
--   5 alta semana adulto    |  6 alta semana niño
--   7 alta finde adulto     |  8 alta finde niño
data Rate = Rate
  { rateId     :: Int
  , rateAmount :: Double
  } deriving (Eq, Show, Read)

data ReservationStatus = Active | Invoiced | Cancelled
  deriving (Eq, Show, Read)

data RoomOccupancy = RoomOccupancy
  { occRoomId   :: Int
  , occRoomType :: String
  , occAdults   :: Int
  , occChildren :: Int
  } deriving (Eq, Show, Read)

type RoomRequest = (String, Int, Int)

data Reservation = Reservation
  { resId          :: Int
  , resGuestName   :: String
  , resCreatedAt   :: DateTime
  , resCheckIn     :: Date
  , resCheckOut    :: Date
  , resAdults      :: Int
  , resChildren    :: Int
  , resStatus      :: ReservationStatus
  , resOccupancies :: [RoomOccupancy]
  , resTotal       :: Double
  } deriving (Eq, Show, Read)

data Invoice = Invoice
  { invId            :: Int
  , invReservationId :: Int
  , invSubtotal      :: Double
  , invTax           :: Double
  , invTotal         :: Double
  } deriving (Eq, Show, Read)

data System = System
  { sysHotel             :: Maybe HotelInfo
  , sysRoomTypes         :: [RoomType]
  , sysRooms             :: [Room]
  , sysRates             :: [Rate]
  , sysReservations      :: [Reservation]
  , sysInvoices          :: [Invoice]
  , sysNextReservationId :: Int
  , sysNextInvoiceId     :: Int
  } deriving (Show, Read)

initialSystem :: System

initialSystem = System
  { sysHotel             = Nothing
  , sysRoomTypes         = []
  , sysRooms             = []
  , sysRates             = []
  , sysReservations      = []
  , sysInvoices          = []
  , sysNextReservationId = 1
  , sysNextInvoiceId     = 1
  }