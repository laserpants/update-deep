module Data.Notification exposing (Notification, NotificationId(..))


type NotificationId
    = Id Int


type alias Notification =
    { id : NotificationId
    , text : String
    }
