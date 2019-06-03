module Data.Notification exposing (..)

type NotificationId = Id Int

type alias Notification =
  { id   : NotificationId 
  , text : String }
