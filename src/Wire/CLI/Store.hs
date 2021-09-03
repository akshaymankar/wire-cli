module Wire.CLI.Store
  ( Store (..),
    saveCreds,
    getCreds,
    saveConvs,
    getConvs,
    saveClientId,
    getClientId,
    saveLastNotificationId,
    getLastNotificationId,
    getConnections,
    saveConnections,
    addConnection,
    addMessage,
    getLastNMessages,
    saveSelf,
    getSelf,
    StoredMessage (..),
    StoredMessageData (..),
    decodeMessage,
    isLoggedIn,
    getCredsOrErr,
  )
where

import Wire.CLI.Store.Effect
import Wire.CLI.Store.StoredMessage
