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
    StoredMessage (..),
  )
where

import Wire.CLI.Store.Effect
import Wire.CLI.Store.StoredMessage
