# Wire

This repository is part of the source code of Wire. You can find more information at [wire.com](https://wire.com) or by contacting opensource@wire.com.

You can find the published source code at [github.com/wireapp/wire](https://github.com/wireapp/wire). 

For licensing information, see the attached LICENSE file and the list of third-party licenses at [wire.com/legal/licenses/](https://wire.com/legal/licenses/).

### generic-message-proto

This repository contains the protobuf specifications used in Wire™.

#### Editing a message

If the content of a previously sent message should be edited, a generic message of type `MessageEdit` has to be sent. It should reference the new content (for now only type `Text` can be edited) as well as the nonce of the message it is replacing. If an edit message is received which is referencing a non existent nonce it should be discarded.
