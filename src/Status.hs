module Status where

import FileManager
import UserManager

data Status = Status { currDir :: Path
                     , fileStructure :: FileStructure
                     , currUser :: User
                     }