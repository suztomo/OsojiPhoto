import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Prelude              (IO)
import Settings             (parseExtra)
import Application          (getApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) getApplication
