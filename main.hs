import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withFashionAd)

main :: IO ()
main = defaultMain fromArgs withFashionAd