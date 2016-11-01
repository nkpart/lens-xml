
import Text.XML.Light
import Text.XML.Light.Lens
import Control.Lens

main :: IO ()
main =
  let v = parseXML sample
   in print $ v ^.. traverse . _Elem . elNameL . qNameL

sample =
  "<a>5</a>"
