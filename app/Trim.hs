import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

main =
  Text.strip <$> TIO.getContents >>= TIO.putStr
