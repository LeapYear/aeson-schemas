import Criterion.Main

import qualified Benchmarks.FromJSON
import qualified Benchmarks.SchemaQQ
import qualified Benchmarks.Show
import qualified Benchmarks.ToJSON

main :: IO ()
main =
  defaultMain
    [ Benchmarks.SchemaQQ.benchmarks
    , Benchmarks.Show.benchmarks
    , Benchmarks.FromJSON.benchmarks
    , Benchmarks.ToJSON.benchmarks
    ]
