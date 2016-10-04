
import Lib

import Criterion.Main


benches = defaultMain
    [ bgroup "Domain parser"
        [ bench "Domain parser" $ whnf id 0
        ]
    ]

main = do
    benches

