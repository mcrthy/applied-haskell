-- EXERCISE: Define the deepseq function yourself in terrms of rnf and seq.

import Control.DeepSeq

deepseq' :: NFData a => a -> b -> b
deepseq' x y = (rnf x) `seq` y

