-- EXERCISE: Define the deepseq function yourself in terrms of rnf and seq.

import Control.DeepSeq

deepseq' x y = (rnf x) `seq` y

