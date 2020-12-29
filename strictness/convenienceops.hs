-- EXERCISE: Define the $! and $!! operators in terms of seq and deepseq respectively.

import Control.DeepSeq

sfx :: (a -> b) -> a -> b
f `sfx` x = x `seq` f x

dsfx :: NFData a => (a -> b) -> a -> b
f `dsfx` x = x `deepseq` f x

