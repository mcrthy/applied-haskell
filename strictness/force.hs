-- EXERCISE: Define the 'force' helper function in terms of seq and deepseq

import Control.DeepSeq

force' :: NFData a => a -> a
force' x = x `deepseq` x
