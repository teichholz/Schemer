-- | 5. Phase Removes set! by wrapping local variables and parameters in one-element vectors called boxes.

-- ((lambda (x)
--   (let ((c 2))
--    (set! x 2)
--    (set! c 4))) 4)
-- =>
-- ((lambda (x)
--   (let ((c (vec 2)))
--    (vector-set! x 0 2)
--    (voctor-set! c 0 4))) (vec 4))

module Phases.Assignment where
