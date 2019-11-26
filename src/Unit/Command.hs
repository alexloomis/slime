module Unit.Command where

import Unit.Type
import Slime.Type

data Command = Move Edge | Hold

resolveUnits :: [Command] -> State Position Position
