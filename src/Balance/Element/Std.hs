module Balance.Element.Std ( StdEl, StdElF (..) ) where


import Balance.Element
import Balance.Element.Fill
import Balance.Element.Grid
import Balance.Element.HBox
import Balance.Element.Rectangular
import Balance.Element.Stack
import Balance.Element.VBox
import Balance.Geometry

import Data.Fix


data StdElF a e = FillEl (FillElement a)
                | GridEl (Grid e a)
                | HBoxEl (HBox e a)
                | StackEl (Stack e)
                | VBoxEl (VBox e a)


type StdEl a = Fix (StdElF a)
