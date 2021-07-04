module main

import StdEnv
import Drawille

import Gast

/* Testing driver module for Drawille. */

Start :: String
Start = "Hello Drawille \xE2\xA0\x81\xE2\xA0\xB2\n" +++
        toDebugString (set (set (create 8 9) 0 0) 1 2) +++
        "================================================================================\n" +++
        toDebugString (fromList (brailleToList 0xF8))
