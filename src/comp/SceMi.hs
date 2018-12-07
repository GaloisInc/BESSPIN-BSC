module SceMi (
              scemiABinNames, scemiVPIWrapperNames,
              scemiLinkABinNames,
              scemiTbABinNames, scemiTbVPIWrapperNames
             ) where

import Flags
import FileNameUtil

-- -------------------------

-- The Classic files are in a subdirectory
libDir :: Flags -> String
libDir flags =
    if (genSceMiClassic flags)
    then (bluespecDir flags) ++ "/Libraries/Classic/"
    else (bluespecDir flags) ++ "/Libraries/"

-- -------------------------

-- The .ba files for all SceMi VPI functions
--
-- To support the old linking style for Verilog, where no .ba file is provided,
-- BSC needs to include the .ba file for all SceMi VPI functions, in case any
-- is used in the Verilog.  So this returns the complete list.
-- (With the new linking style where the design is provided as .ba files,
-- a walk of the hierarchy will reveal which VPI functions are used.)
--
scemiABinNames :: Flags -> [String]
scemiABinNames flags =
    [ (libDir flags) ++ n ++ "." ++ abinSuffix
    | n <- scemiVPINames flags
    ]

-- The VPI wrapper files for all SceMi VPI functions
--
-- The generated C wrappers for the VPI functions from the above .ba files.
--
scemiVPIWrapperNames :: Flags -> [String]
scemiVPIWrapperNames flags =
    [ (libDir flags) ++ "vpi_wrapper_" ++ n ++ "." ++ suf
    | n <- scemiVPINames flags
    , suf <- [ cSuffix, hSuffix ]
    ]

-- The wrapper names
scemiVPINames :: Flags -> [String]
scemiVPINames flags =
    if (genSceMiClassic flags)
    then [ "bscemi_" ++ n
         | n <- [ "send", "recv", "request"
                , "is_buffer_available"
                , "is_data_available"
                , "open_socket", "close_socket"
                , "shutdown_triggered"
                ]
         ]
    else [ "bluenoc_" ++ n
         | n <- [ "open_tcp_socket"
                , "close_tcp_socket"
                , "send_tcp_beat"
                , "recv_tcp_beat"
                ]
         ]

-- -------------------------

-- The .ba files for SceMi VPI functions that can be added by scemilink
--
-- Even when linking a design provided as .ba files, the scemilink program
-- can generate Verilog that uses VPI functions, and those uses will not be
-- found by a walk of the .ba files, so BSC needs to include them.
--
scemiLinkABinNames :: Flags -> [String]
scemiLinkABinNames flags =
    [ (libDir flags) ++ n ++ "." ++ abinSuffix
    | n <- scemiLinkVPINames flags
    ]

scemiLinkVPINames :: Flags -> [String]
scemiLinkVPINames flags =
    if (genSceMiClassic flags)
    then [ "bscemi_" ++ n
         | n <- [ "open_socket" ]
         ]
    else [ "bluenoc_" ++ n
         | n <- [ "open_tcp_socket" ]
         ]

-- -------------------------

-- The .ba files for all SceMi Tb VPI functions
--
-- This is the equivalent of scemiABinNames, but for the testbench side;
-- so see the comment on scemiABinNames above.
--
scemiTbABinNames :: Flags -> [String]
scemiTbABinNames flags =
    [ (libDir flags) ++ n ++ "." ++ abinSuffix
    | n <- scemiTbVPINames flags
    ]

-- The VPI wrapper files for all SceMi Tb VPI functions
--
-- The generated C wrappers for the VPI functions from the above .ba files.
--
scemiTbVPIWrapperNames :: Flags -> [String]
scemiTbVPIWrapperNames flags =
    [ (libDir flags) ++ "vpi_wrapper_" ++ n ++ "." ++ suf
    | n <- scemiTbVPINames flags
    , suf <- [ cSuffix, hSuffix ]
    ]

-- The wrapper names
scemiTbVPINames :: Flags -> [String]
scemiTbVPINames flags =
    [ "bsvscemi_" ++ n
    | n <- [ "bind_message_inport"
           , "bind_message_outport"
           , "bind_inpipe"
           , "bind_outpipe"
           , "message_inport_proxy_ready"
           , "message_inport_proxy_send"
           , "message_outport_proxy_data_get"
           , "message_outport_proxy_ready"
           , "inpipe_proxy_ready"
           , "inpipe_proxy_send"
           , "inpipe_proxy_send_immediate"
           , "outpipe_proxy_data_get"
           , "outpipe_proxy_data_get_immediate"
           , "outpipe_proxy_ready"
           , "shutdown"
           ]
    ] ++
    [ "bluenoc_" ++ n
    | n <- [ "close_tcp_socket"
           , "open_tcp_socket"
           , "recv_tcp_beat"
           , "send_tcp_beat"
           ]
    ] ++
    [ "bsvsimtb_" ++ n
    | n <- [ "interp_start"
           , "interp_message_get"
           , "interp_message_send"
           ]
    ] ++
    [ "emu_" ++ n
    | n <- [ "stop"
           , "run"
           ]
    ] ++
    [ "timer_" ++ n
    | n <- [ "clear"
           , "show"
           ]
    ]

-- -------------------------

