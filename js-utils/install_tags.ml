let package_name = "ppx_pipebang"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_pipebang", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ppx", Some "../lib/ppx_pipebang/ppx")
    ],
    [])
  ]
