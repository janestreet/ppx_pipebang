let package_name = "ppx_pipebang"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_pipebang", None)
    ],
    [ ("META", None)
    ])
  ; ("libexec",
    [ ("built_exec_ppx", Some "ppx")
    ],
    [])
  ]
