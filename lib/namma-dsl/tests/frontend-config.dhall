let rootPrefix =
      "/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/tests/src-read-only"

let defaultImportModules =
      [ { _1 = "Int", _2 = "Prim" }, { _1 = "Array", _2 = "Prim" } ]

let defaultImports = [ "Common.Types.Config", "Prim2 as Prim2" ]

let GeneratorType =
      < SERVANT_API
      | SERVANT_API_DASHBOARD
      | API_TYPES
      | DOMAIN_HANDLER
      | DOMAIN_HANDLER_DASHBOARD
      | BEAM_QUERIES
      | CACHED_QUERIES
      | BEAM_TABLE
      | DOMAIN_TYPE
      | SQL
      | PURE_SCRIPT_FRONTEND
      | PURE_SCRIPT_FRONTEND_COMPONENT
      | PURE_SCRIPT_FRONTEND_SCREEN
      >

in  { _fGenRootPath = rootPrefix
    , _fComponentModulePrefix = "Front.Components"
    , _fScreenModulePrefix = "Front.Screens"
    , _fDefaultImportMapper = defaultImportModules
    , _fDefaultImports = defaultImports
    , _fGenerate = [ GeneratorType.PURE_SCRIPT_FRONTEND_COMPONENT ]
    }
