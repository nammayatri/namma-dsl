let outputPrefix =
      "/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/tests/src-read-only/"

let outputPath =
      { _apiRelatedTypes = outputPrefix ++ "UI/Api/Types"
      , _beamQueries = outputPrefix ++ "Storage/Queries"
      , _extraBeamQueries = outputPrefix ++ "Storage/Queries/Extra"
      , _beamTable = outputPrefix ++ "Storage/Beam"
      , _domainHandler = outputPrefix ++ "UI/Api/Action"
      , _domainType = outputPrefix ++ "Storage/Domain/Types"
      , _servantApi = outputPrefix ++ "UI/Api"
      , _sql = outputPrefix ++ "/migrations"
      , _purescriptFrontend = ""
      }

let GeneratorType =
      < SERVANT_API
      | API_TYPES
      | DOMAIN_HANDLER
      | BEAM_QUERIES
      | BEAM_TABLE
      | DOMAIN_TYPE
      | SQL
      | PURE_SCRIPT_FRONTEND
      >

let DefaultImports =
      { _qualifiedImports : List Text
      , _simpleImports : List Text
      , _generationType : GeneratorType
      }

let defaultTypeImportMapper =
      [ { _1 = "Text", _2 = "Kernel.Prelude" }
      , { _1 = "Maybe", _2 = "Kernel.Prelude" }
      , { _1 = "Double", _2 = "Kernel.Prelude" }
      , { _1 = "TimeOfDay", _2 = "Kernel.Prelude" }
      , { _1 = "Day", _2 = "Data.Time.Calendar" }
      , { _1 = "Int", _2 = "Kernel.Prelude" }
      , { _1 = "Bool", _2 = "Kernel.Prelude" }
      , { _1 = "Id", _2 = "Kernel.Types.Id" }
      , { _1 = "ShortId", _2 = "Kernel.Types.Id" }
      , { _1 = "UTCTime", _2 = "Kernel.Prelude" }
      , { _1 = "Meters", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecMeters", _2 = "Kernel.Types.Common" }
      , { _1 = "Kilometers", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecMoney", _2 = "Kernel.Types.Common" }
      , { _1 = "Seconds", _2 = "Kernel.Types.Common" }
      ]

let extraDefaultFields =
      [ { _1 = "merchantId", _2 = "Maybe (Id Merchant)" }
      , { _1 = "merchantOperatingCityId"
        , _2 = "Maybe (Id MerchantOperatingCity)"
        }
      , { _1 = "createdAt", _2 = "UTCTime" }
      , { _1 = "updatedAt", _2 = "UTCTime" }
      ]

let sqlMapper =
      [ { _1 = "\\[Text\\]", _2 = "text[]" }
      , { _1 = "Text", _2 = "text" }
      , { _1 = "\\[Id ", _2 = "text[]" }
      , { _1 = "Id ", _2 = "character varying(36)" }
      , { _1 = "\\[ShortId ", _2 = "text[]" }
      , { _1 = "ShortId ", _2 = "character varying(36)" }
      , { _1 = "Int", _2 = "integer" }
      , { _1 = "Double", _2 = "double precision" }
      , { _1 = "HighPrecMoney", _2 = "double precision" }
      , { _1 = "Money", _2 = "integer" }
      , { _1 = "Bool", _2 = "boolean" }
      , { _1 = "UTCTime", _2 = "timestamp with time zone" }
      , { _1 = "TimeOfDay", _2 = "time without time zone" }
      , { _1 = "Day", _2 = "date" }
      , { _1 = "Seconds", _2 = "integer" }
      , { _1 = "Kilometers", _2 = "integer" }
      , { _1 = "Meters", _2 = "integer" }
      ]

in  { _output = outputPath
    , _storageConfig =
      { _dbName = "namma-db"
      , _sqlTypeMapper = sqlMapper
      , _extraDefaultFields = extraDefaultFields
      }
    , _defaultImports = [] : List DefaultImports
    , _defaultTypeImportMapper = defaultTypeImportMapper
    , _generate =
      [ GeneratorType.DOMAIN_TYPE
      , GeneratorType.SQL
      , GeneratorType.BEAM_TABLE
      , GeneratorType.BEAM_QUERIES
      , GeneratorType.DOMAIN_HANDLER
      , GeneratorType.API_TYPES
      , GeneratorType.SERVANT_API
      ]
    }
