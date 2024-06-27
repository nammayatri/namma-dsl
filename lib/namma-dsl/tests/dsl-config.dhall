let outputPrefix =
      "/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/tests/src-read-only/"

let sqlOutputPaths =
      [ { _1 = outputPrefix ++ "/migrations", _2 = "atlas_app" }
      , { _1 = outputPrefix ++ "/migrations2", _2 = "atlas_driver" }
      ]

let outputPath =
      { _apiRelatedTypes = outputPrefix ++ "UI/Api/Types"
      , _beamQueries = outputPrefix ++ "Storage/Queries"
      , _extraBeamQueries = outputPrefix ++ "Storage/Queries/Extra"
      , _cachedQueries = outputPrefix ++ "Storage/CachedQueries"
      , _extraCachedQueries = outputPrefix ++ "Storage/CachedQueries/Extra"
      , _beamTable = outputPrefix ++ "Storage/Beam"
      , _domainHandler = outputPrefix ++ "UI/Api/Action"
      , _domainType = outputPrefix ++ "Domain/Types"
      , _servantApi = outputPrefix ++ "UI/Api"
      , _sql = sqlOutputPaths
      , _purescriptFrontend = outputPrefix ++ "UI/Api222"
      }

let GeneratorType =
      < SERVANT_API
      | API_TYPES
      | DOMAIN_HANDLER
      | BEAM_QUERIES
      | CACHED_QUERIES
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
      , { _1 = "DbHash", _2 = "Kernel.External.Encryption" }
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

let defaultImports =
      [ { _simpleImports =
          [ "EulerHS.Prelude", "Servant", "Tools.Auth", "Kernel.Utils.Common" ]
        , _qualifiedImports =
          [ "Domain.Types.Person"
          , "Kernel.Prelude"
          , "Control.Lens"
          , "Domain.Types.Merchant"
          , "Environment"
          , "Kernel.Types.Id"
          ]
        , _generationType = GeneratorType.SERVANT_API
        }
      , { _simpleImports =
          [ "EulerHS.Prelude hiding (id)"
          , "Servant"
          , "Tools.Auth"
          , "Data.OpenApi (ToSchema)"
          ]
        , _qualifiedImports =
          [ "Kernel.Prelude"
          , "Domain.Types.Person"
          , "Domain.Types.Merchant"
          , "Environment"
          , "Kernel.Types.Id"
          ]
        , _generationType = GeneratorType.API_TYPES
        }
      , { _simpleImports =
          [ "EulerHS.Prelude hiding (id)"
          , "Servant"
          , "Tools.Auth"
          , "Data.OpenApi (ToSchema)"
          ]
        , _qualifiedImports =
          [ "Kernel.Prelude"
          , "Domain.Types.Person"
          , "Domain.Types.Merchant"
          , "Environment"
          , "Kernel.Types.Id"
          ]
        , _generationType = GeneratorType.DOMAIN_HANDLER
        }
      , { _simpleImports = [] : List Text
        , _qualifiedImports = [ "Tools.Beam.UtilsTH" ]
        , _generationType = GeneratorType.DOMAIN_TYPE
        }
      , { _simpleImports =
          [ "Kernel.Prelude"
          , "Tools.Beam.UtilsTH"
          , "Kernel.External.Encryption"
          ]
        , _qualifiedImports = [ "Database.Beam as B" ]
        , _generationType = GeneratorType.BEAM_TABLE
        }
      , { _simpleImports =
          [ "Kernel.Beam.Functions"
          , "Kernel.Prelude"
          , "Kernel.External.Encryption"
          , "Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)"
          , "Kernel.Types.Error"
          ]
        , _qualifiedImports = [ "Sequelize as Se" ]
        , _generationType = GeneratorType.BEAM_QUERIES
        }
      , { _simpleImports = [ "Kernel.Prelude", "Kernel.Utils.Common" ]
        , _qualifiedImports = [ "Kernel.Storage.Hedis as Hedis" ]
        , _generationType = GeneratorType.CACHED_QUERIES
        }
      ]

in  { _output = outputPath
    , _rootPaths =
      [ "/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/src/" ]
    , _storageConfig =
      { _sqlTypeMapper = sqlMapper
      , _extraDefaultFields = extraDefaultFields
      , _defaultCachedQueryKeyPrefix = "driverOffer:"
      }
    , _defaultImports = defaultImports
    , _defaultTypeImportMapper = defaultTypeImportMapper
    , _generate =
      [ GeneratorType.DOMAIN_TYPE
      , GeneratorType.SQL
      , GeneratorType.BEAM_TABLE
      , GeneratorType.BEAM_QUERIES
      , GeneratorType.DOMAIN_HANDLER
      , GeneratorType.API_TYPES
      , GeneratorType.SERVANT_API
      , GeneratorType.CACHED_QUERIES
      , GeneratorType.PURE_SCRIPT_FRONTEND
      ]
    }
