let rootPrefixes =
      [ "/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/tests/src-read-only"
      ]

let moduleMaps =
      [ { _1 = "Colors", _2 = "UI.Frontend.Back" }
      , { _1 = "animConfig", _2 = "UI.Frontend.Back" }
      , { _1 = "feedbackBasedOnRatingView", _2 = "UI.Frontend.Back" }
      ]

in  { _tdRootPaths = rootPrefixes, _defaultModuleMapper = moduleMaps }
