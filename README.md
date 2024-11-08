# Namma DSL

This tutorial provides a comprehensive guide to understanding and working with the Domain Specific Language (NammaDSL) in the NammaYatri. It covers the creation of YAML files, code generation, and compilation, as well as the syntax for defining APIs and storage.

#### Contents:
  - [Creating YAML Files](#creating-yaml-files)
  - [Api DSL](#api-dsl)
  - [Storage DSL](#storage-dsl)

# Creating YAML Files

1. **Location**: Create a YAML file inside the `spec` folder of the module you are working on. For instance, if you are working on the `rider-app`, the path would be:
    ```
    rider-platform/rider-app/spec
    ```
    Depending on the type of specification you are defining, you may place the file inside either the `API` or `Storage` folder.

2. **Code Generation**: After defining the YAML files, execute the following command to generate the code:
    ```
    , run-generator
    ```
    This command generates Haskell Beam, query, and domain files in the `src-read-only` directory, as well as the SQL queries.

    **Important Note**:
     This command will only generate those spec files which are new or changed. This is done by getting the current hash of the spec file and comparing with the   file hash of the HEAD commit

     ```
     , run-generator --all
     ```
     use "--all" args to generate all the spec files



3. **Compilation**: Compile the code using:
    ```
    cabal build all
    ```
---
---


# Api DSL

#### Syntax for API DSL

- `imports`: Used for importing predefined types.
- `importPackageOverrides`: Used to override import packages [See More](#import-package-override)
- `module`: Specifies the name of the module.

  **Note:** In case of dashboard APIs, all APIs in the module have the same API prefix, by default it is `module` converted to camel case.
- `apiPrefix`: overwrite default API prefix for main and helper dashboard API (optional, specific for dashboard).

  **Note:** Empty value `""` allowed for `apiPrefix` and`helperApiPrefix`, it means no API prefix.
- `helperApiPrefix`: overwrite API prefix for helper dashboard API (optional, specific for dashboard).
- `types`: Defines the request and response types for your APIs. This field is optional. Same as [Complex Types in Storage DSL](#complex-types). Types are defined in the following format:
    ```
    {TypeName}:
      - {field1}: {field1Type}
      - {field2}: {field2Type}
      - derive: {Any extra derivation}
    ```
  **Note:** Old syntax does not preserve fields order and will be deprecated.
    ```
    {TypeName}:
      {field1}: {field1Type}
      {field2}: {field2Type}
      derive: {Any extra derivation}
    ```
    Enum types can be defined as:
    ```
    {TypeName}:
      - enum: {enum1},{enum2}
      - derive: {Any extra derivation}
    ```
    To make newtype or type instead of data use `recordType: NewType | Data (Default) | Type`
    ```
    {TypeName}:
      - recordType: NewType
      - {fieldName}: {fieldType}
      - derive: {Any extra derivation}

    {TypeName}:
      - recordType: Type
      - type: {fieldType}
    ```
    To create default HideSecrets instance use `derive` keyword (specific for dashboard)
    ```
    {TypeName}:
      - {fieldName}: {fieldType}
      - derive: "'HideSecrets"
    ```
- `apis`: Contains all the APIs in the following format:
    - `{httpMethod}` (HTTP method GET | POST | PUT | DELETE)
        - `endpoint`: API path
           ```
             /path1/path2/{pathParam1}/path3/{pathParam2}
           ```
           **Note:** Path param types should be mentioned in the **params** part below.
        - `name`: API name. Sometimes two different APIs have the same API name auto generated from path, so it can be overwritten (optional)

          **Note:** Be careful when you change `apiName` for already existing API. When `apName` changed, `Endpoint` and `UserActonType` generation will be also changed, old data should be migrated as follow:
          ```
          migrate:
            endpoint: <oldEndpoint>
            userActionType: <oldUserActionType>
          ```
          Typically,`<oldEndpoint>` and `<oldUserActionType>` are the same values in this format:
          ```
          PROVIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK
          ```
        - `response`:
          - `type`: Type of response
        - `request`:
          - `type`: Type of request (optional)
        - `multipart`:
          - `type`: Type of request in case of multipart request (optional)
        - `auth`: Authentication method (default: TokenAuth) [See More](#api-auth)
        - `query`: List of query parameters
          ```
          - {queryParam1}: {queryParam1Type}
          ```
          **Note:** Old syntax does not preserve params order and will be deprecated.
          ```
          {queryParam1}: {queryParam1Type}
          ```
        - `mandatoryQuery`: List of mandatory query parameters
          ```
          - {mandatoryQueryParam1}: {mandatoryQueryParam1Type}
          ```
          **Note:** Old syntax does not preserve params order and will be deprecated.
          ```
          {mandatoryQueryParam1}: {mandatoryQueryParam1Type}
          ```
        - `params`: List of path parameters
          ```
          {pathParam1}: {pathParam1Type}
          {pathParam2}: {pathParam2Type}
          ```
        - `headers`: List of headers
          ```
          headers:
           - {header1}: {headerType1}
           - {header2}: {headerType2}
          ```
        - `helperApi`: Recursively contains dashboard helper API in the same format as main API  (optional, specific for dashboard)
        - `validation`: Qualified name for request validation function (optional)
- Example:
  ```yaml
    imports: {}
    module: Sos
    types:
      SosRes:
        - sosId: Id Sos

      SosDetailsRes:
        - sos: Maybe Sos

      SosReq:
        - flow: SosType
        - rideId: Id Ride

      SosUpdateReq:
        - status: SosStatus
        - comment: Maybe Text

    apis:
      # GET /sos/getDetails
      - GET:
          endpoint: /sos/getDetails/{rideId}
          auth: TokenAuth
          params:
            rideId: Id Ride
          response:
            type: API.Types.UI.Sos.SosDetailsRes

      # # POST /sos/{sosId}/status
      - POST:
          endpoint: /sos/{sosId}/status
          auth: TokenAuth RIDER_TYPE
          params:
            sosId: Id Sos
          request:
            type: API.Types.UI.Sos.SosUpdateReq
          response:
            type: Kernel.Types.APISuccess.APISuccess
  ```

#### Api Auth
- AdminTokenAuth
- ApiTokenAuth
- TokenAuth (default)
- NoAuth
- SafetyWebhookAuth
    - MERCHANT_SERVER
- TokenAuth
    - RIDER_TYPE
    - PROVIDER_TYPE (This has MerchantOperatingCityId)
- DashboardAuth
  -  DASHBOARD_USER
  -  DASHBOARD_ADMIN
  -  FLEET_OWNER
  -  DASHBOARD_RELEASE_ADMIN
  -  MERCHANT_ADMIN
  -  MERCHANT_MAKER
  -  MERCHANT_CHECKER
  -  MERCHANT_SERVER
  -  MERCHANT_USER
- ApiAuth ServerName ApiEntity UserActionType
    ```
    auth: ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS LIST
    ```
---
---

# Storage DSL

#### Syntax for Storage DSL

- `imports`: Used for importing predefined types. [See More](#imports)
- `{dataTypeName}`: Specifies the name of the module.
  - `tableName`: Optional name of the table, It takes the snake_case of the `dataTypeName` if not defined.
  - `fields`: Lists all fields of the table with Haskell type. [See More](#fields)
  - `constraints`: PrimaryKey | SecondaryKey | NotNull | AUTOINCREMENT
  - `importPackageOverrides`: Used to override import packages [See More](#import-package-override)
  - `types`: User-defined types, similar to API types. [See More](#complex-types)
  - `derives`: Override derives of the main Data type.
     ```yaml
      derives: "Show,Eq,Ord"
     ```
  - `beamType`: User-defined beam type for a specified data type. [See More](#beam-type)
  - `beamFields`: User-defined beam field name change or use it if you want to have something different on beamside. [See More](#beam-fields)

  - `beamInstance`: We can mention the beam instance we need using this field [See More](#beam-instance)
  - `sqlType`: User-defined sql type for a field. [See More](#sql-type)
  - `default`: Default sql value for fields, if any
      ```yaml
      fields:
        scheduleTryTimes: '[Int]'
        tripCategory: Text
      default:
        tripCategory: "'All'"
        scheduleTryTimes: "'{1800, 900, 300}'"
      ```
  - `queries`: All Beam queries for the table. [See More](#queries)
  - `cachedQueries:` Cached queries for the table. [See More](#cached-queries)
  - `fromTType`: FromTType of fields, if applicable. [See More](#tottype-and-fromttype)
  - `toTType`: ToTType of fields, if applicable. [See More](#tottype-and-fromttype)
  - `excludedFields`: There are some common fields like merchantId, merchantOperatingCityId, createdAt and updatedAt which are auto added in the data type. To remove them use this.
      ```yaml
      excludedFields:
        - merchantOperatingCityId
        - merchantId
      ```
  - `extraIndexes` : Any additional indexes
  [See More](#generating-sql-indexes-or-unique-constraints)
  - `extraOperations` : Extra Operations [See More](#extra-operations)
---

#### Imports

- You have to provide the module name in imports
  ```yaml
  imports:
      Merchant: Domain.Types.Merchant
      FRFSSearch: Domain.Types.FRFSSearch
  ```

- **Note:** These common types are auto imported, so you can directly use them without importing
    ```
    Text -> Kernel.Prelude
    Maybe -> Kernel.Prelude
    Double -> Kernel.Prelude
    TimeOfDay -> Kernel.Prelude
    Day -> Data.Time.Calendar
    Int -> Kernel.Prelude
    Bool -> Kernel.Prelude
    Id -> Kernel.Types.Id
    ShortId -> Kernel.Types.Id
    UTCTime -> Kernel.Prelude
    Meters -> Kernel.Types.Common
    HighPrecMeters -> Kernel.Types.Common
    Kilometers -> Kernel.Types.Common
    HighPrecMoney -> Kernel.Types.Common
    Seconds -> Kernel.Types.Common
    ```
---
#### Import Package Override
```
imports:
  DataType1: Domain.Types.DataType1
```
To change package:
```
importPackageOverrides:
  Domain.Types.DataType1: dashboard-api
```
Generated import in haskell:
```haskell
import "dashboard-api" Domain.Types.DataType1
```

Sometimes we may need to skip package override when we generate in the same package. Then we should specify package mapping in `dhall` configs:

```dsl-config.dhall
  , _packageMapping =
      [ { _1 = GeneratorType.API_TYPES, _2 = "dashboard-api" }
        { _1 = GeneratorType.SERVANT_API, _2 = "rider-app" }
      ]
```

Generated import in haskell for `API_TYPES`:
```haskell
import "this" Domain.Types.DataType1
```
Generated import in haskell for `SERVANT_API`:
```haskell
import "dashboard-api" Domain.Types.DataType1
```

---
#### Fields

- In the field section mention field name and it's corresponding Haskell type
  ```yaml
  imports : {}

  LmsModule:
    tableName : lms_module

    fields:
      id : Id LmsModule
      merchantOperatingCityId : Id MerchantOperatingCity
      category : LmsCategory
      createdAt : UTCTime
      updatedAt : UTCTime
      duration : Int
      noOfVideos : Int
      rank : Int
      variant : Maybe Variant
      moduleCompletionCriteria : ModuleCompletionCriteria
  ```
- For Simple field types (which are just imported) it will be copied in the beam side as well unless we mention a specific [Beam Type](#beam-type)

- If the Field is a [Complex Type](#complex-types) then it would be recursively split in the beam side unless we mention a Specific [Beam Type](#beam-type)

- In case of Id, ShortId the Beam Type will be considered as Text

- If we want Imported Data type on domain side and corresponding *Id* on the beam side you we use the [WithId extensions](#withid-extensions) with the type definition.


---

#### Beam Type
- This is used to mention a specific beam type for a corresponding data type if required.
- Users have to provide the [FromTType and ToTType](#tottype-and-fromttype) transformer for this.
  Example:
    This will have int  on the domain side and Text on the beam side.
  ```yaml
  fields:
    field1: Int

  beamType:
    field1: Text
  ```
---

#### Beam Fields
- For just changing name of a beam field:
    ```yaml
      fields:
        a: Int
        b: Text
      beamFields:
        a: "aa"
        b: "bb"
    ```
- For having completely different number of fields and types on beam side for a specific domain field:
    ```yaml
      # SomeType = SomeType {
      #   integerValueInText :: Text,
      #   version :: Int
      # }
      import:
        SomeType: Domain.Types.SomeType
      Some:
        fields:
          id: Id Some
          val: SomeType # See this is an imported type
        beamFields:
          val:
            intValue: Int
            intValueInText: Text
        # We have to right the toTType and fromTType functions
        toTType:
            intValue: (Kernel.Prelude.read . Domain.Types.SomeType.integerValueInText)
            intValueInText: Domain.Types.SomeType.integerValueInText
        fromTType:
            val: mkVal
    ```
    Generated Code:
    1. Domain Type:
       ```haskell
       data Some = Some
        { id :: Kernel.Types.Id.Id Domain.Types.Some.Some,
          val :: Domain.Types.SomeType.SomeType,
          merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
          merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
          createdAt :: Kernel.Prelude.UTCTime,
          updatedAt :: Kernel.Prelude.UTCTime
        }
        deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

       ```
    2. Beam Type:
        ```haskell
        data SomeT f = SomeT
          { id :: B.C f Kernel.Prelude.Text,
            intValue :: B.C f Kernel.Prelude.Int,
            intValueInText :: B.C f Kernel.Prelude.Text,
            merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
            merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
            createdAt :: B.C f Kernel.Prelude.UTCTime,
            updatedAt :: B.C f Kernel.Prelude.UTCTime
          }
          deriving (Generic, B.Beamable)
        ```
---
#### Beam Instance
 - Currently there are three Beam Instance available -
    - MakeTableInstances (Taken by default if nothing specified)
      ```yaml
      beamInstance: MakeTableInstances
      ```
      ```haskell
      $(mkTableInstances ''PersonT "person")
      ```
    - MakeTableInstancesGenericSchema
      ```yaml
      beamInstance: MakeTableInstancesGenericSchema
      ```
      ```haskell
      $(mkTableInstancesGenericSchema ''PersonT "person")
      ```
    - MakeTableInstancesWithTModifier <extra params>
      ```yaml
      beamInstance: MakeTableInstancesWithTModifier [("deviceOS", "device_o_s")]
      ```
      ```haskell
      $(mkTableInstancesWithTModifier ''MetaDataT "meta_data" [("deviceOS", "device_o_s")])
      ```
    - Custom <instance name> <extra param>
      ```yaml
      beamInstance: Custom mkCacParseInstace [[Table2],[Table3]]
      ```
      ```haskell
      $(mkCacParseInstace ''MetaDataT [[Table2], [Table3]])
      ```
        Limitation for now: Try not to include space inside a extra param as we split on space to get the params seperately
        Give like this,
        ```yaml
         beamInstance: Custom mkCacParseInstace "table_name" [Table2] [Table3] [(a,b,c)]
        ```
    - If require more than one
      ```yaml
      beamInstance:
        - MakeTableInstances
        - Custom mkCacParseInstace [[Table2],[Table3]]
        - Custom Tool.Something.mkSomething "abc" [(a,b,c)] [[a],[b],[c]]
      ```
      ```haskell
      $(mkTableInstances ''PersonT "person")
      $(mkCacParseInstace ''MetaDataT [[Table2], [Table3]])
      $(Tool.Something.mkSomething ''MetaDataT "abc" [(a,b,c)] [[a],[b],[c]])
      ```
---
#### SQL TYPE
- Generally sql type is auto detected according to beam types, but if not detected it takes **text** by default.
- The sql type of a specific field can be changed using **sqlType** like below
  Example:
  ```yaml
    fields:
       field1: "[Int]"
    sqlType:
       field1: "text[]"
  ```
---
#### ToTType And FromTType
- If required a transformer function user can mention it like below example. The function definition will be created in a seperate file with **error "TODO"** and user must implement it.
- If the function is monadic use **M** extension with the type definition
- See [imported and monadic transformer functions](#imported-and-monadic-transformer-functions) part for more details on extension
  Example:
  ```yaml
  toTType:
    subscriberUrl: Kernel.Prelude.func1|I
    gatewayUrl: showBaseUrlSimple  # This function will be created in seperated file as it's not imported
    registryUrl: showBaseUrl|M # This function will be created in seperated file as it's not imported

  fromTType:
    updatedAt: Kernel.Prelude.fromMaybe createdAt|I
    isScheduled: makeSchelude
    tripCategory: Kernel.Prelude.fromMaybe (Domain.Types.Common.OneWay Domain.Types.Common.OneWayOnDemandDynamicOffer)|I
  ```
  **Note:**
    - See in case of **tripCategory** how everything is qualified, also see for **updatedAt** all are qualified except the createdAt variable. This is important as the dsl can figure out the required imports from there.
---
#### Imported and Monadic Transformer Functions
- If the transformer function needs to be imported user can give **I** extension with the TType functions
- If the transformer function is monadic use **M** extension
- User can use both like **IM** or **MI** if its imported and also monadic
  Examples:
   1. [Estimate.yaml](https://github.com/nammayatri/nammayatri/blob/main/Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/Estimate.yaml#L41)
   2. [BecknConfig.yaml](https://github.com/nammayatri/nammayatri/blob/main/Backend/app/rider-platform/rider-app/Main/spec/Storage/BecknConfig.yaml#L47)
- Embedded transfromer function **E**. This is be used to embed various imported functions and variables in user defined order. Can be combined with **M** like **EM**
  Example Field:
    ```yaml
      fields:
        isVerified: Bool
        verificationUrl: Text
        aadharId: Text
    ```
  Lets Consider ToTType for this example.
  Example usage:
   1. If isVerified uses aadardId, verificationUrl in this order without passing itself as arg
      ```yaml
        toTType:
          isVerified: (K.B.verify aadharId verificationUrl)|E
      ```
   2. if required in different order like this also with itself being passed.
      ```yaml
        toTType:
          isVerified: K.B.verify isVerified aadharId verificationUrl)|EM
      ```
   3. Much more can achieved by this embedded mode like this:
      ```yaml
        toTType:
          isVerified: K.B.verify isVerified (K.B.C.isCorrectAadhar aadharId) verificationUrl)|EM
      ```



---

#### WithId Extensions

- **WithId:** This is used when we want's imported data type's Id as the Beam Field. This does not create the data type in the create beam query.
- **WithCachedId:** Same as WithId, only difference is the **create** and **findbyId** query is imported from Storage.CachedQuery.*

- **WithIdCreate**, **WithCachedIdCreate:** Use this when its required to create the data type in create query. Important: The imported Data type should have **create** function in it's corresponding query file
  Example:
  ```yaml
  fields:
    fareParams: Maybe FareParameters|WithIdCreate
    farePolicy: Maybe FarePolicy|WithCachedId

  ```
  Generated create query:
  ```haskell
  create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.Estimate.Estimate -> m ()
  create tbl = do
    Kernel.Prelude.whenJust tbl.fareParams Storage.Queries.FareParameters.create
    createWithKV tbl
  ```
  Generated ToTType and FromTType conversions:
  ```haskell
  instance FromTType' Beam.Estimate Domain.Types.Estimate.Estimate where
    fromTType' Beam.EstimateT {..} = do
      fareParams' <- maybe (pure Nothing) (Storage.Queries.FareParameters.findById . Kernel.Types.Id.Id) fareParamsId
      farePolicy' <- maybe (pure Nothing) (Storage.CachedQueries.FarePolicy.findById . Kernel.Types.Id.Id) farePolicyId
      pure $
        Just
          Domain.Types.Estimate.Estimate
            {
              fareParams = fareParams',
              farePolicy = farePolicy'
            }

  instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
    toTType' Domain.Types.Estimate.Estimate {..} = do
      Beam.EstimateT
        { Beam.fareParamsId = (Kernel.Types.Id.getId . (.id) <$>) fareParams,
          Beam.farePolicyId = (Kernel.Types.Id.getId . (.id) <$>) farePolicy
        }
  ```
---
#### Queries
- Syntax:
  ```
  queries:
    {query function name}:
      kvFunction: {kv function name}
      params: [field1, field2 .. ] Array of field to be updated in update queries
      where:
        {where clause}
      orderby: {field name} (optional)
  ```
- Where Clause syntax:
  ```
  where:
    - {operator1}:
        - field1
        - {operator2}:
          - field2
          - field3
          - {operator3}:
            - field4
            - field5
  ```
- Fields that are used in params and where clause can be of 3 types:
   - Domain Type Fields: Normal fields in the domain type. On this fields domain to beam convertion function will be applied.
   - Beam Type Fields: We can directly pass beam type fields to params and where clause by adding a **|B**
     Example:
     ```
      where:
        not_eq
      where:
        not_eq:
          - id: id2|B
     ```
     at the end of the fieldName. On this fields domain to beam convertion will not be applied.
   - Constants: Constant can be added in place of a field.
      (Note: This are considered as beam type fields, hence domain to beam convertion will not be applied). All the constant types and their respective alpha symbol:
        - String -> CS
        - Bool -> CB
        - Integer -> CI
        - Double/Float -> CD
        - Any Imported type -> CIM or C
    Examples:
      ```
      myname|CS -> "myname"
      123|CI -> 123
      123|CS -> "123"
      0.23|CD -> 0.23
      "0.34"|CS -> "0.23"
      true|CB -> True
      Domain.Something.defaultValue|CIM -> Domain.Something.defaultValue (and Domain.Something is added to imports)
      ```
   - Various usage scenerios:
      - Updating a field named status with value NEW to any other status passed as variable
          ```
        kvFunction: updateWithKV
        params:
          - status: newStatus
        where:
          eq:
            - status: NEW|CIM
          ```
      - Now if we want to hardcode the status value to CONFIRMED
          ```
        kvFunction: updateWithKV
        params:
          - status: Domain.Types.DataType.CONFIRMED|CIM
        where:
          eq:
            - status: NEW|CIM
          ```
      - Now if we want to filter over some id passed as beam type
          ```
          where:
          and:
            - eq:
                - status: NEW|CIM
                - id|B
          ```
- List of where operators:
    - and
    - or
    - in
    - eq
    - gt
    - lt
    - gte
    - lte
    - not_\<Any Comparison Operator\> Example: not_eq, not_lt

  Example:
  ```yaml
  LmsModule:
    fields:
      id: Id LmsModule
      category: LmsCategory
      question: Question
      field1: Text
      field2: Text
      field3: Text

    types:
      LmsCategory:
        enum: "Safety, Financial, Training"
      QuestionType:
        enum: "Maths, Physics, Chemistry"
        derive: "Show"
      Question:
        question: Text
        tp: QuestionType
        derive: "Show,Eq"
    queries:
        findByComplexCondition:
          kvFunction: findAllWithOptionsKV
          where:
            and:
              - field1
              - or:
                  - field2
                  - field3
              - category
              - question
          orderBy: createdAt
        updateQuestionById:
          kvFunction: updateWithKV
          params:
            - question
          where:
            id
  ```
  Generated Query:
  ```haskell
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.field1 $ Se.Eq field1,
          Se.Or
            [ Se.Is Beam.field2 $ Se.Eq field2,
              Se.Is Beam.field3 $ Se.Eq field3
            ],
          Se.Is Beam.category $ Se.Eq category,
          Se.Is Beam.questionQuestion $ Se.Eq $ Domain.Types.LmsModule.question question,
          Se.Is Beam.questionTp $ Se.Eq $ Domain.Types.LmsModule.tp question
        ]
    ]
  updateQuestionById question (Kernel.Types.Id.Id id) = do
    _now <- getCurrentTime
    updateWithKV
      [ Se.Set Beam.questionQuestion $ Domain.Types.LmsModule.question question,
        Se.Set Beam.questionTp $ Domain.Types.LmsModule.tp question,
        Se.Set Beam.updatedAt _now
      ]
      [ Se.Is Beam.id $ Se.Eq id
      ]
  ```
---
#### Cached Queries
##### Syntax
  * **cachedQueries:**
    * **queryName:**
      * **withCrossAppRedis:** true|false (Optional param, default: false)
      * **returnType/cacheDataType:** One|Array (Optional param, default: Auto detected by the query name) [See More](#cached-query-return-type)
      * **queryType:** FindOnly|FindAndCache|CacheOnly|DeleteCache (Optional param, default: Auto detect using function name or if failed takes FindAndCache as default) [See More](#cached-query-type)
      * **dbQuery:** The db query name that should be called to find in db (Important for FindAndCache type, not required for other types)
      * **dbQueryParams:** **[Params]** (See param section below for more info on Params, Important for FindAndCache types) [See More](#cached-query-params)
      * **keyParams:** **[Params]** (The params used to make keys) [See More on how keys are made](#key-maker)
      * **keyMaker:** keyMaker function name [See More on how keys are made](#key-maker)
      * **paramsOrder: [Param names]** , Optional, can be used to specify the function params order.

##### Cached Query Type
- There are four types of cached queries generations:
  - **FindAndCache**: Finds in kv and if not present finds in db and caches the result in kv
  - **FindOnly**: Only finds in kv and returns
  - **CacheOnly**: Just cache the DataType/[DataType] with a given key
  - **DeleteCache**: Delete the cache for a given key
##### Cached Query Return Type
- Find queries have the return type as either **[DataType]** or **Maybe DataType** denoted by **Array** and **One** respectively.

##### Key Maker
  - By default without **keyMaker** if params provided are
      ```
      DataType:
        fields:
          param1: Id DataType
          param2: Int

    .....
    cachedQueries:
        keyParams:
          - param1
          - param2
          - param3: Id Something
      ```
    then notice param3 is an external param not in fields [See more about external params](#cached-query-params).
    By default params with Id's and ShortId's text is extracted, but for others **show** function is used.
    Default key making:
    ```
      "Param1-"
    <> param1.getId
    <> ":Param2-"
    <> show param2
    <> ":Param3-"
    <> param3.getId
    ```
  - Otherwise of keyMaker is provided. Let's say
    ```
    keyMaker: makeKey
    ```
    or

    ```
    keyMaker: Some.Other.Module.makeKey
    ```
    then the params will be passed to makeKey function to get keys
    ```
    makeKey param1 param2 param3
    ```
    and user have to define the makeKey function in the extra Cached Query file if not qualified.

##### Cached Query Params
- Beam queries lacks extra params or constants but we have both in Cached Queries
- Param can be 3 types:
    - Normal param which is present in fields of defined data type
    - Constant like String,Int,Bool,Double,Other Imported Things
    - Variable with a type
- While defining constant user have to give append the constant type at the end.
  All the constant types and their respective alpha symbol:
    1. String -> CS
    2. Bool -> CB
    3. Integer -> CI
    4. Double/Float -> CD
    5. Any Imported type -> CIM or C
  Examples:
  ```
   myname|CS -> "myname"
   123|CI -> 123
   123|CS -> "123"
   0.23|CD -> 0.23
   "0.34"|CS -> "0.23"
   true|CB -> True
   Domain.Something.defaultValue|CIM -> Domain.Something.defaultValue (and Domain.Something is added to imports)
  ```
- To define an extra variable param not present in fields we have to also provide the type of the param like this:
  ```
  param1: Int
  param2: Maybe Domain.Something.Thing
  ```
- So lets mix them together and make a keyMaker:
  ```
  DataType:
        fields:
          param1: Id DataType
          param2: Int

    .....
    cachedQueries:
        keyMaker: makeMyAwesomeKey
        keyParams:
          - param1
          - param2
          - param3: Id Something
          - true|CB
          - 0.2231|CD
          - myName|CS
          - Domain.Types.Something.defaultValue|CIM
  ```
  So, the generated keyParams will be -
  ```
  import qualified Domain.Types.Something
  .....
  .....
  let key = makeMyAwesomeKey param1 param2 param3 true 0.2231 "myName" Domain.Types.Something.defaultValue
  ```

##### Cached Queries Generation Example:
- DSL code:
```yaml
    cachedQueries:
      deleteQuery:
          withCrossAppRedis: true
          keyParams:
           - id
           - requestId
           - 0|CI
      cacheQuery:
        withCrossAppRedis: false
        cacheDataType: One
        queryType: CacheOnly
        keyMaker: makeKeyForCaching
        keyParams:
          - id
          - requestId
          - Kernel.Prelude.True|CIM
          - createdAt
          - something: Maybe UTCTime #Any extra param
      findAndCacheQuery:
        withCrossAppRedis: true
        returnType: Array
        queryType: FindAndCache
        keyMaker: makeKey1
        paramsOrder:
          - id
          - createdAt
          - requestId
          - something
        keyParams:
          - id
          - requestId
          - Kernel.Prelude.True|CIM
          - createdAt
          - something: Maybe UTCTime
        dbQuery: findAAAAA
        dbQueryParams:
          - id
          - requestId
          - True|CIM
          - minFare
      findOnlyQuery:
        withCrossAppRedis: true
        returnType: Array
        queryType: FindOnly
        keyMaker: makeKey1
        paramsOrder:
          - id
          - createdAt
          - requestId
          - something
        keyParams:
          - id
          - requestId
          - Kernel.Prelude.True|CIM
          - createdAt
          - something: Maybe UTCTime
    extraOperations:
      - EXTRA_CACHED_QUERY_FILE

```
- Generated Code:
```haskell
cacheQuery ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Domain.Types.Estimate.Estimate -> m ())
cacheQuery id requestId createdAt something dataToBeCached = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeKeyForCaching id requestId Kernel.Prelude.True createdAt something) dataToBeCached expTime

deleteQuery :: CacheFlow m r => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
deleteQuery id requestId = do Hedis.withCrossAppRedis (Hedis.del $ "driverOffer:CachedQueries:Estimate:" <> ":Id-" <> Kernel.Types.Id.getId id <> ":RequestId-" <> Kernel.Types.Id.getId requestId <> ":Constant-" <> show (0))

findAndCacheQuery ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Common.HighPrecMoney -> m ([Domain.Types.Estimate.Estimate]))
findAndCacheQuery id createdAt requestId something minFare = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeKey1 id requestId Kernel.Prelude.True createdAt something)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.withCrossAppRedis $ Hedis.setExp (makeKey1 id requestId Kernel.Prelude.True createdAt something) dataToBeCached expTime
              )
                /=<< Queries.findAAAAA id requestId True minFare
        )

findOnlyQuery ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m ([Domain.Types.Estimate.Estimate]))
findOnlyQuery id createdAt requestId something = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeKey1 id requestId Kernel.Prelude.True createdAt something)
    >>= ( \case
            Just a -> pure a
            Nothing -> pure []
        )

```
---
#### Complex Types
- These are user defined type apart from the main data type
- Beam tables are not created for this types.
- `fromTType` must be mentioned for complex types excluding enums.
- These are generally enums and small types which are used in the main Data type.
- To make Enum use `enum` keyword.
- To derive use `derive` keyword. Most of the required deriving are already included but if any extra required user can add using it.
- To override default derives use `derive'` keyword.
- To make newtype or type instead of data use `recordType: NewType | Data (Default) | Type`
  Examples:
  ```yaml
  LmsModule:
    fields:
      id: Id LmsModule
      category: LmsCategory
      question: Question

    types:
      LmsCategory:
        enum: "Safety, Financial, Training"
      QuestionType:
        enum: "Maths, Physics, Chemistry"
        derive: "Show"
      Question:
        question: Text
        tp: QuestionType
        derive: "Show,Eq"
    fromTType:
      question: mkQuestion
  ```
  Beam side the above `Question` complex type will be split into many simple fields unless a specific beam type is mentioned by user.
  ```haskell
  data LmsModuleT f = LmsModuleT
    { category :: B.C f Domain.Types.LmsModule.LmsCategory,
      id :: B.C f Kernel.Prelude.Text,
      questionQuestion :: B.C f Kernel.Prelude.Text,
      questionTp :: B.C f Domain.Types.LmsModule.QuestionType,
      merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
      merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
      createdAt :: B.C f Kernel.Prelude.UTCTime,
      updatedAt :: B.C f Kernel.Prelude.UTCTime
    } deriving (Generic, B.Beamable)
  ```

#### Generating SQL Indexes or Unique constraints
- Toggle index generation by adding `GENERATE_INDEXES` to extraOperations section
- Adding SecondaryKey constraint to a field will generate index for that field (Default case)
  ```yaml
    constraints:
      domain: PrimaryKey
      version: PrimaryKey
      order: PrimaryKey|SecondaryKey
  ```
  Output:
  ```sql
  CREATE INDEX app_dynamic_logic_element_idx_order ON atlas_app.app_dynamic_logic_element USING btree ("order");
  ```
- If required to toggle off the above default case we can add `NO_DEFAULT_INDEXES` to extraOperations
- To generate anything extra apart from the above cases we can use `extraIndexes` option:
  Syntax -
  ```yaml
  extraIndexes:
    - name: < Optional field >
      columns: [field1, field2 .. ]
      unique: <Optional Boolean field > Used for making unique constraint
  ```
  Examples 1:
  ```yaml
  extraIndexes:
    - columns: [domain, version]
  ```
  Output
  ```sql
  CREATE INDEX app_dynamic_logic_element_idx_domain_version ON atlas_app.app_dynamic_logic_element USING btree (domain, version);
  ```

  Example 2:
  ```yaml
  extraIndexes:
    - columns: [domain, version]
      unique: true
  ```
  Output:
  ```sql
  ALTER TABLE atlas_app.app_dynamic_logic_element ADD CONSTRAINT app_dynamic_logic_element_unique_idx_domain_version UNIQUE (domain, version);
  ```


#### Extra Operations
- This a part where we can define an array of operation that need to be done.
- For now we have these operations:
  - `EXTRA_QUERY_FILE` : This operation is used to make an extra Query file which can be edited by user. This can be used if there is a complex query which user is not able to create using dsl query builder.
  Example:
    ```yaml
    extraOperations:
        - EXTRA_QUERY_FILE
    ```
  - `EXTRA_DOMAIN_TYPE_FILE`: Used to create extra Domain Type file
  - `EXTRA_CACHED_QUERY_FILE`: Creates extra Cached Query File
  - `GENERATE_INDEXES`: Toggle for generating indexes
  - `NO_DEFAULT_INDEXES`: Toggle for stopping default index generations wrt secondary keys
  - `EXTRA_API_TYPES_FILE`: Creates extra API Types File which can be edited by user. This can be used if user need to define a complex type or custom instance, which is not able to generate using dsl
  - `EXTRA_API_COMMON_TYPES_FILE`: Creates extra API Common Types File which can be edited by user. Difference between this operation and previous one is that `EXTRA_API_COMMON_TYPES_FILE` does not import generated api types, but vise versa it can be imported by generated api types module
