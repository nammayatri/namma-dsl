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
- `types`: Defines the request and response types for your APIs. This field is optional. Same as [Complex Types in Storage DSL](#complex-types). Types are defined in the following format:
    ```
    {TypeName}:
      {field1}: {field1Type}
      {field2}: {field2Type}
      derive: {Any extra derivation}
    ```
    Enum types can be defined as:
    ```
    {TypeName}:
      enum: {enum1},{enum2}
      derive: {Any extra derivation}
    ```
- `apis`: Contains all the APIs in the following format:
    - `{httpMethod}` (HTTP method GET | POST | PUT | DELETE)
        - `endpoint`: API path
           ```
             /path1/path2/{pathParam1}/path3/{pathParam2}
           ```
           **Note:** Path param types should be mentioned in the **params** part below.
        - `response`:
          - `type`: Type of response
        - `request`:
          - `type`: Type of request (optional)
        - `auth`: Authentication method (default: TokenAuth) [See More](#api-auth)
        - `query`: List of query parameters
          ```
          {queryParam1}: {queryParam1Type}
          ```
        - `mandatoryQuery`: List of mandatory query parameters
          ```
          {mandatoryQueryParam1}: {mandatoryQueryParam1Type}
          ```
        - `params`: List of path parameters
          ```
          {pathParam1}: {pathParam1Type}
          {pathParam2}: {pathParam2Type}
          ```
- Example:
  ```yaml
    imports: {}
    module: Sos
    types:
      SosRes:
        sosId: Id Sos

      SosDetailsRes:
        sos: Maybe Sos

      SosReq:
        flow: SosType
        rideId: Id Ride

      SosUpdateReq:
        status: SosStatus
        comment: Maybe Text

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
  - `fromTType`: FromTType of fields, if applicable. [See More](#tottype-and-fromttype)
  - `toTType`: ToTType of fields, if applicable. [See More](#tottype-and-fromttype)
  - `excludedFields`: There are some common fields like merchantId, merchantOperatingCityId, createdAt and updatedAt which are auto added in the data type. To remove them use this.
      ```yaml
      excludedFields:
        - merchantOperatingCityId
        - merchantId
      ```
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
      params: Array of field to be updated in update queries
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
- List of where operators:
    - and
    - or
    - in
    - eq
    - gt
    - lt
    - gte
    - lte

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
#### Complex Types
- These are user defined type apart from the main data type
- Beam tables are not created for this types.
- `fromTType` must be mentioned for complex types excluding enums.
- These are generally enums and small types which are used in the main Data type.
- To make Enum use `enum` keyword and to derive use `derive` keyword. Most of the required deriving are already included but if any extra required user can add using it.
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

#### Extra Operations
- This a part where we can define an array of operation that need to be done.
- For now we have these operations:
  - `EXTRA_QUERY_FILE` : This operation is used to make an extra Query file which can be edited by user. This can be uses if there is a complex query which user is not able to create using dsl query builder.
  Example:
    ```yaml
    extraOperations:
        - EXTRA_QUERY_FILE
    ```
