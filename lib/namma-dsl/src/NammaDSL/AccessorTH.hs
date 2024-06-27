{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.AccessorTH where

import NammaDSL.Utils
import Text.RawString.QQ

$( makeAccKeysTH
     [r|
    fields
    excludedFields
    excludedDefaultQueries
    tableName
    fromTType
    toTType
    beamFields
    queries
    kvFunction
    orderBy
    params
    importPackageOverrides
    intermediateTransformers
    extraOperations
    derives
    beamInstance
    domainInstance
    types
    recordType
    imports
    constraints
    sqlType
    beamType
    default
    where
    module
    apis
    endpoint
    auth
    request
    type
    format
    response
    query
    mandatoryQuery
    headers
    name
    typeConstraint
    defaultQueryTypeConstraint
    fullObjectAsParam
    cachedQueries
    withCrossAppRedis
    keyMaker
    keyParams
    dbQuery
    dbQueryParams
    cacheDataType
    returnType
    paramsOrder
    queryType
    extImports
    |]
 )
