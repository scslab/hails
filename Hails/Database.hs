{-# LANGUAGE Trustworthy #-}

{- |

This module exports the database interface used by apps and policy
modules to carry out database queries. The Hails data model is similar
to that of MongoDB. Below we highlight some similarities and
difference. We refer the interested reader to the documentation in
"Hails.PolicyModule" for more details on the role of labels in Hails.

At the coarsest level code can execute database actions ('DBAction')
against the 'Database' of a policy module using 'withPolicyModule'.
Different from MongoDB's notion of a database, Hails databases have an
associated 'Label' which is used to restrict who can access the
database.

Each 'Database' is composed of a set of 'Collection's. The existence
of a collection is protected by a collection-set label, which is,
intern, protected by the database label. A collection is an approach
to organizing and grouping elements of the same model. For example,
collection \"users\" may contain elements (documents) corresponding to
users of the system. Each collection has a label, clearance, and
associated collection policy. The label of a collection serves the
same role as the database label, but at a finer grain: it protects who
can read and write to the collection. The collection clearance is also
a label, but its role is to set an upper bound on the sensitivity of
data that is and can be stored in the collection. For example, the
collection \"user\" may set a clearance such that the system\'s
private keys cannot be stored in the collection (by accident or
malice). The collection policy specifies how elements of the
collection are to be labeled when retrieved from the database.

The aforementioned elements of a collection are documents of type
'HsonDocument'. Documents are the basic storage units composed of a
fields (of type 'HsonField'), which are effectively key-value pairs.
The first part of the collection policy is to specify how such
documents are labeled upon retrieval from the database. Namely, by
providing a function from the document to a label.  Keys, or field
names, have type 'FieldName' while values have type 'HsonValue'. Hails
values are a subset of MongoDB's BSON specification. The second part
of the collection policy is used to specify if a field value is
publicly-searchable (i.e., readable by anybody that can read from the
collection) or labeled according to a function that may depend on the
data contained within the document itself. Hence, different form
MongoDB\'s documents, Hails documents are typically labeled and thus
protect the potentially-sensitive data contained within.

This module is analogous to "Database.MongoDB" and uses MongoDB as the
backed. Since the interfaces are similar we recommend glancing at
their documentation as well.

-}

module Hails.Database (
  -- * Hails database monad
    DBAction, MonadDB(..)
  , withDBContext
  , withPolicyModule
  , getDatabase, getDatabaseP
  -- ** Exception thrown by failed database actions
  , DBError(..)
  -- * Database layers
  -- ** Database
  , DatabaseName
  , Database, databaseName, databaseLabel, databaseCollections
  -- ** Collection
  , CollectionName
  , CollectionSet
  , Collection, colName, colLabel, colClearance, colPolicy
  -- ** Policy errors
  , PolicyError(..)
  -- ** Documents
  , module Hails.Data.Hson
  , LabeledHsonDocument
  -- * Database queries
  -- ** Write (insert/save)
  , InsertLike(..)
  -- ** Read
  , find, findP
  , next, nextP
  , findOne, findOneP
  -- *** Cursor
  , Cursor, curLabel
  -- *** Selection
  , Select(..)
  , Selection(..)
  , Selector
  -- *** Query
  , Query(..)
  , QueryOption(..)
  , Limit
  , BatchSize
  , Order(..)
  -- ** Delete
  , delete, deleteP
  ) where

import Hails.Data.Hson
import Hails.Database.Core
import Hails.Database.TCB
import Hails.Database.Query
import Hails.PolicyModule
