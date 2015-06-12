ebenezer
========

[![Build Status](https://travis-ci.org/shyam334/ebenezer.svg)](https://travis-ci.org/shyam334/ebenezer)


```
Scalding and Cascading support for using scrooge with parquet.
```

[Scaladoc](https://commbank.github.io/ebenezer/latest/api/index.html)

Usage
-----

See https://commbank.github.io/ebenezer/index.html

### Introspection API

Iterator of records for a path:
```scala

import au.com.cba.omnia.ebenezer.introspect.ParquetIntrospectTools
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path

val conf = new Configuration
ParquetIntrospectTools.iteratorFromPath(conf, new Path("some/path/x.parquet")).foreach(record =>
  println(record)
)
```

### Introspection CLI

The introspection tool can be packaged in a fat jar using `./sbt assembly` and the run using
Hadoop
```
hadoop jar ebenezer-assembly-$VERSION.jar au.com.cba.omnia.ebenezer.cli.Cat some/path/x.parquet
```
This process may take some time as Hadoop unpacks the fat jar prior to running.  Running under
OS X (or other case-insensitive filesystems) may result in an error relating to a file called
`META-INF/LICENSE` conflicting with a lower-case file of the same name.  If this occurs, a
workaround is to remove `META-INF/LICENSE` from the fat jar as follows:
```
zip -d ebenezer-assembly-$VERSION.jar META-INF/LICENSE
```

### Read Parquet / Scrooge via Scalding

Given a thrift record of:
```thrift
#@namespace scala com.cba.omnia.ebenezer.example

struct Customer {
  1: string id
  2: string name
  3: string address
  4: i32 age
}
```

Create a scalding `TypedPipe` with:

```scala

import au.com.cba.omnia.ebenezer.example.Customer
import au.com.cba.omnia.ebenezer.scrooge.ParquetScroogeSource
import au.com.cba.omnia.ebenezer.scrooge.hive.PartitionHiveParquetScroogeSource
import com.twitter.scalding._

/* create a consumable TypedPipe of any ThriftStruct from parquet file(s). */
val pipe: TypedPipe[Customer] =
  ParquetScroogeSource[Customer]("customers")

val conf = new HiveConf

vap pipe2: TypedPipe[Customer] =
  PartitionHiveParquetScroogeSource[Customer](args("db"), args("table"), List(("id", "string")), conf)


```


### Write Parquet / Scrooge via Scalding

Given same `Customer` thrift record.

Write out a scalding `TypedPipe` with:

```scala

import cascading.flow.FlowDef
import au.com.cba.omnia.ebenezer.example.Customer
import au.com.cba.omnia.ebenezer.scrooge.ParquetScroogeSource
import au.com.cba.omnia.ebenezer.scrooge.hive.PartitionHiveParquetScroogeSink
import com.twitter.scalding._
import com.twitter.scalding.typed.IterablePipe

val data = List(
  Customer("CUSTOMER-1", "Fred", "Bedrock", 40),
  Customer("CUSTOMER-2", "Wilma", "Bedrock", 40),
  Customer("CUSTOMER-3", "Barney", "Bedrock", 39),
  Customer("CUSTOMER-4", "BamBam", "Bedrock", 2)
)

/* given any TypedPipe of a ThriftStruct */
val pipe: TypedPipe[Customer] =
  IterablePipe[Customer](data, implicitly[FlowDef], implicitly[Mode])

/* write using a ParquetScroogeSource of that type. */
pipe.write(
  ParquetScroogeSource[Customer]("customers"))

pipe.map(c => (c.id, c))
  .write(PartitionHiveParquetScroogeSink[String, Customer](args("db"), args("table"), List(("id", "string")), conf))

```

#### Partitioning

Currently it is not possible to read the partition information back in. Instead only the actual
content of the files can be read back in. Hence to read Hive information it is important to not have
information as part of the partition columns/path that is not in the files themselves.

In order to avoid confusing Hive and Impala the name for partition columns has to be different to
the name of fields in the thrift structure.


Internals
---------

### Parquet Integrations

Lets just say they are horribly complex. And even more so when you are dealing with
things like thift/scrooge that don't easily map onto the underlying structure.

From top to bottom we have these components:

```
*Source                 -- Scalding integration

*Scheme                 -- Cascading integration

*{Input,Output}Formats  -- Hadoop integration, note that there are 4 provided versions,
                           ParquetInputFormat, ParquetOutputFormat, DeprecatedParquetInputFormat and
                           DeprecatedParquetOutputFormat. There Deprecated* versions are for Hadoop
                           1.x, the other versions are for Hadoop 2.x. However, there are some
                           configuration mechanisms that are done under ParquetInputFormat/
                           ParquetOutputFormat that are for _both_ the 1.x and 2.x versions (for
                           example, setReadSupport).

{Read,Write}Support     -- Used by the {Deprecated}Parquet{Input,Output}Formats for customization of
                           record reading and writing.

*{Read,Write}Support    -- Implementations of {Read,Write}Support for a specific integration.

*RecordMaterializer     -- Top level of ReadSupport responsible for reading all records via a
                           RecordConverter

*RecordConverter        -- Conver one parquet record into one target record.

*StructConverter        -- Map integration library structs (e.g. Thrift) onto the Parquet schema,
                           used in both read and write. Closely related to *SchemaConverter just
                           at a different layer. *StructConverters are normally for intra format
                           conversion (i.e. thrift to parquet-thrift).

*SchemaConverter        -- Map integration library structs (e.g. Thrift) onto the Parquet schema,
                           used in both read and write. Closely related to *StructConverter just
                           at a different layer. *StructConverters are normally for inter format
                           conversion (i.e. parquet-thrift to raw parquet).

*WriteProtocol          -- Does actual conversion from data structure to bytes.

*ReadProtocol           -- Does actual conversion from byte to data structure.

```

This is a lot of things that fit together to do not very much. But they are all essential to
getting something integrated with parquet. The parquet-mr project already provides many
integrations, and in practice you can leverage those quite heavily. For example, the scrooge
support in ebenezer works on top of the parquet-thrift integration and doesn't start from
scratch.

The integrations included in ebenezer are largely based on code structures from the parquet-mr
project (and its submodules). For example the scrooge integration closely matches what exists
for the libthrift integration included in parquet-mr.

NOTE: parquet-mr includes a scrooge integration but it is not complete (no write support) and
      will unlikely ever be generally useable as there is no infrastructure for doing cross-compiles
      and other essentials for existing in a scala world.

### Introspection

The purpose of the introspection integration is to be able to read files in a parquet format
with an unknown structure.

The core of introspection is the `com.cba.omnia.ebenezer.introspect.Record` data type. It
represents a single parquet record of any schema.

The most common use of the introspection is for simple command line tools like a `cat` like
command that will stream the contents of parquet file (but in a readable way). An example
of this is included in ebenezer, see `com.cba.ombnia.ebenezer.cli.Cat`.

The introspection integration is somewhat straight forward, the most complicated part are
the `Introsepction*Converter` types which are used to recursively decode the parquet structure.
To understand the converters, the entry point is `IntrospectionRecordConverter`. which matches
out enums, primitives, lists, maps and nested records. Enum and Primitive converters do just
what they say on the box, but List and Map converters will match on their internal structure
and may loop back onto any of the other converters.


### Scrooge

The purpose of the scrooge integration is to be able to read files in a parquet format
with a structure defined by Scrooge generate Struct/Codecs.

The most straight forward way to do this is via `scalding` with the `ParquetScroogeSource`.
This source, can be used for both reading and writing with a `TypedPipe`. The test cases in
`com.cba.omnia.ebenezer.scrooge.ParquetScroogeSourceSpec` provide reasonable examples for
doing so.

A rough mud map of the integration is (note that `*` indicated parquet-mr provided components):

High level:

```

      [ ParquetScroogeSource: Scalding integration ]
                          |
                          | (depends on)
                          v
      [ ParquetScroogeScheme: Cascading integration ] - (inherits from) -> [ ParquetValueScheme*: Base parquet-cascading integration ]
           |                                                                                                      |
           | (uses for the heavy lifting)                                         (configures read/write support) |
           |                                                                                                      |
           |-  [ DeprecatedParquetOutputFormat* ]--- (configured via ) -----> [ DeprecatedParquetOutputFormat* ] -|
           \-  [ DeprecatedParquetInputFormat* ] --- (configured via ) -----> [ ParquetInputFormat* ]            -/


```

Read pipeline (driven by `DeprecatedParquetInputFormat`):

```
      [ ScroogeReadSupport: Wires together read pipeline ]--- (configured by) ---> [ ScroogeReadWriteSupport: holds thrift struct class name ]
                        |       \
                        |        \ (Builds a parquet-thrift StructType via a scrooge.ThriftStructCodec)
      (creates to read  |         \
         all records)   |          \- [ ScroogeStructConverter ]
                        |          /
                        v         / (Output StructType is used by Materializer/Converter to read records)a
      [ ScroogeRecordMaterializer ]
                  |
                  | (creates to read each record, via the ThriftStructCodec and the parquet-thrift integration)
                  v
      [ ScroogeRecordConverter ]
                  |
                  | (relies on parquet-thrift integration for everything _except_ decoding the actual bytes)
                  v
      [ ThriftRecordConverter* ]

```

Write pipeline (driven by `DeprecatedParquetOutputFormat`):

```
      [ ScroogeWriteSupport: Wires together write pipeline ]--- (configured by) ---> [ ScroogeReadWriteSupport: holds thrift struct class name ]
                        |       \
                        |        \ (Builds a parquet-thrift StructType via a scrooge.ThriftStructCodec)
      (creates to write |         \
         all records)   |          \- [ ScroogeStructConverter ]
                        |          |
                        |          | (Uses the parquet-thrift StructType to build a parquet MessageType)
                        |          |
                        |          /- [ ThriftSchemaConverter ]
                        |         /
                        |        / (Output StructType and MessageType is used by parquet ColumnIO / ParquetWriteProtocol.
                        v       /
      [ ParquetWriteProtocol*: Does actual writing of records ] --- (wraps) --> [ ColumnIO*: manages read / write with column schema  }
```

Example Apps
------------

The example apps are in `example1` and `example2`. You need to copy a `hive-site.xml` into
`example{1,2}/src/main/resources/hive-site.xml` from either the `hive-site.xml.local`
(if you are running locally) or `hive-site.xml.remote` (if you are running on the cluster).

`hive-site.xml.remote` should be the cluster's `hive-site.xml` but with three extra properties:

```xml
<property>
  <name>hive.metastore.uris</name>
  <value>thrift://data-2.cluster:9083</value>
</property>
<property>
  <name>hive.exec.dynamic.partition.mode</name>
  <value>nonstrict</value>
</property>
<property>
  <name>yarn.resourcemanager.address</name>
  <value>bogus</value>
</property>
```

To run locally, use `example{1,2}/bin/test-local`.

To run on a cluster, it will scp the jar to `mgmt`, use `example{1,2}/bin/test-remote`.

Job 1 populates hive with some parquet data. Job 2 must run after Job 1. Job 2 queries that
data with HQL and creates a new table.

Future
------

 - Add a shell exec wrapper around cli tools so they are easier to deploy.  Avoid a fat jar
   if possible, to make execution faster.
 - Add more interesting introspection / query tools.
 - Add a more direct way of writing data (at the moment, reads can happen via MR or directly,
   but writes always go via MR).
 - Push down stream changes into scrooge which would help remove some of the reflection code
   used to interogate the generated ThriftCodec.

Known Issues
------------

* Writing out hive files currently only works if the metastore is specified as thrift endpoint
  instead of database.
  ```
    <property>
      <name>hive.metastore.uris</name>
      <value>thrift://metastore:9083</value>
    </property>
    ```
* In order to run queries the hive-site.xml need to include the `yarn.resourcemanager.address`
  property even if the value is bogus.
  ```
    <property>
      <name>yarn.resourcemanager.address</name>
      <value>bogus</value>
    </property>
  ```
* In order to run queries with partitioning the partition mode needs to be set to nonstrict.
  ```
    <property>
      <name>hive.exec.dynamic.partition.mode</name>
      <value>nonstrict</value>
    </property>
  ```
* The default heap size on the client machine is not large enough to deal with many hundreds of
  parquet files with schemas containing many hundreds of columns.

  The way to increase the client heap size will depend on your hadoop installation: it may involve
  setting `HADOOP_HEAPSIZE`, or using cloudera manager, or perhaps something else. Be sure to test
  your settings afterwards to ensure that hadoop hasn't overriden your setting with another value
  from another piece of configuration.
