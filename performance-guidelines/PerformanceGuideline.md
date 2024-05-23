
# Performance Guidelines

## Content

- [Use column store](#use-column-store)
- [Checklist for database-specific implementations](#checklist-for-database-specific-implementations)
	- [Currencies and units](#currencies-and-units)
	- [Rounding behavior](#rounding-behavior)
	- [Text sorting](#text-sorting)
	- [Converting pool and cluster tables](#converting-pool-and-cluster-tables)
	- [Sort behavior](#sort-behavior)
- 	[Performance recommendations for Open SQL](#performance-recommendations-for-open-sql)
	- [Keep the result set small](#keep-the-result-set-small)
	- [Minimize amount of transferred data](#minimize-amount-of-transferred-data)
	- [Reduce the number of query executions](#reduce-the-number-of-query-executions)
	- [Use indexes properly](#use-indexes-properly)
	- [Push data-intensive logic down to the database](#push-data-intensive-logic-down-to-the-database)
    
## Use column store
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [This section](#use-column-store)

By default, store every table in the column store, as for most normal applications, only select fields are used at any given time, and column store tables give advantage in terms of searching and storage space required due to compression.

Tables that contain application data are always stored in the column store. This applies particularly to tables that contain a large number of data records, because the column store provides better compression properties. This also applies to tables that are to be used for text searches.

An exceptional case to use row store would be, if, for example, a table is accessed predominantly by time-critical DML statements (Data Manipulation Language, e.g. `UPDATE`, `INSERT`, or `DELETE`). This must not be an application table on which you subsequently want to perform analyses. Therefore, primarily technical or internal SAP tables are the kinds of usage that are qualified to be defined as row store.

## Checklist for database-specific implementations
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [This section](#checklist-for-database-specific-implementations)

### Currencies and units
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Checklist for database-specific implementations](#checklist-for-database-specific-implementations) > [This section](#currencies-and-units)

When handling currencies or units of measure, you must ensure that any amount or quantity values take into account the pre-configured decimal places (e.g. amount fields, which may contain values in Japanese Yen). When handling with such fields, you must ensure that this displacement is taken into account before an output for an end user. This takes place in ABAP, for example, via specific conversion functions or the `WRITE` statement, or via annotation `Semantics.amount.currencyCode` for amount fields and `Semantics.quantity.unitOfMeasure` for quantity fields in CDS.

### Rounding behavior
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Checklist for database-specific implementations](#checklist-for-database-specific-implementations) > [This section](#rounding-behavior)

When calculating with decimals, rounding behavior plays an important role, especially for monetary amounts. Small rounding differences can have a major impact on totals, so you should make sure to minimize rounding errors. When converting a currency, you should, if possible, only perform the conversion after an aggregation, which is also advantageous from a runtime perspective.

### Text sorting
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Checklist for database-specific implementations](#checklist-for-database-specific-implementations) > [This section](#text-sorting)

The sorting of texts depends on the current language settings. In the ABAP command `SORT`, therefore, the addition `AS TEXT` will sort the character strings alphabetically according to the set text environment. If you sort content in an SQL statement via the addition `ORDER BY`, however, it is sorted in a binary manner according to the internal presentation. For example in German, the name *“Möller”* appears after *“Muller,”* although it should appear alphabetically after “Moller.” For this reason, its recommended that you usually sort texts, which you present in an ABAP application for an end user, in an application server.

### Converting pool and cluster tables
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Checklist for database-specific implementations](#checklist-for-database-specific-implementations) > [This section](#converting-pool-and-cluster-tables)

During the migration to SAP HANA most pool and cluster tables are converted to transparent tables, problems may occur if there is code that relies on an implicit sort , or directly accesses the internal physical clusters or pools.

### Sort behavior
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Checklist for database-specific implementations](#checklist-for-database-specific-implementations) > [This section](#sort-behavior)

If no `ORDER BY` was specified in the SQL statement, the sequence in which the records are read is unpredictable. For pool and cluster tables, an implicit sorting is always performed by the database interface. This is lost after the conversion to a transparent table, because no automatic `ORDER BY` is added here to the statement. Access to pool and cluster tables must therefore be analyzed with regard to their sorting during a migration. In this case, the Code Inspector provides a separate check—”*Find SELECT for Pool/Cluster Tab without ORDER BY*”.

Changes can also occur in the implicit sort behavior for existing transparent tables. Classic row-oriented databases are usually accessed via a primary or secondary index. SAP HANA is column-oriented, there is no secondary index, and the data can be read in parallel. So, if you require a specific sorting of data when you access a database, use the addition `ORDER BY` explicitly.

## Performance recommendations for Open SQL
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [This section](#performance-recommendations-for-open-sql)

### Keep the result set small
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Performance recommendations for Open SQL](#performance-recommendations-for-open-sql) > [This section](#keep-the-result-set-small)

Keep the result set as small as possible when reading data from the database. You can minimize the result set using various measures: using a `WHERE` clause, `HAVING` clause, transferring only required rows.

### Minimize amount of transferred data
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Performance recommendations for Open SQL](#performance-recommendations-for-open-sql) > [This section](#minimize-amount-of-transferred-data)

Transfer as little data as possible between the database and the application server. If you require only a certain number of rows, use the `UP TO n ROWS` addition to further restrict the number of rows.

If the system calculates with a certain `WHERE` condition that has unnecessary duplicate entries regarding the selected columns, use the `SELECT DISTINCT` statement to remove the duplicate entries already in the database.

Select only columns in a database table that are also required in the ABAP program. Although the addition `INTO CORRESPONDING FIELDS OF` selects only the columns that are also in the above objective when `*` is specified, extra effort is involved in comparing names in the database interface. Use this addition sparingly and only for larger result sets

If data is required only for calculations, it is better to perform these calculations in the database and transfer the results rather than transferring all data and performing the calculation in the ABAP program.

Perform existence checks efficiently. To determine whether there is a data record for a specific key, for example, you should not use `SELECT COUNT(*)` because the number is irrelevant in this case. For such an existence check, you require only a single field of the data record you seek- this should be a field of the index that is in use. Use it together with `UP TO 1 ROWS`.

Change only required columns- when changing rows from work areas, too much data is usually transferred and columns that have not changed are also overwritten. Change with the `UPDATE` statement only the desired columns, mentioned in the `SET` statement.

### Reduce the number of query executions
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Performance recommendations for Open SQL](#performance-recommendations-for-open-sql) > [This section](#reduce-the-number-of-query-executions)

Each SQL statement in an ABAP program that is sent to the database involves a certain effort in the database. The statement itself and its associated parameters are transferred to the database. It must analyze the statement in terms of the syntax and search by hash function in the SQL cache. Also authorizations and the existence of database objects must be checked to ensure they are present. The results of the query must also be transferred. To reduce the load on the database, keep the number of accesses as low as possible.

Choose the addition `INTO TABLE` when reading with `SELECT` instead of the `SELECT ... ENDSELECT` loop. Such loops can also be avoided via the `FOR ALL ENTRIES` and `JOIN` constructs. Do not execute `SELECT` Statements in the loop via Internal Tables.

For write accesses, rely wherever possible on set operations with internal tables- use `INSERT ... FROM TABLE` instead of `INSERT INTO` in loop.

Make sure you do not repeatedly access the same data. For example, avoid a `SELECT` before a `DELETE` for the same data record.

### Use indexes properly
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Performance recommendations for Open SQL](#performance-recommendations-for-open-sql) > [This section](#use-indexes-properly)

In a classic DB you can minimize the effort of the data search with an index. This rule changes in SAP HANA, and it has a lower priority. This is because no index at all is required in SAP HANA in many cases due to most applications would use column store.

An index may still be required for very large tables, due to the CPU consumption could reduced by using the index. In SAP HANA, indexes are usually created for individual columns. Indexes that span multiple columns are the exception. Create an index for the single column that has the most selective condition.

### Push data-intensive logic down to the database
> [Performance Guidelines](#performance-guidelines) > [Content](#content) > [Performance recommendations for Open SQL](#performance-recommendations-for-open-sql) > [This section](#push-data-intensive-logic-down-to-the-database)

By using SAP HANA, the paradigm is to work close to the data. So where in a typical scenario, classically data would be brought over to the application server to be further processed, this has shifted to pushing the code down to the database where possible. However, there are still some caveats.

Accessing the buffer on the application server is still faster than accessing the database, also in the case of SAP HANA. Use buffering where appropriate. Do not use buffering for frequently changing data.

If the sorting in the database cannot be mapped via an index that is used for the selection, you should sort in the ABAP application server—especially if the total dataset to be sorted is required by the application. If, however, the sorting of a large dataset is required to calculate a smaller result (for example, determining the five best customers in relation to order value), the sorting should be left to the database.

Avoid the multiple reading of identical data. Usually, internal tables or even buffers are used to avoid identical accesses.
