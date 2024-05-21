

# Naming Conventions

The following specification applies to any S/4 core development in SHAPE+ project except the following:

- Tools from Basis support coming with package Z3S*
- Interface monitor ZSMO (package ZSX*)
- Certain ZSMO interfaces from DPAG, which are migrated in a Brownfield approach. Names and package assignment of these objects are not changed during migration.

## Packages

The primary tasks of the package concept are to group development objects and to control dependencies. Package structures can be formed on the basis of functional, technical and organisational criteria.

- Coding from SAP Notes are assigned to package `ZOSS`.
- Development objects, which are related to the **Application Interface Framework (AIF)** such as custom structures are assigned to package `ZAIF`.
- In-app extensibility items are assigned to local package `$INAPP` and later reassigned.
- All extensions are assigned to package `ZFIN` and its subpackages. 
- Packages for BW extractors follow the following convention `ZFIN_ETL_<process tower>`

Package `ZFIN` is the root package of a package hierarchy:

  Level 0: `ZFIN`  
  Level 1: `ZFIN-<process tower>`, e.g. `ZFIN_CA`  
  Level 2: `ZFIN-<process tower>-<module>`, e.g. `ZFIN_CA_OM`  
  Level 3: `ZFIN-<process tower>-<submodule>`, e.g. `ZFIN_CA_OM_CCA`  
  
Level 2 and level 3 are structured according to IMG.  
  
To put it more explicitly:
  
     `ZFIN`
       `ZFIN_CA`
          `ZFIN_CA_OM`
             ...
             `ZFIN_CA_OM_CCA`
             ...
          `ZFIN_CA_PC`
             ...
          `ZFIN_CA_CROSS`   
       `ZFIN_RTR`
          ... 
       `ZFIN_OTC`
          ...
       `ZFIN_PTP`
          ...
       `ZFIN_MDM`
          ...
       `ZFIN_CROSS`
          ...

Packages ending with `_CROSS` indicate overarching development objects.


## General rules

According to the modern ABAP Best Practices (Clean Code), using of prefixes is obsolete.

Development themes will now differ in terms of belonging to the development package.

Although the old rule that all **dictionary objects** should start with the letter `Z` remain.

When changes are to be made in the legacy code, the naming convention should follow the rules already used in the legacy code.

## Development Objects

The namespace for development objects is defined as follows: `Z*`

### Tables, Lists, Arrays

Development objects like tables, lists or arrays, which are intended to contain multiple rows or values should use the plural in their names. A single row or value should use the singular.

Example: `countries` for a local table of countries and `country` for the row from it.

### Classes, Interfaces, and Objects

Classes, Interfaces, and Objects should not have prefixes (except Z) in their names. They also should be named using **nouns**.

Example: `CLASS zuser_preferences_`

### Programs/Transactions

Program names should not have prefixes (except Z) in their names. They should be named using a combination of **nouns** and **verbs**. 

Example 'Program ZQUEUE_REPROCESS' , 'Program ZTRIGGER_GR_POST'

### Enhancement Names

Enhancement names should not have prefixes (except Z) in their names. They should be named using a combination of **nouns** and **verbs**.  

Example 'BADI Implementation ZVALIDATE_ALLOC' , 'BADI Implementation ZWKLIST_ITEM_CREATE'

### Methods and Function Modules

Methods and function modules should not have prefixes (except Z) in their names. They also should be named using **verbs**.

Example: `METHODS read_entries`.

Boolean methods, wherever possible, should use verbs like `is_` or `has_`.

Example: `IF is_empty( table )`.

### Import/Export Parameters

Import and export parameters should not have prefixes.

### FORM subroutines

No prefixes, e.g. to distinguish between USING and CHANGING parameters, should be used.

### Visibility, Context

No prefixes are to be used to distinguish between the kinds of visibility.

To avoid name clashes within the method of class, e.g. with importing parameters, using the self-reference `me->` is proposed.

Example:
```ABAP
class zgame_board_as_list definition.
  public section.
    methods constructor
      importing x_dimension type i
                y_dimension type i.
  private section.
    data x_dimension type i.
    data y_dimension type i.
endclass.

class zgame_board_as_list implementation.
  method constructor.
    me->x_dimension = x_dimension.
    me->y_dimension = y_dimension.
  endmethod.
endclass.
```

### Constants

Constants should not have prefixes and should be grouped in **enumeration classes** (instead of interfaces) as attributes, because interfaces generally supposed to be later on implemented in classes and should not be used a storage of values.

Example:
```ABAP
CLASS zmessage_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

When collecting constants in a loose way, for example in local report, they also should be grouped.

Example:
```ABAP
CONSTANTS:
  BEGIN OF message_severity,
    warning TYPE symsgty VALUE 'W',
    error   TYPE symsgty VALUE 'E',
  END OF message_severity
```

### Fields in Tables and Structures

Since developments mainly take place in the customer namespace (`Y` and `Z`), a prefix (`ZZ_`) is generally omitted for field names.

If fields of master data management are stored in SAP-standard tables or structures, the field name is supplemented with the prefix `ZZ_`.

For fields originating from the Custom Fields app, where the system enforces the use of the `ZZ1_` prefix, that prefix supersedes others.

### Return of values

The following rules should be used when returning values from methods:
- In methods with exactly one simple return value (variable, object reference), returning parameters are preferred.
- In all other cases, exporting or changing parameters are used.

### Status feedback

Example: Success notification from change methods (action carried out or action not carried out).

Recommendation:
- The successful execution of the method is implicitly confirmed by a smooth process without exception. Status notification is not needed.
- An unsuccessful execution of the method is indicated by missing return values (to be checked by the caller!).
- Return parameters, such as `OK`, are only to be defined and used if they are necessary for controlling the program flow further.

### Error handling

Examples:
- Import parameters or mandatory fields are missing
- Access to the object is blocked
- Functionally/technically inconsistent data in the object

Recommendation:
- In case of an error, an exception with an explanatory error message must be generated (possibly long text).

Technical advice:
- Defining an exception class for each package. Format: `Z*`
- Using a common message class for all exception classes. Format: `Z*`

## CDS Objects

### CDS Entity

CDS entities use the following:
| View Type | Annotation | Prefix |
| --- | --- | --- |
| Basic interface view | `@VDM.viewType: #BASIC` | Mandatory: `ZI_`<br>Recommended: `ZI_B` |
| Composite interface view | `@VDM.viewType: #COMPOSITE` | Mandatory: `ZI_`<br>Recommended: `ZI_C` |
| Consumption view (projection) | `@VDM.viewType: #CONSUMPTION` | `ZC_` |
| Extension include view | `@VDM.viewType: #EXTENSION` | `ZE_` |
| View extension | `@VDM.viewExtension: true` | `ZX_` |
| Private view | `@VDM.private: true` | `ZP` |
| Remote API view | `@VDM.lifecycle.contract.type: #PUBLIC_REMOTE_API` | `ZA_` |

For custom CDS objects that originate from the Custom CDS Views app, where a system-enforced prefix must be used (e.g. `ZZ1_`), then that may replace the first `Z` in the above list of prefixes.

### Access Control (DCL source)

The Access Control name should be identical to the name of its controlled CDS view.

### Behavior Definition

A behavior definition always has the same name as the root entity of the business object.

### Behavior Implementation

Classes that implement the behavior of a business object begin with prefix `ZBP_` (`BP` = Behavior Pool).

Example: `ZBP_TRAVEL_U`.

### Metadata Extension

A metadata extension has the same name as the CDS entity it relates to. If you use more than one metadata extension for one CDS entity, you can add a numbered suffix.

Example:
CDS entity: `ZC_BOOKING_U_M`
Metadata Extensions: `ZC_BOOKING_U_M2`

## Business Services

### Service Definition

In general, service definitions will have a `Z` prefix.

### Service Binding

The name format for service binding is
`ZX_*_Y`

Values for *X*:
| Value | Meaning |
| --- | --- |
| UI | If the service is exposed as a UI service |
| API | If the service is exposed as Web API. |

Values for *Y*:
| Value | Meaning |
| --- | --- |
| 02 | If the service is bound to OData protocol version 2 |
| 04 | If the service is bound to OData protocol version 4 |

Example: `ZUI_TRAVEL_U_02`
