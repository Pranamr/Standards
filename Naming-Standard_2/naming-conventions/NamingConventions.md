
# **Naming Conventions**

## About This Document

### Introduction

The purpose of this reference document is to define naming conventions that will be utilized as the development standards for all SAP ERP development. By following these standards, the on-going maintenance of custom development objects will be a highly-manageable entity.



### Document Conventions

These are the symbols and word conventions used to help the user.

| **Convention** | **Explanation** |
| :-: | --- |
| 💡 | Tip boxes identify shortcuts or important information that may make processing easier. |

## Packages

Related objects in the ABAP Development Workbench are grouped together in a development class. The assignment of an object to a development class is entered in the object catalog (TADIR). The class determines the transport layer that defines the transport attributes of an object.

Development Classes can be a maximum of 30 characters long.

To expedite the implementation of an automated transport process, please evaluate the development class of all existing objects that you are modifying. If they do not belong to one of the approved development classes, please reassign them to one. The list of approved development classes and their descriptions can be found at the end of this document.

The use of the latest package capabilities is encouraged. Utilize package hierarchies and interfaces:

- To improve the structure of the software
g- To encapsulate software functions more strictly
- To make well-defined interfaces available to all applications
- To clarify responsibilities

| 💡 | Important to note to pick a package that is the correct transport layer, e.g.<br>Z\_FIN\_ENHANCEMENTS is set to go to QUALITY only (programs/tables)<br>Z\_FIN\_OUTPUT is set to go across clients and to QUALITY (for Smart Forms) |
| :-: | :-- |

Example: Z\_FIN\_OUTPUT

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates the type of objects contained in the development class. | $ – Local objects<br>T – Test objects (not automatically assigned to a correction)<br>Y – Customer temporary objects<br>Z – Customer objects for production<br>Others – SAP objects |
| 3 – 4 | Functional assignment<br>_Sub-module ID_<br>**Use of the Sub-module ID naming standard is optional** | [_View Functional areas_](#_Functional_Areas) |
| 5 – 30 | Free form text | Description of class |

## Correction and Transport System (CTS)

### Request and Task Descriptions

Example 1: _CR/RFCXXXXXXX_ Add addition logic for validation to user exit logic

Example 2: _PRBXXXXXX_ ALV addition field Output Issue

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 – N | CR#, RFC#, or PRB# |  |
| N – 60 | Free form text | Description of change |

## ABAP Objects

### ABAP Naming Conventions

Example: Z\_FIN\_BL\_UNAPPLY\_ALV

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates the type of objects contained in the development class. | $ -- Local objects<br>T -- Test objects (not automatically assigned to a correction)<br>Y -- Customer temporary objects<br>Z -- Customer objects for production<br>Others -- SAP objects |
| 3 -- N | Functional assignment<br>_Sub-module ID_<br>**Use of the Sub-module ID naming standard is optional** | [_View Functional areas_](#_Functional_Areas) |
| N -- 30 | Free form text | Description of class |

### ABAP Program Elements

Following is a discussion of each element, presented in the order they are usually required when creating a new ABAP program.

#### Attributes

##### Authorization Group

DHL requires that all custom developed programs have an authorization group assignment that meets the DHL's SAP Security and Authorization Guidelines.

##### Package

Packages are used in the Transport system. They are used to group all objects that are related to the same development to ensure they are and transported together.

##### Type

###### Executable program (1)

Use this type only where technically necessary, such as background jobs. With regards to ABAP Objects, these programs should **not** contain any operational statements outside of local classes.

The event block, START-OF-SELECTION, should be implemented merely as an entry point, where a method of a local class is invoked.

###### INCLUDE program (I)

Automatically set by the ABAP Workbench.

###### Module pool (M)

Automatically set by the Screen Builder.

###### Function group (F)

Automatically set by the Function Builder.

###### Subroutine pool (S)

Use this type when programming business application functionality in local classes, where a method is invoked by a transaction code.

A subroutine pool that contains only methods can be directly compared to a Java program that is executed by calling its main method.

###### Interface pool (J)

Automatically set by the Class Builder

###### Class pool (K)

Automatically set by the Class Builder.

###### Logical Database

Do NOT use a logical database as a program attribute. To use an existing logical database, call it explicitly using the function module LDB\_PROCESS from within a method.

##### Unicode Checks Active

This must always be set on new development.

##### Fixed-point arithmetic

This must always be set.

#### Naming

Example: Z\_FIN\_CR\_MALUS\_DISPLAY

**Note**: ABAP program names may be up to 40 characters long.

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer program for production<br>Y -- Test program |
| 3 -- N | Functional Area | [_View Functional areas_](#_Functional_Areas) |
| N -- 40 | Program description | Descriptive, yet concise text. Use underscores to enhance readability. |

#### Documentation

User documentation should be maintained using the ABAP documentation feature through the menu toolbar _Goto --- Documentation --- Change_. You will then be presented with SAP's standard program documentation template with paragraph headers for:

- TITLE: copied from the attributes screen title field.

- PURPOSE: detailed User description of the program's purpose. Include any precautions the user should be aware of resulting from the execution of this program.

- INTEGRATION: integration points with other applications or partner systems

- PREREQUISITES: describe any prerequisite activities.

- FEATURES:
	- Selection: advice on selection screen fields
	- Standard Variants: -pre-defined variants set up for general reporting needs
	- Output: content of the print list

- ACTIVITIES: follow-on activities. As an example, instructions for interactive reporting

- EXAMPLES: processing hints, helpful information on running the program.

This documentation is then available to the user at execution time through the same menu path described above or though "Program Documentation" icon which is automatically added to the report selection screen Application Toolbar in the GUI interface.

#### Text Elements

Text elements help you to write language-independent programs. They are stored in the text pool of their language, and can be translated using the normal translation process.

Prerequisite: You have maintained text elements in the original language.

##### Procedure

1. Choose _Utilities_ ---_Translation_ --- _Short and long texts_.
 The initial screen of the translation transaction (**SE63**) appears.
2. Choose _Translation_ --- _Short texts_ --- _Program texts_.
3. Enter the program name, the source language, and the target language.
4. Choose _Edit_.
 The text elements are displayed in the language in which you created them.

1. Translate the texts.
2. _Save_ your translation.

##### Result

You have created a text-pool for different languages.

Once you have created text-pools for different languages, you can change the language in which you run the program by changing one of the following:

- The logon language; the default language for your program is the logon language of the user.
- The SET LANGUAGE statement: this ABAP statement allows you to set the output language explicitly and independently of the logon language.

Syntax:
```ABAP
SET LANGUAGE <lang>.
```

The language *\<lang\>* can be a literal or a variable.

Once you have set a language (using either method), the system only looks in the text pool of that language. If it cannot find the relevant text symbols in that pool, it displays the default text specified in the program source code (if one exists), otherwise, it skips the corresponding WRITE statement.

| 💡 | No text should be hard-coded into the source code. All selection texts, plain text, and text within messages should be assigned a text element accordingly, or comes from a message class. |
| :-: | :-- |

| 💡 |  Messages to the user should be output using the message syntax and a message class. |
| :-: | :-- |

## Data Declaration

### Internal tables

If prefixing is required, internal table names should begin with a *GT\_* for global internal tables, or *_LT\_* for local internal tables. The remaining characters should be filled with a descriptive name in plural form, or the table name that it most closely resembles from the data dictionary.

Internal tables must not use a header row.

Internal tables which will be read multiple times should be a hashed or a sorted table when possible to enhance the performance.

Below is an example for data from MARA table:

```ABAP
DATA:
  gt_mara TYPE HASHED TABLE OF mara
    WITH UNIQUE KEY matnr.
```

When an internal table is used and the program only needs specific fields from the SAP table(s); a TYPE should be created with those specific fields, unless using in-line declaration.

The following internal table is a subset of the Plant Material master data table MARC.

```ABAP
TYPES:
   BEGIN OF ty_marc,
     matnr TYPE marc-matnr,
     werks TYPE marc-werks,
     mmsta TYPE marc-mmsta,
   END OF ty_marc.

DATA:
  gt_marc TYPE HASHED TABLE OF ty_marc
    WITH UNIQUE KEY primary_key COMPONENTS matnr werks.
```

### Variables

If prefixing is required, for variables that are defined after data dictionary fields, use the prefix *GV\_* for global variables and  *LV\_* for local variables. The remaining positions should be filled with the a descriptive text or field name from the data dictionary.

Classical ABAP:
```ABAP
DATA:
  gv_matnr TYPE matnr.
```

ABAP with in-line declaration:
```ABAP
DATA(matnr) = <vbap>-matnr.
```

When the variable is a structure or a work area, use the _GS\__ prefix for global structures and _LS\_ for local structures. This is helpful for maintenance of the program; other developers will be able to see in a quick glance this is a work area (i.e. structured variable) rather than just a single value.

Classical ABAP:
```ABAP
DATA:
  ls_marc TYPE ty_marc.
```

ABAP with in-line declaration:
```ABAP
SELECT matnr, werks, mmsta
  FROM marc
  INTO TABLE @DATA(ls_marc)
 WHERE matnr IN so_matnr
   AND werks IN so_werks.
```

| 💡 | Always use TYPE instead of LIKE to be Unicode compliant. |
| :-: | :-- |

| 💡 | Keep variables as local as possible, wherever possible. |
| :-: | :-- |

### Field-symbols

Avoid using field-symbols in a global context, which may cause issues for future maintenance.

Classical ABAP with prefix:
```ABAP
FIELD-SYMBOLS:
  <fs_bsid> TYPE ty_bsid.
```

ABAP with in-line declaration without prefix:
```ABAP
LOOP AT lt_bsid ASSIGNING FIELD-SYMBOL(<bsid>).
```

### Select-options

Begin with *S\_*, and the remaining positions should be filled with the field name from the data dictionary:

```ABAP
SELECT-OPTIONS:
  s_matnr FOR marc-matnr.
```

### Parameters

For parameters that are defined like data dictionary fields (use this whenever possible), begin with a _P\__, and the remaining positions should be filled with a descriptive text, or the field name from the data dictionary:

```ABAP
PARAMETERS:
  p_werks TYPE marc-werks.
```

### Types

With the new Unicode standards the TYPES statement will be utilized when defining structures for internal tables and work areas, and begin with _TY\__ for global types, or _LTY\__ for local types.

```ABAP
TYPES:
 BEGIN OF ty_marc,
   matnr TYPE marc-matnr,
   werks TYPE marc-werks,
   mmsta TYPE marc-mmsta,
 END OF ty_marc.
```

| 💡 | Use fields of tables as reference to give context. |
| :-: | :-- |

### Messages

All programs should contain at least one message of type *S* (Success) so that the end-user or support sees a response from the system. When programs are scheduled to run in the background, the only way to influence the job log is via message commands. All status and error messages should be accessed via the message class libraries.

#### New message classes

Creation of a message class or a new message should only be done if an accurate contextual message does not already exist in SAP.

To search for a message, execute SAP transaction SE16 or its equivalent, choose table **T100**, and enter a portion of the message into the Text field. Note that searching is case-sensitive.

Example: ZM

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z – Customer message class for production<br>Y – Test message class |
| 2 | Application Area | A – Assets Management<br>C – Production Planning<br>D – DASS (Control Station)<br>E – EDI<br>F – Financial Accounting<br>G – General Ledger<br>H – Human Resources<br>I – Plant Maintenance<br>J – Publishing<br>K – Controlling<br>L – Inventory Management<br>M – Materials Management<br>N – Hospital<br>P – Project Systems<br>R – EIS<br>Q – Quality Management<br>S – BASIS<br>U – Enterprise Data Model<br>V – Sales and Distribution<br>W – Merchandise Management System<br>X – Utility<br>Y – Customer Head Office<br>Z – Customer Branch |

#### New messages

Numbered 001 – 999, and the next free number should be used. Long text should be added to all new messages to provide a course of action if the message is displayed in the program.

### Header

```ABAP
*==========================================================*
*                   DHL IT SERVICES                        *
*==========================================================*
*  ABAP NAME   :  Z_FI_BL_UNPPLY_ALV                       *
*  AUTHOR      :  Venu Gopal T                             *
*  DATE CREATED:  02/11/2020                               *
*----------------------------------------------------------*
*  DESCRIPTION: Financial Bank Accounting                  *
*  This report lists the processed Customer Unapplied      *
*  bank transactions.                                      *
*----------------------------------------------------------*
*  P R O G R A M   C H A N G E   H I S T O R Y             *
*----------------------------------------------------------*
*  Change No.  Date        SAP User ID   Description       *
*----------------------------------------------------------*
*  CR/RFCXXXXX 01/11/2020  VENUTALA      Process Customer  *
*  unapplied bank transactions.                            * 
*==========================================================*
REPORT <repid> NO STANDARD PAGE HEADING
               MESSAGE-ID <XX>
               LINE-SIZE 132
               LINE-COUNT 65.
                               
* Data declaration, parameters, select options can be in a _TOP include
INCLUDE z_fin_bl_unapply_alv_top.

* All Forms can be in a separate _Fxx include
INCLUDE z_fin_bl_unapply_alv_f01.

* Function keys, etc.
AT SELECTION-SCREEN.

* Authorization checks, manipulate selection screen
INITIALIZATION.

* Select statements, get statements, file reads, etc.
START-OF-SELECTION.

* Processing after data retrieval
END-OF-SELECTION.
```

### Variants

Variants are program-specific objects that define set parameters entries required at program execution time. If the Variant is intended for use with a scheduled background job, the Variant should be built in the Development environment and moved to Quality and Production with the program source.

The name of the variant should include an identifier, e.g. plant code/customer number, or some text that that is self-explanatory.

## Function Groups

Example: ZFIN1

A function group is the main program for the function modules it contains. Functions that use the same data are normally assigned to the same function group. It is common practice to place all function modules that use the same data in a single function group. For example, if you have a set of function modules that all use the same internal table, you could place them in a function group containing the table definition in its global data.

Function Groups may be named using up to 26 characters.

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer program for production<br>Y -- Test program |
| 2 -- N | Functional assignment (freeform) | Use underscores to enhance readability. |

## Function Modules

Function modules should always reference data dictionary structures in the interfaces for export, import, and tables. This is mandatory if the function module being created is to be used for RFC (remote function call) interfaces.

Function Module names can be up to 30 characters.

Example: Z\_MM\_GET\_BOM

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer program for production<br>Y -- Test program |
| 2 | Underscore | To promote readability |
| 3 -- n | Functional Area | View [Functional areas](#_Functional_Areas) |
| n+1 | Underscore | To promote readability |
| n+2 -- 30 | Program description | Descriptive, yet concise text. Use underscores to enhance readability. |

## ABAP Classes

| 💡 | Create global classes if it will be re-used, or are expected to be re-used, by more than one application. |
| :-: | --- |

While building classes, restrict the number of public components. All components that can be private should be private.

The naming conventions are documented at the end of this section.

### Class Properties

**Instantiation**

Consider using private instantiation and offer appropriate factory methods. This allows you to:
- Control the instantiation of your class from within the class.
- Control the parameters passed to the constructor.
- Share the instances of your class effectively by passing the same object reference to different clients when appropriate.

**Final**

Set classes as 'Final'. Remove the 'Final' setting only when subclasses are needed for inheritance.

Use inheritance moderately. When in doubt, prefer interfaces to achieve polymorphism. Inheritance is one of the most powerful, but also one of the more subtle, features that ABAP OO offers. If you use inheritance (e.g. for reusing method implementations), keep the paths of your inheritance tree short and terminate them with final classes. For the reuse of interfaces, exploit the concept of interface composition in favor of inheritance.

**General Data**

| **Field** | **Setting** |
| --- | --- |
| Released internally | Always blank |
| Fixed point arithmetic | Always marked to on |
| Unicode checks active | Always marked to on |
| Shared Memory-Enable | Optional |
| Message Class | Optional |
| Program Status | Set to 'Customer Production Program' |

### Class Components

#### Forward Declarations

Forward Declarations replace Type-Pools and Include statements. Therefore, use 'Forward Declarations' to pull in pooled statements rather than coding the TYPE-POOLS or INCLUDE statement within class methods.

#### Documentation

- Use the Method Documentation editor to document all class methods. It is mandatory to document all public and protected methods. Documentation for Private methods is optional. However, complex logic should be documented within the method to enable easier maintenance.
- Use the Class Documentation text editor to document the Class and include links to the public method documentation. Links are added by using the menu path Insert-\&gt;Link and then choose 'Class-Method'.
- Class history (flowerbox) should be maintained at the bottom of the Class Documentation. To paste in the template while editing Class Documentation, navigate the menu path: Insert Text  Documentation. On the pop-up screen, enter "Z\_MAINTENANCE\_LOG" for the text name and select the option 'Expand Immediately'. Once the template is pasted into your class documentation, modify as desired.

#### Attributes

| 💡 | The visibility preference of attributes is private or protected over public. When public visibility is required, they should be declared as 'READ-ONLY', unless there is justification. Public attributes that are not declared as 'READ-ONLY' compromise the encapsulation property of object-oriented programming. |
| :-: | :-- |

- Always declare attributes as private or protected where possible.
- If public attributes are required, they should be declared as 'READ-ONLY', unless there is a requirement to reduce encapsulation.
- Only create a global attribute if the attribute is used by more than one method or by external classes. Otherwise keep attributes defined within their local methods.
- LIKE vs. TYPE: in data definitions, use TYPE instead of LIKE to be ABAP OO-compliant.

### Methods

- Methods that can be set with 'Private' or 'Protected' visibility, must be set accordingly
- Document all public and protected methods using the method documentation editor, with the exception of the CONSTRUCTOR and CLASS\_CONSTRUCTOR classes. Method documentation is not required for the CONSTRUCTOR and CLASS\_CONSTRUCTOR methods.
- The cyclomatic number of a method (the number of branches due to tests) should not be greater than 10.
- See the section ABAP Object Component Naming Conventions, for method naming convention.
- Use exception classes for error processing.
- The use of static methods, such as CLASS\_CONSTRUCTOR, should be limited. Instantiation is preferred over the direct use of classes via static methods are that you can:
	- Use inheritance and method redefinition, if needed.
	- Control the moment of instantiation and the lifetime of the object

#### Class-local types

When defining local classes and local types within a global class, separate the local class definition and types from the local class implementations, into their proper locations. See the workbench menu for Class-local types in the class builder.

#### Macros

Use methods or procedures to modularize the functionality of a program, rather than macros.

The use of macros is discouraged for the following reasons:
- Method parameters are regarded from the point of view of the method that implements them. Please see the section, Data Definition Naming Conventions, for method parameter naming conventions.
- They increase difficulty of reading code.
- Their implementation provides only bare minimum of functionality.
- The debugger cannot be used on macro-generated code.

#### Exception Classes

- In exception processing use class-based exceptions.
- Do NOT catch exceptions by specifying the abstract root classes of the exception tree: CX\_ROOT, CX\_STATIC\_CHECK, CX\_DYNAMIC\_CHECK and CX\_NO\_CHECK. Only use the abstract root classes when catching exceptions at the highest levels of software architecture (e.g., some global dispatcher, garbage collector, network connection, etc).
- When creating a new exception sub-classes, determine the right exception type super-class: CX\_STATIC\_CHECK, CX\_DYNAMIC\_CHECK or CX\_NO\_CHECK.
- For naming conventions, see the section: **ABAP Object Component Naming Conventions**

| 💡 | Most of the ABAP OO standards used here are reproduced from the articles "An Insider Guide to Writing Awesome ABAP Programs" Parts 1 -- 3. Authored by Andreas Blumenthal and Horst Keller. These articles appeared in the SAP Professional Journal January-March 2006. |
| :-: | :-- |

#### ABAP Object Naming Conventions

The following conventions are from the SAP Help Library for Class Builder

- When naming the development objects use glossary terms instead of dictionary terms. For example ZCL\_DOCUMENT is preferred over ZCL\_BELNR.
- Use English Names.
- Since names are not case sensitive, use the underscore as a separator in the name.
- Names should describe the action, not the implementation of the action. For example: PRINT\_FORM is preferred over FORM\_TO\_SPOOL.

##### Type Naming Convention

| **Type** | **Global Prefix** | **Local**  **Prefix** | **Rule** | **Example** |
| --- | --- | --- | --- | --- |
| Classes | ZCL\_ | LCL\_ | Use singular nouns | ZCL\_COMPANY\_CODE |
| Interfaces | ZIF\_ | LIF\_ | Use singular nouns | ZIF\_STATUS\_MANAGEMENT |
| Exception Classes | ZCX\_ | LCX\_ | Use singular nouns | ZCX\_STATUS\_EXCEPTIONS |
| Object Service Classes | ZCL\_OS\_ | LCL\_OS\_ | Use singular nouns | |
| Object Service Interfaces | ZIF\_OS\_ | LIF\_OS\_ | Use singular nouns | |
| Object Service Exception Classes | ZCX\_OS\_ | LCX\_OS\_ | Use singular nouns | |
| BAdI Classes | ZCL\_BADI\_ | n/a | Use singular nouns | |
| BAdI Interfaces | ZIF\_BADI\_ | n/a | Use singular nouns | |
| BAdI Exception Classes | ZCX\_BADI\_ | n/a | Use singular nouns | |

##### Component Naming Convention

| **Component** | **Prefix** | **Rule** | **Example** |
| --- | --- | --- | --- |
| Methods | n/a | Should begin with a verb | GET\_STATUS |
| Events | n/a | Should have the form \&lt;noun\&gt;\_\&lt;participle\&gt; | BUTTON\_PUSHED |
| Local types within a class | TY\_ | Should consist of singular nouns | TY\_TREE\_LIST |
| Variable attributes | n/a | Avoid using verbs at the beginning of the name, in order to avoid conflicts with method names. | LINE\_COUNT |
| Constant attributes | C\_ | | C\_MAX\_LINE |

##### Concrete Method Description Conventions

| **Concrete Method** | **Prefix** | **Rule** | **Example** |
| --- | --- | --- | --- |
| Attribute Access | SET\_\<attribute\_name\>, GET\_<attribute\_name\> | GET\_ or SET\_ to be used. | GET\_STATUS |
| Event handler methods | ON\_\<event\_name\> | The event being handled, must be prefixed with ON\_, followed by the event name. | ON\_BUTTON\_PUSHED |
| Methods that perform type operations | AS\_\<new type\> | Should consist of singular nouns | AS\_STRING |
| Methods that return a Boolean value | IS\_\<adjective\> | These methods do NOT return exceptions. Returned values must be ABAP_FALSE or ABAP_TRUE (or their equivalents). | IS\_OPEN, IS\_EMPTY, IS\_ACTIVE |
| Check methods | CHECK\_\<objective\> | The objective should consist of singular nouns. | CHECK\_AUTHORIZATION, CHECK\_PROCESS\_DATE |

## Customer Enhancement (User-Exits)

When creating a new user-exit, always create a new INCLUDE for your specific task, unless this is a BAdI implementation. This will keep the code separate from other developers and will allow changes for multiple projects.

Verify the version you are working on is the same as the version in production. If not work with other developer who is working on the user exit to make sure these will move to production in the proper order, without overwriting one another's code. Do not use CALL TRANSACTION, COMMIT WORK, CHECK, EXIT statements or add any new popup windows within a user exit. Any of these statements can have adverse effects. If you must call a function or transaction that will do a COMMIT WORK, call it in a SEPARATE TASK so it does not have any negative effect on the current transaction.

E.g. change is needed in MV45AFZZ, find the place you need the code and insert the following code (named according to your project) and make all changes within the include:

```ABAP
INCLUDE z_sd_mv45afzz_1234_gof.
```

| 💡 | Be certain to not use EXIT statements in this new INCLUDE, because if there are multiple INCLUDES after yours they may not get executed. |
| :-: | :-- |

### SMOD/CMOD

Transaction SMOD is used to view SAP enhancements.

Transaction CMOD is used to implement an enhancement into a project.

### BAdI

| 💡 | To see what BAdIs are called for any given transaction, go to t-code SE37 and put a break-point within the function SXV\_GET\_CLIF\_BY\_NAME. View the name field when the break-point is hit – this will give you all BAdIs which are executed. |
| :-: | :-- |

SAP has implemented numerous areas where custom development can be placed using BAdIs. When possible, the development team should attempt to implement a BAdI rather than a CMOD or basic user-exit, because BAdI enables multiple implementations.

To view BAdIs available through transaction SE18; to create a BAdI use transaction SE19.

Enhancement Implementation:  ZENH_*
BAdi – Implementation:  ZBADI_*


### VOFM

Transaction VOFM is used for Requirement and Formulas. The functional analyst should tell you the menu path and the original requirement to copy from. For example the requirement may be for a new scale-based formula, the functional specification should specify to follow the menu path: _Formulas_ --- _Scale base_.

The functional/business specification should also state what the source is.

The developer should create a new routing (by entering a 3-digit number starting with 9). When double-clicking on the new routine number, the developer will be asked for an object access code. The developer should screenshot this and send the request to their Technical Manager or Team Lead (along with the CR / RFC and description of use). After obtaining an approval, send the request to the IT DPDHL BASIS team to get the access code. Once this has been received, the developer can then copy the code from the original routine into the new routine and make required changes.

### Substitutions/Validations

### BTE

## PF-Status

PF-Status is a custom status bar defined to be used by a screen, or interactive program to enable user actions. The standard SAP format must be used. The table below defines the usage of standard function keys. These function keys do not have to be enabled but when used, must comply- with the standards defined in this section.

These function keys are **not** to be replaced by user-defined keys.

| **PF Key** | **OK-CODE** | **Description** |
| --- | --- | --- |
| 01 | HELP | Display help screen for current field |
| 02 | SELE | Select entry (same as double-click) |
| 03 | BACK | Exit current transaction. Overrides mandatory entries |
| 04 | LIST | List Possible Entries |
| 09 | MARK | Select |
| 10 | | Menu bar |
| 11 | SAVE | Save entries |
| 12 | CANCEL | Cancel current request, Mandatory entries override |
| 13 | PRINT | Print |
| 14 | DELETE | Delete |
| 15 | EXIT | Exit (Fast EDIT) leave transaction. Same as PF03 |
| 21 | TOP | First page of document |
| 22 | PAGE\_UP | Previous page |
| 23 | PAGE\_DOWN | Next page |
| 24 | BOTTOM | Last page of document |

## Dialog Programs (module pools)

### Main program

Example: SAPDZMFM

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 -- 3 | SAP-given static prefix | SAP |
| 4 | Module pool type | M -- Screen module pool<br>D -- Dialog module pool<br>U -- Update module pool<br>F -- Subroutine module pool |
| 5 | Designation | Z – Customer module pool |
| 6 | Application area | View [Functional areas](#_Functional_Areas) |
| 7 -- 8 | User defined; should be meaningful for the application | |

### Include programs

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 -- 5 | Same as positions 4 -- 8 in Module pool name | |
| 6 -- 8 | Object name | TOP – Global declarations<br>O*nn* – PBO modules (*nn* = 01 -- 99)<br>I*nn* – PAI modules (*nn* = 01 -- 99)<br>F*nn* – Forms (subroutines) (*nn* = 01 -- 99) |

### Screen painter

SAP screens are referred to as dynpros. Standard SAP components, such as transactions, menus and tables contain dynpros and the associated processing logic. A developer may also create customized dynpros.

The identification of a screen painter dynpro consists of an ABAP program name and a 4-digit dynpro number. The number range reserved for custom dynpros should be between 9000 – 9999. The initial screen number for a given transaction should be 9000 and additional screen numbers increment by 10.

Example: Module Pool SAPMZF01 Screen 9000

### Flow logic

```ABAP
***************************************************************
PROCESS BEFORE OUTPUT.

  MODULE set_status.  "Called by all screens 
 
* Enter screen specific modules here
* e.g. dynamic screen, locking, etc. 
 
*************************************************************** 
PROCESS AFTER INPUT. 
* Process fast-exit commands 
  MODULE pai_exit AT EXIT-COMMAND.  “Called by all screens 
 
* Save OK-CODE pressed 
  MODULE pai_initialize.  "Called by all screens * 
 
* Enter screen specific modules here
* E.g. field checks, loops, table controls, etc.
 
* Process user command (OK-CODE) after all checks completed. 
  MODULE user_command. 
```

### Modules

#### Process before output (PBO)

```ABAP
* INCLUDE MZxxxOO1. 
*************************************************************** 
*	Module SET_STATUS OUTPUT 
*************************************************************** 
*	<descriptive text> 
***************************************************************
MODULE set_status OUTPUT. 
  CASE sy-dynnr.  "Check screen number number.
    WHEN ‘0100’.  "Screen description of screen 100
      SET PF-STATUS ‘100MAIN’.
      SET TITLEBAR ‘100’. 
 	WHEN ‘0200’.  "Screen description for screen 200
      SET PF-STATUS ‘200MAIN’. 
      SET TITLEBAR ‘200’. 
 	WHEN OTHERS. 
      "add any subsequent screens...
 	ENDCASE. 
ENDMODULE.  “SET_STATUS OUTPUT 
```

#### Process after input (PAI)

```ABAP
 
************************************************************ 
*	Module PAI_INPUT INPUT 
************************************************************ 
*	<descriptive text> 
************************************************************
MODULE pai_input INPUT. 
  CASE sy-dynnr.  "Check screen number 
    WHEN ‘0100’.  "Screen 0100 
      SET SCREEN 0. 
      LEAVE SCREEN.
    WHEN ‘0200’.  "Screen 0200 
*	  First process any de-queuing of entries, pop-ups to confirm data loss, etc.   
      CASE sy-ucomm.  "Check which fast exit pressed 
        WHEN ‘RW’. 
          SET SCREEN 0. 
        WHEN OTHERS. 
          SET SCREEN 0100.
      ENDCASE 
      LEAVE SCREEN. 
*   Add any subsequent screens
  ENDCASE. 
 
ENDMODULE.  "PAI_INPUT INPUT

************************************************************ 
*	Module PAI_INITIALIZE INPUT 
************************************************************ 
*	<descriptive text> 
************************************************************ 
MODULE pai_initialize INPUT. 
  "Set new OK Code. 
  save_ok = ok_code.
  
  "Clear OK Code. 
  CLEAR ok_code.
  
ENDMODULE.  “PAI_INITIALIZE INPUT

************************************************************ 
*	Module USER_COMMAND INPUT 
************************************************************ 
*	<descriptive text> 
************************************************************
MODULE user_command INPUT. 
 
  CASE SY-DYNNR.  "Check Screen number 
    WHEN ‘0100’.  "Screen 100 (enter description) 
      PERFORM PROCESS_0100_OK_CODES. “(Form routine to process OK codes)
    WHEN ‘0200’.  " Screen 200 (enter description) 
      PERFORM PROCESS-0200-OK-CODES. “(Form routine to process OK codes)
*   Add any subsequent screens
 	ENDCASE. 

ENDMODULE.   	“USER_COMMAND INPUT
```

## Transaction Codes

Example: ZEWM\_INV\_RPT (EWM Inventory Report)

The maximum name length for transaction codes is 20 characters.

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer program for production<br>Y -- Test program |
| 2 -- 20 | Meaningful transaction name | Descriptive, yet concise text. Use underscores to enhance readability. |

## Data Dictionary

### Tables

| 💡 | All indexes must be approved by the SAP Technical Lead or Technical Manager before being moved to Production. |
| :-: | :-- |

Technical attributes shall be determined according to the development specification.

The maximum name size for customer tables is 16 characters.

#### Tables

Example: ZMM\_LOOKUP

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer program for production<br>Y -- Test program |
| 3 -- n | Functional Area | View Functional areas |
| n+1 | Underscore | To promote readability |
| n+2 -- 16 | Program description | Descriptive, yet concise text. Use underscores to enhance readability. |


#### Tables MDG

Example: ZMDG\_C\_BP\_RULES

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer Table|
| 2 -- 4 | Functional Area | MDG |
| 5| Underscore | To promote readability |
| 6 | Type of Table | C = Customizing Table <br>  D = Application Table <br> T = Table Type |
| 7 | Underscore | To promote readability |
| 8 -- 16 | Table description | Descriptive, yet concise text. Use underscores to enhance readability. |

### Views

The creation of a view is to be used as a third option. Please try direct table selects, then a table join before resorting to a view. If a view does need to be created, proper documentation and reasoning must be presented at the time of the code review.

Views are transparent tables consisting of an alternate view or combination of multiple tables, such as a joined table. The length of the view name will vary based on the primary table name for that view. The maximum name length of a view is 16 characters.

Example: ZMM\_V\_MATERIALS

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z – Customer program for production Y – Test program |
| 2 -- n | Functional Area | View [Functional areas](#_Functional_Areas) |
| n+1 -- n+3 | \_V\_ | V – for view |
| n+4 -- 16 | Program description | Descriptive, yet concise text. |

#### Structures

Used for common data layouts between programs.

Structure name length may be a maximum of 30.

Example: ZMM\_S\_INVENTORY

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer program for production<br>Y -- Test program |
| 2 – n | Functional Area | View [Functional areas](#_Functional_Areas) |
| n+1 – n+3 | \_S\_ | S = Structure |
| n+4 – 30 | Program description | Descriptive, yet concise text. Use underscores to enhance readability. |

#### Table Types

Used for common data arrays between programs.

Table Type name length may be a maximum of 30.

Example: ZMM\_T\_INVENTORY

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer program for production<br>Y -- Test program |
| 2 -- n | Functional Area | View [Functional areas](#_Functional_Areas) |
| n+1 -- n+3 | \_T\_ | S = Structure |
| n+4 -- 30 | Program description | Descriptive, yet concise text. Use underscores to enhance readability. |

#### Fields/Domains/Data Elements

Follow SAP naming conventions except for placing a Z in the first position.

Example: ZWERKS.

The maximum length for table fields is 30 characters.

Data Elements and Domain names are up to a maximum of 30 characters.

## Reports

### ALV

All new reports should be done in the ALV format. There should be no WRITE statements used in future user-facing programs.

Use ABAP class methods instead of the older REUSE\_ALV\_... function modules. With the exception of the REUSE\_ALV\_VARIANTXXX functions since the new class does not give this as an option. Examples of the use of these are in the template mentioned above.

## Interface

### IDocs

#### Message Types

A logical name for a packet of IDoc data being sent inbound/outbound to/from SAP R/3.

Example: ZMMFDR

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 |  Customer object for production prefix; required by SAP | Z |
| 2 -- 3 | Application area | View [Functional areas](#_Functional_Areas) |
| 4 -- 6 | Free form – meaningful acronym | |

#### Process Codes (Inbound/Outbound)

The code that identifies the process type. The process type determines which process is used to convert and IDoc to a SAP document.

Process Code names may be up to 30 characters long.

Example: ZMMF

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Customer object for production prefix; required by SAP | Z |
| 2 -- 30 | Free form – meaningful acronym | |

#### Inbound Function Modules

Function modules that convert inbound IDocs into SAP documents/data.

Example: Z\_IDOC\_INPUT\_ZMMFDR

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Customer object for production prefix; required by SAP | Z |
| 3 -- 6 | | IDOC |
| 8 -- 12 | | INPUT |
| 14 -- 19 | IDoc message type | *\<message type\>* |

#### Outbound Function Modules

Function modules that convert SAP documents/data into outbound IDocs

Example: Z\_MASTER\_IDOC\_CREATE\_ZMMFST

Another acceptable naming convention is Z\_IDOC\_OUTPUT\_*\<message type\>*.

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Customer object for production prefix; required by SAP | Z |
| 3 -- 8 | | MASTER |
| 10 -- 13 | | IDOC |
| 15 -- 20 | | CREATE |
| 22 -- 27 | IDoc message type | *\<message type\>* |

#### IDoc Types

SAP format that is used to interpret the data of a business transaction. An IDoc type consists of the following components:
- A control record – It is identical for each IDoc type
- Several data records – One data record consists of a fixed key part and a variable data part. The data part is interpreted using segments, which differ depending on the IDoc type selected.
- Several status records – They are identical for each IDoc type and describe the statuses an IDoc has already passed through or the status an IDoc has attained.

Example: Z\_MM\_FDR01

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 |  Customer object for production prefix; required by SAP | Z |
| 2 | Application area | View [Functional areas](#_Functional_Areas) |
| 3 -- 6 | Free form – meaningful acronym | |
| 7 -- 8 | Version number | |

#### Segments

Data record of an IDoc type.

##### User-Defined Segments

Example: Z1FDRH1

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 -- 2 |  Customer object for production prefix; required by SAP | Z1 |
| 3 -- 5 | Free from --- meaningful acronym | |
| 6 | - Header data<br>- Item data |
| 7 | Hierarchical level of header or item | 1 -- 9 |

##### Referencing SAP Segments

Example: Z1EDP16

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 -- 2 |  Customer object for production prefix; required by SAP | Z1 |
| 3 -- 7 | SAP Segment Name | |

### XML

### Communication Protocols

#### FTP/sFTP

#### RFC

#### HTTP/HTTPS

#### Web Service

### External Files

- Currently:
	- The SAP DB server has a directory on it for external files - /interface/. From that directory, create an appropriate sub-directory and place the files in there. For example, /interface/Adyen/IN holds external files for Adyen interface , /interface/ADC/PK/IN holds external files for the SAP to ADC interface.
	- Example: The logical file name ADYEN\_PAYMENT\_INF points to the file name Payment\_*DDMMYYY*\_SERVICE.CSV on /interface/ADYEN/IN/. The data contained in this file is used when creating payments using Cash App conversion.
- In the future:
	- External files are to be defined as logical files via the customization menu path. This allows for consistent file naming and alleviates programs from being dependent on the actual UNIX file name
	- Execute transaction code: FILE

## SAP-standard Objects

SAP **delivered objects are not to be modified.** User-exits must be used whenever possible or address modifications via the enhancement facilities provided. 

**Copying and modifying a SAP-standard object is also considered a modification to SAP-standard objects.** Only in a highly critical scenario with no alternatives can exception be given. Approval will be required from a VP-level. The copied object must then be maintained and substituted in the program path.

For ABAP programs, functions, includes, and reports, the first position is to be changed to Z. For Module Pools and Module Pool Includes, change the 5th position to a Z. The purpose of this procedure is to ensure that all SAP delivered code remains intact as well as identifying all program customizations when migrating to a new release.

In the event that the above is not possible, Include modules must be added to where the customized code must be called. The include module must indicate (in the documentation) its purpose and location where it is called from.

## Security & Authorization

### Object Class

An authorization object class groups common check objects together in a logical group. Authorization class has a length of four (4) characters and should be defined as follows:

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 |  Customer object for production prefix; required by SAP | Z |
| 2 -- 4 | Acronym for purpose | |

### Check objects (Authorization Objects)

Check objects are objects that are called from ABAP to check the login user's authority to perform a function or check access to specific field values.

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Designates type of object | Z -- Customer program for production<br>Y -- Test program |
| 3 -- N | Functional Area | View Functional areas |
| N -- XX | Program description | Descriptive, yet concise text. Use underscores to enhance readability. |

## SAPscript Forms

### SAPscript Naming Convention

Do not modify SAP-standard Forms. If imperative and approved, make a copy and modify the copy. Use the documentation object to record the original form's name and any related transactions. The Form name can be up to 16 characters.

| **Position** | **Description** | **Sample values** |
| --- | --- | --- |
| 1 | Customer object for production indicator | **Z** |
| | Use DHL if Form is a DPDHL standard. Use company code if form is customized specifically for country. | **CA13 -- BRAMPTON, CA**<br>**NL13 -- DHL EXPRESS NL**<br>**DHL – If global (not country-specific)** |
| | Separator | **\_** |
| | Remaining space up to 16. See exceptions below. | Descriptive text |
| | Exceptions to Descriptive Text:<br>Special naming conventions for these form types:<br>- 3-character code<br>- Output Indicator<br>- If Form is a Vidifax, add 'X' to end of name. | **POR** -- Purchase Order ZCA13\_POR (Canada custom PO)<br>**RFQ** -- RFQ ZUS13\_RFQ (Americas custom RFQ)<br>**SAC** -- Schedule Agreement and Contract ZNL13\_SAC<br>**X (Vidifax)** -- ZCA13\_PORX (Canada custom Vidifax) |

### SAPscript Development Class

Forms must be assigned to a defined SAPscript development class.

| **Position** | **Description** | **Sample values** |
| --- | --- | --- |
| 1 | Customer object for production indicator | Z |
| 2 | Underscore | \_ |
| 3 -- 4 | 2-character functional assignment | e.g. SD, MM, FIN |
| 5 | Underscore | \_ |
| 6 -- 11 | OUTPUT | OUTPUT |

<br>

| 💡 | If the development class does not exist, it must be created and documented. |
| :-: | :-- |

### Basic Settings Tab

The following settings should be used unless otherwise specified. From t-code SE71, follow menu path *Utilities --- Change Page format*, to change page format, e.g. from DINA4 to Letter.

| **Page format** | **All other settings** |
| --- | --- |
| INCH11 | Use original form setting unless otherwise necessary. |

### Layout Windows

Windows should have descriptive names:
- MAIN – Contains line item information
- *XXXX*ADDR – Address (*XXXX* – 4-character description of address)
- *XXX*TOTAL – Total (*XXX* – 3-character description of total, e.g. QTY, INV, etc.)
- HEADER – Header information
- FOOTER – Footer information
- LOGO – DPDHL logo

### Paragraph Format

| Component | Format |
| --- | --- |
| Paragraph name | Length 2. First character must be a letter. |
| Paragraph | The default paragraph should be named **AS**. Define the settings for the default paragraph. |
| Standard attributes | Use default settings unless necessary. |

<br>

| 💡 | Be cautious when changing a SAP-defined paragraph, a change can have odd results if it's used in other parts of the form. It's better to make a copy in most cases. |
| :-: | :-- |

### Character Format

| Component | Format |
| --- | --- |
| Character format name | Character length 2 |
| Standard attributes | Use default SAP setting unless required by layout form. |

### Text Elements

Names should be descriptive. Spaces are not allowed per SAP.

## Adobe Forms

### Naming Conventions

#### Interface

##### Interface Name

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 -- 3 | Always contains this static value -\&gt; | ZIF |
| 4 | Underscore | To promote readability |
| 5 -- n | Functional Area | _View Functional areas_ |
| n+1 | Underscore | To promote readability |
| n+2 -- 30 | Interface description OR SAP Interface name | Descriptive, yet concise text. Use underscores to enhance readability. Note: When the interface is a copy of a standard SAP interface, the description should be the SAP interface name |

#### Interface Elements

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 -- n | Prefix | **Form Interface**<br>I -- Import<br>E -- Export<br>EX -- Exceptions<br><br>**Global Definitions**<br>_(Global Data)_<br>GV -- Variable<br>GS -- Structure<br>GT -- Table<br><br>TY -- Types<br>\<fs\> -- Field Symbols<br><br>**Initialization**<br>IO -- Code Initialization |
| n+1 | Underscore | To promote readability |
| n+2 | Element description | Descriptive, yet concise text. Use underscores to enhance readability. |

#### Forms

Naming an Adobe form will depend on whether the form has been copied from a standard SAP form template or created entirely as custom. If the form has been copied from a standard SAP template, the original name of the form needs to be included in the form name. This creates a logical link between the two and also simplifies the process of locating existing versions of the template.

E.g. _/NFM/MEDRUCK\_PO as_ _ **Z\_MM\_MEDRUCK\_PO** _

##### Print Forms

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Prefix | Z -- Customer program for production<br>Y -- Test program |
| 2 | Underscore; to promote readability | _ |
| 3 -- n | Functional Area | [_View Functional areas_](#_Functional_Areas) |
| n+1 | Underscore; to promote readability | _ |
| n+2 -- 30 | Form description | Descriptive, yet concise text. Use underscores to enhance readability<br>Note: _When the form is a copy of a standard SAP form, the description should be the SAP form name_ |

##### Interactive Forms

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 | Prefix | Z -- Customer program for production<br>Y -- Test program |
| 2 | Underscore; to promote readability | _ |
| 3 to n | Mode | **ONLI** for Online form **OFFL** for Offline form |
| n+1 | Underscore | To promote readability |
| n+2 to 30 | Form description | Descriptive, yet concise text. Use underscores to enhance readability<br>Note: _When the form is a copy of a standard SAP form, the description should be the SAP form name_ |

#### Objects & Components

##### Context Objects

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 -- 3 | Prefix | GRA -- Graphic Node<br>ADR -- Address Node<br>TXT -- Text node<br>FOL -- Folder<br>DTA -- Data<br>STR -- Structure<br>LOP -- Loop |
| 4 | Underscore; to promote readability | _ |
| 5 -- n | Description | Descriptive, yet concise text. Use underscores to enhance readability |

##### Standard Objects

| **Position** | **Description** | **Value** |
| --- | --- | --- |
| 1 -- 3 | Prefix | CNT  -- Content area<br>IMG -- Image<br>IMF -- Image field<br>SUB -- Subform<br>NUM -- Numeric<br>TBL -- Table<br>IOF -- Input/output field<br>STX -- Static text<br>TXT -- Text<br>DEC -- Decimal<br>DAT -- Date<br>TIM -- Time<br>LIB -- List box<br>DDL -- Drop down list<br>CIR -- Circle<br>REC -- Rectangle<br>LIN -- Line<br>BAR -- Barcode |
| 4 | Underscore; to promote readability | _ |
| 5 -- n | Description | Descriptive, yet concise text. Use underscores to enhance readability |

**Form Builder Components**

| **Position** | **Description** | **Value** |
| --- | --- | --- | 
| 1 -- 5 | Prefix | FBLFD -- Localized full date<br>FBLLD -- Localized long date<br>FBLMD -- Localized medium date<br>FBLSD -- Localized short date<br>FBTBC -- Table Calcs |
| 6 | Underscore; to promote readability | _ |
| 7 -- n | Description | Descriptive, yet concise text. Use underscores to enhance readability |

**Web Dynpro Components**

| **Position** | **Description** | **Value** |
| --- | --- | --- | 
| 1 -- n | Prefix | WDS -- Submit<br>WDCHKF -- Check Fields<br>WDEDL -- Enumerated drop-down list<br>WDREV -- Review Copy<br>WDXS -- ActiveX Submit<br>WDXCHKF -- ActiveX Check Fields<br>WDXEDL -- ActiveX Enumerated drop-down list<br>WDXREV -- ActiveX Review Copy<br>WDNS -- Native submit<br>WDNCHKF -- Native Check fields<br>WDNEDL -- Native Enumerated drop-down list |
| n+1 | Underscore; to promote readability | _ |
| n+2 | Description | Descriptive, yet concise text. Use underscores to enhance readability |

# Appendices

## [Functional Areas](#_Functional_Areas)

Abbreviations:
- SD – Sales/distribution 
	- BD – Basic Data 
	- BI – Billing  
	- CS – Sales Support 
	- CX – Common across SD
	- ED – EDI 
	- IS – Sales Info system 
	- SH – Shipping 
	- SL – Sales 
	- SO – Sales Order Management 
	- TR - Transportation 
- MM – Material management
	-  BD – Basic Data 
	- CM – Common across MM
	- ED – EDI 
	- IM – Inventory Management 
	- PI – Purchasing Info System 
	- IV – Invoice Verification 
	- PR – Purchasing 
	- WM – Warehouse Management
	- CB – Consumption-Based Planning 
- PP – Production planning 
	- PD – Plant Data Collection 
	- AO – Assembly Orders 
	- PC – Planning & Control  
	- PO – Production Orders 
	- MD – Material Requirements Planning 
	- CP – Capacity Planning 
	- MP – Master Planning 
	- SP – Sales & Operations Planning 
- FI – Financial 
	- AP – Accounts Payable 
	- AR – Accounts Receivable 
	- CF – Common across FI
	- BL – Electronic Banking
	- ED - EDI 
	- FC – Financial Controlling 
	- FM – Funds Management 
	- GL – General Ledger 
	- LC – Consolidation 
- SM – Service Management 
- QM – Quality Management 
	- QP – Quality Planning 
	- QI – Quality Inspection 
	- QC – Quality Control 
	- CA – Quality Certificates 
	- QN – Quality Notifications 
- PM – Project Management 
- HR – Human Resources 
	- BD – Basic Data 
	- CC – Common across HR 
	- PA – Personal Administration 
	- TI – Time Management 
	- IW – Incentive Wages 
	- BN – Benefits
	- PY - Payroll 
- CA – Cross-Application Components 
- AU – ABAP Development Utilities 

## Local Data Definition Naming Conventions

### Classical Programming & Class Attributes

| **Data type** | **Global Prefix** | **Local Prefix** |
| --- | --- | --- |
| Type | ty\_ | lty\_ |
| Table | gt\_ | lt\_ |
| Structure | gs\_ | ls\_ |
| Variable | gv\_ | lv\_ |
| Constant | gc\_ | lc |
| Object | go\_ | lo\_ |

### ABAP OO Programming

| **Parameter Type** | **Data Type** | **Prefix** | **Rule** | **Example** |
| --- | --- | --- | --- | --- |
| **Importing** | single fields, generics, numeric | i\_ | singular nouns | i\_document\_number |
| **Importing** | objects | io\_ | singular nouns | io\_grid |
| **Importing** | structures | is\_ | singular nouns | is\_bapiret2 |
| **Importing** | table types | it\_ | singular nouns | it\_bapiret2\_tab |
| **Exporting** | single fields, generics, numeric | e\_ | singular nouns | e\_document\_number |
| **Exporting** | objects | eo\_ | singular nouns | eo\_grid |
| **Exporting** | structures | es\_ | singular nouns | es\_bapiret2 |
| **Exporting** | table types | et\_ | singular nouns | et\_bapiret2\_tab |
| **Changing** | single fields, generics, numeric | c\_ | singular nouns | c\_document\_number |
| **Changing** | objects | co\_ | singular nouns | co\_grid |
| **Changing** | structures | cs\_ | singular nouns | cs\_bapiret2 |
| **Changing** | table types | ct\_ | singular nouns | ct\_bapiret2\_tab |
| **Returning** | single fields | r\_ | singular nouns | r\_document\_number |
| **Returning** | objects | ro\_ | singular nouns | ro\_grid |
| **Returning** | structures | rs\_ | singular nouns | rs\_bapiret2 |
| **Returning** | table types | rt\_ | singular nouns | rt\_bapiret2\_tab |
