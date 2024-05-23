
# Coding Standards

## About This Document

### Introduction

The purpose of this reference document is to define programming standards and development practices that will be utilized as the development standards for all ERP development. By following these standards, the ongoing maintenance of custom development objects will be a highly manageable entity.

### Document Conventions

These are the symbols and word conventions used to help the user.

| **Convention** | **Explanation** |
| :-: | --- |
| 💡 | Tip boxes identify shortcuts or important information that may make processing easier. |

## Authorization Checks

Authorization objects should be checked at the selection screen of report programs or at the transaction code level. All check objects created must be documented thoroughly and given to the security administrator.

When analyzing what authorization profile to utilize, it is best to assign the same security profile that is assigned to the user that will be using the program.

## Batch Data Communications (BDC)

BDCs are very performance intensive and all measures should be taken to NOT create BDCs unless absolutely necessary. First check for an existing BAPI and if it does not exist, check for new BAPIs on OSS (possible to add them to our current system).

Batch data input sessions should be limited to approx. 250 @actions per BDC group. This number can vary greatly depending on the quality of the data being processed. Large batch ABAP programs are to be avoided due to the single-threaded nature of the batch environment. These programs are to be broken up into smaller batch programs, where appropriate.

Utilize transaction code SHDB to record an R/3 transaction and generate a program skeleton that contains all screen and field information in the required BDC format. Replace the field entries you wish to convert as variables and add the necessary file handling logic to the generated program.

Eliminate duplicate reads of master date (use of a select single within a loop). Populate an internal table with master data and only update this internal table with a new record from the database when needed.

## SAP-Standard Functional Data Manipulation

SAP-standard functional tables, e.g. BKPF, VBAP, are only to be updated through SAP provided code. This can be done through the use of BAPIs and existing SAP function modules (if released), and as a last resort through a BDC.

Direct updates to SAP-standard functional tables are **forbidden**. If there is a requirement to modify an existing custom program and you see direct updates to SAP tables, bring this to your supervisor's attention immediately.

## External File Usage

Structures for external files are to be defined in the data dictionary using fields, data elements, domains and where appropriate, check tables. They can then be referenced in ABAP programs when declaring internal tables. An example of an external file structure is ZMPO1. This is a structure used to hold extract data for legacy Purchase Orders that will be loaded into an SAP system.

## Subroutines (FORMs)

Use FORM paragraphs whenever possible to improve readability and logic flow. If a block of code is executed more than once, it should be placed in a subroutine at the bottom of the code.

FORM paragraphs should be placed in an INCLUDE ending with *\_Fxx*, can create additional INCLUDES if needed.

Use local variables whenever possible within subroutines for modularization purposes. However, frequently used subroutines within one program call should use global variables to eliminate creation time of those local variables.

Use of the 'CHANGING' clause is up to programmer preference but is good for subroutine documentation.

Never use TABLES parameter as it is not Unicode-compliant. The table should already be global. If it is not, then pass with a USING parameter and not a TABLES parameter and type the parameter as a table type.

Subroutines that are to be called by multiple programs should be created as a class method or a function module.

## Coding Standards

### SELECT ... ENDSELECT

Never use SELECT ... ENDSELECT, unless doing a single select without the complete table key in the WHERE clause. Other than this case, SELECT ... ENDSELECT must not be used. SELECT ... INTO should be used instead.

ABAP with in-line declaration:
```ABAP
SELECT matnr, werks, mmsta 
  INTO DATA(@ls_marc)
  FROM marc 
  UP TO 1 ROWS 
  WHERE werks IN s_werks.
ENDSELECT. 
```

Do not have multiple SELECT statements to the same table unless necessary – if there is logic that change the way you need to SELECT the data, then create your WHERE clause based on this logic; however only have one SELECT statement using the different WHERE clause. This will allow easier maintenance in future changes to the program.

### JOIN

Use efficient joins – typically no more than 2 tables joined together at one time unless they are small tables. Use fully qualified joins only.

When using JOIN, use the table name for each reference or use a meaningful alias.

E.g. ABAP with in-line declaration:
```ABAP
SELECT mseg~matnr, mkpf~mblnr, mkpf~mjahr, mseg~zeile,
       mseg~werks, mseg~meins, mseg~menge, mkpf~budat,
       mseg~bwart, mseg~shkzg
  INTO TABLE @DATA(lt_material_docs)
  FROM mkpf
  INNER JOIN mseg 
  ON  mseg~mblnr EQ mkpf~mblnr
  AND mseg~mjahr EQ mkpf~mjahr
  WHERE mkpf~budat IN @s_budat
    AND mseg~matnr IN @s_matnr
    AND mseg~bwart IN @s_bwart
    AND mseg~werks IN @s_werks.
```

### FOR ALL ENTRIES

Verify the table used in FOR ALL ENTRIES is filled with at least 1 entry prior to using it – otherwise if it is INITIAL the statement will result in a full table scan, i.e. disregarding the WHERE clause.

Be aware that the result will be DISTINCT.

### LOOP AT

When LOOPing AT data, prefer to use the ASSIGNING *\<field\_symbol\>* clause instead of INTO. This will create a pointer to the current row, rather than copying data. You can make changes to the data directly through the field-symbol and no MODIFY is needed. Therefore, processing using a field symbol is faster. E.g.:

ABAP with in-line declaration:
```ABAP
LOOP AT lt_marc ASSIGNING FIELD-SYMBOL(<marc>).
  <marc>-mmsta = abap_true.
ENDLOOP. 
```

Do not do a LOOP AT an internal table and then have conditional IF or CHECK which could be done within the WHERE clause of the LOOP – this approach will be much faster.

### READ TABLE

When using a READ TABLE statement to see if one record exists and you will not be using any of the data within the table, use TRANSPORTING NO FIELDS addition instead of ASSIGNING to a field-symbol.

Classical ABAP:
```ABAP
READ TABLE lt_marc TRANSPORTING NO FIELDS
  WITH TABLE KEY matnr = ls_material_doc-matnr
                 werks = ls_material_doc-werks.

IF sy-subrc EQ 0. 
* execute code if record found 
ELSE. 
* execute code if record not found
ENDIF. 
```

New ABAP with in-line declaration:
```ABAP
IF line_exists( lt_marc[ 
  matnr = ls_material_doc-matnr
  werks = ls_material_doc-werks ] ).
* execute code if record found 
ELSE.
* execute code if record not found 
ENDIF.
```

If you need values from within the table, ASSIGN them to a field-symbol and access the data through the field-symbol.

Classical ABAP with in-line declaration:
```ABAP
READ TABLE lt_marc ASSIGNING FIELD-SYMBOL(<marc>)
  WITH TABLE KEY matnr = ls_material_doc-matnr
                 werks = ls_material_doc-werks.

IF sy-subrc EQ 0. 
* execute code if record found 
ELSE. 
* execute code if record not found
ENDIF. 
```

New ABAP with in-line declaration:
```ABAP
ASSIGN lt_marc[
  matnr = ls_material_doc-matnr
  werks = ls_material_doc-werks ]
  TO FIELD-SYMBOL(<marc>).

IF sy-subrc EQ 0. 
* execute code if record found 
ELSE. 
* execute code if record not found
ENDIF. 
```

| 💡 | Check the field-symbol is assigned prior attempting to read from it --- at runtime, certain cases may cause the field-symbol to be unassigned and the code will short-dump. |
| :-: | :-- |

### Boolean conditions: IF ... ENDIF, AND, OR

Make sure the first IF statements are for the most frequently true case for better performance.

For a logical AND statement in an IF construct, put the most likely FALSE case first.

For a logical OR statement, put the most likely TRUE statement first.

### MOVE-CORRESPONDING

This statement, along with other corresponding statements, should be used only if there is no more efficient ways, e.g. MOVE, etc.

If moving a data structure to an equivalent structure, move the entire data structure using an assignment, e.g. \<lfa1_ds\> = \<lfa1\>.

### READ TABLE

When reading a table using the 'WITH KEY' option, ensure that the key is filled from the left most field in the table.

When reading a sorted table, a hashed table, or a table with a secondary key defined, use WITH TABLE KEY.

### COLLECT

The COLLECT statement could lead to being CPU-intensive. Consider using cache variables. E.g.:

Classical ABAP with in-line declaration:
```ABAP
READ TABLE gt_lips_total ASSIGNING FIELD-SYMBOL(<lips_total>)
  WITH TABLE KEY vbeln = gv_vbeln
	             vbelp = gv_vbelp.

IF sy-subrc EQ 0. 
* record found, update it
  <lips_total>-menge = <lips_total>-menge + gv_menge.
ELSE. 
* does not yet exist in new table, add it
  INSERT VALUE #( 
    vbeln = gv_vbeln
    vbelp = gv_vbelp
    menge = gv_menge )
    INTO TABLE gt_lips_total. 
ENDIF.
```

## Code Analysis/Performance Check

The Extended Syntax Check, t-code SLIN will identify 'dead code' (code which due to modifications, may no longer be called in a program) and other inefficient code. Whenever, you go for Extended Syntax Check, include 'character strings' option along with all other options.

### ABAP Runtime Analysis Tool
Accessible via:
- T-code SAT (older version: SE30), or via menu path *System --- Utilities --- Runtime analysis --- Execute*
- T-code ST12 (single transaction analysis)

After limiting the analysis to a specific object (a transaction, a program, a function module, etc.), execute the object. The result contains information such as number of database accesses, runtime, etc., which a further analysis can be performed on to identify potential issues that need to be addressed, or potential optimization points.

### SQL Performance Trace Tool
Use the SQL trace to analyze if there are problems with database accesses. You have the option of performing a SQL trace, a Buffer trace, an Enqueue trace, or an RFC trace. This is accessible via t-code ST05, or use ST12 for an integrated tracing.

## Maintenance of Existing Production Code

When changing production code, the "Change History" comment block must be updated in the main program.

For global classes, update the "Change History" in the CONSTRUCTOR method.

Make the code Unicode compliant. If this is an urgent matter and there are hundreds of Unicode compliant issues, let development management know.

Do not comment out code and move commented code into subsequent systems. Delete code that are no longer used --- Version Management will allow you to get back to it if needed.

## SAPscript Forms

When customizing SAPscript forms for the organization, follow these standards. Properties (language key, default text format, etc.) that are not mentioned in this document should use the SAP defaults.

| 💡 | SAPscript Forms are client-dependent objects, create them in the Configuration client, e.g. DEV client 000, and transport them to the other DEV clients for unit testing.<br>SAPscript Form design and configuration should be in, e.g. DEV client 000, but the subroutines which can be called from inside the form should be done in e.g. DEV client 300 (as it's an ABAP report).<br>If you write your own SAPscript program (i.e. Print Program), you should do it in, e.g. DEV client 300, too, as it's an ABAP report. |
| :-: | :-- |

| 💡 | The user settings determine how information is presented to the user. Although the same standards apply whichever settings are chosen, this document contains screens based on the settings shown below. From the initial SAPscript screen (t-code SE71), follow *Settings --- Form Painter*. |
| :-: | :-- |

### General SAPscript Programming Guidelines Documentation

Use the Documentation sub-object to record helpful information. Items that should be noted include the name of the original SAP standard form you copied, output configuration transcodes, and any information on how to create and display form test data.

### Windows

| Window Name/Type | Description |
| --- | --- |
| MAIN | Used for the MAIN window only. |
| VAR | Window with variable contents. The text can vary on each page in which the window is positioned. Variable windows are formatted for each page. |
| CONST<br>\* Only seen when not using the Graphical Painter | Do not use this type. As of release 4.0 it behaves the same as VAR. Using type VAR will avoid confusion. |

### Page Names and Counter attributes

Most forms will need only these three pages:
- FIRST – Initial page
- NEXT – Subsequent Pages
- LAST – Last Page (if required)

When additional pages are required, use descriptive names. The Page numbering type should be Arabic unless requested otherwise.

#### Control Commands -- Address

When possible, use the following when formatting the address:
/: **ADDRESS** [DELIVERY] [TYPE t] [PARAGRAPH a] [PRIORITY p] [LINES l]
/: **TITLE** title
/: **NAME** name1[,name2[,name3[,name4]]]
/: **PERSON** name of natural person [TITLE form of address]
/: **PERSONNUMBER** number of the person
/: **DEPARTMENT** department
/: **STREET** street name HOUSE house number
/: **LOCATION** additional location information
/: **POBOX** po box [CODE post code / zip code] [CITY city]
/: **POSTCODE** post code / zip\_code
/: **CITY** city1[,city2]
/: **NO\_UPPERCASE\_FOR\_CITY**
/: **REGION** county / state
/: **COUNTRY** recipient country [LANGUAGE language code]
/: **COUNTRY\_IN\_REC\_LANG**
/: **LANG\_FOR\_COUNTRY** language key
/: **FROMCOUNTRY** sender country
/: **ADDRESSNUMBER** address number
/: **ENDADDRESS**

#### Fonts

When possible, use the same font type throughout the form. Also, use the same font sizes for the main body of the layout. Use **bold** characters to highlight text.

#### Window coordinates

In specifying window coordinates and sizes, use **whole numbers** as much as possible.

#### Printer

In order to make sure that the layout set functions properly, the configuration of the output device should be checked. Confirm that the print control device defined for the printer is capable of printing the text formats (font type, font size, character pitch) and paper size. For **double-sided** printouts, the printer definition should be checked.

#### Advanced modifications

Do not modify the Print Program unless other options have been ruled out. This is due to the complexity of the maintenance issues. Most modifications can be met with SAPscript Form commands. In SAPscript, you can use the PERFORM command in a window to carry out advanced functions like:
- Obtain data from the database that is needed at print time.
- Access tables not declared in the print program.
- Carry out complex ABAP calculations.
- Format data.

Do not confuse the SAPscript PERFORM command with the ABAP PERFORM statement. A special table structure is used with the SAPscript PERFORM command.

## Adobe Forms

### PDF Scenarios

#### Print Forms

Print forms are PDF forms that are used for output purposes only. No user interaction is possible.

#### Online Scenario

Online scenarios feature the following characteristics. access to an SAP system is required and possible, as well as integration with other user-interface technology (such as a web browser).

### Print program

In order to call PDF form either *Print form* or *Offline interactive form*, the program should call the following sequence of function modules:
- FP\_JOB\_OPEN
- FP\_FUNCTION\_MODULE\_NAME (form name is not hard-coded)
- CALL FUNCTION \'\<name\_returned\_from\_ FP\_FUNCTION\_MODULE\_NAME\>\'
- FP\_JOB\_CLOSE

<br>

| 💡 | To avoid hard coding form name in program to get its function module name, while defining output type in NACE, remember to add form name in "PDF/Smartform Form" field and "Form Type" as PDF. At runtime, NAST structure field SFORM will contain form name. |
| :-: | :-- |

Unless a specific output device is required, the default printer should be set to 'PDF1'. This will resolve to the default PDF printer for any user.

| 💡 | For print program syntax, follow ABAP coding standards as laid out in the ABAP sections of this document. |
| :-: | :-- |

### Configurable Objects

#### Standard Texts

Texts should not be placed directly onto a form but created as Standard Texts in transaction SO10, and defined in the form context in ADLC.

#### Image Library

Form graphics (such as logo, signatures, etc.) should be created in the SAP Form Graphics library (t-code SE78) and bound to a graphics node in the form context in ADLC.

#### Address Nodes

Address lines should not be passed directly to a form unless a specific format unsupported by ADLC is required. Address nodes should be created in the form context in ADLC and bound to an address number for automatic formatting.

### Scripting

For Adobe print forms scripting should be kept to a minimum. FormCalc should always be used over JavaScript.

| 💡 | Use of JavaScript may degrade performance, hence FormCalc is the preferred scripting language. |
| :-: | :-- |

### Master Pages

Master pages should be used sparingly and should only contain template objects that should appear on multiple pages. In the case of a single page form, only objects that would logically appear on multiple pages should be placed on the master page.

E.g. _Page numbers, logo, headers & footers, layout guides_

For a complex requirement, it is acceptable to use multiple master pages in a single form, however this does not exempt the form from following the master page usage guidelines listed here.

### Documentation

Form documentation should be maintained. To maintain form documentation, navigate to menu path *Goto --- Documentation --- Change* from the Form Builder.

Form history (flowerbox) should be maintained at the bottom of the Form Documentation. To paste in the template while editing Class Documentation, navigate to menu path *Insert --- Text --- Documentation*. On the pop-up screen, enter "Z\_MAINTENANCE\_LOG" for the text name and select the option "Expand Immediately". Once the template is pasted into your class documentation, modify as desired.

| 💡 | Form history (flowerbox) should be maintained at the bottom of the Form Documentation using text name *Z\_MAINTENANCE\_LOG* |
| :-: | :-- |

# Procedures

## Technical Specification Design Review

For new objects, the specification must be approved by the Technical Design Review team. Send a link to the specification to the e-mail recipient: SHAPE+ DEVELOPMENT DESIGN REVIEW.

## Code Review

If not using the transport workflow tool, send the code review to the mailbox SAP

Transport Approvals SHAPE+ TRANSPORT APPROVAL. Include a link to the specification, Extended Standards Check and the as Code Inspector tests. If applicable, also include an SQL trace and a runtime analysis.

If using the transport workflow tool, make sure to perform the Extended Standards Check and Code Inspector check. Justify any violations in the code via comments.
