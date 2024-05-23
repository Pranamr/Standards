# Security Guidelines
The following are guidelines related to security in the ABAP space.

## ABAP Coding

### SQL Injection

The following is adopted from a SAP Blog post titled "How to Protect Your ABAP Code Against SQL Injection Attacks" on 20 November 2013.
Reference: https://blogs.sap.com/2013/11/20/how-to-protect-your-abap-code-against-sql-injection-attacks/

SQL injection is one of the most common code injection attack methods, in which malicious SQL statements are inserted to execute unauthorized SQL statements in the database, e.g. read data or modify data in the database. They can be inserted as input by the user through the user interface or by a program through the parameter interface from outside.

Two SQL injection variants can be found in ABAP. First is direct usage of user input to create or modify ABAP Open SQL statements to access or modify data in the database (dynamic Open SQL Injection). The other option is when using native SQL commends constructed from user input, for example when using the ABAP Database Connectivity API (ADBC) to execute database statements.

The following is an example:
```ABAP
REPORT Z_SQL_INJECTION_OPENSQL.
PARAMETERS: street  TYPE zemployees–street    LOWER CASE,
            zipcode TYPE zemployees–zipcode   LOWER CASE,
            city    TYPE zemployees–city      LOWER CASE,
            phone   TYPE zemployees–phone_ext.
DATA: set_expr TYPE string,
      user     TYPE xubname.
IF street IS NOT INITIAL.
  set_expr = set_expr && ` STREET    = ‘` && street && `’`.
ENDIF.
IF zipcode IS NOT INITIAL.
  set_expr = set_expr && ` ZIPCODE   = ‘` && zipcode && `’`.
ENDIF.
IF city IS NOT INITIAL.
  set_expr = set_expr && ` CITY      = ‘` && city && `’`.
ENDIF.
IF phone IS NOT INITIAL.
  set_expr = set_expr && ` PHONE_EXT = `  && phone.
ENDIF.
IF set_expr IS NOT INITIAL.
  user = cl_abap_syst=>get_user_name( ).
  UPDATE zemployees
     SET (set_expr)
   WHERE userid = user.
  IF sy–subrc = 0.
    MESSAGE ‘Your address was changed.’ TYPE ‘I’.
  ELSE.
    MESSAGE ‘Error when trying to update your address!’ TYPE ‘E’.
  ENDIF.ELSE.
  MESSAGE ‘No data given => No Update!’ TYPE ‘I’.
ENDIF. 
```

This report uses four input parameters: street, ZIP code, city and phone number, from SAP GUI to update the user’s address in the table ZEMPLOYEES. 

After entering the new address information, the user executes the report. The Open SQL statement which is transferred to the database looks like this:
```ABAP
UPDATE zemployees SET STREET = ‘xyz’ ZIPCODE = ‘1234’ CITY = ‘xyz’ PHONE_EXT = 123 WHERE userid = 'TEST'.
```
As this is a valid Open SQL statement, the report runs successfully. Now the user’s address is updated.

#### Where the problem lies
Assume the table `ZEMPLOYEES` has a `SALARY` field. A malicious user might want to update their address as such:
- STREET: ``xyz' SALARY = '3000``
- ZIPCODE: ``1234``
- CITY: ``xyz``
- PHONE: ``123``

After the input is given to the report, the Open SQL statement is generated as follows:
```ABAP
UPDATE zemployees SET STREET = ‘xyz’ SALARY = ‘3000’ ZIPCODE = ‘1234’ CITY = ‘xyz’ PHONE_EXT = 123 WHERE userid = 'TEST'.
```

This is also a valid Open SQL statement. The report would run successfully with this input. After execution of the report, not only the user’s address is updated, but also the salary of the user. The user would have updated his salary.

The problem is that this report uses a user’s input as a dynamic SQL change expression (``UPDATE zemployees SET (set_expr) WHERE userid = user``) without sanitizing or encoding the input provided by the user. As a result, the user can enter his or her own code (``xyz‘ SALARY = ‘3000``) which modifies the ABAP Open SQL statement and thus enables the user to manipulate data he or she should not be able to.

This is one potential Open SQL injection attack vector.

#### SQL Injection in ABAP
Most ABAP programs use Open SQL statements to access the central database. The Open SQL syntax allows you to specify every clause of an Open SQL statement as one data object in parentheses to set its value dynamically. As in the example shown above, `set_expr` is used in a SET clause in the UPDATE statement. If such a value is set from outside as part of Open SQL statements without sufficient escaping, it creates a potential SQL injection vulnerability in your ABAP code.

The following manipulation attacks of dynamic Open SQL are possible:
- Manipulation of the dynamic WHERE condition
- Manipulation of the SET clause in the statement UPDATE
- Manipulation of a dynamic WHERE condition using the parameter I_FILTER of the object services method CREATE_QUERY

The following attacks on unauthorized data accesses of dynamic Open SQL are possible:
- Illegal read access to a database table with a SELECT statement
- Illegal read access to table columns
- Illegal use of columns in a dynamic GROUP BY clause
- Illegal use of columns in a dynamic HAVING clause

#### SQL injection countermeasures for Open SQL
The entire problem occurs because the dynamic SQL statement generation uses input from an insecure source, for example from outside the program. Therefore all ABAP developers should be aware of this risk and try to secure their code by strict validation or encoding any input for the Open SQL statement generation.  Use the following two rules against Open SQL injection attacks in your ABAP code:
- Use static Open SQL statements where possible.
Check whether it is really necessary to use dynamic Open SQL or dynamic SQL statements in ADBC (ABAP Database Connectivity API). If not, switching to static SQL statements will eliminate the risk of an SQL injection.
- Always validate or encode input for dynamic statements.
If dynamic SQL statements must be used, then use class CL_ABAP_DYN_PRG to implement input checks and escape the input for the dynamic clauses.

Now let’s fix the vulnerability of the program above using the static method `QUOTE` of the class `CL_ABAP_DYN_PRG`. The method puts single quotes around the input value and escapes single quotes to prevent unwanted Open SQL statements. The phone parameter in the report has type integer and does not need to be escaped. 

```ABAP
REPORT Z_SQL_INJECTION_OPENSQL.
PARAMETERS: street  TYPE zemployees–street    LOWER CASE,
            zipcode TYPE zemployees–zipcode   LOWER CASE,
            city    TYPE zemployees–city      LOWER CASE,
            phone   TYPE zemployees–phone_ext.DATA: set_expr TYPE string,
      user     TYPE xubname.
IF street IS NOT INITIAL.
  set_expr = set_expr && ` STREET    = ` && cl_abap_dyn_prg=>quote( street ).
ENDIF.
IF zipcode IS NOT INITIAL.
  set_expr = set_expr && ` ZIPCODE   = ` && cl_abap_dyn_prg=>quote( zipcode ).
ENDIF.
IF city IS NOT INITIAL.
  set_expr = set_expr && ` CITY      = ` && cl_abap_dyn_prg=>quote( city ).
ENDIF.
IF phone IS NOT INITIAL.
  set_expr = set_expr && ` PHONE_EXT = ` && phone.
ENDIF.
IF set_expr IS NOT INITIAL.
  user = cl_abap_syst=>get_user_name( ).
  UPDATE zemployees
     SET (set_expr)
   WHERE userid = user.
  IF sy–subrc = 0.
    MESSAGE ‘Your address was changed.’ TYPE ‘I’.
  ELSE.
    MESSAGE ‘Error when trying to update your address!’ TYPE ‘E’.
  ENDIF.ELSE.
  MESSAGE ‘No data given => No Update!’ TYPE ‘I’.
ENDIF.
```

If the user puts `xyz’ SALARY = ‘3000` as a new address parameter, the SQL statement which is transferred to the database is as follows:
```ABAP
UPDATE employees SET STREET  = ‘xyz” salary = ”3000’ ZIPCODE = ‘1234’ CITY    = ‘xyz’  WHERE userid = TEST.
```

This is a valid SQL statement and will execute successfully. But the input `xyz' SALARY = '3000` is encoded and transferred as one complete string `'xyz'' SALARY = ''3000'` for the STREET parameter and put into the SET clause of the ABAP SQL statement.  The ABAP compiler recognizes salary as part of the input for STREET, not as a separate column any more. Therefore a user would be unable to manipulate the column `SALARY`. As a result, the code is secured against SQL injection attacks.

The class `CL_ABAP_DYN_PRG` offers not only a quote() method, but also many other methods to check and validate the contents of variables. Some of the methods are as follows.
- check_column_name() checks whether the input is a valid column name.
- check_table_name_str() checks whether the input is a valid database table name in a specific package.
- check_whitelist_str() checks the input against  a given whitelist.

More information about the class CL_ABAP_DYN_PRG can be found in the class documentation.

## Code Vulnerability Analyzer (CVA)

The following is adopted from the following:
- SAP Blog post titled "Code Vulnerability Analyzer Checks" by Peter Barker on 19 January 2017
Reference: https://blogs.sap.com/2017/01/19/code-vulnerability-analyzer-checks/
- SAP Blog post titled "SAP Code Vulnerability Analyzer (CVA) - FAQs" by Peter Barker on 8 December 2020
Reference: https://blogs.sap.com/2020/12/08/sap-code-vulnerability-analyzer-cva-faqs/

### Introduction

SAP Code Vulnerability Analyzer (CVA) is based on the infrastructure of ABAP Test Cockpit (ATC). ATC provides a general check infrastructure including standard checks for functional correctness and performance. CVA delivers additional security checks.  

### Background about Code Vulnerability Analyzer

The product “SAP Code Vulnerability Analyzer” is available for carrying out security checks. This is also called the code vulnerability analyzer (CVA). The CVA carries out a static analysis of the ABAP source code and reports possible security risks.

The CVA is a product in its own right and is subject to separate remuneration. If you enable the execution of the security checks, additional license costs are incurred.

You can allow the execution of system-wide security checks with the report `RSLIN_SEC_LICENSE_SETUP`. To carry out this step, you require authorization to make changes to global ATC check variants. Before activating the checks, make sure that you possess a valid license for the product. If in doubt, contact your sales representative at SAP.

Once the security checks are enabled, you can execute them in the ABAP Test Cockpit (ATC), the Code Inspector (SCI), and the extended program check. In the system, the security checks are sometimes called “Security Analyses in Extended Program Check (`SLIN_SEC`)”. We recommend that you execute the security checks via the ATC only. This is possible for automatic mass runs and in the ABAP development environment for individual objects.

SAP Note 1921820 describes the releases and Support Packages with which the code vulnerability analyzer is available. SAP Note 1949276 describes restrictions pertaining to the functional scope. The documentation for the report `RSLIN_SEC_LICENSE_SETUP` describes the individual security checks in more detail, among other things.

#### General points about security common to some of all of the checks below

Security problems can occur wherever external data (such as user input) is processed further without being checked.

Sometimes external data is used within a dynamic clause of an OPEN SQL statement. This could enable potential attackers to gain unauthorized access to the SAP database of the system by making unexpected input. This is known as an **SQL injection**.

Sometimes external data is used as a file name to open or delete files on the application server. This can give potential attackers access to the file system of the application server, so enabling them to access confidential information, modify file contents, and change the way the system behaves. This is known as **directory traversal**.

### Hard-coded user name – 0821

User name queries in ABAP indicate security problems. User-specific code often presents a back door for attackers.

#### Procedure

Check if the user name query could possibly indicate a back door. This can be the case, for example, if authorization checks are performed that depend on the user name. Remove these back doors.
Remove any user-dependent code that is not required to run the program.
Some applications cases require user-specific code. However, there is currently no method of detecting these code sections and excluding them from the checks.
The check runs in all places where the current user name (taken directly or indirectly from the system fields SY-UNAME or SYST-UNAME) is compared with a fixed value (taken directly or indirectly from a string literal). To do this, it views the logical condition of the predefined functions BOOLC or BOOLX and the following statements:

1. IF, ELSEIF
1. CASE, WHEN
1. CHECK
1. ASSERT

Some predefined user accounts delivered by SAP have special functions. Since the system must respond to these special user names in certain ways at particular times, comparisons made with these values are permitted and do not produce warnings. The following are some of the user names relevant here:

- CSMREG
- DDIC
- EARLYWATCH
- GOINGLIVE
- SAP
- SAP\*
- SAPCPIC
- SAPSYS
- SAPTERM
- TMSADM

If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.

### Possible SQL injection (WHERE condition) – 1101

Potential manipulation of the dynamic where condition

The **dynamic** WHERE **clause** makes it possible for attackers to inject additional OR conditions that increase the volume of data selected in unexpected ways.

#### Procedure

First check whether it is necessary to use dynamic Open SQL. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, the input data must be checked appropriately before being used in dynamic clauses.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):

1. ESCAPE\_QUOTES
1. ESCAPE\_QUOTES\_STR
1. QUOTE
1. QUOTE\_STR
1. CHECK\_CHAR\_LITERAL
1. CHECK\_STRING\_LITERAL
1. CHECK\_INT\_VALUE
1. CHECK\_VARIABLE\_NAME
1. CHECK\_COLUMN\_NAME
1. CHECK\_WHITELIST\_STR
1. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Ranges tables in a WHERE clause are not at risk of SQL injections. However, if technical reasons require ranges tables in WHERE conditions to be converted and then used in a dynamic WHERE clause, it is best to use the function module FREE\_SELECTIONS\_RANGE\_2\_WHERE. This module is identified as a secure data source; this is not usually the case in self-programmed conversions.

SAP discourages the use of literals with backquotes (\`) in Open SQL statements, even though this is allowed by the syntax. Literals of this type look like string literals in ABAP, but do not have the same attributes. Literals in Open SQL statements are passed to the database, but any blanks at the end of the literals are ignored, which is not the case in ABAP string literals. This is why the use of literals with backquotes (\`) can be confusing.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible directory traversal – 1104

Potential manipulation of the file name in the statement OPEN DATASET or DELETE DATASET

#### Procedure

Externally defined file names must be **validated** before a file is opened or deleted. This is done by the function module FILE\_VALIDATE\_NAME. This function module was made available with the Support Packages or correction instructions listed in SAP Note **1497003**. The ABAP keyword documentation contains an example of file name validation.

Another option is to call the function module FILE\_GET\_NAME\_AND\_VALIDATE or FILE\_GET\_NAME. No unchecked data can be included in the parameter LOGICAL\_FILENAME here. This means the automated check currently requires LOGICAL\_FILENAME to be a constant. In the case of the function module FILE\_GET\_NAME, no unchecked data can be included in the parameters PARAMETER\_1, PARAMETER\_2, and PARAMETER\_3 either.

The function module FILE\_GET\_NAME\_AND\_VALIDATE was made available with SAP Note **1957910**.

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

A **local data flow analysis** is performed.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Possible system command injection (addition FILTER) – 1106

Potential manipulation in the FILTER addition of the statement OPEN DATASET

The addition FILTER of the statement OPEN DATASET can be used to pass statements to the operating system. If this addition is filled from input data, potential attackers can inject further statements and modify the behavior of the application server in unexpected ways. These are known as **system command injections**.

#### Procedure

First check whether it is absolutely necessary to use the addition FILTER. If this is the case, an input check must be performed. The addition FILTER should not be used to send operating system commands.

If an operating system command call is absolutely necessary, however, the SAPXPG mechanism must be used. This offers increased security due to the following characteristics:

- Abstraction from different operating systems
- Predefined operating system commands
- Stricter handling of parameters
- Allows check modules (such as allow lists) to be defined
- Predefined authorization check

New operating system commands must first be defined using transaction SM69. If possible, omit input values because these can also cause a security problem. The function module SXPG\_CALL\_SYSTEM can be used to make calls. See **System Command Injections** for more information.

In some cases, the class **CL\_ABAP\_DYN\_PRG** can be used to perform an allow list check. Here, the following methods are sufficient for the machine check in question (if the RETURNING parameter of the method in question is used in further processing):

1. CHECK\_WHITELIST\_STR
1. CHECK\_WHITELIST\_TAB

The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.

The escape methods of the class CL\_ABAP\_DYN\_PRG are not suitable for operating system commands (or their parameters).

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

A **local data flow analysis** is performed.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Possible ABAP command injection – 1108

Potential injection of harmful code in the statements INSERT REPORT and GENERATE SUBROUTINE POOL

The statements INSERT REPORT and GENERATE SUBROUTINE POOL are used to generate ABAP programs dynamically, which can then be executed. If user input is entered directly in the source code of these generated programs, an attacker could potentially execute any of the operations in the system. These are known as **ABAP command injections**.

#### Procedure

Dynamic generation of ABAP code always carries a high level of risk to security. First, always check whether other dynamic programming methods can be used instead. If dynamic generations are absolutely necessary, all input data must be checked separately and appropriately.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):

1. ESCAPE\_QUOTES
1. ESCAPE\_QUOTES\_STR
1. QUOTE
1. QUOTE\_STR
1. CHECK\_CHAR\_LITERAL
1. CHECK\_STRING\_LITERAL
1. CHECK\_INT\_VALUE
1. CHECK\_VARIABLE\_NAME
1. CHECK\_COLUMN\_NAME
1. CHECK\_TABLE\_OR\_VIEW\_NAME\_STR
1. CHECK\_TABLE\_OR\_VIEW\_NAME\_TAB
1. CHECK\_TABLE\_NAME\_STR
1. CHECK\_TABLE\_NAME\_TAB
1. CHECK\_WHITELIST\_STR
1. CHECK\_WHITELIST\_TAB

Checks on the merged ABAP code passed to the statements INSERT REPORT or GENERATE SUBROUTINE POOL are not feasible.

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible ABAP command injection via RFC Call – 1109

Potential injection of harmful code when the RFC-enabled function module RFC\_ABAP\_INSTALL\_AND\_RUN was called

Calling the RFC-enabled function module RFC\_ABAP\_INSTALL\_AND\_RUN allows dynamic ABAP programs (in remote systems) to be generated, which can then be executed. If user input is entered directly in the source code of these generated programs, an attacker could potentially execute any of the operations in the system. These are known as **ABAP command injections**.

#### Procedure

Dynamic generation of ABAP code always carries a high level of risk to security. First, always check whether other dynamic programming methods can be used instead. If dynamic generations are absolutely necessary, all input data must be checked separately and appropriately.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. ESCAPE\_QUOTES
2. ESCAPE\_QUOTES\_STR
3. QUOTE
4. QUOTE\_STR
5. CHECK\_CHAR\_LITERAL
6. CHECK\_STRING\_LITERAL
7. CHECK\_INT\_VALUE
8. CHECK\_VARIABLE\_NAME
9. CHECK\_COLUMN\_NAME
10. CHECK\_TABLE\_OR\_VIEW\_NAME\_STR
11. CHECK\_TABLE\_OR\_VIEW\_NAME\_TAB
12. CHECK\_TABLE\_NAME\_STR
13. CHECK\_TABLE\_NAME\_TAB
14. CHECK\_WHITELIST\_STR
15. CHECK\_WHITELIST\_TAB

Checks on the merged ABAP code passed to the function module RFC\_ABAP\_INSTALL\_AND\_RUN are not feasible.

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Potentially important reports deleted from the ABAP repository 1110

Potentially important reports deleted from the ABAP repository

Security problems can occur wherever external data (such as user input) is processed further without being checked.
The statement DELETE REPORT or the function module RS\_DELETE\_PROGRAM can be used to delete programs from the ABAP repository. If the name of the deleted program is derived from user input, an attacker can potentially delete ABAP programs required by the application or the system and hence impair system functions.

#### Procedure

If at all possible, the names of programs deleted using DELETE REPORT or RS\_DELETE\_PROGRAM should not be derived from user input for security reasons. If this is absolutely necessary, the input data must be checked first accordingly.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in Validation by Methods of CL\_ABAP\_DYN\_PRG. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note 1852318 provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
1. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report RSLIN\_SEC\_DISPLAY\_BADIS.
Performs a local data flow analysis.
If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an exemption should be requested in ATC.

### Possible SQL injection (SET clause) 1112

Potential manipulation of the SET clause in the statement UPDATE

Potential attackers can use the **dynamic SET clause** to inject additional modifying expressions into the statement UPDATE, and so make unexpected database changes.

#### Procedure

First check whether it is necessary to use dynamic Open SQL. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, the input data must be checked appropriately before being used in dynamic clauses.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. ESCAPE\_QUOTES
2. ESCAPE\_QUOTES\_STR
3. QUOTE
4. QUOTE\_STR
5. CHECK\_CHAR\_LITERAL
6. CHECK\_STRING\_LITERAL
7. CHECK\_INT\_VALUE
8. CHECK\_VARIABLE\_NAME
9. CHECK\_COLUMN\_NAME
10. CHECK\_WHITELIST\_STR
11. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

SAP discourages the use of literals with backquotes (`) in Open SQL statements, even though this is allowed by the syntax. Literals of this type look like string literals in ABAP, but do not have the same attributes. Literals in Open SQL statements are passed to the database, but any blanks at the end of the literals are ignored, which is not the case in ABAP string literals. This is why the use of literals with backquotes (`) can be confusing.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible SQL injection (column names) 1114

Potential read performed on invalid table columns

An attacker may be able to access forbidden columns by **specifying the selected columns dynamically**. Furthermore, in certain contexts (in INTO CORRESPONDING FIELDS) the attacker could attempt to manipulate the application by renaming columns (“COL1 AS COL2”).

#### Procedure

First check whether it is necessary to use dynamic Open SQL. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, the input data must be checked appropriately before being used in dynamic clauses.

If table columns exist that must not be read in the context in question, an allow list check is a good idea (see below). In other case, this message is a false alarm.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible SQL injection (GROUP BY clause) 1116

Potential use of illegal columns in a dynamic GROUP BY clause

Potential attackers can use the dynamic GROUP BY clause to modify the selection in unexpected ways.

#### Procedure

First check whether it is necessary to use dynamic Open SQL. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, the input data must be checked appropriately before being used in dynamic clauses.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_COLUMN\_NAME
2. CHECK\_WHITELIST\_STR
3. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible SQL injection (HAVING clause) – 1117

Potential use of illegal columns in a dynamic HAVING clause

Potential attackers can use the dynamic HAVING clause to modify the selection in unexpected ways.

#### Procedure

First check whether it is necessary to use dynamic Open SQL. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, the input data must be checked appropriately before being used in dynamic clauses.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. ESCAPE\_QUOTES
2. ESCAPE\_QUOTES\_STR
3. QUOTE
4. QUOTE\_STR
5. CHECK\_CHAR\_LITERAL
6. CHECK\_STRING\_LITERAL
7. CHECK\_INT\_VALUE
8. CHECK\_VARIABLE\_NAME
9. CHECK\_COLUMN\_NAME
10. CHECK\_WHITELIST\_STR
11. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

SAP discourages the use of literals with backquotes (`) in Open SQL statements, even though this is allowed by the syntax. Literals of this type look like string literals in ABAP, but do not have the same attributes. Literals in Open SQL statements are passed to the database, but any blanks at the end of the literals are ignored, which is not the case in ABAP string literals. This is why the use of literals with backquotes (`) can be confusing.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible SQL injection (table name when reading) – 1118

Potential read performed on an illegal database table in a SELECT statement

Potential attackers can **specify tables dynamically** and by doing this run operations on database tables other than those intended.

#### Procedure

First check whether it is necessary to use dynamic Open SQL. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, the input data must be checked appropriately before being used in dynamic clauses.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_TABLE\_OR\_VIEW\_NAME\_STR
2. CHECK\_TABLE\_OR\_VIEW\_NAME\_TAB
3. CHECK\_TABLE\_NAME\_STR
4. CHECK\_TABLE\_NAME\_TAB
5. CHECK\_WHITELIST\_STR
6. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible SQL injection (table name when writing) – 1120

Potential read performed on an illegal database table in a modifying OpenSQL statement

Potential attackers can **specify tables dynamically** and by doing this run operations on database tables other than those intended.

#### Procedure

First check whether it is necessary to use dynamic Open SQL. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, the input data must be checked appropriately before being used in dynamic clauses.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_TABLE\_OR\_VIEW\_NAME\_STR
2. CHECK\_TABLE\_OR\_VIEW\_NAME\_TAB
3. CHECK\_TABLE\_NAME\_STR
4. CHECK\_TABLE\_NAME\_TAB
5. CHECK\_WHITELIST\_STR
6. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Potential read performed using an invalid secondary database connection in an Open SQL statement – 1121

Security problems can occur wherever external data (such as user input) is processed further without being checked.
Here, external data is used within a dynamic clause of an OPEN SQL statement. This could enable potential attackers to gain unauthorized access to the SAP database of the system by making unexpected input. This is known as an SQL injection.
Attackers can potentially use dynamically specified CONNECTIONs to execute operations using a secondary database connection.

#### Procedure

First check whether it is necessary to use dynamic Open SQL. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, the input data must be checked appropriately before being used in dynamic clauses.
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report RSLIN\_SEC\_DISPLAY\_BADIS.
If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.
A local data flow analysis is performed.

### Possible SQL injection via object services – 1122

Potential manipulation of a dynamic WHERE condition using the parameter I\_FILTER of the object services method CREATE\_QUERY

In the present case, external data is passed to the method CREATE\_QUERY of an object services class. This class handles the data like a dynamic WHERE clause. This makes it possible for attackers to inject additional OR conditions that increase the volume of data selected in unexpected ways. This is known as an **SQL injection**.

#### Procedure

The input values for the parameter I\_FILTER of the object services method CREATE\_QUERY must be made subject to an input check.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. ESCAPE\_QUOTES
2. ESCAPE\_QUOTES\_STR
3. QUOTE
4. QUOTE\_STR
5. CHECK\_CHAR\_LITERAL
6. CHECK\_STRING\_LITERAL
7. CHECK\_INT\_VALUE
8. CHECK\_VARIABLE\_NAME
9. CHECK\_COLUMN\_NAME
10. CHECK\_WHITELIST\_STR
11. CHECK\_WHITELIST\_TAB

Furthermore, the function module FREE\_SELECTIONS\_RANGE\_2\_WHERE is also accepted as a suitable input check.

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

SAP discourages the use of literals with backquotes (`) in Open SQL statements, even though this is allowed by the syntax. Literals of this type look like string literals in ABAP, but do not have the same attributes. Literals in Open SQL statements are passed to the database, but any blanks at the end of the literals are ignored, which is not the case in ABAP string literals. This is why the use of literals with backquotes (`) can be confusing.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible Directory Traversal via file utilities class – 1124

Potential manipulation of the file name in the method CREATE\_UTF8\_FILE\_WITH\_BOM of the class CL\_ABAP\_FILE\_UTILITIES

#### Procedure

Externally defined file names must be **validated** before a file is opened or deleted. This is done by the function module FILE\_VALIDATE\_NAME. This function module was made available with the Support Packages or correction instructions listed in SAP Note **1497003**. The ABAP keyword documentation contains an **example** of file name validation.

Another option is to call the function module FILE\_GET\_NAME\_AND\_VALIDATE or FILE\_GET\_NAME. No unchecked data can be included in the parameter LOGICAL\_FILENAME here. This means the automated check currently requires LOGICAL\_FILENAME to be a constant. In the case of the function module FILE\_GET\_NAME, no unchecked data can be included in the parameters PARAMETER\_1, PARAMETER\_2, and PARAMETER\_3 either.

The function module FILE\_GET\_NAME\_AND\_VALIDATE was made available with SAP Note **1957910**.

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

A **local data flow analysis** is performed.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Potential directory traversal due to insecure parameters – 1126

Non-secure parameter of the function module FILE\_GET\_NAME used

In this case, the external data is passed to the non-secure parameters of function module FILE\_GET\_NAME.  If this data is part of the file name, users might be able to access data for which they have no authorization and read or even overwrite this data. This is also known as **directory traversal**.

#### Procedure

It is advisable to use the function module FILE\_GET\_NAME\_AND\_VALIDATE. This function module was made available with SAP Note **1957910**.

If the function module FILE\_GET\_NAME still needs to be used:

In some cases, the class **CL\_ABAP\_DYN\_PRG** can be used to perform an allow list check. Here, the following methods are sufficient for the machine check in question (if the RETURNING parameter of the method in question is used in further processing):



         1. CHECK\_WHITELIST\_STR
         1. CHECK\_WHITELIST\_TAB

The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

A **local data flow analysis** is performed. Here the parameters PARAMETER\_1, PARAMETER\_2, PARAMETER\_3 of the function module FILE\_GET\_NAME are regarded as critical.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Possible SQL injection via ADBC (DDL) – 1128

Potential injection of harmful SQL statements or clauses in execution of DDL statements in ADBC

In the present case, external data is injected into an SQL statement that is passed for execution by the ADBC API of the database. This could enable potential attackers to gain unauthorized access to the SAP database of the system by making unexpected input. This is known as an **SQL injection**.

Unlike using Open SQL with dynamic clauses, ADBC is used to specify complete SQL statements. This increases the level of risk.

#### Procedure

First check whether it is necessary to use dynamic ADBC. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, consider a switch to dynamic Open SQL. This reduces the opportunities for attacks considerably.

If it is absolutely essential that you use ADBC, make sure that no user input is entered directly into the SQL statement. Use ? placeholders for dynamic components of the statement. The relevant ABAP data objects can then be connected to these placeholders by using the method SET\_PARAM of class CL\_SQL\_STATEMENT.

In exceptional cases, it may still be necessary to create the SQL statement based on user entries. These entries must be thoroughly checked beforehand.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible SQL injection via ADBC (DML) – 1130

Potential injection of harmful SQL statements of clauses in execution of DML statements in ADBC

In the present case, external data is injected into an SQL statement that is passed for execution by the ADBC API of the database. This could enable potential attackers to gain unauthorized access to the SAP database of the system by making unexpected input. This is known as an **SQL injection**.

Unlike using Open SQL with dynamic clauses, ADBC is used to specify complete SQL statements. This increases the level of risk.

#### Procedure

First check whether it is necessary to use dynamic ADBC. Switching to static OPEN SQL provides a full solution to the security problem. If this is not possible, consider a switch to dynamic Open SQL. This reduces the opportunities for attacks considerably.

If it is absolutely essential that you use ADBC, make sure that no user input is entered directly into the SQL statement. Use ? placeholders for dynamic components of the statement. The relevant ABAP data objects can then be connected to these placeholders by using the method SET\_PARAM of class CL\_SQL\_STATEMENT.

In exceptional cases, it may still be necessary to create the SQL statement based on user entries. These entries must be thoroughly checked beforehand.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. ESCAPE\_QUOTES
2. QUOTE
3. CHECK\_CHAR\_LITERAL
4. CHECK\_INT\_VALUE
5. CHECK\_VARIABLE\_NAME
6. CHECK\_COLUMN\_NAME
7. CHECK\_TABLE\_OR\_VIEW\_NAME\_STR
8. CHECK\_TABLE\_OR\_VIEW\_NAME\_TAB
9. CHECK\_TABLE\_NAME\_STR
10. CHECK\_TABLE\_NAME\_TAB
11. CHECK\_WHITELIST\_STR
12. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Possible cross-site scripting – 1132

Potential risk of cross-site scripting

In the case in question, external data is used without encoding to generate a dynamic HTML page and then forwarded to the browser. This makes it possible for an attacker to send unwanted scripts to the browser of the victim (using unwanted input) and execute malicious code there. In this case, this is known as reflected **cross-site scripting (XSS)**.

#### Procedure

The first check should verify whether modern Web frameworks can be used with automatic XSS protection, for example Web Dynpro or BSP with HTMLB extension, where encoding is activated globally (using the attribute forceEncode=”ENABLED” of the HTMLB:CONTENT tag). If this is not possible, the external data must be encoded before being used in the dynamic HTML page.

For BSP pages, global encoding can be set using the global page attribute forceEncode:
``<%@page language=”abap” forceEncode=”html|url|javascript|css”%>``

If necessary, global encoding can be overwritten by local encoding:
- ``<%html=...%>`` – HTML encoding
- ``<%url=...%>`` – URL encoding for parameter names or parameter values of URLs
- ``<%javascript=...%>`` – JavaScript encoding
- ``<%css=...%>`` – CSS encoding
- ``<%raw=...%>`` – No encoding

More information can be found in SAP Note **887168**.

An encoding API should be used for generic Web applications. From Release SAP\_BASIS 731, the built-in function ESCAPE:
- out = escape(val = val format = cl\_abap\_format=>e\_xss\_ml)
- out = escape(val = val format = cl\_abap\_format=>e\_xss\_js)
- out = escape(val = val format = cl\_abap\_format=>e\_xss\_url)
- out = escape(val = val format = cl\_abap\_format=>e\_xss\_css)

In lower releases, methods of the class CL\_ABAP\_DYN\_PRG:
- CL\_ABAP\_DYN\_PRG=>ESCAPE\_XSS\_XML\_HTML
- CL\_ABAP\_DYN\_PRG=>ESCAPE\_XSS\_JAVASCRIPT
- CL\_ABAP\_DYN\_PRG=>ESCAPE\_XSS\_CSS
- CL\_ABAP\_DYN\_PRG=>ESCAPE\_XSS\_URL

The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.

For more information, see “XSS Secure Programming Guide”. SAP Note **1714836**.

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Potential reflected cross-site scripting – 1134

Potential reflected cross-site scripting

In the case in question, external data from a HTTP request (such as URL parameters and HTML form fields) are used without encoding to generate a dynamic HTML page and then forwarded to the browser. This makes it possible for an attacker to send unwanted scripts to the browser of the victim (using unwanted input) and execute malicious code there. In this case, this is known as reflected **cross-site scripting (XSS)**.

#### Procedure

The first check should verify whether modern Web frameworks can be used with automatic XSS protection, for example Web Dynpro or BSP with HTMLB extension, where encoding is activated globally (using the attribute forceEncode=”ENABLED” of the HTMLB:CONTENT tag). If this is not possible, the external data must be encoded before being used in the dynamic HTML page.


For BSP pages, global encoding can be set using the global page attribute forceEncode:
``<%@page language=”abap” forceEncode=”html|url|javascript|css”%>``

If necessary, global encoding can be overwritten by local encoding:
- ``<%html=...%>`` – HTML encoding
- ``<%url=...%>`` – URL encoding for parameter names or parameter values of URLs
- ``<%javascript=...%>`` – JavaScript encoding
- ``<%css=...%>`` – CSS encoding
- ``<%raw=...%>`` – No encoding

More information can be found in SAP Note **887168**.

An encoding API should be used for generic Web applications. From Release SAP\_BASIS 731, the built-in function ESCAPE:
- out = escape(val = val format = cl\_abap\_format=>e\_xss\_ml)
- out = escape(val = val format = cl\_abap\_format=>e\_xss\_js)
- out = escape(val = val format = cl\_abap\_format=>e\_xss\_url)
- out = escape(val = val format = cl\_abap\_format=>e\_xss\_css)

In lower releases, methods of the class CL\_ABAP\_DYN\_PRG:
- CL\_ABAP\_DYN\_PRG=>ESCAPE\_XSS\_XML\_HTML
- CL\_ABAP\_DYN\_PRG=>ESCAPE\_XSS\_JAVASCRIPT
- CL\_ABAP\_DYN\_PRG=>ESCAPE\_XSS\_CSS
- CL\_ABAP\_DYN\_PRG=>ESCAPE\_XSS\_URL

The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.

For more information, see “XSS Secure Programming Guide”. SAP Note **1714836**.

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### User-controlled dynamic CALL FUNCTION via RFC – 1140

Potential call of invalid function module using RFC

In the case in question, external data is used as the name of a function modules that is called using RFC. An attacker might be able to call forbidden function modules in other systems by entering unexpected data in this location. The problem is made worse because function parameters with incorrect names at runtime are ignored in this case and character-like parameters with incorrect lengths are truncated if necessary. This is also the case if ‘NONE’ was specified as DESTINATION.

#### Procedure

First check whether it is necessary to use **dynamic procedure calls**. Switching to static calls provides a full solution to the security problem presented here. If this is not possible, the input data must be checked accordingly before being used as function module names.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

**Note**

The message in questions is only produced for RFC calls, i.e. calls with the addition DESTINATION. In other **dynamic function module calls**, **SLIN\_SEC 1144** is produced if necessary.

### User-controlled dynamic program unit call – 1141

Potential call of an illegal program using the statement SUBMIT

In the case in question, external data is used within a dynamic program call (meaning a **dynamic call** of a report or transaction). This enables potential attackers to call unexpected program units by making unexpected input, and thereby illegally modify the running of the program.

#### Procedure

First check whether it is necessary to use dynamic program calls. Switching to static calls provides a full solution to the security problem presented here. If this is not possible, the input data must be checked accordingly before being used as report names or transaction names.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### User-controlled dynamic CALL TRANSACTION – 1142

Dynamic CALL TRANSACTION without allow list check

When a transaction is started from the user interface or by executing the ABAP command LEAVE TO TRANSACTION, an authorization check is performed automatically. The check is always made using the authorization object S\_TCODE . In the transaction code editor (transaction SE93), developers can specify an authorization object with authorization values. If an entry of this type is found, the authorization specified there is checked.

These authorization checks are not usually made  when the ABAP command CALL TRANSACTION is executed. The developer of the program in question must decide whether the transaction start authorization is checked.

A corresponding authorization check was found in the dynamic CALL TRANSACTION in question. This authorization check is usually enough to secure the dynamic CALL TRANSACTION statement in question. In special cases, it may be appropriate to have an additional allow list check (see the following explanation).

In the case in question, external data is used within a dynamic program call (meaning a **dynamic call** of a report or transaction). This enables potential attackers to call unexpected program units by making unexpected input, and thereby illegally modify the running of the program.

#### Procedure

First check whether it is necessary to use dynamic program calls. Switching to static calls provides a full solution to the security problem presented here. If this is not possible, the input data must be checked accordingly before being used as report names or transaction names.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### User-controlled dynamic LEAVE TO TRANSACTION – 1143

Potential call of an unwanted transaction using the statement LEAVE TO TRANSACTION

In the case in question, external data is used within a dynamic program call (meaning a **dynamic call** of a report or transaction). This enables potential attackers to call unexpected program units by making unexpected input, and thereby illegally modify the running of the program.

#### Procedure

First check whether it is necessary to use dynamic program calls. Switching to static calls provides a full solution to the security problem presented here. If this is not possible, the input data must be checked accordingly before being used as report names or transaction names.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Dynamic function module call controllable from UI or via RFC – 1144

UI-driven or RFC-driven dynamic call of a function module

Here, the name of a called function module can be controlled using either the user interface or a parameter of an RFC-enabled function module.

An attacker might be able to call forbidden function modules by entering unexpected data in this location.

#### Procedure

First check whether it is necessary to use dynamic function module calls. Switching to static calls provides a full solution to the security problem presented here. If this is not possible, the input data must be checked accordingly before being used as function module names.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Dynamic function module calls are frequent, which means that only those calls are registered here for which some or all of the function module name can be controlled meaningfully using the user interface or RFC. To do this, a local data flow analysis is performed.

If a source code position is flagged and does not present a security problem and an input check (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

**Note:**
This message (SLIN\_SEC 1144) is **not** raised for **dynamic function module calls** that represent an **RFC call**. In dynamic RFC calls like this, **SLIN\_SEC 1140** is produced if necessary. For this reason, the checks SLIN\_SEC 1140 and 1144:
- must always be performed together (neither of the checks should be suppressed using its priority) or
- must always be suppressed together using their priorities.

A local data flow analysis is performed, which means that exactly those occurrences are registered in which the function module name can be controlled using the user interface of the wrapper program or in which dynamic calls within a function group can be controlled using the parameter of an RFC-enabled function module of this function group.

### Static CALL TRANSACTION without check of authorization object from SE93 – 114A

Static CALL TRANSACTION without check on authorization from the transaction editor

When a transaction is started from the user interface or by executing the ABAP command LEAVE TO TRANSACTION, an authorization check is performed automatically. The check is always made using the authorization object S\_TCODE . In the transaction code editor (transaction SE93), developers can specify an authorization object with authorization values. If an entry of this type is found, the authorization specified there is checked.

These authorization checks are not usually made  when the ABAP command CALL TRANSACTION is executed. The developer of the program in question must decide whether the transaction start authorization is checked.

Here, a check on the authorization object S\_TCODE was found. The additional check was specified in the transaction editor, but no check on this authorization was found in the code. This could present a security risk.

#### Procedure

If the authorization check is missing by accident, it can implemented in full by simply calling the function module AUTHORITY\_CHECK\_TCODE . From SAP\_BASIS 7.40 SP 02, the addition WITH AUTHORITY-CHECK in CALL TRANSACTION can also be used to do this. It works in the same way as the function module call. Both methods check the authorization object S\_TCODE. It may be a good idea to remove the explicit AUTHORITY-CHECK on S\_TCODE.

If the check on the authorization object from the transaction editor in CALL TRANSACTION was skipped intentionally, the addition WITHOUT AUTHORITY-CHECK in CALL TRANSACTION can be used from SAP\_BASIS 7.40 SP 02. In systems with older releases, the message must be suppressed using an exemption.

For each CALL TRANSACTION, a search is made for the AUTHORITY-CHECKs or a suitable call of the function module AUTHORITY\_CHECK\_TCODE within the same procedure (event, method, function, form routine). In called procedures, a search is also performed for these authorization checks up to a nesting depth of five. If an AUTHORITY-CHECK on the authorization object S\_TCODE is found, but no AUTHORITY-CHECK that matches the entry from the transaction editor, the message is displayed.

### Static CALL TRANSACTION without check of authorization object S\_TCODE – 114B

Static CALL TRANSACTION without check on authorization object S\_TCODE.

When a transaction is started from the user interface or by executing the ABAP command LEAVE TO TRANSACTION, an authorization check is performed automatically. The check is always made using the authorization object S\_TCODE . In the transaction code editor (transaction SE93), developers can specify an authorization object with authorization values. If an entry of this type is found, the authorization specified there is checked.

These authorization checks are not usually made  when the ABAP command CALL TRANSACTION is executed. The developer of the program in question must decide whether the transaction start authorization is checked.

Here, a check on the authorization object from the transaction editor (SE93) was found. The code, however, did not contain a check on the authorization object S\_TCODE for the called transaction. This could present a security risk.

#### Procedure

If the authorization check is missing by accident, it can implemented in full by simply calling the function module AUTHORITY\_CHECK\_TCODE . From SAP\_BASIS 7.40 SP 02, the addition WITH AUTHORITY-CHECK in CALL TRANSACTION can also be used to do this. It works in the same way as the function module call. Both methods check the authorization from the transaction editor (SE93) and the authorization object S\_TCODE. It may be a good idea to remove the explicit AUTHORITY-CHECK on the authorization from the transaction editor (SE93).

If the check on the authorization object S\_TCODE in CALL TRANSACTION was skipped intentionally, the addition WITHOUT AUTHORITY-CHECK in CALL TRANSACTION can be used from SAP\_BASIS 7.40 SP 02. In systems with older releases, the message must be suppressed using an exemption.

For each CALL TRANSACTION, a search is made for the AUTHORITY-CHECKs or a suitable call of the function module AUTHORITY\_CHECK\_TCODE within the same procedure (event, method, function, form routine). In called procedures, a search is also performed for these authorization checks up to a nesting depth of five. If an AUTHORITY-CHECK on the authorization from the transaction editor (SE93) is found, but no AUTHORITY-CHECK on S\_TCODE, the message is displayed.

### Static CALL TRANSACTION without authorization check – 114C

Static CALL TRANSACTION without authorization check

When a transaction is started from the user interface or by executing the ABAP command LEAVE TO TRANSACTION, an authorization check is performed automatically. The check is always made using the authorization object S\_TCODE . In the transaction code editor (transaction SE93), developers can specify an authorization object with authorization values. If an entry of this type is found, the authorization specified there is checked.

These authorization checks are not usually made  when the ABAP command CALL TRANSACTION is executed. The developer of the program in question must decide whether the transaction start authorization is checked.

Here, none of the authorization checks above were found in the code. This could present a security risk.

#### Procedure

If the authorization checks are missing by accident, they can implemented in full by simply calling the function module AUTHORITY\_CHECK\_TCODE . From SAP\_BASIS 7.40 SP 02, the addition WITH AUTHORITY-CHECK in CALL TRANSACTION can also be used to do this. It works in the same way as the function module call. Both methods check the authorization object S\_TCODE and the authorization from the transaction editor (SE93). However, you should note that table TCDCOUPLES is taken into account for function module AUTHORITY\_CHECK\_TCODE.

In a few situations, it is intentional that no authorizations are checked in CALL TRANSACTION (or only the authorization for the authorization S\_TCODE). In these cases, the addition WITHOUT AUTHORITY-CHECK can be used in CALL TRANSACTION from SAP\_BASIS 7.40 SP 02. In systems with older releases, the message must be suppressed using an exemption. In dynamic CALL TRANSACTIONSs, it is only acceptable to skip the authorization check if the set of callable transactions is restricted (usually by using a allow list check).

SAP recommends that the entries in table TCDCOUPLES (transaction SE97) are no longer used to control the behavior.

For each CALL TRANSACTION, a search is made for the AUTHORITY-CHECKs or a suitable call of the function module AUTHORITY\_CHECK\_TCODE within the same procedure (event, method, function, form routine). In called procedures, a search is also performed for these authorization checks up to a restricted nesting depth. The message is displayed if none of the authorization checks above are found.

### Static CALL TRANSACTION without authorization check (restricted function) – 114D

Static CALL TRANSACTION without authorization check in the case of restricted functions

When a transaction is started from the user interface or by executing the ABAP command LEAVE TO TRANSACTION, an authorization check is performed automatically. The check is always made using the authorization object S\_TCODE . In the transaction code editor (transaction SE93), developers can specify an authorization object with authorization values. If an entry of this type is found, the authorization specified there is checked.

These authorization checks are not usually made  when the ABAP command CALL TRANSACTION is executed. The developer of the program in question must decide whether the transaction start authorization is checked.

Here, none of the authorization checks above were found in the code. This could present a security risk. The security risk becomes slightly less severe because a restriction to a specific context (AND SKIP FIRST SCREEN or USING bdc\_tab) was found in the statement CALL TRANSACTION .

#### Procedure

If the authorization checks are missing by accident, they can implemented in full by simply calling the function module AUTHORITY\_CHECK\_TCODE . From SAP\_BASIS 7.40 SP 02, the addition WITH AUTHORITY-CHECK in CALL TRANSACTION can also be used to do this. It works in the same way as the function module call. Both methods check the authorization object S\_TCODE and the authorization from the transaction editor (SE93). However, you should note that table TCDCOUPLES is taken into account for function module AUTHORITY\_CHECK\_TCODE.

In a few situations, it is intentional that no authorizations are checked in CALL TRANSACTION (or only the authorization for the authorization S\_TCODE). In these cases, the addition WITHOUT AUTHORITY-CHECK can be used in CALL TRANSACTION from SAP\_BASIS 7.40 SP 02. In systems with older releases, the message must be suppressed using an exemption. In dynamic CALL TRANSACTIONSs, it is only acceptable to skip the authorization check if the set of callable transactions is restricted (usually by using an allow list check).

SAP recommends that the entries in table TCDCOUPLES (transaction SE97) are no longer used to control the behavior.

For each CALL TRANSACTION, a search is made for the AUTHORITY-CHECKs or a suitable call of the function module AUTHORITY\_CHECK\_TCODE within the same procedure (event, method, function, form routine). In called procedures, a search is also performed for these authorization checks up to a restricted nesting depth. The message is displayed if none of the authorization checks above are found.

### Dynamic CALL TRANSACTION without authorization check – 114E

Dynamic CALL TRANSACTION without authorization check

When a transaction is started from the user interface or by executing the ABAP command LEAVE TO TRANSACTION, an authorization check is performed automatically. The check is always made using the authorization object S\_TCODE . In the transaction code editor (transaction SE93), developers can specify an authorization object with authorization values. If an entry of this type is found, the authorization specified there is checked.

These authorization checks are not usually made  when the ABAP command CALL TRANSACTION is executed. The developer of the program in question must decide whether the transaction start authorization is checked.

In the dynamic CALL TRANSACTION in question, none of the authorization checks above were found in the code. This could present a security risk.

An allow list check restricts the set of potential transactions called, which means that the security risk is already reduced. Depending on the context, this restriction may be enough.

#### Procedure

If the authorization checks are missing by accident, they can implemented in full by simply calling the function module AUTHORITY\_CHECK\_TCODE . From SAP\_BASIS 7.40 SP 02, the addition WITH AUTHORITY-CHECK in CALL TRANSACTION can also be used to do this. It works in the same way as the function module call. Both methods check the authorization object S\_TCODE and the authorization from the transaction editor (SE93). However, you should note that table TCDCOUPLES is taken into account for function module AUTHORITY\_CHECK\_TCODE.

In a few situations, it is intentional that no authorizations are checked in CALL TRANSACTION (or only the authorization for the authorization S\_TCODE). In these cases, the addition WITHOUT AUTHORITY-CHECK can be used in CALL TRANSACTION from SAP\_BASIS 7.40 SP 02. In systems with older releases, the message must be suppressed using an exemption. In dynamic CALL TRANSACTIONSs, it is only acceptable to skip the authorization check if the set of callable transactions is restricted (usually by using an allow list check).

SAP recommends that the entries in table TCDCOUPLES (transaction SE97) are no longer used to control the behavior.

For each dynamic CALL TRANSACTION, a search is made for the a suitable call of the function module AUTHORITY\_CHECK\_TCODE within the same procedure (event, method, function, form routine). In called procedures, a search is also performed for these authorization checks up to a restricted nesting depth. The message is displayed if none of the authorization checks above are found.

### Dynamic CALL TRANSACTION with dataflow and without authorization check – 114F

Dynamic CALL TRANSACTION without authorization check and without an allow list check

In the case in question, external data is used within a **dynamic transaction call**. This enables potential attackers to call unexpected transactions by making unexpected input, and thereby illegally modify the running of the program.

When a transaction is started from the user interface or by executing the ABAP command LEAVE TO TRANSACTION, an authorization check is performed automatically. The check is always made using the authorization object S\_TCODE . In the transaction code editor (transaction SE93), developers can specify an authorization object with authorization values. If an entry of this type is found, the authorization specified there is checked.

These authorization checks are not usually made  when the ABAP command CALL TRANSACTION is executed. The developer of the program in question must decide whether the transaction start authorization is checked.

In this dynamic CALL TRANSACTION, the potential influence of the user input on the called transaction was identified and, additionally, none of the authorization checks mentioned above were found in the source code. This could present a security risk.

#### Procedure

Both the authorization check aspect and the aspect of dynamics in the name of the called transaction must be considered. In most situations, an authorization check is the best choice for removing the security problem.

### Authorization check

If the authorization checks are missing by accident, they can implemented in full by simply calling the function module AUTHORITY\_CHECK\_TCODE . From SAP\_BASIS 7.40 SP 02, the addition WITH AUTHORITY-CHECK in CALL TRANSACTION can also be used to do this. It works in the same way as the function module call. Both methods check the authorization object S\_TCODE and the authorization from the transaction editor (SE93). However, you should note that table TCDCOUPLES is taken into account for function module AUTHORITY\_CHECK\_TCODE.

In a few situations, it is intentional that no authorizations are checked in CALL TRANSACTION (or only the authorization for the authorization S\_TCODE). In these cases, the addition WITHOUT AUTHORITY-CHECK can be used in CALL TRANSACTION from SAP\_BASIS 7.40 SP 02. In systems with older releases, the message must be suppressed using an exemption. In dynamic CALL TRANSACTIONSs, it is only acceptable to skip the authorization check if the set of callable transactions is restricted (usually by using an allow list check).

SAP recommends that the entries in table TCDCOUPLES (transaction SE97) are no longer used to control the behavior.

### Dynamics in the name of the called transaction

First check whether it is necessary to use dynamic transaction calls. Switching to static calls provides a solution to the security problem presented here at least. If this is not possible, the input data must be checked accordingly before being used as transaction names.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

If an authorization check is not predefined, an error message stating that the authorization check is missing might be raised the next time the program is checked.

A **local data flow analysis** is performed.

For each dynamic CALL TRANSACTION, a search is made for the a suitable call of the function module AUTHORITY\_CHECK\_TCODE within the same procedure (event, method, function, form routine). In called procedures, a search is also performed for these authorization checks up to a restricted nesting depth. The message is displayed if none of the authorization checks above are found.

### Dynamic CALL TRANSACTION with data flow, authorization check of S\_TCODE – 114G

Dynamic CALL TRANSACTION with potentially incomplete authorization check

In the case in question, external data is used within a **dynamic transaction call**. This enables potential attackers to call unexpected transactions by making unexpected input, and thereby illegally modify the running of the program.

When a transaction is started from the user interface or by executing the ABAP command LEAVE TO TRANSACTION, an authorization check is performed automatically. The check is always made using the authorization object S\_TCODE . In the transaction code editor (transaction SE93), developers can specify an authorization object with authorization values. If an entry of this type is found, the authorization specified there is checked.

These authorization checks are not usually made  when the ABAP command CALL TRANSACTION is executed. The developer of the program in question must decide whether the transaction start authorization is checked.

In this dynamic CALL TRANSACTION, the potential influence of the user input on the called transaction was identified and, additionally, only one authorization check on the authorization object S\_TCODE was found in the source code. The authorization check might be incomplete, since the system analysis cannot decide whether a check was made on the authorization from the transaction editor (transaction SE93). This could present a security risk.

#### Procedure

Both the authorization check aspect and the aspect of dynamics in the name of the called transaction must be considered. In most situations, an authorization check is the best choice for removing the security problem.

### Authorization check

If the authorization checks are missing by accident, they can implemented in full by simply calling the function module AUTHORITY\_CHECK\_TCODE . From SAP\_BASIS 7.40 SP 02, the addition WITH AUTHORITY-CHECK in CALL TRANSACTION can also be used to do this. It works in the same way as the function module call. Both methods check the authorization object S\_TCODE and the authorization from the transaction editor (SE93). However, you should note that table TCDCOUPLES is taken into account for function module AUTHORITY\_CHECK\_TCODE.

In a few situations, it is intentional that no authorizations are checked in CALL TRANSACTION (or only the authorization for the authorization S\_TCODE). In these cases, the addition WITHOUT AUTHORITY-CHECK can be used in CALL TRANSACTION from SAP\_BASIS 7.40 SP 02. In systems with older releases, the message must be suppressed using an exemption. In dynamic CALL TRANSACTIONSs, it is only acceptable to skip the authorization check if the set of callable transactions is restricted (usually by using an allow list check).

SAP recommends that the entries in table TCDCOUPLES (transaction SE97) are no longer used to control the behavior.

### Dynamics in the name of the called transaction

First check whether it is necessary to use dynamic transaction calls. Switching to static calls provides a solution to the security problem presented here. If this is not possible, the input data must be checked accordingly before being used as transaction names.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

A **local data flow analysis** is performed.

For each dynamic CALL TRANSACTION, a search is made for the a suitable call of the function module AUTHORITY\_CHECK\_TCODE within the same procedure (event, method, function, form routine). In called procedures, a search is also performed for these authorization checks up to a restricted nesting depth. The message is displayed if none of the authorization checks above are found.

### Usage of an obsolete escaping method – 1150

Obsolete escape method used

The ESCAPE methods of classes CL\_HTTP\_UTILITY, CL\_HTTP\_SERVER and CL\_HTTP\_CLIENT are obsolete and do not fulfill the current OWASP recommendations. They have been replaced by the predefined function ESCAPE( ).

#### Procedure

Calls of the obsolete escape methods must be replaced by calls of the predefined function ESCAPE( ), appropriate for the context in question. Valid formats are the values E\_XSS\_\* from the class CL\_ABAP\_FORMAT. More information is available in the ABAP keyword documentation.

The specified formats are not available for the predefined function ESCAPE( ) in releases older than SAP\_BASIS 731. Instead, you need to use the ESCAPE\_XSS\_\* methods from class CL\_ABAP\_DYN\_PRG. The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.

All calls of the obsolete methods are reported. This relates to the following methods:
- CL\_HTTP\_UTILITY=>ESCAPE\_JAVASCRIPT
- CL\_HTTP\_UTILITY=>ESCAPE\_HTML
- CL\_HTTP\_UTILITY=>ESCAPE\_URL
- CL\_HTTP\_UTILITY=>ESCAPE\_WML
- CL\_HTTP\_UTILITY=>ESCAPE\_XML\_CHAR\_DATA
- CL\_HTTP\_UTILITY=>ESCAPE\_XML\_ATTR\_VALUE
- CL\_HTTP\_SERVER=>ESCAPE\_HTML
- CL\_HTTP\_SERVER=>ESCAPE\_URL
- CL\_HTTP\_CLIENT=>ESCAPE\_HTML
- CL\_HTTP\_CLIENT=>ESCAPE\_URL
- 
### forceEncode=”enabled” not specified for htmlb:content – 1151

On BSP pages that use htmlb, the values of the attributes of the htmlb tags are encoded only if the attribute forceEncode in htmlb:content has the value ‘enabled’. This is the only way to ensure that the encoding matches the context in which an attribute value is used in the finished HTML document.

#### Procedure

The attribute forceEncode of the htmlb tag htmlb:content is set to the value ‘enabled’.

For more information, see “XSS Secure Programming Guide”. SAP Note **1714836**.

If the position in question does not have any security problems and there is no point in modifying the BSP page, an **exemption** should be requested in ATC.

### In tag htmlb:content an obsolete design is specified or none at all – 1152

**Obsolete design or no design specified for htmlb:content**

Message number 1152

The designs CLASSIC and DESIGN2002 are no longer supported. This means that any XSS weaknesses in the associated BSP extensions htmlb, xhtmlb, and so on are no longer corrected.

Only the designs DESIGNS2003 and DESIGN2008 (and any later designs) encode all attributes in full and are secure with respect to cross site scripting (XSS).

#### Procedure

Obsolete designs can no longer be used. A design must be specified explicitly, since otherwise the obsolete design CLASSIC is used.

For more information, see “XSS Secure Programming Guide”. SAP Note **1714836**.

### AUTHORITY-CHECK without processing of sy-subrc – 1160

SY-SUBRC not evaluated after the statement AUTHORITY-CHECK

After the statement AUTHORITY-CHECK, sy-subrc needs to be read. If sy-subrc is not equal to 0, the authorization check was not successful.

The check confirms whether a read is performed on sy-subrc directly after AUTHORITY-CHECK. Statements such as ENDIF, simple assignments (that do not modify sy-subrc), or comments are permitted before reads. Some declarative statements, such as DATA, are also valid. Many ABAP statements (such as REFRESH) set sy-subrc and are not allowed. If there is an empty IF/ENDIF block after AUTHORITY-CHECK, a problem is detected.

#### Procedure

Insert a sy-subrc query with a valid response (for example, cancel).

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Call of a security-relevant method without handling the return value – 1161

Return value (for example, SY-SUBRC) not evaluated after a security-relevant method was called

It is necessary to evaluate the return value (for example, SY-SUBRC) after a security-relevant method such as CL\_SACF=>AUTH\_CHECK\_SPEC( ) has been called.

From the perspective of this check, the following methods are security-relevant:
- CL\_SACF=>AUTH\_CHECK\_SPEC( )
- CL\_SACF=>AUTH\_CHECK\_SPEC\_USER( )

Further procedures can be registered as security-relevant using the BAdI **SLIN\_BADI\_SEC\_PROCEDURES**. All registered security-relevant function modules and methods can be displayed using the report **RSLIN\_SEC\_DISPLAY\_SECREL\_PROC**.

This check checks whether the return value (for example, SY-SUBRC) is read immediately after the procedure call. Statements such as ENDIF, simple assignments (that do not modify the return value), or comments are permitted before reads. Some declarative statements, such as DATA, are also valid. If there is an empty IF/ENDIF block after the method call, a problem is registered.

#### Procedure

Insert an evaluation of the return value (for example, SY-SUBRC) with an appropriate response (for example, error message or cancel).

If the message is caused by an inconsistent **BAdI implementation**, contact the person responsible for the BAdI implementation classes specified in the message details.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Return value (for example, SY-SUBRC) not evaluated after a security-relevant subroutine was called. – 1162

Return value (for example, SY-SUBRC) not evaluated after a security-relevant subroutine was called.

Once the security-relevant subroutine (FORM routine) has been called, a check must be run to verify that execution was successful. In general this is an evaluation of the corresponding output parameter or a check of system field SY-SUBRC, if the subroutine uses this as an implicit output parameter.

#### Procedure

Insert an evaluation of the return value (for example, SY-SUBRC) with an appropriate response (for example, error message or cancel).

If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.

A check is run to verify if an evaluation is performed after an external security-relevant subroutine is called.

The check can be expanded by using a BAdI.

Further procedures can be registered as security-relevant using the BAdI SLIN\_BADI\_SEC\_PROCEDURES. All registered security-relevant procedures can be displayed using the report RSLIN\_SEC\_DISPLAY\_SECREL\_PROC.

This check checks whether the return value (for example, SY-SUBRC) is read immediately after the procedure call or AUTHORITY-CHECK. Statements such as ENDIF, simple assignments (that do not modify the return value), or comments are permitted before reads. If there is an empty IF/ENDIF block after the procedure call, a problem is registered.

### Potentially security-relevant procedure without registration – 1163###  

Potentially security-relevant procedure without registration

Directly after the call of an authorization check using the statement AUTHORITY-CHECK or after the call of a similar procedure, the result of the call in question must be checked. To do this after the statement AUTHORITY-CHECK, the system field SY-SUBRC must be queried. After a procedure is called, the corresponding output parameter must be queried.

In some cases, the authorization check is made in a procedure A and the value of SY-SUBRC (or a corresponding output parameter) returned to its own caller B. This delegates the job of responding appropriately to a failed authorization check to the the caller B. The procedure A becomes a security-relevant procedure and it must now be ensured that all callers of the procedure A check the returned result (in exactly the way they would if they had called the statement AUTHORITY-CHECK themselves).

In this case, we know that a globally visible procedure delegates the evaluation of the result of an authorization check to its calling programs. Therefore the procedure should be registered as security-relevant in CVA, so that all its calling programs can be checked, to verify that they evaluate the returned result.

**Example**

The function module is a global procedure, which wraps statement AUTHORITY-CHECK. The successful execution check is delegated to the calling program.
```ABAP
FUNCTION Z_AUTHORITY_CHECK.
  ...
*” EXCEPTIONS
*” NO_AUTHORITY

  ...

  AUTHORITY-CHECK OBJECT ‘S_TCODE’
  ID ‘TCD’ FIELD I_TCODE.

  IF SY-SUBRC 0.
    MESSAGE EXXX(XX) RAISING NO_AUTHORITY.
  ENDIF.

ENDFUNCTION. 
```

#### Procedure

First check whether it is necessary to use the global procedure. Switching to a local procedure (for example, from a public to a private method) means that 1163 messages are no longer displayed.

If it is not possible to switch, you should register the procedure as security-relevant using BAdl SLIN\_BADI\_SEC\_PROCEDURES. Once the procedure has been registered as security-relevant, 1163 messages are no longer displayed. Instead, new security messages can now be displayed in all programs that use the registered
If it is not possible to switch, you should register the procedure as security-relevant using BAdl SLIN\_BADI\_SEC\_PROCEDURES. Once the procedure has been registered as security-relevant, 1163 messages are no longer displayed. Instead, new security messages can now be displayed in all programs that use the registered procedure.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.

This check verifies whether the return value (for example, SY-SUBRC) is read immediately after the procedure call or AUTHORITY-CHECK. In the case in question, this return is either reused as the return code of the current procedure or a classic exception is raised dependent on the return code (which creates a new return code). This means that the handling of the value is delegated to the caller of the current procedure.

### Return value (for example, SY-SUBRC) not evaluated after a security-relevant local procedure was called – 1164###  

Return value (for example, SY-SUBRC) not evaluated after a security-relevant local procedure was called

Directly after the call of an authorization check using the statement AUTHORITY-CHECK or after the call of a similar procedure, the result of the call in question must be checked. To do this after the statement AUTHORITY-CHECK, the system field SY-SUBRC must be queried. After a procedure is called, the corresponding output parameter must be queried.

In some cases, the authorization check is made in a procedure A and the value of SY-SUBRC (or a corresponding output parameter) returned to its own caller B. This delegates the job of responding appropriately to a failed authorization check to the the caller B. The procedure A becomes a security-relevant procedure and it must now be ensured that all callers of the procedure A check the returned result (in exactly the way they would if they had called the statement AUTHORITY-CHECK themselves).

#### Procedure

After the procedure call, run the evaluation of the return value with an appropriate response (for example, error message or cancel).

If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.

This check verifies whether the return value (for example, SY-SUBRC) is read immediately after the procedure call or AUTHORITY-CHECK. In the case in question, this return is either reused as the return code of the current procedure or a classic exception is raised dependent on the return code (which creates a new return code). This means that the handling of the value is delegated to the caller of the current procedure.

### Call of a security-relevant function without processing sy-subrc – 1165

After a security-relevant function module such as AUTHORITY\_CHECK\_TCODE or AUTHORITY\_CHECK is called, the return value (for example, SY-SUBRC) must be evaluated.

From the perspective of this check, the following function modules are security-relevant:
- AUTHORITY\_CHECK
- AUTHORITY\_CHECK\_TCODE
- SU\_RAUTH\_CHECK\_FOR\_USER
- VIEW\_AUTHORITY\_CHECK
- FILE\_VALIDATE\_NAME
- AUTHORITY\_CHECK\_DATASET
- AUTHORITY\_CHECK\_RFC

Further procedures can be registered as security-relevant using the BAdI **SLIN\_BADI\_SEC\_PROCEDURES**. All registered security-relevant function modules and methods can be displayed using the report **RSLIN\_SEC\_DISPLAY\_SECREL\_PROC**.

This check checks whether the return value (for example, SY-SUBRC) is read immediately after the procedure call. Statements such as ENDIF, simple assignments (that do not modify the return value), or comments are permitted before reads. Some declarative statements, such as DATA, are also valid. If there is an empty IF/ENDIF block after the method call, a problem is registered.

#### Procedure

Insert an evaluation of the return value (for example, SY-SUBRC) with an appropriate response (for example, error message or cancel).

If the message is caused by an inconsistent **BAdI implementation**, contact the person responsible for the BAdI implementation classes specified in the message details.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Call of system function CALL “SYSTEM” – 1170

Statement CALL “SYSTEM” used

The system function call CALL ‘SYSTEM’ can be used to execute operating system commands. However, calling operating system commands can be a potential security problem. This is a particular risk in cases where end users can modify or manipulate the parameters of an operating system command.

Usually all system function calls (CALL cfunc) are only allowed to be used in system programs and are not allowed to be used in application programs.

#### Procedure

Operating system commands should not be called from ABAP.

If an operating system command call is absolutely necessary, however, the SAPXPG mechanism must be used. This offers increased security due to the following characteristics:
- Abstraction from different operating systems
- Predefined operating system commands
- Stricter handling of parameters
- Allows check modules (such as allow lists) to be defined
- Predefined authorization check

New operating system commands must first be defined using transaction SM69. If possible, omit input values because these can also cause a security problem. The function module SXPG\_CALL\_SYSTEM can be used to make calls. See **System Command Injections** for more information.

All uses of the system function call CALL ‘SYSTEM’ are registered.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### System function call with potential user input on FIELD – 1171

C function call with names as potential user input

The ABAP command CALL cfunc can be used to execute specific C functions in the kernel. However, calling C functions can be a potential security problem.

Potential attackers can call unwanted kernel functions by making unexpected input, and thereby illegally modify the running of the program.

#### Procedure

If possible, avoid calling C functions from ABAP. You can do this by using official APIs (such as function modules and methods).

If this is not possible, check whether it is really necessary to use **dynamic calls** of C functions. One solution is to switch to using static calls. If this is not possible either, the input data must be checked accordingly before being used as C function names.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. CHECK\_WHITELIST\_STR
2. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

If user input is incorporated in the names cfunc, the CALL statement is reported. A **local data flow analysis** is performed to do this.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### AUTHORITY-CHECK for specified User – 1180

AUTHORITY-CHECK with explicit user name

The statement AUTHORITY-CHECK is used together with the addition FOR USER to check the authorization of the user specified by FOR USER (and not for the logged on user). This is often a very sensible requirement.

Example: Data needs to be processed using a specific user in a background job. Before the job is scheduled, AUTHORITY-CHECK FOR USER checks whether the user has the required authorization. In the background job itself, however, the current user should check the authorization again.

If the user name specified using FOR USER has an external source (such as GUI input, an RFC interface, or a file), an attack can manipulate the user name in certain circumstances. In this way, this variant of the statement AUTHORITY-CHECK can be misused to gain additional authorizations that the logged on user is not allowed to have. The addition FOR USER can be used to specify a predefined/delivered user (such as DDDIC or SAP\*), which can then be used in certain circumstances to gain administrative rights.

The function modules AUTHORITY\_CHECK and SU\_RAUTH\_CHECK\_FOR\_USER offer similar options and can also be viewed as a potential source of hazards.

#### Procedure

Try to avoid using the addition FOR USER with the statement AUTHORITY-CHECK . The addition FOR USER sy-uname or FOR USER syst-uname in particular is usually not required. Instead, use the statement AUTHORITY-CHECK without the addition FOR USER. The same applies to the function modules AUTHORITY\_CHECK and SU\_RAUTH\_CHECK\_FOR\_USER.

If an authorization check needs to be run for a user other than the logged on user, it is important to ensure that the user name specified for the addition FOR USER cannot be manipulated externally (by making entries on the user interface, for example). Checks against an allow list can be a good way of verifying user names entered from outside. Request an **exemption** for cases where the addition FOR USER is essential and not just useful.

In the example above (authorization check before scheduling a background job for the user name to be used to run the job), it is important that you perform an authorization check yourself in the background job.

If you want to query the authorization for the user currently logged on locally in RFC calls of the function modules AUTHORITY\_CHECK and SU\_RAUTH\_CHECK\_FOR\_USER, you should obtain the user name using a call of the method cl\_abap\_syst=>get\_user\_name( ) and pass it to the function module. This method, however, still produces a security message (such as “Querying an authorization for a user presents a security risk.”). Request an **exemption** in this case.

All uses of the statement AUTHORITY-CHECK with the addition FOR USER are reported. Here, the source of the user name is not checked. Any allow list checks are ignored.

The local use of the function modules AUTHORITY\_CHECK and SU\_RAUTH\_CHECK\_FOR\_USER is handled in a similar way, since they offer mainly the same functions.

### AUTHORITY-CHECK for SY-UNAME – 1181

AUTHORITY-CHECK with addition FOR USER sy-uname

The statement AUTHORITY-CHECK is used together with the addition FOR USER to check the authorization of the user specified by FOR USER (and not for the logged on user). This is often a very sensible requirement.

It does not make any sense, however, to specify FOR USER sy-uname or FOR USER syst-uname.

If the function modules AUTHORITY\_CHECK and SU\_RAUTH\_CHECK\_FOR\_USER are called, the situation is different. Local calls of these function modules are not a good idea, since the statement AUTHORITY-CHECK is far quicker and produces the same result. In RFC calls of these functions, it is often a requirement to check the authorization for the same user ID as the logged on user in the local system. Here it is advisable to obtain the name of the user logged on locally using the method cl\_abap\_syst=>get\_user\_name( ).

#### Procedure

Do not use the additions FOR USER sy-uname or FOR USER syst-uname in the statement AUTHORITY-CHECK.

Replace the local calls of the function modules AUTHORITY\_CHECK and SU\_RAUTH\_CHECK\_FOR\_USER by the statement AUTHORITY-CHECK without the clause FOR USER.

If you want to pass the user name of the user logged on locally for RFC calls of these function modules, use the method cl\_abap\_syst=>get\_user\_name( ) to obtain the user name. However, you should note that this user name might be assigned to a different person in the called system.

The additions FOR USER sy-uname and FOR USER syst-uname are registered in the statement AUTHORITY-CHECK.

Local calls of the function modules AUTHORITY\_CHECK and SU\_RAUTH\_CHECK\_FOR\_USER produce a message when the user name sy-uname or syst-uname is specified.

### The dynamic WHERE condition allows a potential code injection – 1190

Potential manipulation of the dynamic WHERE condition in an internal table

The dynamic WHERE clause makes it possible for attackers to inject additional conditions, joined using OR or AND, that modify the volume of data selected in unexpected ways. These conditions can also contain functional ABAP method calls.

#### Procedure

First check whether it is necessary to use the dynamic WHERE clause. Switching to a static WHERE clause provides a full solution to the security problem.  If this is not possible, the input data must be checked appropriately before being used in the dynamic clause.

The documentation of the class **CL\_ABAP\_DYN\_PRG** explains how input data needs to be handled when constructing a dynamic WHERE clause.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. ESCAPE\_QUOTES
2. ESCAPE\_QUOTES\_STR
3. QUOTE
4. QUOTE\_STR
5. CHECK\_CHAR\_LITERAL
6. CHECK\_STRING\_LITERAL
7. CHECK\_INT\_VALUE
8. CHECK\_VARIABLE\_NAME
9. CHECK\_COLUMN\_NAME
10. CHECK\_WHITELIST\_STR
11. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

### Potentially missing authorization check in a report– 11A1

Authorization checks should be used to secure reports against being called by unauthorized users.

#### Procedure

Check whether an authorization check exists for this report and, if necessary, add a check at the start of the report. If the report is already being used or has been delivered, a switchable authorization check should be integrated using the method call CL\_SACF=>AUTH\_CHECK\_SPEC( ).

The report code is scanned for specific authorization checks (see below), including its form routines, dynpro modules, function calls, and method calls.

The following authorization checks are found:
- ABAP command: AUTHORITY-CHECK (without the addition FOR USER)
- Function call: CALL FUNCTION ‘AUTHORITY\_CHECK’
- Function call: CALL FUNCTION ‘AUTHORITY\_CHECK\_TCODE’
- Function call: CALL FUNCTION ‘AUTH\_CHECK\_TCODE’
- Method call: CALL METHOD CL\_SACF=>AUTH\_CHECK\_SPEC

Authorization checks against authorization object B\_BUP\_PCPT are ignored.

The system also searches for potentially dangerous statements. These include database access and file access in particular. If a statement like this is found before an authorization check, a message is displayed.

It is also possible to assign an authorization group to the report, in which case an authorization check does not need to be integrated and no message is displayed (if the group is valid). The authorization group can be entered in the properties of the report.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.

### Potentially missing authorization check in an RFC function module – 11A2

Authorization checks should be used to secure RFC function modules against being called by unauthorized users.

#### Procedure

Check whether an authorization check exists for this RFC function module and, if necessary, add a check at the start of the RFC function module. If the RFC function module is already being used or has been delivered, a switchable authorization check should be integrated using the method call CL\_SACF=>AUTH\_CHECK\_SPEC( ).

The RFC function module code is scanned for specific authorization checks (see below), including its form routines, dynpro modules, function calls, and method calls.

The following authorization checks are found:
- ABAP command: AUTHORITY-CHECK
- Function call: CALL FUNCTION ‘AUTHORITY\_CHECK’
- Function call: CALL FUNCTION ‘AUTHORITY\_CHECK\_TCODE’
- Function call: CALL FUNCTION ‘AUTH\_CHECK\_TCODE’
- Method call: CALL METHOD CL\_SACF=>AUTH\_CHECK\_SPEC

Authorization checks against authorization object B\_BUP\_PCPT are ignored.

The system also searches for potentially dangerous statements. These include database access and file access in particular. If a statement like this is found before an authorization check, a message is displayed.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.

### Use of command COMMUNICATION – 11C1

Statement COMMUNICATION used

The statement COMMUNICATION is based on a non-secure communication protocol. Neither this protocol nor the statement COMMUNICATION should be used any longer.

#### Procedure

Use a different communication protocol, such as RFC, instead of the existing code.

All places where the statement COMMUNICATION is used are detected.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Potential SQL injection – 11D1

Potential injection of malicious SQL statements or clauses

In the case in question, external data is injected into an SQL statement that is passed for execution by the database. This could enable potential attackers to gain unauthorized access to the SAP database of the system by making unexpected input. This is known as an **SQL injection**.

#### Procedure

It is important that user input is not injected directly into the SQL statement.  If it is necessary to create the SQL statement based on user input regardless of this, this input must be thoroughly checked beforehand.

The class CL\_ABAP\_DYN\_PRG can be used to implement input checks as described in **Validation by Methods of CL\_ABAP\_DYN\_PRG**. (The individual methods in the class CL\_ABAP\_DYN\_PRG became available in different Support Packages or SAP Notes. SAP Note **1852318** provides an overview of these methods.) In the case in question, the following methods of this class are viewed as sufficient by the automated check (if the RETURNING parameter of the method in question is used in further processing):
1. ESCAPE\_QUOTES
2. QUOTE
3. CHECK\_CHAR\_LITERAL
4. CHECK\_INT\_VALUE
5. CHECK\_VARIABLE\_NAME
6. CHECK\_COLUMN\_NAME
7. CHECK\_TABLE\_OR\_VIEW\_NAME\_STR
8. CHECK\_TABLE\_OR\_VIEW\_NAME\_TAB
9. CHECK\_TABLE\_NAME\_STR
10. CHECK\_TABLE\_NAME\_TAB
11. CHECK\_WHITELIST\_STR
12. CHECK\_WHITELIST\_TAB

Secure data sources can also be displayed using the report **RSLIN\_SEC\_DISPLAY\_BADIS**.

Performs a **local data flow analysis**.

If a source code position is flagged and does not present a security problem and an input check or escape action (for example, using a method from CL\_ABAP\_DYN\_PRG) is not appropriate, an **exemption** should be requested in ATC.

**Missing content check during HTTP upload -11F1**

Method IF\_HTTP\_REQUEST~GET\_DATA( ) was called without parameter VSCAN\_SCAN\_ALWAYS.

In certain cases, this could lead to **Cross-Site-Scripting (XSS[)**](https://blogs.sap.com/2017/01/19/code-vulnerability-analyzer-checks/ABEN.XSS_SCRTY)** via file upload (MIME sniffing). For more information, see the “SAP NetWeaver Security Guide” (“Network Services” -> “Preventing Cross-Site Scripting From Uploads”).

#### Procedure

Method IF\_HTTP\_REQUEST~GET\_DATA( ) should be called with parameter VSCAN\_SCAN\_ALWAYS set (see Note **1883929**).

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

Calls of methods IF\_HTTP\_REQUEST~GET\_DATA( ) and IF\_HTTP\_ENTITY~GET\_DATA( ) without parameter VSCAN\_SCAN\_ALWAYS are reported.

**See Also**

“XSS Secure Programming Guide”. SAP Note **1714836**.

SAP Note **1693981**.

### Direct read access to critical database tables – 11G0

Read on sensitive database tables

A sensitive database table was read in a customer system.

These can be SAP tables (if not in the list of database tables registered as harmless).

These can also be all database tables registered as sensitive tables in the list.

Database tables can be registered as sensitive or harmless using the BAdI SLIN\_BADI\_DBTAB\_ACCESS (see the **documentation**).

#### Procedure

These database tables must not be accessed.

### Write access to sensitive database tables – 11G1

Write on sensitive database tables

A write was performed on a sensitive database table in a customer system.

These can be SAP tables (if not in the list of database tables registered as harmless).

These can also be all database tables registered as sensitive tables in the list.

Database tables can be registered as sensitive or harmless using the BAdI SLIN\_BADI\_DBTAB\_ACCESS (see the **documentation**).

#### Procedure

These database tables must not be accessed.

### Hard-coded password – 11K1

A method or function was called with a hard coded password. This is a possible indication, for example, that a user or an RFC destination exists in the system with logon data from the source code.

**Example**

CALL FUNCTION ‘BAPI\_USER\_CREATE1’   EXPORTING     USERNAME  = ‘USER1’     PASSWORD  = **‘Abcd1234’**     …

#### Procedure

Verify that the hard coded password is necessary and, if possible, stop using the hard coded value.

The check does not return a message in the following cases:
- The password comes from user input.
- The password is from the database.
- The password was generated, for example using the function SUSR\_GENERATE\_PASSWORD. Some APIs, such as BAPI\_USER\_CREATE1, return a generated password when a user is created.

All method calls and function calls are logged in which a password-relevant IMPORTING CHANGING parameter has a hard coded value. A parameter is password-relevant if it follows a particular naming convention.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Field with hard coded password – 11K2###  

A variable or constant with a hard coded password exists. This is a possible indication, for example, that a user or an RFC destination exists in the system with logon data from the source code.

**Example**

FUNCTION ‘GET\_USER\_PASSWORD’
\*”————————————–
\*”\*”Local Interface:
\*” EXPORTING
\*” VALUE(EV\_PASSWORD) TYPE STRING
\*”—————————————

EV\_PASSWORD = ‘Abcd1234’

ENDFUNCTION. 

#### Procedure

Verify that the hard coded password is necessary and, if possible, stop using the hard coded value.

The check does not return a message in the following cases:
- The password comes from user input.
- The password is from the database.
- The password was generated, for example using the function SUSR\_GENERATE\_PASSWORD.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.

All password-relevant fields that have a hard-coded value are reported. A field is a password-relevant field if it follows a specific naming convention or has a specific type.

### Field with Base64-encoded password – 11K3###  

There is a variable with a Base64-encoded password. This can be an indication that the password was not sent or saved securely.

**Example**

``L_ENCRYPTED_PASSWORD = CL_HTTP_UTILITY=>ENCODE_BASE64( I_PASSWORD ).``

#### Procedure

Do not use Base64 as the encryption algorithm. Base64 encodes and does not encrypt. If possible, use the default methods for authentication and saving passwords.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an exemption should be requested in ATC.

All Base64-encoded password-relevant fields are highlighted. A field is a password-relevant field if it follows a specific naming convention or has a specific type.

The following Base64 encoding APIs are analyzed:

CL\_HTTP\_UTILITY=>ENCODE\_BASE64 

CL\_HTTP\_UTILITY=>ENCODE\_X\_BASE64 

SSFC\_BASE64\_ENCODE 

### Open URL redirect – 11P1

Potential Unvalidated URL Redirect

In the case in question, external data from an HTTP request (such as URL parameters or HTML form fields) is used as a source for URL redirect, or the data is used to create an HTTP Header with dynamic field names. This makes it possible for attackers to redirect victims to other websites. These cases are known as **unvalidated URL redirects**.

**Example**

It is important to differentiate between the following two cases:
**1. URL redirect API call**
```ABAP
DATA RESPONSE TYPE REF TO IF_HTTP_RESPONSE.
...
RESPONSE->REDIRECT( LV_URL ).
RESPONSE->SET_HEADER_FIELD( NAME = ‘LOCATION’ VALUE = LV_URL ).
RESPONSE->SET_HEADER_FIELD( NAME = ‘REFRESH’ VALUE = LV_URL ).
```

**2. Call of IF\_HTTP\_RESPONSE~SET\_HEADER\_FIELDS or SET\_HEADER\_FIELD with dynamic HTTP Header field names**
```ABAP
RESPONSE->SET_HEADER_FIELD( NAME = LV_NAME VALUE = LV_URL ).
RESPONSE->SET_HEADER_FIELDS( FIELDS = LT_HTTPNVP_TAB ).
```

#### Procedure

##### URL redirect API call

It is first necessary to check whether URL redirects by URL destination can be avoided by user input. If this is not possible, the input data should be checked via an allow list before being used in HTTP redirects.

In this case, the automated check views the allow list method CL\_HTTP\_UTILITY=>CHECK\_HTTP\_WHITELIST, with a corresponding ENTRY\_TYPE (for example, **99** – Generic Redirect), as sufficient, if the RETURNING parameter of the method is used for further processing.

This method is provided in support packages or correction instructions (see SAP Note **2189853**).

##### SET\_HEADER\_FIELDS or SET\_HEADER\_FIELD with dynamic HTTP Header field names

In this case, the automatic check cannot estimate whether the call with dynamic field names will result in a URL redirect. Therefore, you should check whether SET\_HEADER\_FIELDS or SET\_HEADER\_FIELD with dynamic fields can be avoided. If this is not possible, you should apply for an exemption in ATC.

A **local data flow analysis** is performed.

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### The BSP application is not protected against XSRF – 11RF

The BSP application is not protected against cross-site request forgeries (XSRF).

#### Procedure

Select the “XSRF protection” checkbox for the BSP application.

If the place in question is not a security risk (Public BSP, has its own XSRF protection), apply for an **exemption** in ATC.

### Hard-coded host name – 11S1

Hard-coded host name, possibly from forgotten test code or an indication of a back door

Host name queries in ABAP indicate security problems. Host-specific code often presents a back door for attackers.

#### Procedure

Check whether the host name query could possibly indicate a back door. Remove these back doors.

Remove any host-specific code that is not required to run the program.

Some applications cases require host-specific code. However, there is currently no method of detecting these code sections and excluding them from the checks.

Check whether suitable customizing or a suitable API could help to apply distinct behavior to individual hosts in these cases.

All occurrences are reported in which the current host name from the system fields SY-HOST and SYST-HOST or from the call of CL\_ABAP\_SYST=>GET\_HOST\_NAME( ) is compared with a fixed value. To do this, it views the logical condition of the predefined functions BOOLC or BOOLX and the following statements:
1. IF, ELSEIF
2. CASE, WHEN
3. CHECK
4. ASSERT

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Hard-coded system ID – 11S2

Hard-coded system ID, possibly from forgotten test code or an indication of a back door

System ID queries in ABAP indicate security problems. System-specific code often presents a back door for attackers.

#### Procedure

Check whether the system ID query could possibly indicate a back door. Remove these back doors.

Remove any system-specific code that is not required to run the program.

Some applications cases require system-specific code. However, there is currently no method of detecting these code sections and excluding them from the checks.

Check whether suitable customizing or a suitable API could help to apply distinct behavior to individual systems in these cases.

All occurrences are reported in which the current system ID from the system fields SY-SYSID and SYST-SYSID are compared with a fixed value. To do this, it views the logical condition of the predefined functions BOOLC or BOOLX and the following statements:
1. IF, ELSEIF
2. CASE, WHEN
3. CHECK
4. ASSERT

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Hard-coded client – 11S3

Hard-coded client from forgotten test Code or that could indicate a back door.

Client queries in ABAP indicate security problems. Client-specific code often presents a back door for attackers.

#### Procedure

Check whether the client number query could possibly indicate a back door. Remove any back doors.

Remove any client-dependent code that is not required to run the program.

Some applications cases require client-specific code. However, there is currently no method of detecting these code sections and excluding them from the checks.

Check whether suitable customizing or a suitable API could help to apply distinct behavior to individual clients in these cases.

All occurrences are reported in which the current client from the system fields SY-MANDT and SYST-MANDT or from the call of

CL\_ABAP\_SYST=>GET\_CLIENT( ) is compared with a fixed value. To do this, it views the logical condition of the predefined functions BOOLC or BOOLX and the following statements:
1. IF, ELSEIF
2. CASE, WHEN
3. CHECK
4. ASSERT

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### Comparison of a specific registered system variable with a fixed value – 11S4

System variable compared with a hard-coded value from forgotten test code or that could indicate a back door

A comparison between one or more registered system variables and a hard-coded value indicates a security problem, since the code dependent on these system variables could be a back door.

#### Procedure

Check whether the system variable query could possibly indicate a back door. Remove these back doors.

Remove any code dependent on these system variables that is not required to run the program.

Some application cases require code dependent on these system variables. However, there is currently no method of detecting these code sections and excluding them from the checks.

Check whether suitable customizing or a suitable API could help to apply distinct behavior in these cases, depending on the registered system variables.

The system variables in question were registered using the BAdI SLIN\_BADI\_SEC\_BACKDOOR as described in the **documentation**.

All occurrences are reported in which the registered system variables from the system structures SY and SYST are compared with a fixed value. To do this, it views the logical condition of the predefined functions BOOLC or BOOLX and the following statements:
1. IF, ELSEIF
2. CASE, WHEN
3. CHECK
4. ASSERT

If the source code position in question does not have any security problems and there is no point in modifying the source code, an **exemption** should be requested in ATC.

### FAQs

#### Security strategy and positioning of CVA

Q: What does “shift testing to the left” mean?
A: Test earlier. The earlier you identify and resolve security vulnerabilities the more damage you avoid and the cheaper it is in the long run. Resolving vulnerabilities after deployment, for example after a penetration test or a cyber attack is far more expensive.

Q: Developers test software so why don’t they also test for security?
A: Because they don’t have a tool.

Q: How can we compare static and dynamic security testing?
A: We recommend customers do both static and dynamic security testing. There might be situations in which some security vulnerabilities in Web-based applications can only be identified when the application is running. As this is late in the development lifecycle, we recommend you start early with static security testing to identify and fix most of the security vulnerabilities and to keep the list of findings once the application is fully running to a minimum. The later we find security vulnerabilities the more expensive it is to fix them. Therefore, static and dynamic security testing is the best combination -static to keep costs down and dynamic to make security testing complete.

>Q: How are static and dynamic testing connected?
>A: A static vulnerability might also crop up as a dynamic vulnerability. It may well be that it looks worse as a dynamic vulnerability than it did as a static one. The combination of static and dynamic = defense in depth.

>Q: What is SAP’s official recommendation for scanning ABAP custom code?
>A: SAP Code Vulnerability Analyzer. The material code for the Suite and S/4 is 7019502.

>Q: There are partner products and other 3rd party products that also scan ABAP custom code. Why choose CVA?
>A: CVA is the recommended product for scanning ABAP custom code for several reasons:
>-   CVA is an ABAP product and is part of ABAP Test Cockpit which is part of NetWeaver. It is not an add-on; it is written in ABAP and is deeply integrated in the SAP landscape unlike alternative products.
>-   SAP uses CVA to scan its own ABAP code.
>-   ABAP is an SAP language and the ABAP language developers work alongside the CVA developers.
>-   To update CVA all you have to do is install the latest SP on your central system (see below for information about a central scanning system). You don’t have to engage in long-winded and error-prone update procedures as with alternative products.

>Q: When was CVA first released as a product?
>A: CVA has been around since 2014.

>Q: How many checks are there?
>A: Around 70, but the number of checks is not significant. The important thing is to have the right checks.

>Q: How often does SAP check its coding?
>A: Once a week, both old and new coding, including when we transport coding.

>Q: Is there any 3rd party software in CVA?
>A: No, there is no 3rd party software in CVA.

>Q: Can customers scan SAP coding with CVA or just their own custom code?
>A: Customers can scan their own code and 3rd party add-ons but not SAP code. This is restricted via namespaces.

>Q: What is the relationship between CVA and ATC?
>A: ATC provides a technical check infrastructure and functional and performance checks. CVA provides security checks based on ATC.

>Q: What is the difference between CVA checks and Code Inspector checks?
>A: The security checks that Code Inspector offers are very superficial. CVA carries out a dataflow analysis which reduces the number of false positives.

>Q: What does CVA offer over and above the Code Inspector (CI) checks?
>A: CVA differs from CI in the following respects:
>- CVA focuses specifically on identifying and managing security vulnerabilities.
>- CVA provides CVA-specific support in case of problems.
>- CI constitutes a general code scanning tool.
>- CI allows developer to suppress issues in the code without applying for an exemption.
>- CI allows developer to create additional custom checks.
>- CI Does not provide vulnerability-specific support.

>Q: Is the transition to S/4 a problem for CVA?
>A: No, because CVA scans static code. It does not analyze business processes.

>Q: How do you start a CVA scan in SAP Cloud Platform ABAP Environment (Steampunk)?
>A: Find your Steampunk system in Eclipse and execute a scan using the packages of the application. Use the variant SAP_CLOUD_PLATFORM_DEFAULT.

#### What can be scanned

>Q: What types of objects are checked?
>A: Programs, function groups, class pools, BSPs, Adobe Forms, Smart Forms.

>Q: How many levels deep does CVA scan code?
>A: The dataflow analysis scans through all call levels within a  **compilation unit**  (program, global class, function group). In some checks we don’t have to analyze a data flow but do other searches in several call levels. Most of these checks follow the call graph across the boundaries of compilation units. It scans the specified entity. For example, if you scan a report but this report is called by a function module then CVA will not scan the function module.

>Q: Why are there BSP checks?
>A: ABAP coding can be embedded in BSP pages. The ABAP code is then part of the generated ABAP coding. The BSP checks check this generated ABAP code. There are also a couple of checks that check BSP attributes.

>Q: Do we have any limitations in BW Systems? Is there anything we should take in account?
>A: No.

>Q: Can we scan generated code, for example, code that Web Dynpro generates?
>A: Yes, there is a checkbox to specify this.

>Q: Does CVA scan SAPscript?
>A: No.

>Q: Does CVA scan Smart Forms and Adobe Forms?
>A: Yes. Smart Forms are scanned as of SAP_BASIS 7.52 SP 01 or 7.53 SP 0 (note 2534180). SAP Interactive Forms by Adobe are scanned as of NW 7.52 SP 03 or 7.53 SP 0 (note 2629856).

>Q: Can CVA scan modifications?
>A: Yes, CVA can scan modifications (from NetWeaver 7.52 onwards), enhancements, user exits and generated code that contains custom code.

>Q: Are custom BSP pages and event handlers scanned?
>A: Yes, this is available with NW 7.50.

>Q: How are customer enhancements dealt with?
>A: Customer enhancements are located in a customer package (or in a customer namespace). This means that findings in enhancements (or findings through which the data flows) are also reported to the customer.

>Q: Can we scan foreign namespaces (fremde Namensräume)?
>A: Yes, but not SAP coding (apart from modifications, enhancements and user exits).

>Q: Can CVA test hidden code?
>A: Hidden code is code in the database which the compiler can execute but which CVA cannot access. The ability to hide code in the database was available in old releases but has now been removed via a security note on all relevant Suite releases.

>Q: Can we scan 3rd party coding that is in a different namespace?
>A: Yes.

>Q: Can we analyze partner code?
>A: Yes.

>Q: Can I select which checks are executed?
>A: You can change the priorities of individual checks and thus suppress findings.

#### Licensing

>Q: How does licensing work?
>A: This is user-based licensing. It is capped at 100 users, so a customer only pays for the first 100 users. A user is anyone who either triggers a CVA run or who uses the results of a CVA run. So, an employee who receives a list of security vulnerabilities from a CVA run is also a user.
>Scenario: I scan my ABAP coding using CVA, so I am a CVA user. I paste the check results into a Word document or into an Excel and send it to my quality manager who reviews the check results. He is now also a CVA user. He forwards it to another developer to resolve the vulnerabilities listed in the check results. That developer is now also a CVA user.

>Q: How does the customer know that he has a license?
>A: They can ask the account executive.

>Q: If a customer employs freelancers on a project basis does the customer have to license each and every freelancer or can the customer just license the maximum number of freelancers working at the company at any one time?
>A: To be fair the customer just licenses generically so that the maximum number of freelancers working at the company at any one time has to be licensed.

>Q: Do customers also need an SAP user as well as a CVA license?
>A: Yes.

>Q: If a customer uses a central system on HEC can they also use it for the Business Suite?
>A: Yes, because it is user-based licensing.

>Q: Can a customer activate CVA even if they don’t have a license?
>A: Technically yes, but a warning popup is displayed.

>Q: Can a company prevent certain developers from using CVA to reduce the number of users and reduce licensing costs?
>A: This would be a false economy because all developers should check their code using CVA.

>Q: Is it possible to arrange a POC (proof-of-concept) so that we can persuade management to buy a CVA license?
>A: Yes, a POC can be arranged with a consultant who will use CVA can scan a part of your ABAP custom code. This will give you an impression of the sorts of vulnerabilities lurking in the rest of your system. If you are interested in a POC please contact your account executive.

#### Releases

>Q: How often are there releases in the cloud and on-premise?
>A: Four times a year in the cloud, once a year on-premise.

>Q: New checks are delivered with the latest SP. How often are there new SPs?
>A: That depends on the release. On old releases about once a year. On S/4 once a quarter.

>Q: How long does it take to create a new check?
>A: Weeks or months. Not days, and not years. SAP delivers it with the next SP. However, bugs are fixed using patches.

>Q: Is there a notification for the customer when a new check is released?
>A: No, but there is a release note listing new checks per release.

>Q: Customers do not get NW 7.53 standalone so how do they get the new checks if they are still on NW 7.52?
>A: SAP downports new checks to NW 7.52.

>Q: Are findings and product release necessarily linked?
>A: No, a customer can release a product even if there are unresolved findings.

#### Setting up and running CVA

>Q: How long does it take to install and set up CVA?
>A: It depends on the landscape but for someone who knows what they are doing about half a day. You have to connect the local systems and so on.

>Q: Which systems do you scan – DEV, QM or PROD?
>A: Generally, the development system or the QM system (it combines development objects, and this can lead to new findings.) but rarely the production system.

>Q: Can you specify that CVA scans the whole system and if so, how long would this take?
>A: Yes. If it is the whole SAP system, it may take a weekend. If it is just a program it may only take a few seconds.

>Q: If scanning the whole system takes too long is there another approach the customer can adopt?
>A: They could go through it package by package.

>Q: How can CVA be implemented at a company without being rejected by developers who are busy with other tasks?
>A: Implementing CVA at a company can often encounter resistance because resolving security vulnerabilities involves extra work. One should adopt a phased approach so that its introduction is acceptable to developers. Here are some recommendations:
>- Gradually extend the scope of what is checked.
>- First, just introduce CVA to a pilot group of developers.
>- Make use of the baseline function to temporarily suppress less critical messages.
>- Take account of the different priorities. For example, hide all priority 3 messages which (in the standard) do not prevent a transport. That will allow developers to focus on what really matters.

>Q: Which variant do I have to select in ATC in order to trigger a CVA scan? Where is this in ATC?
>A: If you select the variant SLIN_SEC then only CVA runs. Or you can create your own variant in Code Inspector and select more than just the CVA checks. If CVA has been activated, then you will find the CVA checks in the category “Security checks”.

>Q: What exactly is SLIN_SEC?
>A: It is a Code Inspector variant.

>Q: How can the number of objects to be checked be restricted?
>A: The object quantity (Objektmenge) is defined in Code Inspector.

>Q: Can you activate / deactivate individual checks?
>A: To a certain extent "yes". You can turn CVA / BSP checks on and off using a Code Inspector variant. That turns on all the CVA checks. Then you can turn off individual checks by specifying the priority “No Message”. However, this is a very roundabout way of doing things. Please bear in mind that setting priorities is effective for all check variants. So, you can’t activate a check abc in variant V1 and deactivate it in variant V2. At least, you would have to turn it on or off in the relevant variant before each check run which is very impractical.

>Q: CVA runs in Eclipse but does a customer have to use Eclipse or can they run CVA in SAP GUI?
>A: They can use SAP GUI instead if they prefer.

>Q: How can you run checks in the background?
>A: Transaction ATC -> (Select a run and schedule it using the button “Schedule”) -> Program -> Execute in background.

>Q: What is the difference between the following two options in Eclipse: RMB on program -> Run As -> "ABAP Test Cockpit" and "ABAP Test Cockpit With…"?
>A: With "ABAP Test Cockpit“ the check run is executed directly. Those checks are executed that are specified in the check variant with the name DEFAULT.  
With "ABAP Test Cockpit With…" you first have the option of making various settings: Add additional objects to be checked, select a different check variant, …To trigger the check run, press F8

#### Dataflow

>Q: What is a dataflow analysis?
>A: Dataflow analysis involves the way data is transferred and processed within a program. The data is tracked through the coding without executing it.

>Q: Dataflow: How many applications does it go through?
>A: Only one; it does not cross compilation units. It stays within, say, a function group, for example; it does not jump between classes, programs and function groups.
>- It can check all the methods of a global class.
>- If an input parameter is used in a report that calls a function module then any vulnerability is only found in the report if that is what you are scanning. To find any vulnerability in the function module you need to scan the function module.
>- If a class has an include then that include is also scanned.

#### Dealing with findings

>Q: When a company does a CVA run for the first time how many findings are likely to be generated?
>A: Quite possibly thousands.

>Q: What options do customers have to visualize the check results?
>A: Once the customer has scanned the ABAP coding the check results are displayed in ATC (ABAP Test Cockpit).  
The check results can also be extracted to Solution Manager and visualized there with the tools available.  
Also, notification e-mails can be requested when scheduling runs in ATC: Schedule Runs -> Mark your run -> Schedule -> See the last box “Notification” -> Mark “Send Notifications to Contacts”.

>Q: Does a finding have an identifiable ID?
>A: Yes, it consists of a hash value for the code line + 4 statements before and after. The ID remains the same even if the coding is moved.

>Q: Which vulnerabilities should one start to fix first?
>A: Here are a few hints:
>- Start with hard-coded user names. These are important and easy to fix.
>- Then go for SQL-injections.
>- After that fix directory traversals.

#### Reporting

>Q: Can the list of findings be exported into a Word document?
>A: Yes.

>Q: Can the quality manager only get a list of vulnerabilities in the form of an Excel / Word document?
>A: No, the quality manager can see them in the system. Excel / Word is just an additional option.

#### Solution Manager

>Q: Why shouldn’t CVA and Solution Manager run on the same system?
>A: Because of the minimum ABAP release. CVA on a central system requires at least NW 7.51. Solution Manager 7.2 SP06 runs on NW 7.40 and no higher.

>Q: Frequently used coding is more critical than coding that is rarely used. How can you tell how often coding is used?
>A: You can see this in Solution Manager.

>Q: Can you resolve the security vulnerabilities from within ChaRM?
>A: No.

>Q: Can you create exemptions from within ChaRM?
>A: No.

>Q: Can ChaRM be used to release transports?
>A: Yes, you release the task (Aufgabe) or transport in ChaRM. You can also specify that transports only take place if the CVA vulnerabilities have been fixed independently of ChaRM.

>Q: Do customers have to resolve vulnerabilities in coding if the coding is never used?
>A: Customers can use Solution Manager to identify coding that is never used. They can then just deactivate the coding instead of resolving the vulnerabilities.

## Fortify SCA

The following is an overview of what would typically be required related to the Fortify SCA tool. Any results would likely require a security expert to analyze first. Follow project's specific process (if any).

### Fortify Static Code Analyzer

Fortify Static Code Analyzer is a set of software security analyzers that search for violations of security specific coding rules and guidelines in a variety of languages. The Fortify Static Code Analyzer language technology provides rich data that enables the analyzers to pinpoint and prioritize violations so that fixes are fast and accurate.

Fortify Static Code Analyzer produces analysis information to help you deliver more secure software, as well as make security code reviews more efficient, consistent, and complete. Its design enables you to quickly incorporate new third-party and customer-specific security rules.

At the highest level, using Fortify Static Code Analyzer involves the following:
- Running Fortify Static Code Analyzer as a stand-alone process or integrating Fortify Static Code Analyzer in a build tool
- Translating the source code into an intermediate translated format
- Scanning the translated code and producing security vulnerability reports
- Auditing the results of the scan, either by opening the results (FPR file) in Fortify
- Audit Workbench or uploading them to Fortify Software Security Center for analysis, or directly with the results displayed on screen.

### Prerequisites

#### Developer

- ITsec Training 2a and 3
- Raise a ticket in Request IT for Eclipse installation
- Raise a ticket in Request IT for Fortify installation

#### Adding Fortify Static Code Analyzer to Your Favorites List

Adding Fortify Static Code Analyzer to your Favorites list is optional, but doing so can make it quicker to access and launch Fortify Static Code Analyzer scans. The following steps assume that you use the user menu in your day-to-day work. If your work is done from a different menu, add the Favorites link to the menu that you use.

Before you create the Fortify Static Code Analyzer entry, make sure that the SAP server is running and you are in the SAP Easy Access area of your web-based client.
1. From the **SAP Easy Access** menu, type S000 into the transaction box.
The **SAP Menu** opens.
2. Right-click the **Favorites** folder and select **Insert transaction**.
The **Manual entry of a transaction** dialog box opens.
3. Type YSCA into the **Transaction Code** box.
4. Click the green check mark button.
The Extract ABAP code and launch SCA item appears in the Favorites list.
5. Click the Extract ABAP code and launch SCA link to launch the Fortify ABAP Extractor.

### Step-by-Step Description

After installation of the Fortify extractor, Eclipse editor and Fortify application, you can start with your security source code scanning. In the next chapters you will read, step-by-step, what you have to do for security source code scanning.

#### Using SAP ABAP Extractor

##### Running the Fortify ABAP Extractor

To run the Fortify ABAP Extractor,:
1. Start the program from the Favorites link, the transaction code, or manually start the YHP_FORTIFY_SCA object.
2. Provide the requested information.
	- To extract program source from program, e.g. ZCQ, enter ZCQ* under the **_Objects section_** in Program input field. You can also use e.g. package or the other fields from the objects section.
	- Fill in the **_Source analyzer parameters_** for
		- Working Directory – to define the directory where the extracted source code will be stored. **( TBD will be an Central one** )
		- Maximum Call-Chain Depth – this defines the level of recursive includes to follow during extraction of the object selection under the Objects section. Start with 1 or 2 here.
The higher this parameter is set the longer the extraction will take. If the parameter is too small, the extractor will not capture all individual sources. If an application source file with a size of 0 appears in the Working Directory, you must set the maximum call-chain depth one higher. A value set to 4 may takes 30 minutes to extract, level 5 takes approximately half a day, depending on the machine and network connection.
	- Fill in the Actions section:
		- Download checkbox selected
		- Create ZIP file checkbox selected
3. Click Execute or F8.
A list would appear and files would be created and visible in Explorer.

##### Process
1. Project Manager order Placeholder for Fortify via DTSS in ITS1
2. Project Manager initiate staffing for SAP Securtiy Engineer
3. After assignment SAP Security Engineer to Project, developer start download
Develop coding and sent zip file to the assigned SAP Security Engineer.
4. SAP Security Engineer start the evaluation/review from the coding and sent
The result report back to the PM/Developer.
