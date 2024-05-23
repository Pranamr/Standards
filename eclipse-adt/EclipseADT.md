# Eclipse IDE Installation Guide
Reference: https://tools.hana.ondemand.com/#abap

## Connection
Use **Zscaler** app to connect to DPDHL network (instead of Cisco VPN) to allow downloading of plugins from official Eclipse, SAP, and abapGit repositories without network connection errors. While it is preferred to use the **ZScaler** app, the remote connection functionality may not be rolled out to your machine at this time.

In the event that it is not feasible to use **Zscaler** app (i.e. you're using **Cisco VPN**), you may need to disconnect the **Cisco VPN** connection first to get the tools from SAP.

Once installed, you will need to connect back to DPDHL network so that you can access the internal systems.

Additionally, if you use **Cisco VPN** from outside of Europe and you can't connect to FTS, you may need to reconnect using the European Cisco VPN node (cvpn-eu) to enable access to FTS.

## Java Runtime Engine (JRE)
This should no longer be needed as more recent Eclipse installation packages have bundled JRE.
1. ~~Go to https://sap.github.io/SapMachine/ in a web browser.~~
2. ~~Choose: **SapMachine 11 (Long Term Support)**, **JRE**, **Windows x64**, and the latest version available.~~

## Eclipse IDE
1. Download latest version of **Eclipse IDE for Java Developers** as indicated in the above [Reference](#installation-of-eclipse-ide). Choose **Windows x86_64**.
2. Install **Eclipse** by unzipping the downloaded zip file to a local location, e.g. ``C:\eclipse``. Create the folder as necessary.
3. **[OPTIONAL -- only if JRE is installed separately]** Install **JRE** by unzipping the downloaded zip file from **Java Runtime Engine** section, to folder ``jre`` in the local location in the previous step, e.g. ``C:\eclipse\jre``. Create the folder as necessary.
4. Optionally, create a shortcut for *eclipse.exe* in the Start menu and/or desktop for easier access.

## Repository Connection
You may want to try using the [ADT for the Impatient guide](https://confluence.dhl.com/pages/viewpage.action?spaceKey=SPLUS&title=ADT+for+the+impatient) by Armin Baumgarten first, before attempting the _Advanced Repository Connection_ steps mentioned below.

Basically, the following are needed:
- Set the SSL trust store of the JVM by setting the following in the ``eclipse.ini`` in the installation folder of Eclipse, under the ``-vmargs`` line:
```
-Djavax.net.ssl.trustStore=NUL
-Djavax.net.ssl.trustStoreType=Windows-ROOT
```
- Set the _General > Network Connections_ setting to use DHL proxy, by setting the _Active Provider_ as Manual, and set ``cloudproxy.dhl.com`` port ``10123`` as the proxy, along with your LDAP credentials.

## Advanced Repository Connection
While Eclipse package may be downloadable and some repository sites may be reachable, some repository sites may still not be fully load-able in Eclipse due to the trust certificates being replaced with the company's internal trust certificates. The symptom of this is when such a repository site is reloaded in the _Preferences > Install/Updates > Available Software Sites_, it will give a "No repository found", even though the site is accessible via a regular web browser.

If the method mentioned in the section before does not work, then a more advanced setup may be tried. This may not be 100% do-able, but it has been tested to be able to access the base repository sites that are necessary.

### Download the Zscaler CA root certificate
1. Navigate to the repository site via a regular web browser, e.g. Microsoft Edge, Google Chrome, or Mozilla Firefox.
2. Click on the Lock icon in the address bar.
3. Select _Connection is secure_.
4. Select _Certificate is valid_ or the _Show certificate_ icon.
5. Select _Details_ tab.
6. Click _Export..._ button.
7. Select a folder, filename, and type, e.g. folder ``C:\Temp``, filename ``zscaler``, type _DER-encoded binary, single certificate_.
8. Click _Save_.

### Adding Zscaler CA root certificate to Eclipse JRE keystore
1. Close Eclipse if it's currently open.
2. Open the _Command Prompt_ or _Terminal_ application.
3. Navigate to ``lib\bin`` directory of the JRE being used for Eclipse. The JRE ``bin`` directory itself may be viewed from the _eclipse.ini_ file (viewable via a text editor), under the _-vm_ line, e.g. ``plugins/org.eclipse.justj.openjdk.hotspot.jre.full.win32.x86_64_17.0.8.v20230831-1047/jre/bin``. In the example, assuming that Eclipse is installed to ``C:\eclipse``, then the path would be ``C:\eclipse\plugins\org.eclipse.justj.openjdk.hotspot.jre.full.win32.x86_64_17.0.8.v20230831-1047\jre\lib\bin``.
4. Execute the following command: ``keytool.exe -import -trustcacerts -file "C:\Temp\zscaler.der" -keystore ..\lib\security\cacerts``. This assumes the Zscaler CA root certificate downloaded before was downloaded to ``C:\Temp`` folder with the name ``zscaler.der``. Adjust as necessary.
5. If prompted for password, the default is searchable on the internet, e.g. search for _"jre default keystore password"_.
6. If prompted, type ``yes``.

### Access previously non-load-able repository sites
1. In Eclipse, for sites that were previously unload-able, try to _Reload_ again.
2. If there is still the same error message, open the site in the web browser, e.g. https://download.eclipse.org/releases/2023-09/
3. The site may list additional subdirectories that are version-specific, e.g. 202309131000. Click to navigate into that subdirectory.
4. Use the resulting URL as a new repository site in Eclipse, e.g. https://download.eclipse.org/releases/2023-09/202309131000/.
5. Test by performing _Reload_ again.
6. If it works, then this site may be used instead and the older site that is not load-able may be unticked.
7. In any case, the repository site that does not work may be set to unticked so as not to be checked.

## ABAP Development Tools (ADT)
 1. Run **Eclipse**.
 2. Go to **Help > Install New Software...** menu.
 3. Put in https://tools.hana.ondemand.com/latest in the **Work with** box and press **Enter**.
 4. Tick all checkboxes at the bottom part.
 5. Tick all checkboxes at the middle part. There are 4 items expected: **ABAP Development Tools**, **Modeling Tools for SAP BW/4HANA and SAP BW powered by SAP HANA**, **SAP Cloud Business Application Tools**, and **SAP HANA Tools**.
 6. Click **Next**.
 7. Overview of what will be installed. There should be no updated components at this stage. Click **Next**.
 8. Select **I accept the terms of the license agreements**. Click **Finish**.
 9. The download and installation progress bar is located at the lower right corner.
 10. Once installation has been completed, a prompt will appear to have **Eclipse** restarted. Click **Restart Now**. 
 11. Once **Eclipse** has been restarted, go to **Window > Perspective > Open Perspective > Other...** menu.
 12. Select **ABAP** from the list. Click **Open**.
 13. In the **Project Explorer** panel located by default to the left, new application servers can be added by using the context menu (i.e. right-clicking), then selecting **New > Project...** in the menu. Select [**ABAP Project**](#abap-project) for on-premise application servers, or [**ABAP Cloud Project**](#abap-cloud-project) for SAP BTP-based instances. Click **Next** and follow instructions until end.

### ABAP Project
 1. Available entries are those registered in **SAP GUI**. Use search bar to select entries. If available and accessible, use SSO. Click **Next**.
 2. Verify and/or change the entries if necessary. If SSO is available, ensure **Single Sign-On** is set to **Enabled**. Click Next.
 3. Verify credentials as necessary. Enter password if not SSO-based. Click **Next** to test.
 4. Verify the information. Click **Finish** to complete the process.

### ABAP Cloud Project
1. Obtain the **Service Key in JSON Format** for the intended ABAP environment in SAP BTP.
2. Select **Service Key**. Click **Next**.
3. Copy-paste from clipboard or import the **Service Key in JSON Format**. Click **Next**.
4. You may be required to logon via web browser first. Use any of the options to do so.
5. Once you have logged on successfully via web browser, return back to **Eclipse**. The process will continue.
6. Verify the information. Click **Finish** to complete the process.

## abapGit
 1. Run **Eclipse**.
 2. Go to **Help > Install New Software...** menu.
 3. Put in https://eclipse.abapgit.org/updatesite/ in the **Work with** box and press **Enter**.
 4. Tick all checkboxes at the bottom part.
 5. Tick all checkboxes at the middle part. There is 1 item expected: **abapGit for ABAP Development Tools (ADT)**.
 6. Click **Next** and follow the instructions. Click **Finish**.
 7. The download and installation progress bar is located at the lower right corner.
 8. Once installation has been completed, you may get a prompt to have **Eclipse** restarted. Click **Restart Now**. 
 9. Once **Eclipse** has been restarted, go to **Window > Show View > Other...** menu.
 10. Select **ABAP > abapGit Repositories** from the list. Click **Open**.
 11. In the lower part panels, select **abapGit Repositories** tab.
 12. Click the green plus icon to **Link New abapGit Repository**.
 13. Enter the Git repository's address. Click **Next** and follow the instructions until finished.
