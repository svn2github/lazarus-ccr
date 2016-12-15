--> Please read! - Choosing an installation folder
===============================

* By default, this app will install into your Program Files folder,
  but the next dialog will invite you to change that.

* The best place to install it is in your development folder,
  where you have your component code
  i.e. /component_root/latest_stable/

* Suggested structure (optional):
* /component_root/latest_stable - component release source files

* /component_root/latest_stable/sourcezip.zip
  - Original zipped release files for OPM (to be uploaded)

* /component_root/latest_stable/jsoneditor.exe

* /component_root/latest_stable/updates/update_component.json
 - Generated update json (to be uploaded)

* /component_root/latest_stable/updates/updatezipfile.zip
 - Updated zipped source files (to be uploaded)

* Each unique component should have its own copy of jsoneditor.exe
* It is designed to be deployed in multiple locations
* Each copy has a unique cfg file

* On first run, jsoneditor.exe will make 2 subfolders:
* /updates
* /locale (for the language files)
* It also makes a config file in the Users/<youname>/Appdata area



