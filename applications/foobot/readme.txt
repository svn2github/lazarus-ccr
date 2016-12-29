Foobot project
==============

The main application Foobot Interregator shows how to pull all the data available from the Foobot API
The monitor application shows how to write a practical graphical Foobot Monitor for one foobot
Both applications have dependency for CryptIni - available via OnlinePackageManager

If you want to build your own GUI, then you need three files:
1. foobot_objects.pas - Object definitions
2. ugenericcollections.pas - helper file for foobot_objects
3. foobot_utilities.pas - the main API

Use the routines in foobot_utilities.pas to:
1. Fetch Foobot identities via Username + API Key
2. Fetch data from an identity

In all cases, you will need a foobot account and an API key.
The API key is freely available from the Foobot website

- minesadorada@charcodelvalle.com