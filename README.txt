==================================================
F-SAC Test - Open Source Testing Application for F-SAC model
Version 1.0,
Copyright (c) Gregor Reichert
==================================================

This is a software to implement the F-SAC model in Visual Basic with the aim to implement this into DWSIM open source precess simulator.


As basis for this implementation the origanal Fortran code of the publication was used.
See the following header taken from readme file of original code:

--------------------------------------------------------------------------------
 Copyright (c) 2011-2013, Federal University of Rio Grande do Sul
 All rights reserved.

 This software is subject to a BSD License, please see the License.txt
 file for more information.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED.

 Authors: Luiz Felipe Kusler Possani
          Rafael de Pelegrini Soares

 This is a demonstration code, for more efficient implementations
 please contact rafael@enq.ufrgs.br.

 PLEASE CITE AS Soares and Gerber (2013), Ind. Eng. Chem. Res. DOI:10.1021/ie400170a.
--------------------------------------------------------------------------------


==================================================
DISCLAIMER
==================================================

The data and information within this software has been obtained from a wide variety of literature sources.  While reasonable care has been exercised in the collection of data and testing of this software, the author of this software disclaims any warranty, expressed or implied, as to the accuracy or reliability of the data or calculations contained therein. The results of calculations obtained from this software yield approximate results, which will not always be suitable for every application.

The software is designed for use by trained professional personnel and is not a substitute for sound professional judgment.  It is the sole responsibility of the user to validate the data presented by this software and to determine whether the results of this program are accurate and suitable for any specific purpose.  No guarantee of accuracy or fitness for any purpose is expressed or implied.  The author strongly recommends that the data be checked against other sources and/or methods before use and application.  The author shall not be held liable for any direct, indirect, consequential or incidental damages incurred through use of the data or calculations. 

==================================================
LICENSE
==================================================

This software is licensed under the GNU General Public License (GPL) Version 3.
Please see licence.txt file from original F-SAC fortran implementation for additional information.
 

==================================================
KNOWN ISSUES
==================================================

Known limitations of actual version:
- Fortran code doesn't seem to depict the formulas described in articles
- In the middle of calculation procedures to calculate sigma profiles the program stalls



==================================================
VERSION HISTORY / CHANGELOG
==================================================

Version 1.0 

- [NEW] Initial implementation, rebuilding the fortran code in visual basic
- [NEW] Only components which are available in both databases of "ChemSep" and "F-SAC" are displayed
