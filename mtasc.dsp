# Microsoft Developer Studio Project File - Name="mtasc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=mtasc - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mtasc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mtasc.mak" CFG="mtasc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mtasc - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "mtasc - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "mtasc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Cmd_Line "NMAKE /f mtasc.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "mtasc.exe"
# PROP BASE Bsc_Name "mtasc.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Cmd_Line "ocamake -opt -pp camlp4o mtasc.dsp extLib.cmxa extc.cmxa "
# PROP Rebuild_Opt "/a"
# PROP Target_File "mtasc.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "mtasc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir ""
# PROP BASE Cmd_Line "NMAKE /f mtasc.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "mtasc.exe"
# PROP BASE Bsc_Name "mtasc.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Cmd_Line "ocamake -lp -custom -pp camlp4o mtasc.dsp extLib.cma extc.cma"
# PROP Rebuild_Opt "/a"
# PROP Target_File "mtasc.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "mtasc - Win32 Release"
# Name "mtasc - Win32 Debug"

!IF  "$(CFG)" == "mtasc - Win32 Release"

!ELSEIF  "$(CFG)" == "mtasc - Win32 Debug"

!ENDIF 

# Begin Group "swflib"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\swflib\actionScript.ml
# End Source File
# Begin Source File

SOURCE=..\swflib\swf.ml
# End Source File
# Begin Source File

SOURCE=..\swflib\swfParser.ml
# End Source File
# Begin Source File

SOURCE=..\swflib\swfZip.ml
# End Source File
# End Group
# Begin Group "main"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\main.ml
# End Source File
# End Group
# Begin Source File

SOURCE=.\class.ml
# End Source File
# Begin Source File

SOURCE=.\expr.ml
# End Source File
# Begin Source File

SOURCE=.\genSwf.ml
# End Source File
# Begin Source File

SOURCE=.\lexer.mll
# End Source File
# Begin Source File

SOURCE=.\parser.ml
# End Source File
# Begin Source File

SOURCE=.\plugin.ml
# End Source File
# Begin Source File

SOURCE=.\typer.ml
# End Source File
# End Target
# End Project
