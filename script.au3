#NoTrayIcon
Global Const $TAGPOINT = "struct;long X;long Y;endstruct"
Global Const $TAGRECT = "struct;long Left;long Top;long Right;long Bottom;endstruct"
Global Const $TAGFILETIME = "struct;dword Lo;dword Hi;endstruct"
Global Const $TAGSYSTEMTIME = "struct;word Year;word Month;word Dow;word Day;word Hour;word Minute;word Second;word MSeconds;endstruct"
Global Const $TAGNMHDR = "struct;hwnd hWndFrom;uint_ptr IDFrom;INT Code;endstruct"
Global Const $TAGCOMBOBOXEXITEM = "uint Mask;int_ptr Item;ptr Text;int TextMax;int Image;int SelectedImage;int OverlayImage;" & "int Indent;lparam Param"
Global Const $TAGNMCOMBOBOXEX = $TAGNMHDR & ";uint Mask;int_ptr Item;ptr Text;int TextMax;int Image;" & "int SelectedImage;int OverlayImage;int Indent;lparam Param"
Global Const $TAGDTPRANGE = "word MinYear;word MinMonth;word MinDOW;word MinDay;word MinHour;word MinMinute;" & "word MinSecond;word MinMSecond;word MaxYear;word MaxMonth;word MaxDOW;word MaxDay;word MaxHour;" & "word MaxMinute;word MaxSecond;word MaxMSecond;bool MinValid;bool MaxValid"
Global Const $TAGEVENTLOGRECORD = "dword Length;dword Reserved;dword RecordNumber;dword TimeGenerated;dword TimeWritten;dword EventID;" & "word EventType;word NumStrings;word EventCategory;word ReservedFlags;dword ClosingRecordNumber;dword StringOffset;" & "dword UserSidLength;dword UserSidOffset;dword DataLength;dword DataOffset"
Global Const $TAGGDIPIMAGECODECINFO = "byte CLSID[16];byte FormatID[16];ptr CodecName;ptr DllName;ptr FormatDesc;ptr FileExt;" & "ptr MimeType;dword Flags;dword Version;dword SigCount;dword SigSize;ptr SigPattern;ptr SigMask"
Global Const $TAGLVITEM = "struct;uint Mask;int Item;int SubItem;uint State;uint StateMask;ptr Text;int TextMax;int Image;lparam Param;" & "int Indent;int GroupID;uint Columns;ptr pColumns;ptr piColFmt;int iGroup;endstruct"
Global Const $TAGNMLISTVIEW = $TAGNMHDR & ";int Item;int SubItem;uint NewState;uint OldState;uint Changed;" & "struct;long ActionX;long ActionY;endstruct;lparam Param"
Global Const $TAGNMLVCUSTOMDRAW = "struct;" & $TAGNMHDR & ";dword dwDrawStage;handle hdc;" & $TAGRECT & ";dword_ptr dwItemSpec;uint uItemState;lparam lItemlParam;endstruct" & ";dword clrText;dword clrTextBk;int iSubItem;dword dwItemType;dword clrFace;int iIconEffect;" & "int iIconPhase;int iPartId;int iStateId;struct;long TextLeft;long TextTop;long TextRight;long TextBottom;endstruct;uint uAlign"
Global Const $TAGNMITEMACTIVATE = $TAGNMHDR & ";int Index;int SubItem;uint NewState;uint OldState;uint Changed;" & $TAGPOINT & ";lparam lParam;uint KeyFlags"
Global Const $TAGMCHITTESTINFO = "uint Size;" & $TAGPOINT & ";uint Hit;" & $TAGSYSTEMTIME & ";" & $TAGRECT & ";int iOffset;int iRow;int iCol"
Global Const $TAGMCMONTHRANGE = "word MinYear;word MinMonth;word MinDOW;word MinDay;word MinHour;word MinMinute;word MinSecond;" & "word MinMSeconds;word MaxYear;word MaxMonth;word MaxDOW;word MaxDay;word MaxHour;word MaxMinute;word MaxSecond;" & "word MaxMSeconds;short Span"
Global Const $TAGMCRANGE = "word MinYear;word MinMonth;word MinDOW;word MinDay;word MinHour;word MinMinute;word MinSecond;" & "word MinMSeconds;word MaxYear;word MaxMonth;word MaxDOW;word MaxDay;word MaxHour;word MaxMinute;word MaxSecond;" & "word MaxMSeconds;short MinSet;short MaxSet"
Global Const $TAGMCSELRANGE = "word MinYear;word MinMonth;word MinDOW;word MinDay;word MinHour;word MinMinute;word MinSecond;" & "word MinMSeconds;word MaxYear;word MaxMonth;word MaxDOW;word MaxDay;word MaxHour;word MaxMinute;word MaxSecond;" & "word MaxMSeconds"
Global Const $TAGNMSELCHANGE = $TAGNMHDR & ";struct;word BegYear;word BegMonth;word BegDOW;word BegDay;word BegHour;word BegMinute;word BegSecond;word BegMSeconds;endstruct;" & "struct;word EndYear;word EndMonth;word EndDOW;word EndDay;word EndHour;word EndMinute;word EndSecond;word EndMSeconds;endstruct"
Global Const $TAGTVITEM = "struct;uint Mask;handle hItem;uint State;uint StateMask;ptr Text;int TextMax;int Image;int SelectedImage;" & "int Children;lparam Param;endstruct"
Global Const $TAGNMTREEVIEW = $TAGNMHDR & ";uint Action;" & "struct;uint OldMask;handle OldhItem;uint OldState;uint OldStateMask;" & "ptr OldText;int OldTextMax;int OldImage;int OldSelectedImage;int OldChildren;lparam OldParam;endstruct;" & "struct;uint NewMask;handle NewhItem;uint NewState;uint NewStateMask;" & "ptr NewText;int NewTextMax;int NewImage;int NewSelectedImage;int NewChildren;lparam NewParam;endstruct;" & "struct;long PointX;long PointY;endstruct"
Global Const $TAGNMTVCUSTOMDRAW = "struct;" & $TAGNMHDR & ";dword DrawStage;handle HDC;" & $TAGRECT & ";dword_ptr ItemSpec;uint ItemState;lparam ItemParam;endstruct" & ";dword ClrText;dword ClrTextBk;int Level"
Global Const $TAGMENUITEMINFO = "uint Size;uint Mask;uint Type;uint State;uint ID;handle SubMenu;handle BmpChecked;handle BmpUnchecked;" & "ulong_ptr ItemData;ptr TypeData;uint CCH;handle BmpItem"
$__TAGREBARBANDINFO = "uint cbSize;uint fMask;uint fStyle;dword clrFore;dword clrBack;ptr lpText;uint cch;" & "int iImage;hwnd hwndChild;uint cxMinChild;uint cyMinChild;uint cx;handle hbmBack;uint wID;uint cyChild;uint cyMaxChild;" & "uint cyIntegral;uint cxIdeal;lparam lParam;uint cxHeader"
If @OSVersion <> "WIN_XP" Then $__TAGREBARBANDINFO &= ";" & $TAGRECT & ";uint uChevronState"
Global Const $TAGNMRBAUTOSIZE = $TAGNMHDR & ";bool fChanged;" & "struct;long TargetLeft;long TargetTop;long TargetRight;long TargetBottom;endstruct;" & "struct;long ActualLeft;long ActualTop;long ActualRight;long ActualBottom;endstruct"
Global Const $TAGNMREBARCHILDSIZE = $TAGNMHDR & ";uint uBand;uint wID;" & "struct;long CLeft;long CTop;long CRight;long CBottom;endstruct;" & "struct;long BLeft;long BTop;long BRight;long BBottom;endstruct"
Global Const $TAGNMTOOLBAR = $TAGNMHDR & ";int iItem;" & "struct;int iBitmap;int idCommand;byte fsState;byte fsStyle;dword_ptr dwData;int_ptr iString;endstruct" & ";int cchText;ptr pszText;" & $TAGRECT
Global Const $TAGOPENFILENAME = "dword StructSize;hwnd hwndOwner;handle hInstance;ptr lpstrFilter;ptr lpstrCustomFilter;" & "dword nMaxCustFilter;dword nFilterIndex;ptr lpstrFile;dword nMaxFile;ptr lpstrFileTitle;dword nMaxFileTitle;" & "ptr lpstrInitialDir;ptr lpstrTitle;dword Flags;word nFileOffset;word nFileExtension;ptr lpstrDefExt;lparam lCustData;" & "ptr lpfnHook;ptr lpTemplateName;ptr pvReserved;dword dwReserved;dword FlagsEx"
Global Const $TAGBITMAPINFO = "struct;dword Size;long Width;long Height;word Planes;word BitCount;dword Compression;dword SizeImage;" & "long XPelsPerMeter;long YPelsPerMeter;dword ClrUsed;dword ClrImportant;endstruct;dword RGBQuad"
Global Const $TAGSCROLLBARINFO = "dword cbSize;" & $TAGRECT & ";int dxyLineButton;int xyThumbTop;" & "int xyThumbBottom;int reserved;dword rgstate[6]"
Global Const $TAGLOGFONT = "long Height;long Width;long Escapement;long Orientation;long Weight;byte Italic;byte Underline;" & "byte Strikeout;byte CharSet;byte OutPrecision;byte ClipPrecision;byte Quality;byte PitchAndFamily;wchar FaceName[32]"
Global Const $TAGSTARTUPINFO = "dword Size;ptr Reserved1;ptr Desktop;ptr Title;dword X;dword Y;dword XSize;dword YSize;dword XCountChars;" & "dword YCountChars;dword FillAttribute;dword Flags;word ShowWindow;word Reserved2;ptr Reserved3;handle StdInput;" & "handle StdOutput;handle StdError"
Global Const $TAGSECURITY_ATTRIBUTES = "dword Length;ptr Descriptor;bool InheritHandle"
Global Const $TAGTEXTMETRIC = "long tmHeight;long tmAscent;long tmDescent;long tmInternalLeading;long tmExternalLeading;" & "long tmAveCharWidth;long tmMaxCharWidth;long tmWeight;long tmOverhang;long tmDigitizedAspectX;long tmDigitizedAspectY;" & "wchar tmFirstChar;wchar tmLastChar;wchar tmDefaultChar;wchar tmBreakChar;byte tmItalic;byte tmUnderlined;byte tmStruckOut;" & "byte tmPitchAndFamily;byte tmCharSet"
Global Const $TAGCHOOSECOLOR = "dword Size;hwnd hWndOwnder;handle hInstance;dword rgbResult;ptr CustColors;dword Flags;lparam lCustData;" & "ptr lpfnHook;ptr lpTemplateName"
Global Const $TAGCHOOSEFONT = "dword Size;hwnd hWndOwner;handle hDC;ptr LogFont;int PointSize;dword Flags;dword rgbColors;lparam CustData;" & "ptr fnHook;ptr TemplateName;handle hInstance;ptr szStyle;word FontType;int SizeMin;int SizeMax"
Func _SINGLETON ( $SOCCURENCENAME , $IFLAG = 0 )
	Local Const $ERROR_ALREADY_EXISTS = 183
	Local Const $SECURITY_DESCRIPTOR_REVISION = 1
	Local $TSECURITYATTRIBUTES = 0
	If BitAND ( $IFLAG , 2 ) Then
		Local $TSECURITYDESCRIPTOR = DllStructCreate ( "byte;byte;word;ptr[4]" )
		Local $ARET = DllCall ( "advapi32.dll" , "bool" , "InitializeSecurityDescriptor" , "struct*" , $TSECURITYDESCRIPTOR , "dword" , $SECURITY_DESCRIPTOR_REVISION )
		If @error Then Return SetError ( @error , @extended , 0 )
		If $ARET [ 0 ] Then
			$ARET = DllCall ( "advapi32.dll" , "bool" , "SetSecurityDescriptorDacl" , "struct*" , $TSECURITYDESCRIPTOR , "bool" , 1 , "ptr" , 0 , "bool" , 0 )
			If @error Then Return SetError ( @error , @extended , 0 )
			If $ARET [ 0 ] Then
				$TSECURITYATTRIBUTES = DllStructCreate ( $TAGSECURITY_ATTRIBUTES )
				DllStructSetData ( $TSECURITYATTRIBUTES , 1 , DllStructGetSize ( $TSECURITYATTRIBUTES ) )
				DllStructSetData ( $TSECURITYATTRIBUTES , 2 , DllStructGetPtr ( $TSECURITYDESCRIPTOR ) )
				DllStructSetData ( $TSECURITYATTRIBUTES , 3 , 0 )
			EndIf
		EndIf
	EndIf
	Local $HANDLE = DllCall ( "kernel32.dll" , "handle" , "CreateMutexW" , "struct*" , $TSECURITYATTRIBUTES , "bool" , 1 , "wstr" , $SOCCURENCENAME )
	If @error Then Return SetError ( @error , @extended , 0 )
	Local $LASTERROR = DllCall ( "kernel32.dll" , "dword" , "GetLastError" )
	If @error Then Return SetError ( @error , @extended , 0 )
	If $LASTERROR [ 0 ] = $ERROR_ALREADY_EXISTS Then
		If BitAND ( $IFLAG , 1 ) Then
			Return SetError ( $LASTERROR [ 0 ] , $LASTERROR [ 0 ] , 0 )
		Else
			Exit - 1
		EndIf
	EndIf
	Return $HANDLE [ 0 ]
EndFunc
Func _STRINGREPEAT ( $SSTRING , $IREPEATCOUNT )
	Local $SRESULT
	Select
	Case Not StringIsInt ( $IREPEATCOUNT )
		SetError ( 1 )
		Return ""
	Case StringLen ( $SSTRING ) < 1
		SetError ( 1 )
		Return ""
	Case $IREPEATCOUNT <= 0
		SetError ( 1 )
		Return ""
Case Else
		For $ICOUNT = 1 To $IREPEATCOUNT
			$SRESULT &= $SSTRING
		Next
		Return $SRESULT
	EndSelect
EndFunc
Global Const $READ_CONTROL = 131072
Global Const $STANDARD_RIGHTS_REQUIRED = 983040
Global Const $STANDARD_RIGHTS_READ = $READ_CONTROL
Global Const $STANDARD_RIGHTS_WRITE = $READ_CONTROL
Global Const $STANDARD_RIGHTS_ALL = 2031616
Global Const $SC_MANAGER_CONNECT = 1
Global Const $SC_MANAGER_CREATE_SERVICE = 2
Global Const $SC_MANAGER_ENUMERATE_SERVICE = 4
Global Const $SC_MANAGER_LOCK = 8
Global Const $SC_MANAGER_QUERY_LOCK_STATUS = 16
Global Const $SC_MANAGER_MODIFY_BOOT_CONFIG = 32
Global Const $SC_MANAGER_ALL_ACCESS = BitOR ( $STANDARD_RIGHTS_REQUIRED , $SC_MANAGER_CONNECT , $SC_MANAGER_CREATE_SERVICE , $SC_MANAGER_ENUMERATE_SERVICE , $SC_MANAGER_LOCK , $SC_MANAGER_QUERY_LOCK_STATUS , $SC_MANAGER_MODIFY_BOOT_CONFIG )
Global Const $SERVICE_QUERY_CONFIG = 1
Global Const $SERVICE_CHANGE_CONFIG = 2
Global Const $SERVICE_QUERY_STATUS = 4
Global Const $SERVICE_ENUMERATE_DEPENDENTS = 8
Global Const $SERVICE_START = 16
Global Const $SERVICE_STOP = 32
Global Const $SERVICE_PAUSE_CONTINUE = 64
Global Const $SERVICE_INTERROGATE = 128
Global Const $SERVICE_USER_DEFINED_CONTROL = 256
Global Const $SERVICE_ALL_ACCESS = BitOR ( $STANDARD_RIGHTS_REQUIRED , $SERVICE_QUERY_CONFIG , $SERVICE_CHANGE_CONFIG , $SERVICE_QUERY_STATUS , $SERVICE_ENUMERATE_DEPENDENTS , $SERVICE_START , $SERVICE_STOP , $SERVICE_PAUSE_CONTINUE , $SERVICE_INTERROGATE , $SERVICE_USER_DEFINED_CONTROL )
Global Const $SERVICE_KERNEL_DRIVER = 1
Global Const $SERVICE_FILE_SYSTEM_DRIVER = 2
Global Const $SERVICE_ADAPTER = 4
Global Const $SERVICE_RECOGNIZER_DRIVER = 8
Global Const $SERVICE_DRIVER = BitOR ( $SERVICE_KERNEL_DRIVER , $SERVICE_FILE_SYSTEM_DRIVER , $SERVICE_RECOGNIZER_DRIVER )
Global Const $SERVICE_WIN32_OWN_PROCESS = 16
Global Const $SERVICE_WIN32_SHARE_PROCESS = 32
Global Const $SERVICE_WIN32 = BitOR ( $SERVICE_WIN32_OWN_PROCESS , $SERVICE_WIN32_SHARE_PROCESS )
Global Const $SERVICE_INTERACTIVE_PROCESS = 256
Global Const $SERVICE_TYPE_ALL = BitOR ( $SERVICE_WIN32 , $SERVICE_ADAPTER , $SERVICE_DRIVER , $SERVICE_INTERACTIVE_PROCESS )
Global Const $SERVICE_STOPPED = 1
Global Const $SERVICE_START_PENDING = 2
Global Const $SERVICE_STOP_PENDING = 3
Global Const $SERVICE_RUNNING = 4
Global Const $SERVICE_CONTINUE_PENDING = 5
Global Const $SERVICE_PAUSE_PENDING = 6
Global Const $SERVICE_PAUSED = 7
Global Const $SERVICE_ACTIVE = 1
Global Const $SERVICE_INACTIVE = 2
Global Const $SERVICE_STATE_ALL = BitOR ( $SERVICE_ACTIVE , $SERVICE_INACTIVE )
Global Const $SC_ENUM_PROCESS_INFO = 0
Func _SERVICE_ENUM ( $ISERVICETYPE , $ISERVICESTATE = $SERVICE_STATE_ALL , $SLOADORDERGROUP = Default , $SCOMPUTERNAME = "" )
	Local $TLOADORDERGROUP , $PLOADORDERGROUP , $HSC , $AVEA , $TEB , $AVEB , $IEE , $IES , $TAGEC , $TEC , $TED
	$TLOADORDERGROUP = DllStructCreate ( "char[" & Number ( $SLOADORDERGROUP <> Default ) * ( StringLen ( $SLOADORDERGROUP ) + 1 ) & "]" )
	DllStructSetData ( $TLOADORDERGROUP , 1 , $SLOADORDERGROUP )
	$PLOADORDERGROUP = DllStructGetPtr ( $TLOADORDERGROUP )
	$HSC = OPENSCMANAGER ( $SCOMPUTERNAME , $SC_MANAGER_ENUMERATE_SERVICE )
	$AVEA = DllCall ( "advapi32.dll" , "int" , "EnumServicesStatusEx" , "hwnd" , $HSC , "dword" , $SC_ENUM_PROCESS_INFO , "dword" , $ISERVICETYPE , "dword" , $ISERVICESTATE , "ptr" , 0 , "dword" , 0 , "dword*" , 0 , "dword*" , 0 , "dword*" , 0 , "ptr" , $PLOADORDERGROUP )
	$TEB = DllStructCreate ( "ubyte[" & $AVEA [ 7 ] & "]" )
	$AVEB = DllCall ( "advapi32.dll" , "int" , "EnumServicesStatusEx" , "hwnd" , $HSC , "dword" , $SC_ENUM_PROCESS_INFO , "dword" , $ISERVICETYPE , "dword" , $ISERVICESTATE , "ptr" , DllStructGetPtr ( $TEB ) , "dword" , DllStructGetSize ( $TEB ) , "dword*" , 0 , "dword*" , 0 , "dword*" , 0 , "ptr" , $PLOADORDERGROUP )
	If $AVEB [ 0 ] = 0 Then $IEE = GETLASTERROR ( )
	CLOSESERVICEHANDLE ( $HSC )
	While $IES < $AVEB [ 8 ]
		$IES += 1
		$TAGEC &= "uint_ptr[2];dword[9];"
	WEnd
	$TEC = DllStructCreate ( StringTrimRight ( $TAGEC , 1 ) , $AVEB [ 5 ] )
	Local $AIEC [ $AVEB [ 8 ] + 1 ] [ 2 ]
	For $I = 1 To $AVEB [ 8 ]
		$AIEC [ $I ] [ 0 ] = DllStructGetData ( $TEC , 2 * ( $I - 1 ) - 1 , 2 ) - DllStructGetData ( $TEC , 2 * $I - 1 , 1 )
		$AIEC [ $I ] [ 1 ] = DllStructGetData ( $TEC , 2 * $I - 1 , 1 ) - DllStructGetData ( $TEC , 2 * $I - 1 , 2 )
	Next
	If $AVEB [ 8 ] > 0 Then $AIEC [ 1 ] [ 0 ] = Execute ( $AVEB [ 5 ] ) + $AVEB [ 6 ] - DllStructGetData ( $TEC , 1 , 1 )
	Local $ASED [ $AVEB [ 8 ] + 1 ] [ 11 ]
	$ASED [ 0 ] [ 0 ] = $AVEB [ 8 ]
	For $I = 1 To $AVEB [ 8 ]
		For $K = 0 To 1
			$TED = DllStructCreate ( "char[" & $AIEC [ $I ] [ $K ] & "]" , DllStructGetData ( $TEC , 2 * $I - 1 , $K + 1 ) )
			$ASED [ $I ] [ $K ] = DllStructGetData ( $TED , 1 )
		Next
		For $K = 2 To UBound ( $ASED , 2 ) - 1
			$ASED [ $I ] [ $K ] = DllStructGetData ( $TEC , 2 * $I , $K - 1 )
		Next
	Next
	Return SetError ( $IEE , 0 , $ASED )
EndFunc
Func CLOSESERVICEHANDLE ( $HSCOBJECT )
	Local $AVCSH = DllCall ( "advapi32.dll" , "int" , "CloseServiceHandle" , "hwnd" , $HSCOBJECT )
	Return $AVCSH [ 0 ]
EndFunc
Func GETLASTERROR ( )
	Local $AIE = DllCall ( "kernel32.dll" , "dword" , "GetLastError" )
	Return $AIE [ 0 ]
EndFunc
Func OPENSCMANAGER ( $SCOMPUTERNAME , $IACCESS )
	Local $AVOSCM = DllCall ( "advapi32.dll" , "hwnd" , "OpenSCManager" , "str" , $SCOMPUTERNAME , "str" , "ServicesActive" , "dword" , $IACCESS )
	Return $AVOSCM [ 0 ]
EndFunc
Func _ARRAYREVERSE ( ByRef $AVARRAY , $ISTART = 0 , $IEND = 0 )
	If Not IsArray ( $AVARRAY ) Then Return SetError ( 1 , 0 , 0 )
	If UBound ( $AVARRAY , 0 ) <> 1 Then Return SetError ( 3 , 0 , 0 )
	Local $VTMP , $IUBOUND = UBound ( $AVARRAY ) - 1
	If $IEND < 1 Or $IEND > $IUBOUND Then $IEND = $IUBOUND
	If $ISTART < 0 Then $ISTART = 0
	If $ISTART > $IEND Then Return SetError ( 2 , 0 , 0 )
	For $I = $ISTART To Int ( ( $ISTART + $IEND - 1 ) / 2 )
		$VTMP = $AVARRAY [ $I ]
		$AVARRAY [ $I ] = $AVARRAY [ $IEND ]
		$AVARRAY [ $IEND ] = $VTMP
		$IEND -= 1
	Next
	Return 1
EndFunc
Func _ARRAYSORT ( ByRef $AVARRAY , $IDESCENDING = 0 , $ISTART = 0 , $IEND = 0 , $ISUBITEM = 0 )
	If Not IsArray ( $AVARRAY ) Then Return SetError ( 1 , 0 , 0 )
	Local $IUBOUND = UBound ( $AVARRAY ) - 1
	If $IEND < 1 Or $IEND > $IUBOUND Then $IEND = $IUBOUND
	If $ISTART < 0 Then $ISTART = 0
	If $ISTART > $IEND Then Return SetError ( 2 , 0 , 0 )
	Switch UBound ( $AVARRAY , 0 )
	Case 1
		__ARRAYQUICKSORT1D ( $AVARRAY , $ISTART , $IEND )
		If $IDESCENDING Then _ARRAYREVERSE ( $AVARRAY , $ISTART , $IEND )
	Case 2
		Local $ISUBMAX = UBound ( $AVARRAY , 2 ) - 1
		If $ISUBITEM > $ISUBMAX Then Return SetError ( 3 , 0 , 0 )
		If $IDESCENDING Then
			$IDESCENDING = - 1
		Else
			$IDESCENDING = 1
		EndIf
		__ARRAYQUICKSORT2D ( $AVARRAY , $IDESCENDING , $ISTART , $IEND , $ISUBITEM , $ISUBMAX )
Case Else
		Return SetError ( 4 , 0 , 0 )
	EndSwitch
	Return 1
EndFunc
Func __ARRAYQUICKSORT1D ( ByRef $AVARRAY , ByRef $ISTART , ByRef $IEND )
	If $IEND <= $ISTART Then Return
	Local $VTMP
	If ( $IEND - $ISTART ) < 15 Then
		Local $VCUR
		For $I = $ISTART + 1 To $IEND
			$VTMP = $AVARRAY [ $I ]
			If IsNumber ( $VTMP ) Then
				For $J = $I - 1 To $ISTART Step - 1
					$VCUR = $AVARRAY [ $J ]
					If ( $VTMP >= $VCUR And IsNumber ( $VCUR ) ) Or ( Not IsNumber ( $VCUR ) And StringCompare ( $VTMP , $VCUR ) >= 0 ) Then ExitLoop
					$AVARRAY [ $J + 1 ] = $VCUR
				Next
			Else
				For $J = $I - 1 To $ISTART Step - 1
					If ( StringCompare ( $VTMP , $AVARRAY [ $J ] ) >= 0 ) Then ExitLoop
					$AVARRAY [ $J + 1 ] = $AVARRAY [ $J ]
				Next
			EndIf
			$AVARRAY [ $J + 1 ] = $VTMP
		Next
		Return
	EndIf
	Local $L = $ISTART , $R = $IEND , $VPIVOT = $AVARRAY [ Int ( ( $ISTART + $IEND ) / 2 ) ] , $FNUM = IsNumber ( $VPIVOT )
	Do
		If $FNUM Then
			While ( $AVARRAY [ $L ] < $VPIVOT And IsNumber ( $AVARRAY [ $L ] ) ) Or ( Not IsNumber ( $AVARRAY [ $L ] ) And StringCompare ( $AVARRAY [ $L ] , $VPIVOT ) < 0 )
				$L += 1
			WEnd
			While ( $AVARRAY [ $R ] > $VPIVOT And IsNumber ( $AVARRAY [ $R ] ) ) Or ( Not IsNumber ( $AVARRAY [ $R ] ) And StringCompare ( $AVARRAY [ $R ] , $VPIVOT ) > 0 )
				$R -= 1
			WEnd
		Else
			While ( StringCompare ( $AVARRAY [ $L ] , $VPIVOT ) < 0 )
				$L += 1
			WEnd
			While ( StringCompare ( $AVARRAY [ $R ] , $VPIVOT ) > 0 )
				$R -= 1
			WEnd
		EndIf
		If $L <= $R Then
			$VTMP = $AVARRAY [ $L ]
			$AVARRAY [ $L ] = $AVARRAY [ $R ]
			$AVARRAY [ $R ] = $VTMP
			$L += 1
			$R -= 1
		EndIf
	Until $L > $R
	__ARRAYQUICKSORT1D ( $AVARRAY , $ISTART , $R )
	__ARRAYQUICKSORT1D ( $AVARRAY , $L , $IEND )
EndFunc
Func __ARRAYQUICKSORT2D ( ByRef $AVARRAY , ByRef $ISTEP , ByRef $ISTART , ByRef $IEND , ByRef $ISUBITEM , ByRef $ISUBMAX )
	If $IEND <= $ISTART Then Return
	Local $VTMP , $L = $ISTART , $R = $IEND , $VPIVOT = $AVARRAY [ Int ( ( $ISTART + $IEND ) / 2 ) ] [ $ISUBITEM ] , $FNUM = IsNumber ( $VPIVOT )
	Do
		If $FNUM Then
			While ( $ISTEP * ( $AVARRAY [ $L ] [ $ISUBITEM ] - $VPIVOT ) < 0 And IsNumber ( $AVARRAY [ $L ] [ $ISUBITEM ] ) ) Or ( Not IsNumber ( $AVARRAY [ $L ] [ $ISUBITEM ] ) And $ISTEP * StringCompare ( $AVARRAY [ $L ] [ $ISUBITEM ] , $VPIVOT ) < 0 )
				$L += 1
			WEnd
			While ( $ISTEP * ( $AVARRAY [ $R ] [ $ISUBITEM ] - $VPIVOT ) > 0 And IsNumber ( $AVARRAY [ $R ] [ $ISUBITEM ] ) ) Or ( Not IsNumber ( $AVARRAY [ $R ] [ $ISUBITEM ] ) And $ISTEP * StringCompare ( $AVARRAY [ $R ] [ $ISUBITEM ] , $VPIVOT ) > 0 )
				$R -= 1
			WEnd
		Else
			While ( $ISTEP * StringCompare ( $AVARRAY [ $L ] [ $ISUBITEM ] , $VPIVOT ) < 0 )
				$L += 1
			WEnd
			While ( $ISTEP * StringCompare ( $AVARRAY [ $R ] [ $ISUBITEM ] , $VPIVOT ) > 0 )
				$R -= 1
			WEnd
		EndIf
		If $L <= $R Then
			For $I = 0 To $ISUBMAX
				$VTMP = $AVARRAY [ $L ] [ $I ]
				$AVARRAY [ $L ] [ $I ] = $AVARRAY [ $R ] [ $I ]
				$AVARRAY [ $R ] [ $I ] = $VTMP
			Next
			$L += 1
			$R -= 1
		EndIf
	Until $L > $R
	__ARRAYQUICKSORT2D ( $AVARRAY , $ISTEP , $ISTART , $R , $ISUBITEM , $ISUBMAX )
	__ARRAYQUICKSORT2D ( $AVARRAY , $ISTEP , $L , $IEND , $ISUBITEM , $ISUBMAX )
EndFunc
Func _ARRAYTOSTRING ( Const ByRef $AVARRAY , $SDELIM = "|" , $ISTART = 0 , $IEND = 0 )
	If Not IsArray ( $AVARRAY ) Then Return SetError ( 1 , 0 , "" )
	If UBound ( $AVARRAY , 0 ) <> 1 Then Return SetError ( 3 , 0 , "" )
	Local $SRESULT , $IUBOUND = UBound ( $AVARRAY ) - 1
	If $IEND < 1 Or $IEND > $IUBOUND Then $IEND = $IUBOUND
	If $ISTART < 0 Then $ISTART = 0
	If $ISTART > $IEND Then Return SetError ( 2 , 0 , "" )
	For $I = $ISTART To $IEND
		$SRESULT &= $AVARRAY [ $I ] & $SDELIM
	Next
	Return StringTrimRight ( $SRESULT , StringLen ( $SDELIM ) )
EndFunc
Global Const $NODE_ELEMENT = 1
Global Const $NODE_TEXT = 3
Global $STRFILE
Global $OXMLMYERROR
Global $SXML_ERROR
Global $FDEBUGGING
Global $DOMVERSION = - 1
Global $OBJDOC
Func _XMLFILEOPEN ( $STRXMLFILE , $STRNAMESPC = "" , $IVER = - 1 , $FVALONPARSE = True )
	If $IVER <> - 1 Then
		If $IVER > - 1 And $IVER < 7 Then
			$OBJDOC = ObjCreate ( "Msxml2.DOMDocument." & $IVER & ".0" )
			If IsObj ( $OBJDOC ) Then
				$DOMVERSION = $IVER
			EndIf
		Else
			MsgBox ( 266288 , "Error:" , "Failed to create object with MSXML version " & $IVER )
			SetError ( 1 )
			Return 0
		EndIf
	Else
		For $X = 8 To 0 Step - 1
			If FileExists ( @SystemDir & "\msxml" & $X & ".dll" ) Then
				$OBJDOC = ObjCreate ( "Msxml2.DOMDocument." & $X & ".0" )
				If IsObj ( $OBJDOC ) Then
					$DOMVERSION = $X
					ExitLoop
				EndIf
			EndIf
		Next
	EndIf
	If Not IsObj ( $OBJDOC ) Then
		_XMLERROR ( "Error: MSXML not found. This object is required to use this program." )
		SetError ( 2 )
		Return - 1
	EndIf
	$OXMLMYERROR = ObjEvent ( "AutoIt.Error" )
	If $OXMLMYERROR = "" Then
		$OXMLMYERROR = ObjEvent ( "AutoIt.Error" , "_XMLCOMEerr" )
	EndIf
	$STRFILE = $STRXMLFILE
	$OBJDOC .async = False
	$OBJDOC .preserveWhiteSpace = True
	$OBJDOC .validateOnParse = $FVALONPARSE
	If $DOMVERSION > 4 Then $OBJDOC .setProperty ( "ProhibitDTD" , False )
	$OBJDOC .Load ( $STRFILE )
	$OBJDOC .setProperty ( "SelectionLanguage" , "XPath" )
	$OBJDOC .setProperty ( "SelectionNamespaces" , $STRNAMESPC )
	If $OBJDOC .parseError .errorCode > 0 Then ConsoleWrite ( $OBJDOC .parseError .reason & @LF )
	If $OBJDOC .parseError .errorCode <> 0 Then
		_XMLERROR ( "Error opening specified file: " & $STRXMLFILE & @CRLF & $OBJDOC .parseError .reason )
		SetError ( 1 , $OBJDOC .parseError .errorCode , - 1 )
		$OBJDOC = 0
		Return - 1
	EndIf
	Return 1
EndFunc
Func _XMLGETALLATTRIB ( $STRXPATH , ByRef $ANAME , ByRef $AVALUE , $STRQRY = "" )
	If Not IsObj ( $OBJDOC ) Then
		_XMLERROR ( "No object passed to function _XMLGetAllAttrib" )
		Return SetError ( 1 , 9 , - 1 )
	EndIf
	Local $OBJNODELIST , $OBJQUERYNODES , $OBJNODE , $ARRRESPONSE [ 2 ] [ 1 ] , $I
	$OBJQUERYNODES = $OBJDOC .selectNodes ( $STRXPATH & $STRQRY )
	If $OBJQUERYNODES .length > 0 Then
		For $OBJNODE In $OBJQUERYNODES
			$OBJNODELIST = $OBJNODE .attributes
			If ( $OBJNODELIST .length ) Then
				_DEBUGWRITE ( "Get all attrib " & $OBJNODELIST .length )
				ReDim $ARRRESPONSE [ 2 ] [ $OBJNODELIST .length + 2 ]
				ReDim $ANAME [ $OBJNODELIST .length ]
				ReDim $AVALUE [ $OBJNODELIST .length ]
				For $I = 0 To $OBJNODELIST .length - 1
					$ARRRESPONSE [ 0 ] [ $I + 1 ] = $OBJNODELIST .item ( $I ) .nodeName
					$ARRRESPONSE [ 1 ] [ $I + 1 ] = $OBJNODELIST .item ( $I ) .Value
					$ANAME [ $I ] = $OBJNODELIST .item ( $I ) .nodeName
					$AVALUE [ $I ] = $OBJNODELIST .item ( $I ) .Value
				Next
			Else
				_XMLERROR ( "No Attributes found for node" )
				Return SetError ( 1 , 0 , - 1 )
			EndIf
		Next
		$ARRRESPONSE [ 0 ] [ 0 ] = $OBJNODELIST .length
		Return $ARRRESPONSE
	EndIf
	_XMLERROR ( "Error retrieving attributes for: " & $STRXPATH & @CRLF )
	Return SetError ( 1 , 0 , - 1 )
EndFunc
Func _XMLGETCHILDTEXT ( $STRXPATH )
	If Not IsObj ( $OBJDOC ) Then
		_XMLERROR ( "No object passed to function _XMLGetChildText" )
		Return SetError ( 1 , 19 , - 1 )
	EndIf
	Local $OBJNODELIST , $ARRRESPONSE [ 1 ] , $XMLERR
	$OBJNODELIST = $OBJDOC .selectSingleNode ( $STRXPATH )
	If Not IsObj ( $OBJNODELIST ) Then
		_XMLERROR ( @CRLF & "No Matching Nodes found" )
		$ARRRESPONSE [ 0 ] = 0
		Return SetError ( 1 , 0 , - 1 )
	EndIf
	If $OBJNODELIST .hasChildNodes ( ) Then
		For $OBJCHILD In $OBJNODELIST .childNodes ( )
			If $OBJCHILD .nodeType = $NODE_ELEMENT Then
				_XMLARRAYADD ( $ARRRESPONSE , $OBJCHILD .baseName )
			ElseIf $OBJCHILD .nodeType = $NODE_TEXT Then
				_XMLARRAYADD ( $ARRRESPONSE , $OBJCHILD .text )
			EndIf
		Next
		$ARRRESPONSE [ 0 ] = UBound ( $ARRRESPONSE ) - 1
		Return $ARRRESPONSE
	EndIf
	$ARRRESPONSE [ 0 ] = 0
	$XMLERR = @CRLF & "No Child Text Nodes found"
	_XMLERROR ( "Error Selecting Node(s): " & $STRXPATH & $XMLERR )
	Return SetError ( 1 , 0 , - 1 )
EndFunc
Func _XMLERROR ( $SERROR = "" )
	If $SERROR = "" Then
		$SERROR = $SXML_ERROR
		$SXML_ERROR = ""
		Return $SERROR
	Else
		$SXML_ERROR = StringFormat ( $SERROR )
	EndIf
	_DEBUGWRITE ( $SXML_ERROR )
EndFunc
Func _XMLCOMEERR ( )
	_COMERRORHANDLER ( )
	Return
EndFunc
Func _COMERRORHANDLER ( $QUIET = "" )
	Local $COMERR_SILENT , $HEXNUMBER
	If $QUIET = True Or $QUIET = False Then
		$COMERR_SILENT = $QUIET
		$QUIET = ""
	EndIf
	$HEXNUMBER = Hex ( $OXMLMYERROR .number , 8 )
	If @error Then Return
	Local $MSG = "COM Error with DOM!" & @CRLF & @CRLF & "err.description is: " & @TAB & $OXMLMYERROR .description & @CRLF & "err.windescription:" & @TAB & $OXMLMYERROR .windescription & @CRLF & "err.number is: " & @TAB & $HEXNUMBER & @CRLF & "err.lastdllerror is: " & @TAB & $OXMLMYERROR .lastdllerror & @CRLF & "err.scriptline is: " & @TAB & $OXMLMYERROR .scriptline & @CRLF & "err.source is: " & @TAB & $OXMLMYERROR .source & @CRLF & "err.helpfile is: " & @TAB & $OXMLMYERROR .helpfile & @CRLF & "err.helpcontext is: " & @TAB & $OXMLMYERROR .helpcontext
	If $COMERR_SILENT <> True Then
		MsgBox ( 0 , @AutoItExe , $MSG )
	Else
		_XMLERROR ( $MSG )
	EndIf
	SetError ( 1 )
EndFunc
Func _DEBUGWRITE ( $STRMSG , $SLINEENDING = @LF )
	If $FDEBUGGING Then
		ConsoleWrite ( StringFormat ( $STRMSG ) & $SLINEENDING )
	EndIf
EndFunc
Func _XMLARRAYADD ( ByRef $AVARRAY , $SVALUE )
	If IsArray ( $AVARRAY ) Then
		ReDim $AVARRAY [ UBound ( $AVARRAY ) + 1 ]
		$AVARRAY [ UBound ( $AVARRAY ) - 1 ] = $SVALUE
		SetError ( 0 )
		Return 1
	Else
		SetError ( 1 )
		Return 0
	EndIf
EndFunc
Global Const $FO_READ = 0
Global Const $FO_OVERWRITE = 2
Func _FILEREADTOARRAY ( $SFILEPATH , ByRef $AARRAY )
	Local $HFILE = FileOpen ( $SFILEPATH , $FO_READ )
	If $HFILE = - 1 Then Return SetError ( 1 , 0 , 0 )
	Local $AFILE = FileRead ( $HFILE , FileGetSize ( $SFILEPATH ) )
	If StringRight ( $AFILE , 1 ) = @LF Then $AFILE = StringTrimRight ( $AFILE , 1 )
	If StringRight ( $AFILE , 1 ) = @CR Then $AFILE = StringTrimRight ( $AFILE , 1 )
	FileClose ( $HFILE )
	If StringInStr ( $AFILE , @LF ) Then
		$AARRAY = StringSplit ( StringStripCR ( $AFILE ) , @LF )
	ElseIf StringInStr ( $AFILE , @CR ) Then
		$AARRAY = StringSplit ( $AFILE , @CR )
	Else
		If StringLen ( $AFILE ) Then
			Dim $AARRAY [ 2 ] = [ 1 , $AFILE ]
		Else
			Return SetError ( 2 , 0 , 0 )
		EndIf
	EndIf
	Return 1
EndFunc
Func _FILEWRITEFROMARRAY ( $SFILEPATH , $AARRAY , $IBASE = 0 , $IUBOUND = 0 , $SDELIMETER = "|" )
	If Not IsArray ( $AARRAY ) Then Return SetError ( 2 , 0 , 0 )
	Local $IDIMS = UBound ( $AARRAY , 0 )
	If $IDIMS > 2 Then Return SetError ( 4 , 0 , 0 )
	Local $ILAST = UBound ( $AARRAY ) - 1
	If $IUBOUND < 1 Or $IUBOUND > $ILAST Then $IUBOUND = $ILAST
	If $IBASE < 0 Or $IBASE > $ILAST Then $IBASE = 0
	Local $HFILEOPEN
	If IsString ( $SFILEPATH ) Then
		$HFILEOPEN = FileOpen ( $SFILEPATH , $FO_OVERWRITE )
	Else
		$HFILEOPEN = $SFILEPATH
	EndIf
	If $HFILEOPEN = - 1 Then Return SetError ( 1 , 0 , 0 )
	Local $IERROR = 0
	Switch $IDIMS
	Case 1
		For $I = $IBASE To $IUBOUND
			If FileWrite ( $HFILEOPEN , $AARRAY [ $I ] & @CRLF ) = 0 Then
				$IERROR = 3
				ExitLoop
			EndIf
		Next
	Case 2
		Local $STEMP
		Local $ICOLS = UBound ( $AARRAY , 2 )
		For $I = $IBASE To $IUBOUND
			$STEMP = $AARRAY [ $I ] [ 0 ]
			For $J = 1 To $ICOLS - 1
				$STEMP &= $SDELIMETER & $AARRAY [ $I ] [ $J ]
			Next
			If FileWrite ( $HFILEOPEN , $STEMP & @CRLF ) = 0 Then
				$IERROR = 3
				ExitLoop
			EndIf
		Next
	EndSwitch
	If IsString ( $SFILEPATH ) Then FileClose ( $HFILEOPEN )
	If $IERROR Then Return SetError ( $IERROR , 0 , 0 )
	Return 1
EndFunc
Global $ISLOCALRU = False
If StringInStr ( "0419 0422 0423" , @OSLang ) Then
	$ISLOCALRU = True
EndIf
Global $SDEFAULTBROWSERTEXT = "Браузер по умолчанию: "
Global $SDATEINSTALLOSTEXT = "Дата установки ОС: "
Global $SBOOTMODETEXT = "Режим загрузки: "
Global $SWARNING = "Внимание!"
Global $SDOWNLOADUP = "Скачать обновления"
Global $SUACDISABLED = "Контроль учётных записей пользователя [b]отключен[/b]"
Global $SUACENABLED = "Контроль учётных записей пользователя [b]включен[/b]"
Global $SCONSENTPROMPTBEHAVIORADMINLOG = "Запрос на повышение прав для администраторов"
Global $SCONSENTPROMPTBEHAVIORUSERLOG = "Запрос на повышение прав для обычных пользователей"
Global $SUACLEVEL1LOG = $SUACDISABLED & " (Уровень 1)"
Global $SUACLEVEL2LOG = $SUACENABLED & " (Уровень 2)"
Global $SUACLEVEL3LOG = $SUACENABLED & " (Уровень 3)"
Global $SUACLEVEL4LOG = $SUACENABLED & " (Уровень 4)"
Global $SDISABLEDLOG = "отключен"
Global $SENABLEDLOG = "включен"
Global $SAUTOUPDATE1 = "Автоматическое обновление отключено"
Global $SAUTOUPDATE2 = "Уведомлять о загрузке и установке обновлений"
Global $SAUTOUPDATE3 = "Загружать автоматически и уведомлять об установке обновлений"
Global $SAUTOUPDATE4 = "Загружать автоматически обновления и устанавливать по заданному расписанию"
Global $SAUTOUPDATE5 = "Разрешено локальным администраторам выбирать режим конфигурации уведомления и установки для автоматического обновления"
Global $SAUTOUPDATEWSUS = "Для обновлений используется сервер WSUS"
Global $SSERVISESTAT1 = "Служба остановлена"
Global $SSERVISESTAT2 = "Служба находится в процессе запуска"
Global $SSERVISESTAT3 = "Служба находится в процессе остановки"
Global $SSERVISESTAT4 = "Служба работает"
Global $SSERVISESTAT5 = "Служба находится в процессе возобновления после приостановки"
Global $SSERVISESTAT6 = "Служба находится в процессе приостановки"
Global $SSERVISESTAT7 = "Служба приостановлена"
Global $SANTIVIRSTAT1 = "Антивирус обновлен"
Global $SANTIVIRSTAT2 = "Антивирус устарел"
Global $SANTIVIRSCANSTAT = "Сканирование [b]отключено[/b]"
Global $SSTATEAVP_DISUP = "выключен и обновлен"
Global $SSTATEAVP_DISOUT = "выключен и устарел"
Global $SSTATEAVP_ENUP = "включен и обновлен"
Global $SSTATEAVP_ENOUT = "включен и устарел"
Global $SSP_NI = "Service Pack не установлен"
Global $SCHECKCONNECT = "Проверка подключения, подождите..."
Global $SERRORCONNECT = "Ошибка подключения к интернету." & " Продолжить проверку с локальной базой?"
Global $SDOWNLOADFILE = "Закачка файла"
Global $SERRORDOWNLOADFILE = "Ошибка при скачивании файла с обновлениями." & @CRLF & "Проверьте подключение к интернету."
Global $SERRORDOWNLOADFILELOCAL = "Ошибка при скачивании файла с обновлениями. Проверьте подключение к интернету." & @CRLF & "Продолжить проверку с локальной базой?"
Global $SDOWNLOADFILEWAIT = "Закачка файла. Подождите ..."
Global $SDOWNBYTES = " скачено байт"
Global $SDONETXT = "Выполнено"
Global $SCHECKWORK = "Идет проверка, подождите..."
Global $SDATEIU = "Дата установки обновлений: "
Global $SREGEDITDISABLELOG = "Редактор реестра отключен"
Global $SCMDDISABLELOG = "Командная строка отключена"
Global $STASKMGRDISABLELOG = "Диспетчер задач отключен"
Global $SSYSTEMRESTOREDISABLELOG = "Восстановление системы отключено"
Global $SCONFIGDISABLELOG = "Настройка параметров восстановления системы отключена"
Global $SCONTROLPANELDISABLELOG = "Панель управления отключена"
Global $SFOLDEROPTIONSDISABLELOG = "Параметр ""Свойства папки"" отключен"
Global $SSECURITYTABDISABLELOG = "Вкладка ""Безопасность"" отключена"
Global $SWINDOWSDEFENDERDISABLELOG = "Защитник Windows отключен"
Global $SWINDOWSUPDATEACCESSDISABLELOG = "Доступ к функциям обновления Windows отключен"
Global $SSECUREDESKTOPDISABLELOG = "Безопасный рабочий стол отключен"
Global $SHIDDENDIRFILEDISABLELOG = "Показ скрытых файлов и папок отключен"
Global $SHIDDENPROGLOG = "[color=blue][b]<< Скрыта[/b][/color]"
Global $SNOACCESSPROCESSLOG = "Нет доступа к запущенному процессу"
Global $SFILENOTFOUNDLOG = "файл не найден!"
Global $SERRORLOG = "Ошибка"
Global $SLANGLOG = "Язык"
Global $SLICENSESTATUS = "Статус лицензии: "
Global $SMSGLICENSEMINUTES = " мин."
Global $SMSGLICENSESTATUSUNLICENSED = "Не имеет лицензии"
Global $SMSGLICENSESTATUSVL = "Срок истечения многопользовательской активации: "
Global $SMSGLICENSESTATUSTBL = "Срок истечения активации по времени: "
Global $SMSGLICENSESTATUSLICENSED = "Постоянная активация прошла успешно."
Global $SMSGLICENSESTATUSINITIALGRACE = "Срок окончания начального льготного периода: "
Global $SMSGLICENSESTATUSADDITIONALGRACE = "Срок окончания дополнительного льготного периода: "
Global $SMSGLICENSESTATUSNONGENUINEGRACE = "Срок окончания льготного периода для неподлинной версии: "
Global $SMSGLICENSESTATUSNOTIFICATION = "Windows находится в режиме уведомления"
Global $SMSGLICENSESTATUSEXTENDEDGRACE = "Срок окончания расширенного льготного периода: "
Global $SHELPUSERUACLOG = "^Рекомендуется включить уровень по умолчанию: Win+R ввести UserAccountControlSettings и Enter^"
Global $SSYSTEMANALYSISHTMLLOG = "Исследование системы завершено."
Global $SREPORTFORUMHTMLLOG = "Отчет для форума"
Global $SFORMREPORTFORUMHTMLLOG = "Сформировать отчет для форума"
Global $A00 = "http://www.microsoft.com/downloads/details.aspx?displaylang=ru&FamilyID="
Global $A26 = "Часто используемые уязвимости не обнаружены."
Global $A27 = "Обнаружено уязвимостей: "
Global $A41 = "Для установки этого обновления требуется установить SP3 для Office 200"
Global $A40 = "Уязвимость общих элементов управления Windows делает возможным удаленное выполнение кода"
Global $A44_LINK = "[b][url=http://www.microsoft.com/ru-ru/download/details.aspx?id=39667]" & $SDOWNLOADUP & "[/url][/b]"
Global $A44 = "Для установки этого обновления требуется установить SP2 для Office 2010" & " " & $A44_LINK
Global $A50 = "Уязвимость компонента Microsoft Graphics делает возможным удаленное выполнение кода"
Global $SFDOMAINPROFILELOG = "Отключен доменный профиль Брандмауэра Windows"
Global $SFPUBLICPROFILELOG = "Отключен общий профиль Брандмауэра Windows"
Global $SFSTANDARDPROFILELOG = "Отключен частный профиль Брандмауэра Windows"
Global $SSECTIONDATEINSTALL = "Установленые программы за 30 дней"
Global $SGUESTENABLEDLOG = "Учетная запись гостя включена."
Global $SPASSWORDREQUIREDLOG = "Пароль установлен."
Global $SPASSWORDNOTREQUIREDLOG = "Пароль не установлен."
Global $SVERSIONLOG = "Версия"
Global $SBUILDLOG = "Сборка"
If Not $ISLOCALRU Then
	$SDEFAULTBROWSERTEXT = "Default Browser: "
	$SDATEINSTALLOSTEXT = "Installation date OS: "
	$SBOOTMODETEXT = "Boot Mode: "
	$SWARNING = "Warning!"
	$SDOWNLOADUP = "Download Update"
	$SUACDISABLED = "User Account Control [b]disabled[/b]"
	$SUACENABLED = "User Account Control [b]enabled[/b]"
	$SCONSENTPROMPTBEHAVIORADMINLOG = "The elevation prompt for administrators"
	$SCONSENTPROMPTBEHAVIORUSERLOG = "The elevation prompt for users"
	$SUACLEVEL1LOG = $SUACDISABLED & " (Level 1)"
	$SUACLEVEL2LOG = $SUACENABLED & " (Level 2)"
	$SUACLEVEL3LOG = $SUACENABLED & " (Level 3)"
	$SUACLEVEL4LOG = $SUACENABLED & " (Level 4)"
	$SDISABLEDLOG = "disabled"
	$SENABLEDLOG = "enabled"
	$SAUTOUPDATE1 = "Never check for updates"
	$SAUTOUPDATE2 = "Notify before download"
	$SAUTOUPDATE3 = "Automatically download and notify of installatio"
	$SAUTOUPDATE4 = "Automatically download and schedule installation"
	$SAUTOUPDATE5 = "Automatic Updates is required and users can configure it"
	$SAUTOUPDATEWSUS = "The computer gets its updates from a WSUS server"
	$SSERVISESTAT1 = "The service has stopped"
	$SSERVISESTAT2 = "The service is starting"
	$SSERVISESTAT3 = "The service is stopping"
	$SSERVISESTAT4 = "The service is running"
	$SSERVISESTAT5 = "The service is about to continue"
	$SSERVISESTAT6 = "The service is pausing"
	$SSERVISESTAT7 = "The service is paused"
	$SANTIVIRSTAT1 = "Antivirus up to date!"
	$SANTIVIRSTAT2 = "Antivirus out of date!"
	$SANTIVIRSCANSTAT = "On Access scanning [b]disabled[/b]"
	$SSTATEAVP_DISUP = "disabled and up to date"
	$SSTATEAVP_DISOUT = "disabled and out of date"
	$SSTATEAVP_ENUP = "enabled and up to date"
	$SSTATEAVP_ENOUT = "enabled and out of date"
	$SSP_NI = "Service Pack not Installed"
	$SCHECKCONNECT = "Checking the connection, wait..."
	$SERRORCONNECT = "Error of connection to the Internet. Continue check with local base?"
	$SDOWNLOADFILE = "Download file"
	$SERRORDOWNLOADFILE = "Error when downloading the update file." & @CRLF & "Check your Internet connection."
	$SERRORDOWNLOADFILELOCAL = "Error when downloading the update file. Check your Internet connection." & @CRLF & "Continue check with local base?"
	$SDOWNLOADFILEWAIT = "Download the file. Wait ..."
	$SDOWNBYTES = " downloaded bytes"
	$SDONETXT = "Done"
	$SCHECKWORK = "Goes check, please wait..."
	$SDATEIU = "Date install updates: "
	$SREGEDITDISABLELOG = "Regedit Disable"
	$SCMDDISABLELOG = "CMD Disable"
	$STASKMGRDISABLELOG = "TaskMgr Disable"
	$SSYSTEMRESTOREDISABLELOG = "System Restore Disable"
	$SCONFIGDISABLELOG = "Settings System Restore Disable"
	$SCONTROLPANELDISABLELOG = "ControlPanel Disable"
	$SFOLDEROPTIONSDISABLELOG = "Parameter ""FolderOptions"" Disable"
	$SSECURITYTABDISABLELOG = "SecurityTab Disable"
	$SWINDOWSDEFENDERDISABLELOG = "Windows Defender Disable"
	$SWINDOWSUPDATEACCESSDISABLELOG = "WindowsUpdateAccess Disable"
	$SSECUREDESKTOPDISABLELOG = "SecureDesktop Disable"
	$SHIDDENDIRFILEDISABLELOG = "Show hidden folder and file Disable"
	$SHIDDENPROGLOG = "[color=blue][b]<< Hidden[/b][/color]"
	$SNOACCESSPROCESSLOG = "No access to running process"
	$SFILENOTFOUNDLOG = "file is not found!"
	$SERRORLOG = "Error"
	$SLICENSESTATUS = "LicenseStatus: "
	$SMSGLICENSEMINUTES = " minutes"
	$SMSGLICENSESTATUSUNLICENSED = "Unlicensed"
	$SMSGLICENSESTATUSVL = "Volume activation will expire : "
	$SMSGLICENSESTATUSTBL = "Timebased activation will expire :"
	$SMSGLICENSESTATUSLICENSED = "The machine is permanently activated."
	$SMSGLICENSESTATUSINITIALGRACE = "Initial grace period ends :"
	$SMSGLICENSESTATUSADDITIONALGRACE = "Additional grace period ends :"
	$SMSGLICENSESTATUSNONGENUINEGRACE = "Non-genuine grace period ends :"
	$SMSGLICENSESTATUSNOTIFICATION = "Windows is in Notification mode"
	$SMSGLICENSESTATUSEXTENDEDGRACE = "Extended grace period ends :"
	$SHELPUSERUACLOG = "^It is recommended to enable (default): Win+R typing UserAccountControlSettings and Enter^"
	$SSYSTEMANALYSISHTMLLOG = "System Analysis - complete."
	$SREPORTFORUMHTMLLOG = "Report for forum"
	$SFORMREPORTFORUMHTMLLOG = "Form report for forum"
	$A00 = "http://www.microsoft.com/downloads/details.aspx?displaylang=en&FamilyID="
	$A26 = "Frequently used critical vulnerabilities not found."
	$A27 = "Vulnerabilities found: "
	$A40 = "Vulnerability in Windows Common Controls Could Allow Remote Code Execution"
	$A41 = "This update require the SP3 installed on Office 200"
	$A44_LINK = "[b][url=http://www.microsoft.com/en-US/download/details.aspx?id=39667]" & $SDOWNLOADUP & "[/url][/b]"
	$A44 = "This update require the SP2 installed on Office 2010" & " " & $A44_LINK
	$A50 = "Vulnerability in Microsoft Graphics component could allow remote code execution"
	$SFDOMAINPROFILELOG = "Disabled the domain profile of Windows Firewall"
	$SFPUBLICPROFILELOG = "Disabled the public profile of Windows Firewall"
	$SFSTANDARDPROFILELOG = "Disabled the standard profile for Windows Firewall"
	$SSECTIONDATEINSTALL = "Installed the program for 30 days"
	$SGUESTENABLEDLOG = "Account guest is enabled."
	$SPASSWORDREQUIREDLOG = "Password required."
	$SPASSWORDNOTREQUIREDLOG = "Not require a password."
	$SVERSIONLOG = "Release"
	$SBUILDLOG = "Build"
	$SLANGLOG = "Lang"
EndIf
Global Const $BF_BOTTOM = 8
Global Const $BF_DIAGONAL = 16
Global Const $BF_LEFT = 1
Global Const $BF_RIGHT = 4
Global Const $BF_TOP = 2
Global Const $BF_RECT = BitOR ( $BF_LEFT , $BF_TOP , $BF_RIGHT , $BF_BOTTOM )
Global Const $BF_DIAGONAL_ENDBOTTOMLEFT = BitOR ( $BF_DIAGONAL , $BF_BOTTOM , $BF_LEFT )
Global Const $BF_DIAGONAL_ENDBOTTOMRIGHT = BitOR ( $BF_DIAGONAL , $BF_BOTTOM , $BF_RIGHT )
Global Const $BF_DIAGONAL_ENDTOPLEFT = BitOR ( $BF_DIAGONAL , $BF_TOP , $BF_LEFT )
Global Const $BF_DIAGONAL_ENDTOPRIGHT = BitOR ( $BF_DIAGONAL , $BF_TOP , $BF_RIGHT )
Global Const $WS_MINIMIZEBOX = 131072
Global Const $WS_SYSMENU = 524288
Global Const $WS_CAPTION = 12582912
Global Const $WS_POPUP = 2147483648
Global Const $WS_EX_TOOLWINDOW = 128
Global Const $WS_EX_TOPMOST = 8
Global Const $WS_EX_WINDOWEDGE = 256
Global Const $GUI_SS_DEFAULT_GUI = BitOR ( $WS_MINIMIZEBOX , $WS_CAPTION , $WS_POPUP , $WS_SYSMENU )
Global Const $WS_EX_PALETTEWINDOW = BitOR ( $WS_EX_TOOLWINDOW , $WS_EX_TOPMOST , $WS_EX_WINDOWEDGE )
Global Const $ASSOCSTR_EXECUTABLE = 2
Global Const $FILE_APPEND_DATA = 4
Global Const $FILE_DELETE_CHILD = 64
Global Const $FILE_EXECUTE = 32
Global Const $FILE_READ_ATTRIBUTES = 128
Global Const $FILE_READ_DATA = 1
Global Const $FILE_READ_EA = 8
Global Const $FILE_WRITE_ATTRIBUTES = 256
Global Const $FILE_WRITE_DATA = 2
Global Const $FILE_WRITE_EA = 16
Global Const $FILE_ALL_ACCESS = BitOR ( $STANDARD_RIGHTS_ALL , $FILE_APPEND_DATA , $FILE_DELETE_CHILD , $FILE_EXECUTE , $FILE_READ_ATTRIBUTES , $FILE_READ_DATA , $FILE_READ_EA , $FILE_WRITE_ATTRIBUTES , $FILE_WRITE_DATA , $FILE_WRITE_EA )
Global Const $SECTION_EXTEND_SIZE = 16
Global Const $SECTION_MAP_EXECUTE = 8
Global Const $SECTION_MAP_READ = 4
Global Const $SECTION_MAP_WRITE = 2
Global Const $SECTION_QUERY = 1
Global Const $SECTION_ALL_ACCESS = BitOR ( $STANDARD_RIGHTS_REQUIRED , $SECTION_EXTEND_SIZE , $SECTION_MAP_EXECUTE , $SECTION_MAP_READ , $SECTION_MAP_WRITE , $SECTION_QUERY )
Global Const $WINSTA_ACCESSCLIPBOARD = 4
Global Const $WINSTA_ACCESSGLOBALATOMS = 32
Global Const $WINSTA_CREATEDESKTOP = 8
Global Const $WINSTA_ENUMDESKTOPS = 1
Global Const $WINSTA_ENUMERATE = 256
Global Const $WINSTA_EXITWINDOWS = 64
Global Const $WINSTA_READATTRIBUTES = 2
Global Const $WINSTA_READSCREEN = 512
Global Const $WINSTA_WRITEATTRIBUTES = 16
Global Const $WINSTA_ALL_ACCESS = BitOR ( $WINSTA_ACCESSCLIPBOARD , $WINSTA_ACCESSGLOBALATOMS , $WINSTA_CREATEDESKTOP , $WINSTA_ENUMDESKTOPS , $WINSTA_ENUMERATE , $WINSTA_EXITWINDOWS , $WINSTA_READATTRIBUTES , $WINSTA_READSCREEN , $WINSTA_WRITEATTRIBUTES )
Global Const $DTT_TEXTCOLOR = 1
Global Const $DTT_BORDERCOLOR = 2
Global Const $DTT_SHADOWCOLOR = 4
Global Const $DTT_SHADOWTYPE = 8
Global Const $DTT_SHADOWOFFSET = 16
Global Const $DTT_BORDERSIZE = 32
Global Const $DTT_FONTPROP = 64
Global Const $DTT_COLORPROP = 128
Global Const $DTT_STATEID = 256
Global Const $DTT_CALCRECT = 512
Global Const $DTT_APPLYOVERLAY = 1024
Global Const $DTT_GLOWSIZE = 2048
Global Const $DTT_COMPOSITED = 8192
Global Const $DTT_VALIDBITS = BitOR ( $DTT_TEXTCOLOR , $DTT_BORDERCOLOR , $DTT_SHADOWCOLOR , $DTT_SHADOWTYPE , $DTT_SHADOWOFFSET , $DTT_BORDERSIZE , $DTT_FONTPROP , $DTT_COLORPROP , $DTT_STATEID , $DTT_CALCRECT , $DTT_APPLYOVERLAY , $DTT_GLOWSIZE , $DTT_COMPOSITED )
Global Const $JOB_OBJECT_ASSIGN_PROCESS = 1
Global Const $JOB_OBJECT_QUERY = 4
Global Const $JOB_OBJECT_SET_ATTRIBUTES = 2
Global Const $JOB_OBJECT_SET_SECURITY_ATTRIBUTES = 16
Global Const $JOB_OBJECT_TERMINATE = 8
Global Const $JOB_OBJECT_ALL_ACCESS = BitOR ( $STANDARD_RIGHTS_ALL , $JOB_OBJECT_ASSIGN_PROCESS , $JOB_OBJECT_QUERY , $JOB_OBJECT_SET_ATTRIBUTES , $JOB_OBJECT_SET_SECURITY_ATTRIBUTES , $JOB_OBJECT_TERMINATE )
Global Const $SEMAPHORE_MODIFY_STATE = 2
Global Const $SEMAPHORE_QUERY_STATE = 1
Global Const $SEMAPHORE_ALL_ACCESS = BitOR ( $STANDARD_RIGHTS_ALL , $SEMAPHORE_MODIFY_STATE , $SEMAPHORE_QUERY_STATE )
Global Const $HKEY_CLASSES_ROOT = 2147483648
Global Const $HKEY_CURRENT_CONFIG = 2147483653
Global Const $HKEY_CURRENT_USER = 2147483649
Global Const $HKEY_LOCAL_MACHINE = 2147483650
Global Const $HKEY_USERS = 2147483651
Global Const $KEY_CREATE_LINK = 32
Global Const $KEY_CREATE_SUB_KEY = 4
Global Const $KEY_ENUMERATE_SUB_KEYS = 8
Global Const $KEY_NOTIFY = 16
Global Const $KEY_QUERY_VALUE = 1
Global Const $KEY_SET_VALUE = 2
Global Const $KEY_WOW64_64KEY = 256
Global Const $KEY_READ = BitOR ( $STANDARD_RIGHTS_READ , $KEY_ENUMERATE_SUB_KEYS , $KEY_NOTIFY , $KEY_QUERY_VALUE )
Global Const $KEY_WRITE = BitOR ( $STANDARD_RIGHTS_WRITE , $KEY_CREATE_SUB_KEY , $KEY_SET_VALUE )
Global Const $KEY_ALL_ACCESS = BitOR ( $STANDARD_RIGHTS_REQUIRED , $KEY_CREATE_LINK , $KEY_CREATE_SUB_KEY , $KEY_ENUMERATE_SUB_KEYS , $KEY_NOTIFY , $KEY_QUERY_VALUE , $KEY_SET_VALUE )
Global Const $SHERB_NOCONFIRMATION = 1
Global Const $SHERB_NOPROGRESSUI = 2
Global Const $SHERB_NOSOUND = 4
Global Const $SHERB_NO_UI = BitOR ( $SHERB_NOCONFIRMATION , $SHERB_NOPROGRESSUI , $SHERB_NOSOUND )
Global Const $FOF_NOCONFIRMATION = 16
Global Const $FOF_NOCONFIRMMKDIR = 512
Global Const $FOF_NOERRORUI = 1024
Global Const $FOF_SILENT = 4
Global Const $FOF_NO_UI = BitOR ( $FOF_NOCONFIRMATION , $FOF_NOCONFIRMMKDIR , $FOF_NOERRORUI , $FOF_SILENT )
Global Const $SFGAO_CANCOPY = 1
Global Const $SFGAO_CANMOVE = 2
Global Const $SFGAO_CANLINK = 4
Global Const $SFGAO_STORAGE = 8
Global Const $SFGAO_CANRENAME = 16
Global Const $SFGAO_CANDELETE = 32
Global Const $SFGAO_HASPROPSHEET = 64
Global Const $SFGAO_DROPTARGET = 256
Global Const $SFGAO_CAPABILITYMASK = BitOR ( $SFGAO_CANCOPY , $SFGAO_CANMOVE , $SFGAO_CANLINK , $SFGAO_CANRENAME , $SFGAO_CANDELETE , $SFGAO_HASPROPSHEET , $SFGAO_DROPTARGET )
Global Const $SFGAO_ISSLOW = 16384
Global Const $SFGAO_GHOSTED = 32768
Global Const $SFGAO_LINK = 65536
Global Const $SFGAO_SHARE = 131072
Global Const $SFGAO_READONLY = 262144
Global Const $SFGAO_HIDDEN = 524288
Global Const $SFGAO_DISPLAYATTRMASK = BitOR ( $SFGAO_ISSLOW , $SFGAO_GHOSTED , $SFGAO_LINK , $SFGAO_SHARE , $SFGAO_READONLY , $SFGAO_HIDDEN )
Global Const $SFGAO_STREAM = 4194304
Global Const $SFGAO_STORAGEANCESTOR = 8388608
Global Const $SFGAO_VALIDATE = 16777216
Global Const $SFGAO_FILESYSANCESTOR = 268435456
Global Const $SFGAO_FOLDER = 536870912
Global Const $SFGAO_FILESYSTEM = 1073741824
Global Const $SFGAO_STORAGECAPMASK = BitOR ( $SFGAO_STORAGE , $SFGAO_LINK , $SFGAO_READONLY , $SFGAO_STREAM , $SFGAO_STORAGEANCESTOR , $SFGAO_FILESYSANCESTOR , $SFGAO_FOLDER , $SFGAO_FILESYSTEM )
Global Const $SFGAO_HASSUBFOLDER = 2147483648
Global Const $SFGAO_PKEYSFGAOMASK = BitOR ( $SFGAO_ISSLOW , $SFGAO_READONLY , $SFGAO_HASSUBFOLDER , $SFGAO_VALIDATE )
Global Const $UHID_MB = 0
Global Const $UHID_BIOS = 1
Global Const $UHID_CPU = 2
Global Const $UHID_HDD = 4
Global Const $UHID_ALL = BitOR ( $UHID_MB , $UHID_BIOS , $UHID_CPU , $UHID_HDD )
Global Const $DESKTOP_CREATEMENU = 4
Global Const $DESKTOP_CREATEWINDOW = 2
Global Const $DESKTOP_ENUMERATE = 64
Global Const $DESKTOP_HOOKCONTROL = 8
Global Const $DESKTOP_JOURNALPLAYBACK = 32
Global Const $DESKTOP_JOURNALRECORD = 16
Global Const $DESKTOP_READOBJECTS = 1
Global Const $DESKTOP_SWITCHDESKTOP = 256
Global Const $DESKTOP_WRITEOBJECTS = 128
Global Const $DESKTOP_ALL_ACCESS = BitOR ( $DESKTOP_CREATEMENU , $DESKTOP_CREATEWINDOW , $DESKTOP_ENUMERATE , $DESKTOP_HOOKCONTROL , $DESKTOP_JOURNALPLAYBACK , $DESKTOP_JOURNALRECORD , $DESKTOP_READOBJECTS , $DESKTOP_SWITCHDESKTOP , $DESKTOP_WRITEOBJECTS )
Global Const $HGDI_ERROR = Ptr ( - 1 )
Global Const $INVALID_HANDLE_VALUE = Ptr ( - 1 )
Global Const $KF_EXTENDED = 256
Global Const $KF_ALTDOWN = 8192
Global Const $KF_UP = 32768
Global Const $LLKHF_EXTENDED = BitShift ( $KF_EXTENDED , 8 )
Global Const $LLKHF_ALTDOWN = BitShift ( $KF_ALTDOWN , 8 )
Global Const $LLKHF_UP = BitShift ( $KF_UP , 8 )
Global Const $TAGMEMORYSTATUSEX = "dword Length;dword MemoryLoad;" & "uint64 TotalPhys;uint64 AvailPhys;uint64 TotalPageFile;uint64 AvailPageFile;" & "uint64 TotalVirtual;uint64 AvailVirtual;uint64 AvailExtendedVirtual"
Func _WINAPI_CLOSEHANDLE ( $HOBJECT )
	Local $ARESULT = DllCall ( "kernel32.dll" , "bool" , "CloseHandle" , "handle" , $HOBJECT )
	If @error Then Return SetError ( @error , @extended , False )
	Return $ARESULT [ 0 ]
EndFunc
Global Const $__WINVER = __WINVER ( )
Global Const $TAGPRINTDLG = "align 2;dword_ptr Size;hwnd hOwner;ptr hDevMode;ptr hDevNames;hwnd hDC;dword Flags;ushort FromPage;ushort ToPage;ushort MinPage;ushort MaxPage;" & __IIF ( @AutoItX64 , "uint" , "ushort" ) & " Copies;ptr hInstance;lparam lParam;ptr PrintHook;ptr SetupHook;ptr PrintTemplateName;ptr SetupTemplateName;ptr hPrintTemplate;ptr hSetupTemplate;"
Func _WINAPI_ASSOCQUERYSTRING ( $SASSOC , $ITYPE , $IFLAGS = 0 , $SEXTRA = "" )
	Local $TYPEOFEXTRA = "wstr"
	If Not StringStripWS ( $SEXTRA , 3 ) Then
		$TYPEOFEXTRA = "ptr"
		$SEXTRA = 0
	EndIf
	Local $RET = DllCall ( "shlwapi.dll" , "uint" , "AssocQueryStringW" , "dword" , $IFLAGS , "dword" , $ITYPE , "wstr" , $SASSOC , $TYPEOFEXTRA , $SEXTRA , "wstr" , "" , "dword*" , 4096 )
	If @error Then
		Return SetError ( 1 , 0 , "" )
	Else
		If $RET [ 0 ] Then
			Return SetError ( 1 , $RET [ 0 ] , "" )
		EndIf
	EndIf
	Return $RET [ 5 ]
EndFunc
Func _WINAPI_GETMODULEFILENAMEEX ( $HPROCESS , $HMODULE = 0 )
	Local $RET = DllCall ( @SystemDir & "\psapi.dll" , "int" , "GetModuleFileNameExW" , "ptr" , $HPROCESS , "ptr" , $HMODULE , "wstr" , "" , "int" , 4096 )
	If ( @error ) Or ( Not $RET [ 0 ] ) Then
		Return SetError ( 1 , 0 , "" )
	EndIf
	Return $RET [ 3 ]
EndFunc
Func _WINAPI_GETPROCESSFILENAME ( $PID = 0 )
	If Not $PID Then
		$PID = @AutoItPID
	EndIf
	Local $HPROCESS = DllCall ( "kernel32.dll" , "ptr" , "OpenProcess" , "dword" , __IIF ( $__WINVER < 1536 , 1040 , 4112 ) , "int" , 0 , "dword" , $PID )
	If ( @error ) Or ( Not $HPROCESS [ 0 ] ) Then
		Return SetError ( 1 , 0 , "" )
	EndIf
	Local $PATH = _WINAPI_GETMODULEFILENAMEEX ( $HPROCESS [ 0 ] )
	_WINAPI_CLOSEHANDLE ( $HPROCESS [ 0 ] )
	If Not $PATH Then
		Return SetError ( 1 , 0 , "" )
	EndIf
	Return $PATH
EndFunc
Func _WINAPI_REGCLOSEKEY ( $HKEY , $FFLUSH = 0 )
	If $FFLUSH Then
		If Not _WINAPI_REGFLUSHKEY ( $HKEY ) Then
			Return SetError ( 1 , @extended , 0 )
		EndIf
	EndIf
	Local $RET = DllCall ( "advapi32.dll" , "long" , "RegCloseKey" , "ulong_ptr" , $HKEY )
	If @error Then
		Return SetError ( 1 , 0 , 0 )
	Else
		If $RET [ 0 ] Then
			Return SetError ( 1 , $RET [ 0 ] , 0 )
		EndIf
	EndIf
	Return 1
EndFunc
Func _WINAPI_REGFLUSHKEY ( $HKEY )
	Local $RET = DllCall ( "advapi32.dll" , "long" , "RegFlushKey" , "ulong_ptr" , $HKEY )
	If @error Then
		Return SetError ( 1 , 0 , 0 )
	Else
		If $RET [ 0 ] Then
			Return SetError ( 1 , $RET [ 0 ] , 0 )
		EndIf
	EndIf
	Return 1
EndFunc
Func _WINAPI_REGOPENKEY ( $HKEY , $SSUBKEY = "" , $IACCESS = 983103 )
	Local $RET = DllCall ( "advapi32.dll" , "long" , "RegOpenKeyExW" , "ulong_ptr" , $HKEY , "wstr" , $SSUBKEY , "dword" , 0 , "dword" , $IACCESS , "ulong_ptr*" , 0 )
	If @error Then
		Return SetError ( 1 , 0 , 0 )
	Else
		If $RET [ 0 ] Then
			Return SetError ( 1 , $RET [ 0 ] , 0 )
		EndIf
	EndIf
	Return $RET [ 5 ]
EndFunc
Func _WINAPI_REGQUERYLASTWRITETIME ( $HKEY )
	Local $TFILETIME = DllStructCreate ( $TAGFILETIME )
	Local $RET = DllCall ( "advapi32.dll" , "long" , "RegQueryInfoKeyW" , "ulong_ptr" , $HKEY , "ptr" , 0 , "ptr" , 0 , "ptr" , 0 , "ptr" , 0 , "ptr" , 0 , "ptr" , 0 , "ptr" , 0 , "ptr" , 0 , "ptr" , 0 , "ptr" , 0 , "ptr" , DllStructGetPtr ( $TFILETIME ) )
	If @error Then
		Return SetError ( 1 , 0 , 0 )
	Else
		If $RET [ 0 ] Then
			Return SetError ( 1 , $RET [ 0 ] , 0 )
		EndIf
	EndIf
	Return $TFILETIME
EndFunc
Func __IIF ( $FTEST , $ITRUE , $IFALSE )
	If $FTEST Then
		Return $ITRUE
	Else
		Return $IFALSE
	EndIf
EndFunc
Func __WINVER ( )
	Local $TOSVI = DllStructCreate ( "dword;dword;dword;dword;dword;wchar[128]" )
	DllStructSetData ( $TOSVI , 1 , DllStructGetSize ( $TOSVI ) )
	Local $RET = DllCall ( "kernel32.dll" , "int" , "GetVersionExW" , "ptr" , DllStructGetPtr ( $TOSVI ) )
	If ( @error ) Or ( Not $RET [ 0 ] ) Then
		Return SetError ( 1 , 0 , 0 )
	EndIf
	Return BitOR ( BitShift ( DllStructGetData ( $TOSVI , 2 ) , - 8 ) , DllStructGetData ( $TOSVI , 3 ) )
EndFunc
Func _HEADERLOG ( $_STITLE , $_SSCRIPTDIRLOG )
	Local $RES_ER
	Local $SWEBSITE = "WebSite: www.safezone.cc"
	Local $SDATELOG = @MDAY & "." & @MON & "." & @YEAR & " " & @HOUR & ":" & @MIN & ":" & @SEC
	_FILEWRITELOG_ ( $MAINLOG , $_STITLE )
	_FILEWRITELOG_ ( $MAINLOG , $SWEBSITE )
	_FILEWRITELOG_ ( $MAINLOG , "DateLog: " & $SDATELOG )
	_FILEWRITELOG_ ( $MAINLOG , "Path starting: " & @ScriptFullPath & $SCMDLINESTART )
	_FILEWRITELOG_ ( $MAINLOG , "Log directory: " & $_SSCRIPTDIRLOG )
	If IsAdmin ( ) Then
		_FILEWRITELOG_ ( $MAINLOG , "IsAdmin: True" )
	Else
		_FILEWRITELOG_ ( $MAINLOG , "IsAdmin: False" )
	EndIf
	_FILEWRITELOG_ ( $MAINLOG , "User: " & @UserName )
EndFunc
Func _HEADERLOGWIN ( )
	Local $RES_ER
	Local $SBOOTMODE
	Local $SSYSTEMINFOLOG
	_FILEWRITELOG_ ( $MAINLOG , _STRINGREPEAT ( "_" , 75 ) )
	_FILEWRITELOG_ ( $MAINLOG , @CRLF )
	$SSYSTEMINFOLOG = _GETSYSTEMINFO ( )
	_FILEWRITELOG_ ( $MAINLOG , $SSYSTEMINFOLOG & " " & $SLANGLOG & ": " & _LANGUAGEOS ( ) )
	_FILEWRITELOG_ ( $MAINLOG , $SDATEINSTALLOSTEXT & _INSTALLDATEWINDOWS ( ) )
	If $SOSVERSION <> "WIN_XP" Then _GETSTATUSLICENSING ( )
	$SBOOTMODE = RegRead ( $HKLM & "\System\CurrentControlSet\Control\SafeBoot\Option" , "OptionValue" )
	$RES_ER = @error
	Switch $RES_ER
	Case $RES_ER = 1
		$SBOOTMODETEXT &= "Normal"
	Case $RES_ER = 0
		If $SBOOTMODE = 1 Then
			$SBOOTMODETEXT &= "Minimal"
		EndIf
		If $SBOOTMODE = 2 Then
			$SBOOTMODETEXT & = "Network"
		EndIf
	EndSwitch
	_FILEWRITELOG_ ( $MAINLOG , $SBOOTMODETEXT )
	_FILEWRITELOG_ ( $MAINLOG , $SDEFAULTBROWSERTEXT & _GETDEFAULTBROWSERMY ( ) )
	_FILEWRITELOG_ ( $MAINLOG , CHECKSYSTEMDRIVE ( ) )
EndFunc
Func _ARRAYADD1D1S ( ByRef $AVARRAY , $VVALUE1 , $IFLAG = 0 )
	If Not IsArray ( $AVARRAY ) Then Return SetError ( 1 , 0 , - 1 )
	If UBound ( $AVARRAY , 0 ) <> 1 Then Return SetError ( 2 , 0 , - 1 )
	If Not $ISWOW64 And $IFLAG Then
		Return 0
	EndIf
	$AVARRAY [ 0 ] += 1
	Local $IUBOUND = $AVARRAY [ 0 ]
	ReDim $AVARRAY [ $IUBOUND + 1 ]
	$AVARRAY [ $IUBOUND ] = $VVALUE1
	Return $IUBOUND
EndFunc
Func _REGREADKEYVALUETOARRAY ( $S_KEY , $IKEY_VALUE_MODE = 0 )
	Local $AKEYSLISTARR [ 1 ] , $ASUBKEYSLISTARR , $IINSTANCE = 0 , $SENUM_KEYVAL , $SCURRENTKEYPATH
	If Not _REGKEYEXISTS ( $S_KEY ) Then Return SetError ( 1 , 0 , $AKEYSLISTARR )
	While 1
		$IINSTANCE += 1
		If $IKEY_VALUE_MODE = 0 Then
			$SENUM_KEYVAL = RegEnumKey ( $S_KEY , $IINSTANCE )
		Else
			$SENUM_KEYVAL = RegEnumVal ( $S_KEY , $IINSTANCE )
		EndIf
		If @error <> 0 Then ExitLoop
		$SCURRENTKEYPATH = $S_KEY & "\" & $SENUM_KEYVAL
		$AKEYSLISTARR [ 0 ] += 1
		ReDim $AKEYSLISTARR [ $AKEYSLISTARR [ 0 ] + 1 ]
		$AKEYSLISTARR [ $AKEYSLISTARR [ 0 ] ] = $SCURRENTKEYPATH
		$ASUBKEYSLISTARR = _REGREADKEYVALUETOARRAY ( $SCURRENTKEYPATH )
		For $J = 1 To $ASUBKEYSLISTARR [ 0 ]
			$AKEYSLISTARR [ 0 ] += 1
			ReDim $AKEYSLISTARR [ $AKEYSLISTARR [ 0 ] + 1 ]
			$AKEYSLISTARR [ $AKEYSLISTARR [ 0 ] ] = $ASUBKEYSLISTARR [ $J ]
		Next
	WEnd
	Return $AKEYSLISTARR
EndFunc
Func _REGKEYEXISTS ( $S_KEY )
	RegRead ( $S_KEY , "" )
	Switch @error
	Case 1 , 2 , 3
		Return SetError ( @error , 0 , 0 )
Case Else
		Return SetError ( 0 , 0 , 1 )
	EndSwitch
EndFunc
Func _GETNEWVERSION ( )
	Local $ISNEWVERSION = 0
	Local $SCURVERSIONMAJOR = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "CurrentMajorVersionNumber" )
	If Not @error Then
		$ISNEWVERSION += 1
	EndIf
	Local $SCURVERSIONMINOR = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "CurrentMinorVersionNumber" )
	If Not @error Then
		$ISNEWVERSION += 1
	EndIf
	If $ISNEWVERSION = 2 Then
		Return $SCURVERSIONMAJOR & "." & $SCURVERSIONMINOR
	EndIf
	Return SetError ( 1 , 0 , 0 )
EndFunc
Func _GETSYSTEMINFO ( )
	Local $SOSNAME , $SEDITIONIDREG
	Local $SRESULT
	Local $SCURVERSIONREG = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "CurrentVersion" )
	Local $SOSBUILD = @OSBuild
	Local $SOSSERVICEPACK = @OSServicePack
	Local $SUBR
	Local $SCURRENTBUILD = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "CurrentBuild" )
	If Not @error Then
		$SOSBUILD = $SCURRENTBUILD
	EndIf
	Local $SCURVERSIONNEW = _GETNEWVERSION ( )
	If Not @error Then
		$SCURVERSIONREG = $SCURVERSIONNEW
	EndIf
	Switch $SOSVERSION
	Case "WIN_XP"
		$SOSNAME = "Windows XP"
	Case "WIN_2003"
		$SOSNAME = "Windows 2003"
	Case "WIN_VISTA"
		$SOSNAME = "Windows Vista"
	Case "WIN_7"
		$SOSNAME = "Windows 7"
	Case "WIN_2008"
		$SOSNAME = "Windows 2008"
	Case "WIN_8"
		If $SCURVERSIONREG = "6.2" Then
			$SOSNAME = "Windows 8"
		Else
			If Int ( $SOSBUILD ) < 10000 Then
				$SOSNAME = "Windows 8.1"
				$SOSVERSION = "WIN_81"
			ElseIf Int ( $SOSBUILD ) > 10000 And Int ( $SOSBUILD ) < 22000 Then
				$SOSNAME = "Windows 10"
				$SOSVERSION = "WIN_10"
			Else
				$SOSNAME = "Windows 11"
				$SOSVERSION = "WIN_11"
			EndIf
		EndIf
	Case "WIN_81"
		$SOSNAME = "Windows 8.1"
	Case "WIN_10"
		If Int ( $SOSBUILD ) > 10000 And Int ( $SOSBUILD ) < 22000 Then
			$SOSNAME = "Windows 10"
			$SOSVERSION = "WIN_10"
		Else
			$SOSNAME = "Windows 11"
			$SOSVERSION = "WIN_11"
		EndIf
Case Else
		$SOSNAME = $SOSVERSION
	EndSwitch
	$SEDITIONIDREG = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "EditionID" )
	$SGLOSBUILD = $SOSBUILD
	$SRESULT = $SOSNAME
	If $SOSSERVICEPACK Then
		$SRESULT &= " " & $SOSSERVICEPACK
	EndIf
	If $SEDITIONIDREG Then
		$SRESULT &= " " & $SEDITIONIDREG
	EndIf
	$SRESULT &= " (" & StringLower ( @OSArch ) & ")"
	If $SOSVERSION = "WIN_10" Or $SOSVERSION = "WIN_11" Then
		$SGLOSRELEASEID = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "DisplayVersion" )
		If Not @error Then
			$SRESULT &= " " & $SVERSIONLOG & ": " & $SGLOSRELEASEID
		Else
			$SGLOSRELEASEID = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "ReleaseId" )
			If Not @error Then
				$SRESULT &= " " & $SVERSIONLOG & ": " & $SGLOSRELEASEID
			EndIf
		EndIf
		$SRESULT &= " (" & $SCURVERSIONREG & "." & $SOSBUILD
		$SUBR = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "UBR" )
		If Not @error Then
			$SRESULT &= "." & $SUBR & ")"
		Else
			$SRESULT &= ")"
		EndIf
	Else
		$SRESULT &= " (" & $SCURVERSIONREG & "." & $SOSBUILD & ")"
	EndIf
	Switch $SOSVERSION
	Case "WIN_XP" , "WIN_2003" , "WIN_VISTA" , "WIN_7" , "WIN_8" , "WIN_81" , "WIN_10" , "WIN_11" , "WIN_2008"
Case Else
		MsgBox ( 4096 + 16 + 0 , $STITLE , "Версия вашей ОС (" & $SOSNAME & " " & StringLower ( @OSArch ) & ") не поддерживается программой " & @CRLF & "Работа программы будет завершена." )
		_EXIT ( )
	EndSwitch
	Return $SRESULT
EndFunc
Func _INSTALLDATEWINDOWS ( )
	Local $SSTRRESULT = ""
	Local $SINSTALLDATE = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion" , "InstallDate" )
	If Not @error Then
		$SSTRRESULT = _EPOCH_DECRYPT ( $SINSTALLDATE )
		$SSTRRESULT = StringRegExpReplace ( $SSTRRESULT , "(\d{4})/(\d{2})/(\d{2})" , "$3.$2.$1" )
	EndIf
	Return $SSTRRESULT
EndFunc
Func _GETDEFAULTBROWSERMY ( )
	Local $SNAMEBROWSER , $SPATHBROWSER
	Local $AOPENWITHLIST [ 11 ] = [ "\shell32.dll" , "\scalc.exe" , "\Excel.exe" , "\MSPaint.exe" , "\notepad.exe" , "\Winword.exe" , "\WordPad.exe" , "\OpenWith.exe" , "\OIS.EXE" , "\excelcnv.exe" , "\sublime_text.exe" ]
	Local $SPATHBROWSERAPI = _WINAPI_ASSOCQUERYSTRING ( ".html" , 2 )
	Local $IFLAG = 0
	If StringInStr ( $SPATHBROWSERAPI , "WINDOWS\system32\LaunchWinApp.exe" ) Then
		Return "Microsoft Edge (" & $SPATHBROWSERAPI & ")"
	EndIf
	For $I = 0 To UBound ( $AOPENWITHLIST ) - 1
		If StringInStr ( $SPATHBROWSERAPI , $AOPENWITHLIST [ $I ] ) Then
			$IFLAG = 1
			ExitLoop
		EndIf
	Next
	If Not $IFLAG And $SPATHBROWSERAPI Then
		Return $SPATHBROWSERAPI
	EndIf
	Local $SEXEBROWSER = RegRead ( $HKLM & "\SOFTWARE\Clients\StartMenuInternet" , "" )
	If @error Then
		Return $SPATHBROWSERAPI
	EndIf
	$SNAMEBROWSER = RegRead ( $HKLM & "\SOFTWARE\Clients\StartMenuInternet\" & $SEXEBROWSER , "" )
	If Not $SNAMEBROWSER Then
		$SNAMEBROWSER = RegRead ( $HKLM & "\SOFTWARE\Clients\StartMenuInternet\" & $SEXEBROWSER , "LocalizedString" )
	EndIf
	If StringInStr ( $SPATHBROWSERAPI , $SEXEBROWSER ) Then
		$SPATHBROWSER = $SPATHBROWSERAPI
	Else
		$SPATHBROWSER = RegRead ( $HKLM & "\SOFTWARE\Clients\StartMenuInternet\" & $SEXEBROWSER & "\shell\open\command" , "" )
		If @error Then
			$SPATHBROWSER = $SPATHBROWSERAPI
		EndIf
	EndIf
	$SPATHBROWSER = StringReplace ( $SPATHBROWSER , """" , "" )
	If Not $SNAMEBROWSER And $SPATHBROWSER Then
		Return $SPATHBROWSER
	EndIf
	If $SNAMEBROWSER And $SPATHBROWSER Then
		Return $SNAMEBROWSER & " (" & $SPATHBROWSER & ")"
	EndIf
EndFunc
Func _GETSTATUSLICENSING ( )
	If $FPRNOTEXISTSWMI Then Return 1
	Local $OWMISERVICE = ObjGet ( "winmgmts:\\.\root\cimv2" )
	If IsObj ( $OWMISERVICE ) Then
		Local $OCOLLECTION = $OWMISERVICE .ExecQuery ( "SELECT Description, LicenseStatus, GracePeriodRemaining, Name FROM SoftwareLicensingProduct WHERE PartialProductKey <> null" )
		If IsObj ( $OCOLLECTION ) Then
			For $OITEM In $OCOLLECTION
				Switch $OITEM .LicenseStatus
				Case 0
					_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSUNLICENSED )
				Case 1
					If $OITEM .GracePeriodRemaining Then
						If StringInStr ( $OITEM .Description , "TIMEBASED_" ) Then
							_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSTBL & $OITEM .GracePeriodRemaining & $SMSGLICENSEMINUTES )
						Else
							_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSVL & $OITEM .GracePeriodRemaining & $SMSGLICENSEMINUTES )
						EndIf
					Else
						_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSLICENSED )
					EndIf
				Case 2
					_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSINITIALGRACE & $OITEM .GracePeriodRemaining & $SMSGLICENSEMINUTES )
				Case 3
					_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSADDITIONALGRACE & $OITEM .GracePeriodRemaining & $SMSGLICENSEMINUTES )
				Case 4
					_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSNONGENUINEGRACE & $OITEM .GracePeriodRemaining & $SMSGLICENSEMINUTES )
				Case 5
					_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSNOTIFICATION )
				Case 6
					_FILEWRITELOG_ ( $MAINLOG , $SLICENSESTATUS & $OITEM .Name & " " & $SMSGLICENSESTATUSEXTENDEDGRACE & $OITEM .GracePeriodRemaining & $SMSGLICENSEMINUTES )
				EndSwitch
			Next
		EndIf
	EndIf
EndFunc
Func _LANGUAGEOS ( )
	Local $SOSLANG = @OSLang
	Select
	Case StringInStr ( "0419" , $SOSLANG )
		Return "Russian(" & $SOSLANG & ")"
	Case StringInStr ( "0422" , $SOSLANG )
		Return "Ukrainian(" & $SOSLANG & ")"
	Case StringInStr ( "0423" , $SOSLANG )
		Return "Belarusian(" & $SOSLANG & ")"
	Case StringInStr ( "0413 0813" , $SOSLANG )
		Return "Dutch(" & $SOSLANG & ")"
	Case StringInStr ( "0409 0809 0c09 1009 1409 1809 1c09 2009 2409 2809 2c09 3009 3409" , $SOSLANG )
		Return "English(" & $SOSLANG & ")"
	Case StringInStr ( "040c 080c 0c0c 100c 140c 180c" , $SOSLANG )
		Return "French(" & $SOSLANG & ")"
	Case StringInStr ( "0407 0807 0c07 1007 1407" , $SOSLANG )
		Return "German(" & $SOSLANG & ")"
	Case StringInStr ( "0410 0810" , $SOSLANG )
		Return "Italian(" & $SOSLANG & ")"
	Case StringInStr ( "0414 0814" , $SOSLANG )
		Return "Norwegian(" & $SOSLANG & ")"
	Case StringInStr ( "0415" , $SOSLANG )
		Return "Polish(" & $SOSLANG & ")"
	Case StringInStr ( "0416 0816" , $SOSLANG )
		Return "Portuguese(" & $SOSLANG & ")"
	Case StringInStr ( "040a 080a 0c0a 100a 140a 180a 1c0a 200a 240a 280a 2c0a 300a 340a 380a 3c0a 400a 440a 480a 4c0a 500a" , $SOSLANG )
		Return "Spanish(" & $SOSLANG & ")"
	Case StringInStr ( "041d 081d" , $SOSLANG )
		Return "Swedish(" & $SOSLANG & ")"
Case Else
		Return $SOSLANG
	EndSelect
EndFunc
Func CHECKSYSTEMDRIVE ( )
	Local $SSTRRES
	Local $ISPACETOTAL = Round ( DriveSpaceTotal ( $SYSTEMDRIVE ) / 1024 , 1 )
	Local $ISPACEFREE = Round ( DriveSpaceFree ( $SYSTEMDRIVE ) / 1024 , 1 )
	Local $ISPACEUSED = Round ( $ISPACETOTAL - $ISPACEFREE , 1 )
	Local $SFSLOG = "FS: ["
	Local $SSPACETOTALLOG = "Capacity: ["
	Local $SSPACEFREELOG = "Free: ["
	Local $SSPACEUSEDLOG = "Used: ["
	Local $SGBLOG = " Gb]"
	Local $SDRIVELOG = "SystemDrive: "
	If $ISLOCALRU Then
		$SFSLOG = "ФС: ["
		$SSPACETOTALLOG = "Емкость: ["
		$SSPACEFREELOG = "Свободно: ["
		$SSPACEUSEDLOG = "Занято: ["
		$SGBLOG = " Гб]"
		$SDRIVELOG = "Системный диск: "
	EndIf
	Local $SFS = $SFSLOG & DriveGetFileSystem ( $SYSTEMDRIVE ) & "]"
	Local $SSPACETOTAL = $SSPACETOTALLOG & $ISPACETOTAL & $SGBLOG
	Local $SSPACEFREE = $SSPACEFREELOG & $ISPACEFREE & $SGBLOG
	Local $SSPACEUSED = $SSPACEUSEDLOG & $ISPACEUSED & $SGBLOG
	$SSTRRES = $SDRIVELOG & $SYSTEMDRIVE & " " & $SFS & " " & $SSPACETOTAL & " " & $SSPACEUSED & " " & $SSPACEFREE
	Return $SSTRRES
EndFunc
Func CHECKPROGOPENLOG ( )
	If $FSILENT_CMD Then
		Return 0
	EndIf
	Local $SPATHPROG = _WINAPI_ASSOCQUERYSTRING ( ".txt" , $ASSOCSTR_EXECUTABLE )
	If FileExists ( $SPATHPROG ) Then
		ShellExecute ( $MAINLOG , "" , "" , "open" )
	Else
		If $ISLOCALRU Then
			MsgBox ( 16 + 4096 , $SERRORLOG , "Лог файл успешно создан!" & @CRLF & "Программа для открытия лога не найдена." )
		Else
			MsgBox ( 16 + 4096 , $SERRORLOG , "The log file has been successfully created!" & @CRLF & "The program to open the log was not found." )
		EndIf
	EndIf
	_CLIPLOGMAIN ( )
EndFunc
Func _CLIPLOGMAIN ( )
	Local $FILE , $TEXT , $CLIP = 0 , $RES_MSG
	$FILE = FileOpen ( $MAINLOG , 0 )
	If $FILE <> - 1 Then
		$TEXT = FileRead ( $FILE )
		If Not @error Then
			$CLIP = ClipPut ( $TEXT )
		EndIf
		FileClose ( $FILE )
		WinWaitActive ( "[REGEXPTITLE:SecurityCheck]" , "" , 5 )
		If $CLIP = 1 Then
			If $ISLOCALRU Then
				MsgBox ( 64 + 262144 , $STITLE , "Содержимое лог файла успешно скопировано в буфер обмена." )
			Else
				MsgBox ( 64 + 262144 , $STITLE , "The contents of the log file have been successfully copied to the clipboard." )
			EndIf
		Else
			MsgBox ( 16 + 262144 , "SecurityCheck" , "Содержимое лог файла не удалось скопировать в буфер обмена." )
		EndIf
	EndIf
EndFunc
Func WRITELINESECTION ( $SSECTION )
	Local $ILENALL = 75
	Local $SSECTIONDOP = " [ " & $SSECTION & " ] "
	Local $IRIGHT , $ILEFT
	Local $SSTR
	Local $SLOG = $MAINLOG
	Local $ILENSECT = StringLen ( $SSECTIONDOP )
	Local $ILENSTR
	Local $IDIFF = $ILENALL - $ILENSECT
	If $IDIFF <= 0 Then
		$SSTR = "-     " & $SSECTIONDOP & "     -"
	Else
		$IRIGHT = Int ( $IDIFF / 2 ) - 1
		$ILEFT = $IRIGHT
		If Mod ( $IDIFF , 2 ) Then
			$ILEFT += 1
		EndIf
		$SSTR = "-" & _STRINGREPEAT ( "-" , $IRIGHT ) & $SSECTIONDOP & _STRINGREPEAT ( "-" , $ILEFT ) & "-"
	EndIf
	$ILENSTR = StringLen ( $SSTR ) - 2
	_FILEWRITELOG_ ( $SLOG , $SSTR )
EndFunc
Func _FILEWRITELOG_ ( $SLOGPATH , $SLOGMSG )
	Local $HOPENFILE = $SLOGPATH , $IOPENMODE = 1 + 32
	Local $SMSG = $SLOGMSG
	If IsString ( $SLOGPATH ) Then
		$HOPENFILE = FileOpen ( $SLOGPATH , $IOPENMODE )
		If $HOPENFILE = - 1 Then
			Return SetError ( 1 , 0 , 0 )
		EndIf
	EndIf
	Local $IRETURN = FileWriteLine ( $HOPENFILE , $SMSG )
	If IsString ( $SLOGPATH ) Then
		$IRETURN = FileClose ( $HOPENFILE )
	EndIf
	If $IRETURN <= 0 Then
		Return SetError ( 2 , $IRETURN , 0 )
	EndIf
	Return $IRETURN
EndFunc
Func _EPOCH_DECRYPT ( $IEPOCHTIME )
	Local $IDAYTOADD = Int ( $IEPOCHTIME / 86400 )
	Local $ITIMEVAL = Mod ( $IEPOCHTIME , 86400 )
	If $ITIMEVAL < 0 Then
		$IDAYTOADD -= 1
		$ITIMEVAL += 86400
	EndIf
	Local $I_WFACTOR = Int ( ( 573371.75 + $IDAYTOADD ) / 36524.25 )
	Local $I_XFACTOR = Int ( $I_WFACTOR / 4 )
	Local $I_BFACTOR = 2442113 + $IDAYTOADD + $I_WFACTOR - $I_XFACTOR
	Local $I_CFACTOR = Int ( ( $I_BFACTOR - 122.1 ) / 365.25 )
	Local $I_DFACTOR = Int ( 365.25 * $I_CFACTOR )
	Local $I_EFACTOR = Int ( ( $I_BFACTOR - $I_DFACTOR ) / 30.6001 )
	Local $ADATEPART [ 3 ]
	$ADATEPART [ 2 ] = $I_BFACTOR - $I_DFACTOR - Int ( 30.6001 * $I_EFACTOR )
	$ADATEPART [ 1 ] = $I_EFACTOR - 1 - 12 * ( $I_EFACTOR - 2 > 11 )
	$ADATEPART [ 0 ] = $I_CFACTOR - 4716 + ( $ADATEPART [ 1 ] < 3 )
	Local $ATIMEPART [ 3 ]
	$ATIMEPART [ 0 ] = Int ( $ITIMEVAL / 3600 )
	$ITIMEVAL = Mod ( $ITIMEVAL , 3600 )
	$ATIMEPART [ 1 ] = Int ( $ITIMEVAL / 60 )
	$ATIMEPART [ 2 ] = Mod ( $ITIMEVAL , 60 )
	Return StringFormat ( "%.2d/%.2d/%.2d %.2d:%.2d:%.2d" , $ADATEPART [ 0 ] , $ADATEPART [ 1 ] , $ADATEPART [ 2 ] , $ATIMEPART [ 0 ] , $ATIMEPART [ 1 ] , $ATIMEPART [ 2 ] )
EndFunc
Func _EPOCH_ENCRYPT ( $DATE )
	Local $MAIN_SPLIT = StringSplit ( $DATE , " " )
	If $MAIN_SPLIT [ 0 ] - 2 Then
		Return SetError ( 1 , 0 , "" )
	EndIf
	Local $ASDATEPART = StringSplit ( $MAIN_SPLIT [ 1 ] , "/" )
	Local $ASTIMEPART = StringSplit ( $MAIN_SPLIT [ 2 ] , ":" )
	If $ASDATEPART [ 0 ] - 3 Or $ASTIMEPART [ 0 ] - 3 Then
		Return SetError ( 1 , 0 , "" )
	EndIf
	If $ASDATEPART [ 2 ] < 3 Then
		$ASDATEPART [ 2 ] += 12
		$ASDATEPART [ 1 ] -= 1
	EndIf
	Local $I_AFACTOR = Int ( $ASDATEPART [ 1 ] / 100 )
	Local $I_BFACTOR = Int ( $I_AFACTOR / 4 )
	Local $I_CFACTOR = 2 - $I_AFACTOR + $I_BFACTOR
	Local $I_EFACTOR = Int ( 1461 * ( $ASDATEPART [ 1 ] + 4716 ) / 4 )
	Local $I_FFACTOR = Int ( 153 * ( $ASDATEPART [ 2 ] + 1 ) / 5 )
	Local $ADAYSDIFF = $I_CFACTOR + $ASDATEPART [ 3 ] + $I_EFACTOR + $I_FFACTOR - 2442112
	Local $ITIMEDIFF = $ASTIMEPART [ 1 ] * 3600 + $ASTIMEPART [ 2 ] * 60 + $ASTIMEPART [ 3 ]
	Return $ADAYSDIFF * 86400 + $ITIMEDIFF
EndFunc
Func _MSOFFICE ( )
	Local $ATEXTLOG
	$ATEXTLOG = _GETOFFICEVERSION ( )
	If $ATEXTLOG [ 0 ] [ 0 ] Then
		WRITELINESECTION ( "MS Office" )
		For $I = 1 To $ATEXTLOG [ 0 ] [ 0 ]
			_FILEWRITELOG_ ( $MAINLOG , $ATEXTLOG [ $I ] [ 0 ] & " v." & $ATEXTLOG [ $I ] [ 1 ] )
		Next
	EndIf
EndFunc
Func _GETOFFICEVERSION ( )
	Dim $AOFFICE [ 10 ] [ 3 ] = [ [ 9 ] , [ "Microsoft Office XP" , "10.0" , "x86" ] , [ "Microsoft Office 2003" , "11.0" , "x86" ] , [ "Microsoft Office 2007" , "12.0" , "x86" ] , [ "Microsoft Office 2010 x86" , "14.0" , "x86" ] , [ "Microsoft Office 2010 x64" , "14.0" , "x64" ] , [ "Microsoft Office 2013 x86" , "15.0" , "x86" ] , [ "Microsoft Office 2013 x64" , "15.0" , "x64" ] , [ "Microsoft Office 2016 x86" , "16.0" , "x86" ] , [ "Microsoft Office 2016 x64" , "16.0" , "x64" ] ]
	Local $SWOW6432NODE
	Local $SKEY
	Local $SVERSION
	Local $SPATH
	Dim $SRESULT [ 1 ] [ 3 ]
	Local $_SOSARCH = @OSArch
	For $I = 1 To $AOFFICE [ 0 ] [ 0 ]
		If $_SOSARCH = "x64" And $AOFFICE [ $I ] [ 2 ] = "x86" Then
			$SWOW6432NODE = "Wow6432Node\"
		ElseIf $_SOSARCH = "x86" And $AOFFICE [ $I ] [ 2 ] = "x64" Then
			ContinueLoop
		Else
			$SWOW6432NODE = ""
		EndIf
		$SKEY = $HKLM & "\SOFTWARE\" & $SWOW6432NODE & "Microsoft\Office\" & $AOFFICE [ $I ] [ 1 ]
		$SVERSION = RegRead ( $SKEY & "\Common\ProductVersion" , "LastProduct" )
		$SPATH = RegRead ( $SKEY & "\Common\InstallRoot" , "Path" )
		If $SVERSION And $SPATH Then
			ReDim $SRESULT [ UBound ( $SRESULT ) + 1 ] [ 3 ]
			$SRESULT [ 0 ] [ 0 ] = UBound ( $SRESULT ) - 1
			$SRESULT [ UBound ( $SRESULT ) - 1 ] [ 0 ] = $AOFFICE [ $I ] [ 0 ]
			$SRESULT [ UBound ( $SRESULT ) - 1 ] [ 1 ] = $SVERSION
			$SRESULT [ UBound ( $SRESULT ) - 1 ] [ 2 ] = $SPATH
		EndIf
	Next
	Return ( $SRESULT )
EndFunc
Local $AHOTFIXES [ 1 ] = [ 0 ]
Func _CHECKHOTFIX ( $AHOTFIXXML )
	Local $ICOUNTFIX = 0
	Local $SDONWLOADALL = ""
	_GETHOTFIXESWMI ( )
	_GETHOTFIXESWUA ( )
	If $AHOTFIXES [ 0 ] Then
		$ICOUNTFIX = $AHOTFIXXML [ 0 ] [ 0 ]
		For $I = 1 To $AHOTFIXXML [ 0 ] [ 0 ]
			If _ISHOTFIXESINSTALLED ( $AHOTFIXXML [ $I ] [ 0 ] ) Then
				$AHOTFIXXML [ $I ] [ 0 ] = ""
				$ICOUNTFIX -= 1
			EndIf
		Next
	EndIf
	If $ICOUNTFIX Then
		WRITELINESECTION ( "HotFix" )
		For $I = 1 To $AHOTFIXXML [ 0 ] [ 0 ]
			If $AHOTFIXXML [ $I ] [ 0 ] Then
				If $AHOTFIXXML [ $I ] [ 2 ] Then
					$SDONWLOADALL = "[color=red][b]" & $SWARNING & " [url=" & $AHOTFIXXML [ $I ] [ 2 ] & "]" & $SDOWNLOADUP & "[/url][/b][/color]"
				Else
					$SDONWLOADALL = "[color=red][b]" & $SWARNING & " " & $SDOWNLOADUP & "[/b][/color]"
				EndIf
				_FILEWRITELOG_ ( $MAINLOG , "HotFix " & $AHOTFIXXML [ $I ] [ 0 ] & " " & $SDONWLOADALL )
				If $AHOTFIXXML [ $I ] [ 6 ] Then _FILEWRITELOG_ ( $MAINLOG , "[color=blue][b]" & $AHOTFIXXML [ $I ] [ 6 ] & "[/b][/color]" )
			EndIf
		Next
	EndIf
EndFunc
Func _GETHOTFIXESWMI ( )
	Local $SPATTERNKB = "(KB\d+)"
	If $FPRNOTEXISTSWMI Then
		Return SetError ( 1 , 0 , 0 )
	EndIf
	Local $OBJWMISERVICE = ObjGet ( "winmgmts:\\.\root\cimv2" )
	Local $COLQUICKFIXES = $OBJWMISERVICE .ExecQuery ( "SELECT HotFixID FROM Win32_QuickFixEngineering" )
	If IsObj ( $COLQUICKFIXES ) Then
		For $OBJQUICKFIX In $COLQUICKFIXES
			If StringRegExp ( $OBJQUICKFIX .HotFixID , $SPATTERNKB , 0 ) Then
				_ARRAYADD1D1S ( $AHOTFIXES , $OBJQUICKFIX .HotFixID )
			ElseIf StringRegExp ( $OBJQUICKFIX .ServicePackInEffect , $SPATTERNKB , 0 ) Then
				_ARRAYADD1D1S ( $AHOTFIXES , $OBJQUICKFIX .ServicePackInEffect )
			EndIf
		Next
	EndIf
EndFunc
Func _GETHOTFIXESWUA ( )
	Local $AFINDKB
	Local $STRHOST = @ComputerName
	Local $SPATTERNKB = "(?i)(KB\d+-?\w+?)(?=\s|$|\))"
	Local $OBJSEARCHER = _MSUPDATESESSION ( $STRHOST )
	If @error Then
		Return SetError ( 1 , 0 , 0 )
	EndIf
	Local $INTHISTORYCOUNT = $OBJSEARCHER .GetTotalHistoryCount
	Local $COLHISTORY = $OBJSEARCHER .QueryHistory ( 0 , $INTHISTORYCOUNT )
	For $OBJKB In $COLHISTORY
		$AFINDKB = StringRegExp ( $OBJKB .Title , $SPATTERNKB , 1 )
		If @error Then ContinueLoop
		For $I = 1 To $AHOTFIXES [ 0 ]
			If $AHOTFIXES [ $I ] = $AFINDKB [ 0 ] Then
				ContinueLoop 2
			EndIf
		Next
		_ARRAYADD1D1S ( $AHOTFIXES , $AFINDKB [ 0 ] )
	Next
	Return 0
EndFunc
Func _MSUPDATESESSION ( $STRHOST )
	Local $OBJSESSION = ObjCreate ( "Microsoft.Update.Session" , $STRHOST )
	If Not IsObj ( $OBJSESSION ) Then
		Return SetError ( 1 , 0 , 0 )
	EndIf
	Return $OBJSESSION .CreateUpdateSearcher
EndFunc
Func _ISHOTFIXESINSTALLED ( $STRKB )
	Local $RESKBFOUND = False
	If $IDEBUG Then
		For $I = 1 To $AHOTFIXES [ 0 ]
			_FILEWRITELOG_ ( $SSCRIPTDIRLOG & "HotFix_Debug.txt" , $AHOTFIXES [ $I ] )
		Next
	EndIf
	For $I = 1 To $AHOTFIXES [ 0 ]
		If $AHOTFIXES [ $I ] = $STRKB Then
			$RESKBFOUND = True
			ExitLoop
		EndIf
	Next
	Return $RESKBFOUND
EndFunc
Func _NOWCALC ( )
	Return ( @YEAR & "/" & @MON & "/" & @MDAY & " " & @HOUR & ":" & @MIN & ":" & @SEC )
EndFunc
Func _DATE_TIME_FILETIMETOARRAY ( ByRef $TFILETIME )
	If ( ( DllStructGetData ( $TFILETIME , 1 ) + DllStructGetData ( $TFILETIME , 2 ) ) = 0 ) Then Return SetError ( 1 , 0 , 0 )
	Local $TSYSTEMTIME = _DATE_TIME_FILETIMETOSYSTEMTIME ( $TFILETIME )
	If @error Then Return SetError ( @error , @extended , 0 )
	Return _DATE_TIME_SYSTEMTIMETOARRAY ( $TSYSTEMTIME )
EndFunc
Func _DATE_TIME_FILETIMETOSTR ( ByRef $TFILETIME , $BFMT = 0 )
	Local $ADATE = _DATE_TIME_FILETIMETOARRAY ( $TFILETIME )
	If @error Then Return SetError ( @error , @extended , "" )
	If $BFMT Then
		Return StringFormat ( "%04d/%02d/%02d %02d:%02d:%02d" , $ADATE [ 2 ] , $ADATE [ 0 ] , $ADATE [ 1 ] , $ADATE [ 3 ] , $ADATE [ 4 ] , $ADATE [ 5 ] )
	Else
		Return StringFormat ( "%02d/%02d/%04d %02d:%02d:%02d" , $ADATE [ 0 ] , $ADATE [ 1 ] , $ADATE [ 2 ] , $ADATE [ 3 ] , $ADATE [ 4 ] , $ADATE [ 5 ] )
	EndIf
EndFunc
Func _DATE_TIME_FILETIMETOSYSTEMTIME ( $PFILETIME )
	Local $TSYSTTIME = DllStructCreate ( $TAGSYSTEMTIME )
	Local $ARESULT = DllCall ( "kernel32.dll" , "bool" , "FileTimeToSystemTime" , "struct*" , $PFILETIME , "struct*" , $TSYSTTIME )
	If @error Then Return SetError ( @error , @extended , 0 )
	Return SetExtended ( $ARESULT [ 0 ] , $TSYSTTIME )
EndFunc
Func _DATE_TIME_SYSTEMTIMETOARRAY ( ByRef $TSYSTEMTIME )
	Local $AINFO [ 8 ]
	$AINFO [ 0 ] = DllStructGetData ( $TSYSTEMTIME , "Month" )
	$AINFO [ 1 ] = DllStructGetData ( $TSYSTEMTIME , "Day" )
	$AINFO [ 2 ] = DllStructGetData ( $TSYSTEMTIME , "Year" )
	$AINFO [ 3 ] = DllStructGetData ( $TSYSTEMTIME , "Hour" )
	$AINFO [ 4 ] = DllStructGetData ( $TSYSTEMTIME , "Minute" )
	$AINFO [ 5 ] = DllStructGetData ( $TSYSTEMTIME , "Second" )
	$AINFO [ 6 ] = DllStructGetData ( $TSYSTEMTIME , "MSeconds" )
	$AINFO [ 7 ] = DllStructGetData ( $TSYSTEMTIME , "DOW" )
	Return $AINFO
EndFunc
Global $ICURRENTDT_EPOCH = _EPOCH_ENCRYPT ( _NOWCALC ( ) )
Func _CHECKPROGDATEINSTALL ( )
	Local $SDATE , $SDATEFORMAT
	Local $ITIMEEPOCH
	Local $IDATECALC
	Local $SHIDDEN
	Local $IPERIODTIME = 30
	If Not $FM1PRINT_CMD Then
		Return 0
	EndIf
	Local $APROGDATE [ $APROGRAM [ 0 ] [ 0 ] + 1 ] [ 5 ]
	For $I = 1 To $APROGRAM [ 0 ] [ 0 ]
		$ITIMEEPOCH = _EPOCH_ENCRYPT ( $APROGRAM [ $I ] [ 4 ] )
		$IDATECALC = ( $ICURRENTDT_EPOCH - $ITIMEEPOCH ) / 86400
		If $IDATECALC <= $IPERIODTIME Then
			$APROGDATE [ 0 ] [ 0 ] += 1
			$APROGDATE [ $APROGDATE [ 0 ] [ 0 ] ] [ 0 ] = $APROGRAM [ $I ] [ 0 ]
			$APROGDATE [ $APROGDATE [ 0 ] [ 0 ] ] [ 1 ] = $APROGRAM [ $I ] [ 1 ]
			$APROGDATE [ $APROGDATE [ 0 ] [ 0 ] ] [ 2 ] = $APROGRAM [ $I ] [ 2 ]
			$APROGDATE [ $APROGDATE [ 0 ] [ 0 ] ] [ 3 ] = $APROGRAM [ $I ] [ 3 ]
			$APROGDATE [ $APROGDATE [ 0 ] [ 0 ] ] [ 4 ] = $APROGRAM [ $I ] [ 4 ]
		EndIf
	Next
	If $APROGDATE [ 0 ] [ 0 ] Then
		ReDim $APROGDATE [ $APROGDATE [ 0 ] [ 0 ] + 1 ] [ 5 ]
		WRITELINESECTION ( $SSECTIONDATEINSTALL )
		_ARRAYSORT ( $APROGDATE , 1 , 1 , 0 , 4 )
		For $I = 1 To $APROGDATE [ 0 ] [ 0 ]
			$SDATEFORMAT = StringRegExpReplace ( $APROGDATE [ $I ] [ 4 ] , "(\d{4})/(\d{2})/(\d{2})" , "$3.$2.$1" )
			If $APROGDATE [ $I ] [ 2 ] Then
				$SDATE = " - (" & $SDATEFORMAT & ")"
			Else
				$SDATE = "(" & $SDATEFORMAT & ")"
			EndIf
			If String ( $APROGDATE [ $I ] [ 3 ] ) = "1" Then
				$SHIDDEN = " " & $SHIDDENPROGLOG
			Else
				$SHIDDEN = ""
			EndIf
			_FILEWRITELOG_ ( $MAINLOG , $APROGDATE [ $I ] [ 0 ] & " [" & $APROGDATE [ $I ] [ 1 ] & "] ---> " & $APROGDATE [ $I ] [ 2 ] & $SDATE & $SHIDDEN )
		Next
	EndIf
EndFunc
Func _CHECKPROGALLINSTALL ( )
	Local $SDATE , $SDATEFORMAT
	Local $SHIDDEN
	If Not $FPROGALLPRINT_CMD Then
		Return 0
	EndIf
	If $APROGRAM [ 0 ] [ 0 ] Then
		WRITELINESECTION ( "Programs" )
		_ARRAYSORT ( $APROGRAM , 1 , 1 , 0 , 4 )
		For $I = 1 To $APROGRAM [ 0 ] [ 0 ]
			$SDATEFORMAT = StringRegExpReplace ( $APROGRAM [ $I ] [ 4 ] , "(\d{4})/(\d{2})/(\d{2})" , "$3.$2.$1" )
			If $APROGRAM [ $I ] [ 2 ] Then
				$SDATE = " - (" & $SDATEFORMAT & ")"
			Else
				$SDATE = "(" & $SDATEFORMAT & ")"
			EndIf
			If String ( $APROGRAM [ $I ] [ 3 ] ) = "1" Then
				$SHIDDEN = " " & $SHIDDENPROGLOG
			Else
				$SHIDDEN = ""
			EndIf
			_FILEWRITELOG_ ( $MAINLOG , $APROGRAM [ $I ] [ 0 ] & " [" & $APROGRAM [ $I ] [ 1 ] & "] ---> " & $APROGRAM [ $I ] [ 2 ] & $SDATE & $SHIDDEN )
		Next
	EndIf
EndFunc
Func _GETDATATIMEREGKEY ( $SKEYPROG )
	Local $SINSTALLDATE
	Local $HKEY , $TFT , $TST
	Local $IACCESS = $KEY_QUERY_VALUE
	Local $AKEYSUBKEY = SPLIT_SROOTKEY ( $SKEYPROG )
	If Not $AKEYSUBKEY [ 1 ] And Not $AKEYSUBKEY [ 2 ] Then
		Return SetError ( 1 , 0 , "" )
	EndIf
	If $ISWOW64 Then
		$IACCESS = BitOR ( $IACCESS , $KEY_WOW64_64KEY )
	EndIf
	$HKEY = _WINAPI_REGOPENKEY ( $AKEYSUBKEY [ 1 ] , $AKEYSUBKEY [ 2 ] , $IACCESS )
	If @error Then
		Return SetError ( 2 , 0 , "" )
	EndIf
	$TFT = _WINAPI_REGQUERYLASTWRITETIME ( $HKEY )
	_WINAPI_REGCLOSEKEY ( $HKEY )
	$SINSTALLDATE = _DATE_TIME_FILETIMETOSTR ( $TFT , 1 )
	Return $SINSTALLDATE
EndFunc
Func SPLIT_SROOTKEY ( $SROOTKEY )
	Local $AVHKEY [ 10 ] [ 3 ] = [ [ "HKCR" , "HKEY_CLASSES_ROOT" , $HKEY_CLASSES_ROOT ] , [ "HKCU" , "HKEY_CURRENT_USER" , $HKEY_CURRENT_USER ] , [ "HKLM" , "HKEY_LOCAL_MACHINE" , $HKEY_LOCAL_MACHINE ] , [ "HKU" , "HKEY_USERS" , $HKEY_USERS ] , [ "HKCC" , "HKEY_CURRENT_CONFIG" , $HKEY_CURRENT_CONFIG ] , [ "HKCR64" , "HKEY_CLASSES_ROOT64" , $HKEY_CLASSES_ROOT ] , [ "HKCU64" , "HKEY_CURRENT_USER64" , $HKEY_CURRENT_USER ] , [ "HKLM64" , "HKEY_LOCAL_MACHINE64" , $HKEY_LOCAL_MACHINE ] , [ "HKU64" , "HKEY_USERS64" , $HKEY_USERS ] , [ "HKCC64" , "HKEY_CURRENT_CONFIG64" , $HKEY_CURRENT_CONFIG ] ]
	Local $AVARRAY [ 3 ]
	If StringInStr ( $SROOTKEY , "\\" ) = 1 Then
		Local $ASCOMPUTER = StringRegExp ( $SROOTKEY , "\\\\[^\\]*\\" , 1 )
		If Not @error Then
			$AVARRAY [ 0 ] = StringTrimRight ( $ASCOMPUTER [ 0 ] , 1 )
			$SROOTKEY = StringReplace ( $SROOTKEY , $ASCOMPUTER [ 0 ] , "" , 1 )
		EndIf
	EndIf
	If StringInStr ( $SROOTKEY , "\" ) = 1 Or StringInStr ( $SROOTKEY , "\" , 0 , - 1 ) = StringLen ( $SROOTKEY ) Or StringInStr ( $SROOTKEY , "\\" ) Then
		$AVARRAY [ 0 ] = ""
		Return $AVARRAY
	Else
		Local $ASSPLIT = StringSplit ( $SROOTKEY , "\" )
		For $I = 0 To UBound ( $AVHKEY ) - 1
			If $ASSPLIT [ 1 ] = $AVHKEY [ $I ] [ 0 ] Or $ASSPLIT [ 1 ] = $AVHKEY [ $I ] [ 1 ] Then
				$AVARRAY [ 1 ] = $AVHKEY [ $I ] [ 2 ]
				ExitLoop
			EndIf
		Next
		If $AVARRAY [ 1 ] = "" Then
			$AVARRAY [ 0 ] = ""
			Return $AVARRAY
		EndIf
		For $I = 2 To $ASSPLIT [ 0 ] - 1
			$AVARRAY [ 2 ] &= $ASSPLIT [ $I ] & "\"
		Next
		If $ASSPLIT [ 0 ] > 1 Then $AVARRAY [ 2 ] &= $ASSPLIT [ $ASSPLIT [ 0 ] ]
	EndIf
	Return $AVARRAY
EndFunc
Global Const $UAC_ELEVATE_WITHOUT_PROMPTING = 0
Global Const $UAC_PROMPT_FOR_CONSENT_SECURE_DESKTOP = 2
Global Const $UAC_PROMPT_FOR_CREDENTIALS = 3
Global Const $UAC_PROMPT_FOR_CONSENT_NONWINDOWS_BINARIES = 5
Global Const $UAC_DISABLED = 0
Global Const $UAC_ENABLED = 1
Func _UAC_GETCONSENTPROMPTBEHAVIORADMIN ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_ELEVATE_WITHOUT_PROMPTING )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "ConsentPromptBehaviorAdmin" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_ELEVATE_WITHOUT_PROMPTING )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETCONSENTPROMPTBEHAVIORUSER ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , - 1 )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "ConsentPromptBehaviorUser" )
	If @error Then
		Return SetError ( @error , 0 , - 1 )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETENABLEINSTALLERDETECTION ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_DISABLED )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "EnableInstallerDetection" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_DISABLED )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETENABLELUA ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_DISABLED )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "EnableLUA" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_DISABLED )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETENABLESECUREUIAPATHS ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_DISABLED )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "EnableSecureUIAPaths" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_DISABLED )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETENABLEUIADESKTOPTOGGLE ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_DISABLED )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "EnableUIADesktopToggle" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_DISABLED )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETENABLEVIRTUALIZATION ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_DISABLED )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "EnableVirtualization" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_DISABLED )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETFILTERADMINISTRATORTOKEN ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_DISABLED )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "FilterAdministratorToken" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_DISABLED )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETPROMPTONSECUREDESKTOP ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_DISABLED )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "PromptOnSecureDesktop" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_DISABLED )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func _UAC_GETVALIDATEADMINCODESIGNATURES ( )
	If StringRegExp ( @OSVersion , "_(XP|200(0|3))" ) Then Return SetError ( - 3 , 0 , $UAC_DISABLED )
	Local $S64BIT = ""
	If @OSArch = "X64" Then $S64BIT = "64"
	Local $IRETURN = RegRead ( "HKEY_LOCAL_MACHINE" & $S64BIT & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "ValidateAdminCodeSignatures" )
	If @error Then
		Return SetError ( @error , 0 , $UAC_DISABLED )
	EndIf
	Return SetError ( @error , 0 , $IRETURN )
EndFunc
Func PLUSONE ( ByRef $IV1 , ByRef $IV2 , ByRef $IV3 , ByRef $IV4 )
	$IV1 += 1
	$IV2 += 1
	$IV3 += 1
	$IV4 += 1
EndFunc
Func _GETLEVELUAC ( )
	Local $IRETURN
	Local $ISOK = 10
	Local $ISUAC1 = 0
	Local $ISUAC2 = 0
	Local $ISUAC3 = 0
	Local $ISUAC4 = 0
	$IRETURN = _UAC_GETFILTERADMINISTRATORTOKEN ( )
	If ( $IRETURN = $UAC_DISABLED ) Then
		PLUSONE ( $ISUAC1 , $ISUAC2 , $ISUAC3 , $ISUAC4 )
	EndIf
	$IRETURN = _UAC_GETENABLEUIADESKTOPTOGGLE ( )
	If ( $IRETURN = $UAC_DISABLED ) Then
		PLUSONE ( $ISUAC1 , $ISUAC2 , $ISUAC3 , $ISUAC4 )
	EndIf
	$IRETURN = _UAC_GETCONSENTPROMPTBEHAVIORADMIN ( )
	If ( $IRETURN = $UAC_ELEVATE_WITHOUT_PROMPTING ) Then
		$ISUAC1 += 1
	ElseIf ( $IRETURN = $UAC_PROMPT_FOR_CONSENT_NONWINDOWS_BINARIES ) Then
		$ISUAC2 += 1
		$ISUAC3 += 1
	ElseIf ( $IRETURN = $UAC_PROMPT_FOR_CONSENT_SECURE_DESKTOP ) Then
		$ISUAC4 += 1
	EndIf
	$IRETURN = _UAC_GETCONSENTPROMPTBEHAVIORUSER ( )
	If ( $IRETURN = $UAC_PROMPT_FOR_CREDENTIALS ) Then
		PLUSONE ( $ISUAC1 , $ISUAC2 , $ISUAC3 , $ISUAC4 )
	EndIf
	$IRETURN = _UAC_GETENABLEINSTALLERDETECTION ( )
	If ( $IRETURN = $UAC_ENABLED ) Then
		PLUSONE ( $ISUAC1 , $ISUAC2 , $ISUAC3 , $ISUAC4 )
	EndIf
	$IRETURN = _UAC_GETVALIDATEADMINCODESIGNATURES ( )
	If ( $IRETURN = $UAC_DISABLED ) Then
		PLUSONE ( $ISUAC1 , $ISUAC2 , $ISUAC3 , $ISUAC4 )
	EndIf
	$IRETURN = _UAC_GETENABLESECUREUIAPATHS ( )
	If ( $IRETURN = $UAC_ENABLED ) Then
		PLUSONE ( $ISUAC1 , $ISUAC2 , $ISUAC3 , $ISUAC4 )
	EndIf
	$IRETURN = _UAC_GETENABLELUA ( )
	If ( $IRETURN = $UAC_DISABLED ) Then
		$ISUAC1 += 1
	ElseIf ( $IRETURN = $UAC_ENABLED ) Then
		$ISUAC2 += 1
		$ISUAC3 += 1
		$ISUAC4 += 1
	EndIf
	$IRETURN = _UAC_GETPROMPTONSECUREDESKTOP ( )
	If ( $IRETURN = $UAC_DISABLED ) Then
		$ISUAC1 += 1
		$ISUAC2 += 1
	ElseIf ( $IRETURN = $UAC_ENABLED ) Then
		$ISUAC3 += 1
		$ISUAC4 += 1
	EndIf
	$IRETURN = _UAC_GETENABLEVIRTUALIZATION ( )
	If ( $IRETURN = $UAC_ENABLED ) Then
		PLUSONE ( $ISUAC1 , $ISUAC2 , $ISUAC3 , $ISUAC4 )
	EndIf
	If $ISUAC1 = $ISOK Then
		Return 1
	EndIf
	If $ISUAC2 = $ISOK Then
		Return 2
	EndIf
	If $ISUAC3 = $ISOK Then
		Return 3
	EndIf
	If $ISUAC4 = $ISOK Then
		Return 4
	EndIf
	Return 0
EndFunc
Opt ( "MustDeclareVars" , 1 )
Global $SVERSIONPROG = "1.4.0.58"
Global $SVERSIONRELEASE = ""
Global $SDATECOMPILATION = " [15.08.24]"
Global $SAUTOR = "by glax24 & Severnyj"
Global $SNAMEPROG = "SecurityCheck"
Global $STITLE = $SNAMEPROG & " " & $SAUTOR & " v." & $SVERSIONPROG & $SVERSIONRELEASE & $SDATECOMPILATION
_SINGLETON ( $SNAMEPROG & $SVERSIONPROG & $SNAMEPROG )
Global $FSHOWFW = True
Global $FPROGALLPRINT_CMD = False
Global $FM1PRINT_CMD = False
Global $FAUTODELSCRIPT_CMD = False
Global $FHTMLLOG_CMD = False
Global $FSILENT_CMD = False
Global $FUNWANTEDAPPS_CMD = False
Global $FLOCALXML_CMD = False
Global $SPATHLOG_CMD = ""
Global $IDEBUG = 0
Global $SCMDLINESTART = ""
_COMMANDLINECHECK ( )
Global $SSCRIPTDIR = @ScriptDir
If StringRight ( $SSCRIPTDIR , 1 ) <> "\" Then $SSCRIPTDIR &= "\"
Global $SYSTEMDRIVE = StringLeft ( @WindowsDir , 2 )
Global $SSCRIPTDIRLOG = $SYSTEMDRIVE & "\SecurityCheck"
If $SPATHLOG_CMD Then $SSCRIPTDIRLOG = $SPATHLOG_CMD
If StringRight ( $SSCRIPTDIRLOG , 1 ) <> "\" Then $SSCRIPTDIRLOG &= "\"
If Not FileExists ( $SSCRIPTDIRLOG ) Then
	DirCreate ( $SSCRIPTDIRLOG )
EndIf
Global $MAINLOG = $SSCRIPTDIRLOG & "SecurityCheck.txt"
If FileExists ( $MAINLOG ) Then
	FileDelete ( $MAINLOG )
EndIf
Global $FPRNOTEXISTSWMI = False
Global $ASERVICE
Global $APROCESS
Global $APROGRAM [ 1 ] [ 5 ] = [ [ 0 ] ]
Global $SOSVERSION = @OSVersion
Global $SGLOSBUILD = ""
Global $SGLOSRELEASEID = ""
Global $ISUPDATEWINDOWS = True
Global $HKCU = "HKCU" , $HKLM = "HKLM" , $HKCR = "HKCR"
Global $ISWOW64 = False
If @OSArch = "X64" Then
	$ISWOW64 = True
	$HKCU = "HKCU64"
	$HKLM = "HKLM64"
	$HKCR = "HKCR64"
ElseIf @OSArch = "IA64" Then
	If $ISLOCALRU Then
		MsgBox ( 4096 + 16 , "Внимание!" , "Программа не поддерживает архитектуру Вашей ОС: " & @OSArch )
	Else
		MsgBox ( 4096 + 16 , "Attention!" , "The program does not support your operating system architecture: " & @OSArch )
	EndIf
	_EXIT ( )
EndIf
Global $SXMLPATHFILE = $SSCRIPTDIR & "SCUpdate.xml"
Global $SROOTX = "SecurityCheck"
Global $FPRWORKXML = True
Global $SXMLFILEINET = $SSCRIPTDIR & "SCUpdateInet.xml"
Global $LINKUPDATEXML = "http://tools.safezone.cc/glax24/SecurityCheck/SCUpdateInet.xml"
Global $SHOSTPING = "safezone.cc"
Global Const $SWMI_MONIKER = "Winmgmts:{ImpersonationLevel=Impersonate,AuthenticationLevel=PktPrivacy,(Debug,Restore,Security)}!\\.\root\cimv2"
Global $OERRORHANDLER = ObjEvent ( "AutoIt.Error" , "_ObjErrorHandler" )
Global $NWMI , $OBJECT_ERROR = 0
STARTSC ( )
Func STARTSC ( )
	GETSERVICEPROCESS ( )
	TESTWMI ( )
	_HEADERLOG ( $STITLE , $SSCRIPTDIRLOG )
	_INETDONWLOADXML ( $LINKUPDATEXML )
	SplashTextOn ( $SNAMEPROG & " " & $SAUTOR , $SCHECKWORK , 270 , 70 , - 1 , - 1 , 0 , "" , 12 )
	_OPENXML ( )
	_CHECKVERSIONSC ( )
	_HEADERLOGWIN ( )
	_INSTALLEDPROGRAMS ( )
	_WORKNODESWINXML ( )
	_WORKNODESOTHXML ( )
	If Not $FUNWANTEDAPPS_CMD Then
		_CHECKPROGDATEINSTALL ( )
		_CHECKPROGALLINSTALL ( )
	EndIf
	WRITELINESECTION ( "End of Log" )
	If $FHTMLLOG_CMD Then
		CREATEHTMLLOG ( )
	Else
		CHECKPROGOPENLOG ( )
	EndIf
	_EXIT ( )
EndFunc
Func _INETDONWLOADXML ( $LINKUPDATEFILE )
	Local $TEMPPERCENT = 0 , $PERCENT
	Local $NBYTES , $NSIZE
	Local $FSUCCESSFUL = False
	Local $IRESPING = 0
	Local $IRESMSG
	If $FLOCALXML_CMD Then
		Return 0
	EndIf
	SplashTextOn ( $SNAMEPROG & " " & $SAUTOR , $SCHECKCONNECT , 270 , 70 , - 1 , - 1 , 0 , "" , 12 )
	For $I = 0 To 2
		$IRESPING = Ping ( $SHOSTPING , 250 )
		If $IRESPING Then ExitLoop
	Next
	If Not $IRESPING Then
		$IRESMSG = MsgBox ( 4096 + 48 + 4 , $STITLE , $SERRORCONNECT )
		If $IRESMSG = 7 Then
			_EXIT ( )
		EndIf
		SplashOff ( )
		Return 1
	EndIf
	SplashOff ( )
	ProgressOn ( $SDOWNLOADFILE , "" , "" , - 1 , - 1 , 2 )
	ProgressSet ( 0 , $SDOWNLOADFILEWAIT )
	Local $HDOWNLOAD = InetGet ( $LINKUPDATEFILE , $SXMLFILEINET , 17 , 1 )
	$NSIZE = InetGetInfo ( $HDOWNLOAD , 1 )
	Do
		Sleep ( 250 )
		$PERCENT = InetGetInfo ( $HDOWNLOAD , 0 )
		If $PERCENT <> $TEMPPERCENT Then
			ProgressSet ( Ceiling ( ( $PERCENT / $NSIZE ) * 100 ) , Ceiling ( $PERCENT ) & $SDOWNBYTES )
		EndIf
		$TEMPPERCENT = $PERCENT
	Until InetGetInfo ( $HDOWNLOAD , 2 )
	$NBYTES = InetGetInfo ( $HDOWNLOAD , 0 )
	$FSUCCESSFUL = InetGetInfo ( $HDOWNLOAD , 3 )
	InetClose ( $HDOWNLOAD )
	If $NBYTES And $FSUCCESSFUL And FileExists ( $SXMLFILEINET ) Then
		$SXMLPATHFILE = $SXMLFILEINET
	Else
		$IRESMSG = MsgBox ( 4096 + 48 + 4 , $STITLE , $SERRORDOWNLOADFILELOCAL )
		If $IRESMSG = 7 Then
			_EXIT ( )
		EndIf
	EndIf
	ProgressSet ( 100 , $SDONETXT , "" )
	Sleep ( 500 )
	ProgressOff ( )
	If Not FileExists ( $SXMLPATHFILE ) Then
		MsgBox ( 4096 + 16 + 0 , $STITLE , $SERRORDOWNLOADFILE )
		_EXIT ( )
	EndIf
EndFunc
Func _WORKNODESWINXML ( )
	Local $SROOTWINDOWS
	Local $AWINDOWS
	Local $AHOTFIXXML
	Local $IERROR
	Local $ISAU_HF = 0
	If Not $FPRWORKXML Then
		Return SetError ( 1 , 0 , 0 )
	EndIf
	If $SOSVERSION = "WIN_2003" Then
		$SROOTWINDOWS = $SROOTX & "/" & "Windows" & "/WIN_XP"
	ElseIf $SOSVERSION = "WIN_10" Then
		If $SGLOSRELEASEID Then
			If StringIsInt ( $SGLOSRELEASEID ) And Int ( $SGLOSRELEASEID ) < 1607 Then
				$SROOTWINDOWS = $SROOTX & "/" & "Windows" & "/" & $SOSVERSION
			Else
				$SROOTWINDOWS = $SROOTX & "/" & "Windows" & "/" & $SOSVERSION & "_" & $SGLOSRELEASEID
			EndIf
		Else
			$SROOTWINDOWS = $SROOTX & "/" & "Windows" & "/" & $SOSVERSION
		EndIf
	ElseIf $SOSVERSION = "WIN_11" Then
		If $SGLOSRELEASEID Then
			$SROOTWINDOWS = $SROOTX & "/" & "Windows" & "/" & $SOSVERSION & "_" & $SGLOSRELEASEID
		Else
			$SROOTWINDOWS = $SROOTX & "/" & "Windows" & "/" & $SOSVERSION
		EndIf
	Else
		$SROOTWINDOWS = $SROOTX & "/" & "Windows" & "/" & $SOSVERSION
	EndIf
	$AWINDOWS = XMLGETPARAM ( $SROOTWINDOWS )
	$IERROR = @error
	$AHOTFIXXML = XMLGETPARAM ( $SROOTWINDOWS & "/Hotfix" )
	$ISAU_HF = _GETSECTIONATTRIBSNUM ( $SROOTWINDOWS & "/Hotfix" , "isAutoUpdate" )
	If Not $IERROR Then
		CHECKSP ( $AWINDOWS )
		CHECKIE ( $AWINDOWS )
		CHECKWINDOWS ( $AWINDOWS )
		If Not $ISAU_HF Or Not $ISUPDATEWINDOWS Then
			_CHECKHOTFIX ( $AHOTFIXXML )
		EndIf
		_MSOFFICE ( )
		CHECKANTIVIRUSWMI ( )
		CHECKFIREWALLWMI ( )
		CHECKFIREWALLWINDOWS ( )
		CHECKANTISPYWAREWMI ( )
	EndIf
EndFunc
Func _GETSECTIONATTRIBSNUM ( $SSECTION , $SATTRIBS )
	Dim $ANAME [ 1 ] , $AVALUE [ 1 ]
	Local $AATTRIBS
	Local $INUMBER = 0
	If $FPRWORKXML Then
		$AATTRIBS = _XMLGETALLATTRIB ( $SSECTION , $ANAME , $AVALUE , "" )
		If Not @error Then
			For $I = 1 To $AATTRIBS [ 0 ] [ 0 ]
				If $AATTRIBS [ 0 ] [ $I ] = $SATTRIBS Then
					$INUMBER = Number ( $AATTRIBS [ 1 ] [ $I ] )
				EndIf
			Next
		EndIf
	EndIf
	Return SetError ( 0 , 0 , $INUMBER )
EndFunc
Func _WORKNODESOTHXML ( )
	Local $I , $I_WIN = 0
	Local $AVALUEXML
	Local $SJAVAWIN
	Local $ACHILDS
	If Not $FPRWORKXML Then
		Return SetError ( 1 , 0 , 0 )
	EndIf
	$ACHILDS = _XMLGETCHILDTEXT ( $SROOTX )
	For $I = 1 To $ACHILDS [ 0 ]
		If $ACHILDS [ $I ] = "Windows" Then
			$I_WIN = $I
			ExitLoop
		EndIf
	Next
	For $I = 1 To $ACHILDS [ 0 ]
		If $I = $I_WIN Then
			ContinueLoop
		EndIf
		If $FUNWANTEDAPPS_CMD And $ACHILDS [ $I ] <> "UnwantedApps" Then
			ContinueLoop
		EndIf
		$AVALUEXML = XMLGETPARAM ( $SROOTX & "/" & $ACHILDS [ $I ] )
		If @error Then
			ContinueLoop
		EndIf
		CHECKVALUEXML ( $AVALUEXML , $ACHILDS [ $I ] )
	Next
EndFunc
Func _ENDSUPPORTWIN ( )
	Local $SSUPPORTEWINXP = "20140408"
	Local $SSUPPORTEWINVISTA = "20170411"
	Local $SSUPPORTEWIN7 = "20200114"
	Local $SSUPPORTEWIN81 = "20230110"
	Local $SSUPPORTEWIN2003 = "20150714"
	Local $SSUPPORTEWIN2008 = "20200114"
	Local $SDATE = @YEAR & @MON & @MDAY
	Local $SDATEESUPPORT = ""
	Local $TEXTESUPPORT
	If $SOSVERSION = "WIN_XP" And $SDATE > $SSUPPORTEWINXP Then
		$SDATEESUPPORT = StringRegExpReplace ( $SSUPPORTEWINXP , "(\d{4})(\d{2})(\d{2})" , "$3.$2.$1" )
	ElseIf $SOSVERSION = "WIN_2003" And $SDATE > $SSUPPORTEWIN2003 Then
		$SDATEESUPPORT = StringRegExpReplace ( $SSUPPORTEWIN2003 , "(\d{4})(\d{2})(\d{2})" , "$3.$2.$1" )
	ElseIf $SOSVERSION = "WIN_VISTA" And $SDATE > $SSUPPORTEWINVISTA Then
		$SDATEESUPPORT = StringRegExpReplace ( $SSUPPORTEWINVISTA , "(\d{4})(\d{2})(\d{2})" , "$3.$2.$1" )
	ElseIf $SOSVERSION = "WIN_7" And $SDATE > $SSUPPORTEWIN7 Then
		$SDATEESUPPORT = StringRegExpReplace ( $SSUPPORTEWIN7 , "(\d{4})(\d{2})(\d{2})" , "$3.$2.$1" )
	ElseIf ( $SOSVERSION = "WIN_8" Or $SOSVERSION = "WIN_81" ) And $SDATE > $SSUPPORTEWIN81 Then
		$SDATEESUPPORT = StringRegExpReplace ( $SSUPPORTEWIN81 , "(\d{4})(\d{2})(\d{2})" , "$3.$2.$1" )
	ElseIf $SOSVERSION = "WIN_2008" And $SDATE > $SSUPPORTEWIN2008 Then
		$SDATEESUPPORT = StringRegExpReplace ( $SSUPPORTEWIN2008 , "(\d{4})(\d{2})(\d{2})" , "$3.$2.$1" )
	EndIf
	If $SDATEESUPPORT Then
		If $ISLOCALRU Then
			$TEXTESUPPORT = "[color=red][b]Расширенная поддержка закончилась " & $SDATEESUPPORT & ", Ваша операционная система может быть уязвима к новым типам угроз[/b][/color]"
		Else
			$TEXTESUPPORT = "[color=red][b]Extended support has ended " & $SDATEESUPPORT & ", Your operating system may be vulnerable to new types of threats[/b][/color]"
		EndIf
		_FILEWRITELOG_ ( $MAINLOG , $TEXTESUPPORT )
	EndIf
EndFunc
Func CHECKSP ( ByRef $AWINDOWS )
	Local $J
	Local $SOSSERVICEPACK = @OSServicePack
	If Not $SOSSERVICEPACK Then $SOSSERVICEPACK = $SSP_NI
	Local $ICHECKVERPROG
	Local $SVERPROGXMLOLD = ""
	Local $SDONWLOADALL = ""
	Local $TEXTESUPPORT = ""
	WRITELINESECTION ( "Windows" )
	_ENDSUPPORTWIN ( )
	For $J = 1 To $AWINDOWS [ 0 ] [ 0 ]
		If $AWINDOWS [ $J ] [ 0 ] = "" Then
			ContinueLoop
		EndIf
		If $SOSVERSION = "WIN_10" Or $SOSVERSION = "WIN_11" Then
			If StringInStr ( "UpdateWin" , $AWINDOWS [ $J ] [ 0 ] ) Then
				If $AWINDOWS [ $J ] [ 2 ] Then
					$SDONWLOADALL = "[color=red][b]" & $SWARNING & " [url=" & $AWINDOWS [ $J ] [ 2 ] & "]" & $SDOWNLOADUP & "[/url][/b][/color]"
				Else
					$SDONWLOADALL = "[color=red][b]" & $SWARNING & " " & $SDOWNLOADUP & "[/b][/color]"
				EndIf
				If $ISLOCALRU Then
					$TEXTESUPPORT = "Расширенная поддержка закончилась " & $SDONWLOADALL
				Else
					$TEXTESUPPORT = "Extended support has ended " & $SDONWLOADALL
				EndIf
				_FILEWRITELOG_ ( $MAINLOG , $TEXTESUPPORT )
				If $AWINDOWS [ $J ] [ 6 ] Then _FILEWRITELOG_ ( $MAINLOG , "[color=blue][b]" & $AWINDOWS [ $J ] [ 6 ] & "[/b][/color]" )
				$AWINDOWS [ $J ] [ 0 ] = ""
				ExitLoop
			EndIf
		EndIf
		If StringInStr ( "ServicePack" , $AWINDOWS [ $J ] [ 0 ] ) Then
			$ICHECKVERPROG = CHECKVERSIONPROG ( @OSServicePack , $AWINDOWS [ $J ] [ 1 ] )
			$SVERPROGXMLOLD = ""
			If $ICHECKVERPROG = 1 Then
				If $AWINDOWS [ $J ] [ 2 ] Then
					$SDONWLOADALL = "[color=red][b]" & $SWARNING & " [url=" & $AWINDOWS [ $J ] [ 2 ] & "]" & $SDOWNLOADUP & "[/url][/b][/color]"
				Else
					$SDONWLOADALL = "[color=red][b]" & $SWARNING & " " & $SDOWNLOADUP & "[/b][/color]"
				EndIf
				_FILEWRITELOG_ ( $MAINLOG , $SOSSERVICEPACK & " " & $SDONWLOADALL )
				If $AWINDOWS [ $J ] [ 6 ] Then _FILEWRITELOG_ ( $MAINLOG , "[color=blue][b]" & $AWINDOWS [ $J ] [ 6 ] & "[/b][/color]" )
			Else
				If @OSServicePack Then
					If $ICHECKVERPROG < 0 Then
						$SVERPROGXMLOLD = " [b][+][/b]"
						_FILEWRITELOG_ ( $MAINLOG , @OSServicePack & $SVERPROGXMLOLD )
					EndIf
				Else
					If $AWINDOWS [ $J ] [ 2 ] Then
						$SDONWLOADALL = "[color=red][b]" & $SWARNING & " [url=" & $AWINDOWS [ $J ] [ 2 ] & "]" & $SDOWNLOADUP & "[/url][/b][/color]"
					Else
						$SDONWLOADALL = "[color=red][b]" & $SWARNING & " " & $SDOWNLOADUP & "[/b][/color]"
					EndIf
					_FILEWRITELOG_ ( $MAINLOG , $SOSSERVICEPACK & " " & $SDONWLOADALL )
					If $AWINDOWS [ $J ] [ 6 ] Then _FILEWRITELOG_ ( $MAINLOG , "[color=blue][b]" & $AWINDOWS [ $J ] [ 6 ] & "[/b][/color]" )
				EndIf
			EndIf
			$AWINDOWS [ $J ] [ 0 ] = ""
			ExitLoop
		EndIf
	Next
EndFunc
Func CHECKIE ( ByRef $AWINDOWS )
	Local $J
	Local $ICHECKVERPROG
	Local $SDONWLOADALL = ""
	Local $SVERPROGXMLOLD = ""
	Local $SIE_VERSION = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Internet Explorer" , "svcVersion" )
	If @error Or Not $SIE_VERSION Then
		$SIE_VERSION = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Internet Explorer" , "Version" )
		If @error Or Not $SIE_VERSION Then
			_FILEWRITELOG_ ( $MAINLOG , "Internet Explorer [color=red]Error Version[/color]" )
			Return 1
		EndIf
	EndIf
	For $J = 1 To $AWINDOWS [ 0 ] [ 0 ]
		If $AWINDOWS [ $J ] [ 0 ] = "" Then
			ContinueLoop
		EndIf
		If StringInStr ( "InternetExplorer" , $AWINDOWS [ $J ] [ 0 ] ) Then
			$ICHECKVERPROG = CHECKVERSIONPROG ( $SIE_VERSION , $AWINDOWS [ $J ] [ 1 ] )
			$SVERPROGXMLOLD = ""
			If $ICHECKVERPROG = 1 Then
				If $AWINDOWS [ $J ] [ 2 ] Then
					$SDONWLOADALL = "[color=red][b]" & $SWARNING & " [url=" & $AWINDOWS [ $J ] [ 2 ] & "]" & $SDOWNLOADUP & "[/url][/b][/color]"
				Else
					$SDONWLOADALL = "[color=red][b]" & $SWARNING & " " & $SDOWNLOADUP & "[/b][/color]"
				EndIf
				_FILEWRITELOG_ ( $MAINLOG , "Internet Explorer " & $SIE_VERSION & " " & $SDONWLOADALL )
				If $AWINDOWS [ $J ] [ 6 ] Then _FILEWRITELOG_ ( $MAINLOG , "[color=blue][b]" & $AWINDOWS [ $J ] [ 6 ] & "[/b][/color]" )
			Else
				If $ICHECKVERPROG < 0 Then $SVERPROGXMLOLD = " [b][+][/b]"
				_FILEWRITELOG_ ( $MAINLOG , "Internet Explorer " & $SIE_VERSION & $SVERPROGXMLOLD )
			EndIf
			$AWINDOWS [ $J ] [ 0 ] = ""
			ExitLoop
		EndIf
	Next
EndFunc
Func CHECKWINDOWS ( ByRef $AWINDOWS )
	Local $SAUOPTIONS
	Local $SAUTEXT = ""
	Local $SNOAUTOUPDATE
	Local $SLASTSUCCESSTIME = ""
	Local $RES_ER = 0
	Local $SSTATUSSERVICE = ""
	Local $SPATHFILEDIR = ""
	Local $SFDEXISTS = ""
	_GETUAC ( )
	$SAUOPTIONS = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update" , "AUOptions" )
	$RES_ER = @error
	If Not $RES_ER Then
		If String ( $SAUOPTIONS ) = "1" Then
			$SAUTEXT = "[color=red][b]" & $SAUTOUPDATE1 & "[/b][/color]"
			$ISUPDATEWINDOWS = False
		ElseIf String ( $SAUOPTIONS ) = "2" Then
			$SAUTEXT = $SAUTOUPDATE2
		ElseIf String ( $SAUOPTIONS ) = "3" Then
			$SAUTEXT = $SAUTOUPDATE3
		ElseIf String ( $SAUOPTIONS ) = "4" Then
			$SAUTEXT = $SAUTOUPDATE4
		ElseIf String ( $SAUOPTIONS ) = "5" Then
			$SAUTEXT = $SAUTOUPDATE5
		Else
			$SAUTEXT = "[color=red][b]Automatic Updates UNKNOWN " & $SAUOPTIONS & "[/b][/color]"
		EndIf
	Else
		If $SOSVERSION <> "WIN_10" And $SOSVERSION <> "WIN_11" Then
			$SAUTEXT = "[color=red][b]" & $SAUTOUPDATE1 & " (" & $RES_ER & ")[/b][/color]"
			$ISUPDATEWINDOWS = False
		EndIf
	EndIf
	If $SAUTEXT Then
		_FILEWRITELOG_ ( $MAINLOG , $SAUTEXT )
	EndIf
	If $SOSVERSION = "WIN_10" Or $SOSVERSION = "WIN_11" Then
		$SAUTEXT = ""
		$SNOAUTOUPDATE = RegRead ( $HKLM & "\SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" , "NoAutoUpdate" )
		$RES_ER = @error
		If Not $RES_ER Then
			If String ( $SNOAUTOUPDATE ) = "1" Then
				_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]" & $SAUTOUPDATE1 & "[/b][/color]" )
				$ISUPDATEWINDOWS = False
			EndIf
		EndIf
		If String ( $SNOAUTOUPDATE ) <> "1" Then
			$SAUTEXT = ""
			$SAUOPTIONS = RegRead ( $HKLM & "\SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" , "AUOptions" )
			$RES_ER = @error
			If Not $RES_ER Then
				If String ( $SAUOPTIONS ) = "2" Then
					$SAUTEXT = $SAUTOUPDATE2
				ElseIf String ( $SAUOPTIONS ) = "3" Then
					$SAUTEXT = $SAUTOUPDATE3
				ElseIf String ( $SAUOPTIONS ) = "4" Then
					$SAUTEXT = $SAUTOUPDATE4
				ElseIf String ( $SAUOPTIONS ) = "5" Then
					$SAUTEXT = $SAUTOUPDATE5
				EndIf
				If $SAUTEXT Then
					_FILEWRITELOG_ ( $MAINLOG , $SAUTEXT )
				EndIf
			EndIf
		EndIf
		$SAUTEXT = ""
		$SAUOPTIONS = RegRead ( $HKLM & "\SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" , "UseWUServer" )
		$RES_ER = @error
		If Not $RES_ER Then
			If String ( $SAUOPTIONS ) = "1" Then
				$SAUTEXT = $SAUTOUPDATEWSUS
			EndIf
			If $SAUTEXT Then
				_FILEWRITELOG_ ( $MAINLOG , $SAUTEXT )
			EndIf
		EndIf
	EndIf
	If $SOSVERSION = "WIN_10" Or $SOSVERSION = "WIN_11" Then
	Else
		$SLASTSUCCESSTIME = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update\Results\Install" , "LastSuccessTime" )
		If Not @error Then
			_FILEWRITELOG_ ( $MAINLOG , $SDATEIU & $SLASTSUCCESSTIME )
		EndIf
	EndIf
	For $J = 1 To $AWINDOWS [ 0 ] [ 0 ]
		If $AWINDOWS [ $J ] [ 3 ] Then
			$SSTATUSSERVICE = GETSTATUSSERVICE ( $AWINDOWS [ $J ] [ 3 ] )
			If $SSTATUSSERVICE Then _FILEWRITELOG_ ( $MAINLOG , $SSTATUSSERVICE )
		EndIf
	Next
	For $J = 1 To $AWINDOWS [ 0 ] [ 0 ]
		If $AWINDOWS [ $J ] [ 4 ] Then
			$SPATHFILEDIR = $SYSTEMDRIVE & $AWINDOWS [ $J ] [ 4 ]
			$SFDEXISTS = _FILEDIREXISTS ( $SPATHFILEDIR )
			If $SFDEXISTS Then _FILEWRITELOG_ ( $MAINLOG , $SFDEXISTS )
		EndIf
	Next
	ADMINRESTRICTIONS ( )
	CHECKGUESTWMI ( )
EndFunc
Func CHECKANTIVIRUSWMI ( )
	Local $STRCOMPUTER = "."
	Local $ATEXTLOG [ 1 ] = [ 0 ]
	Local $SSTATE
	If $FPRNOTEXISTSWMI Then Return 1
	If ( $SOSVERSION = "WIN_VISTA" ) Or ( $SOSVERSION = "WIN_7" ) Or ( $SOSVERSION = "WIN_8" ) Or ( $SOSVERSION = "WIN_81" ) Or ( $SOSVERSION = "WIN_10" ) Or ( $SOSVERSION = "WIN_11" ) Then
		Local $OBJWMISERVICE = ObjGet ( "winmgmts:\\" & $STRCOMPUTER & "\root\SecurityCenter2" )
		If Not IsObj ( $OBJWMISERVICE ) Then Return 1
		Local $COLITEMS = $OBJWMISERVICE .ExecQuery ( "SELECT * FROM AntiVirusProduct" )
		If IsObj ( $COLITEMS ) Then
			For $OBJITEM In $COLITEMS
				$SSTATE = _GETSTATEAVP ( $OBJITEM .ProductState )
				_ARRAYADD1D1S ( $ATEXTLOG , $OBJITEM .DisplayName & $SSTATE )
			Next
		EndIf
	ElseIf $SOSVERSION = "WIN_XP" Or $SOSVERSION = "WIN_2003" Then
		Local $OBJWMISERVICE = ObjGet ( "winmgmts:\\" & $STRCOMPUTER & "\root\SecurityCenter" )
		If Not IsObj ( $OBJWMISERVICE ) Then Return 1
		Local $COLITEMS = $OBJWMISERVICE .ExecQuery ( "SELECT * FROM AntiVirusProduct" )
		If IsObj ( $COLITEMS ) Then
			For $OBJITEM In $COLITEMS
				$SSTATE = _GETSTATEAVPXP ( $OBJITEM .onAccessScanningEnabled , $OBJITEM .productUptoDate )
				_ARRAYADD1D1S ( $ATEXTLOG , $OBJITEM .DisplayName & $SSTATE )
			Next
		EndIf
	EndIf
	If $ATEXTLOG [ 0 ] Then
		WRITELINESECTION ( "Antivirus_WMI" )
		For $I = 1 To $ATEXTLOG [ 0 ]
			_FILEWRITELOG_ ( $MAINLOG , $ATEXTLOG [ $I ] )
		Next
	EndIf
EndFunc
Func CHECKFIREWALLWMI ( )
	Local $STRCOMPUTER = "."
	Local $ATEXTLOG [ 1 ] = [ 0 ]
	Local $SSTATE
	If $FPRNOTEXISTSWMI Then Return 1
	If ( $SOSVERSION = "WIN_VISTA" ) Or ( $SOSVERSION = "WIN_7" ) Or ( $SOSVERSION = "WIN_8" ) Or ( $SOSVERSION = "WIN_81" ) Or ( $SOSVERSION = "WIN_10" ) Or ( $SOSVERSION = "WIN_11" ) Then
		Local $OBJWMISERVICE = ObjGet ( "winmgmts:\\" & $STRCOMPUTER & "\root\SecurityCenter2" )
		If Not IsObj ( $OBJWMISERVICE ) Then Return 1
		Local $COLITEMS = $OBJWMISERVICE .ExecQuery ( "SELECT * FROM FirewallProduct" )
		If IsObj ( $COLITEMS ) Then
			For $OBJITEM In $COLITEMS
				$SSTATE = _GETSTATEFIREWALL ( $OBJITEM .ProductState )
				_ARRAYADD1D1S ( $ATEXTLOG , $OBJITEM .DisplayName & $SSTATE )
			Next
		EndIf
	ElseIf $SOSVERSION = "WIN_XP" Or $SOSVERSION = "WIN_2003" Then
		Local $OBJWMISERVICE = ObjGet ( "winmgmts:\\" & $STRCOMPUTER & "\root\SecurityCenter" )
		If Not IsObj ( $OBJWMISERVICE ) Then Return 1
		Local $COLITEMS = $OBJWMISERVICE .ExecQuery ( "SELECT * FROM FirewallProduct" )
		If IsObj ( $COLITEMS ) Then
			For $OBJITEM In $COLITEMS
				$SSTATE = _GETSTATEFIREWALLXP ( $OBJITEM .enabled )
				_ARRAYADD1D1S ( $ATEXTLOG , $OBJITEM .DisplayName & $SSTATE )
			Next
		EndIf
	EndIf
	If $ATEXTLOG [ 0 ] Then
		$FSHOWFW = False
		WRITELINESECTION ( "Firewall_WMI" )
		For $I = 1 To $ATEXTLOG [ 0 ]
			_FILEWRITELOG_ ( $MAINLOG , $ATEXTLOG [ $I ] )
		Next
	EndIf
EndFunc
Func _GETSTATEAVP ( $ISTATE )
	Local $SSTR = ""
	Local $SHEX
	Local $SMID
	Switch $ISTATE
	Case 266240 , 397568 , 397312
		$SSTR = " (" & $SSTATEAVP_ENUP & ")"
	Case 262144 , 393472 , 393216
		$SSTR = " (" & $SSTATEAVP_DISUP & ")"
	Case 397584 , 266256 , 397328
		$SSTR = " (" & $SSTATEAVP_ENOUT & ")"
	Case 262160 , 393232 , 393488 , 270336
		$SSTR = " (" & $SSTATEAVP_DISOUT & ")"
	EndSwitch
	If Not $SSTR Then
		$SHEX = String ( Hex ( $ISTATE , 5 ) )
		$SMID = StringMid ( $SHEX , 2 , 2 )
		If $SMID = "10" Or $SMID = "11" Then
			$SSTR = " (" & $SENABLEDLOG & ")"
		Else
			$SSTR = " (" & $SDISABLEDLOG & ")"
		EndIf
	EndIf
	Return $SSTR
EndFunc
Func _GETSTATEAVPXP ( $FSCANNINGENABLED , $FUPTODATE )
	Local $SSTR = ""
	If $FSCANNINGENABLED And $FUPTODATE Then
		$SSTR = " (" & $SSTATEAVP_ENUP & ")"
	ElseIf $FSCANNINGENABLED And Not $FUPTODATE Then
		$SSTR = " (" & $SSTATEAVP_ENOUT & ")"
	ElseIf Not $FSCANNINGENABLED And $FUPTODATE Then
		$SSTR = " (" & $SSTATEAVP_DISUP & ")"
	ElseIf Not $FSCANNINGENABLED And Not $FUPTODATE Then
		$SSTR = " (" & $SSTATEAVP_DISOUT & ")"
	EndIf
	Return $SSTR
EndFunc
Func _GETSTATEFIREWALL ( $ISTATE )
	Local $SSTR = ""
	Switch $ISTATE
	Case 266240 , 397568 , 397312
		$SSTR = " (" & $SENABLEDLOG & ")"
	Case 262144 , 393472 , 393216
		$SSTR = " (" & $SDISABLEDLOG & ")"
	Case 397584 , 266256 , 397328
		$SSTR = " (" & $SENABLEDLOG & ")"
	Case 262160 , 393232 , 393488
		$SSTR = " (" & $SDISABLEDLOG & ")"
	EndSwitch
	Return $SSTR
EndFunc
Func _GETSTATEFIREWALLXP ( $ISTATE )
	Local $SSTR = ""
	If $ISTATE Then
		$SSTR = " (" & $SENABLEDLOG & ")"
	Else
		$SSTR = " (" & $SDISABLEDLOG & ")"
	EndIf
	Return $SSTR
EndFunc
Func CHECKANTISPYWAREWMI ( )
	Local $STRCOMPUTER = "."
	Local $ATEXTLOG [ 1 ] = [ 0 ]
	Local $SSTATE
	If $FPRNOTEXISTSWMI Then Return 1
	If ( $SOSVERSION = "WIN_VISTA" ) Or ( $SOSVERSION = "WIN_7" ) Or ( $SOSVERSION = "WIN_8" ) Or ( $SOSVERSION = "WIN_81" ) Or ( $SOSVERSION = "WIN_10" ) Or ( $SOSVERSION = "WIN_11" ) Then
		Local $OBJWMISERVICE = ObjGet ( "winmgmts:\\" & $STRCOMPUTER & "\root\SecurityCenter2" )
		If Not IsObj ( $OBJWMISERVICE ) Then Return 1
		Local $COLITEMS = $OBJWMISERVICE .ExecQuery ( "SELECT * FROM AntiSpywareProduct" )
		If IsObj ( $COLITEMS ) Then
			For $OBJITEM In $COLITEMS
				$SSTATE = _GETSTATEAVP ( $OBJITEM .ProductState )
				_ARRAYADD1D1S ( $ATEXTLOG , $OBJITEM .DisplayName & $SSTATE )
			Next
		EndIf
	EndIf
	If $ATEXTLOG [ 0 ] Then
		WRITELINESECTION ( "AntiSpyware_WMI" )
		For $I = 1 To $ATEXTLOG [ 0 ]
			_FILEWRITELOG_ ( $MAINLOG , $ATEXTLOG [ $I ] )
		Next
	EndIf
EndFunc
Func _GETSECTIONHIDDEN ( $SSECTION )
	Dim $ANAME [ 1 ] , $AVALUE [ 1 ]
	Local $AATTRIBS
	Local $ISHIDDEN = 0
	If $FPRWORKXML Then
		$AATTRIBS = _XMLGETALLATTRIB ( $SROOTX & "/" & $SSECTION , $ANAME , $AVALUE , "" )
		If Not @error Then
			For $I = 1 To $AATTRIBS [ 0 ] [ 0 ]
				If $AATTRIBS [ 0 ] [ $I ] = "Hidden" Then
					$ISHIDDEN = Number ( $AATTRIBS [ 1 ] [ $I ] )
				EndIf
			Next
		EndIf
	EndIf
	Return SetError ( 0 , 0 , $ISHIDDEN )
EndFunc
Func CHECKFIREWALLWINDOWS ( )
	Local $SSTATUSSERVICE
	Local $SENABLE
	Local $IERROR
	If Not $FSHOWFW Then
		Return 0
	EndIf
	If ( $SOSVERSION = "WIN_VISTA" ) Or ( $SOSVERSION = "WIN_7" ) Or ( $SOSVERSION = "WIN_8" ) Or ( $SOSVERSION = "WIN_81" ) Or ( $SOSVERSION = "WIN_10" ) Or ( $SOSVERSION = "WIN_11" ) Then
		WRITELINESECTION ( "FirewallWindows" )
		$SSTATUSSERVICE = GETSTATUSSERVICE ( "MpsSvc" )
		If $SSTATUSSERVICE Then
			_FILEWRITELOG_ ( $MAINLOG , $SSTATUSSERVICE )
		EndIf
		$SENABLE = RegRead ( $HKLM & "\SYSTEM\CurrentControlSet\Services\SharedAccess\Parameters\FirewallPolicy\DomainProfile" , "EnableFirewall" )
		$IERROR = @error
		If Not $IERROR Then
			If String ( $SENABLE ) = "0" Then
				_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]" & $SFDOMAINPROFILELOG & "[/b][/color]" )
			EndIf
		Else
			_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]" & $SFDOMAINPROFILELOG & "[/b][/color]" )
		EndIf
		$SENABLE = RegRead ( $HKLM & "\SYSTEM\CurrentControlSet\Services\SharedAccess\Parameters\FirewallPolicy\PublicProfile" , "EnableFirewall" )
		$IERROR = @error
		If Not $IERROR Then
			If String ( $SENABLE ) = "0" Then
				_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]" & $SFPUBLICPROFILELOG & "[/b][/color]" )
			EndIf
		Else
			_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]" & $SFPUBLICPROFILELOG & "[/b][/color]" )
		EndIf
		$SENABLE = RegRead ( $HKLM & "\SYSTEM\CurrentControlSet\Services\SharedAccess\Parameters\FirewallPolicy\StandardProfile" , "EnableFirewall" )
		$IERROR = @error
		If Not $IERROR Then
			If String ( $SENABLE ) = "0" Then
				_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]" & $SFSTANDARDPROFILELOG & "[/b][/color]" )
			EndIf
		Else
			_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]" & $SFSTANDARDPROFILELOG & "[/b][/color]" )
		EndIf
	EndIf
EndFunc
Func CHECKVALUEXML ( ByRef $AVALXML , $SSECTION )
	Local $SSTATUSSERVICE
	Local $I , $J
	Local $SPATHFILEDIR = ""
	Local $SPATHPROCESS = ""
	Local $SFDEXISTS = ""
	Local $IPRINTSECTION = 1
	Local $ICHECKVERPROG
	Local $SVERPROGXMLOLD = ""
	Local $SDONWLOADALL = ""
	Local $SLHINFO = ""
	Local $SHIDDENTEXT = ""
	Local $ISCHKHIDDEN
	$ISCHKHIDDEN = _GETSECTIONHIDDEN ( $SSECTION )
	For $I = 1 To $APROGRAM [ 0 ] [ 0 ]
		For $J = 1 To $AVALXML [ 0 ] [ 0 ]
			If $AVALXML [ $J ] [ 0 ] = "" Then
				ContinueLoop
			EndIf
			If StringRegExp ( $APROGRAM [ $I ] [ 0 ] , "(?i)" & $AVALXML [ $J ] [ 0 ] ) Then
				If $IPRINTSECTION Then
					WRITELINESECTION ( $SSECTION )
					$IPRINTSECTION = 0
				EndIf
				$ICHECKVERPROG = CHECKVERSIONPROG ( $APROGRAM [ $I ] [ 1 ] , $AVALXML [ $J ] [ 1 ] )
				$SVERPROGXMLOLD = ""
				$SLHINFO = ""
				$SHIDDENTEXT = ""
				If $AVALXML [ $J ] [ 8 ] Then
					$SLHINFO = " " & $AVALXML [ $J ] [ 8 ]
				EndIf
				If $AVALXML [ $J ] [ 9 ] Then
					$SLHINFO &= " " & $AVALXML [ $J ] [ 9 ]
				EndIf
				If $ISCHKHIDDEN And $APROGRAM [ $I ] [ 3 ] = 1 Then
					$SHIDDENTEXT = " " & $SHIDDENPROGLOG
				EndIf
				If $IDEBUG Then
					_FILEWRITELOG_ ( $MAINLOG , "Debug - [" & $APROGRAM [ $I ] [ 0 ] & "] [" & $AVALXML [ $J ] [ 0 ] & "]" )
					_FILEWRITELOG_ ( $MAINLOG , "Debug - [" & $APROGRAM [ $I ] [ 1 ] & "] [" & $AVALXML [ $J ] [ 1 ] & "]=" & $ICHECKVERPROG )
				EndIf
				If $ICHECKVERPROG = 1 Then
					If $AVALXML [ $J ] [ 2 ] Then
						If $AVALXML [ $J ] [ 7 ] Then
							$SDONWLOADALL = "[color=red][b]" & $SWARNING & " [url=" & $AVALXML [ $J ] [ 2 ] & "]" & $SDOWNLOADUP & "[/url][/b][/color]" & " -[color=red][b]" & "" & " [url=" & $AVALXML [ $J ] [ 7 ] & "]" & $SDOWNLOADUP & "[/url][/b][/color]"
						Else
							$SDONWLOADALL = "[color=red][b]" & $SWARNING & " [url=" & $AVALXML [ $J ] [ 2 ] & "]" & $SDOWNLOADUP & "[/url][/b][/color]"
						EndIf
					Else
						$SDONWLOADALL = "[color=red][b]" & $SWARNING & " " & $SDOWNLOADUP & "[/b][/color]"
					EndIf
					_FILEWRITELOG_ ( $MAINLOG , $APROGRAM [ $I ] [ 0 ] & " v." & $APROGRAM [ $I ] [ 1 ] & " " & $SDONWLOADALL & $SHIDDENTEXT & $SLHINFO )
					If $AVALXML [ $J ] [ 6 ] Then _FILEWRITELOG_ ( $MAINLOG , "[color=blue][b]" & $AVALXML [ $J ] [ 6 ] & "[/b][/color]" )
				Else
					If $APROGRAM [ $I ] [ 1 ] Then
						If $ICHECKVERPROG < 0 Then $SVERPROGXMLOLD = " [b][+][/b]"
						_FILEWRITELOG_ ( $MAINLOG , $APROGRAM [ $I ] [ 0 ] & " v." & $APROGRAM [ $I ] [ 1 ] & $SVERPROGXMLOLD & $SHIDDENTEXT & $SLHINFO )
					Else
						_FILEWRITELOG_ ( $MAINLOG , $APROGRAM [ $I ] [ 0 ] & $SHIDDENTEXT & $SLHINFO )
					EndIf
				EndIf
				ExitLoop
			EndIf
		Next
	Next
	For $J = 1 To $AVALXML [ 0 ] [ 0 ]
		If $AVALXML [ $J ] [ 3 ] Then
			$SSTATUSSERVICE = GETSTATUSSERVICE ( $AVALXML [ $J ] [ 3 ] )
			If $SSTATUSSERVICE Then
				If $IPRINTSECTION Then
					WRITELINESECTION ( $SSECTION )
					$IPRINTSECTION = 0
				EndIf
				_FILEWRITELOG_ ( $MAINLOG , $SSTATUSSERVICE )
			EndIf
		EndIf
		If $AVALXML [ $J ] [ 4 ] Then
			$SPATHFILEDIR = $SYSTEMDRIVE & $AVALXML [ $J ] [ 4 ]
			$SFDEXISTS = _FILEDIREXISTS ( $SPATHFILEDIR )
			If $SFDEXISTS Then
				If $IPRINTSECTION Then
					WRITELINESECTION ( $SSECTION )
					$IPRINTSECTION = 0
				EndIf
				_FILEWRITELOG_ ( $MAINLOG , $SFDEXISTS )
			EndIf
		EndIf
		If $AVALXML [ $J ] [ 5 ] Then
			$SPATHPROCESS = CHECKRUNNINGPROCESS ( $AVALXML [ $J ] [ 5 ] )
			If $SPATHPROCESS Then
				If $IPRINTSECTION Then
					WRITELINESECTION ( $SSECTION )
					$IPRINTSECTION = 0
				EndIf
				_FILEWRITELOG_ ( $MAINLOG , $SPATHPROCESS )
			EndIf
		EndIf
	Next
EndFunc
Func _WINAPI_GETPROCESSFILENAMEEX ( $PID = 0 )
	If Not $PID Then
		$PID = @AutoItPID
	EndIf
	Local $HPROCESS = DllCall ( "kernel32.dll" , "ptr" , "OpenProcess" , "dword" , __IIF ( $__WINVER < 1536 , 1024 , 4096 ) , "int" , 0 , "dword" , $PID )
	If ( @error ) Or ( Not $HPROCESS [ 0 ] ) Then
		Return SetError ( 1 , 0 , "" )
	EndIf
	Local $PATH = _WINAPI_QUERYFULLPROCESSIMAGENAME ( $HPROCESS [ 0 ] )
	_WINAPI_CLOSEHANDLE ( $HPROCESS [ 0 ] )
	If Not $PATH Then
		Return SetError ( 2 , 0 , "" )
	EndIf
	Return $PATH
EndFunc
Func _WINAPI_QUERYFULLPROCESSIMAGENAME ( $HPROCESS )
	Local $RET = DllCall ( "kernel32.dll" , "int" , "QueryFullProcessImageNameW" , "ptr" , $HPROCESS , "dword" , 0 , "wstr" , "" , "dword*" , 4096 )
	If ( @error ) Or ( Not $RET [ 0 ] ) Then
		Return SetError ( 1 , 0 , "" )
	EndIf
	Return $RET [ 3 ]
EndFunc
Func CHECKRUNNINGPROCESS ( $SPROCESSMASKA )
	Local $SPATHPROC = ""
	Local $SVERSIONPROC
	Local $SRESULT = ""
	Local $ICOUNTPROCESS = 0
	Local $APATHPROCESS [ 1 ]
	Local $I , $J
	$APATHPROCESS [ 0 ] = 0
	For $I = 1 To $APROCESS [ 0 ] [ 0 ]
		If $APROCESS [ $I ] [ 0 ] = "" Or $APROCESS [ $I ] [ 1 ] = "" Then
			ContinueLoop
		EndIf
		If StringCompare ( $APROCESS [ $I ] [ 0 ] , $SPROCESSMASKA ) = 0 Then
			If $__WINVER >= 1536 Then
				$SPATHPROC = _WINAPI_GETPROCESSFILENAMEEX ( $APROCESS [ $I ] [ 1 ] )
			Else
				$SPATHPROC = _WINAPI_GETPROCESSFILENAME ( $APROCESS [ $I ] [ 1 ] )
			EndIf
			If Not $SPATHPROC Then
				$SPATHPROC = PROCESSPIDTOPATH ( $APROCESS [ $I ] [ 1 ] )
			EndIf
			If $SPATHPROC = "" Then
				$SRESULT = $APROCESS [ $I ] [ 0 ]
			Else
				If FileExists ( $SPATHPROC ) Then
					$SVERSIONPROC = FileGetVersion ( $SPATHPROC )
					$SRESULT = $SPATHPROC & " v." & $SVERSIONPROC
				Else
					$SRESULT = $SPATHPROC
				EndIf
			EndIf
			For $J = 1 To $APATHPROCESS [ 0 ]
				If StringCompare ( $APATHPROCESS [ $J ] , $SRESULT ) = 0 Then
					ContinueLoop 2
				EndIf
			Next
			$ICOUNTPROCESS += 1
			ReDim $APATHPROCESS [ $ICOUNTPROCESS + 1 ]
			$APATHPROCESS [ 0 ] = $ICOUNTPROCESS
			$APATHPROCESS [ $ICOUNTPROCESS ] = $SRESULT
		EndIf
	Next
	Return _ARRAYTOSTRING ( $APATHPROCESS , @CRLF , 1 )
EndFunc
Func PROCESSPIDTOPATH ( $ICHECKPID )
	Local $SEXECUTABLEPATH = ""
	If $FPRNOTEXISTSWMI Then Return SetError ( 1 , 0 , "" )
	Local $OBJWMISERVICE = ObjGet ( "WinMgmts:\\.\root\cimv2" )
	If Not IsObj ( $OBJWMISERVICE ) Then Return SetError ( 2 , 0 , "" )
	Local $COLITEMS = $OBJWMISERVICE .ExecQuery ( "SELECT * FROM Win32_Process Where ProcessId=""" & $ICHECKPID & """" )
	If IsObj ( $COLITEMS ) Then
		For $OBJITEM In $COLITEMS
			$SEXECUTABLEPATH = $OBJITEM .ExecutablePath
		Next
	EndIf
	If $SEXECUTABLEPATH = "" Then
		Return SetError ( 3 , 0 , "" )
	EndIf
	Return $SEXECUTABLEPATH
EndFunc
Func ADMINRESTRICTIONS ( )
	REGEDITCHECK ( )
	CMDCHECK ( )
	TASKMGRCHECK ( )
	SYSTEMRESTORECHECK ( )
	CONFIGSYSTEMRESTORECHECK ( )
	CONTROLPANELCHECK ( )
	FOLDEROPTIONSCHECK ( )
	SECURITYTABCHECK ( )
	ANTISPYWAREWINCHECK ( )
	WINDOWSUPDATEACCESSCHECK ( )
	HIDDENDIRFILECHECK ( )
EndFunc
Func REGEDITCHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System" , "DisableRegedit" )
	$REGVAL += RegRead ( $HKCU & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System" , "DisableRegedit" )
	$REGVAL += RegRead ( $HKCU & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System" , "DisableRegistryTools" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SREGEDITDISABLELOG )
	EndIf
EndFunc
Func CMDCHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKCU & "\SOFTWARE\Policies\Microsoft\Windows\System" , "DisableCMD" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SCMDDISABLELOG )
	EndIf
EndFunc
Func TASKMGRCHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKLM & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "DisableTaskMgr" )
	$REGVAL += RegRead ( $HKCU & "\Software\Microsoft\Windows\CurrentVersion\Policies\System" , "DisableTaskMgr" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $STASKMGRDISABLELOG )
	EndIf
EndFunc
Func CONFIGSYSTEMRESTORECHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKLM & "\SOFTWARE\Policies\Microsoft\Windows NT\SystemRestore" , "DisableConfig" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SCONFIGDISABLELOG )
	EndIf
EndFunc
Func CONTROLPANELCHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKCU & "\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer" , "NoControlPanel" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SCONTROLPANELDISABLELOG )
	EndIf
EndFunc
Func FOLDEROPTIONSCHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKCU & "\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer" , "NoFolderOptions" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SFOLDEROPTIONSDISABLELOG )
	EndIf
EndFunc
Func SECURITYTABCHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKCU & "\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer" , "NoSecurityTab" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SSECURITYTABDISABLELOG )
	EndIf
EndFunc
Func ANTISPYWAREWINCHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKCU & "\Software\Policies\Microsoft\Windows Defender" , "DisableAntiSpyware" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SWINDOWSDEFENDERDISABLELOG )
	EndIf
EndFunc
Func WINDOWSUPDATEACCESSCHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKCU & "\Software\Microsoft\Windows\CurrentVersion\Policies\WindowsUpdate" , "DisableWindowsUpdateAccess" )
	If $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SWINDOWSUPDATEACCESSDISABLELOG )
	EndIf
EndFunc
Func HIDDENDIRFILECHECK ( )
	Local $REGVAL = 0
	$REGVAL = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced\Folder\Hidden\SHOWALL" , "CheckedValue" )
	If Not $REGVAL Then
		_FILEWRITELOG_ ( $MAINLOG , $SHIDDENDIRFILEDISABLELOG )
	EndIf
EndFunc
Func SYSTEMRESTORECHECK ( )
	Local $REGVAL
	Local $I = 1
	If $SOSVERSION = "WIN_XP" Or $SOSVERSION = "WIN_2003" Then
		$REGVAL = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion\SystemRestore" , "DisableSR" )
		If $REGVAL Then
			_FILEWRITELOG_ ( $MAINLOG , $SSYSTEMRESTOREDISABLELOG )
		EndIf
	ElseIf $SOSVERSION = "WIN_VISTA" Or $SOSVERSION = "WIN_7" Or $SOSVERSION = "WIN_8" Or $SOSVERSION = "WIN_81" Or ( $SOSVERSION = "WIN_10" ) Or ( $SOSVERSION = "WIN_11" ) Then
		$REGVAL = RegEnumVal ( $HKLM & "\SOFTWARE\Microsoft\Windows NT\CurrentVersion\SPP\Clients" , $I )
		If @error < 0 Then
			_FILEWRITELOG_ ( $MAINLOG , $SSYSTEMRESTOREDISABLELOG )
		EndIf
	EndIf
EndFunc
Func _FILEDIREXISTS ( $SPATHFD )
	Local $SVERSIONFILE
	Local $SRESULT = ""
	If FileExists ( $SPATHFD ) Then
		If StringInStr ( FileGetAttrib ( $SPATHFD ) , "D" ) Then
			$SRESULT = $SPATHFD
		Else
			$SVERSIONFILE = FileGetVersion ( $SPATHFD )
			If @error Then
				$SVERSIONFILE = ""
			Else
				$SVERSIONFILE = " v." & $SVERSIONFILE
			EndIf
			$SRESULT = $SPATHFD & $SVERSIONFILE
		EndIf
	EndIf
	Return $SRESULT
EndFunc
Func _GETUAC ( )
	Local $SENABLELUA = ""
	Local $SCONSENTPROMPTBEHAVIORADMIN = ""
	Local $SCONSENTPROMPTBEHAVIORUSER = ""
	Local $IRETURN = 0
	Local $SMSG = ""
	Local $IUACDISABLE = 0
	If ( $SOSVERSION = "WIN_81" ) Or ( $SOSVERSION = "WIN_7" ) Or ( $SOSVERSION = "WIN_8" ) Or ( $SOSVERSION = "WIN_10" ) Or ( $SOSVERSION = "WIN_11" ) Then
		$IRETURN = _GETLEVELUAC ( )
		Switch $IRETURN
		Case 1
			$SMSG = "[color=red][b]" & $SUACLEVEL1LOG & "[/b][/color]"
			$IUACDISABLE = 1
		Case 2
			$SMSG = $SUACLEVEL2LOG
		Case 3
			$SMSG = $SUACLEVEL3LOG
		Case 4
			$SMSG = $SUACLEVEL4LOG
		EndSwitch
		If $SMSG Then
			_FILEWRITELOG_ ( $MAINLOG , $SMSG )
		Else
			$SENABLELUA = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System" , "EnableLUA" )
			If Not @error Then
				If String ( $SENABLELUA ) = "0" Then
					_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]" & $SUACDISABLED & "[/b][/color]" )
					$IUACDISABLE = 1
				ElseIf String ( $SENABLELUA ) = "1" Then
					_FILEWRITELOG_ ( $MAINLOG , $SUACENABLED )
				Else
					_FILEWRITELOG_ ( $MAINLOG , "[color=red][b]UAC Unknown " & $SENABLELUA & "[/b][/color]" )
				EndIf
			Else
			EndIf
			$SCONSENTPROMPTBEHAVIORADMIN = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System" , "ConsentPromptBehaviorAdmin" )
			If Not @error Then
				If String ( $SCONSENTPROMPTBEHAVIORADMIN ) = "0" Then
					_FILEWRITELOG_ ( $MAINLOG , $SCONSENTPROMPTBEHAVIORADMINLOG & " [color=red][b]" & $SDISABLEDLOG & "[/b][/color]" )
					$IUACDISABLE = 1
				EndIf
			Else
				_FILEWRITELOG_ ( $MAINLOG , $SCONSENTPROMPTBEHAVIORADMINLOG & " [color=red][b]" & $SDISABLEDLOG & "[/b][/color]" )
				$IUACDISABLE = 1
			EndIf
			$SCONSENTPROMPTBEHAVIORUSER = RegRead ( $HKLM & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System" , "ConsentPromptBehaviorUser" )
			If Not @error Then
				If String ( $SCONSENTPROMPTBEHAVIORUSER ) = "0" Then
					_FILEWRITELOG_ ( $MAINLOG , $SCONSENTPROMPTBEHAVIORUSERLOG & " [color=red][b]" & $SDISABLEDLOG & "[/b][/color]" )
				EndIf
			Else
				_FILEWRITELOG_ ( $MAINLOG , $SCONSENTPROMPTBEHAVIORUSERLOG & " [color=red][b]" & $SDISABLEDLOG & "[/b][/color]" )
			EndIf
		EndIf
		If $IUACDISABLE Then _FILEWRITELOG_ ( $MAINLOG , "[color=blue][b]" & $SHELPUSERUACLOG & "[/b][/color]" )
	EndIf
EndFunc
Func CHECKGUESTWMI ( )
	Local $STRCOMPUTER = "."
	Local $ATEXTLOG [ 1 ] = [ 0 ]
	Local $SPASSWORDREQUIRED
	If $FPRNOTEXISTSWMI Then Return 1
	Local $OBJWMISERVICE = ObjGet ( "winmgmts:\\" & $STRCOMPUTER & "\root\CIMV2" )
	If Not IsObj ( $OBJWMISERVICE ) Then Return 1
	Local $COLITEMS = $OBJWMISERVICE .ExecQuery ( "SELECT * FROM Win32_UserAccount Where LocalAccount = True And Disabled = False" )
	If IsObj ( $COLITEMS ) Then
		For $OBJITEM In $COLITEMS
			If ( $OBJITEM .Name = "Гость" Or $OBJITEM .Name = "Guest" ) Then
				If $OBJITEM .PasswordRequired Then
					$SPASSWORDREQUIRED = $SPASSWORDREQUIREDLOG
				Else
					$SPASSWORDREQUIRED = $SPASSWORDNOTREQUIREDLOG
				EndIf
				_ARRAYADD1D1S ( $ATEXTLOG , $SGUESTENABLEDLOG & " " & $SPASSWORDREQUIRED )
			EndIf
		Next
	EndIf
	If $ATEXTLOG [ 0 ] Then
		For $I = 1 To $ATEXTLOG [ 0 ]
			_FILEWRITELOG_ ( $MAINLOG , $ATEXTLOG [ $I ] )
		Next
	EndIf
EndFunc
Func _OPENXML ( )
	If FileExists ( $SXMLPATHFILE ) Then
		_XMLFILEOPEN ( $SXMLPATHFILE )
		If @error Then
			$FPRWORKXML = False
			_FILEWRITELOG_ ( $MAINLOG , $SXMLPATHFILE & "- " & _XMLERROR ( "" ) )
		EndIf
	Else
		_FILEWRITELOG_ ( $MAINLOG , $SXMLPATHFILE & " - " & $SFILENOTFOUNDLOG )
		$FPRWORKXML = False
	EndIf
	If StringInStr ( $SXMLPATHFILE , "SCUpdateInet.xml" ) And Not $FPRWORKXML Then
		$FPRWORKXML = True
		$SXMLPATHFILE = $SSCRIPTDIR & "SCUpdate.xml"
		If FileExists ( $SXMLPATHFILE ) Then
			_XMLFILEOPEN ( $SXMLPATHFILE )
			If @error Then
				$FPRWORKXML = False
				_FILEWRITELOG_ ( $MAINLOG , $SXMLPATHFILE & "- " & _XMLERROR ( "" ) )
			EndIf
		Else
			_FILEWRITELOG_ ( $MAINLOG , $SXMLPATHFILE & " - " & $SFILENOTFOUNDLOG )
			$FPRWORKXML = False
		EndIf
		If $FPRWORKXML Then
			_FILEWRITELOG_ ( $MAINLOG , $SXMLPATHFILE & " ... OK" )
		EndIf
	EndIf
EndFunc
Func XMLGETPARAM ( $SROOTNODES )
	Local $ICOUNT = 0
	Local $ACHILDS
	Dim $ANAME [ 1 ] , $AVALUE [ 1 ]
	Local $SHELPUSERRUEN = "HelpUser"
	Local $SHELPINFORUEN = "HelpInfo"
	Local $SLINKINFORUEN = "LinkInfo"
	Local $STEXTLINKRUEN = "Link"
	Local $STEXTLINKRUENX64 = "Link"
	Local $STEXTLINKX64 = "LinkX64"
	If $ISLOCALRU Then
		$STEXTLINKRUEN &= "RU"
		$STEXTLINKRUENX64 &= "RU"
		$SHELPUSERRUEN &= "RU"
		$SHELPINFORUEN &= "RU"
		$SLINKINFORUEN &= "RU"
	Else
		$STEXTLINKRUEN &= "EN"
		$STEXTLINKRUENX64 &= "EN"
		$SHELPUSERRUEN &= "EN"
		$SHELPINFORUEN &= "EN"
		$SLINKINFORUEN &= "EN"
	EndIf
	$STEXTLINKRUENX64 &= "X64"
	Local $STEXTLINK2RUEN = "Link2"
	Local $STEXTLINK2RUENX64 = "Link2"
	Local $STEXTLINK2X64 = "Link2X64"
	If $ISLOCALRU Then
		$STEXTLINK2RUEN &= "RU"
		$STEXTLINK2RUENX64 &= "RU"
	Else
		$STEXTLINK2RUEN &= "EN"
		$STEXTLINK2RUENX64 &= "EN"
	EndIf
	$STEXTLINK2RUENX64 &= "X64"
	Local $SNAME
	Local $SVERSION
	Local $SLINK , $SLINKX64
	Local $SLINKRUEN , $SLINKRUENX64
	Local $SLINK2 , $SLINK2X64
	Local $SLINK2RUEN , $SLINK2RUENX64
	Local $SSERVICE
	Local $SPATH
	Local $SPROCESS
	Local $SHELPUSER
	Local $SHELPINFO
	Local $SLINKINFO
	Dim $APROGRAMXML [ 1 ] [ 10 ] = [ [ 0 ] ]
	Local $AATTRIBS
	Local $APATTERN [ 1 ] [ 1 ] = [ [ 0 ] ]
	Local $APATTERN1 [ 1 ] [ 1 ] = [ [ 0 ] ]
	Local $APATTERN2 [ 1 ] [ 1 ] = [ [ 0 ] ]
	Local $APATTERN3 [ 1 ] [ 1 ] = [ [ 0 ] ]
	Local $APATTERN4 [ 1 ] [ 1 ] = [ [ 0 ] ]
	Local $APATTERN5 [ 1 ] [ 1 ] = [ [ 0 ] ]
	Local $ACHILDS = _XMLGETCHILDTEXT ( $SROOTNODES )
	If @error Then
		Return SetError ( @error , 0 , $APROGRAMXML )
	EndIf
	For $I = 1 To $ACHILDS [ 0 ]
		$AATTRIBS = _XMLGETALLATTRIB ( $SROOTNODES & "/" & $ACHILDS [ $I ] & "[" & $I & "]" , $ANAME , $AVALUE )
		If @error Then
			ContinueLoop
		EndIf
		$APATTERN [ 0 ] [ 0 ] = 0
		$SNAME = ""
		$SVERSION = ""
		$SLINK = ""
		$SLINKX64 = ""
		$SLINKRUEN = ""
		$SLINKRUENX64 = ""
		$SLINK2 = ""
		$SLINK2X64 = ""
		$SLINK2RUEN = ""
		$SLINK2RUENX64 = ""
		$SSERVICE = ""
		$SPATH = ""
		$SPROCESS = ""
		$SHELPUSER = ""
		$SHELPINFO = ""
		$SLINKINFO = ""
		For $J = 1 To $AATTRIBS [ 0 ] [ 0 ]
			Select
			Case $AATTRIBS [ 0 ] [ $J ] = "Pattern1"
				$APATTERN1 = $AATTRIBS
				ContinueLoop 2
			Case $AATTRIBS [ 0 ] [ $J ] = "Pattern2"
				$APATTERN2 = $AATTRIBS
				ContinueLoop 2
			Case $AATTRIBS [ 0 ] [ $J ] = "Pattern3"
				$APATTERN3 = $AATTRIBS
				ContinueLoop 2
			Case $AATTRIBS [ 0 ] [ $J ] = "Pattern4"
				$APATTERN4 = $AATTRIBS
				ContinueLoop 2
			Case $AATTRIBS [ 0 ] [ $J ] = "Pattern5"
				$APATTERN5 = $AATTRIBS
				ContinueLoop 2
			Case $AATTRIBS [ 0 ] [ $J ] = "Pattern"
				If $AATTRIBS [ 1 ] [ $J ] = "1" Then
					$APATTERN = $APATTERN1
					ExitLoop
				ElseIf $AATTRIBS [ 1 ] [ $J ] = "2" Then
					$APATTERN = $APATTERN2
					ExitLoop
				ElseIf $AATTRIBS [ 1 ] [ $J ] = "3" Then
					$APATTERN = $APATTERN3
					ExitLoop
				ElseIf $AATTRIBS [ 1 ] [ $J ] = "4" Then
					$APATTERN = $APATTERN4
					ExitLoop
				ElseIf $AATTRIBS [ 1 ] [ $J ] = "5" Then
					$APATTERN = $APATTERN5
					ExitLoop
				EndIf
			Case $AATTRIBS [ 0 ] [ $J ] = "Name"
				$SNAME = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = "Version"
				$SVERSION = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = $STEXTLINKRUEN
				$SLINKRUEN = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = $STEXTLINKRUENX64
				$SLINKRUENX64 = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = "Link"
				$SLINK = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = "LinkX64"
				$SLINKX64 = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = $STEXTLINK2RUEN
				$SLINK2RUEN = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = $STEXTLINK2RUENX64
				$SLINKRUENX64 = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = "Link2"
				$SLINK2 = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = "Link2X64"
				$SLINK2X64 = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = "Service"
				$SSERVICE = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = "Path"
				$SPATH = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = "Process"
				$SPROCESS = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = $SHELPUSERRUEN
				$SHELPUSER = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = $SHELPINFORUEN
				$SHELPINFO = $AATTRIBS [ 1 ] [ $J ]
			Case $AATTRIBS [ 0 ] [ $J ] = $SLINKINFORUEN
				$SLINKINFO = $AATTRIBS [ 1 ] [ $J ]
			EndSelect
		Next
		If $APATTERN [ 0 ] [ 0 ] Then
			For $J = 1 To $APATTERN [ 0 ] [ 0 ]
				Select
				Case $APATTERN [ 0 ] [ $J ] = $STEXTLINKRUEN
					$SLINKRUEN = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = $STEXTLINKRUENX64
					$SLINKRUENX64 = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = "Link"
					$SLINK = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = "LinkX64"
					$SLINKX64 = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = $STEXTLINK2RUEN
					$SLINK2RUEN = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = $STEXTLINK2RUENX64
					$SLINKRUENX64 = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = "Link2"
					$SLINK2 = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = "Link2X64"
					$SLINK2X64 = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = "Service"
					$SSERVICE = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = "Path"
					$SPATH = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = "Process"
					$SPROCESS = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = $SHELPUSERRUEN
					$SHELPUSER = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = $SHELPINFORUEN
					$SHELPINFO = $APATTERN [ 1 ] [ $J ]
				Case $APATTERN [ 0 ] [ $J ] = $SLINKINFORUEN
					$SLINKINFO = $APATTERN [ 1 ] [ $J ]
				EndSelect
			Next
		EndIf
		If $SNAME Or $SPROCESS Or $SPATH Or $SSERVICE Then
			$ICOUNT += 1
			ReDim $APROGRAMXML [ $ICOUNT + 1 ] [ 10 ]
			$APROGRAMXML [ $ICOUNT ] [ 0 ] = $SNAME
			$APROGRAMXML [ $ICOUNT ] [ 1 ] = $SVERSION
			If $ISWOW64 And ( $SLINKX64 Or $SLINKRUENX64 ) Then
				If $SLINKRUENX64 Then
					$APROGRAMXML [ $ICOUNT ] [ 2 ] = $SLINKRUENX64
				Else
					$APROGRAMXML [ $ICOUNT ] [ 2 ] = $SLINKX64
				EndIf
			Else
				If $SLINKRUEN Then
					$APROGRAMXML [ $ICOUNT ] [ 2 ] = $SLINKRUEN
				Else
					$APROGRAMXML [ $ICOUNT ] [ 2 ] = $SLINK
				EndIf
			EndIf
			If $ISWOW64 And ( $SLINK2X64 Or $SLINK2RUENX64 ) Then
				If $SLINK2RUENX64 Then
					$APROGRAMXML [ $ICOUNT ] [ 7 ] = $SLINK2RUENX64
				Else
					$APROGRAMXML [ $ICOUNT ] [ 7 ] = $SLINK2X64
				EndIf
			Else
				If $SLINK2RUEN Then
					$APROGRAMXML [ $ICOUNT ] [ 7 ] = $SLINK2RUEN
				Else
					$APROGRAMXML [ $ICOUNT ] [ 7 ] = $SLINK2
				EndIf
			EndIf
			$APROGRAMXML [ $ICOUNT ] [ 3 ] = $SSERVICE
			$APROGRAMXML [ $ICOUNT ] [ 4 ] = $SPATH
			$APROGRAMXML [ $ICOUNT ] [ 5 ] = $SPROCESS
			$APROGRAMXML [ $ICOUNT ] [ 6 ] = $SHELPUSER
			$APROGRAMXML [ $ICOUNT ] [ 8 ] = $SLINKINFO
			$APROGRAMXML [ $ICOUNT ] [ 9 ] = $SHELPINFO
			$APROGRAMXML [ 0 ] [ 0 ] = $ICOUNT
		EndIf
	Next
	If $APROGRAMXML [ 0 ] [ 0 ] = 0 Then
		SetError ( 1 , 0 , 0 )
	Else
		Return $APROGRAMXML
	EndIf
EndFunc
Func _CHECKVERSIONSC ( )
	Dim $ANAME [ 1 ] , $AVALUE [ 1 ]
	Local $AATTRIBS
	Dim $AINFOSC [ 2 ] [ 2 ] = [ [ "VersionSC" , "0.0" ] , [ "VersionXML" , "0.0" ] ]
	Local $ICHECKVER = 0
	Local $SVERPROGOLD = ""
	Local $IERROR = 0
	Local $SSTRHEADERXML = ""
	Local $IMSGBOXRES
	If $FPRWORKXML Then
		$AATTRIBS = _XMLGETALLATTRIB ( $SROOTX , $ANAME , $AVALUE , "" )
		If Not @error Then
			For $J = 0 To UBound ( $AINFOSC ) - 1
				For $I = 1 To $AATTRIBS [ 0 ] [ 0 ]
					If $AATTRIBS [ 0 ] [ $I ] = $AINFOSC [ $J ] [ 0 ] Then
						$AINFOSC [ $J ] [ 1 ] = $AATTRIBS [ 1 ] [ $I ]
					EndIf
				Next
			Next
		EndIf
	EndIf
	$ICHECKVER = CHECKVERSIONPROG ( $SVERSIONPROG , $AINFOSC [ 0 ] [ 1 ] )
	If $ICHECKVER = 1 Then
		$IERROR = 1
		$SVERPROGOLD = "VersionSC: " & $AINFOSC [ 0 ] [ 1 ] & " [b][+][/b]" & @CRLF
	EndIf
	$SSTRHEADERXML = $SVERPROGOLD & $AINFOSC [ 1 ] [ 0 ] & ": " & $AINFOSC [ 1 ] [ 1 ]
	_FILEWRITELOG_ ( $MAINLOG , $SSTRHEADERXML )
	If $IERROR Then
		_FILEWRITELOG_ ( $MAINLOG , "Update SC - Check Version" )
		$IMSGBOXRES = MsgBox ( 4096 + 64 + 4 , $SNAMEPROG & " " & $SAUTOR & " to Update" , "Обновите программу." & @CRLF & "Скачать последнию версию программы?" )
		If $IMSGBOXRES = 6 Then
			If $FHTMLLOG_CMD Then
				ShellExecute ( "http://tools.safezone.cc/glax24/SecurityCheck/SecurityCheckH.exe" )
			Else
				ShellExecute ( "http://tools.safezone.cc/glax24/SecurityCheck/SecurityCheck.exe" )
			EndIf
		EndIf
		_EXIT ( )
	EndIf
EndFunc
Func _INSTALLEDPROGRAMS ( )
	Local $AUNINSTALLKEY [ 1 ] = [ 0 ]
	_ARRAYADD1D1S ( $AUNINSTALLKEY , $HKLM & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall" )
	_ARRAYADD1D1S ( $AUNINSTALLKEY , $HKCU & "\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall" )
	_ARRAYADD1D1S ( $AUNINSTALLKEY , $HKLM & "\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall" , 1 )
	_ARRAYADD1D1S ( $AUNINSTALLKEY , $HKCU & "\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall" , 1 )
	For $I = 1 To $AUNINSTALLKEY [ 0 ]
		GETINSTALLPROG ( $AUNINSTALLKEY [ $I ] )
	Next
EndFunc
Func GETINSTALLPROG ( $SKEY )
	Local $ERRREG = 0
	Local $I = 0
	Local $SDISPLAYNAME
	Local $SDISPLAYVERSION
	Local $SUNINSTALLSTRING
	Local $SISHIDDEN
	Local $SDATEINSTALL
	Local $AKEY = _REGREADKEYVALUETOARRAY ( $SKEY )
	$ERRREG = @error
	If $ERRREG Or UBound ( $AKEY ) = 0 Then Return SetError ( 1 , 0 , "" )
	If Not $AKEY [ 0 ] Then Return SetError ( 2 , 0 , "" )
	ReDim $APROGRAM [ $AKEY [ 0 ] + 1 + $APROGRAM [ 0 ] [ 0 ] ] [ 5 ]
	For $I = 1 To UBound ( $AKEY ) - 1
		$SDISPLAYNAME = RegRead ( $AKEY [ $I ] , "DisplayName" )
		$SDISPLAYVERSION = RegRead ( $AKEY [ $I ] , "DisplayVersion" )
		$SUNINSTALLSTRING = RegRead ( $AKEY [ $I ] , "UninstallString" )
		$SISHIDDEN = RegRead ( $AKEY [ $I ] , "SystemComponent" )
		$SDATEINSTALL = _GETDATATIMEREGKEY ( $AKEY [ $I ] )
		If $SDISPLAYNAME Then
			For $J = 1 To $APROGRAM [ 0 ] [ 0 ]
				If $APROGRAM [ $J ] [ 0 ] = "" Then
					ContinueLoop
				EndIf
				If StringCompare ( $APROGRAM [ $J ] [ 0 ] , $SDISPLAYNAME ) = 0 Then
					ContinueLoop 2
				EndIf
			Next
			$APROGRAM [ 0 ] [ 0 ] += 1
			$APROGRAM [ $APROGRAM [ 0 ] [ 0 ] ] [ 0 ] = $SDISPLAYNAME
			$APROGRAM [ $APROGRAM [ 0 ] [ 0 ] ] [ 1 ] = $SDISPLAYVERSION
			$APROGRAM [ $APROGRAM [ 0 ] [ 0 ] ] [ 2 ] = $SUNINSTALLSTRING
			$APROGRAM [ $APROGRAM [ 0 ] [ 0 ] ] [ 3 ] = $SISHIDDEN
			$APROGRAM [ $APROGRAM [ 0 ] [ 0 ] ] [ 4 ] = $SDATEINSTALL
		EndIf
	Next
	ReDim $APROGRAM [ $APROGRAM [ 0 ] [ 0 ] + 1 ] [ 5 ]
EndFunc
Func GETSERVICEPROCESS ( )
	$ASERVICE = _SERVICE_ENUM ( $SERVICE_WIN32 , $SERVICE_STATE_ALL )
	$APROCESS = ProcessList ( )
	If @error Or IsArray ( $APROCESS ) <> 1 Or UBound ( $APROCESS , 0 ) <> 2 Or $APROCESS [ 0 ] [ 0 ] <= 0 Then
		Global $APROCESS [ 1 ] [ 1 ]
		$APROCESS [ 0 ] [ 0 ] = 0
	EndIf
EndFunc
Func GETSTATUSSERVICE ( $SSERVISE )
	Local $I
	Local $SSTATUSSERVICE = ""
	For $I = 1 To UBound ( $ASERVICE ) - 1
		If StringInStr ( $ASERVICE [ $I ] [ 0 ] , $SSERVISE ) Then
			If $ASERVICE [ $I ] [ 3 ] = $SERVICE_STOPPED Then
				$SSTATUSSERVICE = $SSERVISESTAT1
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_START_PENDING Then
				$SSTATUSSERVICE = $SSERVISESTAT2
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_STOP_PENDING Then
				$SSTATUSSERVICE = $SSERVISESTAT3
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_RUNNING Then
				$SSTATUSSERVICE = $SSERVISESTAT4
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_CONTINUE_PENDING Then
				$SSTATUSSERVICE = $SSERVISESTAT5
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_PAUSE_PENDING Then
				$SSTATUSSERVICE = $SSERVISESTAT6
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_PAUSED Then
				$SSTATUSSERVICE = $SSERVISESTAT7
			Else
				$SSTATUSSERVICE = "The service has error status"
			EndIf
			Return $ASERVICE [ $I ] [ 1 ] & " (" & $ASERVICE [ $I ] [ 0 ] & ")" & " - " & $SSTATUSSERVICE
		EndIf
	Next
	Return $SSTATUSSERVICE
EndFunc
Func GETSTATUSSERVICESTOP ( $SSERVISE )
	Local $I
	For $I = 1 To $ASERVICE [ 0 ] [ 0 ]
		If StringInStr ( $ASERVICE [ $I ] [ 0 ] , $SSERVISE ) Then
			If $ASERVICE [ $I ] [ 3 ] = $SERVICE_STOPPED Then
				Return 1
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_START_PENDING Then
				Return 1
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_STOP_PENDING Then
				Return 1
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_RUNNING Then
				Return 0
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_CONTINUE_PENDING Then
				Return 1
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_PAUSE_PENDING Then
				Return 1
			ElseIf $ASERVICE [ $I ] [ 3 ] = $SERVICE_PAUSED Then
				Return 1
			Else
				Return 1
			EndIf
		EndIf
	Next
	Return 1
EndFunc
Func TESTWMI ( )
	Local $ERROR , $OBJ_WMI
	_WMI_OBJERRORRESET ( )
	Local $SSTATSERVICE = GETSTATUSSERVICESTOP ( "Winmgmt" )
	Switch $SSTATSERVICE
	Case 0
		$OBJ_WMI = ObjGet ( $SWMI_MONIKER & ":Win32_LocalTime" )
		If Not $OBJECT_ERROR And IsObj ( $OBJ_WMI ) Then
			$NWMI = 1
		Else
			$NWMI = 0
		EndIf
Case Else
		$NWMI = 0
	EndSwitch
	$OBJ_WMI = 0
	If $NWMI <> 1 Then
		$FPRNOTEXISTSWMI = True
	EndIf
EndFunc
Func _WMI_OBJERRORRESET ( )
	$OBJECT_ERROR = 0
EndFunc
Func _OBJERRORHANDLER ( )
	If Not IsObj ( $OERRORHANDLER ) Then
		$FPRNOTEXISTSWMI = True
		SetError ( - 1 )
	EndIf
	If $OBJECT_ERROR Then
		$OERRORHANDLER .Clear
		$FPRNOTEXISTSWMI = True
		Return
	Else
		$OBJECT_ERROR = 1
	EndIf
	$OERRORHANDLER .Clear
	Return SetError ( - 1 )
EndFunc
Func _COMMANDLINECHECK ( )
	Local $I = 0
	If $CMDLINE [ 0 ] Then
		For $I = 1 To $CMDLINE [ 0 ]
			If $CMDLINE [ $I ] = "/progall" Then
				$FPROGALLPRINT_CMD = True
				$SCMDLINESTART &= " " & $CMDLINE [ $I ]
			ElseIf $CMDLINE [ $I ] = "/m1" Then
				$FM1PRINT_CMD = True
				$SCMDLINESTART &= " " & $CMDLINE [ $I ]
			ElseIf $CMDLINE [ $I ] = "/autodelscript" Then
				$FAUTODELSCRIPT_CMD = True
			ElseIf $CMDLINE [ $I ] = "/htmllog" Then
				$FHTMLLOG_CMD = True
			ElseIf $CMDLINE [ $I ] = "/silent" Then
				$FSILENT_CMD = True
				$SCMDLINESTART &= " " & $CMDLINE [ $I ]
			ElseIf $CMDLINE [ $I ] = "/unwanted" Then
				$FUNWANTEDAPPS_CMD = True
				$SCMDLINESTART &= " " & $CMDLINE [ $I ]
			ElseIf $CMDLINE [ $I ] = "/localdb" Then
				$FLOCALXML_CMD = True
				$SCMDLINESTART &= " " & $CMDLINE [ $I ]
			ElseIf $CMDLINE [ $I ] = "/debug" Then
				$IDEBUG = 1
				$SCMDLINESTART &= " " & $CMDLINE [ $I ]
			ElseIf $CMDLINE [ $I ] = "/pathlog" Then
				If StringRegExp ( $CMDLINE [ $I + 1 ] , "(?i)^([a-z]:\\.*\\?)$" ) Then
					$SPATHLOG_CMD = $CMDLINE [ $I + 1 ]
					$SCMDLINESTART &= " " & $CMDLINE [ $I ] & " " & $CMDLINE [ $I + 1 ]
					$I += 1
				EndIf
			ElseIf $CMDLINE [ $I ] = "/?" Then
				MsgBox ( 32 , "Help" , "SecurityCheck.exe [-! <parameters>]" & @CRLF & "Параметры командной строки:" & @CRLF & "     /m1 - записать в лог установленные программы за месяц" & @CRLF & "     /progall - записать в лог все установленные программы" & @CRLF & "     /unwanted - вывести в лог только нежелательные программы" & @CRLF & "     /silent - не открывать созданный лог" & @CRLF & "     /localdb - использовать локальную базу" & @CRLF & "     /pathlog <path> - каталог сохранения лога" & @CRLF & "     /htmllog - создать дополнительный лог в html" & @CRLF & "Пример: " & @CRLF & "SecurityCheck.exe -! /m1 /htmllog" )
				_EXIT ( )
			EndIf
		Next
	EndIf
EndFunc
Func AUTODELETESCRIPT ( )
	Local $RES
	Local $SFILE1 = $SSCRIPTDIR & "SCUpdate.xml"
	Local $SFILE2 = $SSCRIPTDIR & "SCUpdateInet.xml"
	Local $SFILE3 = $SSCRIPTDIR & @ScriptName
	If Not @Compiled Then Return 1
	$RES = FileDelete ( $SFILE1 )
	If $RES = 0 Or FileExists ( $SFILE1 ) Then
		RunWait ( @ComSpec & " /c " & "del /F /Q /A """ & $SFILE1 & """" , @SystemDir , @SW_HIDE )
	EndIf
	$RES = FileDelete ( $SFILE2 )
	If $RES = 0 Or FileExists ( $SFILE2 ) Then
		RunWait ( @ComSpec & " /c " & "del /F /Q /A """ & $SFILE2 & """" , @SystemDir , @SW_HIDE )
	EndIf
	$RES = FileDelete ( $SFILE3 )
	If $RES = 0 Or FileExists ( $SFILE3 ) Then
		Run ( @ComSpec & " /c " & "del /F /Q /A """ & $SFILE3 & """" , @SystemDir , @SW_HIDE )
	EndIf
EndFunc
Func CREATEHTMLLOG ( )
	Dim $ARECORDSHTML
	Dim $ARECORDSTXT
	Local $I
	Local $SSTRTEMP
	Local $NUMREPLACEMENTS
	Local $HFILE
	Local $MAINLOGHTML = $SSCRIPTDIRLOG & "SecurityCheck.html"
	If FileExists ( $MAINLOGHTML ) Then
		FileDelete ( $MAINLOGHTML )
	EndIf
	If Not FileExists ( $MAINLOG ) Then Return 1
	If Not _FILEREADTOARRAY ( $MAINLOG , $ARECORDSHTML ) Then
		Return 1
	EndIf
	$ARECORDSTXT = $ARECORDSHTML
	For $I = 1 To $ARECORDSHTML [ 0 ]
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , $STITLE , "<b><font size=""+1"">" & $STITLE & "</font></b>" )
		If @extended Then
			ContinueLoop
		EndIf
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "&" , "&amp;" )
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "<" , "&lt;" )
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , ">" , "&gt;" )
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , """" , "&quot;" )
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "www.safezone.cc" , "<a href=""http://www.safezone.cc"" target=""_blank"">SafeZone.cc</a>" )
		$ARECORDSHTML [ $I ] = StringRegExpReplace ( $ARECORDSHTML [ $I ] , "\[b\]\[(\+)\]\[/b\]" , "<b>($1)</b>" )
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[color=red]" , "<b class=""bor"">" )
		If @extended Then
			$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[/color]" , "</b>" )
			$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[b]" , "" , 1 )
			If @extended Then
				$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[/b]" , "" , 1 )
			EndIf
		EndIf
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[color=blue]" , "<b class=""blue"">" )
		If @extended Then
			$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[/color]" , "</b>" )
			$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[b]" , "" )
			If @extended Then
				$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[/b]" , "" )
			EndIf
		EndIf
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[b]" , "<b>" )
		If @extended Then
			$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[/b]" , "</b>" )
		EndIf
		$ARECORDSHTML [ $I ] = StringRegExpReplace ( $ARECORDSHTML [ $I ] , "\[i\](.+?)\[/i\]" , "<i>$1</i>" )
		$ARECORDSHTML [ $I ] = StringRegExpReplace ( $ARECORDSHTML [ $I ] , "\[u\](.+?)\[/u\]" , "<u>$1</u>" )
		$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[url=" , "<a href=""" )
		If @extended Then
			$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "[/url]" , "</a>" )
			$ARECORDSHTML [ $I ] = StringReplace ( $ARECORDSHTML [ $I ] , "]" , """ target=""_blank"">" )
		EndIf
		If StringInStr ( $ARECORDSHTML [ $I ] , "___________________________________________________________________________" ) Then
		EndIf
		If StringRegExp ( $ARECORDSHTML [ $I ] , "(-+?\s\[)" , 0 ) Then
			$ARECORDSHTML [ $I ] = StringRegExpReplace ( $ARECORDSHTML [ $I ] , "-" , "&mdash;" )
			$ARECORDSHTML [ $I ] = "<b>" & $ARECORDSHTML [ $I ] & "</b>"
		EndIf
	Next
	For $I = 1 To $ARECORDSTXT [ 0 ]
		If $I = 1 Then
			$ARECORDSTXT [ $I ] = "szStr = """ & $ARECORDSTXT [ $I ] & "\n"";"
			ContinueLoop
		EndIf
		$ARECORDSTXT [ $I ] = StringReplace ( $ARECORDSTXT [ $I ] , "\" , "\\" )
		$ARECORDSTXT [ $I ] = StringReplace ( $ARECORDSTXT [ $I ] , """" , "\""" )
		$ARECORDSTXT [ $I ] = StringReplace ( $ARECORDSTXT [ $I ] , "'" , "\'" )
		$ARECORDSTXT [ $I ] = StringReplace ( $ARECORDSTXT [ $I ] , "&" , "\&" )
		$ARECORDSTXT [ $I ] &= "\n"";"
		$ARECORDSTXT [ $I ] = "szStr += """ & $ARECORDSTXT [ $I ]
	Next
	$HFILE = FileOpen ( $MAINLOGHTML , 1 )
	FileWriteLine ( $HFILE , "<HTML>" )
	FileWriteLine ( $HFILE , "<HEAD>" )
	FileWriteLine ( $HFILE , "<TITLE>" & $SNAMEPROG & " " & $SAUTOR & "</TITLE>" )
	FileWriteLine ( $HFILE , "<META http-equiv=""Content-Type"" content=""text/html; charset=windows-1251"">" )
	FileWriteLine ( $HFILE , "<META http-equiv=""nocache"">" )
	FileWriteLine ( $HFILE , "<style type=""text/css"">" )
	FileWriteLine ( $HFILE , " {" )
	FileWriteLine ( $HFILE , "    TEXT-INDENT: 2em;" )
	FileWriteLine ( $HFILE , "    margin-top: 0pt;" )
	FileWriteLine ( $HFILE , "    margin-bottom: 0pt;" )
	FileWriteLine ( $HFILE , " }" )
	FileWriteLine ( $HFILE , "/* Бордовый */.bor" )
	FileWriteLine ( $HFILE , "  {COLOR: #aa0000}" )
	FileWriteLine ( $HFILE , "/* Голубой */.blue" )
	FileWriteLine ( $HFILE , "  {COLOR: #0373FE}" )
	FileWriteLine ( $HFILE , "</style>" )
	FileWriteLine ( $HFILE , "</HEAD>" )
	FileWriteLine ( $HFILE , "<script type=""text/javascript"">" )
	FileWriteLine ( $HFILE , "function add_log () {" )
	FileWriteLine ( $HFILE , "var szStr;" )
	FileWriteLine ( $HFILE , "szStr    = """";" )
	_FILEWRITEFROMARRAY ( $HFILE , $ARECORDSTXT , 1 )
	FileWriteLine ( $HFILE , "document.forms.LogForm.CureLog.value = szStr;" )
	FileWriteLine ( $HFILE , "}" )
	FileWriteLine ( $HFILE , "</script>" )
	FileWriteLine ( $HFILE , "<BODY bgcolor=""#EDF4F2"">" )
	FileWriteLine ( $HFILE , "<p><pre>" )
	_FILEWRITEFROMARRAY ( $HFILE , $ARECORDSHTML , 1 )
	FileWriteLine ( $HFILE , "<br>" & $SSYSTEMANALYSISHTMLLOG )
	FileWriteLine ( $HFILE , "" )
	FileWriteLine ( $HFILE , "<b>" & $SREPORTFORUMHTMLLOG & "</b><form name=""LogForm""> <textarea name=""CureLog"" rows=10 cols=80></textarea></form>" )
	FileWriteLine ( $HFILE , "<input type=""button"" value=""" & $SFORMREPORTFORUMHTMLLOG & """ onClick=""add_log()"">" )
	FileWriteLine ( $HFILE , "<hr>" )
	FileWriteLine ( $HFILE , "</pre></p>" )
	FileWriteLine ( $HFILE , "</BODY>" )
	FileWriteLine ( $HFILE , "</HTML>" )
	FileClose ( $HFILE )
	If FileExists ( $MAINLOGHTML ) Then
		If Not $FSILENT_CMD Then
			ShellExecute ( $MAINLOGHTML )
		EndIf
	EndIf
EndFunc
Func CHECKVERSIONPROG ( $SVERSION , $SVERSIONLAST )
	Local $ERROR_1 = 0 , $ERROR_2 = 0
	Local $CHECKUP = 0
	Local $K
	Local $ISTRINGCOMPARE = 0
	If Not $SVERSION Or Not $SVERSIONLAST Then Return $CHECKUP
	Local $AVER = StringSplit ( $SVERSION , "." )
	$ERROR_1 = @error
	Local $AVERLAST = StringSplit ( $SVERSIONLAST , "." )
	$ERROR_2 = @error
	If $ERROR_1 And $ERROR_2 Then
		$ISTRINGCOMPARE = StringCompare ( $SVERSION , $SVERSIONLAST )
		If $ISTRINGCOMPARE < 0 Then $CHECKUP = 1
		If $ISTRINGCOMPARE > 0 Then $CHECKUP = - 1
		Return $CHECKUP
	EndIf
	If $AVER [ 0 ] <> $AVERLAST [ 0 ] Then
		If $AVER [ 0 ] < $AVERLAST [ 0 ] Then
			ReDim $AVER [ $AVERLAST [ 0 ] + 1 ]
			For $K = ( $AVER [ 0 ] + 1 ) To $AVERLAST [ 0 ]
				$AVER [ $K ] = 0
			Next
			$AVER [ 0 ] = $AVERLAST [ 0 ]
		ElseIf $AVERLAST [ 0 ] < $AVER [ 0 ] Then
			ReDim $AVERLAST [ $AVER [ 0 ] + 1 ]
			For $K = ( $AVERLAST [ 0 ] + 1 ) To $AVER [ 0 ]
				$AVERLAST [ $K ] = 0
			Next
			$AVERLAST [ 0 ] = $AVER [ 0 ]
		EndIf
	EndIf
	For $K = 1 To $AVER [ 0 ]
		If StringIsDigit ( $AVER [ $K ] ) = 0 Then
			$AVER [ $K ] = StringRegExpReplace ( $AVER [ $K ] , "[^0-9]" , "" )
		EndIf
		If StringIsDigit ( $AVERLAST [ $K ] ) = 0 Then
			$AVERLAST [ $K ] = StringRegExpReplace ( $AVERLAST [ $K ] , "[^0-9]" , "" )
		EndIf
		If Number ( $AVER [ $K ] ) = Number ( $AVERLAST [ $K ] ) Then
			ContinueLoop
		EndIf
		If Number ( $AVER [ $K ] ) < Number ( $AVERLAST [ $K ] ) Then
			$CHECKUP = 1
			ExitLoop
		Else
			$CHECKUP = - 1
			ExitLoop
		EndIf
	Next
	Return $CHECKUP
EndFunc
Func _EXIT ( )
	SplashOff ( )
	If $FAUTODELSCRIPT_CMD Then
		AUTODELETESCRIPT ( )
	EndIf
	Exit
EndFunc
