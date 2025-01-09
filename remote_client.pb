EnableExplicit
IncludeFile "Curve64Unicode.pb"
Structure settingsStructure
  host.s
  port.i
  name.s   
  pass.s  
  isPaired.i
  Role.i
  isAuthorized.i
  Connect.i
  dis.i
  isWaitPart.i  
  isBroadcast.i
  isReadyRecieve.i
  canvasH.i
  canvasW.i
  frameW.i
  frameH.i
  imgquality.i
  isChangeQuality.i
  quit.i
  thrCN.i
  thrMS.i
  thrAP.i
  
  thrCNquit.i
  thrMSquit.i
  thrAPquit.i
  delayTime.i  
  clipboardtext.s
  Newclipboardtext.s
  clipboardfilename.s
  Newclipboardfilename.s
  lastpingTimer.i
  isAskPair.i
  AskPairName.s
  AskPairTime.i
  AskWinState.i
  PrivateKey.s
  CPublicKey.s
  Secret.s
  *SecretBuf32 
  RanD.s
  currentJob.i
  IamFileTranciever.i
  IamsendallFiles.i
  IamFileTrancieverName.s
  IamFileReciever.i
  TransferFileUploaded.i
  TransferFilesize.i
  TransferName.s
  WpartX.i
  WpartY.i
EndStructure

Structure jobtructure
  *pointer
  WpartX.i
  WpartY.i
  partX.i
  partY.i
EndStructure

Structure ClientsListStructure
  isOnline.i
  
EndStructure

Enumeration
  #File
  #File2
EndEnumeration

Enumeration
  #CanvasGaget                  
  #EditorGaget
  #StringGadgetClid
  #ButtonGadgetClCon
  #ButtonGadgetClCloseCon
  #ComboGadgetClid
  #SpinGagetImgQuality
  #set_clipboard_id
  #ButtonGadgetRequestYes
  #ButtonGadgetRequestNo
  #txtRequestGadget
  #AutoPairCheckBox
  #ButtonGadgetPassOk
  #StringGadgetPass
  #ButtonGadgetClADD
  #ButtonGadgetTransferToRemote
  #ButtonGadgetTransferFromRemote
  #ButtonGadgetServer
  #IPGadget
  #ButtonIPSave
EndEnumeration

#askpair_id = 1
#login_id = 2
#answerpair_id = 3
#text_id = 4
#imagetransfer_id = 5
#sendpart_id = 6
#ping_id = 7
#autopair_id = 8
#askfileclip_id = 9
#filepart_id = 10
#continue_id = 11


#minDelay = 40
#maxDelay = 500

#maxtcp = 65536
#MB = 1024*1024
#GB = 1024*1024*1024
#align_size = 256

#CAPTUREBLT = $40000000 

Define *CurveP, *CurveGX, *CurveGY, *Curveqn
*CurveP = Curve::m_getCurveValues()
*CurveGX = *CurveP+32
*CurveGY = *CurveP+64
*Curveqn = *CurveP+96

Define NewMap ClientsList.ClientsListStructure()
Define NewMap settings.settingsStructure()
Define Dim Roles.s(2)
Roles(0)="Master":Roles(1)="Slave"
Define MutexConsole, sendMutex, ImgMutex
Define canvasMouseX, canvasMouseY, DroppedFrames, isInCnvasArea= #False,  thrMS, thrAP
ImgMutex = CreateMutex()
MutexConsole = CreateMutex()
sendMutex = CreateMutex()

Define totalsended.f, Kbsecond.d, fps.d
Define NewMap req.jobtructure()
Define jobMutex  = CreateMutex()
Define NewMap keybardmap()

Define SettingsFileName$="Clients.dat"

Procedure SPrint(text$)
  Protected count
  Shared MutexConsole
  LockMutex(MutexConsole)  
  count = CountGadgetItems(#EditorGaget)
  AddGadgetItem(#EditorGaget, 0, FormatDate("%hh:%ii:%ss ", Date())+" "+text$)
 
  If count>10   
    RemoveGadgetItem(#EditorGaget, count-1)
  EndIf  
  
  UnlockMutex(MutexConsole)
EndProcedure

Procedure.i SendQuestion(con_id,string$)
  Protected err
  Shared settings(), sendMutex, totalsended
  If con_id
    LockMutex(sendMutex)
    settings("1")\lastpingTimer  = ElapsedMilliseconds()
    SendNetworkString(con_id,string$+#LF$,#PB_Unicode)  
    totalsended + StringByteLength(string$+#LF$, #PB_Unicode)/1024
    Debug "send to socket :"+Str(con_id)+">"+string$
    UnlockMutex(sendMutex)
  EndIf
  ProcedureReturn err
EndProcedure

Procedure.s cutHex(a$)
  a$=Trim(UCase(a$)) 
  If Left(a$,2)="0X" 
    a$=Mid(a$,3,Len(a$)-2)
  EndIf 
  If Len(a$)=1
    a$="0"+a$
  EndIf
  ProcedureReturn a$
EndProcedure

Procedure.s getElem(js.i,pname.s="",pelem.l=0,aelem.l=0)
  Protected result$,jsFloat_g
  
  result$=""
  If IsJSON(js) And GetJSONMember(JSONValue(js), pname)
    Select JSONType(GetJSONMember(JSONValue(js), pname))
        
      Case #PB_JSON_String
        result$= GetJSONString(GetJSONMember(JSONValue(js), pname))          
      Case #PB_JSON_Array
        
        
        
        If JSONArraySize(GetJSONMember(JSONValue(js), pname))>pelem
          Select JSONType(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
            Case #PB_JSON_String
              result$= GetJSONString(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
            Case #PB_JSON_Number            
              result$= Str(GetJSONInteger(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem)))    
              jsFloat_g=GetJSONDouble(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
              
            Case #PB_JSON_Array
              If JSONArraySize(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))>aelem             
                result$+ GetJSONString(GetJSONElement(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem),aelem))
              EndIf
            Case #PB_JSON_Boolean
              result$=Str(GetJSONBoolean(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem)))
              
          EndSelect
          
        EndIf
        
      Case #PB_JSON_Boolean
        result$=Str(GetJSONBoolean(GetJSONMember(JSONValue(js), pname)))
        
        
      Case #PB_JSON_Number
        
        result$= Str(GetJSONInteger(GetJSONMember(JSONValue(js), pname)))
        
        
    EndSelect
    
  EndIf
  
  ProcedureReturn result$
EndProcedure

Procedure.s commpressed2uncomressedPub(ha$)
  Protected y_parity, ruc$, x$, a$, *a, *res
  Shared *CurveP
  *a = AllocateMemory(64)  
  *res=*a + 32  
  
  y_parity = Val(Left(ha$,2))-2
  x$ = Right(ha$,Len(ha$)-2)
  
  a$=RSet(x$, 64,"0")
  Curve::m_sethex32(*a, a$)  
  Curve::m_YfromX64(*res,*a, *CurveP)  
  
  If PeekB(*res)&1<>y_parity
    Curve::m_subModX64(*res,*CurveP,*res,*CurveP)
  EndIf
  
  ruc$ = Curve::m_gethex32(*res)
  
  FreeMemory(*a)
  ProcedureReturn x$+ruc$
  
EndProcedure

Procedure.s uncomressed2commpressedPub(ha$)
  Protected  x$,y$,ru$,rc$
  ha$=LCase(ha$)
  If Left(ha$,2)="04" And Len(ha$)=130
    ha$=Right(ha$,Len(ha$)-2)
  EndIf
  
  
  x$=RSet(Left(ha$,64),64,"0")  
  y$=RSet(Right(ha$,64),64,"0")
  ru$="04"+x$+y$
  If FindString("13579bdf",Right(y$,1))>0
    rc$="03"+x$
  Else
    rc$="02"+x$
  EndIf
  
  ProcedureReturn rc$
  
EndProcedure


Procedure.s GetPrivateKey()
  Protected a$
  a$ =Trim(ComputerName() + CPUName())
  ProcedureReturn LCase(SHA1Fingerprint(@a$, StringByteLength(a$)))
EndProcedure

Procedure.s GetPublicKey(priv.s)
  Protected a$, *a=AllocateMemory(32), *cx=AllocateMemory(32), *cy=AllocateMemory(32)
  Shared *CurveGY, *CurveGX, *CurveP
  
  Curve::m_sethex32(*a, priv)
  Curve::m_PTMULX64(*cx, *cy, *CurveGx, *CurveGY, *a,*CurveP)
  
  a$ = uncomressed2commpressedPub(Curve::m_gethex32(*cx) + Curve::m_gethex32(*cy))
  FreeMemory(*a)
  FreeMemory(*cx)
  FreeMemory(*cy)
  ProcedureReturn LCase(a$)
EndProcedure

Procedure.s GetSecretKey(priv.s, compressedPub.s)
  Protected a$, *prv=AllocateMemory(32), *secr=AllocateMemory(32), *ax=AllocateMemory(32), *ay=AllocateMemory(32), *cx=AllocateMemory(32), *cy=AllocateMemory(32), uncompressedPub.s, x$, y$
  Shared *CurveGY, *CurveGX, *CurveP
  
  uncompressedPub = commpressed2uncomressedPub(compressedPub)
  x$=RSet(Left(uncompressedPub,64),64,"0")  
  y$=RSet(Right(uncompressedPub,64),64,"0")
  
  Curve::m_sethex32(*ax, x$)
  Curve::m_sethex32(*ay, y$)  
  Curve::m_sethex32(*prv, priv)
  
  Curve::m_PTMULX64(*cx, *cy, *ax, *ay, *prv,*CurveP)  
  a$ = Curve::m_gethex32(*cx) 
  
  FreeMemory(*prv)
  FreeMemory(*ax)
  FreeMemory(*ay)
  FreeMemory(*cx)
  FreeMemory(*cy)
  ProcedureReturn LCase(a$)
  
EndProcedure

; some random generated data uisng CryptRandom()
; size of Key = 32 (AES-256Bits)
; size of InitVect = 16 (CBC)
; PaddingB64 is a random 64-char string needed for padding short strings used with Base64Encoding and AES
DataSection
  ENC_InitVect:
  Data.b  $8C,$0D,$4E,$ED,$67,$96,$FB,$F0,$D3,$72,$87,$35,$3E,$BE,$42,$A2
  ENC_PaddingB64:
  Data.s  "c1e39477b19d95223577a457960959be693060b0c683ec7aaf127d87807c6e02"
EndDataSection

Procedure.i _EncBuffer(*Input, *Output)
  Shared settings() 
  If (MemorySize(*Input) >= 16) And (MemorySize(*Input) = MemorySize(*Output))
    If AESEncoder(*Input, *Output, MemorySize(*Input), settings("1")\SecretBuf32, 256, ?ENC_InitVect, #PB_Cipher_CBC)
      ProcedureReturn #True
    EndIf
  EndIf
  ProcedureReturn #False   ; AES cant encrypt a memory block with size < 16
  
EndProcedure

Procedure.i _DecBuffer(*Input, *Output)
  Shared settings() 
  If (MemorySize(*Input) >= 16) And (MemorySize(*Input) = MemorySize(*Output))
    If AESDecoder(*Input, *Output, MemorySize(*Input), settings("1")\SecretBuf32, 256, ?ENC_InitVect, #PB_Cipher_CBC)
      ProcedureReturn #True
    EndIf
  EndIf
  ProcedureReturn #False   ; AES cant encrypt a memory block with size < 16
  
EndProcedure

Procedure.s EncString(in.s)
  Protected paddStr.s, wrd.s, out.s, *p, *mb0, *mb1, *mb2, n0, n1, n2
  
  If in = "" : ProcedureReturn "" : EndIf
  
  Restore ENC_PaddingB64
  Read.s paddStr
  
  ; we always padd as input could be too short
  n0      = StringByteLength(paddStr) + StringByteLength(in) + SizeOf(Character)
  *mb0    = AllocateMemory(n0) : *p = *mb0
  CopyMemoryString(@paddStr, @*p)
  CopyMemoryString(@in)
  
  ; encrypting using AES
  n1      = n0
  *mb1    = AllocateMemory(n1)
  _EncBuffer(*mb0, *mb1)
  
  ; encoding encrypted buffer using Base64 to mask \0 if any (needed for PeekS())
  n2      = n1 * 1.4
  *mb2    = AllocateMemory(n2)
  Base64Encoder(*mb1, n1, *mb2, n2)
  
  ; encrypted buffer is now ready for PeekS()
  out    = PeekS(*mb2)
  
  FreeMemory(*mb0)
  FreeMemory(*mb1)
  FreeMemory(*mb2)
  
  ProcedureReturn out
  
EndProcedure

Procedure.s DecString(in.s)
  ; decrypts a string encrypted with EncString()
  Protected paddStr.s, wrd.s, out.s, *mb1, *mb2, n0, n1, n2
  
  If in = "" : ProcedureReturn "" : EndIf
  
  ; decode encrypted string using Base64 --> encrypted-buffer
  n0      = StringByteLength(in)
  *mb1    = AllocateMemory(n0)
  n1      = Base64Decoder(@in, n0, *mb1, n0)
  *mb1    = ReAllocateMemory(*mb1, n1)        ; adjust size fo encrypted-buffer! 
  
  n2      = n1
  *mb2    = AllocateMemory(n2)
  _DecBuffer(*mb1, *mb2)
  wrd     = PeekS(*mb2)
  
  Restore ENC_PaddingB64
  Read.s paddStr
  
  If Mid(wrd, 1, Len(paddStr)) = paddStr
    out = Mid(wrd, Len(paddStr)+1)
  EndIf
  
  FreeMemory(*mb1)
  FreeMemory(*mb2)
  
  ProcedureReturn out
  
EndProcedure

Procedure.s RanD()
  Protected *Buffer=AllocateMemory(32), a$
  Shared settings()
  RandomData(*Buffer, 32)
  a$ = SHA1Fingerprint(*Buffer, 32)
  settings("1")\RanD = a$
  FreeMemory(*Buffer)
  ProcedureReturn a$
EndProcedure

Procedure AcceptSkipPair(isAccept)
  Protected tempjson, get_work, Values, get_string.s
  Shared settings()
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #answerpair_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "answerpair")
    Values =SetJSONArray(AddJSONMember(get_work, "params"))       
    SetJSONString(AddJSONElement(Values), settings("1")\AskPairName)
    If isAccept
      SetJSONString(AddJSONElement(Values), RanD())
    Else
      SetJSONString(AddJSONMember(get_work, "error"), "Connection skiped")
    EndIf
    SetJSONBoolean(AddJSONMember(get_work, "result"), isAccept) 
    
    get_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
    SendQuestion(settings("1")\Connect,get_string) 
  EndIf
EndProcedure

Procedure IamReadytoContinue()
  Protected tempjson, get_work, Values, get_string.s
  Shared settings()
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #continue_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "continue")                          
    get_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
    SendQuestion(settings("1")\Connect,get_string) 
  EndIf
EndProcedure

Procedure AskPair(pairname$)
  Protected tempjson, get_work, send_string.s, Values
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect    
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #askpair_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "askpair")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))       
      SetJSONString(AddJSONElement(Values), pairname$)
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    SendQuestion(settings("1")\Connect,send_string) 
  EndIf
EndProcedure

Procedure AskFileClipBoard()
  Protected tempjson, get_work, send_string.s, Values
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect    
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #askfileclip_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "askfileclip")      
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    SendQuestion(settings("1")\Connect,send_string) 
  EndIf
EndProcedure

Procedure AskSendingData(frameid, full_size, computeCRC32file, WpartX, WpartY, partX, partY)
  Protected tempjson, get_work, send_string.s, Values, clip$
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect And settings("1")\isBroadcast
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #sendpart_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "datapart")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))      
      SetJSONInteger(AddJSONElement(Values), full_size)     
      SetJSONString(AddJSONElement(Values), Hex(computeCRC32file))
      SetJSONInteger(AddJSONElement(Values), WpartX)
      SetJSONInteger(AddJSONElement(Values), WpartY)
      SetJSONInteger(AddJSONElement(Values), partX)
      SetJSONInteger(AddJSONElement(Values), partY)
      SetJSONInteger(AddJSONElement(Values), frameid)
      clip$ = GetClipboardText()
      If settings("1")\clipboardtext <> clip$ And clip$<>""  
        If settings("1")\Newclipboardtext= clip$
          settings("1")\clipboardtext = clip$          
        Else
          settings("1")\clipboardtext = clip$
          settings("1")\Newclipboardtext = clip$
          SetJSONString(AddJSONElement(Values), "t"+EncString(settings("1")\clipboardtext))
        EndIf        
      ElseIf settings("1")\Newclipboardfilename <> settings("1")\clipboardfilename And settings("1")\Newclipboardfilename<>""
        settings("1")\clipboardfilename = settings("1")\Newclipboardfilename
        SetJSONString(AddJSONElement(Values), "f"+EncString(settings("1")\clipboardfilename))
      EndIf
      
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    SendQuestion(settings("1")\Connect,send_string) 
  EndIf
EndProcedure

Procedure AskSendingFile(full_size, computeCRC32file, Name.s, isLastFile)
  Protected tempjson, get_work, send_string.s, Values
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect  And settings("1")\IamFileTranciever
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #filepart_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "filepart")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))      
      SetJSONInteger(AddJSONElement(Values), full_size)     
      SetJSONString(AddJSONElement(Values), Hex(computeCRC32file))
      SetJSONString(AddJSONElement(Values), Name)  
      SetJSONBoolean(AddJSONMember(get_work, "lastfile"), isLastFile) 
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    SendQuestion(settings("1")\Connect,send_string) 
  EndIf
EndProcedure

Procedure sendPing()
  Protected tempjson, get_work, send_string.s
  Shared settings()
  tempjson = CreateJSON(#PB_Any)
  If settings("1")\isAuthorized And settings("1")\Connect
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #ping_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "ping")    
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
      SendQuestion(settings("1")\Connect,send_string) 
    EndIf
  EndIf
EndProcedure

Procedure sendBackFrameId(frameid)
  Protected tempjson, get_work, send_string.s, Values
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #sendpart_id) 
      SetJSONInteger(AddJSONMember(get_work, "result"), frameid)                            
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
      SendQuestion(settings("1")\Connect,send_string) 
    EndIf    
  EndIf
EndProcedure

Procedure SendTextFrame(text.s)
  Protected tempjson, get_work, send_string.s, Values
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #text_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "text")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))       
      SetJSONString(AddJSONElement(Values), text)                            
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
      SendQuestion(settings("1")\Connect,send_string) 
    EndIf    
  EndIf
EndProcedure

Procedure SendKeyFrame(symbol.i, isPresed)
  Protected tempjson, get_work, send_string.s, Values
  Static lastsymbol, lastisPresed
  Shared settings()
  If symbol<>lastsymbol Or  lastisPresed<>isPresed
    lastsymbol = symbol : lastisPresed = isPresed
    If settings("1")\isAuthorized And settings("1")\Connect
      tempjson = CreateJSON(#PB_Any)
      If tempjson   
        get_work = SetJSONObject(JSONValue(tempjson))   
        SetJSONInteger(AddJSONMember(get_work, "id"), #text_id) 
        SetJSONString(AddJSONMember(get_work, "method"), "keyboard")
        Values =SetJSONArray(AddJSONMember(get_work, "params")) 
        SetJSONString(AddJSONElement(Values), EncString("key:"+Str(symbol)+":"+Str(isPresed)))        
        send_string=ComposeJSON(tempjson)
        FreeJSON(tempjson)
        SendQuestion(settings("1")\Connect,send_string) 
      EndIf    
    EndIf
  EndIf
EndProcedure

Procedure SendMouseFrame(posX, posY, isLBpressed, isMBpressed, isRBpressed, isWheel)
  Protected tempjson, get_work, send_string.s, Values, scaleH.d, scaleW.d
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect And settings("1")\isReadyRecieve
    scaleH = settings("1")\frameH / settings("1")\canvasH
    scaleW = settings("1")\frameW / settings("1")\canvasW
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #text_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "mouse")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))       
      SetJSONInteger(AddJSONElement(Values), scaleH * posX)  
      SetJSONInteger(AddJSONElement(Values), scaleW * posY)  
      SetJSONInteger(AddJSONElement(Values), isLBpressed)  
      SetJSONInteger(AddJSONElement(Values), isMBpressed)  
      SetJSONInteger(AddJSONElement(Values), isRBpressed)  
      SetJSONInteger(AddJSONElement(Values), isWheel)
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
      SendQuestion(settings("1")\Connect,send_string) 
    EndIf    
  EndIf
EndProcedure

Procedure SetQualityFrame(imgQuality)
  Protected tempjson, get_work, send_string.s, Values
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect And settings("1")\isReadyRecieve
    If imgQuality<0
      imgQuality =0
    ElseIf imgQuality>9
      imgQuality = 9
    EndIf
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #text_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "setquality")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))       
      SetJSONInteger(AddJSONElement(Values), imgQuality)     
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
      SendQuestion(settings("1")\Connect,send_string) 
    EndIf    
  EndIf
EndProcedure



Procedure SendClipboardTxt()
  Protected tempjson, get_work, send_string.s, Values
  Shared settings()
  If settings("1")\isAuthorized And settings("1")\Connect And settings("1")\isReadyRecieve
    
    tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #set_clipboard_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "setclipboardtxt")  
      Values =SetJSONArray(AddJSONMember(get_work, "params"))       
      SetJSONString(AddJSONElement(Values), EncString(settings("1")\clipboardtext))   
      send_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
      SendQuestion(settings("1")\Connect,send_string) 
    EndIf    
  EndIf
EndProcedure

Procedure Mouse_Wheel (Delta)
  Protected In.INPUT
  In\type        = #INPUT_MOUSE;
  In\mi\dwFlags  = #MOUSEEVENTF_WHEEL
  In\mi\mouseData = Delta*120
  SendInput_(1,@In,SizeOf(INPUT))
EndProcedure

Procedure MouseSetPos(x.i, y.i)
  SetCursorPos_(x, y)
EndProcedure

Procedure MouseLBdown()
  mouse_event_(#MOUSEEVENTF_LEFTDOWN,0,0,0,0)
EndProcedure

Procedure MouseMBdown() 
  mouse_event_(#MOUSEEVENTF_MIDDLEDOWN,0,0,0,0)  
EndProcedure

Procedure MouseRBdown()  
  mouse_event_(#MOUSEEVENTF_RIGHTDOWN,0,0,0,0)    
EndProcedure

Procedure MouseLBup()
  mouse_event_(#MOUSEEVENTF_LEFTUP,0,0,0,0)
EndProcedure

Procedure MouseRBup()
  mouse_event_(#MOUSEEVENTF_RIGHTUP,0,0,0,0)  
EndProcedure

Procedure MouseMBup()
  mouse_event_(#MOUSEEVENTF_MIDDLEUP,0,0,0,0)  
EndProcedure

Procedure m_check_equilASM(*s,*t, w, h, linelenght, depth)  
  !mov r10,[p.v_linelenght]
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]  
  !mov r9, rsi
  !mov r8, rdi
  
  !mov ch,[p.v_depth]
  !mov cl,ch
  !mov dl,[p.v_w]
  !mov dh,[p.v_h]
  !llm_check_equil_continue2:
  
  !mov al,[rsi]
  !mov bl,[rdi]
  !add rsi,1
  !add rdi,1 
  
  !cmp al,bl
  !jne llm_check_equil_exit_noteqil2
  !dec cl
  !cmp cl,0  
  !jnz llm_check_equil_continue2
  !mov cl,ch
  
  
  !dec dl 
  !cmp dl,0  
  !jnz llm_check_equil_continue2
  !mov dl,[p.v_w]
  !dec dh
  !cmp dh,0
  !jz llm_check_equil2
  !mov rsi, r9
  !mov rdi, r8
  
  !mov rax,r10
  !add rsi,rax
  !add rdi,rax
  !mov r9, rsi
  !mov r8, rdi
  
  !jmp llm_check_equil_continue2  
  
  !llm_check_equil2:  
  !mov rax,1
  !jmp llm_check_equil_exit2
  
  !llm_check_equil_exit_noteqil2:  
  !mov rax,0
  !llm_check_equil_exit2:
  
  ProcedureReturn  
EndProcedure

Procedure CopyRectangleASM(*s,*t, w, h, linelenghtS, linelenghtT, depth)
  
  !mov r10,[p.v_linelenghtS]  
  !mov r11,[p.v_linelenghtT]
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]  
  !mov r9, rsi
  !mov r8, rdi
  
  !mov ch,[p.v_depth]
  !mov cl,ch
  !mov dl,[p.v_w]
  !mov dh,[p.v_h]
  !copy_continue:
  
  !mov al,[rsi]
  !mov [rdi], al
  !add rsi,1
  !add rdi,1 
  
  !dec cl
  !cmp cl,0  
  !jnz copy_continue
  !mov cl,ch
  
  
  !dec dl 
  !cmp dl,0  
  !jnz copy_continue
  !mov dl,[p.v_w]
  !dec dh
  !cmp dh,0
  !jz copy_exit
  !mov rsi, r9
  !mov rdi, r8
  
  !mov rax,r10
  !add rsi,rax
  !mov rax,r11
  !add rdi,rax
  !mov r9, rsi
  !mov r8, rdi
  
  !jmp copy_continue  
  
  !copy_exit:  
  
EndProcedure

Procedure CopyRectangle(*s,*t, w, h, linelenghtS, linelenghtT, depth)
  Protected *localS, *localT, localH
  *localS = *s
  *localT = *t
  localH  = h
  While localH
    CopyMemory(*localS, *localT, w * depth)
    *localS + linelenghtS
    *localT + linelenghtT
    localH -1 
  Wend
EndProcedure

Procedure.i m_check_equil(*s,*t, w, h, linelenght, depth)
  Protected *localS, *localT, localH, result = 1
  *localS = *s
  *localT = *t
  localH  = h
  While localH
    If Not CompareMemory(*localS, *localT,  w * depth)
      result=0
      Break
    EndIf
    *localS + linelenght
    *localT + linelenght
    localH -1 
  Wend
  ProcedureReturn result
EndProcedure

Procedure getpart(h.i, part.i )
  While h%part
    part+1
  Wend
  ProcedureReturn part
EndProcedure

Procedure resizeCanvas(canvid.i, w.i, h.i)
  Protected curwinW, curwinH, scalex.d, offsetTopY = 90, offsetBottomY = 20, offsetX = 10, newW, newH
  Shared settings()
  curwinW = WindowWidth(0)  
  curwinH = WindowHeight(0) 
  
  scalex = h / w
  
  If curwinW>=w And curwinH>=h + offsetTopY + 5
    newW = w
    newH = h
    offsetBottomY = 5
  Else
    newW = curwinW-20
    newH = (curwinW-20) * scalex
  EndIf  
  
  If newH + offsetTopY + offsetBottomY>curwinH
    scalex = w / h
    newH = curwinH - offsetTopY - offsetBottomY
    newW = newH * scalex
  EndIf
  
  offsetX = (curwinW -newW)/2
  offsetTopY = (curwinH-offsetTopY-newH)/2+offsetTopY
  settings("1")\canvasH = newH
  settings("1")\canvasW = newW
  ResizeGadget(#CanvasGaget, offsetX, offsetTopY, settings("1")\canvasW, settings("1")\canvasH)
  If IsImage(1) And IsImage(5)
    If settings("1")\canvasH = h And settings("1")\canvasW = w
      SetGadgetAttribute(#CanvasGaget, #PB_Canvas_Image , ImageID(1))
    Else
      scalex = h/w      
      CopyImage(1, 5)      
      curwinW = settings("1")\canvasW       
      If Not ResizeImage(5, curwinW , curwinW*scalex)
        Debug "gg"
      EndIf
      SetGadgetAttribute(#CanvasGaget, #PB_Canvas_Image , ImageID(5))  
    EndIf
    
  EndIf
EndProcedure


Procedure addPart(i)
  Protected msginit$="[ADDPART]", *Buffer, *BufferAES, WpartX, WpartY, partX, partY, jobkey$, j, currentJob, precurrentJob, isInform=#False
  Protected w, h, *ImageAddress, depth, size, *buftemp, *buftemp_unalign, buflinewidth, buflinewidthOld, scalex.d, buflinewidth2img, insecondsend, speed.f
  Protected imagsize, possize, *posArray, copiedElements, *pointerPosAyyay, *bufPointer, ix, jx, addpointerx, addpointer, curwinW, curwinH, newheight, copiedElementsTemp
  Protected filename$, TransferFilesize, computeCRC32file, totalloadbytes, maxloadbytes, loadedBytes, *pp, resultUploadedBytes, boxw.f, lastboxupdatetimer, totaltransferfiles, starttime
  Shared settings(), req(), jobMutex, totalsended, DroppedFrames
  
  settings("1")\thrAP = 1
  Repeat
    
    If isInform = #True And settings("1")\isReadyRecieve=#False
      Sprint(msginit$+" Thread stoped")      
      isInform = #False      
    EndIf    
    
    If settings("1")\thrAPquit = #False
      If isInform = #False And settings("1")\isReadyRecieve=#True
        Sprint(msginit$+" Thread started")
        isInform = #True
        If IsImage(1)
          FreeImage(1)
          HideGadget(#CanvasGaget, 1)
          HideGadget(#SpinGagetImgQuality, 1)          
          HideGadget(#ButtonGadgetServer, 0)
        EndIf
      EndIf
      
      If settings("1")\isReadyRecieve=#True        
        If settings("1")\IamFileTranciever = #False
          If settings("1")\IamFileReciever = #False
            If MapSize(req())>0
              If settings("1")\thrAPquit = #False And settings("1")\isReadyRecieve=#True
                currentJob + 1  
                jobkey$ = Str(currentJob)
                LockMutex(jobMutex) 
                
                
                If Not FindMapElement(req(), jobkey$)
                  Sprint(msginit$+" Can`t find map["+jobkey$+"]")
                EndIf
                *Buffer = req(jobkey$)\pointer
                WpartX = req(jobkey$)\WpartX
                WpartY = req(jobkey$)\WpartY
                settings()\WpartX = WpartX
                settings()\WpartY = WpartY
                partX = req(jobkey$)\partX
                partY = req(jobkey$)\partY  
                size = MemorySize(req(jobkey$)\pointer)
                UnlockMutex(jobMutex)
                
                
                
                copiedElements = PeekU(*Buffer)
                possize = 2 +  copiedElements * 2
                imagsize = size - possize
                
                *BufferAES = AllocateMemory(imagsize)
                AESDecoder(*Buffer + possize, *BufferAES, MemorySize(*BufferAES), settings("1")\SecretBuf32, 256, ?ENC_InitVect, #PB_Cipher_CBC)
                
                If Not CatchImage(2, *BufferAES, imagsize)
                  Sprint(msginit$+" Can`t catch image")
                  settings("1")\dis = #True
                EndIf
                FreeMemory(*BufferAES)
                If settings("1")\dis = #False
                  Debug ">>>>copiedElements:" + Str(copiedElements)
                  *posArray = AllocateMemory(possize - 2)  
                  CopyMemory(*Buffer + 2, *posArray, possize - 2)
                  
                  LockMutex(jobMutex)
                  FreeMemory(req(jobkey$)\pointer)
                  DeleteMapElement(req(), jobkey$) 
                  UnlockMutex(jobMutex)
                  
                  
                  w = WpartX * partX
                  h = WpartY * partY 
                  
                  settings("1")\frameW = w
                  settings("1")\frameH = h
                  
                  StartDrawing(ImageOutput(2))
                  *ImageAddress = DrawingBuffer()
                  depth = ImageDepth(2)/8
                  buflinewidth = DrawingBufferPitch()
                  imagsize = buflinewidth * OutputHeight()
                  Debug ">>>>buflinewidth:"+Str(buflinewidth)
                  
                  size = w * h * depth
                  If *buftemp_unalign
                    If MemorySize(*buftemp_unalign)<>size+#align_size
                      Debug "NEWW"      
                      FreeMemory(*buftemp_unalign)
                      
                      *buftemp_unalign = AllocateMemory(size+#align_size, #PB_Memory_NoClear)
                      *buftemp= *buftemp_unalign + #align_size-(*buftemp_unalign % #align_size)
                    EndIf 
                  Else
                    *buftemp_unalign = AllocateMemory(size+#align_size, #PB_Memory_NoClear)
                    *buftemp= *buftemp_unalign + #align_size-(*buftemp_unalign % #align_size)
                  EndIf
                  
                  CopyMemory(*ImageAddress, *buftemp, imagsize)
                  StopDrawing()  
                  FreeImage(2)
                  buflinewidthOld = buflinewidth
                                
                                
                  If IsImage(1)
                    If ImageWidth(1)<>w Or ImageHeight(1)<>h
                      FreeImage(1)
                      CreateImage(1, w, h, depth * 8)
                      resizeCanvas(#CanvasGaget, w, h)
                      StartDrawing(CanvasOutput(#CanvasGaget))
                      FillMemory(DrawingBuffer(), DrawingBufferPitch() * OutputHeight(), $FF)
                      StopDrawing()
                    EndIf
                  Else
                    HideGadget(#CanvasGaget, 0)
                    HideGadget(#SpinGagetImgQuality, 0)
                    resizeCanvas(#CanvasGaget, w, h)
                    CreateImage(1, w, h, depth * 8)                                     
                    HideGadget(#ButtonGadgetServer, 1)
                  EndIf
                  
                  
                  
                  StartDrawing(ImageOutput(1))
                  *ImageAddress = DrawingBuffer()
                  buflinewidth= DrawingBufferPitch()
                  
                  *bufPointer = *buftemp
                  *pointerPosAyyay = *posArray
                  
                  jx = 0
                  ix = 0
                  copiedElementsTemp = copiedElements
                  While copiedElementsTemp
                    
                    j = PeekA(*pointerPosAyyay)
                    i = PeekA(*pointerPosAyyay + 1)
                    *pointerPosAyyay + 2
                    addpointer = i * WpartY * buflinewidth + j * WpartX * depth
                    addpointerx = ix * WpartY * buflinewidthOld + jx * WpartX * depth
                    CopyRectangleASM( *bufPointer + addpointerx, *ImageAddress + addpointer, WpartX, WpartY, buflinewidthOld, buflinewidth, depth)
                    jx+1
                    If jx>=partX
                      ix+1
                      jx=0
                    EndIf 
                    copiedElementsTemp - 1
                  Wend
                  
                  StopDrawing()
                  
                  If settings("1")\canvasH = h And settings("1")\canvasW = w
                    Debug "No resize"
                    SetGadgetAttribute(#CanvasGaget, #PB_Canvas_Image , ImageID(1))
                  Else
                    scalex = h/w                  
                    CopyImage(1, 5)
                    
                    ;StartDrawing(ImageOutput(5))
                    ;DrawingMode(#PB_2DDrawing_Outlined )
                    
                    ;copiedElementsTemp = copiedElements
                    ;*pointerPosAyyay = *posArray
                    ;While copiedElementsTemp
                    ;j = PeekA(*pointerPosAyyay)
                    ;i = PeekA(*pointerPosAyyay + 1)
                    ;*pointerPosAyyay + 2
                    ;Box(j * WpartX, h-(i+1) * WpartY, WpartX, WpartY, #Red)
                    ;copiedElementsTemp-1
                    ;Wend
                    ;DrawingMode(#PB_2DDrawing_Default )
                    ;StopDrawing()
                    curwinW = settings("1")\canvasW 
                    
                    If Not ResizeImage(5, curwinW , curwinW * scalex)
                      Debug "gg"
                    EndIf
                    SetGadgetAttribute(#CanvasGaget, #PB_Canvas_Image , ImageID(5))
                  EndIf
                  
                  
                  
                  FreeMemory(*posArray)
                  
                  
                EndIf
                
                
                
                
              EndIf
            EndIf
          Else
            
            
            
            
            ;recieved file draw info
            i=0
            scalex = h/w
            curwinW = settings("1")\canvasW 
            CreateImage(6, curwinW , curwinW * scalex, 24)
            lastboxupdatetimer = ElapsedMilliseconds()
            filename$ = ""
            totaltransferfiles = CountString(settings("1")\IamFileTrancieverName, #LF$)
            While  And settings("1")\Connect And settings("1")\isReadyRecieve And settings("1")\IamFileReciever
              If ElapsedMilliseconds()-lastboxupdatetimer>50
                lastboxupdatetimer = ElapsedMilliseconds()
                If settings("1")\TransferName<>filename$
                  i+1
                  filename$ = settings("1")\TransferName
                  starttime = ElapsedMilliseconds()
                  StartDrawing(ImageOutput(6))               
                  Box(0,0,curwinW , curwinW * scalex, $000000)
                  DrawingMode(#PB_2DDrawing_Outlined )
                  Box(curwinW/4, (curwinW * scalex - 40)/2, curwinW/2, 40 , $FFFFFF) 
                  
                  DrawText(curwinW/4, (curwinW * scalex - 40)/2 - 20, filename$, $FFFFFF)
                  
                  DrawText(curwinW/4, (curwinW * scalex - 40)/2 + 50, "Downloading.."+Str(i)+"/"+Str(totaltransferfiles), $FFFFFF)
                  
                  DrawingMode(#PB_2DDrawing_Default)   
                  StopDrawing()
                EndIf
                StartDrawing(ImageOutput(6))             
                DrawingMode(#PB_2DDrawing_Default) 
                boxw = settings("1")\TransferFileUploaded / settings("1")\TransferFilesize * (curwinW/2-4)
                Box(curwinW/4+2, (curwinW * scalex - 40)/2+2 , boxw, 36 , $00FF00)
                If (ElapsedMilliseconds() - starttime)>0
                  boxw = (ElapsedMilliseconds() - starttime)/1000.0
                  speed = (settings("1")\TransferFileUploaded /  boxw)/ #MB
                Else
                  speed=0.0
                EndIf
                Box(curwinW/4+150,(curwinW * scalex - 40)/2 + 50,200 , 40, $000000)
                DrawText(curwinW/4+150, (curwinW * scalex - 40)/2 + 50, StrD(speed,2)+"MB/s", $FFFFFF)
                
                StopDrawing()
                SetGadgetAttribute(#CanvasGaget, #PB_Canvas_Image , ImageID(6))                  
              EndIf
              Delay(1)
            Wend
            FreeImage(6)
            
          EndIf
          
        Else
          ;send file
          If settings("1")\IamsendallFiles = #False
            i=1
            scalex = h/w
            curwinW = settings("1")\canvasW 
            filename$ = StringField(settings("1")\IamFileTrancieverName, i, #LF$)
            CreateImage(6, curwinW , curwinW * scalex, 24)           
            lastboxupdatetimer = ElapsedMilliseconds()
            totaltransferfiles = CountString(settings("1")\IamFileTrancieverName, #LF$)
            While filename$<>"" And settings("1")\Connect And settings("1")\isReadyRecieve
              
              StartDrawing(ImageOutput(6))               
              Box(0,0,curwinW , curwinW * scalex, $000000)
              DrawingMode(#PB_2DDrawing_Outlined )
              Box(curwinW/4, (curwinW * scalex - 40)/2, curwinW/2, 40 , $FFFFFF) 
              
              DrawText(curwinW/4, (curwinW * scalex - 40)/2 - 20, GetFilePart(filename$), $FFFFFF)
              
              DrawText(curwinW/4, (curwinW * scalex - 40)/2 + 50, "Uploading.."+Str(i)+"/"+Str(totaltransferfiles), $FFFFFF)
              
              DrawingMode(#PB_2DDrawing_Default)   
              StopDrawing()
              starttime = ElapsedMilliseconds()
              
              Debug" send file ["+filename$+"]"
              If ReadFile(#File2,filename$,#PB_File_SharedRead) 
                TransferFilesize = FileSize(filename$)
                computeCRC32file = CRC32FileFingerprint(filename$)
                If i=totaltransferfiles
                  AskSendingFile(TransferFilesize, computeCRC32file, GetFilePart(filename$), #True)
                Else
                  AskSendingFile(TransferFilesize, computeCRC32file, GetFilePart(filename$), #False)
                EndIf
                Delay(5)
                *Buffer=AllocateMemory(#maxtcp)
                
                totalloadbytes = 0
                maxloadbytes=TransferFilesize
                If maxloadbytes>#maxtcp
                  maxloadbytes = #maxtcp
                EndIf
                
                While settings("1")\Connect And settings("1")\isReadyRecieve
                  *pp = *Buffer
                  loadedBytes =ReadData(#File2, *pp, maxloadbytes)
                  Debug "read "+Str(loadedBytes)+"b from file"
                  If loadedBytes
                    Repeat            
                      settings("1")\lastpingTimer  = ElapsedMilliseconds()
                      resultUploadedBytes = SendNetworkData(settings("1")\Connect,*pp, loadedBytes)
                      Debug "upload "+Str(resultUploadedBytes)+"b to server"
                      If resultUploadedBytes=-1
                        resultUploadedBytes = 0
                        Delay(5)
                      EndIf
                      totalsended + resultUploadedBytes/1024
                      
                      totalloadbytes + resultUploadedBytes
                      *pp+resultUploadedBytes
                      loadedBytes - resultUploadedBytes
                    Until loadedBytes = 0 Or settings("1")\Connect=#False Or settings("1")\isReadyRecieve=#False
                  EndIf
                  
                  If totalloadbytes<TransferFilesize
                    If totalloadbytes+maxloadbytes>TransferFilesize
                      maxloadbytes = TransferFilesize-totalloadbytes                
                    EndIf
                  Else
                    Debug "total uploaded "+Str(totalloadbytes)
                    Break
                  EndIf
                  If ElapsedMilliseconds()-lastboxupdatetimer>50
                    lastboxupdatetimer = ElapsedMilliseconds()
                    boxw = totalloadbytes / TransferFilesize * (curwinW/2-4)
                    StartDrawing(ImageOutput(6))
                    Box(curwinW/4+2, (curwinW * scalex - 40)/2+2 , boxw, 36 , $00FF00)
                    
                    If (ElapsedMilliseconds() - starttime)>0
                      boxw = (ElapsedMilliseconds() - starttime)/1000.0
                      speed = (totalloadbytes /  boxw)/ #MB
                    Else
                      speed=0.0
                    EndIf
                    Box(curwinW/4+150,(curwinW * scalex - 40)/2 + 50,200 , 40, $000000)
                    DrawText(curwinW/4+150, (curwinW * scalex - 40)/2 + 50, StrD(speed,2)+"MB/s", $FFFFFF)
                    
                    StopDrawing()
                    SetGadgetAttribute(#CanvasGaget, #PB_Canvas_Image , ImageID(6))
                  EndIf
                  Delay(1)
                Wend
                FreeMemory(*Buffer)
                CloseFile(#File2)
                Sprint(msginit$ + filename$ + " uploaded")
                
              Else
                Sprint("Can`t open file file ["+settings("1")\IamFileTrancieverName+"]")
                settings("1")\dis = #True
                Break
              EndIf
              i+1
              filename$ = StringField(settings("1")\IamFileTrancieverName, i, #LF$)
            Wend
            FreeImage(6)
            settings("1")\IamsendallFiles = #True
          EndIf
          
        EndIf
        
        
        
        
      EndIf
      
      
    EndIf
    Delay(1)
  Until settings("1")\thrAPquit
  
  If IsImage(2)
    FreeImage(2)
  EndIf
  If IsImage(5)
    FreeImage(5)
  EndIf
  If *buftemp_unalign
    FreeMemory(*buftemp_unalign)
  EndIf
  Sprint(msginit$ + "Thread closed")
  settings("1")\thrAP = 0
  
EndProcedure  

Procedure MakeScreenshot(u)
  Protected msginit$="[CASTER]", i,j, hDC, DeskDC, Color, pos, res, scalex.d, *ImageAddress, *TemoBuf1, isInform = #False, *pp, totalloadbytes, maxloadbytes
  Protected resultUploadedBytes
  Protected depth.i, size, partX, partY, WpartX, WpartY, buflinewidth, isEquil, addpointer, *TemoBuffer_unalign, *TemoBuffer, *BlackBuffer_unalign, *BlackBuffer
  Protected copiedElements, bufsize.d, ix, jx, addpointerx, *Buffer, TransferFilesize, *transferBuffer, computeCRC32memory 
  Protected deskW, deskH, *blackPointer, BlackBufferSize, *blackPos, possize, elmentsY.i
  Protected ci.CURSORINFO, iconinfo.ICONINFO, Coordinate.POINT, xpos, ypos, suspendCounter 
  Protected ScreenWidth, ScreenHeight, trgDC, ResultImg, hImage, timerbegin, TempImage
  Protected buflinewidthOld,  computeCRC32file, loadedBytes, filename$, totaltransferfiles, BitBlt
  
  Shared settings(), jobMutex,  totalsended, ImgMutex
  
  
  
  settings("1")\thrMS = 1 
  Repeat
    If isInform = #True And settings("1")\isBroadcast=#False
      Sprint(msginit$+" Broadcast screen stoped")
      deskW=0 : deskH=0
      isInform = #False
      
    EndIf
    While settings("1")\isBroadcast = #False
      If settings("1")\thrMSquit
        Break
      EndIf
      Delay(100)
    Wend
    
    timerbegin = ElapsedMilliseconds()
    
    If settings("1")\thrMSquit = #False
      If isInform = #False And settings("1")\isBroadcast=#True
        Sprint(msginit$+" Broadcast screen started")
        deskW=0 : deskH=0
        isInform = #True
        settings("1")\currentJob = 0
      EndIf
      
      If settings("1")\isBroadcast = #True
        If settings("1")\IamFileTranciever = #False 
          If settings("1")\IamFileReciever = #False            
            
            ScreenWidth = GetSystemMetrics_(#SM_CXSCREEN)
            ScreenHeight = GetSystemMetrics_(#SM_CYSCREEN)
            If deskW<>ScreenWidth Or  deskH<>ScreenHeight  
              ;Sprint(msginit$+" deskW:"+Str(deskW)+"deskH:"+Str(deskH)+" swx:"+Str(ScreenWidth)+"swy:"+Str(ScreenHeight))
              deskW=ScreenWidth
              deskH=ScreenHeight
              depth = 3
              size = depth * deskW * deskH * 2
              partX= getpart(deskW, 64)
              WpartX = deskW/partX
              partY= getpart(deskH, 64)
              WpartY= deskH/partY
              Debug "partX:"+Str(partX)+" partY:"+Str(partY)
              Debug "WpartX:"+Str(WpartX)+" WpartY:"+Str(WpartY)
              If *TemoBuffer_unalign
                FreeMemory(*TemoBuffer_unalign)
              EndIf
              *TemoBuffer_unalign = AllocateMemory(size+#align_size)
              *TemoBuffer= *TemoBuffer_unalign + #align_size-(*TemoBuffer_unalign % #align_size)
              
              If *BlackBuffer_unalign
                FreeMemory(*BlackBuffer_unalign)
              EndIf
              BlackBufferSize = size + (partX * partY * 2 + 2)
              *BlackBuffer_unalign = AllocateMemory(BlackBufferSize +  #align_size)
              *BlackBuffer= *BlackBuffer_unalign + #align_size-(*BlackBuffer_unalign % #align_size)
              
              If IsImage(0)
                FreeImage(0)
              EndIf
              hImage  = CreateImage(0, ScreenWidth, ScreenHeight, 24)
              If Not hImage
                Sprint(msginit$+" Can`t create image0")
                settings("1")\dis = #True
                Break
              EndIf
              If trgDC
                ReleaseDC_(GetDesktopWindow_(),trgDC)
              EndIf
              trgDC = GetDC_(GetDesktopWindow_())
              If Not trgDC
                Sprint(msginit$+" Can`t create trgDC")
                settings("1")\dis = #True
                Break
              EndIf
            ElseIf settings("1")\isChangeQuality
              settings("1")\isChangeQuality = #False
              FillMemory(*TemoBuffer_unalign, size+#align_size)          
            EndIf
            
            If hImage And trgDC
              hDC = StartDrawing(ImageOutput(0))
              bitBlt = BitBlt_(hDC, 0, 0, ScreenWidth, ScreenHeight, trgDC, 0, 0, #SRCCOPY|#CAPTUREBLT)
              ;GetCursorPos_(@Coordinate.POINT)
              ;xpos = Coordinate\x ; x pos of mouse 
              ;ypos = Coordinate\y ; y pos of mouse cursor
              ci\cbSize = SizeOf(CURSORINFO)
              GetCursorInfo_(@ci)
              xpos = ci\ptScreenPos\x
              ypos = ci\ptScreenPos\y
              
              GetIconInfo_(ci\hCursor, iconinfo.ICONINFO)          
              
              DrawIconEx_(hDC, xpos - iconinfo\xHotspot ,ypos - iconinfo\yHotspot, ci\hCursor, 32, 32, 0, 0, #DI_NORMAL|#DI_COMPAT )
              
              *ImageAddress = DrawingBuffer()     
              buflinewidth= DrawingBufferPitch()     
              
              *blackPointer = *BlackBuffer + (partX * partY * 2 + 2)
              *blackPos = *BlackBuffer + 2
              copiedElements = 0
              ix=0
              jx=0
              For i = 0 To partY-1
                For j = 0 To partX-1
                  addpointer = i * WpartY * buflinewidth + j * WpartX * depth
                  isEquil = m_check_equil(*ImageAddress + addpointer,*TemoBuffer + addpointer, WpartX, WpartY, buflinewidth, depth)          
                  If Not isEquil
                    PokeA(*blackPos, j)
                    PokeA(*blackPos+1, i)
                    *blackPos + 2
                    addpointerx = ix * WpartY * buflinewidth + jx * WpartX * depth
                    CopyRectangle( *ImageAddress + addpointer, *blackPointer + addpointerx, WpartX, WpartY, buflinewidth, buflinewidth, depth)  
                    jx+1
                    If jx>=partX
                      ix+1
                      jx=0
                    EndIf            
                    copiedElements + 1
                    ;copy rect to temobuffer
                    CopyRectangle( *ImageAddress + addpointer, *TemoBuffer + addpointer, WpartX, WpartY, buflinewidth, buflinewidth, depth) 
                  EndIf
                Next j
              Next i
              PokeU(*BlackBuffer, copiedElements)  
              
              ;CopyMemory(*ImageAddress, *TemoBuffer, buflinewidth * deskH )
              
              StopDrawing()
              ;!!!! Cursor GDI object should be deleted
              DeleteObject_(iconinfo\hbmMask)
              DeleteObject_(iconinfo\hbmColor)
              
              buflinewidthOld = buflinewidth
              
              
              
              If copiedElements                
                suspendCounter = 0
                elmentsY = Round(copiedElements / partX, #PB_Round_Up)
                If copiedElements>partX
                  ResultImg = CreateImage(3, partX * WpartX, elmentsY * WpartY, 24)
                Else
                  ResultImg = CreateImage(3, copiedElements * WpartX, elmentsY * WpartY, 24)
                EndIf
                
                If ResultImg
                  Debug "partX:"+Str(partX)+" partY:"+Str(partY)+" elmentsY:"+Str(elmentsY)
                  StartDrawing(ImageOutput(3)) 
                  *ImageAddress = DrawingBuffer()  
                  buflinewidth= DrawingBufferPitch()
                  *blackPointer = *BlackBuffer + (partX * partY * 2 + 2)
                  
                    ix=0
                    jx=0
                    elmentsY = copiedElements
                    While elmentsY
                      addpointer = ix * WpartY * buflinewidthOld + jx * WpartX * depth
                      addpointerx = ix * WpartY * buflinewidth + jx * WpartX * depth
                      CopyRectangle(*blackPointer + addpointer,  *ImageAddress + addpointerx, WpartX, WpartY, buflinewidthOld, buflinewidth, depth)  
                      jx+1
                      If jx>=partX
                        ix+1
                        jx=0
                      EndIf 
                      elmentsY - 1
                    Wend
                  
                  StopDrawing()   
                  
                  If settings("1")\imgquality<9                
                    *Buffer = EncodeImage(3, #PB_ImagePlugin_JPEG, settings("1")\imgquality+2, 24)                
                  Else
                    *Buffer = EncodeImage(3, #PB_ImagePlugin_PNG, 0, 24)
                  EndIf
                  FreeImage(3)
                  Debug "MemorySize(*Buffer):"+Str(MemorySize(*Buffer))
                  
                  
                  settings("1")\currentJob + 1
                  possize = 2 + copiedElements * 2
                  
                  TransferFilesize = MemorySize(*Buffer) + possize
                  *transferBuffer = AllocateMemory(TransferFilesize)
                   
                  
                  CopyMemory(*BlackBuffer, *transferBuffer, possize)                  
                  
                  AESEncoder(*Buffer, *transferBuffer + possize, MemorySize(*Buffer), settings("1")\SecretBuf32, 256, ?ENC_InitVect, #PB_Cipher_CBC)
                  
                  computeCRC32memory = CRC32Fingerprint(*transferBuffer, TransferFilesize)
                  
                  ;send data                  
                  AskSendingData(settings("1")\currentJob, TransferFilesize, computeCRC32memory, WpartX, WpartY, partX, partY)
                  Delay(5)
                  totalloadbytes=0
                  maxloadbytes=TransferFilesize
                  If TransferFilesize>#maxtcp
                    maxloadbytes = #maxtcp
                  EndIf   
                  *pp = *transferBuffer
                  While settings("1")\Connect And settings("1")\isBroadcast
                    Debug "["+Str(i)+"] send chunk:"+Str(maxloadbytes)+" b"    
                    
                    resultUploadedBytes = SendNetworkData(settings("1")\Connect,*pp, maxloadbytes)
                    settings("1")\lastpingTimer  = ElapsedMilliseconds()
                    If resultUploadedBytes=-1
                      Delay(5)
                      
                      resultUploadedBytes = 0
                      Sprint(msginit$+" UUpppsss")
                      suspendCounter +1
                      If suspendCounter>5
                        settings("1")\dis = #True
                      EndIf
                      Break
                    Else
                      suspendCounter = 0
                    EndIf
                    totalsended + resultUploadedBytes / 1024 
                    
                    totalloadbytes + resultUploadedBytes
                    *pp+resultUploadedBytes
                    
                    If totalloadbytes<TransferFilesize
                      If totalloadbytes+maxloadbytes>TransferFilesize
                        maxloadbytes = TransferFilesize-totalloadbytes
                      EndIf
                    EndIf
                    
                    If totalloadbytes>=TransferFilesize            
                      Break
                    EndIf
                    Delay(1)
                  Wend
                  
                  
                  FreeMemory(*Buffer)
                  FreeMemory(*transferBuffer)
                EndIf
              Else
                ;no elements to copy so check ping
                If ElapsedMilliseconds() - settings("1")\lastpingTimer>10000
                  sendPing()
                EndIf
              EndIf
            EndIf
            
            
            
          EndIf
          
          
        Else
          ;send file
          If settings("1")\IamsendallFiles = #False
            i=1
            filename$ = StringField(settings("1")\IamFileTrancieverName, i, #LF$)
            totaltransferfiles = CountString(settings("1")\IamFileTrancieverName, #LF$)
            While filename$<>"" And settings("1")\Connect And settings("1")\isBroadcast
              Debug" send file ["+filename$+"]"
              If ReadFile(#File2,filename$,#PB_File_SharedRead) 
                TransferFilesize = FileSize(filename$)
                computeCRC32file = CRC32FileFingerprint(filename$)
                
                If i=totaltransferfiles
                  AskSendingFile(TransferFilesize, computeCRC32file, GetFilePart(filename$), #True)
                Else
                  AskSendingFile(TransferFilesize, computeCRC32file, GetFilePart(filename$), #False)
                EndIf
                
                Delay(5)
                *Buffer=AllocateMemory(#maxtcp)
                
                totalloadbytes = 0
                maxloadbytes=TransferFilesize
                If maxloadbytes>#maxtcp
                  maxloadbytes = #maxtcp
                EndIf
                
                While settings("1")\Connect And settings("1")\isBroadcast
                  *pp = *Buffer
                  loadedBytes =ReadData(#File2, *pp, maxloadbytes)
                  Debug "read "+Str(loadedBytes)+"b from file"
                  If loadedBytes
                    Repeat            
                      settings("1")\lastpingTimer  = ElapsedMilliseconds()
                      resultUploadedBytes = SendNetworkData(settings("1")\Connect,*pp, loadedBytes)
                      Debug "upload "+Str(resultUploadedBytes)+"b to server"
                      If resultUploadedBytes=-1
                        resultUploadedBytes = 0
                      EndIf
                      totalsended + resultUploadedBytes/1024
                      
                      totalloadbytes + resultUploadedBytes
                      *pp+resultUploadedBytes
                      loadedBytes - resultUploadedBytes
                    Until loadedBytes = 0 Or settings("1")\Connect=#False Or settings("1")\isBroadcast=#False
                  EndIf
                  
                  If totalloadbytes<TransferFilesize
                    If totalloadbytes+maxloadbytes>TransferFilesize
                      maxloadbytes = TransferFilesize-totalloadbytes                
                    EndIf
                  Else
                    Debug "total uploaded "+Str(totalloadbytes)
                    Break
                  EndIf
                  Delay(1)
                Wend
                FreeMemory(*Buffer)
                CloseFile(#File2)
                Sprint(msginit$ + filename$ + " uploaded")
                
              Else
                Debug" Can`t open file file ["+settings("1")\IamFileTrancieverName+"]"
                settings("1")\dis = #True
                Break
              EndIf
              i+1
              filename$ = StringField(settings("1")\IamFileTrancieverName, i, #LF$)
            Wend
            settings("1")\IamsendallFiles = #True
          EndIf 
        EndIf
      EndIf
      
      If settings("1")\delayTime - (ElapsedMilliseconds()-timerbegin)>0      
        Delay(settings("1")\delayTime - (ElapsedMilliseconds()-timerbegin))
      Else
        Delay(settings("1")\delayTime)
      EndIf
    EndIf
    
  Until settings("1")\thrMSquit
  
  If IsImage(0)
    FreeImage(0)
  EndIf  
  
  If trgDC
    ReleaseDC_(GetDesktopWindow_(),trgDC)
  EndIf
  
  If *TemoBuffer_unalign
    FreeMemory(*TemoBuffer_unalign)
  EndIf
  
  If *BlackBuffer_unalign
    FreeMemory(*BlackBuffer_unalign)
  EndIf
  Sprint(msginit$ + "Thread closed")
  settings("1")\thrMS = 0
EndProcedure

Procedure kk(a.i, isPresed)
  Protected Dim KeyArray.INPUT( 0 )
  Protected *G_key.INPUT = @KeyArray( 0 )
  *G_key\type = #INPUT_KEYBOARD
  *G_key\ki\wVk = a
  If isPresed
    *G_key\ki\dwFlags = 0
  Else
    *G_key\ki\dwFlags = #KEYEVENTF_KEYUP 
  EndIf
  SendInput_( 1, KeyArray(), 40 )
EndProcedure

Procedure HideAskPair(isHide)   
  HideGadget(#StringGadgetClid, 1-isHide)
  HideGadget(#ButtonGadgetClCon, 1-isHide)
  HideGadget(#ButtonGadgetClCloseCon, 1-isHide)
  
  HideGadget(#txtRequestGadget, isHide)
  HideGadget(#ButtonGadgetRequestYes, isHide)
  HideGadget(#ButtonGadgetRequestNo, isHide)
EndProcedure

Procedure DisGadCon()
  If Len(GetGadgetText(#StringGadgetClid))<>66    
    DisableGadget(#ButtonGadgetClCon, 1)
  Else    
    DisableGadget(#ButtonGadgetClCon, 0)
  EndIf
EndProcedure

Procedure conecthost(i)
  Protected totalloadbytes, maxloadbytes, loadedbytes, *pp, err, batchCRC32, get_work_pair_string.s, quit=#False, timeout, metod_json_answer$
  Protected get_string.s,  pars_res, *Buffer, ReceivedBytes, ReceivedBytesTemp, answer_t$, pos, pos2, answer_f$, tempjson, get_work, Values, get_work_authorize_string.s, id_json_answer,msginit$
  Protected TransferFileCRC32.s,  *transferBuffer, *transferPointer, computeCRC32memory, computeCRC32file, currentJob
  Protected TransferWpartX, TransferWpartY, TransferpartX, TransferpartY, bufsize, *BufferPointer, ReconectTimer, key, ping_string.s, partkey$, clipText$
  Protected TransferType, TransferIsLastFile
  Shared settings(), Roles(), ClientsList(), jobMutex,  req(), totalsended, DroppedFrames, isInCnvasArea,  thrMS, thrAP, keybardmap()
  
  *Buffer = AllocateMemory(65536*20)
  msginit$ ="[GETWORK] "
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #login_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "Login")
    Values =SetJSONArray(AddJSONMember(get_work, "params"))      
    SetJSONString(AddJSONElement(Values), settings("1")\name)     
    SetJSONString(AddJSONElement(Values), settings("1")\pass)
    get_work_authorize_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
  EndIf
  
  
  Sprint(msginit$ + "Thread started")
  settings("1")\thrCN = 1
  settings("1")\dis = #True
  
  Repeat
    If settings("1")\dis 
      
      ResizeWindow(0,  #PB_Ignore,  #PB_Ignore,  600, 100)
      
      HideGadget(#CanvasGaget, 1)
      HideGadget(#SpinGagetImgQuality, 1)
      HideGadget(#ButtonGadgetServer, 0)
      HideGadget(#ButtonGadgetClADD, 1)
      HideGadget(#ButtonGadgetClCon, 0)
      ClearClipboard()
      settings("1")\clipboardtext=""
      settings("1")\clipboardfilename=""
      settings("1")\Newclipboardfilename=""
      DisableGadget(#ButtonGadgetTransferToRemote, 1)
      DisableGadget(#ButtonGadgetTransferFromRemote, 1)
      
      ReceivedBytes = 0      
      DroppedFrames = 0
      settings("1")\lastpingTimer  = ElapsedMilliseconds()
      settings("1")\imgquality = 5
      SetGadgetState (#SpinGagetImgQuality, settings("1")\imgquality)
      
      settings("1")\thrMSquit = #True
      settings("1")\thrAPquit = #True
      settings("1")\delayTime = #minDelay
      settings("1")\isAuthorized =#False
      settings("1")\isBroadcast =#False
      settings("1")\isReadyRecieve = #False
      settings("1")\isPaired = #False
      settings("1")\isWaitPart = #False
      settings("1")\IamFileReciever = #False
      settings("1")\IamFileTranciever = #False
      settings("1")\IamsendallFiles = #True
      DisableGadget(#ButtonGadgetClCloseCon, 1)
      DisableGadget(#ButtonGadgetClCon, 1)
      currentJob = 0
      If settings("1")\Connect
        CloseNetworkConnection(settings("1")\Connect)
        settings("1")\Connect = 0
      EndIf
      
      Delay(1000)
      
      
      
      If *transferBuffer
        FreeMemory(*transferBuffer)
        *transferBuffer = 0
      EndIf
      LockMutex(jobMutex)
      If MapSize(req())
        ForEach req()
          FreeMemory(req()\pointer)
          DeleteMapElement(req()) 
        Next
      EndIf
      UnlockMutex(jobMutex)
      
      If MapSize(keybardmap())
        ForEach keybardmap()          
          kk(Val(MapKey(keybardmap())), 0)
        Next
      EndIf
      ClearMap(keybardmap())      
      
      
      Sprint(msginit$ + "Try conect to "+settings("1")\host)
      settings("1")\Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,2000)
      If Not settings("1")\Connect
        ReconectTimer = ElapsedMilliseconds()
        While Not settings("1")\Connect          
          If settings("1")\thrCNquit
            Break
          EndIf
          
          
          Delay(50)
          If ElapsedMilliseconds() - ReconectTimer>1000  
            Debug "try conect to getwork"
            Sprint(msginit$ + "Try conect to "+settings("1")\host)
            settings("1")\Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,2000)
            ReconectTimer = ElapsedMilliseconds()
          EndIf
        Wend        
      EndIf   
      totalsended = 0
      If settings("1")\thrCNquit = #False 
        settings("1")\dis= #False
        SendQuestion(settings("1")\Connect,get_work_authorize_string) 
        
      EndIf
    EndIf
    
    If settings("1")\Connect And settings("1")\thrCNquit = #False 
      Select NetworkClientEvent(settings("1")\Connect) 
        Case #PB_NetworkEvent_Data   
          
          *BufferPointer = *Buffer
          If ReceivedBytes
            *BufferPointer + ReceivedBytes
            ReceivedBytesTemp = ReceivedBytes
          Else
            ReceivedBytesTemp = 0
          EndIf
          ReceivedBytes = ReceiveNetworkData(settings("1")\Connect, *BufferPointer, 65536) 
          ReceivedBytes + ReceivedBytesTemp
          *BufferPointer = *Buffer
          
          totalsended + ReceivedBytes/1024
          If ReceivedBytes>0
            Repeat 
              If settings("1")\isWaitPart
                ReceivedBytesTemp = ReceivedBytes
                If ReceivedBytes + settings("1")\TransferFileUploaded>settings("1")\TransferFilesize                
                  ReceivedBytes = settings("1")\TransferFilesize - settings("1")\TransferFileUploaded                
                EndIf
                settings("1")\TransferFileUploaded + ReceivedBytes
                Select TransferType
                  Case 0
                    ;Display part
                    CopyMemory(*BufferPointer, *transferPointer, ReceivedBytes) 
                    Debug "ReceivedBytes:"+Str(ReceivedBytes)+"/"+Str(ReceivedBytesTemp)+" TransferFileUploaded:"+Str(settings("1")\TransferFileUploaded)
                    *transferPointer + ReceivedBytes
                    If settings("1")\TransferFileUploaded>=settings("1")\TransferFilesize
                      settings("1")\isWaitPart = #False
                      computeCRC32memory = CRC32Fingerprint(*transferBuffer, settings("1")\TransferFilesize)
                      If LCase(TransferFileCRC32)= LCase(Hex(computeCRC32memory))
                        Debug "VALID CRC32"
                        ; do something with data
                        currentJob + 1                  
                        LockMutex(jobMutex)
                        req(Str(currentJob))\pointer = AllocateMemory(settings("1")\TransferFilesize)        
                        CopyMemory(*transferBuffer, req(Str(currentJob))\pointer, settings("1")\TransferFilesize)             
                        req(Str(currentJob))\WpartX = TransferWpartX
                        req(Str(currentJob))\WpartY = TransferWpartY
                        req(Str(currentJob))\partX = TransferpartX
                        req(Str(currentJob))\partY  = TransferpartY
                        UnlockMutex(jobMutex)
                        Debug "WpartX:"+Str(req(Str(currentJob))\WpartX) +" WpartY:"+Str(req(Str(currentJob))\WpartY)
                        Debug "partX:"+Str(req(Str(currentJob))\partX)+ " partY:"+Str(req(Str(currentJob))\partY)
                      Else
                        Sprint(msginit$ + "INVALID CRC32 FRAME > NEED:"+TransferFileCRC32+", GOT:"+LCase(Hex(computeCRC32memory)))
                      EndIf
                      FreeMemory(*transferBuffer)
                      *transferBuffer = 0
                    EndIf
                  Case 1
                    ;file part                                   
                    If OpenFile(#File2, settings("1")\TransferName+"temp" ,#PB_File_Append)
                      WriteData(#File2, *BufferPointer, ReceivedBytes)
                      CloseFile(#File2)
                    EndIf
                    If settings("1")\TransferFileUploaded>=settings("1")\TransferFilesize
                      settings("1")\isWaitPart = #False
                      computeCRC32file = CRC32FileFingerprint(settings("1")\TransferName+"temp")                   
                      If LCase(TransferFileCRC32) = LCase(Hex(computeCRC32file))
                        If FileSize(settings("1")\TransferName)
                          DeleteFile(settings("1")\TransferName, #PB_FileSystem_Force)
                        EndIf
                        RenameFile(settings("1")\TransferName+"temp", settings("1")\TransferName)
                        Sprint(msginit$ + settings("1")\TransferName+" downloaded")
                      Else
                        Sprint(msginit$ + settings("1")\TransferName+" INVALID CRC32 FILE > NEED:"+TransferFileCRC32+", GOT:"+LCase(Hex(computeCRC32file)))
                        DeleteFile(settings("1")\TransferName+"temp", #PB_FileSystem_Force)
                      EndIf
                      If TransferIsLastFile = #True
                        IamReadytoContinue()
                        settings("1")\IamFileReciever = #False
                      EndIf
                    EndIf
                EndSelect
                
                If ReceivedBytes<>ReceivedBytesTemp 
                  
                  *BufferPointer + ReceivedBytes
                  ReceivedBytes = ReceivedBytesTemp - ReceivedBytes
                  Debug" After:"+Str(ReceivedBytes)
                Else
                  ReceivedBytes = 0
                EndIf
                
              EndIf
              If ReceivedBytes>0 And settings("1")\isWaitPart = #False
                answer_t$=PeekS(*BufferPointer, ReceivedBytes/2,#PB_Unicode)  
                pos2 = 0
                pos=FindString(answer_t$, "{")
                If pos
                  pos2=FindString(answer_t$, "}"+#LF$,pos+1)
                EndIf
                If pos And pos2        
                  ReceivedBytes - pos2 *2 - 2
                  *BufferPointer + pos2 *2 +2
                  answer_f$=Mid(answer_t$,pos,pos2-pos+1)  
                  Debug">>"+answer_f$ +"<<" +Str(ReceivedBytes)
                  answer_f$ = RTrim(answer_f$,"}")
                  answer_f$ = LTrim(answer_f$,"{")                   
                  answer_f$ = "{"+answer_f$+"}"
                  
                  pars_res=ParseJSON(#PB_Any, answer_f$)                    
                  If pars_res
                    metod_json_answer$ = LCase(getElem(pars_res,"method",0))
                    id_json_answer=Val(LCase(getElem(pars_res,"id",0)))
                    If metod_json_answer$<>""
                      ;ASKING
                      If Not settings("1")\isPaired
                        ; When not paired
                        Select metod_json_answer$ 
                            
                          Case "pair"
                            If settings("1")\isAskPair = #False
                              Sprint(msginit$+getElem(pars_res,"params",0)+" ask you")
                              If GetGadgetState(#AutoPairCheckBox)=#PB_Checkbox_Unchecked
                                
                                SetGadgetText(#txtRequestGadget, getElem(pars_res,"params",0)+" ask you")
                                HideAskPair(#False) 
                                
                                settings("1")\isAskPair = #True
                                settings("1")\AskPairName = getElem(pars_res,"params",0)
                                settings("1")\AskPairTime = Date() 
                                settings("1")\AskWinState = GetWindowState(0)
                                SetWindowState(0, #PB_Window_Normal) 
                                SetActiveWindow(0)
                              Else
                                ;auto accept pair
                                If FindMapElement(ClientsList(), getElem(pars_res,"params",0))
                                  settings("1")\AskPairName = getElem(pars_res,"params",0)
                                  Sprint(msginit$+"Auto pair accept") 
                                  settings("1")\Secret = GetSecretKey(settings("1")\PrivateKey, settings("1")\AskPairName)
                                  Curve::m_sethex32(settings("1")\SecretBuf32, settings("1")\Secret)
                                  ;Sprint("Secret Key "+Curve::m_gethex32(settings("1")\SecretBuf32) ,#colorRed)
                                  AcceptSkipPair(#True)
                                Else
                                  SetGadgetText(#txtRequestGadget, getElem(pars_res,"params",0)+" ask you")
                                  HideAskPair(#False) 
                                  
                                  settings("1")\isAskPair = #True
                                  settings("1")\AskPairName = getElem(pars_res,"params",0)
                                  settings("1")\AskPairTime = Date() 
                                  settings("1")\AskWinState = GetWindowState(0)
                                  SetWindowState(0, #PB_Window_Normal) 
                                  SetActiveWindow(0)
                                EndIf
                                
                              EndIf
                            EndIf
                        EndSelect
                      Else
                        ;when paired
                        Select LCase(metod_json_answer$)
                          Case "continue"
                            settings("1")\IamFileTranciever = #False
                            If settings("1")\Role = 0 And settings("1")\isReadyRecieve = #True;master
                              DisableGadget(#CanvasGaget,0)
                              DisableGadget(#SpinGagetImgQuality,0)
                            EndIf
                          Case "askfileclip"
                            If settings("1")\Role = 1 And settings("1")\isBroadcast = #True;slave
                              settings("1")\IamFileTrancieverName = settings("1")\clipboardfilename
                              settings("1")\IamsendallFiles = #False
                              settings("1")\IamFileTranciever = #True
                            EndIf
                          Case "setclipboardtxt"
                            If settings("1")\Role = 1 And settings("1")\isBroadcast = #True;slave                           
                              settings("1")\Newclipboardtext = DecString(getElem(pars_res,"params",0))                                
                              SetClipboardText(settings("1")\Newclipboardtext)                              
                              Sprint(msginit$+"Got clipboard")
                            EndIf                            
                          Case "setquality"
                            If settings("1")\Role = 1 And settings("1")\isBroadcast = #True;slave
                              Values = Val(getElem(pars_res,"params",0))
                              If Values>9
                                Values=9
                              ElseIf Values<0
                                Values = 0
                              EndIf
                              settings("1")\isChangeQuality = #True
                              settings("1")\imgquality = Values
                              If settings("1")\imgquality<9
                                Sprint(msginit$+"Used JPEG image quality "+Str(Values+2))
                              Else
                                Sprint(msginit$+"Used PNG image quality")
                              EndIf
                            EndIf
                          Case "text"
                            Sprint(msginit$+"Got text ["+getElem(pars_res,"params",0)+"]")   
                            Select Left(LCase(getElem(pars_res,"params",0)),5)
                              Case "encod"
                                If settings("1")\Role = 1
                                  If settings("1")\RanD = DecString( Right(getElem(pars_res,"params",0), Len(getElem(pars_res,"params",0))-5 ))
                                    SendTextFrame("ready")
                                  Else
                                    settings("1")\dis = #True
                                  EndIf                                  
                                EndIf
                              Case "ready"
                                If settings("1")\Role = 0;master
                                  SendTextFrame("start")                                  
                                  settings("1")\isReadyRecieve = #True
                                  settings("1")\thrAPquit = #False
                                  thrAP = CreateThread(@addPart(),1)
                                  If WindowHeight(0)<140 
                                    ResizeWindow(0,  #PB_Ignore,  #PB_Ignore,  #PB_Ignore, 600)
                                  EndIf
                                EndIf
                              Case "start"
                                If settings("1")\Role = 1 And settings("1")\isBroadcast = #False;slave                                  
                                  settings("1")\isBroadcast = #True  
                                  settings("1")\thrMSquit = #False
                                  thrMS = CreateThread(@MakeScreenshot(),1)
                                  
                                EndIf                              
                            EndSelect
                          Case "mouse"  
                            If settings("1")\Role = 1 And settings("1")\isBroadcast = #True;slave
                              MouseSetPos(Val(getElem(pars_res,"params",0)), Val(getElem(pars_res,"params",1)))
                              If Val(getElem(pars_res,"params",2))=1
                                MouseLBdown()
                              ElseIf Val(getElem(pars_res,"params",2))=0
                                MouseLBup()
                              EndIf
                              If Val(getElem(pars_res,"params",3))=1
                                MouseMBdown()
                              ElseIf Val(getElem(pars_res,"params",3))=0
                                MouseMBup()
                              EndIf
                              If Val(getElem(pars_res,"params",4))=1
                                MouseRBdown()
                              ElseIf Val(getElem(pars_res,"params",4))=0
                                MouseRBup()
                              EndIf
                              If Val(getElem(pars_res,"params",5))<>0
                                Mouse_Wheel (Val(getElem(pars_res,"params",5)))
                              EndIf
                            EndIf
                          Case "keyboard"
                            If settings("1")\Role = 1 And settings("1")\isBroadcast = #True;slave
                              partkey$ = DecString(getElem(pars_res,"params",0))
                              If Left(partkey$,3) = "key"
                                key = Val(StringField(partkey$, 2, ":"))
                              EndIf                              
                              If Val(StringField(partkey$, 3, ":"))
                                ;keybd_event_(key,0,0,0)
                                kk(key, 1)
                                keybardmap(Str(key))=1
                              Else
                                ;keybd_event_(key,0,#KEYEVENTF_KEYUP,0)
                                If FindMapElement(keybardmap(), Str(key))
                                  DeleteMapElement(keybardmap(), Str(key))
                                EndIf
                                kk(key, 0)
                              EndIf
                            EndIf
                          Case "datapart" 
                            If settings("1")\Role = 0 And settings("1")\isReadyRecieve = #True;master
                              settings("1")\isWaitPart = #True                          
                              settings("1")\TransferFilesize = Val(getElem(pars_res,"params",0))
                              TransferFileCRC32 = LCase(cutHex(getElem(pars_res,"params",1)))
                              TransferWpartX = Val(getElem(pars_res,"params",2))
                              TransferWpartY = Val(getElem(pars_res,"params",3))
                              TransferpartX = Val(getElem(pars_res,"params",4))
                              TransferpartY =  Val(getElem(pars_res,"params",5))
                              If settings("1")\IamFileTranciever = #False 
                                sendBackFrameId(Val(getElem(pars_res,"params",6)))
                              EndIf
                              clipText$ = getElem(pars_res,"params",7)
                              If clipText$<>"" And isInCnvasArea
                                If Left(clipText$,1)="t" 
                                  ;recieve text frome slave clipboard
                                  clipText$ = Right(clipText$, Len(clipText$)-1)
                                  
                                  
                                  settings("1")\Newclipboardtext = DecString(clipText$)                                
                                  SetClipboardText(settings("1")\Newclipboardtext) 
                                  
                                  
                                  Sprint(msginit$+"Got clipboard text")
                                ElseIf Left(clipText$,1)="f" And settings("1")\Role = 0 And settings("1")\isReadyRecieve = #True;master
                                                                                                                                ;recieve filename frome slave clipboard
                                  clipText$ = Right(clipText$, Len(clipText$)-1)
                                  clipText$ = DecString(clipText$)                                
                                  ClearClipboard()
                                  settings("1")\Newclipboardfilename = clipText$                                  
                                  Sprint(msginit$+"Got clipboard file name "+clipText$)                              
                                EndIf
                              EndIf
                              TransferType = 0  
                              settings("1")\TransferFileUploaded = 0
                              *transferBuffer = AllocateMemory(settings("1")\TransferFilesize)
                              *transferPointer = *transferBuffer
                            EndIf
                          Case "filepart"                            
                            settings("1")\isWaitPart = #True 
                            settings("1")\IamFileReciever = #True
                            settings("1")\TransferFilesize = Val(getElem(pars_res,"params",0))
                            TransferFileCRC32 = LCase(cutHex(getElem(pars_res,"params",1)))
                            settings("1")\TransferName = getElem(pars_res,"params",2)
                            TransferType = 1 
                            TransferIsLastFile  = Val(getElem(pars_res,"lastfile",0))
                            settings("1")\TransferFileUploaded = 0 
                            If FileSize(settings("1")\TransferName+"temp")
                              DeleteFile(settings("1")\TransferName+"temp")
                            EndIf
                        EndSelect
                      EndIf
                      
                    Else
                      ;ANSWERS
                      If id_json_answer
                        If Not settings("1")\isPaired
                          ; WHen not paired
                          Select id_json_answer      
                            Case #ping_id
                              
                            Case #login_id
                              If Not Val(getElem(pars_res,"result",0))
                                If LCase(getElem(pars_res,"error",0))="invalid_login"
                                  Sprint(msginit$+">>Invalid Login<<")
                                  Delay(1000)
                                  ;settings("1")\dis= #True
                                  settings("1")\isAuthorized =#False                        
                                EndIf 
                                
                              Else
                                Sprint(msginit$+"Authorized")
                                Sprint(msginit$ + "Your ID "+settings("1")\name)
                                settings("1")\isAuthorized =#True
                                DisGadCon()
                              EndIf                      
                            Case #askpair_id
                              If Val(getElem(pars_res,"result",0))
                                settings("1")\RanD = getElem(pars_res,"encdata",0)
                                ;Sprint(msginit$+"Paired [Master] =>"+settings("1")\RanD)
                                settings("1")\isPaired = #True
                                DisableGadget(#ButtonGadgetClCloseCon, 0)
                                DisableGadget(#ButtonGadgetClCon, 1)
                                settings("1")\Role = 0
                                If Not FindMapElement(ClientsList(), GetGadgetText(#StringGadgetClid))
                                  HideGadget(#ButtonGadgetClCon, 1)
                                  HideGadget(#ButtonGadgetClADD, 0)
                                EndIf
                                
                                SendTextFrame("encod" + EncString(settings("1")\RanD))                                
                                
                              Else
                                Sprint(msginit$+"Not paired, reason["+LCase(getElem(pars_res,"error",0))+"]")
                                settings("1")\isPaired = #False
                                DisGadCon()
                                DisableGadget(#ButtonGadgetClCloseCon, 1)
                              EndIf                          
                            Case #answerpair_id 
                              If Val(getElem(pars_res,"result",0))
                                Sprint(msginit$+"Paired [Slave]")
                                settings("1")\isPaired = #True
                                DisableGadget(#ButtonGadgetClCon, 1)
                                
                                DisableGadget(#ButtonGadgetClCloseCon, 0)
                                settings("1")\Role = 1
                                SetGadgetText(#StringGadgetClid, settings("1")\AskPairName)
                                If Not FindMapElement(ClientsList(), settings("1")\AskPairName)
                                  HideGadget(#ButtonGadgetClCon, 1)
                                  HideGadget(#ButtonGadgetClADD, 0)
                                EndIf
                                
                                
                                
                              Else
                                Sprint(msginit$+"Not paired, reason["+LCase(getElem(pars_res,"error",0))+"]")
                                settings("1")\isPaired = #False
                                DisGadCon()
                                DisableGadget(#ButtonGadgetClCloseCon, 1)
                              EndIf                      
                          EndSelect
                        Else
                          ;When paired
                          Select id_json_answer
                            Case #sendpart_id
                              Values = Abs( settings("1")\currentJob - Val(getElem(pars_res,"result",0)))
                              If Values>6 And settings("1")\delayTime<#maxDelay
                                settings("1")\delayTime + 10
                                If settings("1")\delayTime>#maxDelay
                                  settings("1")\delayTime = #maxDelay
                                EndIf                                
                                Sprint(msginit$+"Delay set to "+Str(settings("1")\delayTime)+"/"+Str(Values))
                              ElseIf Values>3 And settings("1")\delayTime<#maxDelay/2
                                settings("1")\delayTime + 10
                                If settings("1")\delayTime>#maxDelay/2
                                  settings("1")\delayTime = #maxDelay/2
                                EndIf                                
                                Sprint(msginit$+"Delay set to "+Str(settings("1")\delayTime)+"/"+Str(Values))
                              ElseIf Values<2 And settings("1")\delayTime>#minDelay
                                settings("1")\delayTime - 10
                                If settings("1")\delayTime<#minDelay
                                  settings("1")\delayTime = #minDelay
                                EndIf
                                Sprint(msginit$+"Delay set to "+Str(settings("1")\delayTime)+"/"+Str(Values))
                              EndIf
                          EndSelect
                          
                        EndIf
                        
                      EndIf
                    EndIf
                    
                    If IsJSON(pars_res)
                      FreeJSON(pars_res)
                    EndIf 
                  Else
                    Sprint(msginit$+"Unknown json")
                  EndIf
                  
                Else
                  Debug "Corrupted json data=> move "+Str(ReceivedBytes)+" bytes to begin of buffer "+PeekS(*BufferPointer, ReceivedBytes,#PB_Unicode)  
                  
                  MoveMemory(*BufferPointer, *Buffer, ReceivedBytes)
                  Break
                EndIf
                
                Debug "REST:"+Str(ReceivedBytes)
              EndIf
            Until ReceivedBytes=0 Or settings("1")\thrCNquit Or settings("1")\dis= #True
          EndIf
          
        Case #PB_NetworkEvent_Disconnect
          Debug "getwork disconnected"
          Sprint(msginit$+" disconnected")
          settings("1")\Connect = 0
          settings("1")\dis= #True
      EndSelect
      
      If ElapsedMilliseconds() - settings("1")\lastpingTimer>10000
        settings("1")\lastpingTimer = ElapsedMilliseconds()                 
        sendPing()        
      EndIf
    EndIf
    Delay (1)
  Until settings("1")\thrCNquit
  If settings("1")\Connect
    CloseNetworkConnection(settings("1")\Connect)
    settings("1")\Connect = 0
  EndIf
  FreeMemory(*Buffer)
  Sprint(msginit$ + "Thread closed")
  settings("1")\thrCN = 0
  ProcedureReturn err  
EndProcedure

Procedure ReadClients()
  Protected Text$
  Shared ClientsList(), SettingsFileName$
  If ReadFile(#File, SettingsFileName$)
    While Eof(0) = 0
      Text$ = ReadString(#File, #PB_Unicode)
      If Len(Text$)=66
        AddGadgetItem(#StringGadgetClid, -1, Text$)
        ClientsList(Text$)\isOnline = #False
      EndIf
    Wend
    CloseFile(#File)
  Else
    Sprint("Can`t open "+SettingsFileName$)
  EndIf
  SetGadgetState(#StringGadgetClid, 0)
EndProcedure

Procedure.i WriteClients(newCl.s)
  Protected Text$, result=0
  Shared ClientsList(), SettingsFileName$
  If OpenFile(#File, SettingsFileName$, #PB_File_Append)   
    WriteStringN(#File, newCl, #PB_Unicode)
    CloseFile(#File)
    result = 1
  Else
    Sprint("Can`t open "+SettingsFileName$)
  EndIf
ProcedureReturn result
EndProcedure

Procedure GetClipboardNameFiles(List Files.s())
  Protected *lpszFileName, hGlobal, hDrop, num_file_names, i
  
  ClearList(Files())
  
  If IsClipboardFormatAvailable_(#CF_HDROP)
    *lpszFileName = AllocateMemory(#MAX_PATH)
    OpenClipboard_(0)
    hGlobal = GetClipboardData_(#CF_HDROP)
    If hGlobal
      hDrop = GlobalLock_(hGlobal)
      If hDrop
        num_file_names = DragQueryFile_(hDrop, -1, #NULL$, 0)
        
        For i = 1 To num_file_names
          DragQueryFile_(hDrop, i - 1, *lpszFileName, #MAX_PATH)
          AddElement(Files())
          Files()  = PeekS(*lpszFileName)
        Next i
        GlobalUnlock_(hGlobal)
      EndIf
    EndIf
    CloseClipboard_()
    FreeMemory(*lpszFileName)
  EndIf
  
  
EndProcedure

Procedure IsFilesClipBoard()
  Protected result
  
  If OpenClipboard_(0)
    If IsClipboardFormatAvailable_(#CF_HDROP)
      result = #True
    EndIf
    CloseClipboard_()
  EndIf
  ProcedureReturn result
EndProcedure

Procedure setipgadget(ip$)
  Protected i, Dim ipfield.i(4)
  For i = 1 To 4
    If CountString(ip$, ".")=3
      If Val(StringField(ip$, i, "."))>255 Or Val(StringField(ip$, i, "."))<0
        ip$=""
        Break
      Else 
        ipfield(i-1)=Val(StringField(ip$, i, "."))
      EndIf
    EndIf
  Next i
  If ip$
    SetGadgetState(#IPGadget, MakeIPAddress(ipfield(0), ipfield(1), ipfield(2), ipfield(3)))
  EndIf
EndProcedure
    
UseJPEGImageEncoder() 
UseJPEGImageDecoder() 
UsePNGImageDecoder()
UsePNGImageEncoder()
UseJPEG2000ImageEncoder()
UseJPEG2000ImageDecoder()

InitNetwork()
Define Event
Define winH, winW, KBTimer, insecondsend.f, a$, thrCN, LastMoveMouseTimer, lastChangeDelay, clipget = #False, lastFileBufferCheck, NewList FileList.s(), clip$, i



settings("1")\pass = "x"
settings("1")\host = "127.0.0.1"
settings("1")\port = 8000
settings("1")\isPaired = #False
settings("1")\isAuthorized = #False
settings("1")\Connect = #False
settings("1")\isWaitPart = #False
settings("1")\isBroadcast = #False
settings("1")\isReadyRecieve = #False
settings("1")\quit = #False
ClearClipboard()

If OpenWindow(0, (GetSystemMetrics_(#SM_CXSCREEN)-600)/2, (GetSystemMetrics_(#SM_CYSCREEN)-600)/2, 600, 100, "RemoteDesktop", #PB_Window_SystemMenu  |#PB_Window_SizeGadget | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget)
  settings("1")\PrivateKey = GetPrivateKey()
  settings("1")\CPublicKey = GetPublicKey(settings("1")\PrivateKey)  
  settings("1")\name = settings("1")\CPublicKey
  settings("1")\SecretBuf32 = AllocateMemory(32)
  
  winH = WindowHeight(0)             
  winW = WindowWidth(0)      
  
  EditorGadget(#EditorGaget, 10, 10, 450, 50, #PB_Editor_ReadOnly)
  SpinGadget(#SpinGagetImgQuality, 540, 10, 50, 25, 0, 9, #PB_Spin_Numeric ) 
  GadgetToolTip(#SpinGagetImgQuality, "Set the image transmission quality ") 
  
  IPAddressGadget(#IPGadget, 10, 10, 200, 20)
  GadgetToolTip(#IPGadget, "Enter IPv4 server address here") 
  
  ButtonGadget(#ButtonIPSave, 220, 10, 55, 20, "Save")
  GadgetToolTip(#ButtonIPSave, "Save current server ip address") 
  
  ButtonGadget(#ButtonGadgetTransferToRemote, 470, 10, 55, 20, "=>remote")
  ButtonGadget(#ButtonGadgetTransferFromRemote, 470, 40, 55, 20, "remote=>")
  DisableGadget(#ButtonGadgetTransferToRemote, 1)
  DisableGadget(#ButtonGadgetTransferFromRemote, 1)
  GadgetToolTip(#ButtonGadgetTransferFromRemote, "Copy file from remote client to program folder")
  GadgetToolTip(#ButtonGadgetTransferToRemote, "Copy file to remote client program folder")
  
  ComboBoxGadget(#StringGadgetClid, 10, 70, 450, 20, #PB_ComboBox_Editable )
  GadgetToolTip(#StringGadgetClid, "Set remote client ID here") 
    
  ButtonGadget(#ButtonGadgetClCon, 470, 70, 55, 20, "Connect")
  GadgetToolTip(#ButtonGadgetClCon, "Connect to selected remote client") 
  
  ButtonGadget(#ButtonGadgetClADD, 470, 70, 55, 20, "Add")
  GadgetToolTip(#ButtonGadgetClADD, "Add client to white list") 
  
  ButtonGadget(#ButtonGadgetClCloseCon, 535, 70, 55, 20, "Break")
  GadgetToolTip(#ButtonGadgetClCloseCon, "Break connection with remote client") 
  
  ButtonGadget(#ButtonGadgetServer, 535, 10, 55, 20, "Server")
  GadgetToolTip(#ButtonGadgetServer, "Set server ip address") 
  
  CheckBoxGadget(#AutoPairCheckBox, 535,  44, 50, 20, "Auto"): SetGadgetState(#AutoPairCheckBox, #PB_Checkbox_Checked)
  GadgetToolTip(#AutoPairCheckBox, "Enable auto connection from white list")
  
  
  TextGadget(#txtRequestGadget, 10, 70, 450, 20, "")
  ButtonGadget(#ButtonGadgetRequestYes, 470, 70, 55, 20, "Accept")
  GadgetToolTip(#ButtonGadgetRequestYes, "Accept incoming connection")
  
  ButtonGadget(#ButtonGadgetRequestNo, 535, 70, 55, 20, "Skip")
  GadgetToolTip(#ButtonGadgetRequestNo, "Skip incoming connection") 
  
  HideGadget(#IPGadget, 1)
  HideGadget(#ButtonIPSave, 1)
  
  HideGadget(#txtRequestGadget, 1)
  HideGadget(#ButtonGadgetRequestYes, 1)
  HideGadget(#ButtonGadgetRequestNo, 1)
  
  HideGadget(#ButtonGadgetClADD, 1)
  
  CanvasGadget(#CanvasGaget,  10, 100, 580, 10, #PB_Canvas_Keyboard)                     
  HideGadget(#CanvasGaget, 1)
  
  DisableGadget(#ButtonGadgetClCon, 1)
  
  SetGadgetState (#SpinGagetImgQuality, 5)
  HideGadget(#SpinGagetImgQuality, 1)
  settings("1")\imgquality = GetGadgetState(#SpinGagetImgQuality)
  SetGadgetAttribute(#CanvasGaget, #PB_Canvas_Cursor, #PB_Cursor_Invisible) 
  
  ;read server IP adress
  If FileSize(SettingsFileName$)=-1
    ;File not found
    If OpenFile(#File, SettingsFileName$)   
      WriteStringN(#File, settings("1")\host, #PB_Unicode)
      CloseFile(#File)
    EndIf
  ElseIf FileSize(SettingsFileName$)>0
    If ReadFile(#File, SettingsFileName$)      
      ;check ip
      a$ = ReadString(#File, #PB_Unicode)
      If CountString(a$, ".")=3
        For i = 1 To 4
          If Val(StringField(a$, i, "."))>255 Or Val(StringField(a$, i, "."))<0
            a$=""
            Break
          EndIf
        Next i
        If a$
          settings("1")\host = a$
        EndIf
      EndIf      
      CloseFile(#File)
    EndIf
  EndIf
  
  setipgadget(settings("1")\host )
  
  thrCN = CreateThread(@conecthost(),0)
  
  
  ReadClients()
  
  
  Repeat 
    ;check fileBuffer
    If settings("1")\IamFileTranciever = #False
      If ElapsedMilliseconds()-lastFileBufferCheck>250
        lastFileBufferCheck = ElapsedMilliseconds()
        If IsFilesClipBoard()
          GetClipboardNameFiles(FileList())
          settings("1")\Newclipboardfilename = ""
          ForEach FileList()
            settings("1")\Newclipboardfilename +  FileList() + #LF$
          Next  
          If settings("1")\Newclipboardfilename <> settings("1")\clipboardfilename And settings("1")\Newclipboardfilename<>""
            If settings("1")\Role = 0 And settings("1")\isReadyRecieve = #True;master
              settings("1")\clipboardfilename = settings("1")\Newclipboardfilename 
              DisableGadget(#ButtonGadgetTransferFromRemote, 1) 
              DisableGadget(#ButtonGadgetTransferToRemote, 0) 
              
            EndIf
          EndIf
        Else
          DisableGadget(#ButtonGadgetTransferToRemote, 1)           
          If settings("1")\Newclipboardfilename <> settings("1")\clipboardfilename And settings("1")\Newclipboardfilename<>""
            ;got clipboard file frome remote
            settings("1")\clipboardfilename = settings("1")\Newclipboardfilename                              
            DisableGadget(#ButtonGadgetTransferFromRemote, 0)                                              
          EndIf
        EndIf
      EndIf
      
      If settings("1")\IamFileTranciever = #False
        If settings("1")\Role = 0 And settings("1")\isReadyRecieve = #True;master          
          clip$ = GetClipboardText()  
          If settings("1")\clipboardtext <> clip$ And clip$<>""          
            If settings("1")\Newclipboardtext= clip$
              settings("1")\clipboardtext = clip$
            Else
              settings("1")\clipboardtext = clip$
              settings("1")\Newclipboardtext = clip$            
              SendClipboardTxt()   
            EndIf
          EndIf               
        EndIf
      EndIf 
    EndIf
    
    
    
    
    
    If settings("1")\isAskPair = #True And Date()-settings("1")\AskPairTime>20
      AcceptSkipPair(#False)
      HideAskPair(#True) 
      
      settings("1")\isAskPair = #False
      settings("1")\AskPairName = ""      
      SetWindowState(0, settings("1")\AskWinState) 
    EndIf
    
    If ElapsedMilliseconds() - KBTimer>1000
      KBTimer = ElapsedMilliseconds()
      If insecondsend>totalsended
        insecondsend = 0
      EndIf
      Kbsecond = (totalsended - insecondsend)
      insecondsend = totalsended   
      
      a$ = "Total:"+StrD(totalsended/1024,2)+"Mb " + StrD(Kbsecond,2)+" Kb/s"
      If settings("1")\Role = 0 And settings("1")\isReadyRecieve
        a$ = "["+ Str(MapSize(req() ))+ "] " + a$
        a$ + " ("+Str(settings("1")\frameW) + "x"+ Str(settings("1")\frameH) +"["+Str(settings()\Wpartx)+"x"+Str(settings()\WpartY)+"]) => ("+Str(settings("1")\canvasW)+"x"+Str( settings("1")\canvasH)+")"
      EndIf
      
        If MapSize(keybardmap())
          a$ = "K["+ Str(MapSize(keybardmap()))+ "] " + a$
        EndIf
      
      SetWindowTitle(0, a$)
    EndIf 
       
    Event  = WaitWindowEvent(10) 
    If Event
      Select Event
        Case #PB_Event_SizeWindow
          resizeCanvas(#CanvasGaget, settings("1")\frameW, settings("1")\frameH)          
        Case #PB_Event_CloseWindow
          settings("1")\quit = #True
          settings("1")\thrAPquit = #True
          settings("1")\thrCNquit = #True
          settings("1")\thrMSquit = #True
        Case #PB_Event_Gadget
          Select EventGadget()
            Case #ButtonGadgetServer
              HideGadget(#EditorGaget,1)
              HideGadget(#IPGadget, 0)
              HideGadget(#ButtonIPSave, 0)
            Case #ButtonIPSave
              HideGadget(#EditorGaget,0)
              HideGadget(#IPGadget, 1)
              HideGadget(#ButtonIPSave, 1)
              a$=GetGadgetText(#IPGadget)
              If a$<>settings("1")\host
                If OpenFile(#File, SettingsFileName$)   
                  WriteStringN(#File, a$, #PB_Unicode)
                  CloseFile(#File)
                  Sprint("New server IP: "+a$+" saved ")
                  settings("1")\host = a$
                  settings("1")\dis = 1
                Else
                  Sprint("Can`t open "+SettingsFileName$)
                EndIf
              EndIf
            Case #ButtonGadgetTransferToRemote
              settings("1")\IamFileTrancieverName = settings("1")\clipboardfilename
              settings("1")\IamsendallFiles = #False
              settings("1")\IamFileTranciever = #True
              DisableGadget(#ButtonGadgetTransferToRemote,1)
              DisableGadget(#CanvasGaget,1)
              DisableGadget(#SpinGagetImgQuality,1)
            Case #ButtonGadgetTransferFromRemote
              AskFileClipBoard()
              DisableGadget(#ButtonGadgetTransferFromRemote,1)
              settings("1")\IamFileTrancieverName = settings("1")\clipboardfilename
            Case #ButtonGadgetClADD
              If Not FindMapElement(ClientsList(), GetGadgetText(#StringGadgetClid))
                ClientsList(GetGadgetText(#StringGadgetClid))\isOnline  = #True
                AddGadgetItem(#StringGadgetClid, -1, GetGadgetText(#StringGadgetClid))
                If WriteClients(GetGadgetText(#StringGadgetClid))
                  Sprint("Client "+Left(GetGadgetText(#StringGadgetClid),5)+".."+Right(GetGadgetText(#StringGadgetClid),5)+" saved")
                EndIf
              EndIf
            Case #SpinGagetImgQuality     
              If settings("1")\imgquality<>GetGadgetState(#SpinGagetImgQuality)
                settings("1")\imgquality = GetGadgetState(#SpinGagetImgQuality)
                SetQualityFrame(settings("1")\imgquality)
              EndIf
            Case #ButtonGadgetClCon 
              If settings("1")\isPaired = #False                
                If GetGadgetText(#StringGadgetClid)<>"" And Len(GetGadgetText(#StringGadgetClid))=66
                  settings("1")\Secret = GetSecretKey(settings("1")\PrivateKey, GetGadgetText(#StringGadgetClid))
                  Sprint("Pairing request sent")
                  Curve::m_sethex32(settings("1")\SecretBuf32, settings("1")\Secret)                   
                  AskPair(GetGadgetText(#StringGadgetClid))
                EndIf
              EndIf
            Case #ButtonGadgetClCloseCon
              If settings("1")\isPaired = #True
                settings("1")\dis = #True
              EndIf              
              
            Case #ButtonGadgetRequestYes
              If settings("1")\isPaired = #False And settings("1")\isAskPair = #True
                AcceptSkipPair(#True)
                HideAskPair(#True) 
                
                settings("1")\isAskPair = #False
                settings("1")\Secret = GetSecretKey(settings("1")\PrivateKey, settings("1")\AskPairName)
                Curve::m_sethex32(settings("1")\SecretBuf32, settings("1")\Secret)                
              EndIf
            Case #ButtonGadgetRequestNo
              If settings("1")\isPaired = #False And settings("1")\isAskPair = #True
                AcceptSkipPair(#False)
                HideAskPair(#True) 
                
                settings("1")\isAskPair = #False
                settings("1")\AskPairName = ""
                
              EndIf
            Case #StringGadgetClid
              Select EventType()                  
                Case #PB_EventType_Change 
                  If Len(GetGadgetText(#StringGadgetClid))<>66
                    SetGadgetColor(#StringGadgetClid, #PB_Gadget_BackColor, $0000FF)
                    DisableGadget(#ButtonGadgetClCon, 1)
                  Else
                    SetGadgetColor(#StringGadgetClid, #PB_Gadget_BackColor, $FFFFFF)
                    DisableGadget(#ButtonGadgetClCon, 0)
                  EndIf
              EndSelect
            Case  #CanvasGaget
              If settings("1")\Role = 0 And settings("1")\isReadyRecieve = #True;master
                Select EventType()
                  Case #PB_EventType_KeyDown 
                    If isInCnvasArea                      
                      SendKeyFrame(GetGadgetAttribute(#CanvasGaget, #PB_Canvas_Key),1)
                    EndIf 
                  Case #PB_EventType_KeyUp 
                    If isInCnvasArea                      
                      SendKeyFrame(GetGadgetAttribute(#CanvasGaget, #PB_Canvas_Key),0)
                    EndIf              
                  Case #PB_EventType_MouseEnter
                    isInCnvasArea = #True
                    
                  Case #PB_EventType_MouseLeave 
                    isInCnvasArea = #False                  
                    SendMouseFrame(canvasMouseX, canvasMouseY, 2, 2, 2, 0)
                  Case #PB_EventType_MouseMove
                    If isInCnvasArea
                      If ElapsedMilliseconds()-LastMoveMouseTimer>10
                        LastMoveMouseTimer = ElapsedMilliseconds()
                        If Abs(canvasMouseX - GetGadgetAttribute(#CanvasGaget, #PB_Canvas_MouseX))>2 Or Abs(canvasMouseY - GetGadgetAttribute(#CanvasGaget, #PB_Canvas_MouseY))>2
                          
                          canvasMouseX = GetGadgetAttribute(#CanvasGaget, #PB_Canvas_MouseX) 
                          canvasMouseY = GetGadgetAttribute(#CanvasGaget, #PB_Canvas_MouseY) 
                          SendMouseFrame(canvasMouseX, canvasMouseY, 2, 2, 2, 0)
                        EndIf
                      EndIf
                    EndIf                  
                  Case #PB_EventType_LeftButtonDown
                    If isInCnvasArea                         
                      SendMouseFrame(canvasMouseX, canvasMouseY, 1, 2, 2, 0)                    
                    EndIf
                  Case #PB_EventType_LeftButtonUp
                    If isInCnvasArea                   
                      SendMouseFrame(canvasMouseX, canvasMouseY, 0, 2, 2, 0)                    
                    EndIf
                    
                  Case #PB_EventType_MiddleButtonDown
                    If isInCnvasArea                    
                      SendMouseFrame(canvasMouseX, canvasMouseY, 2, 1, 2, 0)                    
                    EndIf
                  Case #PB_EventType_MiddleButtonUp
                    If isInCnvasArea                    
                      SendMouseFrame(canvasMouseX, canvasMouseY, 2, 0, 2, 0)                    
                    EndIf
                    
                  Case #PB_EventType_RightButtonDown
                    If isInCnvasArea                    
                      SendMouseFrame(canvasMouseX, canvasMouseY, 2, 2, 1, 0)                    
                    EndIf
                  Case #PB_EventType_RightButtonUp 
                    If isInCnvasArea                    
                      SendMouseFrame(canvasMouseX, canvasMouseY, 2, 2, 0, 0)                    
                    EndIf
                    
                    
                  Case #PB_EventType_MouseWheel 
                    If isInCnvasArea               
                      SendMouseFrame(canvasMouseX, canvasMouseY, 2, 2, 2, GetGadgetAttribute(#CanvasGaget, #PB_Canvas_WheelDelta ))                    
                    EndIf
                    
                EndSelect
              EndIf
          EndSelect
      EndSelect
    Else
      
    EndIf
    
    
  Until settings("1")\thrAP=0 And settings("1")\thrCN=0 And  settings("1")\thrMS=0 And settings("1")\quit
  
  
  Delay(1000)
EndIf


; IDE Options = PureBasic 5.31 (Windows - x64)
; CursorPosition = 442
; FirstLine = 417
; Folding = +--------
; EnableUnicode
; EnableThread
; EnableAdmin
; Executable = remote_client4.exe
; DisableDebugger