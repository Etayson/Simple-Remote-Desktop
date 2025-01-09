Structure pairReqStructure  
  *buff
  bufsize.i
EndStructure

Structure ClientInfoStructure  
  authorization.i  
  time.i
  ClientId.i
  ClientIP.i
  name.s  
  quit.i
  isPaired.i  
  pairname.s
  pairId.i
  idThread.i
  threadquit.i
  List pairreq.pairReqStructure()
EndStructure

Structure ServerStructure
  Quit.i
  Map Client.ClientInfoStructure()
EndStructure

Structure SocketStructure
  id.i
  ClientId.i
  req.s  
EndStructure



Structure settingsStructure
  port.i   
EndStructure

Declare PairThread(ClientId)

#MB=1048576
#GB=1073741824

#colorBlue=1
#colorGreen=2
#colorCyan=3
#colorRed=4
#colorMagenta=5
#colorBrown=6
#colorDefault=7
#colorDarkgrey=8
#colorYellow=14
#colorWhite=15
#colorbrightmagenta=13
#colorBrightGreen = 10
Enumeration
  #Server  
EndEnumeration

Define Server.ServerStructure
Define NewMap settings.settingsStructure()
Define NewMap socketreq.SocketStructure()
 
Define stratumwork_g

Define MutexConsole, MutexClientMap, socketreqMutex
MutexConsole = CreateMutex()
MutexClientMap = CreateMutex()
socketreqMutex = CreateMutex()

Procedure SPrint(text$, cl)
 Shared MutexConsole
  LockMutex(MutexConsole)
  ConsoleColor(cl,0)
  Debug FormatDate("%hh:%ii:%ss ", Date())+" "+text$
  PrintN(FormatDate("%hh:%ii:%ss ", Date())+" "+text$)  
  ConsoleColor(#colorDefault,0)
  UnlockMutex(MutexConsole)
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

Procedure.i SendQuestionstratum(con_id,string$)
 Protected err
 err=0
 
  If con_id
    SendNetworkString(con_id,string$+#LF$,#PB_Unicode)
    Debug "send to client :"+Str(con_id)+">"+string$
  EndIf
 ProcedureReturn err  
EndProcedure

Procedure sendTotalClients(*Server.ServerStructure)
  Protected NewMap templist.s()
  Shared MutexClientMap
  LockMutex(MutexClientMap)        
    ForEach *Server\Client()        
      If *Server\Client()\authorization And *Server\Client()\isPaired = #False          
        templist(Str(*Server\Client()\ClientId))= *Server\Client()\name
      EndIf
    Next
  UnlockMutex(MutexClientMap)
    
  FreeMap(templist())
EndProcedure

Procedure AnaliseRequest(reqid)
  Protected answer_f$, metod_json_answer$, id_json_answer, pars_res, name$, pass$, tempjson, get_work, get_resfalse.s, batchdata$, batchLength, batchCRC32$, computeCRC32, filename$
  Protected ClientID, ClientIP, PairId, pairName.s, encData$
  Protected procname$ = "[SERVER] ", Error$, get_pair.s
  
  Shared MutexClientMap,  settings(),  socketreq(), socketreqMutex, Server.ServerStructure
  
  Debug( "Thread AnaliseRequest #"+Str(reqid)+" started") 
  
  ClientID = socketreq(Str(reqid))\ClientId
  ClientIP = Server\Client(Str(ClientID))\ClientIP
  answer_f$ = socketreq(Str(reqid))\req
   If Server\Client(Str(ClientID))\authorization
     procname$ + Str(ClientID)+"\"+IPString(ClientIP)+" ["+Server\Client(Str(ClientID))\name+"]"
  EndIf
   
  pars_res=ParseJSON(#PB_Any, answer_f$) 
  If pars_res 
    metod_json_answer$ = LCase(getElem(pars_res,"method",0))
    id_json_answer = Val(LCase(getElem(pars_res,"id",0)))
                  
    Select metod_json_answer$
      Case "ping"
        If  Server\Client(Str(ClientID))\authorization 
            tempjson = CreateJSON(#PB_Any)
                If tempjson 
                  get_work = SetJSONObject(JSONValue(tempjson))                  
                  SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
                  SetJSONBoolean(AddJSONMember(get_work, "result"), #True) 
                  SetJSONNull(AddJSONMember(get_work, "error"))
                  get_resfalse.s=ComposeJSON(tempjson)
                  FreeJSON(tempjson)
                  SendQuestionstratum(ClientID,get_resfalse)
                EndIf
        EndIf
      Case "login"         
        If Not Server\Client(Str(ClientID))\authorization            
              name$=LCase(getElem(pars_res,"params",0))
              pass$ = getElem(pars_res,"params",1)
                                                          
              If Len(name$)<>66
                name$=""
              EndIf
              If name$ 
                LockMutex(MutexClientMap)
                ForEach Server\Client() 
                  If Server\Client()\name = name$ And Server\Client()\authorization
                    Error$ = "invalid_login"
                    Break
                  EndIf
                Next
                UnlockMutex(MutexClientMap)
              EndIf
              If Error$="" And name$         
                LockMutex(MutexClientMap)                  
                Server\Client(Str(ClientID))\name = name$           
                Server\Client(Str(ClientID))\authorization = #True    
                UnlockMutex(MutexClientMap)                 
                Sprint(procname$+Str(ClientID)+"\"+IPString(ClientIP)+" AUTORIZED AS ["+name$ + "] TOTAL CLIENTS: " + Str(MapSize(Server\Client())),#colorMagenta)
                tempjson = CreateJSON(#PB_Any)
                If tempjson 
                  get_work = SetJSONObject(JSONValue(tempjson))                  
                  SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
                  SetJSONBoolean(AddJSONMember(get_work, "result"), #True) 
                  SetJSONNull(AddJSONMember(get_work, "error"))
                  get_resfalse.s=ComposeJSON(tempjson)
                  FreeJSON(tempjson)
                  SendQuestionstratum(ClientID,get_resfalse)
                EndIf
              
              Else 
                If Error$=""
                  Error$ = "invalid_login"
                EndIf
              EndIf
        Else
           Error$ = "already_authorized" 
        EndIf
        
           
      Case "askpair"        
        If Server\Client(Str(ClientID))\authorization
          If Not Server\Client(Str(ClientID))\isPaired
            name$=LCase(getElem(pars_res,"params",0))
            Server\Client(Str(ClientID))\pairname = ""
            Server\Client(Str(ClientID))\pairId = 0
            LockMutex(MutexClientMap)        
            ForEach Server\Client()        
              If Server\Client()\name = name$ And Server\Client()\authorization 
                If Not Server\Client()\isPaired
                  If Server\Client()\ClientId<>ClientID
                    PairId = Server\Client()\ClientId
                    pairName = Server\Client()\name  
                  Else
                    Error$ = "you_can`t_pair_yorself"
                  EndIf
                Else
                  Error$ = "client_already_paired"
                EndIf
                Break
              EndIf
            Next
            UnlockMutex(MutexClientMap)
            If PairId
              Server\Client(Str(ClientID))\pairname = pairName
              Server\Client(Str(ClientID))\pairId = PairId
              
              tempjson = CreateJSON(#PB_Any)
              If tempjson   
                get_work = SetJSONObject(JSONValue(tempjson))   
                SetJSONInteger(AddJSONMember(get_work, "id"), 3) 
                SetJSONString(AddJSONMember(get_work, "method"), "pair")
                Values =SetJSONArray(AddJSONMember(get_work, "params"))                 
                SetJSONString(AddJSONElement(Values), Server\Client(Str(ClientID))\name)
                get_pair.s=ComposeJSON(tempjson)
                FreeJSON(tempjson)
                SendQuestionstratum(PairId,get_pair)
              EndIf 
            Else
              If Error$=""
                Error$ = "paired_client_not_found"
              EndIf
            EndIf
          Else
            Error$ = "you_already_paired"
          EndIf          
        Else
          Error$ = "unauthorized"                              
        EndIf                           
        
             
        Case "answerpair" 
          If Server\Client(Str(ClientID))\authorization
            name$=LCase(getElem(pars_res,"params",0))
            encData$ = getElem(pars_res,"params",1)
            pairName = Server\Client(Str(ClientID))\name
            
              LockMutex(MutexClientMap)        
              ForEach Server\Client()        
                If Server\Client()\name = name$ And Server\Client()\authorization  And  Server\Client()\pairname = pairName And   Server\Client()\pairId = ClientID And Not Server\Client()\isPaired          
                  PairId = Server\Client()\ClientId
                  pairName = Server\Client()\name  
                  If Val(getElem(pars_res,"result",0))
                    ;accept pair                   
                    Server\Client()\isPaired = #True
                    Server\Client(Str(ClientID))\pairId = PairId
                    Server\Client(Str(ClientID))\pairname = pairName
                    Server\Client(Str(ClientID))\isPaired = #True
                    Sprint(procname$+ " <> [" +Server\Client(Str(ClientID))\pairname+"]",#colorGreen)
                    Server\Client(Str(ClientID))\threadquit = #False
                    Server\Client(Str(ClientID))\idThread = CreateThread(@PairThread(), ClientID)
                    Server\Client(Str(PairId))\threadquit = #False
                    Server\Client(Str(PairId))\idThread = CreateThread(@PairThread(), PairId)
                  Else
                    ;skiped pair
                    Server\Client()\pairname = ""
                    Server\Client()\pairId = 0
                    Server\Client()\isPaired = #False                    
                  EndIf
                  
                  Break
                EndIf
              Next
              UnlockMutex(MutexClientMap)
              If PairId
                tempjson = CreateJSON(#PB_Any)
                If tempjson 
                  get_work = SetJSONObject(JSONValue(tempjson))                  
                  SetJSONInteger(AddJSONMember(get_work, "id"), 1) 
                  SetJSONBoolean(AddJSONMember(get_work, "result"), Val(getElem(pars_res,"result",0))) 
                  SetJSONString(AddJSONMember(get_work, "error"),Error$)
                  SetJSONString(AddJSONMember(get_work, "encdata"),encData$)
                  get_resfalse.s=ComposeJSON(tempjson)
                  FreeJSON(tempjson)
                  SendQuestionstratum(PairId,get_resfalse)
                EndIf
                tempjson = CreateJSON(#PB_Any)
                If tempjson 
                  get_work = SetJSONObject(JSONValue(tempjson))                  
                  SetJSONInteger(AddJSONMember(get_work, "id"), 3) 
                  SetJSONBoolean(AddJSONMember(get_work, "result"), Val(getElem(pars_res,"result",0))) 
                  SetJSONString(AddJSONMember(get_work, "error"),Error$)
                  
                  get_resfalse.s=ComposeJSON(tempjson)
                  FreeJSON(tempjson)
                  SendQuestionstratum(ClientID,get_resfalse)
                EndIf
              EndIf          
        Else
          Error$ = "unauthorized"                              
        EndIf                           
        
        
    EndSelect   
    If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))            
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
    If IsJSON(pars_res)
      FreeJSON(pars_res) 
    EndIf
  EndIf
  
  LockMutex(socketreqMutex)
    DeleteMapElement(socketreq(), Str(reqid))
  UnlockMutex(socketreqMutex)
  Debug( "Thread AnaliseRequest #"+Str(reqid)+" ended") 
    
EndProcedure

Procedure Socket_ClientDisconnect(ClientID.i, *Server.ServerStructure) 
  Protected procname$="[SERVER] ", pairId
  Shared socketreqMutex
  If FindMapElement(*Server\Client(), Str(ClientID)) 
    *Server\Client(Str(ClientID))\threadquit = #True
    
    If *Server\Client(Str(ClientID))\isPaired = #False
      DeleteMapElement(*Server\Client(), Str(ClientID))   
      SPrint("TOTAL CLIENTS: " + Str(MapSize(*Server\Client())),#colorMagenta) 
    Else      
      pairId = *Server\Client(Str(ClientID))\pairId
      If FindMapElement(*Server\Client(), Str(pairId)) 
        CloseNetworkConnection(pairId)
        *Server\Client(Str(pairId))\threadquit = #True
        ;DeleteMapElement(*Server\Client(), Str(pairId))
      EndIf
      
    EndIf
  
EndIf
EndProcedure

Procedure Socket_Server(*Server.ServerStructure)
  
  Protected.i SEvent, ClientID, i, ClientIP,  ReceivedBytes, procname$="[SERVER] "
  Protected answer_t$, answer_f$, pos, pos2, pars_res, metod_json_answer$, id_json_answer, get_work, get_restrue.s, resultArr, ArrayValue, resultArr_1, mining_set_difficulty.s, filename$, computeCRC32file
  Protected full_size, pairId
  Protected  get_true.s, get_resfalse.s, poolid, socket, answer_ff$, ShareID, curmill
  Protected name$, worker$, posname,  a$, pass$
  Protected NewMap templist(), socketreqN
  Protected *Buffer, counter, tempjson, extr$, mining_subscribe_answer.s, flagidpool, sendedBytes, *pp, ping.s, pairreqN
    
  Shared MutexClientMap, settings(),socketreq(), socketreqMutex, stratumwork_g
  
                 
  SPrint(procname$ + "started And listen port:"+Str(settings("1")\port),#colorGreen)
  
  *Buffer = AllocateMemory(65536)
  If *Buffer
    Repeat
      stratumwork_g = 0
      ;-delete unatorized users
      counter+1
      If counter&$1ff=0 
      
        LockMutex(MutexClientMap)        
          ForEach *Server\Client()        
            If *Server\Client()\time And Not*Server\Client()\authorization 
              ; if connected but not autorized more than 2s
              If Date() - *Server\Client()\time > 2              
                  templist(Str(*Server\Client()\ClientID))=*Server\Client()\quit        
              EndIf 
            EndIf 
            If *Server\Client()\time And *Server\Client()\authorization 
              ; if connected but not autorized more than 2s
              If Date() - *Server\Client()\time > 30             
                  templist(Str(*Server\Client()\ClientID))=*Server\Client()\quit        
              EndIf 
            EndIf 
          Next
        UnlockMutex(MutexClientMap)       
       
        ForEach templist()
          If templist()=#False
            If Val(MapKey(templist()))>0
              CloseNetworkConnection(Val(MapKey(templist())))
            EndIf
            SPrint( "CLIENT: "+MapKey(templist()) + " TIME OUT. KICK...", #colorRed)
          Else
            Debug( "CLIENT: "+MapKey(templist()) + " DISCONNECT...")
          EndIf
          LockMutex(MutexClientMap)
            Socket_ClientDisconnect(Val(MapKey(templist())), *Server)
          UnlockMutex(MutexClientMap)
        Next
        ClearMap(templist())
      EndIf
      
      SEvent = NetworkServerEvent(#Server)
      If SEvent 
        ClientID = EventClient()
        ClientIP = GetClientIP(ClientID)
        Select SEvent
          Case #PB_NetworkEvent_Connect
            
              LockMutex(MutexClientMap)
                *Server\Client(Str(ClientID))\ClientId = ClientID
                *Server\Client(Str(ClientID))\ClientIP = ClientIP                                     
                *Server\Client(Str(ClientID))\time = Date()
                *Server\Client(Str(ClientID))\authorization = #False
                *Server\Client(Str(ClientID))\quit = #False
                
              UnlockMutex(MutexClientMap)
                
              SPrint(procname$ + "CLIENT: "+Str(ClientID)+" IP: "+IPString(*Server\Client(Str(ClientID))\ClientIP)+" HAS CONNECTED !",#colorMagenta)
              SPrint(procname$ + "TOTAL CLIENTS: " + Str(MapSize(*Server\Client())),#colorMagenta)
              
            Case #PB_NetworkEvent_Data
              *Server\Client(Str(ClientID))\time = Date()
              ;FillMemory(*Buffer, 10000)
              ReceivedBytes = ReceiveNetworkData(ClientID, *Buffer, 65536)            
              If ReceivedBytes>=0
                Debug "recieved "+Str(ReceivedBytes)+" bytes"
                If *Server\Client(Str(ClientID))\authorization And *Server\Client(Str(ClientID))\isPaired
                  ;if paired send all data to paired client
                  pairId = *Server\Client(Str(ClientID))\pairId
                  If *Server\Client(Str(pairId))\authorization And *Server\Client(Str(pairId))\isPaired
                    LockMutex(socketreqMutex)
                    LastElement(*Server\Client(Str(pairId))\pairreq())
                    AddElement(*Server\Client(Str(pairId))\pairreq())                                  
                    *Server\Client(Str(pairId))\pairreq()\buff = AllocateMemory(ReceivedBytes)                    
                    *Server\Client(Str(pairId))\pairreq()\bufsize = ReceivedBytes                    
                    CopyMemory(*Buffer, *Server\Client(Str(pairId))\pairreq()\buff, ReceivedBytes)
                    UnlockMutex(socketreqMutex)
                    
                    
                    
                  EndIf
                Else
                  answer_t$=PeekS(*Buffer, ReceivedBytes,#PB_Unicode)  
                  Debug answer_t$
                  pos=FindString(answer_t$, "{")
                  While pos                
                    pos2=FindString(answer_t$, "}",pos+1)
                    If pos2            
                      answer_f$=Mid(answer_t$,pos,pos2-pos+1)            
                      answer_f$ = RTrim(answer_f$,"}")
                      answer_f$ = LTrim(answer_f$,"{")                   
                      answer_f$ = "{"+answer_f$+"}"
                      
                      If MapSize(socketreq())=0
                        socketreqN = 0
                      EndIf
                      LockMutex(socketreqMutex)
                      socketreq(Str(socketreqN))\id = socketreqN
                      socketreq(Str(socketreqN))\ClientId = ClientID
                      socketreq(Str(socketreqN))\req = answer_f$    
                      UnlockMutex(socketreqMutex)
                      CreateThread(@AnaliseRequest(),socketreqN)
                      
                      
                     
                      socketreqN+1
                        
                      answer_t$= Right(answer_t$, Len(answer_t$)-pos2)
                      pos=FindString(answer_t$, "{")
                    Else
                     pos=0
                    EndIf
                  Wend  
                EndIf
                           
                
             EndIf
          Case #PB_NetworkEvent_Disconnect
          SPrint( "CLIENT: "+Str(ClientID) + " IP:"+IPString(*Server\Client(Str(ClientID))\ClientIP)+" DISCONNECT (CLIENT SIDE)",#colorMagenta)
          LockMutex(MutexClientMap)
              *Server\Client(Str(ClientID))\quit = #True
              Socket_ClientDisconnect(ClientID, *Server)
          UnlockMutex(MutexClientMap)
              
      EndSelect
    EndIf
    Delay(1)    
    Until *Server\Quit   
    FreeMemory(*Buffer)
    FreeMap(templist())
  Else
    SPrint(procname$ + "Can't allocate buffer",#colorred)
  EndIf  
EndProcedure

Procedure PairThread(ClientId)
  Protected *pp, *Buffer, bufsize, msginit$ = "PairThread ", sendedBytes
  Shared socketreqMutex, Server.ServerStructure
  SPrint(msginit$ + "started for "+Str(ClientId), #colorWhite)
  Repeat
    If ListSize(Server\Client(Str(ClientId))\pairreq())
      LockMutex(socketreqMutex)
      
        FirstElement(Server\Client(Str(ClientId))\pairreq()) 

        bufsize =  Server\Client(Str(ClientId))\pairreq()\bufsize
        *Buffer = AllocateMemory( bufsize )
        CopyMemory(Server\Client(Str(ClientId))\pairreq()\buff, *Buffer, bufsize)
        FreeMemory(Server\Client(Str(ClientId))\pairreq()\buff)
        DeleteElement(Server\Client(Str(ClientId))\pairreq(),1)        
     
      UnlockMutex(socketreqMutex)  
      If FindMapElement(Server\Client(), Str(ClientId))
        *pp = *Buffer
        Repeat    
          If Not FindMapElement(Server\Client(), Str(ClientId))
            Break
          EndIf
          If Server\Client(Str(ClientID))\threadquit
            Break
          EndIf
          sendedBytes = SendNetworkData(ClientId,*pp, bufsize)
          
          If sendedBytes = -1
            sendedBytes = 0
            Delay(1)
          EndIf
          *pp + sendedBytes
          bufsize = bufsize - sendedBytes
          ;Delay(1)
        Until bufsize=0
      Else
        SPrint(msginit$+ "Client "+Str(ClientId)+"Not found",#colorRed)        
      EndIf
      FreeMemory(*Buffer)
      *Buffer = 0
      
      
    EndIf
    Delay(1)    
  Until Server\Quit  Or  Server\Client(Str(ClientID))\threadquit
  
  LockMutex(socketreqMutex)
      ResetList(Server\Client(Str(ClientID))\pairreq())
      While NextElement(Server\Client(Str(ClientID))\pairreq())
        ; This is OK since the first call to NextElement() will move the current element to the first item in the list
        FreeMemory(Server\Client(Str(ClientID))\pairreq()\buff)
      Wend
  UnlockMutex(socketreqMutex)
  FreeList(Server\Client(Str(ClientID))\pairreq())
  DeleteMapElement(Server\Client(), Str(ClientID)) 
  SPrint(msginit$ + "stoped for "+Str(ClientId), #colorRed)
  

EndProcedure

Define StratServ, Thread

InitNetwork()
OpenConsole()
settings("1")\port=8000

If settings("1")\port
  StratServ = CreateNetworkServer(#Server,settings("1")\port,#PB_Network_TCP)
  If StratServ=0 
    Sprint("Can't create the SSserver ("+Str(settings("1")\port)+" in use ?).", #colorred) 
    Delay(2000)        
    End  
  EndIf
  Thread = CreateThread(@Socket_Server(), @Server)  
Else
  End
EndIf

Repeat 
  Delay(100)
ForEver  

; IDE Options = PureBasic 5.31 (Windows - x64)
; CursorPosition = 577
; FirstLine = 456
; Folding = --
; EnableUnicode
; EnableThread
; Executable = remote_server.exe
; DisableDebugger